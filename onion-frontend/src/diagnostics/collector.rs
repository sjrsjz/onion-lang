
//! 诊断信息收集与统一接口模块。
//!
//! 提供统一的诊断对象收集、分级处理与批量报告能力，
//! 支持直接 Diagnostic 与 `Box<dyn Diagnostic>` 的无缝混用。
//!
//! # 主要功能
//! - 诊断对象的统一封装与分级
//! - 错误与警告的批量收集与报告
//! - 支持诊断对象的深拷贝与只读访问
//!
//! # 用法
//! ```ignore
//! let mut collector = DiagnosticCollector::new();
//! collector.report(MyDiagnostic::new(...));
//! if collector.has_errors() { ... }
//! ```

use crate::diagnostics::{Diagnostic, ReportSeverity};


/// 诊断对象统一封装 trait。
///
/// 用于将任意实现 Diagnostic 的类型（包括 `Box<dyn Diagnostic>`）
/// 统一转换为 `Box<dyn Diagnostic>`，并获取其严重级别。
pub trait IntoDiagnosticBox {
    /// 转换为 `Box<dyn Diagnostic>`
    fn into_diagnostic_box(self) -> Box<dyn Diagnostic>;
    /// 获取诊断对象的严重级别
    fn severity(&self) -> ReportSeverity;
}

impl<T: Diagnostic + 'static> IntoDiagnosticBox for T {
    fn into_diagnostic_box(self) -> Box<dyn Diagnostic> {
        Box::new(self)
    }
    
    fn severity(&self) -> ReportSeverity {
        Diagnostic::severity(self)
    }
}

impl IntoDiagnosticBox for Box<dyn Diagnostic> {
    fn into_diagnostic_box(self) -> Box<dyn Diagnostic> {
        self
    }
    
    fn severity(&self) -> ReportSeverity {
        Diagnostic::severity(self.as_ref())
    }
}


/// 诊断信息收集器。
///
/// 支持批量收集、分级处理、深拷贝与批量报告等功能。
#[derive(Debug)]
pub struct DiagnosticCollector {
    diagnostics: Vec<Box<dyn Diagnostic>>,
}

impl Clone for DiagnosticCollector {
    fn clone(&self) -> Self {
        DiagnosticCollector {
            diagnostics: self.diagnostics.iter().map(|o| o.copy()).collect(),
        }
    }
}

impl DiagnosticCollector {
    /// 创建新的诊断收集器。
    pub fn new() -> Self {
        DiagnosticCollector {
            diagnostics: Vec::new(),
        }
    }

    /// 报告致命错误并立即返回 Err(())。
    ///
    /// 仅允许 ReportSeverity::Error 级别。
    pub fn fatal<T: IntoDiagnosticBox, R>(&mut self, v: T) -> Result<R, ()> {
        let severity = v.severity();
        self.diagnostics.push(v.into_diagnostic_box());
        assert_eq!(severity, ReportSeverity::Error);
        Err(())
    }

    /// 报告诊断对象，若为错误则返回 Err(())，否则 Ok(())。
    pub fn may_fatal<T: IntoDiagnosticBox>(&mut self, v: T) -> Result<(), ()> {
        let severity = v.severity();
        self.diagnostics.push(v.into_diagnostic_box());
        if severity == ReportSeverity::Error {
            Err(())
        } else {
            Ok(())
        }
    }

    /// 报告任意诊断对象。
    pub fn report<T: IntoDiagnosticBox>(&mut self, v: T) {
        self.diagnostics.push(v.into_diagnostic_box());
    }

    /// 返回所有收集到的诊断信息的只读切片。
    pub fn diagnostics(&self) -> &[Box<dyn Diagnostic>] {
        &self.diagnostics
    }

    /// 批量打印所有诊断信息。
    pub fn report_all(&self) {
        for diag in &self.diagnostics {
            println!("{}\n", diag.format_report());
        }
    }

    /// 是否包含错误级别的诊断。
    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.severity() == ReportSeverity::Error)
    }
}
