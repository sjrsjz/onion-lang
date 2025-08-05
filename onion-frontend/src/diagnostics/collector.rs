use crate::diagnostics::{Diagnostic, ReportSeverity};

/// 一个 trait，用于统一处理直接的 Diagnostic 和 Box<dyn Diagnostic>
pub trait IntoDiagnosticBox {
    fn into_diagnostic_box(self) -> Box<dyn Diagnostic>;
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
    pub fn new() -> Self {
        DiagnosticCollector {
            diagnostics: Vec::new(),
        }
    }

    pub fn fatal<T: IntoDiagnosticBox, R>(&mut self, v: T) -> Result<R, ()> {
        let severity = v.severity();
        self.diagnostics.push(v.into_diagnostic_box());
        assert_eq!(severity, ReportSeverity::Error);
        Err(())
    }

    pub fn may_fatal<T: IntoDiagnosticBox>(&mut self, v: T) -> Result<(), ()> {
        let severity = v.severity();
        self.diagnostics.push(v.into_diagnostic_box());
        if severity == ReportSeverity::Error {
            Err(())
        } else {
            Ok(())
        }
    }

    pub fn report<T: IntoDiagnosticBox>(&mut self, v: T) {
        self.diagnostics.push(v.into_diagnostic_box());
    }


    /// 返回所有收集到的诊断信息的只读切片。
    pub fn diagnostics(&self) -> &[Box<dyn Diagnostic>] {
        &self.diagnostics
    }

    pub fn report_all(&self) {
        for diag in &self.diagnostics {
            println!("{}\n", diag.format_report());
        }
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.severity() == ReportSeverity::Error)
    }
}
