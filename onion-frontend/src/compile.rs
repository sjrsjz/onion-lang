//! Onion 语言编译主流程模块。
//!
//! 本模块负责将 Onion 源代码从 Token 流、AST、IR 直至最终字节码的完整编译流程。
//! 包含诊断收集、循环依赖检测、编译时求值、语义分析、IR 生成与字节码翻译等核心步骤。
//!
//! # 编译流程概述
//! 1. 词法分析：源码 → Token 流
//! 2. 语法分析：Token 流 → AST
//! 3. 编译时求值：处理 @ 表达式、宏、依赖等
//! 4. 自动捕获与语义分析：变量作用域、类型检查
//! 5. IR 生成：AST → 中间表示（IR）
//! 6. 字节码生成：IR → 虚拟机指令
//!
//! # 主要函数
//! - `build_code`：主编译入口，生成 IRPackage
//! - `compile_to_bytecode`：IRPackage → 虚拟机字节码
//!
//! # 诊断与错误处理
//! - 所有阶段均支持详细的错误收集与报告
//! - 支持 IO 错误、借用错误、语法/语义/编译时错误等

use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::RwLock;

use crate::diagnostics::Diagnostic;
use crate::diagnostics::collector::DiagnosticCollector;
use crate::ir_generator::ir_generator;
use crate::parser::Source;
use crate::parser::analyzer::analyze_ast;
use crate::parser::analyzer::auto_capture_and_rebuild;
use crate::parser::ast::ast_token_stream;
use crate::parser::ast::build_ast;
use crate::parser::comptime::solver::ComptimeSolver;
use crate::parser::lexer::tokenizer;
use crate::utils::cycle_detector::CycleDetector;
use onion_vm::types::lambda::vm_instructions::instruction_set::VMInstructionPackage;
use onion_vm::types::lambda::vm_instructions::ir::DebugInfo;
use onion_vm::types::lambda::vm_instructions::ir::Functions;
use onion_vm::types::lambda::vm_instructions::ir::IR;
use onion_vm::types::lambda::vm_instructions::ir::IRPackage;
use onion_vm::types::lambda::vm_instructions::ir_translator::IRTranslator;

/// 编译阶段诊断信息。
///
/// 封装编译流程中可能出现的 IO、借用等错误。
#[derive(Debug, Clone)]
pub enum CompileDiagnostic {
    /// IO 错误（如文件读取失败）
    IOError(String),
    /// 多线程借用错误（如锁定失败）
    BorrowError(String),
}

impl Diagnostic for CompileDiagnostic {
    fn severity(&self) -> crate::diagnostics::ReportSeverity {
        match self {
            CompileDiagnostic::IOError(_) => crate::diagnostics::ReportSeverity::Error,
            CompileDiagnostic::BorrowError(_) => crate::diagnostics::ReportSeverity::Error,
        }
    }

    fn title(&self) -> String {
        "Compile Error".to_string()
    }

    fn message(&self) -> String {
        match self {
            CompileDiagnostic::IOError(msg) => format!("I/O Error: {}", msg),
            CompileDiagnostic::BorrowError(msg) => format!("Borrow Error: {}", msg),
        }
    }

    fn location(&self) -> Option<crate::diagnostics::SourceLocation> {
        None
    }

    fn help(&self) -> Option<String> {
        None
    }

    fn copy(&self) -> Box<dyn Diagnostic> {
        Box::new(self.clone())
    }
}

/// 主编译入口：从源码生成中间表示（IRPackage）。
///
/// # 步骤说明
/// 1. 词法分析并过滤注释
/// 2. 语法分析生成 AST
/// 3. 检查导入循环（CycleDetector）
/// 4. 编译时求值（ComptimeSolver）
/// 5. 自动变量捕获与语义分析
/// 6. 生成 IR 并追加主返回指令
/// 7. 构建最终 IRPackage
///
/// # 错误处理
/// - 词法/语法/语义/编译时错误均通过 DiagnosticCollector 收集
/// - IO 错误、借用错误通过 CompileDiagnostic 上报
///
/// # 参数
/// - `collector`：诊断信息收集器
/// - `source`：源码对象
///
/// # 返回
/// - `Ok(IRPackage)`：编译成功
/// - `Err(())`：编译失败，错误已收集
pub fn build_code(collector: &mut DiagnosticCollector, source: &Source) -> Result<IRPackage, ()> {
    let tokens = tokenizer::tokenize(&source);
    let tokens = tokenizer::reject_comment(&tokens);
    let gathered = ast_token_stream::from_stream(&tokens);
    let ast = build_ast(collector, gathered)?;

    // 检查导入循环，初始化 CycleDetector
    let no_path = PathBuf::from(".");
    let import_cycle_detector = CycleDetector::new()
        .enter(
            fs::canonicalize(source.file_path().unwrap_or(&no_path))
                .map_err(|e| collector.report(CompileDiagnostic::IOError(e.to_string())))?,
        )
        .expect("unreachable!");
    let mut comptime_solver =
        ComptimeSolver::new(Arc::new(RwLock::new(HashMap::new())), import_cycle_detector);
    let solve_result = comptime_solver.solve(&ast);
    match comptime_solver.diagnostics().read() {
        Ok(diagnostics) => {
            for diag in diagnostics.diagnostics() {
                collector.report(diag.copy());
            }
        }
        Err(_) => {
            return collector.fatal(CompileDiagnostic::BorrowError(
                "Failed to read diagnostics".to_string(),
            ));
        }
    }

    let ast = solve_result?;

    let (_required_vars, ast) = auto_capture_and_rebuild(&ast);

    let _analyze_result = analyze_ast(&ast, collector, &None)?;

    let namespace = ir_generator::NameSpace::new("Main".to_string(), None);
    let mut functions = Functions::new();
    let mut ir_generator = ir_generator::IRGenerator::new(&mut functions, namespace);

    let ir = ir_generator.generate(collector, &ast)?;

    let mut ir = ir;
    ir.push((DebugInfo::new((0, 0)), IR::Return));
    functions.append("__main__".to_string(), ir);

    Ok(functions.build_instructions(Some(source.content_str())))
}

/// IRPackage 编译为虚拟机字节码。
///
/// 将中间表示（IRPackage）翻译为 Onion 虚拟机可执行的指令包。
///
/// # 参数
/// - `package`：IRPackage
///
/// # 返回
/// - `Ok(VMInstructionPackage)`：翻译成功
/// - `Err(String)`：翻译失败，返回错误信息
pub fn compile_to_bytecode(package: &IRPackage) -> Result<VMInstructionPackage, String> {
    let mut translator = IRTranslator::new(package);
    match translator.translate() {
        Ok(_) => Ok(translator.get_result()),
        Err(e) => Err(format!("IR translation failed: {:?}", e)),
    }
}
