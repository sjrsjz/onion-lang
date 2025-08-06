use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::RwLock;

use crate::diagnostics::Diagnostic;
use crate::diagnostics::collector::DiagnosticCollector;
use crate::ir_generator::ir_generator;
use crate::parser::analyzer::analyze_ast;
use crate::parser::analyzer::auto_capture_and_rebuild;
use crate::parser::ast::ast_token_stream;
use crate::parser::ast::build_ast;
use crate::parser::comptime::solver::ComptimeSolver;
use crate::parser::lexer::tokenizer;
use crate::parser::Source;
use crate::utils::cycle_detector::CycleDetector;
use onion_vm::types::lambda::vm_instructions::instruction_set::VMInstructionPackage;
use onion_vm::types::lambda::vm_instructions::ir::DebugInfo;
use onion_vm::types::lambda::vm_instructions::ir::Functions;
use onion_vm::types::lambda::vm_instructions::ir::IR;
use onion_vm::types::lambda::vm_instructions::ir::IRPackage;
use onion_vm::types::lambda::vm_instructions::ir_translator::IRTranslator;

#[derive(Debug, Clone)]
pub enum CompileDiagnostic {
    IOError(String),
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

// Compile code and generate intermediate representation
pub fn build_code(collector: &mut DiagnosticCollector, source: &Source) -> Result<IRPackage, ()> {
    let tokens = tokenizer::tokenize(&source);
    let tokens = tokenizer::reject_comment(&tokens);
    let gathered = ast_token_stream::from_stream(&tokens);
    let ast = build_ast(collector, gathered)?;

    // 使用当前路径进入
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

    let _analyse_result = analyze_ast(&ast, collector, &None)?;

    let namespace = ir_generator::NameSpace::new("Main".to_string(), None);
    let mut functions = Functions::new();
    let mut ir_generator = ir_generator::IRGenerator::new(&mut functions, namespace);

    let ir = ir_generator.generate(collector, &ast)?;

    let mut ir = ir;
    ir.push((DebugInfo::new((0, 0)), IR::Return));
    functions.append("__main__".to_string(), ir);

    Ok(functions.build_instructions(Some(source.content_str())))
}

// Compile IR to bytecode
pub fn compile_to_bytecode(package: &IRPackage) -> Result<VMInstructionPackage, String> {
    let mut translator = IRTranslator::new(package);
    match translator.translate() {
        Ok(_) => Ok(translator.get_result()),
        Err(e) => Err(format!("IR translation failed: {:?}", e)),
    }
}
