use std::collections::HashMap;
use std::fs;

use crate::ir_generator::ir_generator;
use crate::parser::analyzer::analyze_ast;
use crate::parser::analyzer::auto_capture_and_rebuild;
use crate::parser::ast::ast_token_stream;
use crate::parser::ast::build_ast;
use crate::parser::comptime::solver::ComptimeSolver;
use crate::parser::lexer::lexer;
use crate::utils::cycle_detector::CycleDetector;
use colored::Colorize;
use onion_vm::types::lambda::vm_instructions::instruction_set::VMInstructionPackage;
use onion_vm::types::lambda::vm_instructions::ir::DebugInfo;
use onion_vm::types::lambda::vm_instructions::ir::Functions;
use onion_vm::types::lambda::vm_instructions::ir::IR;
use onion_vm::types::lambda::vm_instructions::ir::IRPackage;
use onion_vm::types::lambda::vm_instructions::ir_translator::IRTranslator;

// Compile code and generate intermediate representation
pub fn build_code(code: &str) -> Result<IRPackage, String> {
    let tokens = lexer::tokenize(code);
    let tokens = lexer::reject_comment(&tokens);
    let gathered = ast_token_stream::from_stream(&tokens);
    let ast = match build_ast(gathered) {
        Ok(ast) => ast,
        Err(err_token) => {
            return Err(err_token.format().to_string());
        }
    };

    // 使用当前路径进入
    let import_cycle_detector = CycleDetector::new()
        .enter(fs::canonicalize(".").map_err(|e| e.to_string())?)
        .expect("unreachable!");
    let mut comptime_solver = ComptimeSolver::new(HashMap::new(), import_cycle_detector);
    let solve_result = comptime_solver.solve(&ast);
    let ast = match solve_result {
        Ok(ast) => ast,
        Err(_) => {
            return Err(format!(
                "Error: {:?}\n\nWarning: {:?}",
                comptime_solver.errors(),
                comptime_solver.warnings()
            ));
        }
    };

    let (_required_vars, ast) = auto_capture_and_rebuild(&ast);

    let analyse_result = analyze_ast(&ast, None);

    let mut errors = "".to_string();
    for error in &analyse_result.errors {
        errors.push_str(&error.format());
        errors.push_str("\n");
    }
    if !analyse_result.errors.is_empty() {
        return Err(format!("{}AST analysis failed", errors));
    }
    for warn in &analyse_result.warnings {
        println!("{}", warn.format().bright_yellow());
    }

    let namespace = ir_generator::NameSpace::new("Main".to_string(), None);
    let mut functions = Functions::new();
    let mut ir_generator = ir_generator::IRGenerator::new(&mut functions, namespace);

    let ir = match ir_generator.generate(&ast) {
        Ok(ir) => ir,
        Err(err) => {
            return Err(format!("Error: {:?}", err));
        }
    };

    let mut ir = ir;
    ir.push((DebugInfo::new((0, 0)), IR::Return));
    functions.append("__main__".to_string(), ir);

    Ok(functions.build_instructions(Some(code.to_string())))
}

// Compile IR to bytecode
pub fn compile_to_bytecode(package: &IRPackage) -> Result<VMInstructionPackage, String> {
    let mut translator = IRTranslator::new(package);
    match translator.translate() {
        Ok(_) => Ok(translator.get_result()),
        Err(e) => Err(format!("IR translation failed: {:?}", e)),
    }
}
