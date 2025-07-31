use crate::dir_stack::DirectoryStack;
use crate::ir_generator::ir_generator;
use crate::parser::analyzer::analyze_ast;
use crate::parser::analyzer::auto_capture_and_rebuild;
use crate::parser::analyzer::expand_macro;
use crate::parser::ast::ast_token_stream;
use crate::parser::ast::build_ast;
use crate::parser::lexer::lexer;
use crate::utils::cycle_detector::CycleDetector;
use colored::Colorize;
use onion_vm::types::lambda::vm_instructions::instruction_set::VMInstructionPackage;
use onion_vm::types::lambda::vm_instructions::ir::DebugInfo;
use onion_vm::types::lambda::vm_instructions::ir::Functions;
use onion_vm::types::lambda::vm_instructions::ir::IRPackage;
use onion_vm::types::lambda::vm_instructions::ir::IR;
use onion_vm::types::lambda::vm_instructions::ir_translator::IRTranslator;

// Compile code and generate intermediate representation
pub fn build_code(
    code: &str,
    cycle_detector: &mut CycleDetector<String>,
    dir_stack: &mut DirectoryStack,
) -> Result<IRPackage, String> {
    let tokens = lexer::tokenize(code);
    let tokens = lexer::reject_comment(&tokens);
    let gathered = ast_token_stream::from_stream(&tokens);
    let ast = match build_ast(gathered) {
        Ok(ast) => ast,
        Err(err_token) => {
            return Err(err_token.format().to_string());
        }
    };
    let macro_result = expand_macro(&ast, cycle_detector, dir_stack);

    let mut errors = "".to_string();
    for error in &macro_result.errors {
        //println!("{}", error.format(code.to_string()).bright_red());
        errors.push_str(&error.format());
        errors.push_str("\n");
    }
    if !macro_result.errors.is_empty() {
        return Err(format!("{}AST analysis failed", errors));
    }
    for warn in &macro_result.warnings {
        println!("{}", warn.format().bright_yellow());
    }

    let (_required_vars, ast) = auto_capture_and_rebuild(&macro_result.result_node);
    
    let analyse_result = analyze_ast(&ast, None, cycle_detector, dir_stack);

    let mut errors = "".to_string();
    for error in &analyse_result.errors {
        //println!("{}", error.format(code.to_string()).bright_red());
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
