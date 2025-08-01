use clap::{Parser, Subcommand};
use colored::*;
use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

// Import necessary modules
use onion_frontend::{
    compile::{build_code, compile_to_bytecode},
    utils::cycle_detector::{self, CycleDetector},
};
use onion_vm::{
    GC,
    lambda::{
        runnable::{Runnable, RuntimeError, StepResult},
        scheduler::scheduler::Scheduler,
    },
    onion_tuple,
    types::{
        lambda::{
            definition::{LambdaBody, LambdaType, OnionLambdaDefinition},
            launcher::OnionLambdaRunnableLauncher,
            vm_instructions::{
                instruction_set::VMInstructionPackage, ir::IRPackage, ir_translator::IRTranslator,
            },
        },
        object::OnionObject,
        tuple::OnionTuple,
    },
    unwrap_object,
    utils::fastmap::OnionFastMap,
};

mod lsp;
mod repl;
mod stdlib;

/// A modern CLI for the Onion programming language
#[derive(Parser)]
#[command(name = "onion-lang")]
#[command(about = "The Onion Programming Language CLI", long_about = None)]
#[command(version = env!("CARGO_PKG_VERSION"))]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile source files to IR or bytecode
    Compile {
        /// Path to the source file
        #[arg(value_name = "FILE")]
        file: PathBuf,
        /// Output file path
        #[arg(short, long, value_name = "OUTPUT")]
        output: Option<PathBuf>,
        /// Compile to bytecode instead of IR
        #[arg(short, long)]
        bytecode: bool,
    },
    /// Run source files, IR files, or bytecode
    Run {
        /// Path to the file to run
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },
    /// Display formatted IR from source files
    DisplayIr {
        /// Path to the source file
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },
    /// Translate IR files to bytecode
    Translate {
        /// Path to the IR file
        #[arg(value_name = "IR_FILE")]
        file: PathBuf,
        /// Output bytecode file path
        #[arg(short, long, value_name = "OUTPUT")]
        output: Option<PathBuf>,
    },
    /// Start an interactive REPL
    Repl,
    /// Start the Language Server Protocol (LSP) server
    Lsp {
        /// Port to run the REPL on
        #[arg(short, long, default_value = "4000", value_name = "PORT")]
        port: u16,
    },
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Compile {
            file,
            output,
            bytecode,
        } => cmd_compile(file, output, bytecode),
        Commands::Run { file } => cmd_run(file),
        Commands::DisplayIr { file } => cmd_display_ir(file),
        Commands::Translate { file, output } => cmd_translate(file, output),
        Commands::Repl => cmd_repl(),
        Commands::Lsp { port } => {
            lsp::start_lsp_server(port).map_err(|e| format!("Failed to start LSP server: {}", e))
        }
    };

    if let Err(e) = result {
        eprintln!("{} {}", "Error:".red().bold(), e);
        std::process::exit(1);
    }
}

fn cmd_compile(file: PathBuf, output: Option<PathBuf>, bytecode: bool) -> Result<(), String> {
    println!("{} {}", "Compiling".green().bold(), file.display());

    if !file.exists() {
        return Err(format!("File '{}' not found", file.display()));
    }
    let code = std::fs::read_to_string(&file)
        .map_err(|e| format!("Failed to read file '{}': {}", file.display(), e))?;

    // Store current working directory BEFORE dir_stack changes it
    let current_dir =
        std::env::current_dir().map_err(|e| format!("Failed to get current directory: {}", e))?;

    let mut dir_stack = create_dir_stack(file.parent())?;
    let absolute_path = dir_stack
        .translate(&file)
        .map_err(|e| format!("Failed to get absolute path: {}", e))?;
    let mut cycle_detector = CycleDetector::new();
    let mut visit_result = cycle_detector
        .visit(
            absolute_path
                .to_str()
                .ok_or("Invalid file path")?
                .to_string(),
        )
        .map_err(|e| format!("Cycle detection failed: {}", e))?;
    if bytecode {
        // Compile to IR first, then to bytecode
        let ir_package = build_code(&code, visit_result.get_detector_mut(), &mut dir_stack)
            .map_err(|e| format!("Compilation failed\n{}", e))?;
        let bytecode_package = compile_to_bytecode(&ir_package)
            .map_err(|e| format!("Bytecode compilation failed: {}", e))?;
        // Restore working directory before writing output
        std::env::set_current_dir(&current_dir)
            .map_err(|e| format!("Failed to restore current directory: {}", e))?;
        let output_path = output.unwrap_or_else(|| file.with_extension("onionc"));

        // Convert relative path to absolute using the original working directory
        let abs_path = if output_path.is_absolute() {
            output_path.clone()
        } else {
            current_dir.join(&output_path)
        };

        match bytecode_package.write_to_file(abs_path.to_str().unwrap()) {
            Ok(_) => {
                println!(
                    "{} {}",
                    "Generated bytecode:".green(),
                    output_path.display()
                );
            }
            Err(e) => {
                return Err(format!(
                    "Failed to write bytecode to '{}': {:?}",
                    abs_path.display(),
                    e
                ));
            }
        }
    } else {
        // Compile to IR
        let ir_package = build_code(&code, visit_result.get_detector_mut(), &mut dir_stack)
            .map_err(|e| format!("Compilation failed\n{}", e))?;

        // Restore working directory before writing output
        std::env::set_current_dir(&current_dir)
            .map_err(|e| format!("Failed to restore current directory: {}", e))?;

        let output_path = output.unwrap_or_else(|| file.with_extension("onionr"));
        ir_package
            .write_to_file(output_path.to_str().unwrap())
            .map_err(|e| format!("Failed to write IR: {:?}", e))?;

        println!("{} {}", "Generated IR:".green(), output_path.display());
    }

    Ok(())
}

fn cmd_run(file: PathBuf) -> Result<(), String> {
    // println!("{} {}", "Running".green().bold(), file.display());

    if !file.exists() {
        return Err(format!("File '{}' not found", file.display()));
    }

    let extension = file.extension().and_then(|s| s.to_str()).unwrap_or("");
    match extension {
        "onionc" => run_bytecode_file(&file),
        "onionr" => run_ir_file(&file),
        _ => run_source_file(&file),
    }
}

fn cmd_display_ir(file: PathBuf) -> Result<(), String> {
    println!("{} {}", "Displaying IR for".cyan().bold(), file.display());

    if !file.exists() {
        return Err(format!("File '{}' not found", file.display()));
    }

    let code = std::fs::read_to_string(&file)
        .map_err(|e| format!("Failed to read file '{}': {}", file.display(), e))?;

    let mut dir_stack = create_dir_stack(file.parent())?;

    let absolute_path = dir_stack
        .translate(&file)
        .map_err(|e| format!("Failed to get absolute path: {}", e))?;
    let mut cycle_detector = CycleDetector::new();
    let mut visit_result = cycle_detector
        .visit(
            absolute_path
                .to_str()
                .ok_or("Invalid file path")?
                .to_string(),
        )
        .map_err(|e| format!("Cycle detection failed: {}", e))?;
    let ir_package = build_code(&code, visit_result.get_detector_mut(), &mut dir_stack)
        .map_err(|e| format!("Compilation failed\n{}", e))?;

    println!("\n{}", "IR Code:".cyan().bold());
    println!("{}", format!("{:#?}", ir_package).dimmed());

    Ok(())
}

fn cmd_translate(file: PathBuf, output: Option<PathBuf>) -> Result<(), String> {
    println!("{} {}", "Translating".yellow().bold(), file.display());

    if !file.exists() {
        return Err(format!("File '{}' not found", file.display()));
    }

    let ir_package = IRPackage::read_from_file(file.to_str().unwrap())
        .map_err(|e| format!("Failed to read IR file: {:?}", e))?;
    let bytecode_package =
        compile_to_bytecode(&ir_package).map_err(|e| format!("Translation failed: {}", e))?;

    let output_path = output.unwrap_or_else(|| file.with_extension("onionc"));

    bytecode_package
        .write_to_file(output_path.to_str().unwrap())
        .map_err(|e| format!("Failed to write bytecode: {:?}", e))?;

    println!(
        "{} {}",
        "Generated bytecode:".green(),
        output_path.display()
    );
    Ok(())
}

fn cmd_repl() -> Result<(), String> {
    repl::start_repl().map_err(|e| format!("REPL error: {:?}", e))
}

// Helper functions

fn run_source_file(file: &Path) -> Result<(), String> {
    let code = std::fs::read_to_string(file)
        .map_err(|e| format!("Failed to read file '{}': {}", file.display(), e))?;
    let absolute_path = file
        .canonicalize()
        .map_err(|e| format!("Failed to get absolute path: {}", e))?;
    let mut dir_stack = create_dir_stack(absolute_path.parent())?;
    let mut cycle_detector = CycleDetector::new();
    let mut visit_result = cycle_detector
        .visit(
            absolute_path
                .to_str()
                .ok_or("Invalid file path")?
                .to_string(),
        )
        .map_err(|e| format!("Cycle detection failed: {}", e))?;
    execute_code(&code, visit_result.get_detector_mut(), &mut dir_stack)
}

fn run_ir_file(file: &Path) -> Result<(), String> {
    let ir_package = IRPackage::read_from_file(file.to_str().unwrap())
        .map_err(|e| format!("Failed to read IR file: {:?}", e))?;

    execute_ir_package(&ir_package)
}

fn run_bytecode_file(file: &Path) -> Result<(), String> {
    let bytecode_package = VMInstructionPackage::read_from_file(file.to_str().unwrap())
        .map_err(|e| format!("Failed to read bytecode file: {:?}", e))?;

    execute_bytecode_package(&bytecode_package)
}

fn create_dir_stack(
    base_dir: Option<&Path>,
) -> Result<onion_frontend::dir_stack::DirectoryStack, String> {
    let dir = base_dir.unwrap_or_else(|| Path::new(".")).to_path_buf();
    onion_frontend::dir_stack::DirectoryStack::new(Some(&dir))
        .map_err(|e| format!("Failed to initialize directory stack: {}", e))
}

fn execute_code(
    code: &str,
    cycle_detector: &mut cycle_detector::CycleDetector<String>,
    dir_stack: &mut onion_frontend::dir_stack::DirectoryStack,
) -> Result<(), String> {
    let ir_package = build_code(code, cycle_detector, dir_stack)
        .map_err(|e| format!("Compilation failed\n{}", e))?;

    execute_ir_package(&ir_package)
}

fn execute_ir_package(ir_package: &IRPackage) -> Result<(), String> {
    let mut translator = IRTranslator::new(ir_package);
    translator
        .translate()
        .map_err(|e| format!("IR translation failed: {:?}", e))?;

    let vm_instructions_package = translator.get_result();
    execute_bytecode_package(&vm_instructions_package)
}

fn execute_bytecode_package(vm_instructions_package: &VMInstructionPackage) -> Result<(), String> {
    let mut gc = GC::new_with_memory_threshold(1024 * 1024); // 1 MB threshold

    match VMInstructionPackage::validate(vm_instructions_package) {
        Err(e) => return Err(format!("Invalid VM instruction package: {}", e)),
        Ok(_) => {}
    }
    // Create standard library object
    let stdlib = stdlib::build_module();
    let mut capture = OnionFastMap::new(vm_instructions_package.create_key_pool());

    // 尝试将标准库对象添加到捕获变量中（当字符串池没有stdlib时会自动忽略）
    capture.push(&"stdlib".to_string(), stdlib.weak().clone());
    // Create Lambda definition
    let lambda = OnionLambdaDefinition::new_static(
        &onion_tuple!(),
        LambdaBody::Instruction(Arc::new(vm_instructions_package.clone())),
        &capture,
        "__main__".to_string(),
        LambdaType::Normal,
    );

    let args = OnionTuple::new_static(vec![]);

    let mut scheduler: Box<dyn Runnable> = Box::new(Scheduler::new(vec![Box::new(
        OnionLambdaRunnableLauncher::new_static(lambda.weak(), args, |r| Ok(r))
            .map_err(|e| format!("Failed to create runnable Lambda: {:?}", e))?,
    )]));
    // Execute code
    loop {
        #[cfg(debug_assertions)]
        gc.collect();
        match scheduler.step(&mut gc) {
            StepResult::Continue => {
                // Continue to next step
            }
            StepResult::SpawnRunnable(_) => {
                return Err("Cannot spawn async task in sync context".to_string());
            }
            StepResult::Error(ref error) => {
                if let RuntimeError::Pending = error {
                    // Pending 状态，正常继续
                    continue;
                }
                eprintln!("\n{}", "--- Runtime Error Occurred ---".red().bold());
                eprintln!("An unrecoverable error was caught at the top level.");

                eprintln!("\n{}", "Error Details:".yellow().underline());
                eprintln!("{}", error);

                eprintln!(
                    "\n{}",
                    "Full Execution Context at Time of Crash:"
                        .yellow()
                        .underline()
                );
                eprintln!("{}", scheduler.format_context());

                return Err("Execution failed. See details above.".to_string());
            }
            StepResult::NewRunnable(_) => {
                unreachable!()
            }
            StepResult::ReplaceRunnable(_) => {
                unreachable!()
            }
            StepResult::Return(ref result) => {
                let result_borrowed = result.weak();
                let result = unwrap_object!(result_borrowed, OnionObject::Pair)
                    .map_err(|e| format!("Failed to unwrap result: {:?}", e))?;
                let success = *unwrap_object!(result.get_key(), OnionObject::Boolean)
                    .map_err(|e| format!("Failed to get success key: {:?}", e))?;
                if !success {
                    // 这是程序逻辑上的失败（例如，断言失败），而不是 VM 崩溃
                    // 我们也可以在这里利用 format_context
                    eprintln!(
                        "{} {}",
                        "Execution returned a failure value:".red().bold(),
                        result
                            .get_value()
                            .to_string(&vec![])
                            .map_err(|e| { format!("Failed to get error message: {:?}", e) })?
                    );

                    // 打印上下文以帮助调试为什么会返回失败
                    eprintln!(
                        "\n{}",
                        "Context at Time of Failure Return:".yellow().underline()
                    );
                    eprintln!("{}", scheduler.format_context());

                    return Err("Execution failed with a returned error value.".to_string());
                }
                let do_not_print = result
                    .get_value()
                    .with_data(|data| match data {
                        OnionObject::Undefined(None) => Ok(true),
                        OnionObject::Tuple(tuple) => Ok(tuple.get_elements().is_empty()),
                        _ => Ok(false),
                    })
                    .map_err(|e| format!("Failed to check if result is undefined: {:?}", e))?;
                if do_not_print {
                    return Ok(());
                }
                // Print result and exit
                println!(
                    "{}",
                    result
                        .get_value()
                        .to_string(&vec![])
                        .map_err(|e| format!("Failed to get result value: {:?}", e))?
                );
                break;
            }
        }
    }
    Ok(())
}
