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
    lambda::{
        runnable::{Runnable, RuntimeError, StepResult},
        scheduler::scheduler::Scheduler,
    },
    types::{
        lambda::{
            definition::{LambdaBody, OnionLambdaDefinition},
            launcher::OnionLambdaRunnableLauncher,
            vm_instructions::{
                instruction_set::VMInstructionPackage, ir::IRPackage, ir_translator::IRTranslator,
            },
        },
        named::OnionNamed,
        object::OnionObject,
        tuple::OnionTuple,
    },
    unwrap_object, GC,
};

mod lsp;
mod repl;
mod stdlib;

/// A modern CLI for the Onion programming language
#[derive(Parser)]
#[command(name = "onion-lang")]
#[command(about = "The Onion Programming Language CLI", long_about = None)]
#[command(version = "0.1.0")]
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
        .get_absolute_path(file.to_str().ok_or("Invalid file path")?)
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
            .map_err(|e| format!("Compilation failed: {}", e))?;
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
            .map_err(|e| format!("Compilation failed: {}", e))?;

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
        .get_absolute_path(file.to_str().ok_or("Invalid file path")?)
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
        .map_err(|e| format!("Compilation failed: {}", e))?;

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
) -> Result<onion_frontend::dir_stack::DirStack, String> {
    let dir = base_dir.unwrap_or_else(|| Path::new(".")).to_path_buf();
    onion_frontend::dir_stack::DirStack::new(Some(&dir))
        .map_err(|e| format!("Failed to initialize directory stack: {}", e))
}

fn execute_code(
    code: &str,
    cycle_detector: &mut cycle_detector::CycleDetector<String>,
    dir_stack: &mut onion_frontend::dir_stack::DirStack,
) -> Result<(), String> {
    let ir_package = build_code(code, cycle_detector, dir_stack)
        .map_err(|e| format!("Compilation failed: {}", e))?;

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
    let mut gc = GC::new();

    match VMInstructionPackage::validate(vm_instructions_package) {
        Err(e) => return Err(format!("Invalid VM instruction package: {}", e)),
        Ok(_) => {}
    }
    // Create standard library object
    let stdlib_pair = OnionNamed::new_static(
        &OnionObject::String("stdlib".to_string()).stabilize(),
        &stdlib::build_module()
            .mutablize(&mut gc)
            .map_err(|e| format!("Failed to create standard library object: {:?}", e))?,
    );

    // Create Lambda definition
    let lambda = OnionLambdaDefinition::new_static(
        &OnionTuple::new_static(vec![&stdlib_pair]),
        LambdaBody::Instruction(Arc::new(vm_instructions_package.clone())),
        None,
        None,
        "__main__".to_string(),
    );

    // let OnionObject::Lambda(lambda_ref) = &*lambda
    //     .weak()
    //     .try_borrow()
    //     .map_err(|e| format!("Failed to borrow Lambda definition: {:?}", e))?
    // else {
    //     return Err("Failed to create Lambda definition".to_string());
    // };

    let args = OnionTuple::new_static(vec![]);

    // 绑定参数
    // let assigned_argument: OnionStaticObject = lambda_ref
    //     .with_parameter(|param| {
    //         unwrap_object!(param, OnionObject::Tuple)?.clone_and_named_assignment(
    //             unwrap_object!(&*args.weak().try_borrow()?, OnionObject::Tuple)?,
    //         )
    //     })
    //     .map_err(|e| format!("Failed to assign arguments to Lambda: {:?}", e))?;

    // let lambda = lambda_ref
    //     .create_runnable(assigned_argument, &lambda, &mut GC::new())
    //     .map_err(|e| format!("Failed to create runnable Lambda: {:?}", e))?;

    // 初始化调度器和GC
    // let mut scheduler = Scheduler::new(vec![lambda]);
    let mut scheduler: Box<dyn Runnable> = Box::new(
        OnionLambdaRunnableLauncher::new_static(&lambda, &args, &|r| {
            Ok(Box::new(Scheduler::new(vec![r])))
        })
        .map_err(|e| format!("Failed to create runnable Lambda: {:?}", e))?,
    );
    // Execute code
    loop {
        match scheduler.step(&mut gc) {
            Ok(step_result) => {
                match step_result {
                    StepResult::Continue => {
                        // Continue to next step
                    }
                    StepResult::NewRunnable(_) => {
                        // Add new runnable to scheduler
                        unreachable!()
                    }
                    StepResult::ReplaceRunnable(r) => {
                        scheduler = r;
                    }
                    StepResult::Return(result) => {
                        let result_borrowed = result
                            .weak()
                            .try_borrow()
                            .map_err(|e| format!("Failed to borrow result: {:?}", e))?;
                        let result = unwrap_object!(&*result_borrowed, OnionObject::Pair)
                            .map_err(|e| format!("Failed to unwrap result: {:?}", e))?;
                        let key_borrowed = result
                            .get_key()
                            .try_borrow()
                            .map_err(|e| format!("Failed to borrow result: {:?}", e))?;
                        let success = *unwrap_object!(&*key_borrowed, OnionObject::Boolean)
                            .map_err(|e| format!("Failed to get success key: {:?}", e))?;
                        if !success {
                            let value_borrowed = result
                                .get_value()
                                .try_borrow()
                                .map_err(|e| format!("Failed to borrow result: {:?}", e))?;
                            println!(
                                "{} {}",
                                "Error:".red().bold(),
                                value_borrowed.to_string(&vec![]).map_err(|e| {
                                    format!("Failed to get error message: {:?}", e)
                                })?
                            );
                            return Err("Execution failed".to_string());
                        }
                        let is_undefined = result
                            .get_value()
                            .with_data(|data| {
                                Ok(unwrap_object!(data, OnionObject::Undefined).is_ok())
                            })
                            .map_err(|e| {
                                format!("Failed to check if result is undefined: {:?}", e)
                            })?;
                        if is_undefined {
                            return Ok(());
                        }
                        // Print result and exit
                        let result_value_borrowed = result
                            .get_value()
                            .try_borrow()
                            .map_err(|e| format!("Failed to borrow result value: {:?}", e))?;
                        println!(
                            "{}",
                            result_value_borrowed
                                .to_string(&vec![])
                                .map_err(|e| format!("Failed to get result value: {:?}", e))?
                        );
                        break;
                    }
                }
            }
            Err(e) => {
                return Err(format!("Execution error: {}", e));
            }
        }
    }
    Ok(())
}
