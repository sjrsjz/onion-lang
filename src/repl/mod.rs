pub mod executor;

pub use executor::ReplExecutor;

use colored::*;
use onion_frontend::utils::cycle_detector;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

use crate::create_dir_stack;

pub fn start_repl() -> Result<()> {
    println!("{}", "Onion Language REPL v0.1.0".cyan().bold());
    println!("{}", "Type 'exit' or press Ctrl+C to quit".dimmed());
    println!();

    let mut rl = DefaultEditor::new()?;

    // Load history from file
    let history_file = get_history_file();
    if let Some(ref path) = history_file {
        let _ = rl.load_history(path);
    }

    let mut repl_executor = ReplExecutor::new();

    loop {
        let prompt = format!(
            "{}[{}]:= ",
            "Out".cyan().bold(),
            repl_executor.history_count()
        );

        match rl.readline(&prompt) {
            Ok(line) => {
                let trimmed = line.trim();

                if trimmed.is_empty() {
                    continue;
                }

                if trimmed == "exit" || trimmed == "quit" {
                    break;
                }

                if trimmed == "help" {
                    print_repl_help();
                    continue;
                }

                if trimmed == "clear" {
                    print!("\x1B[2J\x1B[1;1H");
                    continue;
                }

                if trimmed == "history" {
                    // 显示历史记录数量
                    let count = repl_executor.history_count();
                    println!("{} {} results", "History:".cyan().bold(), count);
                    continue;
                }

                if trimmed == "clear_history" {
                    // 清空历史记录
                    repl_executor.clear_history();
                    println!("{}", "History cleared".green());
                    continue;
                }

                rl.add_history_entry(&line)?;

                // Execute the code using REPL executor
                let mut dir_stack = match create_dir_stack(None) {
                    Ok(stack) => stack,
                    Err(e) => {
                        eprintln!("{} {}", "error:".red().bold(), e);
                        continue;
                    }
                };

                let mut cycle_detector = cycle_detector::CycleDetector::new();

                match repl_executor.execute_code(&line, &mut cycle_detector, &mut dir_stack) {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("{} {}", "error:".red().bold(), e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("{}", "Interrupted".yellow());
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("{}", "EOF".yellow());
                break;
            }
            Err(err) => {
                eprintln!("{} {:?}", "error:".red().bold(), err);
                break;
            }
        }
    }

    // Save history to file
    if let Some(ref path) = history_file {
        let _ = rl.save_history(path);
    }

    println!("{}", "Goodbye!".cyan());
    Ok(())
}

fn print_repl_help() {
    println!("{}", "Onion REPL Commands:".cyan().bold());
    println!("  {}  - Show this help", "help".yellow());
    println!("  {}  - Clear the screen", "clear".yellow());
    println!("  {}  - Exit the REPL", "exit/quit".yellow());
    println!("  {}  - Show history count", "history".yellow());
    println!("  {}  - Show last execution result", "last".yellow());
    println!("  {}  - Clear execution history", "clear_history".yellow());
    println!();
    println!("{}", "Language Features:".cyan().bold());
    println!(
        "  - Variables: {} or {}",
        "let x = 5".green(),
        "x := 5".green()
    );
    println!("  - Functions: {}", "fn add(a, b) { a + b }".green());
    println!("  - Standard library: {}", "@required stdlib".green());
    println!(
        "  - Output tuple: {} (contains execution results)",
        "Out".green()
    );
    println!();
    println!("{}", "Output Tuple Usage:".cyan().bold());
    println!(
        "  - Access results: {} (Out is a tuple containing all results)",
        "Out".green()
    );
    println!("  - Results are automatically added to Out after each execution");
    println!();
}

fn get_history_file() -> Option<std::path::PathBuf> {
    dirs::config_dir()
        .map(|mut path| {
            path.push("onion-lang");
            std::fs::create_dir_all(&path).ok()?;
            path.push("repl_history.txt");
            Some(path)
        })
        .flatten()
}
