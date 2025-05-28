use colored::*;
use rustyline::error::ReadlineError;
use rustyline::{DefaultEditor, Result};

use crate::{execute_code, create_dir_stack};

pub fn start_repl() -> Result<()> {
    println!("{}", "Onion Language REPL v0.1.0".cyan().bold());
    println!("{}", "Type 'exit' or press Ctrl+C to quit".dimmed());
    println!();    let mut rl = DefaultEditor::new()?;

    // Load history from file
    let history_file = get_history_file();
    if let Some(ref path) = history_file {
        let _ = rl.load_history(path);
    }

    let mut line_number = 1;

    loop {
        let prompt = format!("{}[{}] ", "onion".cyan().bold(), line_number);
        
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
                
                rl.add_history_entry(&line)?;
                
                // Execute the code
                let mut dir_stack = match create_dir_stack(None) {
                    Ok(stack) => stack,
                    Err(e) => {
                        eprintln!("{} {}", "error:".red().bold(), e);
                        continue;
                    }
                };
                
                match execute_code(&line, &mut dir_stack) {
                    Ok(_) => {},
                    Err(e) => {
                        eprintln!("{} {}", "error:".red().bold(), e);
                    }
                }
                
                line_number += 1;
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
    println!();
    println!("{}", "Language Features:".cyan().bold());
    println!("  - Variables: {} or {}", "let x = 5".green(), "x := 5".green());
    println!("  - Functions: {}", "fn add(a, b) { a + b }".green());
    println!("  - Standard library: {}", "@required stdlib".green());
    println!();
}

fn get_history_file() -> Option<std::path::PathBuf> {
    dirs::config_dir().map(|mut path| {
        path.push("onion-lang");
        std::fs::create_dir_all(&path).ok()?;
        path.push("repl_history.txt");
        Some(path)
    }).flatten()
}
