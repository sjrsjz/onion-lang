pub mod executor;

pub use executor::ReplExecutor;

use colored::*;
use rustyline::error::ReadlineError;
use std::sync::atomic::Ordering;
use std::sync::{Arc, atomic::AtomicBool};
// Updated imports for Editor, Config, Helper, and other traits
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::{Completer, Config, Editor, Helper, Hinter, Result, Validator};
// Imports for the fields in ReplHelper
use rustyline::completion::FilenameCompleter;
use rustyline::hint::HistoryHinter;
use rustyline::validate::MatchingBracketValidator;

// Add this use statement

#[derive(Helper, Completer, Hinter, Validator)]
struct ReplHelper {
    #[rustyline(Completer)]
    completer: FilenameCompleter,
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
}

impl Highlighter for ReplHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool, // We ignore _default as we have one main prompt style
    ) -> std::borrow::Cow<'b, str> {
        // Color the entire prompt string that was passed to readline()
        std::borrow::Cow::Owned(prompt.cyan().bold().to_string())
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> std::borrow::Cow<'h, str> {
        std::borrow::Cow::Owned(hint.dimmed().italic().to_string())
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> std::borrow::Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_candidate<'c>(
        &self,
        candidate: &'c str,
        completion: rustyline::CompletionType,
    ) -> std::borrow::Cow<'c, str> {
        self.highlighter.highlight_candidate(candidate, completion)
    }

    fn highlight_char(&self, line: &str, pos: usize, forced: bool) -> bool {
        // Adhering to rustyline 14.0.0 Highlighter trait signature
        self.highlighter.highlight_char(line, pos, forced)
    }
}

pub fn start_repl() -> Result<()> {
    let version = env!("CARGO_PKG_VERSION");
    println!(
        "{}",
        format!("Onion Language REPL v{version}").cyan().bold()
    );
    println!("{}", "Type \'exit\' or press Ctrl+C to quit".dimmed());
    println!();

    // Configure rustyline editor
    let config = Config::builder()
        .history_ignore_space(true)
        .completion_type(rustyline::CompletionType::List)
        .edit_mode(rustyline::EditMode::Emacs)
        .build();

    let helper = ReplHelper {
        completer: FilenameCompleter::new(),
        highlighter: MatchingBracketHighlighter::new(),
        hinter: HistoryHinter::new(),
        validator: MatchingBracketValidator::new(),
    };
    let mut rl = Editor::with_config(config)?;
    rl.set_helper(Some(helper));

    // Load history from file
    let history_file = get_history_file();
    if let Some(ref path) = history_file {
        let _ = rl.load_history(path);
    }

    let interrupted = Arc::new(AtomicBool::new(false));
    // Clone for the Ctrl-C handler
    let handler_interrupted = interrupted.clone();
    ctrlc::set_handler(move || {
        println!(
            "{}",
            "Ctrl+C received, attempting to interrupt execution...".yellow()
        );
        handler_interrupted.store(true, Ordering::SeqCst);
    })
    .expect("Error setting Ctrl-C handler");

    // Pass a clone to the executor
    let mut repl_executor = ReplExecutor::new(interrupted.clone());

    loop {
        // Reset the interrupted flag at the beginning of each command cycle
        interrupted.store(false, Ordering::SeqCst);

        // The prompt passed to readline is now plain text.
        // The Highlighter will color it.
        let plain_prompt = format!("Out[{}]:= ", repl_executor.history_count());

        match rl.readline(&plain_prompt) {
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

                let line = "@required stdlib;\n@required Out;\n".to_string() + &line;

                match repl_executor.execute_code(&line) {
                    Ok(_) => {}
                    Err(e) => {
                        eprintln!("{} {}", "Error:".red().bold(), e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("{}", "Interrupted".yellow());
                // Ensure the flag is clear if REPL exits due to Ctrl+C during input
                interrupted.store(false, Ordering::SeqCst);
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("{}", "EOF".yellow());
                break;
            }
            Err(err) => {
                eprintln!("{} {:?}", "Error:".red().bold(), err);
                break;
            }
        }
    }

    // Save history to file
    if let Some(ref path) = history_file {
        let _ = rl.save_history(path);
    }

    println!("{}", "Exit".cyan());
    Ok(())
}

fn print_repl_help() {
    println!("{}", "Onion REPL Commands:".cyan().bold());
    println!("  {}  \t\t- Show this help", "help".yellow());
    println!("  {}  \t\t- Clear the screen", "clear".yellow());
    println!("  {}  \t\t- Exit the REPL", "exit/quit".yellow());
    println!("  {}  \t\t- Show history count", "history".yellow());
    println!(
        "  {}  \t- Clear execution history",
        "clear_history".yellow()
    );
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
    dirs::config_dir().and_then(|mut path| {
        path.push("onion-lang");
        std::fs::create_dir_all(&path).ok()?;
        path.push("repl_history.txt");
        Some(path)
    })
}
