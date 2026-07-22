//! rustyline-backed interactive REPL for the CLI (`mutsu` with no script).
//!
//! The line-accumulation / value-display semantics live in
//! [`crate::repl_core`] so the WASM playground shares them verbatim.

use crate::Interpreter;
use crate::repl_core::{LineResult, process_line};
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;

pub fn run_repl() {
    let mut rl = match DefaultEditor::new() {
        Ok(editor) => editor,
        Err(err) => {
            eprintln!("Failed to initialize line editor: {}", err);
            std::process::exit(1);
        }
    };

    let history_path = dirs_path();
    if let Some(ref path) = history_path {
        let _ = rl.load_history(path);
    }

    let mut interpreter = Interpreter::new();
    interpreter.set_program_path("<repl>");
    interpreter.set_immediate_stdout(true);
    let mut accumulated = String::new();

    loop {
        let prompt = if accumulated.is_empty() { "> " } else { "* " };

        match rl.readline(prompt) {
            Ok(line) => {
                let _ = rl.add_history_entry(&line);
                let (result, display) = process_line(&mut interpreter, &mut accumulated, &line);
                if let Some(text) = display {
                    print!("{}", text);
                }
                if matches!(result, LineResult::Continue) {
                    continue;
                }
            }
            Err(ReadlineError::Interrupted) => {
                // Ctrl-C: cancel current input
                accumulated.clear();
                continue;
            }
            Err(ReadlineError::Eof) => {
                // Ctrl-D: exit
                break;
            }
            Err(err) => {
                eprintln!("Error: {}", err);
                break;
            }
        }
    }

    if let Some(ref path) = history_path {
        let _ = rl.save_history(path);
    }
}

fn dirs_path() -> Option<std::path::PathBuf> {
    let home = std::env::var("HOME").ok()?;
    let dir = std::path::PathBuf::from(home).join(".mutsu");
    let _ = std::fs::create_dir_all(&dir);
    Some(dir.join("history"))
}
