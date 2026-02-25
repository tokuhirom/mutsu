use crate::Interpreter;
use crate::builtins::native_method_0arg;
use crate::value::Value;
use rustyline::DefaultEditor;
use rustyline::error::ReadlineError;

/// Check if the input has unbalanced brackets, suggesting more input is needed.
fn is_incomplete(input: &str) -> bool {
    let mut depth_brace = 0i32;
    let mut depth_paren = 0i32;
    let mut depth_bracket = 0i32;
    let mut in_single_quote = false;
    let mut in_double_quote = false;
    let mut prev = '\0';

    for ch in input.chars() {
        if in_single_quote {
            if ch == '\'' && prev != '\\' {
                in_single_quote = false;
            }
            prev = ch;
            continue;
        }
        if in_double_quote {
            if ch == '"' && prev != '\\' {
                in_double_quote = false;
            }
            prev = ch;
            continue;
        }
        match ch {
            '\'' => in_single_quote = true,
            '"' => in_double_quote = true,
            '{' => depth_brace += 1,
            '}' => depth_brace -= 1,
            '(' => depth_paren += 1,
            ')' => depth_paren -= 1,
            '[' => depth_bracket += 1,
            ']' => depth_bracket -= 1,
            _ => {}
        }
        prev = ch;
    }

    depth_brace > 0 || depth_paren > 0 || depth_bracket > 0
}

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
    let mut accumulated = String::new();

    loop {
        let prompt = if accumulated.is_empty() { "> " } else { "* " };

        match rl.readline(prompt) {
            Ok(line) => {
                if accumulated.is_empty() {
                    accumulated = line;
                } else {
                    accumulated.push('\n');
                    accumulated.push_str(&line);
                }

                if is_incomplete(&accumulated) {
                    continue;
                }

                let _ = rl.add_history_entry(&accumulated);

                let output_before = interpreter.output().len();
                match interpreter.run(&accumulated) {
                    Ok(result) => {
                        let output_after = interpreter.output().len();
                        if output_after > output_before {
                            let new_output = &interpreter.output()[output_before..output_after];
                            print!("{}", new_output);
                        } else if !result.is_empty() {
                            print!("{}", result);
                        } else if let Some(value) = interpreter.last_value.take()
                            && !matches!(value, Value::Nil)
                        {
                            if let Some(Ok(gist)) = native_method_0arg(&value, "gist") {
                                println!("{}", gist.to_string_value());
                            } else {
                                println!("{}", value.to_string_value());
                            }
                        }
                    }
                    Err(err) => {
                        // If it looks like incomplete input (parse error), try accumulating
                        if err.code.is_some_and(|c| c.is_parse()) && is_incomplete(&accumulated) {
                            continue;
                        }
                        eprintln!("Error: {}", err.message);
                    }
                }

                accumulated.clear();
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
