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

/// Result of processing a single REPL line.
enum LineResult {
    /// Need more input (incomplete expression).
    Continue,
    /// Line was processed (output may have been produced).
    Done,
}

/// Process a single line of REPL input. Returns the display string (if any)
/// and whether more input is needed.
///
/// This function is the testable core of the REPL loop — it has no I/O
/// dependencies beyond the `Interpreter`.
fn process_line(
    interpreter: &mut Interpreter,
    accumulated: &mut String,
    line: &str,
) -> (LineResult, Option<String>) {
    if accumulated.is_empty() {
        *accumulated = line.to_string();
    } else {
        accumulated.push('\n');
        accumulated.push_str(line);
    }

    if is_incomplete(accumulated) {
        return (LineResult::Continue, None);
    }

    // Skip empty or whitespace-only input
    if accumulated.trim().is_empty() {
        accumulated.clear();
        return (LineResult::Done, None);
    }

    interpreter.clear_output();
    let display = match interpreter.run(accumulated) {
        Ok(_) => {
            let output = interpreter.output().to_string();
            let display = if !output.is_empty() {
                Some(output)
            } else if let Some(value) = interpreter.last_value.take()
                && !matches!(value, Value::Nil)
            {
                let text = if let Some(Ok(gist)) = native_method_0arg(&value, "gist") {
                    format!("{}\n", gist.to_string_value())
                } else {
                    format!("{}\n", value.to_string_value())
                };
                Some(text)
            } else {
                None
            };
            interpreter.clear_output();
            display
        }
        Err(err) => {
            // If it looks like incomplete input (parse error), try accumulating
            if err.code.is_some_and(|c| c.is_parse()) && is_incomplete(accumulated) {
                return (LineResult::Continue, None);
            }
            Some(format!("Error: {}\n", err.message))
        }
    };

    accumulated.clear();
    (LineResult::Done, display)
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

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: feed lines into the REPL core and collect all display output.
    fn repl_session(lines: &[&str]) -> Vec<String> {
        let mut interpreter = Interpreter::new();
        interpreter.set_program_path("<repl-test>");
        let mut accumulated = String::new();
        let mut outputs = Vec::new();

        for line in lines {
            let (_result, display) = process_line(&mut interpreter, &mut accumulated, line);
            if let Some(text) = display {
                outputs.push(text);
            }
        }
        outputs
    }

    #[test]
    fn test_say_prints_once() {
        let out = repl_session(&["say 'hello'"]);
        assert_eq!(out, vec!["hello\n"]);
    }

    #[test]
    fn test_say_does_not_repeat_on_next_input() {
        let out = repl_session(&["say 'hello'", "3"]);
        assert_eq!(out, vec!["hello\n", "3\n"]);
    }

    #[test]
    fn test_empty_line_produces_no_output() {
        let out = repl_session(&["say 'hello'", "", "say 'world'"]);
        assert_eq!(out, vec!["hello\n", "world\n"]);
    }

    #[test]
    fn test_expression_shows_value() {
        let out = repl_session(&["1 + 2"]);
        assert_eq!(out, vec!["3\n"]);
    }

    #[test]
    fn test_variable_persists_across_lines() {
        let out = repl_session(&["my $x = 42", "say $x + 1"]);
        // Variable declared in one line is accessible in the next
        assert!(out.iter().any(|s| s.contains("43")));
    }

    #[test]
    fn test_multiline_block() {
        let out = repl_session(&["if True {", "  say 'yes'", "}"]);
        assert_eq!(out, vec!["yes\n"]);
    }

    #[test]
    fn test_whitespace_only_line_ignored() {
        let out = repl_session(&["   ", "  \t  "]);
        assert!(out.is_empty());
    }
}
