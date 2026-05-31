use std::env;
use std::fs;
#[cfg(feature = "native")]
use std::io::IsTerminal;
use std::io::{self, Read};

use mutsu::{Interpreter, RuntimeError, Value};

/// Extract a short, human-readable summary from a verbose parse error message.
/// The parser produces messages like:
///   "Confused. parse error at line 1, column 1: expected expected statement ..."
/// We extract a cleaner version for display.
fn short_parse_message(msg: &str) -> String {
    // Strip "Confused. " prefix
    let body = msg.strip_prefix("Confused. ").unwrap_or(msg);

    // Strip " \u{2014} near: ..." suffix (em-dash followed by near)
    let body = body
        .find("\u{2014} near:")
        .map(|i| body[..i].trim_end())
        .or_else(|| body.find(" — near:").map(|i| body[..i].trim_end()))
        .unwrap_or(body);

    // Strip "parse error at line N, column M: " prefix to get the core message
    let core = if let Some(pos) = body.find(": expected ") {
        &body[pos + 2..] // skip ": " to get "expected ..."
    } else if let Some(pos) = body.find(": unparsed ") {
        &body[pos + 2..]
    } else {
        body
    };

    // Remove duplicate "expected expected" from Display wrapping
    let core = core
        .strip_prefix("expected expected ")
        .map(|rest| format!("expected {}", rest))
        .unwrap_or_else(|| core.to_string());

    // Strip internal location details like "at line N (after M stmts)"
    let core = strip_internal_location(&core);

    // Simplify long alternative lists
    simplify_expected_list(&core)
}

/// Strip internal location details like "at line N (after M stmts)" from parse messages.
fn strip_internal_location(msg: &str) -> String {
    let mut result = msg.to_string();
    while let Some(start) = result.find(" at line ") {
        let rest = &result[start + " at line ".len()..];
        let digit_end = rest
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(rest.len());
        if digit_end == 0 {
            break;
        }
        let mut end = start + " at line ".len() + digit_end;
        // Check for optional " (after N stmts)"
        let after_digits = &result[end..];
        if after_digits.starts_with(" (after ")
            && let Some(paren_close) = after_digits.find(')')
        {
            end += paren_close + 1;
        }
        result = format!("{}{}", &result[..start], &result[end..]);
    }
    result
}

/// Trim long "expected X or Y or Z or ..." lists to a reasonable length.
fn simplify_expected_list(msg: &str) -> String {
    if let Some(pos) = msg.rfind(": expected ") {
        let prefix = &msg[..pos];
        let alternatives = &msg[pos + 11..]; // after ": expected "
        let parts: Vec<&str> = alternatives.split(" or ").collect();
        if parts.len() > 5 {
            let kept: Vec<&str> = parts[..5].to_vec();
            return format!("{}: expected {} or ...", prefix, kept.join(" or "));
        }
    }
    msg.to_string()
}

/// Format a parse error with a source code snippet and caret indicator,
/// matching Raku's ===SORRY!=== format.
fn format_parse_error(err: &RuntimeError, source: &str, program_name: &str) -> String {
    let mut out = String::new();

    out.push_str(&format!(
        "===SORRY!=== Error while compiling {}\n",
        program_name
    ));

    let short_msg = short_parse_message(&err.message);
    out.push_str(&short_msg);
    out.push('\n');

    if let Some(line) = err.line {
        out.push_str(&format!("at {}:{}\n", program_name, line));

        let source_lines: Vec<&str> = source.lines().collect();
        if line >= 1 && line <= source_lines.len() {
            let src_line = source_lines[line - 1];
            let col = err.column.unwrap_or(1);
            let col_idx = col.saturating_sub(1).min(src_line.len());
            let before = &src_line[..col_idx];
            let after = &src_line[col_idx..];
            out.push_str(&format!("------>{}{}\n", before, after));
            let padding = " ".repeat(7 + col_idx);
            out.push_str(&format!("{}^", padding));
        }
    }

    if let Some(hint) = &err.hint {
        out.push('\n');
        out.push_str(hint);
    }

    out
}

fn print_error(prefix: &str, err: &RuntimeError, source: Option<&str>, program_name: Option<&str>) {
    // For runtime errors with a backtrace, use the Raku-style format:
    // message\n  in sub foo at file line N\n  in block <unit> at file line N
    if err.backtrace.is_some() && err.code.is_none() {
        eprintln!("{}", err.message);
        if let Some(ref bt) = err.backtrace {
            eprintln!("{}", bt);
        }
        return;
    }

    // For parse errors with source context, show a nice snippet
    if let (Some(code), Some(src), Some(name)) = (err.code, source, program_name)
        && code.is_parse()
        && err.line.is_some()
    {
        eprintln!("{}", format_parse_error(err, src, name));
        return;
    }

    eprintln!("{}: {}", prefix, err.message);
    if let Some(line) = err.line {
        if let Some(col) = err.column {
            eprintln!("  at line {}, column {}", line, col);
        } else {
            eprintln!("  at line {}", line);
        }
    }
    if let Some(hint) = &err.hint {
        eprintln!("  hint: {}", hint);
    }
}

fn print_help(program: &str) {
    println!("Usage: {} [OPTIONS] [FILE | -e CODE]", program);
    println!();
    println!("Options:");
    println!("  -e CODE        Evaluate CODE");
    println!("  -I PATH        Add PATH to the module search path");
    println!("  -M MODULE      use MODULE before executing program (repeatable)");
    println!("  --dump-ast     Dump the AST instead of executing");
    println!("  --doc          Render Pod documentation from the source");
    println!("  --repl         Start the interactive REPL");
    println!("  --no-precomp   Disable module precompilation cache");
    println!("  -h, --help     Show this help message");
    println!();
    println!("Environment variables:");
    println!("  MUTSULIB       Colon-separated list of module search paths");
    println!("                 (added before -I paths, so -I takes priority)");
}

fn print_negation_error(option: &str) -> ! {
    println!("SORRY! Option '{}' cannot be negated", option);
    std::process::exit(1);
}

fn handle_negated_short_option(
    arg: &str,
    auto_print: &mut bool,
    auto_loop: &mut bool,
) -> Option<Result<(), ()>> {
    let name = arg.strip_prefix("-/")?;
    if name.len() != 1 {
        print_negation_error(arg);
    }
    Some(match name {
        "h" | "v" => Ok(()),
        "n" => {
            *auto_loop = false;
            Ok(())
        }
        "p" => {
            *auto_print = false;
            Ok(())
        }
        _ => Err(()),
    })
}

fn handle_negated_long_option(
    arg: &str,
    dump_ast: &mut bool,
    doc_mode: &mut bool,
    repl_flag: &mut bool,
    no_precomp: &mut bool,
) -> Option<Result<(), ()>> {
    let name = arg.strip_prefix("--/")?;
    Some(match name {
        "help" | "version" => Ok(()),
        "dump-ast" => {
            *dump_ast = false;
            Ok(())
        }
        "doc" => {
            *doc_mode = false;
            Ok(())
        }
        "repl" => {
            *repl_flag = false;
            Ok(())
        }
        "no-precomp" => {
            *no_precomp = false;
            Ok(())
        }
        _ => Err(()),
    })
}

fn main() {
    // Spawn the real entry point on a thread with a larger stack to avoid
    // stack overflows during deeply-recursive grammar matching.
    let builder = std::thread::Builder::new().stack_size(32 * 1024 * 1024);
    let handler = builder
        .spawn(run_main)
        .expect("failed to spawn main thread");
    match handler.join() {
        Ok(()) => {}
        Err(payload) => std::panic::resume_unwind(payload),
    }
}

fn run_main() {
    let args: Vec<String> = env::args().collect();

    let mut dump_ast = false;
    let mut doc_mode = false;
    let mut repl_flag = false;
    let mut no_precomp = false;
    let mut auto_print = false;
    let mut auto_loop = false;
    let mut lib_paths: Vec<String> = Vec::new();
    let mut preload_modules: Vec<String> = Vec::new();
    let mut filtered_args: Vec<String> = Vec::new();
    let mut iter = args[1..].iter();
    let mut seen_source = false; // true once -e CODE or a filename is consumed
    while let Some(arg) = iter.next() {
        // Once we've seen the source (code or filename), remaining args are
        // script arguments passed to @*ARGS / MAIN, not mutsu flags.
        if seen_source {
            filtered_args.push(arg.clone());
            continue;
        }
        if let Some(result) = handle_negated_short_option(arg, &mut auto_print, &mut auto_loop) {
            if result.is_ok() {
                continue;
            }
            print_negation_error(arg);
        }
        if let Some(result) = handle_negated_long_option(
            arg,
            &mut dump_ast,
            &mut doc_mode,
            &mut repl_flag,
            &mut no_precomp,
        ) {
            if result.is_ok() {
                continue;
            }
            print_negation_error(arg);
        }
        if arg == "--help" || arg == "-h" {
            print_help(&args[0]);
            return;
        } else if arg == "--version" || arg == "-v" {
            println!("mutsu 0.1.0");
            return;
        } else if arg == "--dump-ast" {
            dump_ast = true;
        } else if arg == "--doc" {
            doc_mode = true;
        } else if arg == "--repl" {
            repl_flag = true;
        } else if arg == "--no-precomp" {
            no_precomp = true;
        } else if arg == "-p" {
            auto_print = true;
        } else if arg == "-n" {
            auto_loop = true;
        } else if arg == "-e" {
            if let Some(code) = iter.next() {
                filtered_args.push("-e".to_string());
                filtered_args.push(code.clone());
                seen_source = true;
            } else {
                eprintln!("Usage: {} -e <code>", args[0]);
                std::process::exit(1);
            }
        } else if arg == "-ne" {
            // Combined -n -e: enable auto-loop and treat the next arg as code
            auto_loop = true;
            if let Some(code) = iter.next() {
                filtered_args.push("-e".to_string());
                filtered_args.push(code.clone());
                seen_source = true;
            } else {
                eprintln!("Usage: {} -ne <code>", args[0]);
                std::process::exit(1);
            }
        } else if arg == "-pe" {
            // Combined -p -e: enable auto-print and treat the next arg as code
            auto_print = true;
            if let Some(code) = iter.next() {
                filtered_args.push("-e".to_string());
                filtered_args.push(code.clone());
                seen_source = true;
            } else {
                eprintln!("Usage: {} -pe <code>", args[0]);
                std::process::exit(1);
            }
        } else if arg.starts_with("--parser=") {
            eprintln!("--parser option is no longer supported");
            std::process::exit(1);
        } else if arg == "-I" {
            if let Some(path) = iter.next() {
                lib_paths.push(path.clone());
            } else {
                eprintln!("Usage: {} -I <path>", args[0]);
                std::process::exit(1);
            }
        } else if let Some(path_suffix) = arg.strip_prefix("-I") {
            lib_paths.push(path_suffix.to_string());
        } else if arg == "-M" {
            if let Some(module) = iter.next() {
                preload_modules.push(module.clone());
            } else {
                eprintln!("Usage: {} -M <module>", args[0]);
                std::process::exit(1);
            }
        } else if let Some(module) = arg.strip_prefix("-M") {
            if module.is_empty() {
                eprintln!("Usage: {} -M <module>", args[0]);
                std::process::exit(1);
            }
            preload_modules.push(module.to_string());
        } else {
            filtered_args.push(arg.clone());
            // If this looks like a filename (not a flag), mark source as seen
            if !arg.starts_with('-') || arg == "-" {
                seen_source = true;
            }
        }
    }

    // MUTSULIB env var: colon-separated paths added before -I paths
    // (so -I takes priority since later paths are searched first)
    if let Ok(mutsulib) = env::var("MUTSULIB") {
        let mut env_paths: Vec<String> = mutsulib
            .split(':')
            .filter(|s| !s.is_empty())
            .map(|s| s.to_string())
            .collect();
        env_paths.append(&mut lib_paths);
        lib_paths = env_paths;
    }

    #[cfg(feature = "native")]
    if repl_flag || (filtered_args.is_empty() && io::stdin().is_terminal()) {
        mutsu::repl::run_repl();
        return;
    }
    #[cfg(not(feature = "native"))]
    if repl_flag {
        eprintln!("REPL is not available in this build");
        std::process::exit(1);
    }

    let (input, program_name, script_args) =
        if !filtered_args.is_empty() && filtered_args[0] == "-e" {
            if filtered_args.len() < 2 {
                eprintln!("Usage: {} -e <code>", args[0]);
                std::process::exit(1);
            }
            let rest = filtered_args[2..].to_vec();
            (filtered_args[1].clone(), "-e".to_string(), rest)
        } else if !filtered_args.is_empty() && filtered_args[0] == "-" {
            // `-` means read from STDIN
            let mut buf = String::new();
            io::stdin().read_to_string(&mut buf).unwrap_or_else(|err| {
                eprintln!("Failed to read stdin: {}", err);
                std::process::exit(1);
            });
            let rest = filtered_args[1..].to_vec();
            (buf, "-".to_string(), rest)
        } else if !filtered_args.is_empty() {
            let path = std::path::Path::new(&filtered_args[0]);
            if path.is_dir() {
                eprintln!("Can not run directory {}: is a directory", filtered_args[0]);
                std::process::exit(1);
            }
            let content = fs::read_to_string(&filtered_args[0]).unwrap_or_else(|_err| {
                eprintln!(
                    "Could not open {}. Failed to stat file: no such file or directory",
                    filtered_args[0]
                );
                std::process::exit(1);
            });
            let rest = filtered_args[1..].to_vec();
            (content, filtered_args[0].clone(), rest)
        } else {
            let mut buf = String::new();
            io::stdin().read_to_string(&mut buf).unwrap_or_else(|err| {
                eprintln!("Failed to read stdin: {}", err);
                std::process::exit(1);
            });
            (buf, "<stdin>".to_string(), Vec::new())
        };

    let input = if auto_print {
        format!("for lines() {{ {};\n.say;\n}}", input)
    } else if auto_loop {
        format!("for lines() {{ {};\n}}", input)
    } else {
        input
    };

    if dump_ast {
        match mutsu::dump_ast(&input) {
            Ok(ast) => println!("{}", ast),
            Err(err) => {
                print_error("Parse error", &err, Some(&input), Some(&program_name));
                std::process::exit(1);
            }
        }
        return;
    }

    if doc_mode {
        match mutsu::doc_mode::run_doc_mode(&input) {
            Ok(result) => {
                print!("{}", result.output);
                if result.status != 0 {
                    std::process::exit(result.status as i32);
                }
                return;
            }
            Err(err) => {
                print_error("Runtime error", &err, Some(&input), Some(&program_name));
                std::process::exit(1);
            }
        }
    }

    let mut interpreter = Interpreter::new();
    interpreter.set_immediate_stdout(true);
    if no_precomp {
        interpreter.set_precomp_enabled(false);
    }
    for path in lib_paths {
        interpreter.add_lib_path(path);
    }
    for module in preload_modules {
        if let Err(err) = interpreter.use_module(&module) {
            print_error("Runtime error", &err, Some(&input), Some(&program_name));
            std::process::exit(1);
        }
    }
    interpreter.set_program_path(&program_name);
    if !script_args.is_empty() {
        interpreter.set_args(script_args.into_iter().map(Value::str).collect());
    }
    match interpreter.run(&input) {
        Ok(_output) => {
            // Output is written directly to stdout during execution.
            // Subtest-indented output is also flushed here.
            interpreter.flush_all_handles();
            interpreter.flush_stderr_buffer();
            let code = interpreter.exit_code();
            // Always call process::exit to terminate any lingering background
            // threads (e.g. TCP accept loops for IO::Socket::Async listeners).
            std::process::exit(code as i32);
        }
        Err(err) => {
            print_error("Runtime error", &err, Some(&input), Some(&program_name));
            interpreter.flush_all_handles();
            interpreter.flush_stderr_buffer();
            let code = interpreter.exit_code();
            std::process::exit(if code != 0 { code as i32 } else { 1 });
        }
    }
}
