use std::env;
use std::fs;
#[cfg(feature = "native")]
use std::io::IsTerminal;
use std::io::{self, Read};

use mutsu::{Interpreter, RuntimeError, Value};

fn print_error(prefix: &str, err: &RuntimeError) {
    eprintln!("{}: {}", prefix, err.message);
    let mut meta = Vec::new();
    if let Some(code) = err.code {
        meta.push(format!("code={}", code));
        if code.is_parse() {
            meta.push("kind=parse".to_string());
        }
    }
    match (err.line, err.column) {
        (Some(line), Some(column)) => meta.push(format!("line={}, column={}", line, column)),
        (Some(line), None) => meta.push(format!("line={}", line)),
        _ => {}
    }
    if !meta.is_empty() {
        eprintln!("{} metadata: {}", prefix, meta.join(", "));
    }
    if let Some(hint) = &err.hint {
        eprintln!("{} hint: {}", prefix, hint);
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

fn main() {
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
                print_error("Parse error", &err);
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
                print_error("Runtime error", &err);
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
            print_error("Runtime error", &err);
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
            if code != 0 {
                std::process::exit(code as i32);
            }
        }
        Err(err) => {
            print_error("Runtime error", &err);
            interpreter.flush_all_handles();
            interpreter.flush_stderr_buffer();
            let code = interpreter.exit_code();
            std::process::exit(if code != 0 { code as i32 } else { 1 });
        }
    }
}
