use std::env;
use std::fs;
use std::io::{self, IsTerminal, Read};

use mutsu::{Interpreter, RuntimeError};

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
    println!("  --dump-ast     Dump the AST instead of executing");
    println!("  --doc          Render Pod documentation from the source");
    println!("  --repl         Start the interactive REPL");
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
    let mut lib_paths: Vec<String> = Vec::new();
    let mut filtered_args: Vec<String> = Vec::new();
    let mut iter = args[1..].iter();
    while let Some(arg) = iter.next() {
        if arg == "--help" || arg == "-h" {
            print_help(&args[0]);
            return;
        } else if arg == "--dump-ast" {
            dump_ast = true;
        } else if arg == "--doc" {
            doc_mode = true;
        } else if arg == "--repl" {
            repl_flag = true;
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
        } else {
            filtered_args.push(arg.clone());
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

    if repl_flag || (filtered_args.is_empty() && io::stdin().is_terminal()) {
        mutsu::repl::run_repl();
        return;
    }

    let (input, program_name) = if !filtered_args.is_empty() && filtered_args[0] == "-e" {
        if filtered_args.len() < 2 {
            eprintln!("Usage: {} -e <code>", args[0]);
            std::process::exit(1);
        }
        (filtered_args[1].clone(), "-e".to_string())
    } else if !filtered_args.is_empty() {
        let content = fs::read_to_string(&filtered_args[0]).unwrap_or_else(|err| {
            eprintln!("Failed to read {}: {}", filtered_args[0], err);
            std::process::exit(1);
        });
        (content, filtered_args[0].clone())
    } else {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf).unwrap_or_else(|err| {
            eprintln!("Failed to read stdin: {}", err);
            std::process::exit(1);
        });
        (buf, "<stdin>".to_string())
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
        let output = mutsu::doc_mode::render_doc(&input);
        print!("{}", output);
        return;
    }

    let mut interpreter = Interpreter::new();
    for path in lib_paths {
        interpreter.add_lib_path(path);
    }
    interpreter.set_program_path(&program_name);
    match interpreter.run(&input) {
        Ok(output) => {
            print!("{}", output);
            let code = interpreter.exit_code();
            if code != 0 {
                std::process::exit(code as i32);
            }
        }
        Err(err) => {
            print_error("Runtime error", &err);
            let output_buf = interpreter.output();
            if !output_buf.is_empty() {
                eprintln!("--- buffered TAP output ---");
                print!("{}", output_buf);
                eprintln!("--- end buffered TAP output ---");
            }
            let code = interpreter.exit_code();
            std::process::exit(if code != 0 { code as i32 } else { 1 });
        }
    }
}
