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

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut dump_ast = false;
    let mut repl_flag = false;
    let mut lib_paths: Vec<String> = Vec::new();
    let mut filtered_args: Vec<String> = Vec::new();
    let mut iter = args[1..].iter();
    while let Some(arg) = iter.next() {
        if arg == "--dump-ast" {
            dump_ast = true;
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

    let mut interpreter = Interpreter::new();
    for path in lib_paths {
        interpreter.add_lib_path(path);
    }
    interpreter.set_program_path(&program_name);
    match interpreter.run(&input) {
        Ok(output) => print!("{}", output),
        Err(err) => {
            print_error("Runtime error", &err);
            let output_buf = interpreter.output();
            if !output_buf.is_empty() {
                eprintln!("--- buffered TAP output ---");
                print!("{}", output_buf);
                eprintln!("--- end buffered TAP output ---");
            }
            std::process::exit(1);
        }
    }
}
