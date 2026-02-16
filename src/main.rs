use std::env;
use std::fs;
use std::io::{self, Read};

use mutsu::Interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut dump_ast = false;
    let mut filtered_args: Vec<String> = Vec::new();
    for arg in &args[1..] {
        if arg == "--dump-ast" {
            dump_ast = true;
        } else if arg.starts_with("--parser=") {
            eprintln!("--parser option is no longer supported");
            std::process::exit(1);
        } else {
            filtered_args.push(arg.clone());
        }
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
                eprintln!("Parse error: {}", err.message);
                std::process::exit(1);
            }
        }
        return;
    }

    let mut interpreter = Interpreter::new();
    interpreter.set_program_path(&program_name);
    match interpreter.run(&input) {
        Ok(output) => print!("{}", output),
        Err(err) => {
            eprintln!("Runtime error: {}", err.message);
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
