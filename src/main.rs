use std::env;
use std::fs;
use std::io::{self, Read};

use mutsu::Interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();

    let (input, program_name) = if args.len() > 1 && args[1] == "-e" {
        if args.len() < 3 {
            eprintln!("Usage: {} -e <code>", args[0]);
            std::process::exit(1);
        }
        (args[2].clone(), "-e".to_string())
    } else if args.len() > 1 {
        let content = fs::read_to_string(&args[1]).unwrap_or_else(|err| {
            eprintln!("Failed to read {}: {}", args[1], err);
            std::process::exit(1);
        });
        (content, args[1].clone())
    } else {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf).unwrap_or_else(|err| {
            eprintln!("Failed to read stdin: {}", err);
            std::process::exit(1);
        });
        (buf, "<stdin>".to_string())
    };

    let mut interpreter = Interpreter::new();
    interpreter.set_program_path(&program_name);
    match interpreter.run(&input) {
        Ok(output) => print!("{}", output),
        Err(err) => {
            eprintln!("Runtime error: {}", err.message);
            std::process::exit(1);
        }
    }
}
