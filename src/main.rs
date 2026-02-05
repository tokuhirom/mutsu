use std::env;
use std::fs;
use std::io::{self, Read};

use mutsu::Interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() > 1 {
        fs::read_to_string(&args[1]).unwrap_or_else(|err| {
            eprintln!("Failed to read {}: {}", args[1], err);
            std::process::exit(1);
        })
    } else {
        let mut buf = String::new();
        io::stdin().read_to_string(&mut buf).unwrap_or_else(|err| {
            eprintln!("Failed to read stdin: {}", err);
            std::process::exit(1);
        });
        buf
    };

    let mut interpreter = Interpreter::new();
    match interpreter.run(&input) {
        Ok(output) => print!("{}", output),
        Err(err) => {
            eprintln!("Runtime error: {}", err.message);
            std::process::exit(1);
        }
    }
}
