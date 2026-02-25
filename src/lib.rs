mod ast;
mod builtins;
mod compiler;
pub mod doc_mode;
mod interpreter;
mod opcode;
mod parse_dispatch;
mod parser;
#[cfg(feature = "native")]
pub mod repl;
mod runtime;
mod token_kind;
mod trace;
mod value;
mod vm;

pub use interpreter::Interpreter;
pub use value::{RuntimeError, RuntimeErrorCode, Value};

/// Parse source code and return a pretty-printed AST string.
#[allow(clippy::result_large_err)]
pub fn dump_ast(input: &str) -> Result<String, RuntimeError> {
    let (stmts, _) = parse_dispatch::parse_source(input)?;
    Ok(format!("{:#?}", stmts))
}

#[cfg(feature = "wasm")]
use wasm_bindgen::prelude::*;

/// Evaluate Raku code and return the output as a string.
/// This is the main entry point for the WASM build.
#[cfg(feature = "wasm")]
#[wasm_bindgen]
pub fn evaluate(code: &str) -> String {
    console_error_panic_hook::set_once();
    let mut interpreter = Interpreter::new();
    match interpreter.run(code) {
        Ok(output) => output,
        Err(err) => format!("Error: {}", err.message),
    }
}
