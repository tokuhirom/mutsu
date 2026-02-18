mod ast;
mod builtins;
mod compiler;
mod interpreter;
mod opcode;
mod parse_dispatch;
mod parser;
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
