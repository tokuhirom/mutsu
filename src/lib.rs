mod ast;
mod builtins;
mod compiler;
mod interpreter;
mod lexer;
mod opcode;
mod parser;
mod trace;
mod value;
mod vm;

pub use interpreter::Interpreter;
pub use value::{RuntimeError, Value};

/// Parse source code and return a pretty-printed AST string.
#[allow(clippy::result_large_err)]
pub fn dump_ast(input: &str) -> Result<String, RuntimeError> {
    let mut lex = lexer::Lexer::new(input);
    let mut tokens = Vec::new();
    loop {
        let tok = lex.next_token();
        let eof = matches!(tok.kind, lexer::TokenKind::Eof);
        tokens.push(tok);
        if eof {
            break;
        }
    }
    let mut parser = parser::Parser::new(tokens);
    let stmts = parser.parse_program()?;
    Ok(format!("{:#?}", stmts))
}
