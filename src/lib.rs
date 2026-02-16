mod ast;
mod builtins;
mod compiler;
mod interpreter;
mod lexer;
mod opcode;
mod parse_dispatch;
mod parser_nom;
mod runtime;
mod trace;
mod value;
mod vm;

pub use interpreter::Interpreter;
pub use value::{RuntimeError, Value};

/// Tokenize source code and return a pretty-printed token list string.
pub fn dump_tokens(input: &str) -> String {
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
    format!("{:#?}", tokens)
}

/// Parse source code and return a pretty-printed AST string.
#[allow(clippy::result_large_err)]
pub fn dump_ast(input: &str) -> Result<String, RuntimeError> {
    let (stmts, _) = parse_dispatch::parse_source(input)?;
    Ok(format!("{:#?}", stmts))
}
