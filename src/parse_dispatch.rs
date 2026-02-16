use crate::ParserBackend;
use crate::ast::Stmt;
use crate::lexer::{Lexer, TokenKind};
use crate::parser::Parser;
use crate::parser_nom;
use crate::value::RuntimeError;

/// Parse source code using the specified parser backend.
/// Returns `(statements, Option<finish_content>)`.
#[allow(clippy::result_large_err)]
pub(crate) fn parse_source(
    input: &str,
    backend: ParserBackend,
) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    match backend {
        ParserBackend::RecursiveDescent => parse_rd(input),
        ParserBackend::Nom => parse_nom(input),
    }
}

#[allow(clippy::result_large_err)]
fn parse_rd(input: &str) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    loop {
        let token = lexer.next_token();
        let end = matches!(token.kind, TokenKind::Eof);
        tokens.push(token);
        if end {
            break;
        }
    }
    // Check for lexer compile errors
    if let Some(err) = lexer.errors.first() {
        return Err(RuntimeError::new(err));
    }
    let finish = lexer.finish_content().map(|s| s.to_string());
    let mut parser = Parser::new(tokens);
    let stmts = parser.parse_program()?;
    Ok((stmts, finish))
}

#[allow(clippy::result_large_err)]
fn parse_nom(input: &str) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    let stmts = parser_nom::parse_program(input)?;
    Ok((stmts, None))
}
