mod expr;
mod helpers;
mod parse_result;
mod primary;
mod stmt;

use crate::ast::Stmt;
use crate::value::RuntimeError;

/// Parse a full program using the nom-based parser.
/// Returns `(statements, Option<finish_content>)`.
#[allow(clippy::result_large_err)]
pub(crate) fn parse_program(input: &str) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    // Split off =finish content before parsing
    let (source, finish_content) = if let Some(idx) = input.find("\n=finish") {
        let content = &input[idx + "\n=finish".len()..];
        // Skip to next newline
        let content = if let Some(nl) = content.find('\n') {
            &content[nl + 1..]
        } else {
            ""
        };
        (&input[..idx], Some(content.to_string()))
    } else {
        (input, None)
    };

    match stmt::program(source) {
        Ok((_rest, stmts)) => Ok((stmts, finish_content)),
        Err(e) => Err(RuntimeError::new(format!("parse error: {}", e))),
    }
}
