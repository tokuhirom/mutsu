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
    expr::reset_expression_memo();
    stmt::reset_statement_memo();
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
        Ok((rest, stmts)) => {
            let rest_trimmed = rest.trim();
            if !rest_trimmed.is_empty() {
                // Show context around where parsing stopped
                let consumed = source.len() - rest.len();
                let line_num = source[..consumed].matches('\n').count() + 1;
                let context: String = rest_trimmed.chars().take(60).collect();
                return Err(RuntimeError::new(format!(
                    "parse error: unparsed input at line {}: {:?}",
                    line_num, context
                )));
            }
            Ok((stmts, finish_content))
        }
        Err(e) => {
            if let Some(consumed) = e.consumed_from(source.len()) {
                let line_num = source[..consumed].matches('\n').count() + 1;
                let context_start = &source[consumed..];
                let context_trimmed = context_start.trim_start();
                if !context_trimmed.is_empty() {
                    let context: String = context_trimmed.chars().take(60).collect();
                    Err(RuntimeError::new(format!(
                        "parse error at line {}: {} — near: {:?}",
                        line_num, e, context
                    )))
                } else {
                    Err(RuntimeError::new(format!(
                        "parse error at line {}: {}",
                        line_num, e
                    )))
                }
            } else {
                Err(RuntimeError::new(format!("parse error: {}", e)))
            }
        }
    }
}
