mod expr;
mod helpers;
mod primary;
mod stmt;

use crate::ast::Stmt;
use crate::value::RuntimeError;

/// Parse a full program using the nom-based parser.
/// Currently a skeleton that supports only `say <expr>;`.
#[allow(clippy::result_large_err)]
pub(crate) fn parse_program(input: &str) -> Result<Vec<Stmt>, RuntimeError> {
    match stmt::program(input) {
        Ok((_rest, stmts)) => Ok(stmts),
        Err(e) => Err(RuntimeError::new(format!("nom parse error: {}", e))),
    }
}
