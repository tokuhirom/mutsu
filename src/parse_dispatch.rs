use crate::ast::Stmt;
use crate::parser;
use crate::value::RuntimeError;

/// Parse source code.
/// Returns `(statements, Option<finish_content>)`.
#[allow(clippy::result_large_err)]
pub(crate) fn parse_source(input: &str) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    parser::parse_program(input)
}
