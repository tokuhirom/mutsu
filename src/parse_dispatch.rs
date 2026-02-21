use crate::ast::Stmt;
use crate::parser;
use crate::value::RuntimeError;

/// Parse source code.
/// Returns `(statements, Option<finish_content>)`.
#[allow(clippy::result_large_err)]
pub(crate) fn parse_source(input: &str) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    parser::parse_program(input)
}

/// Best-effort parse: returns all statements that could be parsed before the
/// first error.  Used for loading modules that may contain unsupported syntax.
pub(crate) fn parse_source_partial(input: &str) -> (Vec<Stmt>, Option<String>) {
    parser::parse_program_partial(input)
}
