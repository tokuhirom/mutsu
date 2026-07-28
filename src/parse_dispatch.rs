use crate::ast::Stmt;
use crate::parser;
use crate::value::RuntimeError;

/// Parse source code as a *nested* sub-parse (EVAL, an embedded `{...}` block in
/// a regex, a prelude injection, a `.AST` round-trip, ...).
///
/// The `use vX` pragma is lexical to a compilation unit, so a nested parse must
/// not leave its own language version behind: `parse_program` resets the parser
/// global to the 6.d default and then adopts whatever pragma `input` declares,
/// which would silently downgrade the enclosing program. A single
/// `EVAL('sprintf("%b",1)')` in a `use v6.e.PREVIEW` file used to drop every
/// later version-gated behavior (sprintf flag semantics, submethod dispatch,
/// grammar `.parse` Failure, ...) back to 6.d.
///
/// Returns `(statements, Option<finish_content>)`.
pub(crate) fn parse_source(input: &str) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    let saved_language_version = parser::current_language_version();
    let result = parser::parse_program(input);
    parser::set_current_language_version(&saved_language_version);
    result
}

/// Parse source code as a compilation unit whose body is about to run: the main
/// program, a `use`d module, a `require`d file. Unlike `parse_source` this keeps
/// the unit's own `use vX` pragma in effect, because the statements that follow
/// execute under it. Callers that load a *nested* unit (`load_module`) restore
/// their own version once the unit's mainline has finished.
pub(crate) fn parse_compilation_unit(
    input: &str,
) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    parser::parse_program(input)
}
