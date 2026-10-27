//! The RakuAST frontend (ADR-10723 Stages 0 and 3).
//!
//! The frontend is `parse -> RakuAST -> lower`: the program's own units (the
//! main program and every `EVAL` string) are converted to their RakuAST tree and
//! lowered back before they are compiled. The result is what the RakuAST
//! frontend will produce once the parser emits RakuAST directly (Stage 2). There
//! is no opt-out.
//!
//! A `use`d module or `require`d file is still compiled from the parser's own
//! tree: a module is shared by every program that uses it, and the cases that
//! keep it out of the round trip are tracked on #15074.
//!
//! `MUTSU_RAKUAST=convert` is the one test level that is left: no subtree
//! crosses as the parser's own tree ([`super::passthrough`]), every node goes
//! through `convert` and `lower`, as `.AST` and `EVAL($ast)` do. The default is
//! the cost-measured level; `convert` keeps the coverage of the `convert` and
//! `lower` arms of the carried families (`scripts/rakuast-frontend.sh check`).
//!
//! A construct the converter or the lowerer refuses is a compile error, never a
//! silent fallback to the parser's own tree: a fallback would count a file as
//! round-tripping when it does not.

use std::sync::OnceLock;

use crate::ast::Stmt;
use crate::value::RuntimeError;

/// Which kind of compilation unit is being parsed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Unit {
    /// The program's main unit.
    Mainline,
    /// An `EVAL` string.
    Eval,
    /// A `use`d module or a `require`d file.
    Module,
}

/// Whether `MUTSU_RAKUAST=convert` is set: the test level without passthrough
/// leaves. Read once per process.
fn convert_level() -> bool {
    static CONVERT: OnceLock<bool> = OnceLock::new();
    *CONVERT.get_or_init(|| convert_level_from(std::env::var("MUTSU_RAKUAST").ok().as_deref()))
}

// Cost: O(1).
fn convert_level_from(rakuast: Option<&str>) -> bool {
    rakuast == Some("convert")
}

/// A short name of the frontend level, part of the precompilation cache key: a
/// module cached under one level is never read under another.
// Cost: O(1).
pub(crate) fn cache_tag() -> &'static str {
    if convert_level() {
        "ast-convert"
    } else {
        "ast"
    }
}

/// Whether the frontend runs a unit of this kind through the round trip. Such a
/// unit is parsed with its spellings kept (ADR-12199): the conversion reads
/// them, and the lowering hands the compiler the same tree it would have had
/// without them.
// Cost: O(1).
pub(crate) fn covers(unit: Unit) -> bool {
    unit != Unit::Module
}

/// Run a parsed compilation unit through the RakuAST round trip when the
/// frontend covers its kind; hand it back untouched otherwise.
pub(crate) fn round_trip_if_enabled(
    stmts: Vec<Stmt>,
    unit: Unit,
) -> Result<Vec<Stmt>, RuntimeError> {
    if !covers(unit) {
        return Ok(stmts);
    }
    round_trip_owned(stmts).map_err(refusal)
}

/// A refusal of the round trip as the mode reports it. An error that already
/// carries an exception is the unit's own compile-time failure (a deferred
/// `X::Obsolete`, which rakudo throws while compiling), not a gap in the
/// conversion, and passes through unchanged.
fn refusal(err: RuntimeError) -> RuntimeError {
    if err.exception.is_some() {
        return err;
    }
    RuntimeError::new(format!(
        "MUTSU_RAKUAST: this compilation unit does not round-trip through RakuAST: {}",
        err.message
    ))
}

/// `parse → RakuAST → lower`, the pipeline the RakuAST frontend replaces the
/// parser's tree with.
pub(crate) fn round_trip(stmts: &[Stmt]) -> Result<Vec<Stmt>, RuntimeError> {
    round_trip_owned(stmts.to_vec())
}

/// [`round_trip`] for a unit the caller owns: the origins come off the tree
/// in place, with no copy of it.
// Cost: O(n), n = size of the unit.
pub(crate) fn round_trip_owned(mut stmts: Vec<Stmt>) -> Result<Vec<Stmt>, RuntimeError> {
    // The conversion matches expressions by shape, so the term origins the
    // spelled parse kept come off first (`expr_origins`).
    let _leaves = (!convert_level()).then(super::passthrough::Scope::enter);
    let _origins = super::expr_origins::strip(&mut stmts);
    let node = super::convert::statement_list(&stmts)?;
    let mut lowered = super::lower(&node)?;
    // The spelled parse keeps bare type names for the conversion; the
    // lowered tree gets the qualification an ordinary parse does.
    crate::parser::qualify_type_names(&mut lowered);
    crate::parser::p6_frame_state::lower_program(&mut lowered);
    Ok(lowered)
}

/// [`round_trip_if_enabled`] for an `EVAL` string, whose bare names may be
/// declared by the scope that called it: `types` and `terms` are those names,
/// and `operators` the operator routines it makes visible.
pub(crate) fn round_trip_eval(
    stmts: &[Stmt],
    types: impl FnOnce() -> Vec<String>,
    terms: impl FnOnce() -> Vec<String>,
    operators: &[String],
) -> Result<Vec<Stmt>, RuntimeError> {
    with_caller_names(types(), terms(), || {
        super::with_caller_operators(operators, || round_trip(stmts))
    })
    .map_err(refusal)
}

/// Run `convert` with `types` and `terms` installed as the names the calling
/// scope declares: rakudo resolves the bare names of an `EVAL` string or of a
/// `Str.AST` against the scope that runs it (`constant b = 1; q[b].AST` is a
/// `Term::Name`), so the unit's own declarations cannot say what they are.
// Cost: O(n) to install, n = number of caller names, plus `convert`.
pub(crate) fn with_caller_names<T>(
    types: Vec<String>,
    terms: Vec<String>,
    convert: impl FnOnce() -> T,
) -> T {
    let _caller = super::bareword::CallerNames::install(types, terms);
    convert()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_convert_selects_the_test_level() {
        assert!(convert_level_from(Some("convert")));
        assert!(!convert_level_from(Some("1")));
        assert!(!convert_level_from(Some("all")));
        assert!(!convert_level_from(Some("0")));
        assert!(!convert_level_from(None));
    }

    #[test]
    fn modules_stay_outside_the_round_trip() {
        assert!(covers(Unit::Mainline));
        assert!(covers(Unit::Eval));
        assert!(!covers(Unit::Module));
    }

    #[test]
    fn unmodelled_literal_refuses_instead_of_falling_back() {
        let stmts = vec![Stmt::Expr(crate::ast::Expr::Literal(
            crate::value::Value::regex("a".to_owned()),
        ))];
        let error = round_trip(&stmts).expect_err("a regex value without source tree must refuse");
        let reported = refusal(error);
        assert!(reported.message.contains("MUTSU_RAKUAST"));
        assert!(
            reported
                .message
                .contains("literal Regex without a declarator tree")
        );
    }
}
