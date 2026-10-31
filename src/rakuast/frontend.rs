//! The RakuAST frontend (ADR-10723 Stages 0 and 3).
//!
//! Every compilation unit (the main program, every `EVAL` string, every `use`d
//! module and `require`d file) is parsed with its spellings kept and finalized
//! in place before it is compiled ([`super::finalize`]): the typed nodes of the
//! migrated families are lowered where they stand. There is no opt-out.
//!
//! `MUTSU_RAKUAST=convert` is the one test level that is left: the whole unit
//! is converted to a `RakuAstNode` tree and lowered back, as `.AST` and
//! `EVAL($ast)` do. It keeps the coverage of the `convert` and `lower` arms
//! (`scripts/rakuast-frontend.sh check`).
//!
//! A construct the converter or the lowerer refuses is a compile error, never a
//! silent fallback to the parser's own tree: a fallback would count a file as
//! round-tripping when it does not.

use std::sync::OnceLock;

use crate::ast::Stmt;
use crate::value::RuntimeError;

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

/// Run a parsed compilation unit through the RakuAST round trip. Every unit is
/// parsed with its spellings kept (ADR-12199): the conversion reads them, and
/// the lowering hands the compiler the same tree it would have had without
/// them.
pub(crate) fn round_trip_unit(stmts: Vec<Stmt>) -> Result<Vec<Stmt>, RuntimeError> {
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
pub(crate) fn round_trip_owned(stmts: Vec<Stmt>) -> Result<Vec<Stmt>, RuntimeError> {
    let mut lowered = if convert_level() {
        let node = super::convert::statement_list(&stmts)?;
        super::lower::lower_converted(&node)?
    } else {
        super::finalize::finalize_unit(stmts)?
    };
    // The spelled parse keeps bare type names for the conversion; the
    // lowered tree gets the qualification an ordinary parse does.
    crate::parser::qualify_type_names(&mut lowered);
    crate::parser::p6_frame_state::lower_program(&mut lowered);
    Ok(lowered)
}

/// [`round_trip_unit`] for an `EVAL` string, whose bare names may be
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

/// Run `convert` with `types` installed as the type names the calling scope of a
/// `Str.AST` declares, for the `is NAME` traits of the declarations in its
/// source (a bare class name is still a `Term::Name`: see [`with_caller_names`]).
// Cost: O(n) to install, n = number of caller type names, plus `convert`.
pub(crate) fn with_caller_trait_types<T>(types: Vec<String>, convert: impl FnOnce() -> T) -> T {
    let _caller = super::bareword::CallerTraitTypes::install(types);
    convert()
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
