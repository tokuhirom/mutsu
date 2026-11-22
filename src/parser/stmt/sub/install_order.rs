//! When a `multi` candidate of the unit is installed, relative to the modules
//! loaded at parse time (ADR-17558 slice 5b).
//!
//! rakudo installs a candidate when the parser reaches it, so one the unit
//! declares before `use M` ranks before the candidates `M` exports and one
//! declared after it ranks after them (an equal-narrowness tie goes to the
//! earlier one). mutsu registers the unit's candidates when the unit runs, so
//! a candidate of the program's top level takes its stamp from the parse
//! instead: the loader reserves it as the parser finishes the declaration, and
//! the registration uses it (`compile_time_loader::PARSE_DECL_ORDER_TRAIT`).
//!
//! A candidate that gets no stamp (a module's compile, an `EVAL`, a nested
//! scope, a package body, a shape the RakuAST finalization rebuilds) keeps the
//! transitional rule of ADR-17558 §7.6: a `use` after it loads in the
//! BEGIN-time prologue, after the candidate is registered. So does a `proto`
//! in those places; one at the program's top level ranks nothing and needs no
//! stamp, so it holds no `use` back (§7.14).

use crate::ast::{Expr, Stmt};
use crate::parser::parse_result::PResult;
use crate::value::Value;

/// Stamp the `multi` declaration `parsed` the parser just finished with its
/// installation order, or note it as a compile-time effect a later `use`
/// must not overtake.
// Cost: O(1).
pub(super) fn note_multi_installed(parsed: PResult<'_, Stmt>) -> PResult<'_, Stmt> {
    let (rest, mut stmt) = parsed?;
    let stamp = stampable(&stmt)
        .then(crate::compile_time_loader::reserve_decl_order)
        .flatten();
    match (stamp, &mut stmt) {
        (Some(order), Stmt::SubDecl { custom_traits, .. }) => custom_traits.push((
            crate::compile_time_loader::PARSE_DECL_ORDER_TRAIT.to_string(),
            Some(Expr::Literal(Value::int(order as i64))),
        )),
        _ => crate::parser::stmt::simple::note_unit_compile_time_effect(),
    }
    // The stamp only orders the candidate; a closed `BEGIN` after it still
    // waits for the prologue, as before (ADR-17558 §7.12).
    crate::parser::stmt::simple::note_unit_prologue_effect();
    Ok((rest, stmt))
}

/// Note a `proto` declaration the parser has reached (see the module docs).
// Cost: O(1).
pub(crate) fn note_proto_installed() {
    let installs_at_parse = !crate::parser::stmt::simple::in_nested_scope()
        && crate::parser::stmt::simple::statement_package().is_none()
        && !crate::parser::in_fragment_parse()
        && crate::compile_time_loader::installs_at_parse();
    if !installs_at_parse {
        crate::parser::stmt::simple::note_unit_compile_time_effect();
    }
    crate::parser::stmt::simple::note_unit_prologue_effect();
}

/// Whether `stmt` is a candidate of the unit's top level outside any package
/// whose declaration the RakuAST finalization keeps as the parser built it
/// (`rakuast::carried`), so the stamp reaches its registration.
// Cost: O(1).
fn stampable(stmt: &Stmt) -> bool {
    let Stmt::SubDecl {
        name_expr,
        associativity,
        precedence_trait,
        signature_alternates,
        ..
    } = stmt
    else {
        return false;
    };
    name_expr.is_none()
        && associativity.is_none()
        && precedence_trait.is_none()
        && signature_alternates.is_empty()
        && !crate::parser::stmt::simple::in_nested_scope()
        && crate::parser::stmt::simple::statement_package().is_none()
        && !crate::parser::in_fragment_parse()
}
