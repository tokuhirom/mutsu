//! A sigilless declaration, `my \x = 5` / `my Int \x := $s`.
//!
//! The parser builds one expansion for both spellings: the declaration wrapped
//! in the bookkeeping statements the compiler needs (`MarkSigillessReadonly`,
//! or `MarkBind` + `MarkSigilless` when the right-hand side can denote a
//! container). The spelling itself is not part of the expansion, and rakudo
//! renders the two differently (`Initializer::Assign` / `Initializer::Bind`),
//! so the declaration carries it as the [`SIGILLESS_DECL`] trait, whose
//! argument says whether it was written with `=`.
//!
//! [`declaration`] is the converter's way back: it accepts a statement only
//! when it has exactly the shape the parser builds, so a `with`/`given` pointy
//! binding, which uses the same bookkeeping statements, is never mistaken for
//! a declaration written in the source.

use super::{Expr, Stmt};

/// The internal trait marking a declaration built from `my \name = …` /
/// `my \name := …`; its argument is `True` for the `=` spelling.
pub(crate) const SIGILLESS_DECL: &str = "__sigilless_decl";

/// What a sigilless declaration says.
pub(crate) struct SigillessDecl<'a> {
    pub name: &'a str,
    pub expr: &'a Expr,
    pub type_constraint: Option<&'a str>,
    pub is_state: bool,
    pub is_our: bool,
    /// Written `my \x = …` rather than `my \x := …`.
    pub assigned: bool,
}

/// The sigilless declaration `stmt` is the parser's expansion of, if it is one.
// Cost: O(t), t = custom traits of the declaration.
pub(crate) fn declaration(stmt: &Stmt) -> Option<SigillessDecl<'_>> {
    let Stmt::SyntheticBlock(stmts) = stmt else {
        return None;
    };
    let (decl, marked) = match stmts.as_slice() {
        [
            decl @ Stmt::VarDecl { .. },
            Stmt::MarkSigillessReadonly(marked),
        ] => (decl, marked),
        [
            Stmt::MarkBind,
            decl @ Stmt::VarDecl { .. },
            Stmt::MarkSigilless(marked),
        ] => (decl, marked),
        _ => return None,
    };
    let Stmt::VarDecl {
        name,
        expr,
        type_constraint,
        is_state,
        is_our,
        is_dynamic: false,
        is_export: false,
        custom_traits,
        where_constraint: None,
        ..
    } = decl
    else {
        return None;
    };
    if name != marked {
        return None;
    }
    let mut assigned = None;
    for (trait_name, arg) in custom_traits {
        match (trait_name.as_str(), arg) {
            ("__has_initializer", None) => {}
            (SIGILLESS_DECL, Some(Expr::Literal(value))) => assigned = Some(value.truthy()),
            _ => return None,
        }
    }
    Some(SigillessDecl {
        name,
        expr,
        type_constraint: type_constraint.as_deref(),
        is_state: *is_state,
        is_our: *is_our,
        assigned: assigned?,
    })
}
