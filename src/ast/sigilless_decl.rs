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
//! The parser keeps the declaration as written (`ParsedStmt::SigillessDecl`);
//! its lowering builds the expansion.

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
    /// `my \name .= method` uses an `Initializer::CallAssign` in RakuAST.
    pub call_assign: Option<(&'a crate::symbol::Symbol, &'a [Expr])>,
}

/// The compiler's form of `my \name .= method` (the declaration plus its
/// readonly marker), for the declaration's initializer `expr` (the method call
/// on the declared type).
// Cost: O(1).
pub(crate) fn call_assign_expansion(
    name: String,
    expr: Expr,
    type_constraint: Option<String>,
    is_state: bool,
    is_our: bool,
) -> Stmt {
    Stmt::SyntheticBlock(vec![
        Stmt::VarDecl {
            anonymous: false,
            name: name.clone(),
            expr,
            type_constraint,
            is_state,
            is_our,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            has_initializer: false,
            where_constraint: None,
        },
        Stmt::MarkSigillessReadonly(name),
    ])
}
