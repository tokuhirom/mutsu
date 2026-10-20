//! Metadata setters paired with unit declarations without adding a scope.

use crate::ast::{Expr, Stmt};

pub(super) fn decl_adverb_meta_stmts(name: &str, adverbs: Vec<(String, Expr)>) -> Vec<Stmt> {
    adverbs
        .into_iter()
        .filter(|(key, _)| matches!(key.as_str(), "ver" | "auth" | "api"))
        .map(|(key, value)| super::class_decl::meta_setter_stmt(name, &key, value))
        .collect()
}

/// Keep the declaration last: `stmt_list` absorbs the rest of the unit there.
pub(super) fn with_meta_stmts(mut meta_stmts: Vec<Stmt>, decl: Stmt) -> Stmt {
    if meta_stmts.is_empty() {
        return decl;
    }
    meta_stmts.push(decl);
    Stmt::SyntheticBlock(meta_stmts)
}
