//! `while my $x { ... }` / `until my $x { ... }`: a loop whose condition is a
//! bare declaration with no initializer.
//!
//! The parser declares the variable once, before the loop, so it is not
//! re-declared (and reset) on every iteration, and re-reads it as the
//! condition: a scopeless `SyntheticBlock([VarDecl, While { cond: $x }])` (or
//! `!$x` for `until`). RakuAST has no such split: the declaration *is* the
//! loop's condition (`Loop::Until(condition => VarDeclaration::Simple)`).
//!
//! The parser keeps the loop with the declaration as its condition
//! (`ParsedStmt::WhileLoop`); its lowering hoists the declaration with
//! [`split_loop_cond_decl`] and [`expand`].

use super::{Expr, Stmt};

/// `SyntheticBlock([decl, loop])`.
// Cost: O(1).
pub(crate) fn expand(decl: Stmt, loop_stmt: Stmt) -> Stmt {
    Stmt::SyntheticBlock(vec![decl, loop_stmt])
}

/// Split an inline no-initializer declaration out of a `while`/`until`
/// condition so it is declared once at loop entry instead of re-declared (and
/// reset to its default) on every iteration. `until my $done { ... CLOSE {
/// $done = True } }` relies on `$done` persisting across iterations.
///
/// Only the no-initializer form is hoisted: `while my $x = expr { }` must keep
/// re-running `= expr` each iteration (that is the whole point of the idiom), so
/// it is left as-is. Returns the hoisted declaration (if any) and the condition
/// to test each iteration (a plain read of the declared variable).
pub(crate) fn split_loop_cond_decl(cond: Expr) -> (Option<Stmt>, Expr) {
    if let Expr::DoStmt(boxed) = &cond
        && let Stmt::VarDecl {
            name,
            has_initializer: false,
            constant: None,
            bind: None,
            ..
        } = boxed.as_ref()
    {
        let read = if let Some(rest) = name.strip_prefix('@') {
            Expr::ArrayVar(rest.to_string())
        } else if let Some(rest) = name.strip_prefix('%') {
            Expr::HashVar(rest.to_string())
        } else {
            Expr::Var(name.clone())
        };
        let decl = (**boxed).clone();
        return (Some(decl), read);
    }
    (None, cond)
}
