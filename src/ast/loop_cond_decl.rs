//! `while my $x { ... }` / `until my $x { ... }`: a loop whose condition is a
//! bare declaration with no initializer.
//!
//! The parser declares the variable once, before the loop, so it is not
//! re-declared (and reset) on every iteration, and re-reads it as the
//! condition: a scopeless `SyntheticBlock([VarDecl, While { cond: $x }])` (or
//! `!$x` for `until`). RakuAST has no such split: the declaration *is* the
//! loop's condition (`Loop::Until(condition => VarDeclaration::Simple)`).
//!
//! [`expand`] builds the parser's shape, [`recognize`] takes it apart again and
//! hands back the loop with the declaration put back as its condition.

use super::{Expr, Stmt};
use crate::token_kind::TokenKind;

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
            custom_traits,
            ..
        } = boxed.as_ref()
        && !custom_traits.iter().any(|(t, _)| t == "__has_initializer")
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

/// The loop of a statement [`expand`] could have built, with its condition
/// replaced by the declaration it reads (`until my $x` is `While` over `!$x`
/// plus `is_until`).
// Cost: O(s), s = the size of the loop body (it is cloned into the result).
pub(crate) fn recognize(stmt: &Stmt) -> Option<Stmt> {
    let Stmt::SyntheticBlock(stmts) = stmt else {
        return None;
    };
    let [
        decl @ Stmt::VarDecl { name, .. },
        Stmt::While { cond, is_until, .. },
    ] = stmts.as_slice()
    else {
        return None;
    };
    let read = match cond {
        Expr::Unary {
            op: TokenKind::Bang,
            expr,
            ..
        } if *is_until => expr.as_ref(),
        other if !*is_until => other,
        _ => return None,
    };
    let reads_declared = match read {
        Expr::Var(n) => n == name,
        Expr::ArrayVar(n) | Expr::HashVar(n) => name.strip_prefix(['@', '%']) == Some(n.as_str()),
        _ => false,
    };
    if !reads_declared {
        return None;
    }
    let Some(Stmt::While { body, label, .. }) = stmts.get(1) else {
        return None;
    };
    let declared = Expr::DoStmt(Box::new(decl.clone()));
    Some(Stmt::While {
        cond: if *is_until {
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(declared),
                word: false,
            }
        } else {
            declared
        },
        body: body.clone(),
        label: label.clone(),
        is_statement_modifier: false,
        is_until: *is_until,
        is_bare_term: false,
    })
}
