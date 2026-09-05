//! Post-parse `*` leaf classifier (ADR-0033 Phase 2).
//!
//! Rewrites every `Expr::Whatever` in a freshly-parsed program to
//! `Expr::WhateverArg` unless its immediate syntactic parent puts it in one of
//! the *value* positions Rakudo's `RakuAST::Term::Whatever` occupies (measured
//! in the ADR's section 2.1 table: a comma operand, a range/series endpoint, an
//! `xx` operand, an assignment/bind RHS, a call/method argument, a whole-slice
//! subscript, a non-currying pseudo-method target, a bareword pair value, or a
//! bare `*` standing alone as a whole statement/grouping). Everything else is
//! `WhateverCode::Argument`.
//!
//! This is a **pure annotation**: per the ADR's section 2.2 invariant, nothing
//! outside `src/rakuast/` may branch on which of the two leaf variants a `*`
//! got — `crate::parser::is_whatever` treats them identically, so a
//! misclassification here can only produce a wrong `.AST` gist, never a wrong
//! program result. The rule is intentionally syntactic and scope-independent
//! (section 2.1): it does not ask "does this subtree curry" (`x`'s `1 x *`
//! curries nothing structurally yet is still `Argument`; a Z/X meta-op operand
//! is never wrapped in a `WhateverCurry` yet is still `Argument`).
//!
//! This module is deliberately the seed of the `plant()` scope-authority
//! function ADR-0033's section 4 calls for (Phase 4): same top-down traversal,
//! same parent-context switch — one phase later it also decides where a
//! priming *scope* begins, not just how a leaf renders.
//!
//! Split into `stmt.rs` (statement-level recursion) and `expr.rs`
//! (expression-level recursion, where the actual leaf-classification table
//! lives) to stay under the repo's 500-line-per-file convention.

mod expr;
mod stmt;

use crate::ast::{Expr, Stmt};

/// Entry point, invoked once from `parser::parse_program` after a program
/// parses successfully.
pub(crate) fn mark_program(stmts: &mut [Stmt]) {
    mark_stmts(stmts);
}

fn mark_stmts(stmts: &mut [Stmt]) {
    for stmt in stmts {
        stmt::mark_stmt(stmt);
    }
}

/// A `*` reached through this call stays a value (`Term::Whatever`) if it is
/// exactly a bare `Expr::Whatever` leaf; anything else (including an
/// already-planted `WhateverCurry`) recurses normally, so a *compound*
/// expression occupying the same syntactic slot (`1..*-1`, `@a[*-1]`) still
/// gets its inner `*` classified as `Argument`.
fn mark_value_leaf(expr: &mut Expr) {
    if matches!(expr, Expr::Whatever) {
        return;
    }
    expr::mark_expr(expr);
}

/// A routine's parameters. Only the two expression-valued fields can hold a
/// `*`: a default (`$x = *`) and a `where` constraint (`$x where * > 0`).
///
/// The walk used to stop at a routine's body, so a `*` in either position kept
/// the value-leaf classification it was parsed with. That was invisible while
/// the converter refused a where-constrained parameter outright; now that it
/// renders one, a mis-classified leaf would show up as a `Term::Whatever` where
/// raku has a `WhateverCode::Argument`.
pub(super) fn mark_param_defs(param_defs: &mut [crate::ast::ParamDef]) {
    for pd in param_defs {
        mark_opt_value_leaf(&mut pd.default);
        mark_opt_box_expr(&mut pd.where_constraint);
    }
}

fn mark_opt_expr(expr: &mut Option<Expr>) {
    if let Some(e) = expr {
        expr::mark_expr(e);
    }
}

fn mark_opt_value_leaf(expr: &mut Option<Expr>) {
    if let Some(e) = expr {
        mark_value_leaf(e);
    }
}

fn mark_opt_box_expr(expr: &mut Option<Box<Expr>>) {
    if let Some(e) = expr {
        expr::mark_expr(e);
    }
}
