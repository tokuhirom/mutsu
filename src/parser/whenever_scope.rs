//! Post-parse check for `whenever` blocks that appear outside the lexical scope
//! of a `react` / `supply` block.
//!
//! Rakudo raises a compile-time `X::Comp::WheneverOutOfScope`
//! ("Cannot have a 'whenever' block outside the scope of a 'supply' or 'react'
//! block") when a `whenever` is not lexically enclosed by a `react` block or a
//! `supply` block. Crossing *any* routine boundary (a `sub`/`method`/pointy or
//! anonymous closure) breaks that enclosure — only inline blocks (`for`, `if`,
//! bare `{ }`, a `whenever` body, etc.) preserve it.
//!
//! This walker mirrors that rule structurally on the AST. It tracks a single
//! boolean `in_scope` ("are we lexically inside a react/supply block, without
//! having crossed a routine boundary?"):
//!
//! * `react { ... }` sets it true for the body.
//! * The emitter closure that `supply { ... }` lowers to (a `Lambda` whose
//!   parameter name starts with `__mutsu_supply_emitter_`) sets it true.
//! * A genuine routine boundary (named `sub`/`method` decl, `sub { }`, pointy
//!   or parameterised closure, and any non-emitter `Lambda`) resets it to false.
//! * Every other construct preserves it.
//!
//! When a `whenever` is reached with `in_scope == false`, we record its line.
//!
//! The walker is intentionally conservative: an unhandled container is simply
//! not recursed into, which can only *miss* an out-of-scope `whenever` (a false
//! negative). A legitimate `whenever` inside a `react`/`supply` block is never
//! reachable behind a routine-boundary reset, so this can never produce a false
//! positive that would reject valid concurrency code.

use crate::ast::{Expr, Stmt};

const SUPPLY_EMITTER_PREFIX: &str = "__mutsu_supply_emitter_";

/// Returns the 1-based source line of the first `whenever` block found outside
/// the scope of a `react`/`supply` block, or `None` if every `whenever` is
/// properly scoped.
pub(crate) fn find_out_of_scope_whenever(stmts: &[Stmt]) -> Option<i64> {
    let mut line = 0i64;
    let mut found: Option<i64> = None;
    walk_stmts(stmts, false, &mut line, &mut found);
    found
}

fn walk_stmts(stmts: &[Stmt], in_scope: bool, line: &mut i64, found: &mut Option<i64>) {
    for s in stmts {
        walk_stmt(s, in_scope, line, found);
    }
}

fn walk_stmt(stmt: &Stmt, in_scope: bool, line: &mut i64, found: &mut Option<i64>) {
    match stmt {
        Stmt::SetLine(n) => *line = *n,

        // Scope openers.
        Stmt::React { body } => walk_stmts(body, true, line, found),
        Stmt::Whenever { supply, body, .. } => {
            if !in_scope && found.is_none() {
                *found = Some(*line);
            }
            walk_expr(supply, in_scope, line, found);
            // A whenever body is itself inside a react/supply scope, and nested
            // `whenever` blocks are allowed there.
            walk_stmts(body, true, line, found);
        }

        // Routine / package boundaries: reset scope for the body.
        Stmt::SubDecl { body, .. }
        | Stmt::MethodDecl { body, .. }
        | Stmt::ClassDecl { body, .. }
        | Stmt::RoleDecl { body, .. }
        | Stmt::Package { body, .. } => {
            walk_stmts(body, false, line, found);
        }

        // Inline blocks / control flow: preserve scope.
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::Given { body, .. }
        | Stmt::When { body, .. }
        | Stmt::While { body, .. }
        | Stmt::Subtest { body, .. } => walk_stmts(body, in_scope, line, found),
        Stmt::For { body, iterable, .. } => {
            walk_expr(iterable, in_scope, line, found);
            walk_stmts(body, in_scope, line, found);
        }
        Stmt::Loop { body, init, .. } => {
            if let Some(init) = init {
                walk_stmt(init, in_scope, line, found);
            }
            walk_stmts(body, in_scope, line, found);
        }
        Stmt::If {
            then_branch,
            else_branch,
            ..
        } => {
            walk_stmts(then_branch, in_scope, line, found);
            walk_stmts(else_branch, in_scope, line, found);
        }
        Stmt::Label { stmt, .. } => walk_stmt(stmt, in_scope, line, found),

        // Expression-bearing statements: descend into the expression to reach
        // `supply { }` emitters and closures.
        Stmt::Expr(e) | Stmt::VarDecl { expr: e, .. } | Stmt::Assign { expr: e, .. } => {
            walk_expr(e, in_scope, line, found);
        }
        Stmt::Take(e, _) => walk_expr(e, in_scope, line, found),

        _ => {}
    }
}

fn walk_expr(expr: &Expr, in_scope: bool, line: &mut i64, found: &mut Option<i64>) {
    match expr {
        // The `supply { }` sugar lowers to `Supply.on-demand(-> $emitter { ... })`
        // where the emitter closure name marks a genuine supply scope.
        Expr::Lambda { param, body, .. } => {
            let opens_scope = param.starts_with(SUPPLY_EMITTER_PREFIX);
            walk_stmts(body, opens_scope, line, found);
        }
        // `sub { }` (is_block == false) is a routine boundary; a bare block
        // `{ }` (is_block == true) is inline and preserves scope.
        Expr::AnonSub { body, is_block, .. } => {
            walk_stmts(body, in_scope && *is_block, line, found);
        }
        Expr::AnonSubParams { body, .. } => walk_stmts(body, false, line, found),

        // Inline block-valued expressions preserve scope.
        Expr::Block(body) | Expr::Gather(body) => walk_stmts(body, in_scope, line, found),
        Expr::DoBlock { body, .. } => walk_stmts(body, in_scope, line, found),
        Expr::DoStmt(s) => walk_stmt(s, in_scope, line, found),

        // Call / method-call arguments may carry the supply emitter closure or
        // other block arguments.
        Expr::Call { args, .. } => {
            for a in args {
                walk_expr(a, in_scope, line, found);
            }
        }
        Expr::MethodCall { target, args, .. } | Expr::HyperMethodCall { target, args, .. } => {
            walk_expr(target, in_scope, line, found);
            for a in args {
                walk_expr(a, in_scope, line, found);
            }
        }

        _ => {}
    }
}
