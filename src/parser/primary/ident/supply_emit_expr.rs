//! Rewrites `emit(...)` calls that sit *inside an expression* of a `supply { … }`
//! body to `$__mutsu_supply_emitter_N.emit(...)`.
//!
//! `rewrite_supply_stmt` (see the parent module) only recognised `emit` as a
//! whole statement, so an `emit` used as a sub-expression —
//! `$x ~~ Foo ?? emit($x) !! die "…"`, exactly what
//! `Cro::HTTP::Middleware::Request.transformer` writes — stayed a bare call and
//! fell back to the *dynamic* emitter stack at runtime. When the supply block is
//! one stage of a pipeline, the dynamically innermost emitter is a neighbouring
//! stage's, so the value skipped the rest of the pipeline.
//!
//! The walk deliberately stops at closure boundaries (`AnonSub`, `Lambda`,
//! `AnonSubParams`): an `emit` inside a closure the body merely *builds* runs
//! wherever that closure is later called, which is what the dynamic stack is for
//! (and rewriting it would hit the closure-capture gap documented on
//! `rewrite_supply_stmt`). Statement-bearing inline blocks are handed back to
//! `rewrite_supply_body`, which is this walker's statement twin.
//!
//! Unhandled `Expr` shapes are returned untouched, so the worst case is the
//! pre-existing dynamic-emitter behaviour, never a miscompile.

use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;

use super::supply::rewrite_supply_body;

/// `emit(ARGS)` -> `$emitter.emit(ARGS)`.
fn emitter_call(emitter_name: &str, args: Vec<Expr>) -> Expr {
    Expr::MethodCall {
        target: Box::new(Expr::Var(emitter_name.to_string())),
        name: Symbol::intern("emit"),
        args,
        modifier: None,
        quoted: false,
    }
}

fn rewrite_boxed(expr: Expr, emitter: &str) -> Box<Expr> {
    Box::new(rewrite_expr(expr, emitter))
}

fn rewrite_all(exprs: Vec<Expr>, emitter: &str) -> Vec<Expr> {
    exprs
        .into_iter()
        .map(|e| rewrite_expr(e, emitter))
        .collect()
}

fn rewrite_stmts(body: Vec<Stmt>, emitter: &str) -> Vec<Stmt> {
    rewrite_supply_body(body, emitter)
}

pub(crate) fn rewrite_expr(expr: Expr, emitter: &str) -> Expr {
    match expr {
        Expr::Call { name, args } if name.resolve().as_str() == "emit" => {
            emitter_call(emitter, rewrite_all(args, emitter))
        }
        Expr::Call { name, args } => Expr::Call {
            name,
            args: rewrite_all(args, emitter),
        },
        Expr::UserRoutineCall { name, args } => Expr::UserRoutineCall {
            name,
            args: rewrite_all(args, emitter),
        },
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => Expr::Ternary {
            cond: rewrite_boxed(*cond, emitter),
            then_expr: rewrite_boxed(*then_expr, emitter),
            else_expr: rewrite_boxed(*else_expr, emitter),
        },
        Expr::Binary { left, op, right } => Expr::Binary {
            left: rewrite_boxed(*left, emitter),
            op,
            right: rewrite_boxed(*right, emitter),
        },
        Expr::Grouped(e) => Expr::Grouped(rewrite_boxed(*e, emitter)),
        Expr::Unary { op, expr } => Expr::Unary {
            op,
            expr: rewrite_boxed(*expr, emitter),
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => Expr::MethodCall {
            target: rewrite_boxed(*target, emitter),
            name,
            args: rewrite_all(args, emitter),
            modifier,
            quoted,
        },
        // Inline blocks run in this frame, so their `emit`s belong to this
        // supply — hand them back to the statement rewriter.
        Expr::Block(body) => Expr::Block(rewrite_stmts(body, emitter)),
        Expr::DoBlock { body, label } => Expr::DoBlock {
            body: rewrite_stmts(body, emitter),
            label,
        },
        other => other,
    }
}
