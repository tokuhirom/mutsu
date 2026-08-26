//! Statement-level recursion for the `*` leaf classifier (see `super`'s module
//! doc). Most of this file exists to reach every `Expr` a statement can embed;
//! the interesting classification table lives in `expr.rs`.

use super::{mark_opt_box_expr, mark_opt_expr, mark_opt_value_leaf, mark_stmts, mark_value_leaf};
use crate::ast::{CallArg, Stmt};

pub(super) fn mark_stmt(stmt: &mut Stmt) {
    match stmt {
        // A bare `*` standing alone as a whole statement (`*;`, a proto's
        // `{*}` body) stays a value; anything else recurses as normal.
        Stmt::Expr(e) => mark_value_leaf(e),
        Stmt::VarDecl {
            expr,
            custom_traits,
            where_constraint,
            ..
        } => {
            // `my $x = *` / `my $x := *` — assignment/bind RHS.
            mark_value_leaf(expr);
            for (_, arg) in custom_traits {
                mark_opt_expr(arg);
            }
            mark_opt_box_expr(where_constraint);
        }
        Stmt::Assign { expr, .. } => mark_value_leaf(expr),
        Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Take(e, _) | Stmt::Goto(e) => {
            super::expr::mark_expr(e);
        }
        Stmt::Say(args) | Stmt::Put(args) | Stmt::Print(args) | Stmt::Note(args) => {
            for a in args {
                mark_value_leaf(a);
            }
        }
        Stmt::Call { args, .. } => {
            for a in args {
                mark_call_arg(a);
            }
        }
        Stmt::For { iterable, body, .. } => {
            super::expr::mark_expr(iterable);
            mark_stmts(body);
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            super::expr::mark_expr(cond);
            mark_stmts(then_branch);
            mark_stmts(else_branch);
        }
        Stmt::While { cond, body, .. } => {
            super::expr::mark_expr(cond);
            mark_stmts(body);
        }
        Stmt::Loop {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(init) = init {
                mark_stmt(init);
            }
            mark_opt_expr(cond);
            mark_opt_expr(step);
            mark_stmts(body);
        }
        Stmt::Given { topic, body, .. } => {
            super::expr::mark_expr(topic);
            mark_stmts(body);
        }
        Stmt::When { cond, body, .. } => {
            super::expr::mark_expr(cond);
            mark_stmts(body);
        }
        Stmt::Whenever { supply, body, .. } => {
            super::expr::mark_expr(supply);
            mark_stmts(body);
        }
        Stmt::Subtest { name, body } => {
            super::expr::mark_expr(name);
            mark_stmts(body);
        }
        Stmt::Label { stmt, .. } => mark_stmt(stmt),
        Stmt::Let { index, value, .. } => {
            if let Some(index) = index {
                super::expr::mark_expr(index);
            }
            if let Some(value) = value {
                mark_value_leaf(value);
            }
        }
        Stmt::TempMethodAssign {
            method_args, value, ..
        } => {
            for a in method_args {
                mark_value_leaf(a);
            }
            mark_value_leaf(value);
        }
        Stmt::HasDecl {
            default,
            is_default,
            where_constraint,
            unknown_traits,
            ..
        } => {
            mark_opt_value_leaf(default);
            mark_opt_value_leaf(is_default);
            mark_opt_box_expr(where_constraint);
            for (_, _, arg) in unknown_traits {
                mark_opt_expr(arg);
            }
        }
        Stmt::SubsetDecl { predicate, .. } => mark_opt_expr(predicate),
        Stmt::DoesDecl { args, .. } => {
            if let Some(args) = args {
                for a in args {
                    super::expr::mark_expr(a);
                }
            }
        }
        Stmt::EnumDecl { variants, .. } => {
            for (_, value) in variants {
                mark_opt_expr(value);
            }
        }
        Stmt::Use { arg, condition, .. } => {
            mark_opt_expr(arg);
            mark_opt_box_expr(condition);
        }
        Stmt::No { arg, .. } => mark_opt_expr(arg),
        // Body-only statements: recurse into the block, nothing else to mark.
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::React { body }
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::Phaser { body, .. }
        | Stmt::Package { body, .. }
        | Stmt::SubDecl { body, .. }
        | Stmt::TokenDecl { body, .. }
        | Stmt::RuleDecl { body, .. }
        | Stmt::MethodDecl { body, .. }
        | Stmt::RoleDecl { body, .. }
        | Stmt::ClassDecl { body, .. }
        | Stmt::AugmentClass { body, .. }
        | Stmt::ProtoDecl { body, .. } => mark_stmts(body),
        // Declarations/markers/control-flow with no expression payload
        // relevant to Whatever-priming (or genuinely rare enough that a
        // missed leaf here is only a cosmetic `.AST` gap, never a runtime
        // behaviour change — see the module doc's safety invariant).
        Stmt::MarkReadonly(..)
        | Stmt::MarkBoundContainer(_)
        | Stmt::MarkBind
        | Stmt::MarkSigillessReadonly(_)
        | Stmt::MarkSigilless(_)
        | Stmt::ProtoToken { .. }
        | Stmt::Need { .. }
        | Stmt::Import { .. }
        | Stmt::Last(_)
        | Stmt::Next(_)
        | Stmt::Redo(_)
        | Stmt::Proceed
        | Stmt::Succeed
        | Stmt::ReactDone
        | Stmt::SupplyBodyDone
        | Stmt::TrustsDecl { .. }
        | Stmt::SetLine(_) => {}
    }
}

fn mark_call_arg(arg: &mut CallArg) {
    match arg {
        CallArg::Positional(e) | CallArg::Slip(e) | CallArg::Invocant(e) => mark_value_leaf(e),
        CallArg::Named { value, .. } => mark_opt_value_leaf(value),
    }
}
