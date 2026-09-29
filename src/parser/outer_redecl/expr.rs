//! Expression half of the declaration-scope walk (see the parent module).

use super::{Ctx, decl_key, ref_key, walk_scoped_body, walk_scoped_body_with_params, walk_stmt};
use crate::ast::Expr;

pub(super) fn walk_call_arg(arg: &crate::ast::CallArg, ctx: &mut Ctx) {
    use crate::ast::CallArg;
    match arg {
        CallArg::Positional(e) | CallArg::Slip(e) | CallArg::Invocant(e) => walk_expr(e, ctx),
        CallArg::Named { value, .. } => {
            if let Some(v) = value {
                walk_expr(v, ctx);
            }
        }
    }
}

pub(super) fn walk_expr(expr: &Expr, ctx: &mut Ctx) {
    match expr {
        Expr::Var(n) => {
            if let Some(k) = ref_key('$', n) {
                ctx.reference(k);
            }
        }
        Expr::ArrayVar(n) => {
            if let Some(k) = ref_key('@', n) {
                ctx.reference(k);
            }
        }
        Expr::HashVar(n) => {
            if let Some(k) = ref_key('%', n) {
                ctx.reference(k);
            }
        }

        // `do STMT` (statement form) shares the enclosing scope, so an inline
        // `do my $x = 5` declares in the current scope. `do { ... }` is a
        // separate `DoBlock` variant handled as a nested scope below.
        Expr::DoStmt(s) => walk_stmt(s, ctx),

        Expr::AssignExpr { name, expr, .. } => {
            if let Some(k) = decl_key(name) {
                ctx.reference(k);
            }
            walk_expr(expr, ctx);
        }

        // Body-bearing expressions open a new nested lexical scope.
        Expr::Block(body)
        | Expr::Gather(body)
        | Expr::DoBlock { body, .. }
        | Expr::Once { body }
        | Expr::PhaserExpr { body, .. } => walk_scoped_body(body, ctx),
        Expr::AnonSub { body, .. } => walk_scoped_body(body, ctx),
        Expr::AnonSubParams {
            body,
            params,
            param_defs,
            ..
        } => walk_scoped_body_with_params(body, params, param_defs, ctx),
        Expr::Lambda { param, body, .. } => {
            walk_scoped_body_with_params(body, std::slice::from_ref(param), &[], ctx)
        }
        // ADR-0033 Phase 1: an un-expanded WhateverCurry marker introduces no
        // named bindings of its own yet (its body still has literal `*`
        // placeholders, not the synthetic `__wc_N` params `build_closure`
        // assigns later) — so unlike `Lambda`/`AnonSubParams` it opens no new
        // scope here; walk its body transparently in the current scope so any
        // *other* variable reference inside it still gets shadow-checked.
        Expr::WhateverCurry(inner) => walk_expr(inner, ctx),
        Expr::Try { body, catch } => {
            walk_scoped_body(body, ctx);
            if let Some(c) = catch {
                walk_scoped_body(c, ctx);
            }
        }

        // Same-scope compound expressions: recurse into children.
        Expr::MethodCall { target, args, .. }
        | Expr::HyperMethodCall { target, args, .. }
        | Expr::CallOn { target, args } => {
            walk_expr(target, ctx);
            for a in args {
                walk_expr(a, ctx);
            }
        }
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            ..
        }
        | Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            ..
        } => {
            walk_expr(target, ctx);
            walk_expr(name_expr, ctx);
            for a in args {
                walk_expr(a, ctx);
            }
        }
        Expr::Call { args, .. } | Expr::UserRoutineCall { args, .. } => {
            for a in args {
                walk_expr(a, ctx);
            }
        }
        Expr::Binary { left, right, .. }
        | Expr::HyperOp { left, right, .. }
        | Expr::HyperFuncOp { left, right, .. }
        | Expr::MetaOp { left, right, .. } => {
            walk_expr(left, ctx);
            walk_expr(right, ctx);
        }
        // `todo/tickets/chained-compare-ast-node.md`: same-scope compound
        // expression, like `Binary` above.
        Expr::ChainedCompare { operands, .. } => {
            for o in operands {
                walk_expr(o, ctx);
            }
        }
        Expr::InfixFunc { left, right, .. } => {
            walk_expr(left, ctx);
            for r in right {
                walk_expr(r, ctx);
            }
        }
        Expr::Feed { source, sink, .. } => {
            walk_expr(source, ctx);
            walk_expr(sink, ctx);
        }
        Expr::Unary { expr, .. }
        | Expr::PostfixOp { expr, .. }
        | Expr::Eager(expr)
        | Expr::Itemize(expr)
        | Expr::DeitemizeForBind(expr)
        | Expr::Reduction { expr, .. }
        | Expr::ZenSlice(expr)
        | Expr::PositionalPair(expr)
        | Expr::IndirectTypeLookup(expr)
        | Expr::SymbolicDeref { expr, .. } => walk_expr(expr, ctx),
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            walk_expr(cond, ctx);
            walk_expr(then_expr, ctx);
            walk_expr(else_expr, ctx);
        }
        Expr::Index { target, index, .. } => {
            walk_expr(target, ctx);
            walk_expr(index, ctx);
        }
        Expr::MultiDimIndex {
            target, dimensions, ..
        } => {
            walk_expr(target, ctx);
            for d in dimensions {
                walk_expr(d, ctx);
            }
        }
        Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value,
            ..
        } => {
            walk_expr(target, ctx);
            for d in dimensions {
                walk_expr(d, ctx);
            }
            walk_expr(value, ctx);
        }
        Expr::IndexAssign {
            target,
            index,
            value,
            ..
        } => {
            walk_expr(target, ctx);
            walk_expr(index, ctx);
            walk_expr(value, ctx);
        }
        Expr::Exists { target, arg, .. } => {
            walk_expr(target, ctx);
            if let Some(a) = arg {
                walk_expr(a, ctx);
            }
        }
        Expr::SymbolicDerefAssign { expr, value, .. }
        | Expr::IndirectTypeLookupAssign { expr, value } => {
            walk_expr(expr, ctx);
            walk_expr(value, ctx);
        }
        Expr::HyperSlice { target, .. } => walk_expr(target, ctx),
        Expr::ArrayLiteral(items) | Expr::BracketArray(items, _) | Expr::CaptureLiteral(items) => {
            for it in items {
                walk_expr(it, ctx);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(v) = v {
                    walk_expr(v, ctx);
                }
            }
        }
        Expr::IndirectCodeLookup { package, .. } => walk_expr(package, ctx),
        Expr::Grouped(inner) => walk_expr(inner, ctx),
        Expr::StringInterpolation(parts) => {
            for p in parts {
                walk_expr(p, ctx);
            }
        }
        Expr::CompoundAssign { target, rhs, .. } => {
            walk_expr(target, ctx);
            walk_expr(rhs, ctx);
        }

        _ => {}
    }
}
