//! The one lowering of an assignment target to the expression the compiler runs
//! (ADR-15334 slice 1): the general target dispatch [`assign_to_target_expr`]
//! that [`super::assign_lower`] falls back to, over the writebacks of
//! [`super::assign_writeback`].

use super::assign_writeback::{
    dynamic_method_lvalue_assign_expr, method_lvalue_assign_expr, named_sub_lvalue_assign_expr,
};
use super::assignment_ro_expr;
use crate::ast::{Expr, Stmt};
use crate::parser::method_lvalue_target_name;
use crate::symbol::Symbol;

pub(crate) fn unwrap_grouped_lvalue(target: Expr) -> Expr {
    match target {
        Expr::Grouped(inner) => unwrap_grouped_lvalue(*inner),
        // A source-preserving literal is an immutable lvalue exactly like the
        // plain literal it wraps; the sink-warn source text is irrelevant here.
        Expr::LiteralSrc(v, _) => Expr::Literal(v),
        // `$(EXPR) = v` decides on the `.item` call the contextualizer is.
        contextualizer @ Expr::Contextualizer { .. } => contextualizer.contextualizer_call(),
        other => other,
    }
}

pub(crate) fn assign_to_target_expr(target: Expr, value: Expr) -> Expr {
    match unwrap_grouped_lvalue(target) {
        Expr::Var(name) => Expr::AssignExpr {
            name,
            expr: Box::new(value),
            is_bind: false,
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(value),
            is_bind: false,
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(value),
            is_bind: false,
        },
        Expr::Index {
            target,
            index,
            is_positional,
            spelling,
            ..
        } => Expr::IndexAssign {
            target,
            index,
            value: Box::new(value),
            is_positional,
            spelling,
        },
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value: Box::new(value),
            is_positional,
        },
        // A method-call lvalue (`$o.a = v` — an rw accessor, or `$o.AT-POS(i) = v`).
        // Mirrors the statement-level `lvalue_assign_to_expr` so a chained assignment
        // whose middle term is a method call (`my $c = $o.a = 42`, `say($o.a = 42)`)
        // lowers to the `__mutsu_assign_method_lvalue` writeback instead of being
        // left unconsumed (which surfaced as a `Confused` parse error).
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            ..
        } => {
            // Match the expression-context lowering in `paren.rs` (NOT the
            // statement-level `lvalue_assign_to_expr`, which drops a
            // `BareWord` target to `None`): a sigilless raw binding (`\h`)
            // reaches here as a `BareWord`, and the writeback needs its name
            // so `h.AT-KEY(k) = v` mutates the bound container in place.
            // The write-back name is a property of what the parentheses
            // hold: `(my $x = $s).substr-rw(...) = $c` writes back through
            // `$x`.
            let target_var_name = method_lvalue_target_name(&target);
            let method_name = if let Some(m @ ('!' | '^')) = modifier {
                format!("{m}{}", name.resolve())
            } else {
                name.resolve()
            };
            method_lvalue_assign_expr(*target, target_var_name, method_name, args, value)
        }
        // An indirect method-call lvalue (`$o."$name"() = v`) in expression
        // position, e.g. the middle term of a chained assignment. Mirror the
        // `MethodCall` arm but with a runtime-computed name (see
        // `dynamic_method_lvalue_assign_expr`).
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            modifier,
            ..
        } => {
            let target_var_name = method_lvalue_target_name(&target);
            dynamic_method_lvalue_assign_expr(
                *target,
                target_var_name,
                *name_expr,
                modifier,
                args,
                value,
            )
        }
        Expr::Call { name, args, .. } => named_sub_lvalue_assign_expr(name.resolve(), args, value),
        Expr::CallOn { target, args } => Expr::Call {
            name: Symbol::intern("__mutsu_assign_callable_lvalue"),
            args: vec![*target, Expr::ArrayLiteral(args), value],
            listop: false,
        },
        // A compound assignment is transparent to the execution AST marker.
        // When its result is itself used as an lvalue (for example the RHS of
        // a reverse meta compound assignment), apply the writeback to the
        // established expansion so short-circuiting still preserves the
        // original assignment semantics.
        Expr::CompoundAssign { expanded, .. } => assign_to_target_expr(*expanded, value),
        // `(cond ?? $a !! $b) = rhs`: the ternary selects an lvalue branch; the
        // assignment writes through to whichever branch is taken. Desugar to
        // `cond ?? ($a = rhs) !! ($b = rhs)`. A non-lvalue branch recurses to the
        // RO-error `other` arm, which raises only when that branch is selected.
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => Expr::Ternary {
            cond,
            then_expr: Box::new(assign_to_target_expr(*then_expr, value.clone())),
            else_expr: Box::new(assign_to_target_expr(*else_expr, value)),
        },
        Expr::SymbolicDeref { sigil, expr } => Expr::SymbolicDerefAssign {
            sigil,
            expr,
            value: Box::new(value),
        },
        Expr::IndirectTypeLookup(inner) => Expr::IndirectTypeLookupAssign {
            expr: inner,
            value: Box::new(value),
        },
        Expr::DoStmt(stmt) => {
            if let Stmt::VarDecl {
                name,
                type_constraint,
                is_state,
                is_our,
                is_dynamic,
                is_export,
                export_tags,
                custom_traits,
                marks,
                where_constraint,
                ..
            } = *stmt
            {
                Expr::DoStmt(Box::new(Stmt::VarDecl {
                    marks,
                    anonymous: false,
                    name,
                    expr: value,
                    type_constraint,
                    is_state,
                    is_our,
                    is_dynamic,
                    is_export,
                    export_tags,
                    custom_traits,
                    has_initializer: false,
                    constant: None,
                    bind: None,
                    where_constraint,
                }))
            } else {
                assignment_ro_expr(Expr::DoStmt(stmt), value)
            }
        }
        other => assignment_ro_expr(other, value),
    }
}
