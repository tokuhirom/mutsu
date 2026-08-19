//! WhateverCode body construction: replacing `*` placeholders with parameter
//! variables (numbered or single `$_`).
//!
//! A nested, already-planted `Expr::WhateverCurry` operand (e.g. `(* - 1)`
//! inside `(* - 1) - 1`) is inlined by recursing straight into its un-curried
//! body — since that body still has literal `Expr::Whatever` placeholders (not
//! yet turned into `$_`/`__wc_N` variables), no renaming pass is needed. This
//! is simpler than the pre-ADR-0033 code, which had to unwrap an
//! already-built `Lambda`/`AnonSubParams` closure and rename its parameter(s)
//! to fit the enclosing numbering scheme.

use super::build::{count_whatever, exprs_structurally_eq};
use crate::ast::Expr;
use crate::parser::is_whatever;
use crate::token_kind::TokenKind;

/// Replace Whatever expressions with numbered parameter variables.
/// `counter` tracks the next parameter index to assign.
pub(crate) fn replace_whatever_numbered(expr: &Expr, counter: &mut usize) -> Expr {
    match expr {
        e if is_whatever(e) => {
            let var_name = format!("__wc_{counter}");
            *counter += 1;
            Expr::Var(var_name)
        }
        Expr::WhateverCurry(inner) => replace_whatever_numbered(inner, counter),
        Expr::Binary {
            left,
            op: TokenKind::AndAnd,
            right,
        } => {
            if let (
                Expr::Binary {
                    left: ll,
                    op: lop,
                    right: lr,
                },
                Expr::Binary {
                    left: rl,
                    op: rop,
                    right: rr,
                },
            ) = (left.as_ref(), right.as_ref())
                && exprs_structurally_eq(lr, rl)
                && count_whatever(lr) > 0
            {
                // Chained comparison expanded to `(ll OP m) && (m OP rr)`: assign
                // params left-to-right (ll, then the shared middle once, then rr)
                // and reuse the same replaced middle in both comparisons so each
                // distinct operand maps to its own positional argument.
                let new_ll = replace_whatever_numbered(ll, counter);
                let new_mid = replace_whatever_numbered(lr, counter);
                let new_rr = replace_whatever_numbered(rr, counter);
                return Expr::Binary {
                    left: Box::new(Expr::Binary {
                        left: Box::new(new_ll),
                        op: lop.clone(),
                        right: Box::new(new_mid.clone()),
                    }),
                    op: TokenKind::AndAnd,
                    right: Box::new(Expr::Binary {
                        left: Box::new(new_mid),
                        op: rop.clone(),
                        right: Box::new(new_rr),
                    }),
                };
            }
            Expr::Binary {
                left: Box::new(replace_whatever_numbered(left, counter)),
                op: TokenKind::AndAnd,
                right: Box::new(replace_whatever_numbered(right, counter)),
            }
        }
        // SmartMatch: only replace Whatever on LHS; RHS is handled at runtime.
        Expr::Binary {
            left,
            op: op @ TokenKind::SmartMatch,
            right,
        } => Expr::Binary {
            left: Box::new(replace_whatever_numbered(left, counter)),
            op: op.clone(),
            right: right.clone(),
        },
        Expr::Binary { left, op, right } => Expr::Binary {
            left: Box::new(replace_whatever_numbered(left, counter)),
            op: op.clone(),
            right: Box::new(replace_whatever_numbered(right, counter)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: op.clone(),
            expr: Box::new(replace_whatever_numbered(expr, counter)),
        },
        Expr::PostfixOp { op, expr } => Expr::PostfixOp {
            op: op.clone(),
            expr: Box::new(replace_whatever_numbered(expr, counter)),
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => Expr::MethodCall {
            target: Box::new(replace_whatever_numbered(target, counter)),
            name: *name,
            args: args.clone(),
            modifier: *modifier,
            quoted: *quoted,
        },
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            modifier,
            quoted,
        } => Expr::DynamicMethodCall {
            target: Box::new(replace_whatever_numbered(target, counter)),
            name_expr: name_expr.clone(),
            args: args.clone(),
            modifier: *modifier,
            quoted: *quoted,
        },
        // A hyper method call curries on its target exactly like a plain one.
        Expr::HyperMethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => Expr::HyperMethodCall {
            target: Box::new(replace_whatever_numbered(target, counter)),
            name: *name,
            args: args.clone(),
            modifier: *modifier,
            quoted: *quoted,
        },
        Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            modifier,
        } => Expr::HyperMethodCallDynamic {
            target: Box::new(replace_whatever_numbered(target, counter)),
            name_expr: name_expr.clone(),
            args: args.clone(),
            modifier: *modifier,
        },
        // Only the *target* of an invocation curries; a Whatever passed as a call
        // *argument* stays a Whatever value (see `count_whatever`/`contains_whatever`),
        // so the args are left untouched rather than replaced by numbered params.
        Expr::CallOn { target, args } => Expr::CallOn {
            target: Box::new(replace_whatever_numbered(target, counter)),
            args: args.clone(),
        },
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => Expr::Index {
            target: Box::new(replace_whatever_numbered(target, counter)),
            index: index.clone(),
            is_positional: *is_positional,
        },
        Expr::InfixFunc {
            name,
            left,
            right,
            modifier,
        } => Expr::InfixFunc {
            name: name.clone(),
            left: Box::new(replace_whatever_numbered(left, counter)),
            right: right
                .iter()
                .map(|a| replace_whatever_numbered(a, counter))
                .collect(),
            modifier: modifier.clone(),
        },
        Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } => Expr::MetaOp {
            meta: meta.clone(),
            op: op.clone(),
            left: Box::new(replace_whatever_numbered(left, counter)),
            right: Box::new(replace_whatever_numbered(right, counter)),
        },
        _ => expr.clone(),
    }
}

/// Replace Whatever and nested single-arg WhateverCode with $_ (for single-arg wrapping).
pub(crate) fn replace_whatever_single(expr: &Expr) -> Expr {
    match expr {
        e if is_whatever(e) => Expr::Var("_".to_string()),
        Expr::WhateverCurry(inner) => replace_whatever_single(inner),
        // SmartMatch: only replace Whatever on LHS; RHS is handled at runtime.
        Expr::Binary {
            left,
            op: op @ TokenKind::SmartMatch,
            right,
        } => Expr::Binary {
            left: Box::new(replace_whatever_single(left)),
            op: op.clone(),
            right: right.clone(),
        },
        Expr::Binary { left, op, right } => Expr::Binary {
            left: Box::new(replace_whatever_single(left)),
            op: op.clone(),
            right: Box::new(replace_whatever_single(right)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: op.clone(),
            expr: Box::new(replace_whatever_single(expr)),
        },
        Expr::PostfixOp { op, expr } => Expr::PostfixOp {
            op: op.clone(),
            expr: Box::new(replace_whatever_single(expr)),
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => Expr::MethodCall {
            target: Box::new(replace_whatever_single(target)),
            name: *name,
            args: args.clone(),
            modifier: *modifier,
            quoted: *quoted,
        },
        Expr::DynamicMethodCall {
            target,
            name_expr,
            args,
            modifier,
            quoted,
        } => Expr::DynamicMethodCall {
            target: Box::new(replace_whatever_single(target)),
            name_expr: name_expr.clone(),
            args: args.clone(),
            modifier: *modifier,
            quoted: *quoted,
        },
        // A hyper method call curries on its target exactly like a plain one.
        Expr::HyperMethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => Expr::HyperMethodCall {
            target: Box::new(replace_whatever_single(target)),
            name: *name,
            args: args.clone(),
            modifier: *modifier,
            quoted: *quoted,
        },
        Expr::HyperMethodCallDynamic {
            target,
            name_expr,
            args,
            modifier,
        } => Expr::HyperMethodCallDynamic {
            target: Box::new(replace_whatever_single(target)),
            name_expr: name_expr.clone(),
            args: args.clone(),
            modifier: *modifier,
        },
        // Only the target curries; a Whatever call *argument* stays a value.
        Expr::CallOn { target, args } => Expr::CallOn {
            target: Box::new(replace_whatever_single(target)),
            args: args.clone(),
        },
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => Expr::Index {
            target: Box::new(replace_whatever_single(target)),
            index: index.clone(),
            is_positional: *is_positional,
        },
        Expr::InfixFunc {
            name,
            left,
            right,
            modifier,
        } => Expr::InfixFunc {
            name: name.clone(),
            left: Box::new(replace_whatever_single(left)),
            right: right.iter().map(replace_whatever_single).collect(),
            modifier: modifier.clone(),
        },
        Expr::MetaOp {
            meta,
            op,
            left,
            right,
        } => Expr::MetaOp {
            meta: meta.clone(),
            op: op.clone(),
            left: Box::new(replace_whatever_single(left)),
            right: Box::new(replace_whatever_single(right)),
        },
        _ => expr.clone(),
    }
}
