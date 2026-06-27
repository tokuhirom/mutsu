//! WhateverCode body construction: replacing `*` placeholders with parameter
//! variables (numbered or single `$_`) and renaming variables when inlining
//! nested WhateverCode closures.

use super::*;

/// Replace Whatever expressions with numbered parameter variables.
/// `counter` tracks the next parameter index to assign.
pub(crate) fn replace_whatever_numbered(expr: &Expr, counter: &mut usize) -> Expr {
    match expr {
        e if is_whatever(e) => {
            let var_name = format!("__wc_{}", counter);
            *counter += 1;
            Expr::Var(var_name)
        }
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
        // Unwrap a nested WhateverCode lambda: reuse its body with renumbered params
        Expr::Lambda { param, body, .. } if param == "_" => {
            // Single-param WhateverCode: rename $_ to the next numbered param
            let var_name = format!("__wc_{}", counter);
            *counter += 1;
            if let Some(Stmt::Expr(e)) = body.first() {
                rename_var(e, "_", &var_name)
            } else {
                Expr::Var(var_name)
            }
        }
        Expr::AnonSubParams { params, body, .. }
            if params.iter().all(|p| p.starts_with("__wc_")) =>
        {
            // Multi-param WhateverCode: renumber all params
            let mut renames = Vec::new();
            for old_name in params {
                let new_name = format!("__wc_{}", counter);
                *counter += 1;
                renames.push((old_name.clone(), new_name));
            }
            let mut body_expr = if let Some(Stmt::Expr(e)) = body.first() {
                e.clone()
            } else {
                return expr.clone();
            };
            for (old, new) in &renames {
                body_expr = rename_var(&body_expr, old, new);
            }
            body_expr
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
        } => Expr::DynamicMethodCall {
            target: Box::new(replace_whatever_numbered(target, counter)),
            name_expr: name_expr.clone(),
            args: args.clone(),
            modifier: *modifier,
        },
        Expr::CallOn { target, args } => Expr::CallOn {
            target: Box::new(replace_whatever_numbered(target, counter)),
            args: args
                .iter()
                .map(|a| replace_whatever_numbered(a, counter))
                .collect(),
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

/// Rename a variable in an expression tree.
fn rename_var(expr: &Expr, old_name: &str, new_name: &str) -> Expr {
    match expr {
        Expr::Var(name) if name == old_name => Expr::Var(new_name.to_string()),
        Expr::Binary { left, op, right } => Expr::Binary {
            left: Box::new(rename_var(left, old_name, new_name)),
            op: op.clone(),
            right: Box::new(rename_var(right, old_name, new_name)),
        },
        Expr::Unary { op, expr } => Expr::Unary {
            op: op.clone(),
            expr: Box::new(rename_var(expr, old_name, new_name)),
        },
        Expr::PostfixOp { op, expr } => Expr::PostfixOp {
            op: op.clone(),
            expr: Box::new(rename_var(expr, old_name, new_name)),
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            quoted,
        } => Expr::MethodCall {
            target: Box::new(rename_var(target, old_name, new_name)),
            name: *name,
            args: args
                .iter()
                .map(|a| rename_var(a, old_name, new_name))
                .collect(),
            modifier: *modifier,
            quoted: *quoted,
        },
        Expr::CallOn { target, args } => Expr::CallOn {
            target: Box::new(rename_var(target, old_name, new_name)),
            args: args
                .iter()
                .map(|a| rename_var(a, old_name, new_name))
                .collect(),
        },
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => Expr::Index {
            target: Box::new(rename_var(target, old_name, new_name)),
            index: Box::new(rename_var(index, old_name, new_name)),
            is_positional: *is_positional,
        },
        _ => expr.clone(),
    }
}

/// Replace Whatever and nested single-arg WhateverCode with $_ (for single-arg wrapping).
pub(crate) fn replace_whatever_single(expr: &Expr) -> Expr {
    match expr {
        e if is_whatever(e) => Expr::Var("_".to_string()),
        // A nested single-arg WhateverCode: unwrap and reuse body (already uses $_)
        Expr::Lambda { param, body, .. } if param == "_" => {
            if let Some(Stmt::Expr(e)) = body.first() {
                e.clone()
            } else {
                Expr::Var("_".to_string())
            }
        }
        // A nested single-param WhateverCode built as AnonSubParams (its body
        // referenced $_, so it uses a numbered param): rename that param to $_
        // so it shares the composed closure's single topic.
        Expr::AnonSubParams {
            params,
            body,
            is_whatever_code: true,
            ..
        } if params.len() == 1 => {
            if let Some(Stmt::Expr(e)) = body.first() {
                rename_var(e, &params[0], "_")
            } else {
                Expr::Var("_".to_string())
            }
        }
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
        } => Expr::DynamicMethodCall {
            target: Box::new(replace_whatever_single(target)),
            name_expr: name_expr.clone(),
            args: args.clone(),
            modifier: *modifier,
        },
        Expr::CallOn { target, args } => Expr::CallOn {
            target: Box::new(replace_whatever_single(target)),
            args: args.iter().map(replace_whatever_single).collect(),
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
