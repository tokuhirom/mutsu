use super::*;

fn compound_index_assign_expr<F>(target: Expr, index: Expr, build_assigned_value: F) -> Expr
where
    F: FnOnce(Expr) -> Expr,
{
    let tmp_idx = format!(
        "__mutsu_idx_{}",
        TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
    );
    let tmp_idx_expr = Expr::Var(tmp_idx.clone());
    let lhs_expr = Expr::Index {
        target: Box::new(target.clone()),
        index: Box::new(tmp_idx_expr.clone()),
        is_positional: false,
    };
    let assigned_value = build_assigned_value(lhs_expr);
    Expr::DoBlock {
        body: vec![
            Stmt::VarDecl {
                name: tmp_idx.clone(),
                expr: index,
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            },
            Stmt::Expr(Expr::IndexAssign {
                target: Box::new(target),
                index: Box::new(tmp_idx_expr.clone()),
                value: Box::new(assigned_value),
                is_positional: true,
            }),
        ],
        label: None,
    }
}

pub(crate) fn build_compound_assign_expr(
    lhs: Expr,
    op: CompoundAssignOp,
    rhs: Expr,
) -> Result<Expr, PError> {
    let lhs = match lhs {
        Expr::Grouped(inner) => *inner,
        other => other,
    };
    Ok(match lhs {
        // Bare `* op= rhs` curries to a WhateverCode that mutates its topic:
        // `{ $_ op= rhs }`. Used for e.g. `@a.map(* *= 2)`.
        Expr::Whatever => {
            let body = build_compound_assign_expr(Expr::Var("_".to_string()), op, rhs)?;
            Expr::Lambda {
                param: "_".to_string(),
                body: vec![crate::ast::Stmt::Expr(body)],
                is_whatever_code: true,
            }
        }
        Expr::AssignExpr {
            name,
            expr,
            is_bind: _,
        } => {
            // ($x += 2) *= 3 → first evaluate inner assign, then apply outer op
            // to the variable's value and assign back.
            // This becomes: { let _ = ($x = $x + 2); $x = $x * 3 }
            Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(Expr::Binary {
                    left: Box::new(Expr::AssignExpr {
                        name,
                        expr,
                        is_bind: false,
                    }),
                    op: op.token_kind(),
                    right: Box::new(rhs),
                }),
                is_bind: false,
            }
        }
        Expr::Var(name) => Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(compound_assigned_value_expr(Expr::Var(name), op, rhs)),
            is_bind: false,
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(compound_assigned_value_expr(Expr::ArrayVar(name), op, rhs)),
            is_bind: false,
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(compound_assigned_value_expr(Expr::HashVar(name), op, rhs)),
            is_bind: false,
        },
        Expr::Index { target, index, .. } => {
            return Ok(compound_index_assign_expr(*target, *index, |lhs_expr| {
                compound_assigned_value_expr(lhs_expr, op, rhs)
            }));
        }
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } if name == "AT-POS" && args.len() == 1 => {
            let index = args.into_iter().next().unwrap_or(Expr::Literal(Value::Nil));
            return Ok(compound_index_assign_expr(*target, index, |lhs_expr| {
                compound_assigned_value_expr(lhs_expr, op, rhs)
            }));
        }
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } => {
            let target_var_name = match target.as_ref() {
                Expr::Var(name) => Some(name.clone()),
                Expr::ArrayVar(name) => Some(format!("@{}", name)),
                Expr::HashVar(name) => Some(format!("%{}", name)),
                Expr::BareWord(name) => Some(name.clone()),
                Expr::DoStmt(s) => crate::parser::stmt::simple_expr_stmt::decl_target_var_name(s),
                _ => None,
            };
            let current_value = Expr::MethodCall {
                target: Box::new((*target).clone()),
                name,
                args: args.clone(),
                modifier: None,
                quoted: false,
            };
            let assigned_value = compound_assigned_value_expr(current_value, op, rhs);
            method_lvalue_assign_expr(
                *target,
                target_var_name,
                name.resolve(),
                args,
                assigned_value,
            )
        }
        Expr::BareWord(name) => Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(compound_assigned_value_expr(Expr::BareWord(name), op, rhs)),
            is_bind: false,
        },
        Expr::BracketArray(items, tc) => Expr::Binary {
            left: Box::new(Expr::BracketArray(items, tc)),
            op: op.token_kind(),
            right: Box::new(rhs),
        },
        other => {
            // For short-circuit operators (or=, and=, ||=, &&=, //=, orelse=,
            // andthen=), preserve short-circuit semantics so that when the LHS
            // triggers short-circuit the RHS is never evaluated and no
            // assignment error is raised.
            if matches!(
                op,
                CompoundAssignOp::KeywordOr
                    | CompoundAssignOp::KeywordAnd
                    | CompoundAssignOp::LogicalOr
                    | CompoundAssignOp::LogicalAnd
                    | CompoundAssignOp::DefinedOr
                    | CompoundAssignOp::Orelse
                    | CompoundAssignOp::Andthen
            ) {
                Expr::Binary {
                    left: Box::new(other),
                    op: op.token_kind(),
                    right: Box::new(Expr::DoBlock {
                        body: vec![
                            Stmt::Expr(rhs),
                            Stmt::Expr(Expr::Call {
                                name: Symbol::intern("__mutsu_assignment_ro"),
                                args: Vec::new(),
                            }),
                        ],
                        label: None,
                    }),
                }
            } else {
                Expr::DoBlock {
                    body: vec![
                        Stmt::Expr(other),
                        Stmt::Expr(rhs),
                        Stmt::Expr(Expr::Call {
                            name: Symbol::intern("__mutsu_assignment_ro"),
                            args: Vec::new(),
                        }),
                    ],
                    label: None,
                }
            }
        }
    })
}

pub(crate) fn build_custom_compound_assign_expr(
    lhs: Expr,
    op_name: String,
    rhs: Expr,
) -> Result<Expr, PError> {
    let lhs = match lhs {
        Expr::Grouped(inner) => *inner,
        other => other,
    };
    Ok(match lhs {
        Expr::Var(name) => Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(Expr::InfixFunc {
                name: op_name,
                left: Box::new(Expr::Var(name)),
                right: vec![rhs],
                modifier: None,
            }),
            is_bind: false,
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name.clone()),
            expr: Box::new(Expr::InfixFunc {
                name: op_name,
                left: Box::new(Expr::ArrayVar(name)),
                right: vec![rhs],
                modifier: None,
            }),
            is_bind: false,
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name.clone()),
            expr: Box::new(Expr::InfixFunc {
                name: op_name,
                left: Box::new(Expr::HashVar(name)),
                right: vec![rhs],
                modifier: None,
            }),
            is_bind: false,
        },
        Expr::Index { target, index, .. } => {
            return Ok(compound_index_assign_expr(*target, *index, |lhs_expr| {
                Expr::InfixFunc {
                    name: op_name,
                    left: Box::new(lhs_expr),
                    right: vec![rhs],
                    modifier: None,
                }
            }));
        }
        _ => return Err(PError::expected("assignment expression")),
    })
}

pub(crate) fn build_meta_assign_expr(
    lhs: Expr,
    meta: String,
    op: String,
    rhs: Expr,
) -> Result<Expr, PError> {
    // For "reduce" meta (plain [op]=), reduce on two values is just the base op.
    if meta == "reduce"
        && let Some(compound_op) = CompoundAssignOp::from_op_name(&op)
    {
        return build_compound_assign_expr(lhs, compound_op, rhs);
    }
    Ok(match lhs {
        Expr::Var(name) => Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(Expr::MetaOp {
                meta,
                op,
                left: Box::new(Expr::Var(name)),
                right: Box::new(rhs),
            }),
            is_bind: false,
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name.clone()),
            expr: Box::new(Expr::MetaOp {
                meta,
                op,
                left: Box::new(Expr::ArrayVar(name)),
                right: Box::new(rhs),
            }),
            is_bind: false,
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name.clone()),
            expr: Box::new(Expr::MetaOp {
                meta,
                op,
                left: Box::new(Expr::HashVar(name)),
                right: Box::new(rhs),
            }),
            is_bind: false,
        },
        Expr::Index {
            target,
            index,
            is_positional,
            ..
        } => {
            let lhs_expr = Expr::Index {
                target: target.clone(),
                index: index.clone(),
                is_positional,
            };
            Expr::IndexAssign {
                target,
                index,
                value: Box::new(Expr::MetaOp {
                    meta,
                    op,
                    left: Box::new(lhs_expr),
                    right: Box::new(rhs),
                }),
                is_positional: true,
            }
        }
        _ => return Err(PError::expected("assignment expression")),
    })
}
