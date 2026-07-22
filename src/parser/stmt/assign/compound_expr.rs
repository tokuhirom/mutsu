use super::*;

fn compound_index_assign_expr<F>(
    target: Expr,
    index: Expr,
    is_positional: bool,
    build_assigned_value: F,
) -> Expr
where
    F: FnOnce(Expr) -> Expr,
{
    let tmp_idx = format!(
        "__mutsu_idx_{}",
        TMP_INDEX_COUNTER.fetch_add(1, Ordering::Relaxed)
    );
    let tmp_idx_expr = Expr::Var(tmp_idx.clone());
    // The read of the current value stays associative-lenient (`is_positional:
    // false`): a missing nested element reads as the type object either way, and
    // a positional read of a not-yet-autovivified intermediate (`@a[0][1] += 1`)
    // would otherwise error. Only the write-back below needs the true subscript
    // kind, to autovivify the correct container (Hash vs Array).
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
            // Preserve the subscript's positional/associative kind so an
            // autovivified intermediate is a Hash for `%h<a><b> += 1`
            // (associative) and an Array for `@a[0][1] += 1` (positional) —
            // a hardcoded `true` here wrongly made `%h<a><b> += 1` an Array.
            Stmt::Expr(Expr::IndexAssign {
                target: Box::new(target),
                index: Box::new(tmp_idx_expr.clone()),
                value: Box::new(assigned_value),
                is_positional,
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
                param_sigilless: false,
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
        Expr::Index {
            target,
            index,
            is_positional,
        } => {
            return Ok(compound_index_assign_expr(
                *target,
                *index,
                is_positional,
                |lhs_expr| compound_assigned_value_expr(lhs_expr, op, rhs),
            ));
        }
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } if name == "AT-POS" && args.len() == 1 => {
            let index = args.into_iter().next().unwrap_or(Expr::Literal(Value::NIL));
            return Ok(compound_index_assign_expr(
                *target,
                index,
                true,
                |lhs_expr| compound_assigned_value_expr(lhs_expr, op, rhs),
            ));
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
        // `(EXPR if COND) op= rhs`: a statement-modifier conditional whose body is
        // an lvalue (`($s = $x.chop if $s) ~= $y`, from P5reset). Push the compound
        // assignment into the then-branch so it runs only when the condition holds;
        // an unrun modifier yields Empty in raku, which the metaop-assign ignores
        // (`($s = "a" if 0) ~= "y"` is a silent no-op). Only a single-expression
        // then-branch with an empty else is a well-defined lvalue.
        Expr::DoStmt(stmt)
            if matches!(
                stmt.as_ref(),
                Stmt::If { then_branch, else_branch, binding_var: None, .. }
                    if then_branch.len() == 1
                        && else_branch.is_empty()
                        && matches!(then_branch[0], Stmt::Expr(_))
            ) =>
        {
            let Stmt::If {
                cond,
                then_branch,
                else_branch,
                binding_var,
            } = *stmt
            else {
                unreachable!("guarded by the match arm")
            };
            let mut then_branch = then_branch;
            let inner = match then_branch.pop() {
                Some(Stmt::Expr(e)) => e,
                _ => unreachable!("guarded by the match arm"),
            };
            let inner_assign = build_compound_assign_expr(inner, op, rhs)?;
            Expr::DoStmt(Box::new(Stmt::If {
                cond,
                then_branch: vec![Stmt::Expr(inner_assign)],
                else_branch,
                binding_var,
            }))
        }
        // `(cond ?? A !! B) op= rhs`: the ternary is an lvalue selecting one
        // branch, so the compound assignment writes through to whichever branch
        // is taken. Desugar to `cond ?? (A op= rhs) !! (B op= rhs)` so only the
        // selected branch is mutated and `rhs` is evaluated once, in that branch.
        // A non-lvalue branch (a literal like `9` in `True ?? 9 !! $l ~= "x"`)
        // recurses to the RO-error `other` arm below, which raises only if that
        // branch is taken -- matching raku ("Cannot modify an immutable Int (9)").
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            let then_assign = build_compound_assign_expr(*then_expr, op, rhs.clone())?;
            let else_assign = build_compound_assign_expr(*else_expr, op, rhs)?;
            Expr::Ternary {
                cond,
                then_expr: Box::new(then_assign),
                else_expr: Box::new(else_assign),
            }
        }
        // `(state $best) max= $score` / `(my $n) += 1`: the declaration *is* the lvalue.
        // Unlike plain `=`, the new value has to read the variable back, so this cannot
        // become a `VarDecl` with an initializer — a `state` initializer runs once, but the
        // compound assignment must run on every pass. Route it through the same
        // callable-lvalue helper the parenthesized `=` form uses: the declaration is
        // evaluated first (declaring the variable and yielding its container), the value
        // then reads it, and the helper assigns back into that container.
        Expr::DoStmt(stmt)
            if crate::parser::stmt::simple_expr_stmt::decl_target_var_name(&stmt).is_some() =>
        {
            let name = crate::parser::stmt::simple_expr_stmt::decl_target_var_name(&stmt)
                .expect("guarded by the match arm");
            let read_back = match name.as_bytes().first() {
                Some(b'@') => Expr::ArrayVar(name[1..].to_string()),
                Some(b'%') => Expr::HashVar(name[1..].to_string()),
                _ => Expr::Var(name),
            };
            let assigned_value = compound_assigned_value_expr(read_back, op, rhs);
            Expr::Call {
                name: Symbol::intern("__mutsu_assign_callable_lvalue"),
                args: vec![
                    Expr::DoStmt(stmt),
                    Expr::ArrayLiteral(Vec::new()),
                    assigned_value,
                ],
            }
        }
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
                    | CompoundAssignOp::Notandthen
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
        Expr::Index {
            target,
            index,
            is_positional,
        } => {
            return Ok(compound_index_assign_expr(
                *target,
                *index,
                is_positional,
                |lhs_expr| Expr::InfixFunc {
                    name: op_name,
                    left: Box::new(lhs_expr),
                    right: vec![rhs],
                    modifier: None,
                },
            ));
        }
        _ => return Err(PError::expected("assignment expression")),
    })
}

/// The sigil-qualified assignment-target name for a `Z=` element-wise assign,
/// or `None` for an LHS shape `Z=` does not element-wise-assign (only plain
/// variables are supported; an `Index` LHS keeps the value-producing path).
fn zip_assign_target_name(lhs: &Expr) -> Option<String> {
    match lhs {
        Expr::Var(name) => Some(name.clone()),
        Expr::ArrayVar(name) => Some(format!("@{name}")),
        Expr::HashVar(name) => Some(format!("%{name}")),
        _ => None,
    }
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
    // `@a Z= rhs` (zip metaoperator on `=`) is element-wise assignment, NOT
    // `@a = (@a Z rhs)`: each `@a[i]` gets `rhs[i]`, trailing `@a` elements keep
    // their value, and a shaped array keeps its shape. `__mutsu_zip_assign`
    // rebuilds the container; the outer assignment stores it back. Only `Z=`
    // (op `=`) is rewritten — `Z=>` / `Z+` / etc. keep the value-producing
    // MetaOp path below.
    if meta == "Z"
        && op == "="
        && let Some(name) = zip_assign_target_name(&lhs)
    {
        let zip_call = Expr::Call {
            name: Symbol::intern("__mutsu_zip_assign"),
            args: vec![lhs, rhs],
        };
        return Ok(Expr::AssignExpr {
            name,
            expr: Box::new(zip_call),
            is_bind: false,
        });
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
                is_positional,
            }
        }
        _ => return Err(PError::expected("assignment expression")),
    })
}
