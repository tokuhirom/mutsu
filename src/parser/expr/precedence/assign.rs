use super::*;

pub(crate) fn assignment_ro_expr(lhs: Expr, rhs: Expr) -> Expr {
    Expr::DoBlock {
        body: vec![
            Stmt::Expr(lhs),
            Stmt::Expr(rhs),
            Stmt::Expr(Expr::Call {
                name: Symbol::intern("__mutsu_assignment_ro"),
                args: Vec::new(),
            }),
        ],
        label: None,
    }
}

pub(crate) fn unwrap_grouped_lvalue(target: Expr) -> Expr {
    match target {
        Expr::Grouped(inner) => unwrap_grouped_lvalue(*inner),
        // A source-preserving literal is an immutable lvalue exactly like the
        // plain literal it wraps; the sink-warn source text is irrelevant here.
        Expr::LiteralSrc(v, _) => Expr::Literal(v),
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
        } => Expr::IndexAssign {
            target,
            index,
            value: Box::new(value),
            is_positional,
        },
        Expr::MultiDimIndex { target, dimensions } => Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value: Box::new(value),
        },
        Expr::Call { name, args } => Expr::Call {
            name: Symbol::intern("__mutsu_assign_named_sub_lvalue"),
            args: vec![
                Expr::Literal(Value::str(name.resolve())),
                Expr::ArrayLiteral(args),
                value,
            ],
        },
        Expr::CallOn { target, args } => Expr::Call {
            name: Symbol::intern("__mutsu_assign_callable_lvalue"),
            args: vec![*target, Expr::ArrayLiteral(args), value],
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
                where_constraint,
                ..
            } = *stmt
            {
                Expr::DoStmt(Box::new(Stmt::VarDecl {
                    name,
                    expr: value,
                    type_constraint,
                    is_state,
                    is_our,
                    is_dynamic,
                    is_export,
                    export_tags,
                    custom_traits,
                    where_constraint,
                }))
            } else {
                assignment_ro_expr(Expr::DoStmt(stmt), value)
            }
        }
        other => assignment_ro_expr(other, value),
    }
}

pub(crate) fn build_compound_assign_target_expr(target: Expr, op_name: &str, value: Expr) -> Expr {
    let target = unwrap_grouped_lvalue(target);
    if op_name == "=" {
        return assign_to_target_expr(target, value);
    }
    let Some(op) = compound_assign_op_from_name(op_name) else {
        return assignment_ro_expr(target, value);
    };
    // Bare `* op= value` curries to a WhateverCode that mutates its topic,
    // i.e. `{ $_ op= value }`. Used for e.g. `@a.map(* *= 2)`.
    if matches!(target, Expr::Whatever) {
        let body = build_compound_assign_target_expr(Expr::Var("_".to_string()), op_name, value);
        return Expr::Lambda {
            param: "_".to_string(),
            body: vec![crate::ast::Stmt::Expr(body)],
            is_whatever_code: true,
        };
    }
    match target {
        Expr::Var(name) => Expr::AssignExpr {
            name: name.clone(),
            expr: Box::new(compound_assigned_value_expr(Expr::Var(name), op, value)),
            is_bind: false,
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name.clone()),
            expr: Box::new(compound_assigned_value_expr(
                Expr::ArrayVar(name),
                op,
                value,
            )),
            is_bind: false,
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name.clone()),
            expr: Box::new(compound_assigned_value_expr(Expr::HashVar(name), op, value)),
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
                value: Box::new(compound_assigned_value_expr(lhs_expr, op, value)),
                is_positional: true,
            }
        }
        Expr::MultiDimIndex { target, dimensions } => {
            let lhs_expr = Expr::MultiDimIndex {
                target: target.clone(),
                dimensions: dimensions.clone(),
            };
            Expr::MultiDimIndexAssign {
                target,
                dimensions,
                value: Box::new(compound_assigned_value_expr(lhs_expr, op, value)),
            }
        }
        Expr::AssignExpr {
            name,
            expr,
            is_bind: _,
        } => {
            if op_name == "=" {
                return Expr::AssignExpr {
                    name,
                    expr: Box::new(value),
                    is_bind: false,
                };
            }
            let Some(op) = compound_assign_op_from_name(op_name) else {
                return assignment_ro_expr(
                    Expr::AssignExpr {
                        name,
                        expr,
                        is_bind: false,
                    },
                    value,
                );
            };
            Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(compound_assigned_value_expr(
                    Expr::AssignExpr {
                        name,
                        expr,
                        is_bind: false,
                    },
                    op,
                    value,
                )),
                is_bind: false,
            }
        }
        Expr::MethodCall {
            target,
            name,
            args,
            modifier: _,
            quoted: _,
        } if name == "AT-POS" && args.len() == 1 => {
            let index = args.into_iter().next().unwrap_or(Expr::Literal(Value::Nil));
            build_compound_assign_target_expr(
                Expr::Index {
                    target,
                    index: Box::new(index),
                    is_positional: true,
                },
                op_name,
                value,
            )
        }
        other => assignment_ro_expr(other, value),
    }
}

pub(crate) fn list_lvalue_assign_expr(items: Vec<Expr>, rhs: Expr) -> Option<Expr> {
    let mut saw_whatever = false;
    let mut lvalues: Vec<Expr> = Vec::new();
    for item in items {
        if matches!(item, Expr::Whatever) {
            saw_whatever = true;
            continue;
        }
        lvalues.push(item);
    }
    if !saw_whatever || lvalues.len() != 1 {
        return None;
    }
    match lvalues.into_iter().next()? {
        Expr::Var(name) => Some(Expr::AssignExpr {
            name,
            expr: Box::new(rhs),
            is_bind: false,
        }),
        Expr::ArrayVar(name) => Some(Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(rhs),
            is_bind: false,
        }),
        Expr::HashVar(name) => Some(Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(rhs),
            is_bind: false,
        }),
        Expr::Index {
            target,
            index,
            is_positional,
        } => Some(Expr::IndexAssign {
            target,
            index,
            value: Box::new(rhs),
            is_positional,
        }),
        _ => None,
    }
}

pub(crate) fn parse_assignment_rhs_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (rest, first) = ternary_mode(input, mode)?;
    let (r, _) = ws(rest)?;
    if !r.starts_with(',') || r.starts_with(",,") {
        return Ok((rest, first));
    }

    let mut items = vec![first];
    let mut cursor = r;
    loop {
        let (r2, _) = parse_char(cursor, ',')?;
        let (r2, _) = ws(r2)?;
        if r2.is_empty() || r2.starts_with(';') || r2.starts_with('}') || r2.starts_with(')') {
            return Ok((r2, Expr::ArrayLiteral(items)));
        }
        let (r3, next) = ternary_mode(r2, mode)?;
        items.push(next);
        let (r3, _) = ws(r3)?;
        if !r3.starts_with(',') || r3.starts_with(",,") {
            return Ok((r3, Expr::ArrayLiteral(items)));
        }
        cursor = r3;
    }
}
