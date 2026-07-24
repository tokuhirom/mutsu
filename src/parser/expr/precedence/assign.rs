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
            quoted: _,
        } => {
            if name == "AT-POS" && args.len() == 1 {
                Expr::IndexAssign {
                    target,
                    index: Box::new(args.into_iter().next().unwrap()),
                    value: Box::new(value),
                    is_positional: true,
                }
            } else {
                // Match the expression-context lowering in `paren.rs` (NOT the
                // statement-level `lvalue_assign_to_expr`, which drops a
                // `BareWord` target to `None`): a sigilless raw binding (`\h`)
                // reaches here as a `BareWord`, and the writeback needs its name
                // so `h.AT-KEY(k) = v` mutates the bound container in place.
                let target_var_name = match target.as_ref() {
                    Expr::Var(v) => Some(v.clone()),
                    Expr::ArrayVar(v) => Some(format!("@{}", v)),
                    Expr::HashVar(v) => Some(format!("%{}", v)),
                    Expr::BareWord(v) => Some(v.clone()),
                    Expr::DoStmt(s) => {
                        crate::parser::stmt::simple_expr_stmt::decl_target_var_name(s)
                    }
                    _ => None,
                };
                let method_name = if modifier == Some('!') {
                    format!("!{}", name.resolve())
                } else {
                    name.resolve()
                };
                crate::parser::stmt::assign::method_lvalue_assign_expr(
                    *target,
                    target_var_name,
                    method_name,
                    args,
                    value,
                )
            }
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
        } => {
            let target_var_name = match target.as_ref() {
                Expr::Var(v) => Some(v.clone()),
                Expr::ArrayVar(v) => Some(format!("@{}", v)),
                Expr::HashVar(v) => Some(format!("%{}", v)),
                Expr::BareWord(v) => Some(v.clone()),
                Expr::DoStmt(s) => crate::parser::stmt::simple_expr_stmt::decl_target_var_name(s),
                _ => None,
            };
            crate::parser::stmt::assign::dynamic_method_lvalue_assign_expr(
                *target,
                target_var_name,
                *name_expr,
                modifier,
                args,
                value,
            )
        }
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
            param_sigilless: false,
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
            let index = args.into_iter().next().unwrap_or(Expr::Literal(Value::NIL));
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
        // `(cond ?? A !! B) op= rhs`: apply the compound assignment through the
        // ternary lvalue to whichever branch is selected. See the matching arm in
        // `build_compound_assign_expr` (compound_expr.rs) for the rationale.
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => Expr::Ternary {
            cond,
            then_expr: Box::new(build_compound_assign_target_expr(
                *then_expr,
                op_name,
                value.clone(),
            )),
            else_expr: Box::new(build_compound_assign_target_expr(
                *else_expr, op_name, value,
            )),
        },
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
    // Parse each comma element at the *list-infix* level (`list_infix_top`):
    // everything tighter than the comma plus the list-infix operators (`Z`/`X`/
    // their meta-ops, the sequence `...`, `minmax`, feed), so `@a[^3] = 1, 2 ... 10`
    // still folds the whole comma list into one sequence. Crucially this stops
    // BEFORE the loose word-logicals (`and`/`or`/`xor`/`andthen`/`orelse`/
    // `notandthen`) — the loosest operators in Raku — so an assignment RHS never
    // absorbs them: `@a[0] = 8 andthen 0` is `(@a[0] = 8) andthen 0`, not
    // `@a[0] = (8 andthen 0)`. The trailing word-logical is left in the stream for
    // the enclosing `and`/`or` chain, which re-attaches it with the whole
    // assignment as its left operand. (`ternary_mode` used to be called here, but
    // it wrongly folded the word-logical into the RHS, storing the tail value;
    // see logic.rs `assign_not_expr_mode`.)
    let (rest, first) = list_infix_top(input, mode)?;
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
            return Ok((r2, finalize_assignment_rhs_list(items)));
        }
        let (r3, next) = list_infix_top(r2, mode)?;
        items.push(next);
        let (r3, _) = ws(r3)?;
        if !r3.starts_with(',') || r3.starts_with(",,") {
            return Ok((r3, finalize_assignment_rhs_list(items)));
        }
        cursor = r3;
    }
}

/// Build the RHS expression from a comma-separated assignment list.
///
/// The sequence operators (`...`/`…`/`...^`/`…^`) and list-infix meta-ops are
/// LOOSER than comma, so `@a[^5] = 1.5, 2.5 ... 5.5` collapses the whole comma
/// list into ONE sequence (seed `1.5, 2.5`), exactly like the declaration form
/// `my @a = 1.5, 2.5 ... 5.5` (which routes through `parse_comma_or_expr`) and
/// the argument-list case (#4755). `normalize_comma_list_items` folds the
/// preceding seeds into the trailing sequence/meta-op; when it collapses to a
/// single such expression we return it directly, otherwise keep the list.
fn finalize_assignment_rhs_list(items: Vec<Expr>) -> Expr {
    let original_len = items.len();
    let normalized = crate::parser::stmt::assign::normalize_comma_list_items(items);
    if normalized.len() == 1 && original_len > 1 {
        return normalized.into_iter().next().unwrap();
    }
    Expr::ArrayLiteral(normalized)
}
