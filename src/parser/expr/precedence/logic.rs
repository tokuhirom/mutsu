use super::*;

pub(crate) fn or_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = assign_or_and_expr(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if mode == ExprMode::ListopArg {
            break;
        }
        if let Some((op @ (LogicalOp::Or | LogicalOp::XorXor | LogicalOp::OrElse), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                assign_or_and_expr(r, mode).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after 'or'/'xor'/'orelse'",
                        r.len(),
                    )
                })?
            } else {
                assign_or_and_expr(r, mode)?
            };
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Low-precedence OR/XOR chain for ternary condition parsing.
/// This intentionally excludes assignment expressions so:
/// `a = b ?? c !! d` parses as `a = (b ?? c !! d)`.
pub(crate) fn or_expr_no_assign_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = and_expr_no_assign_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if mode == ExprMode::ListopArg {
            break;
        }
        if let Some((op @ (LogicalOp::Or | LogicalOp::XorXor | LogicalOp::OrElse), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = and_expr_no_assign_mode(r, mode)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

/// Assignment expressions at the or/and level: $var = expr
/// This sits between or/and and not in precedence.
pub(crate) fn assign_or_and_expr(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    and_expr_mode(input, mode)
}

/// Low-precedence: and / andthen / notandthen
pub(crate) fn and_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = assign_not_expr_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if mode == ExprMode::ListopArg {
            break;
        }
        if let Some((op @ (LogicalOp::And | LogicalOp::AndThen | LogicalOp::NotAndThen), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                assign_not_expr_mode(r, mode).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after 'and'/'andthen'/'notandthen'",
                        r.len(),
                    )
                })?
            } else {
                assign_not_expr_mode(r, mode)?
            };
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

pub(crate) fn and_expr_no_assign_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = not_expr_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        if mode == ExprMode::ListopArg {
            break;
        }
        if let Some((op @ (LogicalOp::And | LogicalOp::AndThen | LogicalOp::NotAndThen), len)) =
            parse_word_logical_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = not_expr_mode(r, mode)?;
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}

pub(crate) fn assign_not_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (rest, expr) = not_expr_mode(input, mode)?;
    let (r, _) = ws(rest)?;

    // Try compound assignment operators (+=, *=, //=, etc.) before simple =
    if let Some((after_op, op)) = parse_compound_assign_op(r) {
        let (after_ws, _) = ws(after_op)?;
        if let Ok((r, rhs)) = parse_assignment_rhs_mode(after_ws, mode)
            && let Ok(result) = build_compound_assign_expr(expr.clone(), op, rhs)
        {
            return Ok((r, result));
        }
    }

    // Binding (`:=`) is an expression-level operator at item-assignment
    // precedence (raku), so it must work unparenthesized after `return`, in a
    // declarator RHS, etc. -- e.g. `return @!specs := @specs`. mutsu otherwise
    // only handled `:=` at the statement level. Compile-time `::=` is left to
    // its own statement-level handler. Only simple sigil-variable lvalues bind
    // here; other lvalue shapes (bareword pseudo-packages like `OUTER`, call
    // results, indexed elements) fall through to the statement-level handler,
    // which validates them and throws X::Bind where appropriate.
    if r.starts_with(":=") && !r.starts_with("::=") {
        let after = &r[2..];
        let (after, _) = ws(after)?;
        if let Ok((r2, rhs)) = ternary_mode(after, mode) {
            let bind_name = match unwrap_grouped_lvalue(expr.clone()) {
                Expr::Var(name) => Some(name),
                Expr::ArrayVar(name) => Some(format!("@{name}")),
                Expr::HashVar(name) => Some(format!("%{name}")),
                _ => None,
            };
            if let Some(name) = bind_name {
                return Ok((
                    r2,
                    Expr::AssignExpr {
                        name,
                        expr: Box::new(rhs),
                        is_bind: true,
                    },
                ));
            }
        }
    }

    if !(r.starts_with('=') && !r.starts_with("==") && !r.starts_with("=>")) {
        return Ok((rest, expr));
    }

    let r = &r[1..];
    let (r, _) = ws(r)?;
    // Item assignment (`=`) to a bare scalar variable binds TIGHTER than the
    // comma operator, so an embedded `$x = 1, 2` parses as `($x = 1), 2`. List
    // assignment to an `@`/`%` container — and the parenthesized list-lvalue form
    // `($x) = 1, 2` — keeps the comma-absorbing RHS. We test the pre-unwrap
    // expression so a grouped `($x)` stays on the list-assignment path. (The
    // statement-level counterpart lives in `stmt/assign.rs::assign_stmt`.)
    let scalar_item_assign = matches!(&expr, Expr::Var(_));
    let (r, rhs) = if scalar_item_assign {
        ternary_mode(r, mode)?
    } else {
        parse_assignment_rhs_mode(r, mode)?
    };

    let expr = unwrap_grouped_lvalue(expr);
    match expr {
        Expr::Var(name) => Ok((
            r,
            Expr::AssignExpr {
                name,
                expr: Box::new(rhs),
                is_bind: false,
            },
        )),
        Expr::ArrayVar(name) => Ok((
            r,
            Expr::AssignExpr {
                name: format!("@{}", name),
                expr: Box::new(rhs),
                is_bind: false,
            },
        )),
        Expr::HashVar(name) => Ok((
            r,
            Expr::AssignExpr {
                name: format!("%{}", name),
                expr: Box::new(rhs),
                is_bind: false,
            },
        )),
        Expr::Index {
            target,
            index,
            is_positional,
        } => {
            if let Expr::Call { name, args } = target.as_ref()
                && name == "__mutsu_subscript_adverb"
                && args.len() >= 3
                && matches!(index.as_ref(), Expr::Literal(Value::Int(1)))
                && matches!(&args[2], Expr::Literal(Value::Str(mode)) if mode.as_str() == "kv" || mode.as_str() == "not-kv")
            {
                return Ok((
                    r,
                    Expr::IndexAssign {
                        target: Box::new(args[0].clone()),
                        index: Box::new(args[1].clone()),
                        value: Box::new(rhs),
                        is_positional,
                    },
                ));
            }
            Ok((
                r,
                Expr::IndexAssign {
                    target,
                    index,
                    value: Box::new(rhs),
                    is_positional,
                },
            ))
        }
        Expr::Call { name, args } => Ok((
            r,
            Expr::Call {
                name: Symbol::intern("__mutsu_assign_named_sub_lvalue"),
                args: vec![
                    Expr::Literal(Value::str(name.resolve())),
                    Expr::ArrayLiteral(args),
                    rhs,
                ],
            },
        )),
        Expr::MultiDimIndex { target, dimensions } => Ok((
            r,
            Expr::MultiDimIndexAssign {
                target,
                dimensions,
                value: Box::new(rhs),
            },
        )),
        Expr::BareWord(name) => Ok((
            r,
            Expr::AssignExpr {
                name,
                expr: Box::new(rhs),
                is_bind: false,
            },
        )),
        Expr::CallOn { target, args } => Ok((
            r,
            if args.is_empty() {
                if let Expr::ArrayLiteral(items) = *target.clone() {
                    if let Some(expr) = list_lvalue_assign_expr(items, rhs.clone()) {
                        expr
                    } else {
                        Expr::Call {
                            name: Symbol::intern("__mutsu_assign_callable_lvalue"),
                            args: vec![*target, Expr::ArrayLiteral(args), rhs],
                        }
                    }
                } else {
                    Expr::Call {
                        name: Symbol::intern("__mutsu_assign_callable_lvalue"),
                        args: vec![*target, Expr::ArrayLiteral(args), rhs],
                    }
                }
            } else {
                Expr::Call {
                    name: Symbol::intern("__mutsu_assign_callable_lvalue"),
                    args: vec![*target, Expr::ArrayLiteral(args), rhs],
                }
            },
        )),
        _ => Ok((rest, expr)),
    }
}
