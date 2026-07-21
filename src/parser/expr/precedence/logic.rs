use super::*;
use crate::value::ValueView;

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

    // Bracket meta-op compound assignment on an *indexed* lvalue: `%h<k> [R//]= $v`,
    // `@a[$i] [+]= 1`. A bare-variable LHS is handled by the dedicated
    // statement-level / call-argument assign paths (which need `expression()` to
    // NOT swallow the `[op]=`, so their comma-boundary logic keeps working) — only
    // subscripted lvalues, which reach here as the parsed `expr`, need this branch.
    // The list-infix / arithmetic loops above bail on `[op]=` so `r` still starts
    // with it.
    if matches!(expr, Expr::Index { .. } | Expr::MultiDimIndex { .. })
        && let Some((after_op, meta, op)) =
            crate::parser::stmt::assign::parse_bracket_meta_assign_op(r)
    {
        let (after_ws, _) = ws(after_op)?;
        if let Ok((r, rhs)) = parse_assignment_rhs_mode(after_ws, mode)
            && let Ok(result) =
                crate::parser::stmt::assign::build_meta_assign_expr(expr.clone(), meta, op, rhs)
        {
            return Ok((r, result));
        }
    }

    // Word-operator compound assignment (`div=`, `mod=`, `gcd=`, `x=`, and
    // user-declared operators). Handled here — not only at the statement level —
    // so it parses as the operand of a looser operator, e.g. the right side of
    // `and`/`or`: `@f.push($p) and $n div= $p while ...` (surfaced by
    // Prime::Factor). The multiplicative parser leaves `div=` alone (see
    // `word_infix_at`), so `not_expr_mode` above returns the bare lvalue here.
    if let Some((after_op, op_name)) = parse_custom_compound_assign_op(r) {
        let (after_ws, _) = ws(after_op)?;
        if let Ok((r, rhs)) = parse_assignment_rhs_mode(after_ws, mode)
            && let Ok(result) = build_custom_compound_assign_expr(expr.clone(), op_name, rhs)
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
            match unwrap_grouped_lvalue(expr.clone()) {
                Expr::Var(name) => {
                    return Ok((
                        r2,
                        Expr::AssignExpr {
                            name,
                            expr: Box::new(rhs),
                            is_bind: true,
                        },
                    ));
                }
                Expr::ArrayVar(name) => {
                    return Ok((
                        r2,
                        Expr::AssignExpr {
                            name: format!("@{name}"),
                            expr: Box::new(rhs),
                            is_bind: true,
                        },
                    ));
                }
                Expr::HashVar(name) => {
                    return Ok((
                        r2,
                        Expr::AssignExpr {
                            name: format!("%{name}"),
                            expr: Box::new(rhs),
                            is_bind: true,
                        },
                    ));
                }
                // Indexed element bind: `%h<k> := v`, `@a[i] := v`. Desugar to an
                // IndexAssign whose value carries the `__mutsu_bind_index_value`
                // marker so the VM takes *bind* (not assign) semantics — matching
                // the statement-level handler. This lets an indexed bind parse in
                // expression context (`my $c = %h<k> := v`, `$c = %h<k> := X.new: ...`,
                // `return @a[i] := v`); previously only simple sigil variables bound
                // here and indexed lvalues were left to the statement-level handler,
                // which is absent in expression context.
                //
                // Only a single-element bind on a plain sigil-variable target is
                // handled here. Slice binds (`@a[0,1] := 4,5`), Whatever-index binds
                // (`@a[*-1] := 42`, illegal → X::Bind::Slice), and binds into a
                // non-variable target (`(1,2)[0] := 3`, `10[0] := 1`, → X::Bind) must
                // keep falling through to the statement-level handler, which
                // distributes/validates them. Guard on those so this fast path does
                // not shadow that handling.
                Expr::Index {
                    target,
                    index,
                    is_positional,
                } if matches!(
                    target.as_ref(),
                    Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::Var(_)
                ) && !matches!(
                    index.as_ref(),
                    Expr::ArrayLiteral(_)
                        | Expr::Whatever
                        | Expr::Lambda {
                            is_whatever_code: true,
                            ..
                        }
                        | Expr::AnonSubParams {
                            is_whatever_code: true,
                            ..
                        }
                ) =>
                {
                    let bind_value = Expr::Call {
                        name: crate::symbol::Symbol::intern("__mutsu_bind_index_value"),
                        args: vec![
                            rhs.clone(),
                            crate::parser::stmt::simple_expr_stmt::lvalue::bind_source_metadata_expr(
                                &rhs,
                            ),
                        ],
                    };
                    return Ok((
                        r2,
                        Expr::IndexAssign {
                            target,
                            index,
                            value: Box::new(bind_value),
                            is_positional,
                        },
                    ));
                }
                _ => {}
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
    // A symbolic scalar deref (`$::('name') = ...`) is an item assignment too, so
    // it binds tighter than the comma (`flat $::('b') = l(), l()` is
    // `flat (($::('b') = l()), l())`). An `@`/`%` symbolic deref keeps the
    // comma-absorbing list-assignment RHS.
    let scalar_item_assign = matches!(&expr, Expr::Var(_))
        || matches!(&expr, Expr::SymbolicDeref { sigil, .. } if sigil == "$")
        || matches!(&expr, Expr::IndirectTypeLookup(_));
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
                && matches!(index.as_ref(), Expr::Literal(lit) if matches!(lit.view(), ValueView::Int(1)))
                && matches!(&args[2], Expr::Literal(lit) if matches!(lit.view(), ValueView::Str(mode) if mode.as_str() == "kv" || mode.as_str() == "not-kv"))
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
        // `$::('name') = expr` in expression context (e.g. as a listop argument
        // `flat $::('b') = l()`). Without this arm the assignment falls through to
        // `_ => (rest, expr)` below, leaving the `=` unconsumed so the surrounding
        // listop swallows the bare deref and the outer parser mis-reads the whole
        // `flat $::('b')` as an assignable call. Mirrors `assign_to_target_expr`.
        Expr::SymbolicDeref { sigil, expr } => Ok((
            r,
            Expr::SymbolicDerefAssign {
                sigil,
                expr,
                value: Box::new(rhs),
            },
        )),
        Expr::IndirectTypeLookup(inner) => Ok((
            r,
            Expr::IndirectTypeLookupAssign {
                expr: inner,
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
        // A method-call lvalue (`$o.a = v`) in expression position, e.g. the
        // middle term of a chained assignment (`my $c = $o.a = 42`). The shared
        // `assign_to_target_expr` lowers it to the rw-accessor writeback; without
        // this arm the `=` was left unconsumed and surfaced as a `Confused` error.
        //
        // The `$(EXPR)` item contextualizer lowers to a bare `.item` method call
        // whose assignment has ITEM (comma-tight) semantics enforced by the
        // statement-level `$(EXPR) = ...` handler; leave its `=` unconsumed (as
        // before) so that handler still runs.
        mc @ Expr::MethodCall { .. } => {
            if matches!(&mc, Expr::MethodCall { name, args, .. } if name == "item" && args.is_empty())
            {
                Ok((rest, mc))
            } else {
                Ok((r, assign_to_target_expr(mc, rhs)))
            }
        }
        _ => Ok((rest, expr)),
    }
}
