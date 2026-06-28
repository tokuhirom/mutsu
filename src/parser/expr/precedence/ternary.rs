use super::*;

pub(crate) fn is_assignment_expr(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::AssignExpr { .. } | Expr::IndexAssign { .. } | Expr::MultiDimIndexAssign { .. }
    )
}

pub(crate) fn comparison_nonassoc_key(op: &TokenKind) -> Option<&'static str> {
    match op {
        TokenKind::LtEqGt => Some("<=>"),
        TokenKind::Ident(name) if name == "leg" => Some("leg"),
        TokenKind::Ident(name) if name == "cmp" => Some("cmp"),
        TokenKind::Ident(name) if name == "coll" => Some("coll"),
        TokenKind::Ident(name) if name == "unicmp" => Some("unicmp"),
        // eqv, before, after are chaining operators (not non-associative)
        _ => None,
    }
}

pub(in crate::parser::expr) fn is_structural_comparison_op(op: ComparisonOp) -> bool {
    // Only truly non-associative operators that return Order (not Bool).
    // eqv, before, after are chaining operators and handled by the
    // regular comparison chaining path.
    matches!(
        op,
        ComparisonOp::Spaceship
            | ComparisonOp::Leg
            | ComparisonOp::Cmp
            | ComparisonOp::Coll
            | ComparisonOp::Unicmp
    )
}

pub(crate) fn structural_comparison_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (rest, left) = junctive_expr_mode(input, mode)?;
    let (r, _) = ws(rest)?;
    let Some((op, len)) = parse_comparison_op(r) else {
        return Ok((rest, left));
    };
    if !is_structural_comparison_op(op) {
        return Ok((rest, left));
    }
    let r = &r[len..];
    let (r, _) = ws(r)?;
    let (r, right) = if mode == ExprMode::Full {
        junctive_expr_mode(r, mode).map_err(|err| PError {
            messages: merge_expected_messages(
                "expected expression after comparison operator",
                &err.messages,
            ),
            remaining_len: err.remaining_len.or(Some(r.len())),
            exception: None,
        })?
    } else {
        junctive_expr_mode(r, mode)?
    };
    let expr = Expr::Binary {
        left: Box::new(left),
        op: op.token_kind(),
        right: Box::new(right),
    };
    let (r2, _) = ws(r)?;
    if let Some((next_op, _)) = parse_comparison_op(r2)
        && is_structural_comparison_op(next_op)
    {
        return Err(non_associative_pair_error(
            comparison_nonassoc_key(&op.token_kind()).unwrap_or("<=>"),
            comparison_nonassoc_key(&next_op.token_kind()).unwrap_or("<=>"),
        ));
    }
    Ok((r, expr))
}

/// Ternary: expr ?? expr !! expr
pub(crate) fn ternary(input: &str) -> PResult<'_, Expr> {
    ternary_mode(input, ExprMode::Full)
}

/// Parse a no-paren callable argument expression.
///
/// This uses the precedence tier just tighter than loose word-logicals
/// (`and`, `or`, `xor`, `orelse`, `andthen`, `notandthen`) so that:
/// - `foo 3 != 3` parses as `foo(3 != 3)`
/// - `isfive 5 and isfive 5` parses as `isfive(5) and isfive(5)`
///
/// Also handles item assignment (`=`, `~=`, `+=`, etc.) within a single
/// argument so that `f $a ~= $b, $c` parses as `f(($a ~= $b), $c)`.
pub(crate) fn call_arg_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, expr) = call_arg_ternary_expr(input)?;
    let (r, _) = ws(rest)?;

    // Handle compound assignment operators (+=, ~=, //=, etc.) in call-arg context.
    // The RHS is a single expression (no comma collection) since commas separate
    // function arguments at this level. Item assignment is looser than the
    // conditional `?? !!`, so the RHS parses a ternary too.
    if let Some((after_op, op)) = parse_compound_assign_op(r) {
        let (after_ws, _) = ws(after_op)?;
        if let Ok((r2, rhs)) = call_arg_ternary_expr(after_ws)
            && let Ok(result) = build_compound_assign_expr(expr.clone(), op, rhs)
        {
            return Ok((r2, result));
        }
    }

    // Handle simple assignment (=) in call-arg context.
    if r.starts_with('=') && !r.starts_with("==") && !r.starts_with("=>") {
        let r_eq = &r[1..];
        let (r_eq, _) = ws(r_eq)?;
        if let Ok((r2, rhs)) = call_arg_ternary_expr(r_eq) {
            return Ok((r2, assign_to_target_expr(expr, rhs)));
        }
    }

    Ok((rest, expr))
}

/// Parse a single call argument at *conditional* (`?? !!`) precedence: an
/// `or_or` expression optionally followed by a ternary. The comma operator
/// separates arguments at a looser level, so `f $cond ?? $a !! $b` binds the
/// ternary inside the argument (matching Raku) rather than wrapping the whole
/// call as `(f $cond) ?? $a !! $b`.
pub(crate) fn call_arg_ternary_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, cond) = or_or_expr_mode(input, ExprMode::NoSequenceNoFeed)?;
    let (r, _) = ws(rest)?;
    let Ok((r, _)) = parse_tag(r, "??") else {
        return Ok((rest, cond));
    };
    let (r, _) = ws(r)?;
    let (r, then_expr) = call_arg_ternary_expr(r).map_err(|err| {
        enrich_expected_error(err, "expected then-expression after '??'", r.len())
    })?;
    let (r, _) = ws(r)?;
    let (r, _) = parse_tag(r, "!!").map_err(|err| {
        enrich_expected_error(err, "expected '!!' in ternary expression", r.len())
    })?;
    let (r, _) = ws(r)?;
    let (r, else_expr) = call_arg_ternary_expr(r).map_err(|err| {
        enrich_expected_error(err, "expected else-expression after '!!'", r.len())
    })?;
    Ok((
        r,
        Expr::Ternary {
            cond: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        },
    ))
}

/// Parse a ternary-level expression that does NOT consume a trailing simple or
/// compound assignment (`=`, `+=`, ...). Used for signature `where` constraints,
/// where a following `=` introduces the parameter default rather than an
/// assignment to the constraint expression. `ternary_mode` cannot be reused here
/// because its no-`??` fallback (`or_expr_mode`) includes assignment, which would
/// swallow the default value (e.g. `$x where Int = 9`).
pub(crate) fn ternary_no_assign(input: &str) -> PResult<'_, Expr> {
    let (rest, cond) = or_expr_no_assign_mode(input, ExprMode::Full)?;
    let (rest_ws, _) = ws(rest)?;
    if rest_ws.starts_with("??") {
        // A conditional constraint: the full ternary parser is safe because a
        // top-level `=` only appears after the complete `?? !!` expression.
        return ternary_mode(input, ExprMode::Full);
    }
    Ok((rest, cond))
}

pub(crate) fn ternary_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (rest, cond) = or_expr_no_assign_mode(input, mode)?;
    let (rest_ws, _) = ws(rest)?;
    if let Ok((input, _)) = parse_tag(rest_ws, "??") {
        let (input, _) = ws(input)?;
        // Parse then-expr at ternary precedence (stops before `!!`)
        let (input, then_expr) = if mode == ExprMode::Full {
            ternary_mode(input, mode).map_err(|err| {
                enrich_expected_error(err, "expected then-expression after '??'", input.len())
            })?
        } else {
            ternary_mode(input, mode)?
        };
        if mode == ExprMode::Full
            && let Expr::BareWord(name) = &then_expr
            && !name.contains("::")
            && !crate::runtime::utils::is_known_type_constraint(name)
        {
            // A bare identifier in then-position is usually the head of a listop
            // call (`?? is-deeply $x, $y !! ...`) whose comma args were not yet
            // consumed -- including the case where the listop gobbles the `!!`
            // (`1 ?? rt123115 !! 3` => X::Syntax::ConditionalOperator::
            // SecondPartGobbled). Erroring lets the caller retry/report it. A
            // bareword that names a known TYPE, though, is a complete
            // then-expression -- a type object in `1 ?? Int !! Str` or a native
            // type in `ptrsize == 4 ?? uint32 !! uint64` -- so types are excluded
            // from this guard above and accepted.
            return Err(PError::expected("expected '!!' in ternary expression"));
        }
        let (input, _) = ws(input)?;
        let (input, _) = if mode == ExprMode::Full {
            parse_tag(input, "!!").map_err(|err| {
                enrich_expected_error(err, "expected '!!' in ternary expression", input.len())
            })?
        } else {
            parse_tag(input, "!!")?
        };
        let (input, _) = ws(input)?;
        // Parse the else-expr. In Full mode do NOT absorb a trailing assignment:
        // `=` is LOOSER than the conditional `?? !!`, so `cond ?? t !! lhs = rhs`
        // parses as `(cond ?? t !! lhs) = rhs` (an lvalue-ternary assignment),
        // not as an assignment nested inside the else-branch. Parsing the
        // else-branch no-assign leaves `= rhs` for the trailing handler below.
        let (input, else_expr) = if mode == ExprMode::Full {
            ternary_no_assign(input).map_err(|err| {
                enrich_expected_error(err, "expected else-expression after '!!'", input.len())
            })?
        } else {
            ternary_mode(input, mode)?
        };
        // A then-branch assignment is a genuine error (the `=` would sit between
        // `??` and `!!`); raku rejects it too. In non-Full modes the else-branch
        // may still have greedily taken an assignment, which is likewise too loose.
        if is_assignment_expr(&then_expr) || is_assignment_expr(&else_expr) {
            return Err(conditional_precedence_too_loose_error());
        }
        let ternary_expr = if let Expr::Binary { left, op, right } = cond {
            // The loose word-logicals (`and`/`andthen`/`notandthen`/`or`/`orelse`)
            // are LOOSER than the conditional `?? !!`, so when one was greedily
            // folded into the condition the ternary must re-associate to bind
            // tighter (its right operand). The tight `&&`/`||` are TIGHTER than
            // `?? !!`, so they must NOT re-associate: `$a && $b ?? $c !! $d` is
            // `($a && $b) ?? $c !! $d`, handled by the fall-through below. The
            // loose `and` carries its own `AndWord` token precisely so it can be
            // distinguished here from the tight `&&` (`AndAnd`).
            if matches!(
                op,
                TokenKind::AndWord
                    | TokenKind::AndThen
                    | TokenKind::NotAndThen
                    | TokenKind::OrWord
                    | TokenKind::OrElse
            ) {
                Expr::Binary {
                    left,
                    op,
                    right: Box::new(Expr::Ternary {
                        cond: right,
                        then_expr: Box::new(then_expr),
                        else_expr: Box::new(else_expr),
                    }),
                }
            } else {
                Expr::Ternary {
                    cond: Box::new(Expr::Binary { left, op, right }),
                    then_expr: Box::new(then_expr),
                    else_expr: Box::new(else_expr),
                }
            }
        } else {
            Expr::Ternary {
                cond: Box::new(cond),
                then_expr: Box::new(then_expr),
                else_expr: Box::new(else_expr),
            }
        };
        // A trailing simple assignment binds the whole ternary as an lvalue:
        // `cond ?? $a !! $b = rhs` is `(cond ?? $a !! $b) = rhs` (raku precedence).
        if mode == ExprMode::Full {
            let (after_ws, _) = ws(input)?;
            if after_ws.starts_with('=')
                && !after_ws.starts_with("==")
                && !after_ws.starts_with("=>")
                && !after_ws.starts_with("=:=")
                && !after_ws.starts_with("=~=")
            {
                let (rhs_in, _) = ws(&after_ws[1..])?;
                let (input2, rhs) = ternary_mode(rhs_in, mode).map_err(|err| {
                    enrich_expected_error(err, "expected value after '='", rhs_in.len())
                })?;
                return Ok((
                    input2,
                    Expr::Call {
                        name: Symbol::intern("__mutsu_assign_callable_lvalue"),
                        args: vec![ternary_expr, Expr::ArrayLiteral(Vec::new()), rhs],
                    },
                ));
            }
        }
        return Ok((input, ternary_expr));
    }
    // No ternary operator: fall back to regular OR-level parsing, which includes
    // assignment expressions.
    or_expr_mode(input, mode)
}
