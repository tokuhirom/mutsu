use super::*;

pub(crate) fn not_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    if input.starts_with("not")
        && !is_ident_char(input.as_bytes().get(3).copied())
        && !input[3..].starts_with('(')
        && !{
            let trimmed = input[3..].trim_start();
            trimmed.starts_with("=>") && !trimmed.starts_with("==>")
        }
    {
        let r = &input[3..];
        let (r, _) = ws(r)?;
        // Use assign_not_expr_mode so that `not $x = 42` parses as `not($x = 42)`
        // since item assignment is tighter than loose unary not/so.
        let (r, expr) = assign_not_expr_mode(r, mode)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(expr),
            },
        ));
    }
    if input.starts_with("so")
        && !is_ident_char(input.as_bytes().get(2).copied())
        && !input[2..].starts_with('(')
    {
        let r = &input[2..];
        let (r, _) = ws(r)?;
        // Use assign_not_expr_mode so that `so $x = 42` parses as `so($x = 42)`
        // since item assignment is tighter than loose unary not/so.
        let (r, expr) = assign_not_expr_mode(r, mode)?;
        return Ok((
            r,
            Expr::Unary {
                op: TokenKind::Question,
                expr: Box::new(expr),
            },
        ));
    }
    flipflop_expr_mode(input, mode)
}

/// Flip-flop operators (`ff`/`fff` and the endpoint-excluding forms) at
/// Raku's *conditional* precedence (`?? !!` level): looser than tight-or
/// (`||`), tight-and (`&&`), and the chaining comparisons (`==` etc.), but
/// tighter than item assignment (`=`) and the comma/list-infix operators. So
/// `$n == 3 ff $n == 6` parses as `($n == 3) ff ($n == 6)`.
pub(crate) fn flipflop_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = or_or_expr_mode(input, mode)?;
    let mut current_assoc_key: Option<String> = None;
    loop {
        let (r, _) = ws(rest)?;
        let Some((name, len)) = parse_flipflop_infix(r) else {
            break;
        };
        if let Some(prev) = current_assoc_key.as_deref()
            && prev != name
        {
            return Err(non_list_associative_error(prev, &name));
        }
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = or_or_expr_mode(r, mode).map_err(|err| {
            enrich_expected_error(err, "expected expression after flip-flop operator", r.len())
        })?;
        left = Expr::InfixFunc {
            name: name.clone(),
            left: Box::new(left),
            right: vec![right],
            modifier: None,
        };
        current_assoc_key = Some(name);
        rest = r;
    }
    Ok((rest, left))
}

/// || , ^^ , and //
pub(crate) fn or_or_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = and_and_expr_mode(input, mode)?;
    let mut last_list_assoc_op: Option<LogicalOp> = None;
    loop {
        let (r, _) = ws(rest)?;
        // Check for negated logical ops first: !|| , !^^
        let (negated, op_result) = if let Some((op, len)) = parse_negated_logical_op(r) {
            if matches!(op, LogicalOp::OrOr | LogicalOp::XorXor) {
                (true, Some((op, len)))
            } else {
                (false, None)
            }
        } else {
            (false, None)
        };
        let (op, len) = if let Some(pair) = op_result {
            pair
        } else if let Some(pair) = parse_or_or_op(r) {
            pair
        } else {
            break;
        };
        if matches!(op, LogicalOp::Min | LogicalOp::Max) {
            if let Some(prev) = last_list_assoc_op
                && prev != op
            {
                {
                    let lhs = match prev {
                        LogicalOp::Min => "min",
                        LogicalOp::Max => "max",
                        _ => unreachable!(),
                    };
                    let rhs = match op {
                        LogicalOp::Min => "min",
                        LogicalOp::Max => "max",
                        _ => unreachable!(),
                    };
                    return Err(syntax_exception(
                        "X::Syntax::NonAssociative",
                        format!(
                            "Only identical operators may be list associative; since '{}' and '{}' differ, they are non-associative and you need to clarify with parentheses",
                            lhs, rhs
                        ),
                    ));
                }
            }
            last_list_assoc_op = Some(op);
        }
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = if mode == ExprMode::Full {
            if is_loose_not_or_so_prefix(r) {
                not_expr_mode(r, mode).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after logical operator",
                        r.len(),
                    )
                })?
            } else {
                and_and_expr_mode(r, mode).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after logical operator",
                        r.len(),
                    )
                })?
            }
        } else if is_loose_not_or_so_prefix(r) {
            not_expr_mode(r, mode)?
        } else {
            and_and_expr_mode(r, mode)?
        };
        let binary = Expr::Binary {
            left: Box::new(left),
            op: op.token_kind(),
            right: Box::new(right),
        };
        left = if negated {
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(binary),
            }
        } else {
            binary
        };
        rest = r;
    }
    Ok((rest, left))
}

pub(crate) fn is_loose_not_or_so_prefix(input: &str) -> bool {
    (input.starts_with("not")
        && !is_ident_char(input.as_bytes().get(3).copied())
        && !input[3..].starts_with('('))
        || (input.starts_with("so")
            && !is_ident_char(input.as_bytes().get(2).copied())
            && !input[2..].starts_with('('))
}

/// &&
pub(crate) fn and_and_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let (mut rest, mut left) = comparison_expr_mode(input, mode)?;
    loop {
        let (r, _) = ws(rest)?;
        // Check for negated !&&
        let (negated, op, len) = if let Some((op, len)) = parse_negated_logical_op(r) {
            if matches!(op, LogicalOp::AndAnd) {
                (true, op, len)
            } else if let Some((op, len)) = parse_and_and_op(r) {
                (false, op, len)
            } else {
                break;
            }
        } else if let Some((op, len)) = parse_and_and_op(r) {
            (false, op, len)
        } else {
            break;
        };
        let r = &r[len..];
        let (r, _) = ws(r)?;
        // `A && not B` / `A && so B`: loose unary `not`/`so` on the RHS of a
        // tight operator binds LOOSELY (`A && (not (B ...))`), so delegate to the
        // loose-unary parser instead of the tighter comparison operand.
        let (r, right) = if is_loose_not_or_so_prefix(r) {
            not_expr_mode(r, mode)?
        } else if mode == ExprMode::Full {
            comparison_expr_mode(r, mode).map_err(|err| {
                enrich_expected_error(err, "expected expression after '&&'", r.len())
            })?
        } else {
            comparison_expr_mode(r, mode)?
        };
        let binary = Expr::Binary {
            left: Box::new(left),
            op: op.token_kind(),
            right: Box::new(right),
        };
        left = if negated {
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(binary),
            }
        } else {
            binary
        };
        rest = r;
    }
    Ok((rest, left))
}

/// Boolean bitwise / junction: ?| ?& ?^ | & ^
pub(crate) fn junctive_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    let next_fn: fn(&str) -> PResult<'_, Expr> = match mode {
        ExprMode::Full => sequence_expr,
        ExprMode::NoSequence => list_infix_expr,
        ExprMode::NoSequenceNoFeed | ExprMode::ListopArg => range_expr,
    };
    let (mut rest, mut left) = next_fn(input)?;
    let mut last_junction: Option<JunctionInfixOp> = None;
    loop {
        let (r, _) = ws(rest)?;
        if let Some((op, len)) = parse_junctive_op(r) {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                next_fn(r).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after junctive operator",
                        r.len(),
                    )
                })?
            } else {
                next_fn(r)?
            };
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            rest = r;
            continue;
        }
        // Junction infix operators: |, &, ^
        if let Some((op, len)) = parse_junction_infix_op(r) {
            if let Some(prev) = last_junction
                && matches!(
                    (prev, op),
                    (JunctionInfixOp::Any, JunctionInfixOp::One)
                        | (JunctionInfixOp::One, JunctionInfixOp::Any)
                )
            {
                return Err(non_list_associative_error(
                    match prev {
                        JunctionInfixOp::Any => "|",
                        JunctionInfixOp::All => "&",
                        JunctionInfixOp::One => "^",
                    },
                    match op {
                        JunctionInfixOp::Any => "|",
                        JunctionInfixOp::All => "&",
                        JunctionInfixOp::One => "^",
                    },
                ));
            }
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = if mode == ExprMode::Full {
                next_fn(r).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after junction operator",
                        r.len(),
                    )
                })?
            } else {
                next_fn(r)?
            };
            left = Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right),
            };
            last_junction = Some(op);
            rest = r;
            continue;
        }
        break;
    }
    Ok((rest, left))
}
