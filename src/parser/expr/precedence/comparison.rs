use super::*;

/// Comparison: ==, !=, <, >, <=, >=, eq, ne, lt, gt, le, ge, ~~, !~~, ===, <=>
pub(crate) fn comparison_expr_mode(input: &str, mode: ExprMode) -> PResult<'_, Expr> {
    fn regex_rhs_needs_more_parsing(rest: &str) -> bool {
        let Ok((rest, _)) = ws(rest) else {
            return false;
        };
        parse_junctive_op(rest).is_some() || parse_junction_infix_op(rest).is_some()
    }

    let (rest, mut left) = structural_comparison_expr_mode(input, mode)?;
    let (r, _) = ws(rest)?;
    // Detect Perl 5 =~ and !~ brainos (only when followed by space or m/)
    if r.starts_with("=~") && !r.starts_with("=~=") && !r.starts_with("=:=") {
        let after = &r[2..];
        if after.starts_with(' ')
            || after.starts_with('\t')
            || after.starts_with("m/")
            || after.starts_with("m ")
        {
            return Err(PError::fatal(
                "X::Obsolete: Unsupported use of =~ to do pattern matching; in Raku please use ~~"
                    .to_string(),
            ));
        }
    }
    if r.starts_with("!~") && !r.starts_with("!~~") {
        let after = &r[2..];
        if after.starts_with(' ')
            || after.starts_with('\t')
            || after.starts_with("m/")
            || after.starts_with("m ")
        {
            return Err(PError::fatal(
                "X::Obsolete: Unsupported use of !~ to do pattern matching; in Raku please use !~~"
                    .to_string(),
            ));
        }
    }
    // Detect !% which is an attempt to negate the non-boolean infix:<%;>
    if r.starts_with("!%") && !r.starts_with("!%%") && !is_ident_char(r.as_bytes().get(2).copied())
    {
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate % because it is not iffy enough",
            r,
        ));
    }
    // Detect !+ !- !* !/ !~ (non-comparison) which cannot be negated
    if r.starts_with("!+") && !r.starts_with("!+&") {
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate + because it is not iffy enough",
            r,
        ));
    }
    if r.starts_with("!-") && !r.starts_with("!--") {
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate - because it is not iffy enough",
            r,
        ));
    }
    if r.starts_with("!*") && !r.starts_with("!**") {
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate * because it is not iffy enough",
            r,
        ));
    }
    if r.starts_with("!/") && !r.starts_with("!//") {
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate / because it is not iffy enough",
            r,
        ));
    }
    // Detect !. which is not a valid metaop
    if r.starts_with("!.") && !r.starts_with("!..") {
        let after = &r[2..];
        let after_trimmed = after.trim_start();
        if after_trimmed.starts_with('"') || after_trimmed.starts_with('\'') {
            return Err(PError::expected_at(
                "X::Obsolete: Unsupported use of !. to concatenate strings; in Raku please use ~",
                r,
            ));
        }
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot negate . because it is too fiddly",
            r,
        ));
    }
    // Detect R. metaop misuse.
    if r.starts_with("R.") && !r.starts_with("R..") {
        let after = &r[2..];
        let after_trimmed = after.trim_start();
        if after_trimmed.starts_with('"') || after_trimmed.starts_with('\'') {
            return Err(PError::expected_at(
                "X::Obsolete: Perl . is dead. Please use ~ to concatenate strings.",
                r,
            ));
        }
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot reverse the args of . because it is too fiddly",
            r,
        ));
    }
    // Detect X. metaop misuse.
    if r.starts_with("X.") && !r.starts_with("X..") {
        let after = &r[2..];
        let after_trimmed = after.trim_start();
        if after_trimmed.starts_with('"') || after_trimmed.starts_with('\'') {
            return Err(PError::expected_at(
                "X::Obsolete: Perl . is dead. Please use ~ to concatenate strings.",
                r,
            ));
        }
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot do . because it is too fiddly",
            r,
        ));
    }
    // Detect Z. metaop misuse.
    if r.starts_with("Z.") && !r.starts_with("Z..") {
        let after = &r[2..];
        let after_trimmed = after.trim_start();
        if after_trimmed.starts_with('"') || after_trimmed.starts_with('\'') {
            return Err(PError::expected_at(
                "X::Obsolete: Perl . is dead. Please use ~ to concatenate strings.",
                r,
            ));
        }
        return Err(PError::expected_at(
            "X::Syntax::CannotMeta: Cannot do . because it is too fiddly",
            r,
        ));
    }
    if let Some((op, len)) = parse_negated_meta_comparison_op(r) {
        let r = &r[len..];
        let (r, _) = ws(r)?;
        let (r, right) = if mode == ExprMode::Full {
            structural_comparison_expr_mode(r, mode).map_err(|err| PError {
                messages: merge_expected_messages(
                    "expected expression after comparison operator",
                    &err.messages,
                ),
                remaining_len: err.remaining_len.or(Some(r.len())),
                exception: None,
            })?
        } else {
            structural_comparison_expr_mode(r, mode)?
        };
        let mut result = Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(Expr::Binary {
                left: Box::new(left),
                op: op.token_kind(),
                right: Box::new(right.clone()),
            }),
        };
        // Support chaining: "a" !after "b" !after "c" → (!after) && (!after)
        let mut prev_right = right;
        let mut r = r;
        loop {
            let (r2, _) = ws(r)?;
            // Try negated meta comparison for chaining
            if let Some((cop, chain_len)) = parse_negated_meta_comparison_op(r2) {
                let r2 = &r2[chain_len..];
                let (r2, _) = ws(r2)?;
                let (r2, next_right) = junctive_expr_mode(r2, mode).map_err(|err| PError {
                    messages: merge_expected_messages(
                        "expected expression after chained negated comparison operator",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r2.len())),
                    exception: None,
                })?;
                let next_cmp = Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(Expr::Binary {
                        left: Box::new(prev_right),
                        op: cop.token_kind(),
                        right: Box::new(next_right.clone()),
                    }),
                };
                result = Expr::Binary {
                    left: Box::new(result),
                    op: TokenKind::AndAnd,
                    right: Box::new(next_cmp),
                };
                prev_right = next_right;
                r = r2;
                continue;
            }
            // Also try regular comparison for mixed chaining
            if let Some((cop, chain_len)) = parse_comparison_op(r2) {
                let r2 = &r2[chain_len..];
                let (r2, _) = ws(r2)?;
                let (r2, next_right) = junctive_expr_mode(r2, mode).map_err(|err| PError {
                    messages: merge_expected_messages(
                        "expected expression after chained comparison operator",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r2.len())),
                    exception: None,
                })?;
                let next_cmp = Expr::Binary {
                    left: Box::new(prev_right),
                    op: cop.token_kind(),
                    right: Box::new(next_right.clone()),
                };
                result = Expr::Binary {
                    left: Box::new(result),
                    op: TokenKind::AndAnd,
                    right: Box::new(next_cmp),
                };
                prev_right = next_right;
                r = r2;
                continue;
            }
            break;
        }
        return Ok((r, result));
    }
    if let Some((op, len)) = parse_comparison_op(r) {
        let r = &r[len..];
        let (r, _) = ws(r)?;
        // Track whether the smartmatch RHS is a regex literal.  When it is,
        // chaining with subsequent comparison operators (eq, ==, …) must be
        // suppressed because the regex literal terminates the smartmatch RHS
        // at its closing delimiter, so `X ~~ /pat/ eq Z` should parse as
        // `(X ~~ /pat/) eq Z` (left-associative), not as a chained comparison.
        let mut rhs_is_regex_lit = false;
        let (r, mut right) = if matches!(op, ComparisonOp::SmartMatch | ComparisonOp::SmartNotMatch)
        {
            if let Ok((rest, expr)) = crate::parser::primary::regex::regex_lit(r) {
                if regex_rhs_needs_more_parsing(rest) {
                    if mode == ExprMode::Full {
                        structural_comparison_expr_mode(r, mode).map_err(|err| {
                            // Preserve fatal errors with structured exceptions (e.g., X::Obsolete)
                            if err.is_fatal() && err.exception.is_some() {
                                return err;
                            }
                            PError {
                                messages: merge_expected_messages(
                                    "expected expression after comparison operator",
                                    &err.messages,
                                ),
                                remaining_len: err.remaining_len.or(Some(r.len())),
                                exception: None,
                            }
                        })?
                    } else {
                        structural_comparison_expr_mode(r, mode)?
                    }
                } else {
                    rhs_is_regex_lit = true;
                    (rest, expr)
                }
            } else if mode == ExprMode::Full {
                structural_comparison_expr_mode(r, mode).map_err(|err| {
                    // Preserve fatal errors with structured exceptions (e.g., X::Obsolete)
                    if err.is_fatal() && err.exception.is_some() {
                        return err;
                    }
                    PError {
                        messages: merge_expected_messages(
                            "expected expression after comparison operator",
                            &err.messages,
                        ),
                        remaining_len: err.remaining_len.or(Some(r.len())),
                        exception: None,
                    }
                })?
            } else {
                structural_comparison_expr_mode(r, mode)?
            }
        } else if mode == ExprMode::Full {
            structural_comparison_expr_mode(r, mode).map_err(|err| {
                // Preserve fatal errors with structured exceptions (e.g., X::Obsolete)
                if err.is_fatal() && err.exception.is_some() {
                    return err;
                }
                PError {
                    messages: merge_expected_messages(
                        "expected expression after comparison operator",
                        &err.messages,
                    ),
                    remaining_len: err.remaining_len.or(Some(r.len())),
                    exception: None,
                }
            })?
        } else {
            structural_comparison_expr_mode(r, mode)?
        };
        if matches!(op, ComparisonOp::SmartMatch | ComparisonOp::SmartNotMatch) {
            right = wrap_smartmatch_rhs(right);
            // Wrap LHS WhateverCode subexpressions (e.g. `*.abs ~~ Code`).
            // SmartMatch suppresses top-level WhateverCode wrapping, but the LHS
            // should still be curried when it contains Whatever.
            if crate::parser::expr::should_wrap_whatevercode(&left) {
                left = crate::parser::expr::wrap_whatevercode(&left);
            }
            // Bare `* ~~ Type` curries to WhateverCode `{ $_ ~~ Type }`.
            // This is only done here (not in should_curry_whatever) because at the
            // top-level wrapping stage, parenthesized `((*)) ~~ Type` also has
            // Expr::Whatever as LHS and should NOT curry — only a true bare `*`
            // operand in the precedence parser should trigger this.
            // We distinguish bare `*` from `((*))` by checking whether the consumed
            // input for the LHS started with `(`.
            if matches!(&left, Expr::Whatever) {
                let lhs_text = input[..input.len() - rest.len()].trim_start();
                if !lhs_text.starts_with('(') {
                    let sm_expr = Expr::Binary {
                        left: Box::new(Expr::Var("_".to_string())),
                        op: op.token_kind(),
                        right: Box::new(right),
                    };
                    return Ok((
                        r,
                        Expr::Lambda {
                            param: "_".to_string(),
                            body: vec![crate::ast::Stmt::Expr(sm_expr)],
                            is_whatever_code: true,
                        },
                    ));
                }
            }
        }
        // When the smartmatch RHS is a regex literal, do not chain with
        // subsequent comparison operators.  The regex literal terminates the
        // smartmatch and the following operator applies to the match result.
        if rhs_is_regex_lit {
            return Ok((r, make_chain_cmp(left, op.token_kind(), right, false)));
        }
        let mut operands = vec![left, right];
        let mut chain_ops: Vec<(TokenKind, bool)> = vec![(op.token_kind(), false)];
        let mut r = r;
        loop {
            let (r2, _) = ws(r)?;
            if let Some((cop, chain_len)) = parse_negated_meta_comparison_op(r2) {
                if is_structural_comparison_op(cop) {
                    break;
                }
                let r2 = &r2[chain_len..];
                let (r2, _) = ws(r2)?;
                let (r2, next_right) =
                    structural_comparison_expr_mode(r2, mode).map_err(|err| PError {
                        messages: merge_expected_messages(
                            "expected expression after chained negated comparison operator",
                            &err.messages,
                        ),
                        remaining_len: err.remaining_len.or(Some(r2.len())),
                        exception: None,
                    })?;
                if let (Some(prev), Some(next)) = (
                    comparison_nonassoc_key(&chain_ops.last().expect("chain op").0),
                    comparison_nonassoc_key(&cop.token_kind()),
                ) {
                    return Err(non_associative_error(&format!("{prev} and {next}")));
                }
                chain_ops.push((cop.token_kind(), true));
                operands.push(next_right);
                r = r2;
                continue;
            }
            if let Some((cop, chain_len)) = parse_comparison_op(r2) {
                if is_structural_comparison_op(cop) {
                    break;
                }
                let r2 = &r2[chain_len..];
                let (r2, _) = ws(r2)?;
                let (r2, next_right) =
                    structural_comparison_expr_mode(r2, mode).map_err(|err| PError {
                        messages: merge_expected_messages(
                            "expected expression after chained comparison operator",
                            &err.messages,
                        ),
                        remaining_len: err.remaining_len.or(Some(r2.len())),
                        exception: None,
                    })?;
                if let (Some(prev), Some(next)) = (
                    comparison_nonassoc_key(&chain_ops.last().expect("chain op").0),
                    comparison_nonassoc_key(&cop.token_kind()),
                ) {
                    return Err(non_associative_error(&format!("{prev} and {next}")));
                }
                let next_right =
                    if matches!(cop, ComparisonOp::SmartMatch | ComparisonOp::SmartNotMatch) {
                        wrap_smartmatch_rhs(next_right)
                    } else {
                        next_right
                    };
                chain_ops.push((cop.token_kind(), false));
                operands.push(next_right);
                r = r2;
                continue;
            }
            break;
        }
        if chain_ops.len() == 1 {
            return Ok((
                r,
                make_chain_cmp(
                    operands[0].clone(),
                    chain_ops[0].0.clone(),
                    operands[1].clone(),
                    false,
                ),
            ));
        }
        let result = if operands.iter().any(contains_whatever) {
            // Keep the historical `lhs < * && * < rhs` shape so WhateverCode wrapping
            // can preserve placeholder semantics in expression arguments.
            build_chain_cmp_expr_with_repeated_middle(&operands, &chain_ops)
        } else {
            build_chain_cmp_expr(&operands, &chain_ops, 0, operands[0].clone())
        };
        return Ok((r, result));
    }

    Ok((rest, left))
}
