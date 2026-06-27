use super::*;

/// Shared loop for Z/X meta operators and infix function calls.
/// Modifies `left` in place and returns the remaining input.
pub(crate) fn parse_list_infix_loop<'a>(
    input: &'a str,
    orig_input: &str,
    left: &mut Expr,
    current_assoc_key: &mut Option<String>,
) -> Result<&'a str, PError> {
    let mut rest = input;
    loop {
        let (r, _) = ws(rest)?;
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
        // Infixed function call: [&func], R[&func], X[&func], Z[&func]
        if let Some((modifier, name, len)) = parse_infix_func_op(r) {
            let op_key = if let Some(modifier) = modifier.as_deref() {
                format!("{modifier}[{name}]")
            } else {
                format!("[{name}]")
            };
            if let Some(prev) = current_assoc_key.as_deref()
                && prev != op_key
            {
                return Err(non_list_associative_error(prev, &op_key));
            }
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, mut right_exprs) = if modifier.as_deref() == Some("X") {
                parse_comma_list_of_range(r)?
            } else {
                let (r, expr) = range_expr(r).map_err(|err| {
                    enrich_expected_error(
                        err,
                        "expected expression after infixed function",
                        r.len(),
                    )
                })?;
                (r, vec![expr])
            };
            // Collect trailing colonpair adverbs (e.g., `3 zin 4 :x(5)`)
            let mut r = r;
            loop {
                let (r2, _) = ws(r)?;
                if r2.starts_with(':')
                    && !r2.starts_with("::")
                    && let Ok((r3, adverb)) = crate::parser::primary::colonpair_expr(r2)
                {
                    right_exprs.push(adverb);
                    r = r3;
                } else {
                    break;
                }
            }
            *left = Expr::InfixFunc {
                name,
                left: Box::new(left.clone()),
                right: right_exprs,
                modifier,
            };
            *current_assoc_key = Some(op_key);
            rest = r;
            continue;
        }
        // Negated bracket infix operators: ![op]
        if let Some(r_after_bang) = r.strip_prefix('!')
            && let Some(bracket_infix) = parse_bracket_infix_op(r_after_bang)
        {
            match bracket_infix {
                BracketInfix::PlainOp(op, len) => {
                    let r = &r[1 + len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after negated bracket infix op",
                            r.len(),
                        )
                    })?;
                    let binary = if let Some(tk) = op_str_to_token_kind(&op) {
                        Expr::Binary {
                            left: Box::new(left.clone()),
                            op: tk,
                            right: Box::new(right),
                        }
                    } else {
                        Expr::InfixFunc {
                            name: op,
                            left: Box::new(left.clone()),
                            right: vec![right],
                            modifier: None,
                        }
                    };
                    *left = Expr::Unary {
                        op: TokenKind::Bang,
                        expr: Box::new(binary),
                    };
                    rest = r;
                    continue;
                }
                BracketInfix::MetaOp(meta, op, len) => {
                    let r = &r[1 + len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after negated bracket meta infix op",
                            r.len(),
                        )
                    })?;
                    *left = Expr::Unary {
                        op: TokenKind::Bang,
                        expr: Box::new(Expr::MetaOp {
                            meta,
                            op,
                            left: Box::new(left.clone()),
                            right: Box::new(right),
                        }),
                    };
                    rest = r;
                    continue;
                }
                BracketInfix::UserInfix(name, len) => {
                    let r = &r[1 + len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after negated bracket user infix op",
                            r.len(),
                        )
                    })?;
                    *left = Expr::Unary {
                        op: TokenKind::Bang,
                        expr: Box::new(Expr::InfixFunc {
                            name,
                            left: Box::new(left.clone()),
                            right: vec![right],
                            modifier: None,
                        }),
                    };
                    rest = r;
                    continue;
                }
            }
        }
        // Bracket infix operators: [+], [R-], [Z*], [Z[cmp]], [blue], etc.
        if let Some(bracket_infix) = parse_bracket_infix_op(r) {
            match bracket_infix {
                BracketInfix::PlainOp(op, len) => {
                    let r = &r[len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after bracket infix op",
                            r.len(),
                        )
                    })?;
                    // Resolve plain op to a Binary expr
                    if let Some(tk) = op_str_to_token_kind(&op) {
                        *left = Expr::Binary {
                            left: Box::new(left.clone()),
                            op: tk,
                            right: Box::new(right),
                        };
                    } else {
                        // Unknown plain op: treat as user-defined infix
                        *left = Expr::InfixFunc {
                            name: op,
                            left: Box::new(left.clone()),
                            right: vec![right],
                            modifier: None,
                        };
                    }
                    rest = r;
                    continue;
                }
                BracketInfix::MetaOp(meta, op, len) => {
                    let r = &r[len..];
                    let (r, _) = ws(r)?;
                    let needs_comma_list = meta == "Z"
                        || meta == "X"
                        || op == "..."
                        || op == "...^"
                        || op == "…"
                        || op == "…^";
                    let (r, right) = if needs_comma_list {
                        let (r, items) = parse_comma_list_of_range_raw(r)?;
                        if items.len() == 1 {
                            (r, items.into_iter().next().unwrap())
                        } else {
                            (r, Expr::ArrayLiteral(items))
                        }
                    } else {
                        range_expr(r).map_err(|err| {
                            enrich_expected_error(
                                err,
                                "expected expression after bracket meta infix op",
                                r.len(),
                            )
                        })?
                    };
                    let (r, right) = if meta == "X" {
                        let (r_ws, _) = ws(r)?;
                        if let Some(after_op) = r_ws.strip_prefix("~~") {
                            let (after_op, _) = ws(after_op)?;
                            let (r_after_rhs, rhs_expr) =
                                junctive_expr_mode(after_op, ExprMode::Full).map_err(|err| {
                                    enrich_expected_error(
                                        err,
                                        "expected expression after smartmatch",
                                        after_op.len(),
                                    )
                                })?;
                            (
                                r_after_rhs,
                                Expr::Binary {
                                    left: Box::new(right),
                                    op: TokenKind::SmartMatch,
                                    right: Box::new(rhs_expr),
                                },
                            )
                        } else {
                            (r_ws, right)
                        }
                    } else {
                        (r, right)
                    };
                    *left = Expr::MetaOp {
                        meta,
                        op,
                        left: Box::new(left.clone()),
                        right: Box::new(right),
                    };
                    rest = r;
                    continue;
                }
                BracketInfix::UserInfix(name, len) => {
                    let r = &r[len..];
                    let (r, _) = ws(r)?;
                    let (r, right) = range_expr(r).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after bracket user infix op",
                            r.len(),
                        )
                    })?;
                    *left = Expr::InfixFunc {
                        name,
                        left: Box::new(left.clone()),
                        right: vec![right],
                        modifier: None,
                    };
                    rest = r;
                    continue;
                }
            }
        }
        // Meta operators: R-, X+, Z~, bare Z, bare X, R[...], etc.
        if let Some((stripped, meta, op_name)) = parse_meta_compound_assign_op(r)
            && meta == "R"
        {
            let (r, _) = ws(stripped)?;
            let (r, rhs) = parse_assign_expr_or_comma(r).map_err(|err| {
                enrich_expected_error(
                    err,
                    "expected expression after reverse meta compound assignment",
                    r.len(),
                )
            })?;
            *left = build_compound_assign_target_expr(rhs, &op_name, left.clone());
            rest = r;
            continue;
        }
        if let Some((meta, op, len)) = parse_meta_op(r) {
            // A reversed range meta-op (`R..`, `R^..`, `R..^`, `R^..^`) carries the
            // same precedence worry as a plain range: `|4 R.. 5` / `~4 R.. 5` mean
            // `|(4 R.. 5)` / `~(4 R.. 5)`, not `(|4) R.. 5`. Fire the worry off the
            // ORIGINAL input text (parentheses are transparent in the AST), matching
            // the direct-range path in `range_expr`.
            if matches!(op.as_str(), ".." | "..^" | "^.." | "^..^") {
                check_range_precedence_worry(orig_input)?;
            }
            let op_key = format!("{meta}{op}");
            if let Some(prev) = current_assoc_key.as_deref()
                && prev != op_key
            {
                return Err(non_list_associative_error(prev, &op_key));
            }
            if meta == "X"
                && op == "cmp"
                && matches!(
                    &left,
                    Expr::MetaOp {
                        meta: prev_meta,
                        op: prev_op,
                        ..
                    } if prev_meta == "X" && prev_op == "cmp"
                )
            {
                return Err(non_associative_error("Xcmp"));
            }
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let needs_comma_list = (meta == "Z" || meta == "X")
                || op == "..."
                || op == "...^"
                || op == "…"
                || op == "…^";
            let (r, right) = if needs_comma_list {
                let (r, items) = parse_comma_list_of_range_raw(r)?;
                if items.len() == 1 {
                    (r, items.into_iter().next().unwrap())
                } else {
                    (r, Expr::ArrayLiteral(items))
                }
            } else {
                range_expr(r).map_err(|err| {
                    enrich_expected_error(err, "expected expression after meta operator", r.len())
                })?
            };
            let (r, right) = if meta == "X" {
                let (r_ws, _) = ws(r)?;
                if let Some(after_op) = r_ws.strip_prefix("~~") {
                    let (after_op, _) = ws(after_op)?;
                    let (r_after_rhs, rhs_expr) = junctive_expr_mode(after_op, ExprMode::Full)
                        .map_err(|err| {
                            enrich_expected_error(
                                err,
                                "expected expression after smartmatch",
                                after_op.len(),
                            )
                        })?;
                    (
                        r_after_rhs,
                        Expr::Binary {
                            left: Box::new(right),
                            op: TokenKind::SmartMatch,
                            right: Box::new(rhs_expr),
                        },
                    )
                } else {
                    (r_ws, right)
                }
            } else {
                (r, right)
            };
            *left = Expr::MetaOp {
                meta,
                op,
                left: Box::new(left.clone()),
                right: Box::new(right),
            };
            *current_assoc_key = Some(op_key);
            rest = r;
            continue;
        }
        // User-defined infix words (typically via my &infix:<...> = ...),
        // e.g. `42 same-in-Int "42"`.
        // Do not span statement boundaries across newlines.
        let ws_before = &rest[..rest.len() - r.len()];
        if !ws_before.contains('\n')
            && let Some((feed_op, len)) = parse_feed_op(r)
        {
            let r = &r[len..];
            let (r, _) = ws(r)?;
            let (r, right) = (if matches!(feed_op, FeedOp::ToLeft | FeedOp::AppendLeft) {
                sequence_expr(r)
            } else {
                range_expr(r)
            })
            .map_err(|err| {
                enrich_expected_error(err, "expected expression after feed operator", r.len())
            })?;
            *left = make_feed_node(feed_op, left.clone(), right);
            rest = r;
            continue;
        }
        if !ws_before.contains('\n')
            && let Some((name, len)) = parse_custom_infix_word(r)
            && crate::parser::stmt::simple::lookup_custom_infix_precedence(&name)
                .is_none_or(|level| level <= crate::parser::stmt::simple::PREC_SEQUENCE)
        {
            let mut r = &r[len..];
            let (r2, _) = ws(r)?;
            let (r2, right) = range_expr(r2).map_err(|err| {
                enrich_expected_error(err, "expected expression after infix operator", r.len())
            })?;
            let assoc = crate::parser::stmt::simple::lookup_user_infix_assoc(&name)
                .unwrap_or_else(|| "left".to_string());
            let mut args = vec![left.clone(), right];
            r = r2;
            if assoc != "left" {
                loop {
                    let (r_ws, _) = ws(r)?;
                    let ws_between = &r[..r.len() - r_ws.len()];
                    if ws_between.contains('\n') {
                        break;
                    }
                    let Some((next_name, next_len)) = parse_custom_infix_word(r_ws) else {
                        break;
                    };
                    if next_name != name {
                        break;
                    }
                    let r_after_op = &r_ws[next_len..];
                    let (r_after_op, _) = ws(r_after_op)?;
                    let (r_after_arg, arg) = range_expr(r_after_op).map_err(|err| {
                        enrich_expected_error(
                            err,
                            "expected expression after infix operator",
                            r_after_op.len(),
                        )
                    })?;
                    args.push(arg);
                    r = r_after_arg;
                }
            }
            // Collect trailing colonpair adverbs (e.g., `3 zin 4 :x(5)`)
            loop {
                let (r_ws, _) = ws(r)?;
                if r_ws.starts_with(':')
                    && !r_ws.starts_with("::")
                    && let Ok((r3, adverb)) = crate::parser::primary::colonpair_expr(r_ws)
                {
                    args.push(adverb);
                    r = r3;
                } else {
                    break;
                }
            }
            *left = match assoc.as_str() {
                "right" => {
                    let mut iter = args.into_iter().rev();
                    let mut acc = iter
                        .next()
                        .unwrap_or(Expr::Literal(crate::value::Value::Nil));
                    for lhs in iter {
                        acc = Expr::InfixFunc {
                            name: name.clone(),
                            left: Box::new(lhs),
                            right: vec![acc],
                            modifier: None,
                        };
                    }
                    acc
                }
                "list" | "chain" => Expr::InfixFunc {
                    name: name.clone(),
                    left: Box::new(args[0].clone()),
                    right: args[1..].to_vec(),
                    modifier: None,
                },
                "non" => {
                    if args.len() > 2 {
                        return Err(non_associative_error(&name));
                    }
                    Expr::InfixFunc {
                        name: name.clone(),
                        left: Box::new(args[0].clone()),
                        right: args[1..].to_vec(),
                        modifier: None,
                    }
                }
                _ => Expr::InfixFunc {
                    name: name.clone(),
                    left: Box::new(args[0].clone()),
                    right: args[1..].to_vec(),
                    modifier: None,
                },
            };
            rest = r;
            continue;
        }
        break;
    }
    Ok(rest)
}
