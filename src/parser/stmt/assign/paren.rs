use super::*;

pub(crate) fn parenthesized_assign_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, _) = parse_char(input, '(')?;
    let (rest, _) = ws(rest)?;
    if let Ok((rest_inner, inner_assign)) = try_parse_assign_expr(rest) {
        let (rest_inner, _) = ws(rest_inner)?;
        if let Ok((rest_after_paren, _)) = parse_char(rest_inner, ')') {
            return Ok((rest_after_paren, inner_assign));
        }
        // If the inner assignment consumed part but left comma-separated items before ')',
        // collect them into a list (e.g. `(@a[1,2] := "a","b")` where RHS is a list).
        if rest_inner.starts_with(',') && !rest_inner.starts_with(",,") {
            // Re-wrap: the inner assignment's RHS should include the remaining items
            if let Expr::IndexAssign {
                target,
                index,
                value,
                is_positional,
                ..
            } = inner_assign
            {
                let mut items = vec![*value];
                let mut r = rest_inner;
                while r.starts_with(',') && !r.starts_with(",,") {
                    let (r2, _) = parse_char(r, ',')?;
                    let (r2, _) = ws(r2)?;
                    if r2.starts_with(')') {
                        r = r2;
                        break;
                    }
                    let (r2, item) = expression_no_sequence(r2)?;
                    items.push(item);
                    let (r2, _) = ws(r2)?;
                    r = r2;
                }
                let (r, _) = parse_char(r, ')')?;
                return Ok((
                    r,
                    Expr::IndexAssign {
                        target,
                        index,
                        value: Box::new(Expr::ArrayLiteral(items)),
                        is_positional,
                        spelling: Default::default(),
                    },
                ));
            } else if let Expr::AssignExpr {
                name,
                expr,
                is_bind,
            } = inner_assign
            {
                // Item assignment (`=`) to a SCALAR is tighter than the comma, so
                // `($x = l(), 3, 4)` parses as `(($x = l()), 3, 4)` -- a list whose
                // first element is the assignment. Only an `@`/`%` (list) LHS
                // absorbs the trailing comma items into its RHS
                // (`(@a = 1, 2)` -> `@a = (1, 2)`). A `:=` bind also absorbs.
                let scalar_item_assign =
                    !is_bind && !name.starts_with('@') && !name.starts_with('%');
                let mut items = if scalar_item_assign {
                    vec![Expr::AssignExpr {
                        name: name.clone(),
                        expr,
                        is_bind,
                    }]
                } else {
                    vec![*expr]
                };
                let mut r = rest_inner;
                while r.starts_with(',') && !r.starts_with(",,") {
                    let (r2, _) = parse_char(r, ',')?;
                    let (r2, _) = ws(r2)?;
                    if r2.starts_with(')') {
                        r = r2;
                        break;
                    }
                    let (r2, item) = expression_no_sequence(r2)?;
                    items.push(item);
                    let (r2, _) = ws(r2)?;
                    r = r2;
                }
                let (r, _) = parse_char(r, ')')?;
                if scalar_item_assign {
                    return Ok((r, Expr::ArrayLiteral(items)));
                }
                // Wrap in Grouped so that expand_call_arg does not split the
                // parenthesized assignment's RHS back into separate call args.
                return Ok((
                    r,
                    Expr::Grouped(Box::new(Expr::AssignExpr {
                        name,
                        expr: Box::new(Expr::ArrayLiteral(items)),
                        is_bind: false,
                    })),
                ));
            }
        }
    }
    let (rest, lhs) = expression_no_sequence(rest)?;
    let (rest, _) = ws(rest)?;
    if let Some((stripped, negate)) = super::strip_atomic_compound_assign(rest) {
        if !matches!(lhs, Expr::Var(_)) && !crate::parser::expr::is_atomic_elem_target(&lhs) {
            return Err(PError::expected("atomic compound assignment expression"));
        }
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, super::atomic_compound_call(lhs, rhs, negate)));
    }
    if let Some((stripped, op)) = parse_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, build_compound_assign_expr(lhs, op, rhs)?));
    }
    if let Some((stripped, op_name)) = parse_custom_compound_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, build_custom_compound_assign_expr(lhs, op_name, rhs)?));
    }
    if let Some((stripped, meta, op)) = parse_bracket_meta_assign_op(rest) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, build_meta_assign_expr(lhs, meta, op, rhs)?));
    }
    if let Some(stripped) = rest.strip_prefix("::=").or_else(|| rest.strip_prefix(":=")) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => {
                let (r, first) = expression_no_sequence(rest)?;
                let (r2, _) = ws(r)?;
                if r2.starts_with(',') && !r2.starts_with(",,") {
                    let mut items = vec![first];
                    let mut r = r2;
                    while r.starts_with(',') && !r.starts_with(",,") {
                        let (r2, _) = parse_char(r, ',')?;
                        let (r2, _) = ws(r2)?;
                        if r2.starts_with(')') {
                            r = r2;
                            break;
                        }
                        let (r2, item) = expression_no_sequence(r2)?;
                        items.push(item);
                        let (r2, _) = ws(r2)?;
                        r = r2;
                    }
                    (r, Expr::ArrayLiteral(items))
                } else {
                    (r, first)
                }
            }
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        let expr = match lhs {
            Expr::Var(name) => Expr::AssignExpr {
                name,
                expr: Box::new(rhs),
                is_bind: true,
            },
            Expr::ArrayVar(name) => Expr::AssignExpr {
                name: format!("@{}", name),
                expr: Box::new(rhs),
                is_bind: true,
            },
            Expr::HashVar(name) => Expr::AssignExpr {
                name: format!("%{}", name),
                expr: Box::new(rhs),
                is_bind: true,
            },
            Expr::Index {
                target,
                index,
                is_positional,
                spelling,
                ..
            } => Expr::IndexAssign {
                target,
                index,
                value: Box::new(rhs),
                is_positional,
                spelling,
            },
            Expr::MultiDimIndex {
                target,
                dimensions,
                is_positional,
            } => Expr::MultiDimIndexAssign {
                target,
                dimensions,
                value: Box::new(rhs),
                is_positional,
            },
            _ => return Err(PError::expected("assignment expression")),
        };
        return Ok((rest, expr));
    }
    if (!rest.starts_with('=') || rest.starts_with("==") || rest.starts_with("=>"))
        && !rest.starts_with("⚛=")
    {
        return Err(PError::expected("assignment expression"));
    }
    let atomic_rest = super::strip_atomic_store_assign(rest);
    let is_atomic = atomic_rest.is_some();
    let rest = atomic_rest.unwrap_or_else(|| &rest[1..]);
    let (rest, _) = ws(rest)?;
    // `$(EXPR) = a, b` forces ITEM assignment: `$(...)` names the same container
    // as EXPR but in item context, so the comma is NOT part of the RHS
    // (`(($(EXPR) = a), b)`) -- unlike a bare `(@a[0] = a, b)` which is list
    // assignment and slurps the comma. Parse the RHS comma-blind and fold any
    // trailing comma list into the parenthesized value.
    let item_form =
        matches!(lhs, Expr::Contextualizer { .. }).then(|| lhs.clone().contextualizer_call());
    if let Expr::MethodCall {
        target, name, args, ..
    } = item_form.as_ref().unwrap_or(&lhs)
        && name == "item"
        && args.is_empty()
        && !is_atomic
    {
        let inner_target = (**target).clone();
        let (r, value) = expression_no_sequence(rest)?;
        let assign = crate::parser::expr::precedence::assign_to_target_expr(inner_target, value);
        let (mut r, _) = ws(r)?;
        if r.starts_with(',') && !r.starts_with(",,") {
            let mut items = vec![assign];
            while r.starts_with(',') && !r.starts_with(",,") {
                let (c, _) = parse_char(r, ',')?;
                let (c, _) = ws(c)?;
                if c.starts_with(')') {
                    r = c;
                    break;
                }
                let (c, item) = expression_no_sequence(c)?;
                items.push(item);
                let (c, _) = ws(c)?;
                r = c;
            }
            let (r, _) = parse_char(r, ')')?;
            return Ok((r, Expr::ArrayLiteral(items)));
        }
        let (r, _) = parse_char(r, ')')?;
        return Ok((r, assign));
    }
    // Parse RHS: try chained assignment first, then comma-separated list for multi-value
    // assignment (e.g. `(%h{|| @a} = 42,666)`).
    let (rest, rhs) = match try_parse_assign_expr(rest) {
        Ok(r) => r,
        Err(_) => {
            let (r, first) = expression_no_sequence(rest)?;
            let (r2, _) = ws(r)?;
            if r2.starts_with(',') && !r2.starts_with(",,") {
                // Comma-separated RHS: collect into an ArrayLiteral
                let mut items = vec![first];
                let mut r = r2;
                while r.starts_with(',') && !r.starts_with(",,") {
                    let (r2, _) = parse_char(r, ',')?;
                    let (r2, _) = ws(r2)?;
                    if r2.starts_with(')') {
                        r = r2;
                        break;
                    }
                    let (r2, item) = expression_no_sequence(r2)?;
                    items.push(item);
                    let (r2, _) = ws(r2)?;
                    r = r2;
                }
                (r, Expr::ArrayLiteral(items))
            } else {
                (r, first)
            }
        }
    };
    let (rest, _) = ws(rest)?;
    let (rest, _) = parse_char(rest, ')')?;
    if let Some(expr) = subscript_adverb_lvalue_assign_expr(lhs.clone(), rhs.clone()) {
        return Ok((rest, expr));
    }
    let expr = match lhs {
        Expr::Var(name) => Expr::AssignExpr {
            name,
            expr: Box::new(rhs),
            is_bind: false,
        },
        Expr::ArrayVar(name) => Expr::AssignExpr {
            name: format!("@{}", name),
            expr: Box::new(rhs),
            is_bind: false,
        },
        Expr::HashVar(name) => Expr::AssignExpr {
            name: format!("%{}", name),
            expr: Box::new(rhs),
            is_bind: false,
        },
        Expr::Index {
            target,
            index,
            is_positional,
            spelling,
            ..
        } => Expr::IndexAssign {
            target,
            index,
            value: Box::new(rhs),
            is_positional,
            spelling,
        },
        Expr::MultiDimIndex {
            target,
            dimensions,
            is_positional,
        } => Expr::MultiDimIndexAssign {
            target,
            dimensions,
            value: Box::new(rhs),
            is_positional,
        },
        Expr::MethodCall {
            target,
            name,
            args,
            modifier,
            ..
        } => {
            if name == "AT-POS"
                && args.len() == 1
                && !matches!(target.as_ref(), Expr::Var(name) | Expr::BareWord(name) if name == "self")
            {
                Expr::IndexAssign {
                    target,
                    index: Box::new(args.into_iter().next().unwrap_or(Expr::Literal(Value::NIL))),
                    value: Box::new(rhs),
                    is_positional: true,
                    spelling: Default::default(),
                }
            } else {
                let target_var_name =
                    crate::parser::stmt::simple_expr_stmt::lvalue::method_lvalue_target_name(
                        &target,
                    );
                let method_name = if let Some(m @ ('!' | '^')) = modifier {
                    format!("{m}{}", name.resolve())
                } else {
                    name.resolve()
                };
                method_lvalue_assign_expr(*target, target_var_name, method_name, args, rhs)
            }
        }
        Expr::Call { name, args, .. } => named_sub_lvalue_assign_expr(name.resolve(), args, rhs),
        Expr::CallOn { target, args } => match *target {
            Expr::ArrayLiteral(items) if args.is_empty() => paren_list_assign_expr(items, rhs),
            target => callable_lvalue_assign_expr(target, args, rhs),
        },
        Expr::ArrayLiteral(items) => paren_list_assign_expr(items, rhs),
        Expr::BareWord(name) => Expr::AssignExpr {
            name,
            expr: Box::new(rhs),
            is_bind: false,
        },
        // Fallback for other lvalue expressions
        other => callable_lvalue_assign_expr(other, Vec::new(), rhs),
    };
    if is_atomic {
        if let Expr::AssignExpr { name, expr, .. } = expr {
            return Ok((rest, crate::ast::atomic_op::store_var(name, *expr)));
        }
        return Err(PError::expected("atomic assignment expression"));
    }
    Ok((rest, expr))
}

pub(crate) fn looks_like_parenthesized_assignment(input: &str) -> bool {
    if !input.starts_with('(') {
        return false;
    }
    let mut chars = input.char_indices().peekable();
    let mut depth = 0usize;
    let mut in_single = false;
    let mut in_double = false;
    while let Some((idx, ch)) = chars.next() {
        if in_single {
            if ch == '\'' {
                in_single = false;
            }
            continue;
        }
        if in_double {
            if ch == '\\' {
                let _ = chars.next();
                continue;
            }
            if ch == '"' {
                in_double = false;
            }
            continue;
        }
        match ch {
            '\'' => in_single = true,
            '"' => in_double = true,
            '(' => depth += 1,
            ')' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    break;
                }
            }
            ':' if depth == 1 => {
                if input[idx + ch.len_utf8()..].starts_with('=') {
                    return true;
                }
            }
            '=' if depth == 1 => {
                let next = input[idx + ch.len_utf8()..].chars().next();
                if !matches!(next, Some('=') | Some('>')) {
                    return true;
                }
            }
            _ => {}
        }
    }
    false
}

/// `(LVALUES) = rhs`: a single target among `*` placeholders takes one item
/// (`list_lvalue_assign_expr`); any other list assigns through the list
/// container as a callable lvalue. RakuAST's lowering builds the same record
/// for an assignment to a parenthesised list.
// Cost: O(n), n = number of list items.
pub(crate) fn paren_list_assign_expr(items: Vec<Expr>, rhs: Expr) -> Expr {
    if let Some(expr) = list_lvalue_assign_expr(items.clone(), rhs.clone()) {
        return expr;
    }
    callable_lvalue_assign_expr(Expr::ArrayLiteral(items), Vec::new(), rhs)
}
