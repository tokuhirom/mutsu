use super::*;

pub(crate) fn parenthesized_assign_expr(input: &str) -> PResult<'_, Expr> {
    let (rest, _) = parse_char(input, '(')?;
    let (rest, _) = ws(rest)?;
    if let Ok((rest_inner, inner_assign)) = try_parse_assign_expr(rest) {
        let (rest_inner, _) = ws(rest_inner)?;
        if let Ok((rest_after_paren, _)) = parse_char(rest_inner, ')') {
            return Ok((rest_after_paren, inner_assign));
        }
    }
    let (rest, lhs) = expression_no_sequence(rest)?;
    let (rest, _) = ws(rest)?;
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
    if let Expr::MethodCall { name, args, .. } = item_form.as_ref().unwrap_or(&lhs)
        && name == "item"
        && args.is_empty()
        && !is_atomic
    {
        let (r, value) = expression_no_sequence(rest)?;
        let assign = super::written_assignment(lhs.clone(), value);
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
    if is_atomic {
        let name = match lhs {
            Expr::Var(name) => name,
            Expr::ArrayVar(name) => format!("@{name}"),
            Expr::HashVar(name) => format!("%{name}"),
            Expr::BareWord(name) => crate::term_names::bareword_target_key(&name),
            _ => return Err(PError::expected("atomic assignment expression")),
        };
        return Ok((rest, crate::ast::atomic_op::store_var(name, rhs)));
    }
    // The typed assignment node, target as written; its lowering owns the
    // per-target dispatch (`rakuast::assign_lower`).
    if crate::rakuast::is_expression_assign_target(&lhs)
        || crate::rakuast::is_callable_lvalue_assign_target(&lhs)
    {
        return Ok((rest, super::written_assignment(lhs, rhs)));
    }
    // Fallback for other lvalue expressions
    Ok((rest, callable_lvalue_assign_expr(lhs, Vec::new(), rhs)))
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
