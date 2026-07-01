use super::*;

/// Parse a single argument in colon method-call syntax (.method: arg1, arg2).
/// Tries colonpair first (:name, :$var, :!flag, :0port), then expression.
pub(crate) fn parse_colon_method_arg(input: &str) -> PResult<'_, Expr> {
    if input.starts_with(':')
        && !input.starts_with("::")
        && let Ok(result) = crate::parser::primary::misc::colonpair_expr(input)
    {
        return Ok(result);
    }
    expression(input)
}

/// Try to parse a single assignment expression: $var op= expr or $var = expr.
/// Returns the expression as Expr::AssignExpr.
pub(in crate::parser) fn try_parse_assign_expr(input: &str) -> PResult<'_, Expr> {
    if input.starts_with('(') {
        if !looks_like_parenthesized_assignment(input) {
            return Err(PError::expected("assignment expression"));
        }
        return parenthesized_assign_expr(input);
    }
    let sigil = input.as_bytes().first().copied().unwrap_or(0);
    if sigil == b'$' && input.as_bytes().get(1).is_some_and(|c| *c == b'=') {
        return Err(PError::expected("assignment expression"));
    }
    if sigil != b'$' && sigil != b'@' && sigil != b'%' {
        return Err(PError::expected("assignment expression"));
    }
    let (r, var) = var_name(input)?;

    // Handle subscripted lvalues: @a[1] = ..., %h{key} = ..., $a[0] = ...
    if r.starts_with('[') || r.starts_with('{') || r.starts_with('<') {
        let closing = match r.as_bytes()[0] {
            b'[' => ']',
            b'{' => '}',
            _ => '>',
        };
        let (r_idx, _) = parse_char(r, r.as_bytes()[0] as char)?;
        let (r_idx, _) = ws(r_idx)?;
        // Parse comma-separated index expressions inside brackets
        let (r_idx, first_expr) = expression(r_idx)?;
        let (mut r_idx, _) = ws(r_idx)?;
        let index_expr = if r_idx.starts_with(',') {
            let mut items = vec![first_expr];
            while r_idx.starts_with(',') {
                let (r2, _) = parse_char(r_idx, ',')?;
                let (r2, _) = ws(r2)?;
                if r2.starts_with(closing) {
                    r_idx = r2;
                    break;
                }
                let (r2, next) = expression(r2)?;
                items.push(next);
                let (r2, _) = ws(r2)?;
                r_idx = r2;
            }
            Expr::ArrayLiteral(items)
        } else {
            first_expr
        };
        let (r_idx, _) = parse_char(r_idx, closing)?;
        let (r_after, _) = ws(r_idx)?;
        // Check for binding assignment (:= or ::=)
        if let Some(stripped) = r_after
            .strip_prefix("::=")
            .or_else(|| r_after.strip_prefix(":="))
        {
            let (rest, _) = ws(stripped)?;
            let (rest, rhs) = match try_parse_assign_expr(rest) {
                Ok(r) => r,
                Err(_) => expression(rest)?,
            };
            let target = match sigil {
                b'@' => Expr::ArrayVar(var.to_string()),
                b'%' => Expr::HashVar(var.to_string()),
                _ => Expr::Var(var.to_string()),
            };
            return Ok((
                rest,
                Expr::IndexAssign {
                    target: Box::new(target),
                    index: Box::new(index_expr),
                    value: Box::new(rhs),
                    is_positional: true,
                },
            ));
        }
        // Check for simple assignment
        if (r_after.starts_with('=') && !r_after.starts_with("==") && !r_after.starts_with("=>"))
            || r_after.starts_with("⚛=")
        {
            let r3 = if let Some(stripped) = r_after.strip_prefix("⚛=") {
                stripped
            } else {
                &r_after[1..]
            };
            let (rest, _) = ws(r3)?;
            let (rest, rhs) = match try_parse_assign_expr(rest) {
                Ok(r) => r,
                Err(_) => expression(rest)?,
            };
            let target = match sigil {
                b'@' => Expr::ArrayVar(var.to_string()),
                b'%' => Expr::HashVar(var.to_string()),
                _ => Expr::Var(var.to_string()),
            };
            return Ok((
                rest,
                Expr::IndexAssign {
                    target: Box::new(target),
                    index: Box::new(index_expr),
                    value: Box::new(rhs),
                    is_positional: true,
                },
            ));
        }
        return Err(PError::expected("assignment expression"));
    }

    let (r2, _) = ws(r)?;
    let prefix = match sigil {
        b'@' => "@",
        b'%' => "%",
        _ => "",
    };
    // .= mutating method call: $var .= method(args) => $var = $var.method(args)
    if let Some(stripped) = r2.strip_prefix(".=") {
        let (r, _) = ws(stripped)?;
        let name = format!("{}{}", prefix, var);
        let method_target = match sigil {
            b'@' => Expr::ArrayVar(var.to_string()),
            b'%' => Expr::HashVar(var.to_string()),
            _ => Expr::Var(var.to_string()),
        };
        // Check for quoted method name: .="method"() or .='method'()
        if let Some((r_after_quote, qname)) = parse_quoted_method_name(r) {
            // Quoted method names require parenthesized arguments
            let (r_after_quote, _) = ws(r_after_quote)?;
            let (rest, args) = if r_after_quote.starts_with('(') {
                let (r2, _) = parse_char(r_after_quote, '(')?;
                let (r2, _) = ws(r2)?;
                let (r2, a) = parse_call_arg_list(r2)?;
                let (r2, _) = ws(r2)?;
                let (r2, _) = parse_char(r2, ')')?;
                (r2, a)
            } else {
                (r_after_quote, vec![])
            };
            let method_expr = match qname {
                QuotedMethodName::Static(mname) => Expr::MethodCall {
                    target: Box::new(method_target),
                    name: Symbol::intern(&mname),
                    args,
                    modifier: None,
                    quoted: true,
                },
                QuotedMethodName::Dynamic(name_expr) => Expr::DynamicMethodCall {
                    target: Box::new(method_target),
                    name_expr: Box::new(name_expr),
                    args,
                    modifier: None,
                },
            };
            return Ok((
                rest,
                Expr::AssignExpr {
                    name,
                    expr: Box::new(method_expr),
                    is_bind: false,
                },
            ));
        }
        // Parse regular method name
        let (r, method_name) =
            take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
        let r_before_ws = r;
        let (r, _) = ws(r)?;
        // Parse optional args in parens or colon-form
        let (rest, args) = if r.starts_with('(') {
            let (r, _) = parse_char(r, '(')?;
            let (r, _) = ws(r)?;
            if r.starts_with(')') {
                let (r, _) = parse_char(r, ')')?;
                (r, vec![])
            } else {
                let mut args = Vec::new();
                let (mut r, first) = expression_no_sequence(r)?;
                args.push(first);
                loop {
                    let (r2, _) = ws(r)?;
                    if !r2.starts_with(',') {
                        r = r2;
                        break;
                    }
                    let (r2, _) = ws(&r2[1..])?;
                    let (r2, next) = expression_no_sequence(r2)?;
                    args.push(next);
                    r = r2;
                }
                let (r, _) = ws(r)?;
                let (r, _) = parse_char(r, ')')?;
                (r, args)
            }
        } else if r_before_ws.starts_with(':') && !r_before_ws.starts_with("::") {
            // Colon-arg syntax: .=method: arg, arg2 (no space before colon)
            let r = &r[1..];
            let (r, _) = ws(r)?;
            let (r, first_arg) = parse_colon_method_arg(r)?;
            let mut args = vec![first_arg];
            let mut r_inner = r;
            loop {
                let (r2, _) = ws(r_inner)?;
                // Adjacent colonpairs without comma
                if r2.starts_with(':')
                    && !r2.starts_with("::")
                    && let Ok((r3, arg)) = crate::parser::primary::misc::colonpair_expr(r2)
                {
                    args.push(arg);
                    r_inner = r3;
                    continue;
                }
                if !r2.starts_with(',') {
                    break;
                }
                let r2 = &r2[1..];
                let (r2, _) = ws(r2)?;
                // Handle trailing comma before ';' or '}'
                if r2.starts_with(';') || r2.starts_with('}') || r2.is_empty() {
                    r_inner = r2;
                    break;
                }
                let (r2, next) = parse_colon_method_arg(r2)?;
                args.push(next);
                r_inner = r2;
            }
            (r_inner, args)
        } else if r.starts_with(':') && !r.starts_with("::") {
            // Fake-infix adverb form: .=method :key<val> (space before colon)
            let mut args = Vec::new();
            let mut r_inner = r;
            while r_inner.starts_with(':') && !r_inner.starts_with("::") {
                if let Ok((r2, arg)) = crate::parser::primary::misc::colonpair_expr(r_inner) {
                    args.push(arg);
                    let (r3, _) = ws(r2)?;
                    r_inner = r3;
                } else {
                    break;
                }
            }
            (r_inner, args)
        } else {
            (r, vec![])
        };
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(Expr::MethodCall {
                    target: Box::new(method_target),
                    name: Symbol::intern(method_name),
                    args,
                    modifier: None,
                    quoted: false,
                }),
                is_bind: false,
            },
        ));
    }
    if let Some((stripped, op)) = parse_compound_assign_op(r2) {
        let (rest, _) = ws(stripped)?;
        // RHS precedence: the word-form loose logicals (`or=`, `and=`, `xor=`,
        // `orelse=`, `andthen=`, `notandthen=`) are LIST-assignment operators, so
        // their RHS absorbs the whole comma list (`$a or= 3, 4` => `$a or= (3,4)`).
        // All other compound ops are item-assignment: comma-blind RHS.
        let (rest, rhs) = if op.is_list_precedence() {
            parse_comma_or_expr(rest)?
        } else {
            match try_parse_assign_expr(rest) {
                Ok(r) => r,
                Err(_) => expression_no_sequence(rest)?,
            }
        };
        let name = format!("{}{}", prefix, var);
        // Read the current value through the sigil-appropriate lvalue so `%h ,= %g`
        // / `@a ,= 3` treat the LHS as a Hash/Array container (not a scalar
        // `Var("h")`), letting the comma operator merge/append the two containers.
        let lhs_expr = match sigil {
            b'@' => Expr::ArrayVar(var.to_string()),
            b'%' => Expr::HashVar(var.to_string()),
            _ => Expr::Var(var.to_string()),
        };
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(compound_assigned_value_expr(lhs_expr, op, rhs)),
                is_bind: false,
            },
        ));
    }
    if let Some((stripped, meta, op)) = parse_meta_compound_assign_op(r2) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        // For scalar variables with Z-meta, fall back to plain compound assignment
        // since scalar Zip on single values is equivalent to the base operator.
        // This avoids Z+ returning an array when assigning to a scalar.
        if meta == "Z"
            && sigil == b'$'
            && let Some(compound_op) = CompoundAssignOp::from_op_name(&op)
        {
            return Ok((
                rest,
                Expr::AssignExpr {
                    name,
                    expr: Box::new(compound_assigned_value_expr(
                        Expr::Var(var.to_string()),
                        compound_op,
                        rhs,
                    )),
                    is_bind: false,
                },
            ));
        }
        let var_expr = match sigil {
            b'@' => Expr::ArrayVar(var.to_string()),
            b'%' => Expr::HashVar(var.to_string()),
            _ => Expr::Var(name.clone()),
        };
        // The reverse meta-op assignment `$x R op= $y` assigns to its RIGHT
        // operand (`$y = $y op $x`), so retarget the assignment to `rhs`.
        if meta == "R" {
            let value = Expr::MetaOp {
                meta,
                op,
                left: Box::new(var_expr),
                right: Box::new(rhs.clone()),
            };
            return Ok((
                rest,
                crate::parser::expr::precedence::assign_to_target_expr(rhs, value),
            ));
        }
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(Expr::MetaOp {
                    meta,
                    op,
                    left: Box::new(var_expr),
                    right: Box::new(rhs),
                }),
                is_bind: false,
            },
        ));
    }
    if let Some((stripped, meta, op)) = parse_bracket_meta_assign_op(r2) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        let var_expr = match sigil {
            b'@' => Expr::ArrayVar(var.to_string()),
            b'%' => Expr::HashVar(var.to_string()),
            _ => Expr::Var(name.clone()),
        };
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(Expr::MetaOp {
                    meta,
                    op,
                    left: Box::new(var_expr),
                    right: Box::new(rhs),
                }),
                is_bind: false,
            },
        ));
    }
    if let Some((stripped, op_name)) = parse_custom_compound_assign_op(r2) {
        let (rest, _) = ws(stripped)?;
        // RHS: no sequence for custom compound assign
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression_no_sequence(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        return Ok((
            rest,
            Expr::AssignExpr {
                name: name.clone(),
                expr: Box::new(Expr::InfixFunc {
                    name: op_name,
                    left: Box::new(Expr::Var(name)),
                    right: vec![rhs],
                    modifier: None,
                }),
                is_bind: false,
            },
        ));
    }
    if let Some(stripped) = r2.strip_prefix("::=").or_else(|| r2.strip_prefix(":=")) {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        return Ok((
            rest,
            Expr::AssignExpr {
                name,
                expr: Box::new(rhs),
                is_bind: true,
            },
        ));
    }
    if let Some(stripped) = r2.strip_prefix("⚛+=") {
        let (rest, _) = ws(stripped)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => expression(rest)?,
        };
        let name = format!("{}{}", prefix, var);
        return Ok((
            rest,
            Expr::Call {
                name: Symbol::intern("__mutsu_atomic_add_var"),
                args: vec![Expr::Literal(Value::str(name)), rhs],
            },
        ));
    }
    // Check simple chained assignment: $var = ...
    if (r2.starts_with('=') && !r2.starts_with("==") && !r2.starts_with("=>"))
        || r2.starts_with("⚛=")
    {
        let is_atomic = r2.starts_with("⚛=");
        let r3 = if is_atomic {
            &r2["⚛=".len()..]
        } else {
            &r2[1..]
        };
        let (rest, _) = ws(r3)?;
        let (rest, rhs) = match try_parse_assign_expr(rest) {
            Ok(r) => r,
            Err(_) => {
                if sigil == b'@' || sigil == b'%' {
                    expression(rest)?
                } else {
                    expression_no_sequence(rest)?
                }
            }
        };
        let name = format!("{}{}", prefix, var);
        if is_atomic {
            return Ok((
                rest,
                Expr::Call {
                    name: Symbol::intern("__mutsu_atomic_store_var"),
                    args: vec![Expr::Literal(Value::str(name)), rhs],
                },
            ));
        }
        let name = format!("{}{}", prefix, var);
        return Ok((
            rest,
            match rewrite_scalar_assignment_rhs_as_sink(name.clone(), rhs.clone()) {
                Some(expr) => expr,
                None => Expr::AssignExpr {
                    name,
                    expr: Box::new(rhs),
                    is_bind: false,
                },
            },
        ));
    }
    Err(PError::expected("assignment expression"))
}
