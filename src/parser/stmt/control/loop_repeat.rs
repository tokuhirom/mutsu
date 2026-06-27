use super::*;

/// Parse a single init/step item for a C-style loop header.
/// In loop headers, commas separate statements, so `my $a = 1, my $b = 2`
/// must parse as two separate declarations, not one list assignment.
/// This function parses `my`/`state` declarations using `expression()` (not
/// `parse_assign_expr_or_comma()`) for the RHS, preventing comma consumption.
fn loop_header_item(input: &str) -> PResult<'_, Stmt> {
    let (input, _) = ws(input)?;
    // Try simple my/state scalar declarations with non-comma-consuming RHS.
    // For `loop (my $a = 1, my $b = 2; ...)`, commas must separate
    // declarations, not be part of the assignment value.
    if keyword("my", input).is_some() || keyword("state", input).is_some() {
        let is_state = keyword("state", input).is_some();
        let rest = keyword("my", input)
            .or_else(|| keyword("state", input))
            .unwrap();
        let (rest, _) = ws1(rest)?;
        // Only handle simple scalar declarations (no type constraint).
        // Typed declarations like `my int $i` fall through to statement().
        if let Ok((rest, name)) = var_name(rest) {
            // var_name strips the sigil; VarDecl name does not include the sigil
            let (rest, _) = ws(rest)?;
            if rest.starts_with('=') && !rest.starts_with("==") && !rest.starts_with("=>") {
                let rest = &rest[1..];
                let (rest, _) = ws(rest)?;
                let (rest, expr) = expression(rest)?;
                return Ok((
                    rest,
                    Stmt::VarDecl {
                        name,
                        expr,
                        type_constraint: None,
                        is_state,
                        is_our: false,
                        is_dynamic: false,
                        is_export: false,
                        export_tags: Vec::new(),
                        custom_traits: Vec::new(),
                        where_constraint: None,
                    },
                ));
            }
            return Ok((
                rest,
                Stmt::VarDecl {
                    name,
                    expr: Expr::Literal(crate::value::Value::Nil),
                    type_constraint: None,
                    is_state,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    where_constraint: None,
                },
            ));
        }
        // Typed or complex declaration (e.g., my int $i = 0) — fall through
        // to statement() which handles the full my_decl grammar.
        return statement(input);
    }
    // Fall back to expression statement (also non-comma-consuming)
    let (rest, expr) = expression(input)?;
    Ok((rest, Stmt::Expr(expr)))
}

/// Build a typed X::Syntax::Malformed error for a bad C-style loop spec.
/// `what` is the `loop spec (...)` description matched by the roast tests.
fn malformed_loop_spec_error(what: &str) -> PError {
    let message = format!("X::Syntax::Malformed: Malformed {}.", what);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), Value::str(message.clone()));
    attrs.insert("what".to_string(), Value::str(what.to_string()));
    let exception = Value::make_instance(Symbol::intern("X::Syntax::Malformed"), attrs);
    PError::fatal_with_exception(message, Box::new(exception))
}

/// Return the slice from just-after `loop (` up to (excluding) the matching `)`,
/// honoring nested brackets and quotes, or None if unbalanced.
fn loop_spec_content(after_open_paren: &str) -> Option<&str> {
    let bytes = after_open_paren.as_bytes();
    let mut depth = 0i32;
    let mut quote: Option<u8> = None;
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if let Some(q) = quote {
            if c == b'\\' {
                i += 2;
                continue;
            }
            if c == q {
                quote = None;
            }
        } else {
            match c {
                b'\'' | b'"' => quote = Some(c),
                b'(' | b'[' | b'{' => depth += 1,
                b']' | b'}' => depth -= 1,
                b')' => {
                    if depth == 0 {
                        return Some(&after_open_paren[..i]);
                    }
                    depth -= 1;
                }
                _ => {}
            }
        }
        i += 1;
    }
    None
}

/// Count the top-level `;`-separated groups in a C-style loop spec (respecting
/// nested brackets/quotes). A valid spec has exactly 3.
fn loop_spec_semicolon_groups(spec: &str) -> usize {
    let bytes = spec.as_bytes();
    let mut depth = 0i32;
    let mut quote: Option<u8> = None;
    let mut semis = 0usize;
    let mut i = 0;
    while i < bytes.len() {
        let c = bytes[i];
        if let Some(q) = quote {
            if c == b'\\' {
                i += 2;
                continue;
            }
            if c == q {
                quote = None;
            }
        } else {
            match c {
                b'\'' | b'"' => quote = Some(c),
                b'(' | b'[' | b'{' => depth += 1,
                b')' | b']' | b'}' => depth -= 1,
                b';' if depth == 0 => semis += 1,
                _ => {}
            }
        }
        i += 1;
    }
    semis + 1
}

/// Validate a C-style loop spec: exactly 3 semicolon-separated expressions.
/// Returns an X::Syntax::Malformed error (with the right `what`) otherwise.
fn check_loop_spec(spec: &str) -> Option<PError> {
    if spec.trim().is_empty() {
        return Some(malformed_loop_spec_error(
            "loop spec (expected 3 semicolon-separated expressions)",
        ));
    }
    let groups = loop_spec_semicolon_groups(spec);
    if groups == 3 {
        return None;
    }
    let what = if groups > 3 {
        "loop spec (expected 3 semicolon-separated expressions but got more)".to_string()
    } else {
        format!("loop spec (expected 3 semicolon-separated expressions but got {groups})")
    };
    Some(malformed_loop_spec_error(&what))
}

/// Parse C-style `loop` or infinite loop.
pub(crate) fn loop_stmt(input: &str) -> PResult<'_, Stmt> {
    let after_kw = keyword("loop", input).ok_or_else(|| PError::expected("loop statement"))?;
    let (rest, _) = ws(after_kw)?;
    // Whitespace between `loop` and `(` marks a real C-style loop spec; `loop(`
    // with no space is a keyword-as-function mistake handled elsewhere, so the
    // spec validation below must NOT preempt it.
    let had_space_before_paren = rest.len() < after_kw.len();
    if rest.starts_with('(') {
        let (rest, _) = parse_char(rest, '(')?;
        // Reject a malformed loop spec (not exactly 3 `;`-separated expressions)
        // with a typed X::Syntax::Malformed before the lenient parse below.
        if had_space_before_paren
            && let Some(spec) = loop_spec_content(rest)
            && let Some(err) = check_loop_spec(spec)
        {
            return Err(err);
        }
        let (rest, _) = ws(rest)?;
        // init: may be comma-separated list of statements
        let (rest, init) = if let Some(rest_after_semi) = rest.strip_prefix(';') {
            (rest_after_semi, None)
        } else {
            let (mut r, first) = loop_header_item(rest)?;
            let mut stmts = vec![first];
            loop {
                let (r2, _) = ws(r)?;
                if let Some(r2_after_comma) = r2.strip_prefix(',') {
                    let (r2, _) = ws(r2_after_comma)?;
                    let (r2, s) = loop_header_item(r2)?;
                    stmts.push(s);
                    r = r2;
                } else {
                    r = r2;
                    break;
                }
            }
            if stmts.len() == 1 {
                (r, Some(Box::new(stmts.into_iter().next().unwrap())))
            } else {
                (r, Some(Box::new(Stmt::SyntheticBlock(stmts))))
            }
        };
        let (rest, _) = ws(rest)?;
        // When init was parsed by loop_header_item, the semicolon separating
        // init from cond is not yet consumed. Consume it now.
        // When init is None, the semicolon was already consumed by strip_prefix(';').
        let rest = if init.is_some() {
            if let Some(r) = rest.strip_prefix(';') {
                r
            } else {
                rest
            }
        } else {
            rest
        };
        let (rest, _) = ws(rest)?;
        // cond
        let (rest, cond) = if let Some(rest) = rest.strip_prefix(';') {
            (rest, None)
        } else {
            let (r, e) = expression(rest)?;
            let (r, _) = ws(r)?;
            let (r, _) = parse_char(r, ';')?;
            (r, Some(e))
        };
        let (rest, _) = ws(rest)?;
        // step: may be comma-separated list of expressions
        let (rest, step) = if rest.starts_with(')') {
            (rest, None)
        } else {
            let (mut r, first) = expression(rest)?;
            let mut exprs = vec![first];
            loop {
                let (r2, _) = ws(r)?;
                if let Some(r2_after_comma) = r2.strip_prefix(',') {
                    let (r2, _) = ws(r2_after_comma)?;
                    let (r2, e) = expression(r2)?;
                    exprs.push(e);
                    r = r2;
                } else {
                    r = r2;
                    break;
                }
            }
            if exprs.len() == 1 {
                (r, Some(exprs.into_iter().next().unwrap()))
            } else {
                // Wrap multiple step expressions in a DoBlock
                let stmts: Vec<Stmt> = exprs.into_iter().map(Stmt::Expr).collect();
                (
                    r,
                    Some(Expr::DoBlock {
                        body: stmts,
                        label: None,
                    }),
                )
            }
        };
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        let (rest, _) = ws(rest)?;
        let (rest, body) = block(rest)?;
        return Ok((
            rest,
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat: false,
                label: None,
            },
        ));
    }
    // Infinite loop
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    // `loop {} while COND` is a syntax error — use `repeat while` instead
    let (check, _) = ws(rest)?;
    if keyword("while", check).is_some() || keyword("until", check).is_some() {
        return Err(PError::fatal(
            "X::Syntax::Confused: Strange text after block (missing semicolon or comma?)"
                .to_string(),
        ));
    }
    Ok((
        rest,
        Stmt::Loop {
            init: None,
            cond: None,
            step: None,
            body,
            repeat: false,
            label: None,
        },
    ))
}

/// Parse `repeat while/until` loop.
pub(crate) fn repeat_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("repeat", input).ok_or_else(|| PError::expected("repeat statement"))?;
    let (rest, _) = ws(rest)?;

    // repeat while/until COND { BODY }
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, (param, _param_def, params, _params_def, _rw_block, _explicit_zero)) =
            parse_for_params(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        let repeat_param = param.or_else(|| params.into_iter().next());
        let init = repeat_param.as_ref().map(|name| {
            Box::new(Stmt::VarDecl {
                name: name.clone(),
                expr: Expr::Literal(crate::value::Value::Nil),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            })
        });
        let step = repeat_param.map(|name| Expr::AssignExpr {
            name,
            expr: Box::new(Expr::Literal(crate::value::Value::Bool(true))),
            is_bind: false,
        });
        return Ok((
            r,
            Stmt::Loop {
                init,
                cond: Some(cond),
                step,
                body,
                repeat: true,
                label: None,
            },
        ));
    }
    if let Some(r) = keyword("until", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, (param, _param_def, params, _params_def, _rw_block, _explicit_zero)) =
            parse_for_params(r)?;
        let (r, _) = ws(r)?;
        let (r, body) = block(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        let repeat_param = param.or_else(|| params.into_iter().next());
        let init = repeat_param.as_ref().map(|name| {
            Box::new(Stmt::VarDecl {
                name: name.clone(),
                expr: Expr::Literal(crate::value::Value::Nil),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            })
        });
        let step = repeat_param.map(|name| Expr::AssignExpr {
            name,
            expr: Box::new(Expr::Literal(crate::value::Value::Bool(true))),
            is_bind: false,
        });
        return Ok((
            r,
            Stmt::Loop {
                init,
                cond: Some(Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                }),
                step,
                body,
                repeat: true,
                label: None,
            },
        ));
    }

    // repeat { BODY } while/until COND
    let (rest, body) = block(rest)?;
    let (rest, _) = ws(rest)?;
    if let Some(r) = keyword("while", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::Loop {
                init: None,
                cond: Some(cond),
                step: None,
                body,
                repeat: true,
                label: None,
            },
        ));
    }
    if let Some(r) = keyword("until", rest) {
        let (r, _) = ws1(r)?;
        let (r, cond) = condition_expr(r)?;
        let (r, _) = ws(r)?;
        let (r, _) = opt_char(r, ';');
        return Ok((
            r,
            Stmt::Loop {
                init: None,
                cond: Some(Expr::Unary {
                    op: TokenKind::Bang,
                    expr: Box::new(cond),
                }),
                step: None,
                body,
                repeat: true,
                label: None,
            },
        ));
    }
    Err(PError::fatal(
        "X::Syntax::Missing: \"while\" or \"until\" required after repeat".to_string(),
    ))
}
