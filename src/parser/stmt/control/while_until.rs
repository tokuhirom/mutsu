use super::*;

/// Parse `while` loop.
pub(crate) fn while_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("while", input).ok_or_else(|| PError::expected("while statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, (param_binding, destructure_params)) = if rest.starts_with("->") {
        let (rest, (param, param_def, params, _params_def, _rw_block, _explicit_zero)) =
            parse_for_params(rest)?;
        if !params.is_empty() {
            return Err(PError::expected_at("single while pointy parameter", rest));
        }
        let destructure_params = param_def.and_then(|def| def.sub_signature);
        (rest, (param, destructure_params))
    } else {
        (rest, (None::<String>, None))
    };
    let (rest, _) = ws(rest)?;
    let (rest, mut body) = block(rest)?;
    if let (Some(param), Some(sub_params)) = (&param_binding, &destructure_params) {
        let mut binds = Vec::new();
        crate::param_destructure::destructure_binds(param, sub_params, &mut binds);
        binds.append(&mut body);
        body = binds;
    }
    // ADR-0048 D5: an explicit signature wins over a placeholder — a
    // `$^name` in the body of a loop that already declares a pointy
    // parameter is raku's `X::Signature::Placeholder`, "Placeholder
    // variable '$^c' cannot override existing signature". Reported here
    // rather than in the compiler because the pointy form is desugared away
    // (into a `VarDecl` plus a `While` over an `AssignExpr`) before codegen.
    if param_binding.is_some()
        && let Some(err) =
            crate::parser::stmt::sub::placeholder_overrides_signature_error(&body, &[])
    {
        return Err(err);
    }
    let (hoisted_decl, cond) = if param_binding.is_none() {
        split_loop_cond_decl(cond)
    } else {
        (None, cond)
    };
    let while_stmt = Stmt::While {
        cond: if let Some(ref param) = param_binding {
            Expr::AssignExpr {
                name: param.clone(),
                expr: Box::new(cond),
                is_bind: false,
            }
        } else {
            cond
        },
        body,
        label: None,
        is_statement_modifier: false,
        is_until: false,
    };
    if let Some(decl) = hoisted_decl {
        return Ok((rest, Stmt::Block(vec![decl, while_stmt])));
    }
    if let Some(param) = param_binding {
        Ok((
            rest,
            Stmt::Block(vec![
                Stmt::VarDecl {
                    name: param,
                    expr: Expr::Literal(crate::value::Value::NIL),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    where_constraint: None,
                },
                while_stmt,
            ]),
        ))
    } else {
        Ok((rest, while_stmt))
    }
}

/// Parse `until` loop.
pub(crate) fn until_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("until", input).ok_or_else(|| PError::expected("until statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, (param_binding, destructure_params)) = if rest.starts_with("->") {
        let (rest, (param, param_def, params, _params_def, _rw_block, _explicit_zero)) =
            parse_for_params(rest)?;
        if !params.is_empty() {
            return Err(PError::expected_at("single until pointy parameter", rest));
        }
        let destructure_params = param_def.and_then(|def| def.sub_signature);
        (rest, (param, destructure_params))
    } else {
        (rest, (None::<String>, None))
    };
    let (rest, _) = ws(rest)?;
    let (rest, mut body) = block(rest)?;
    if let (Some(param), Some(sub_params)) = (&param_binding, &destructure_params) {
        let mut binds = Vec::new();
        crate::param_destructure::destructure_binds(param, sub_params, &mut binds);
        binds.append(&mut body);
        body = binds;
    }
    // ADR-0048 D5: an explicit signature wins over a placeholder — a
    // `$^name` in the body of a loop that already declares a pointy
    // parameter is raku's `X::Signature::Placeholder`, "Placeholder
    // variable '$^c' cannot override existing signature". Reported here
    // rather than in the compiler because the pointy form is desugared away
    // (into a `VarDecl` plus a `While` over an `AssignExpr`) before codegen.
    if param_binding.is_some()
        && let Some(err) =
            crate::parser::stmt::sub::placeholder_overrides_signature_error(&body, &[])
    {
        return Err(err);
    }
    let (hoisted_decl, cond) = if param_binding.is_none() {
        split_loop_cond_decl(cond)
    } else {
        (None, cond)
    };
    let cond_expr = if let Some(ref param) = param_binding {
        Expr::AssignExpr {
            name: param.clone(),
            expr: Box::new(cond),
            is_bind: false,
        }
    } else {
        cond
    };
    let while_stmt = Stmt::While {
        cond: Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(cond_expr),
        },
        body,
        label: None,
        is_statement_modifier: false,
        is_until: true,
    };
    if let Some(decl) = hoisted_decl {
        return Ok((rest, Stmt::Block(vec![decl, while_stmt])));
    }
    if let Some(param) = param_binding {
        Ok((
            rest,
            Stmt::Block(vec![
                Stmt::VarDecl {
                    name: param,
                    expr: Expr::Literal(crate::value::Value::NIL),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    where_constraint: None,
                },
                while_stmt,
            ]),
        ))
    } else {
        Ok((rest, while_stmt))
    }
}
