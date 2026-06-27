use super::*;

/// Parse `while` loop.
pub(crate) fn while_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("while", input).ok_or_else(|| PError::expected("while statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, param_binding) = if rest.starts_with("->") {
        let (rest, (param, _param_def, params, _params_def, _rw_block, _explicit_zero)) =
            parse_for_params(rest)?;
        if !params.is_empty() {
            return Err(PError::expected_at("single while pointy parameter", rest));
        }
        (rest, param)
    } else {
        (rest, None::<String>)
    };
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
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
                    expr: Expr::Literal(crate::value::Value::Nil),
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
    let (rest, param_binding) = if rest.starts_with("->") {
        let (rest, (param, _param_def, params, _params_def, _rw_block, _explicit_zero)) =
            parse_for_params(rest)?;
        if !params.is_empty() {
            return Err(PError::expected_at("single until pointy parameter", rest));
        }
        (rest, param)
    } else {
        (rest, None::<String>)
    };
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
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
                    expr: Expr::Literal(crate::value::Value::Nil),
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
