use super::*;

/// Parse a while clause, retaining its written signature.
pub(crate) fn while_stmt(input: &str) -> PResult<'_, Stmt> {
    parse_loop(input, false)
}

/// Parse an until clause, retaining its written signature.
pub(crate) fn until_stmt(input: &str) -> PResult<'_, Stmt> {
    parse_loop(input, true)
}

fn parse_loop(input: &str, is_until: bool) -> PResult<'_, Stmt> {
    let rest = keyword(if is_until { "until" } else { "while" }, input)
        .ok_or_else(|| PError::expected("while/until statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, defs) = if rest.starts_with("->") || rest.starts_with("<->") {
        let (rest, (_, def, params, _, _, _)) = parse_for_params(rest)?;
        if !params.is_empty() {
            return Err(PError::expected_at(
                "single while/until pointy parameter",
                rest,
            ));
        }
        (rest, Some(def.into_iter().collect::<Vec<_>>()))
    } else {
        (rest, None)
    };
    let (rest, _) = ws(rest)?;
    let (rest, body) = block_with_pointy_params(rest, defs.as_deref().unwrap_or(&[]))?;
    if defs.is_some()
        && let Some(err) =
            crate::parser::stmt::sub::placeholder_overrides_signature_error(&body, &[])
    {
        return Err(err);
    }
    Ok((rest, loop_pointy_clause(cond, defs, body, is_until, None)))
}

/// The one expansion of a written while/until clause, shared with RakuAST.
/// The source record precedes the executable tree and is skipped by codegen.
// Cost: O(n), n = size of the condition, parameter and body.
pub(crate) fn loop_pointy_clause(
    cond: Expr,
    defs: Option<Vec<ParamDef>>,
    body: Vec<Stmt>,
    is_until: bool,
    label: Option<String>,
) -> Stmt {
    let record = defs.as_ref().map(|params| {
        Stmt::SourceForm(Box::new(crate::ast::SourceForm::ControlPointy {
            kind: if is_until {
                crate::ast::ControlPointyKind::Until
            } else {
                crate::ast::ControlPointyKind::While
            },
            condition: cond.clone(),
            label: label.clone(),
            param_defs: params.clone(),
            body: body.clone(),
        }))
    });
    let expanded = expand_loop(
        cond,
        defs.and_then(|mut defs| defs.pop()),
        body,
        is_until,
        label,
    );
    match record {
        Some(record) => Stmt::SyntheticBlock(vec![record, expanded]),
        None => expanded,
    }
}

fn expand_loop(
    cond: Expr,
    param_def: Option<ParamDef>,
    mut body: Vec<Stmt>,
    is_until: bool,
    label: Option<String>,
) -> Stmt {
    let param_binding = param_def.as_ref().map(|def| def.name.clone());
    if let Some(def) = &param_def
        && let Some(sub_params) = &def.sub_signature
    {
        let mut binds = Vec::new();
        crate::param_destructure::destructure_binds(&def.name, sub_params, &mut binds);
        binds.append(&mut body);
        body = binds;
    }
    // Test the condition value itself before assigning an aggregate: a
    // Failure or an empty list must not become a truthy one-element array.
    let aggregate_tmp = param_binding
        .as_deref()
        .filter(|p| p.starts_with('@') || p.starts_with('%'))
        .map(|_| "mutsu-while-cond".to_string());
    if let (Some(param), Some(tmp)) = (&param_binding, &aggregate_tmp) {
        body.insert(
            0,
            Stmt::Assign {
                name: param.clone(),
                expr: Expr::Var(tmp.clone()),
                op: AssignOp::Assign,
                target_is_sigilless: false,
            },
        );
    }
    let sigilless_tmp = sigilless_loop_tmp(
        &param_binding,
        param_def.as_ref().is_some_and(|def| def.sigilless),
        &mut body,
    );
    let (hoisted_decl, cond) = if param_binding.is_none() {
        split_loop_cond_decl(cond)
    } else {
        (None, cond)
    };
    let cond = if let Some(param) = &param_binding {
        Expr::AssignExpr {
            name: aggregate_tmp
                .clone()
                .or_else(|| sigilless_tmp.clone())
                .unwrap_or_else(|| param.clone()),
            expr: Box::new(cond),
            is_bind: aggregate_tmp.is_some(),
        }
    } else {
        cond
    };
    let stmt = Stmt::While {
        cond: if is_until {
            Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(cond),
                word: false,
            }
        } else {
            cond
        },
        body,
        label,
        is_statement_modifier: false,
        is_until,
        is_bare_term: false,
    };
    if let Some(decl) = hoisted_decl {
        return Stmt::Block(vec![decl, stmt]);
    }
    let Some(param) = param_binding else {
        return stmt;
    };
    let declared = if let Some(tmp) = sigilless_tmp {
        vec![tmp]
    } else {
        std::iter::once(param).chain(aggregate_tmp).collect()
    };
    Stmt::Block(
        declared
            .into_iter()
            .map(|name| Stmt::VarDecl {
                name,
                expr: Expr::Literal(Value::NIL),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            })
            .chain(std::iter::once(stmt))
            .collect(),
    )
}

/// The name of the scalar temporary a `while`/`until COND -> \r` loop assigns
/// its condition to, when the parameter is sigilless; the body then starts
/// with the per-iteration binding `my \r = $tmp`. `None` for any other
/// parameter, which the loop assigns directly.
// Cost: O(b), b = statements of the loop body (one insertion at its head).
fn sigilless_loop_tmp(
    param_binding: &Option<String>,
    sigilless: bool,
    body: &mut Vec<Stmt>,
) -> Option<String> {
    let param = param_binding.as_ref().filter(|_| sigilless)?;
    let tmp = "mutsu-while-cond".to_string();
    body.insert(0, simple_pointy_bind(param, &Expr::Var(tmp.clone()), true));
    Some(tmp)
}
