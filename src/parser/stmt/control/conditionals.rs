use super::*;

static IF_BIND_TMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone)]
pub(crate) struct IfChainClause {
    cond: Expr,
    then_branch: Vec<Stmt>,
    binding_var: Option<String>,
}

pub(crate) struct ElseClause {
    binding_params: Option<Vec<ParamDef>>,
    body: Vec<Stmt>,
}

fn conditional_expr(input: &str) -> PResult<'_, Expr> {
    match parse_comma_or_expr(input) {
        Ok((rest, cond)) => {
            let (tail, _) = ws(rest)?;
            if condition_has_assignment_tail(tail)
                && let Ok((assign_rest, assign_cond)) =
                    super::super::assign::try_parse_assign_expr(input)
            {
                return Ok((assign_rest, assign_cond));
            }
            Ok((rest, cond))
        }
        Err(_) => condition_expr(input),
    }
}

pub(crate) fn if_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("if", input).ok_or_else(|| PError::expected("if statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = conditional_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, binding_params) = parse_if_binding_params(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, raw_then_branch) = block(rest)?;
    let (rest, _) = ws(rest)?;
    let (binding_var, then_branch) = lower_if_clause_binding(binding_params, raw_then_branch);

    let mut clauses = vec![IfChainClause {
        cond,
        then_branch,
        binding_var,
    }];
    let (rest, (mut elsif_clauses, else_clause)) = parse_elsif_chain(rest)?;
    clauses.append(&mut elsif_clauses);

    let stmt = lower_if_chain(clauses, else_clause);
    Ok((rest, stmt))
}

fn parse_if_binding_params(input: &str) -> PResult<'_, Option<Vec<ParamDef>>> {
    let Some(rest) = input.strip_prefix("->") else {
        return Ok((input, None));
    };
    let (rest, _) = ws(rest)?;
    // Zero-parameter pointy block: `if EXPR -> { ... }`
    if rest.starts_with('{') {
        return Ok((rest, Some(Vec::new())));
    }

    let (rest, params) = if let Some(rest) = rest.strip_prefix('(') {
        let (rest, _) = ws(rest)?;
        let (rest, params) = super::super::parse_param_list_pub(rest)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        (rest, params)
    } else {
        super::super::parse_param_list_pub(rest)?
    };
    let (rest, _) = ws(rest)?;
    let (rest, _) = if let Some(after_arrow) = rest.strip_prefix("-->") {
        let (rest, _) = super::super::parse_return_type_annotation_pub(after_arrow)?;
        let (rest, _) = ws(rest)?;
        (rest, ())
    } else {
        (rest, ())
    };
    Ok((rest, Some(params)))
}

fn is_simple_if_binding(param: &ParamDef) -> bool {
    param.traits.is_empty()
        && param.shape_constraints.is_none()
        && !param.named
        && !param.slurpy
        && !param.double_slurpy
        && param.default.is_none()
        && !param.optional_marker
        && param.type_constraint.is_none()
        && param.sub_signature.is_none()
        && param.outer_sub_signature.is_none()
        && param.code_signature.is_none()
}

fn next_if_bind_tmp_name() -> String {
    let tmp_idx = IF_BIND_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("$__mutsu_if_bind_{tmp_idx}")
}

fn lower_if_clause_binding(
    binding_params: Option<Vec<ParamDef>>,
    then_branch: Vec<Stmt>,
) -> (Option<String>, Vec<Stmt>) {
    let Some(param_defs) = binding_params else {
        return (None, then_branch);
    };
    if param_defs.is_empty() {
        return (None, then_branch);
    }
    if param_defs.len() == 1 && is_simple_if_binding(&param_defs[0]) {
        return (Some(param_defs[0].name.clone()), then_branch);
    }

    let source_binding = next_if_bind_tmp_name();
    let source_expr = Expr::Var(source_binding.trim_start_matches('$').to_string());
    // For **@ (double slurpy) as the only positional param, pass the condition
    // as a single argument without slipping, so the list is captured as-is.
    let has_only_double_slurpy = param_defs
        .iter()
        .filter(|p| !p.named)
        .all(|p| p.double_slurpy);
    let args = if has_only_double_slurpy {
        vec![source_expr]
    } else {
        vec![Expr::Unary {
            op: TokenKind::Pipe,
            expr: Box::new(source_expr),
        }]
    };
    let call_expr = Expr::CallOn {
        target: Box::new(Expr::AnonSubParams {
            params: param_defs.iter().map(|p| p.name.clone()).collect(),
            param_defs,
            return_type: None,
            body: then_branch,
            is_rw: false,
            is_whatever_code: false,
        }),
        args,
    };
    (Some(source_binding), vec![Stmt::Expr(call_expr)])
}

fn ensure_last_clause_binding_var(clauses: &mut [IfChainClause]) -> Option<String> {
    let last_clause = clauses.last_mut()?;
    Some(if let Some(existing) = &last_clause.binding_var {
        existing.clone()
    } else {
        let generated = next_if_bind_tmp_name();
        last_clause.binding_var = Some(generated.clone());
        generated
    })
}

fn lower_else_binding(source_binding: &str, else_clause: ElseClause) -> Vec<Stmt> {
    let Some(param_defs) = else_clause.binding_params else {
        return else_clause.body;
    };
    if param_defs.is_empty() {
        return else_clause.body;
    }
    if param_defs.len() == 1 && is_simple_if_binding(&param_defs[0]) {
        let mut body = Vec::with_capacity(else_clause.body.len() + 1);
        body.push(Stmt::VarDecl {
            name: param_defs[0].name.clone(),
            expr: Expr::Var(source_binding.trim_start_matches('$').to_string()),
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            where_constraint: None,
        });
        body.extend(else_clause.body);
        return body;
    }

    let call_expr = Expr::CallOn {
        target: Box::new(Expr::AnonSubParams {
            params: param_defs.iter().map(|p| p.name.clone()).collect(),
            param_defs,
            return_type: None,
            body: else_clause.body,
            is_rw: false,
            is_whatever_code: false,
        }),
        args: vec![Expr::Unary {
            op: TokenKind::Pipe,
            expr: Box::new(Expr::Var(
                source_binding.trim_start_matches('$').to_string(),
            )),
        }],
    };
    vec![Stmt::Expr(call_expr)]
}

/// `else if` is a C-ism; Raku spells it `elsif`. Raise the dedicated
/// `X::Syntax::Malformed::Elsif` with Raku's exact diagnostic message.
fn malformed_elsif_error() -> PError {
    let msg = "In Raku, please use \"elsif' instead of \"else if\"".to_string();
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("message".to_string(), crate::value::Value::str(msg.clone()));
    let ex = crate::value::Value::make_instance(
        crate::symbol::Symbol::intern("X::Syntax::Malformed::Elsif"),
        attrs,
    );
    PError::fatal_with_exception(msg, Box::new(ex))
}

pub(crate) fn parse_elsif_chain(
    input: &str,
) -> PResult<'_, (Vec<IfChainClause>, Option<ElseClause>)> {
    let mut rest = input;
    let mut clauses = Vec::new();
    let mut last_orwith_cond: Option<Expr> = None;

    loop {
        if let Some(r) = keyword("elsif", rest) {
            let (r, _) = ws1(r)?;
            let (r, cond) = conditional_expr(r)?;
            let (r, _) = ws(r)?;
            let (r, binding_params) = parse_if_binding_params(r)?;
            let (r, _) = ws(r)?;
            let (r, raw_then_branch) = block(r)?;
            let (r, _) = ws(r)?;
            let (binding_var, then_branch) =
                lower_if_clause_binding(binding_params, raw_then_branch);
            clauses.push(IfChainClause {
                cond,
                then_branch,
                binding_var,
            });
            last_orwith_cond = None;
            rest = r;
            continue;
        }
        if let Some(r) = keyword("orwith", rest) {
            let (r, _) = ws1(r)?;
            let (r, orwith_cond_expr) = condition_expr(r)?;
            let (r, _) = ws(r)?;
            // Check for optional pointy block: orwith EXPR -> $param { ... }
            let (r, orwith_param_name) = if let Some(r2) = r.strip_prefix("->") {
                let (r2, _) = ws(r2)?;
                if let Some(r_after_sigil) = r2.strip_prefix('$') {
                    let end = r_after_sigil
                        .find(|c: char| !c.is_alphanumeric() && c != '_' && c != '-')
                        .unwrap_or(r_after_sigil.len());
                    let name = &r_after_sigil[..end];
                    let r2 = &r_after_sigil[end..];
                    let (r2, _) = ws(r2)?;
                    (r2, Some(name.to_string()))
                } else {
                    (r, None)
                }
            } else {
                (r, None)
            };
            let (r, orwith_body) = block(r)?;
            // Topicalize $_ and optional param in the orwith body
            let mut orwith_then = vec![topicalize(&orwith_cond_expr)];
            if let Some(ref pname) = orwith_param_name {
                orwith_then.push(Stmt::VarDecl {
                    name: pname.clone(),
                    expr: orwith_cond_expr.clone(),
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    where_constraint: None,
                });
            }
            orwith_then.extend(orwith_body);
            // orwith uses .defined as the condition
            last_orwith_cond = Some(orwith_cond_expr.clone());
            let orwith_cond = Expr::MethodCall {
                target: Box::new(orwith_cond_expr),
                name: Symbol::intern("defined"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            };
            let (r, _) = ws(r)?;
            clauses.push(IfChainClause {
                cond: orwith_cond,
                then_branch: orwith_then,
                binding_var: None,
            });
            rest = r;
            continue;
        }
        break;
    }

    if let Some(r) = keyword("else", rest) {
        let (r, _) = ws(r)?;
        // `else if ...` is the C-style spelling of `elsif`; Raku rejects it with a
        // dedicated, helpful diagnostic (X::Syntax::Malformed::Elsif) rather than a
        // generic "expected '{'" parse error.
        if keyword("if", r).is_some() {
            return Err(malformed_elsif_error());
        }
        let (r, binding_params) = parse_if_binding_params(r)?;
        let (r, _) = ws(r)?;
        let (r, mut body) = block(r)?;
        // If the last clause was `orwith`, topicalize $_ in the else body
        if let Some(ref orwith_expr) = last_orwith_cond {
            let mut topicalized = vec![topicalize(orwith_expr)];
            topicalized.append(&mut body);
            body = topicalized;
        }
        return Ok((
            r,
            (
                clauses,
                Some(ElseClause {
                    binding_params,
                    body,
                }),
            ),
        ));
    }

    Ok((rest, (clauses, None)))
}

pub(crate) fn lower_if_chain(
    mut clauses: Vec<IfChainClause>,
    else_clause: Option<ElseClause>,
) -> Stmt {
    let mut else_branch = if let Some(else_clause) = else_clause {
        let mut body = lower_else_clause(&mut clauses, else_clause);
        // An explicit `else {}` with an empty body should evaluate to Nil,
        // not be indistinguishable from a missing else clause.
        if body.is_empty() {
            body.push(Stmt::Expr(Expr::Literal(crate::value::Value::Nil)));
        }
        body
    } else {
        Vec::new()
    };

    while let Some(clause) = clauses.pop() {
        else_branch = vec![Stmt::If {
            cond: clause.cond,
            then_branch: clause.then_branch,
            else_branch,
            binding_var: clause.binding_var,
        }];
    }

    else_branch
        .pop()
        .expect("if chain must have at least one clause")
}

fn lower_else_clause(clauses: &mut [IfChainClause], else_clause: ElseClause) -> Vec<Stmt> {
    if else_clause.binding_params.is_none() {
        return else_clause.body;
    }
    let Some(source_binding) = ensure_last_clause_binding_var(clauses) else {
        return else_clause.body;
    };
    lower_else_binding(&source_binding, else_clause)
}

/// Parse `unless` statement.
pub(crate) fn unless_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("unless", input).ok_or_else(|| PError::expected("unless statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, cond) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block(rest)?;
    // unless cannot have else/elsif/orwith — check but consume the trailing clause
    let (check, _) = ws(rest)?;
    for kw in &["else", "elsif", "orwith"] {
        if let Some(r) = keyword(kw, check) {
            // Consume the rest of the invalid clause to produce a hard error
            let (r, _) = ws(r)?;
            // Skip condition (if any) and block
            let r = if kw != &"else" {
                if let Ok((r, _)) = expression(r) { r } else { r }
            } else {
                r
            };
            let (r, _) = ws(r)?;
            let r = if let Ok((r, _)) = block(r) { r } else { r };
            return Ok((
                r,
                Stmt::Die(Expr::Literal(crate::value::Value::str(format!(
                    "X::Syntax::UnlessElse: unless does not allow '{kw}'"
                )))),
            ));
        }
    }
    Ok((
        rest,
        Stmt::If {
            cond: Expr::Unary {
                op: TokenKind::Bang,
                expr: Box::new(cond),
            },
            then_branch: body,
            else_branch: Vec::new(),
            binding_var: None,
        },
    ))
}
