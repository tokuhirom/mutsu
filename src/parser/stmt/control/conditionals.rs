use super::*;

use super::conditional_binding::{
    ensure_last_clause_binding_var, lower_else_binding, lower_if_clause_binding,
    parse_if_binding_params,
};

#[derive(Clone)]
pub(crate) struct IfChainClause {
    pub(super) cond: Expr,
    pub(super) then_branch: Vec<Stmt>,
    pub(super) binding_var: Option<String>,
    /// `Some(WithBlockKind::Orwith)` for an `orwith` clause, which desugars
    /// into the same `.defined` conditional an `elsif` would but which raku
    /// models as its own `Statement::Orwith`. See `Stmt::If`'s `with_kind`.
    pub(super) with_kind: Option<WithBlockKind>,
}

pub(crate) struct ElseClause {
    pub(super) binding_params: Option<Vec<ParamDef>>,
    pub(super) body: Vec<Stmt>,
}

pub(super) fn conditional_expr(input: &str) -> PResult<'_, Expr> {
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
    let (rest, raw_then_branch) =
        block_with_pointy_params(rest, binding_params.as_deref().unwrap_or(&[]))?;
    let (rest, _) = ws(rest)?;
    let (binding_var, then_branch) = lower_if_clause_binding(binding_params, raw_then_branch);

    let mut clauses = vec![IfChainClause {
        cond,
        then_branch,
        binding_var,
        with_kind: None,
    }];
    let (rest, (mut elsif_clauses, else_clause)) = parse_elsif_chain(rest)?;
    clauses.append(&mut elsif_clauses);

    let stmt = lower_if_chain(clauses, else_clause);
    Ok((rest, stmt))
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
            // Rakudo accepts the condition immediately after the keyword, as
            // in `elsif($condition)`.  The opening parenthesis is still the
            // condition expression here, not a call to `elsif`.
            let (r, _) = ws(r)?;
            let (r, cond) = conditional_expr(r)?;
            let (r, _) = ws(r)?;
            let (r, binding_params) = parse_if_binding_params(r)?;
            let (r, _) = ws(r)?;
            let (r, raw_then_branch) =
                block_with_pointy_params(r, binding_params.as_deref().unwrap_or(&[]))?;
            let (r, _) = ws(r)?;
            let (binding_var, then_branch) =
                lower_if_clause_binding(binding_params, raw_then_branch);
            clauses.push(IfChainClause {
                cond,
                then_branch,
                binding_var,
                with_kind: None,
            });
            last_orwith_cond = None;
            rest = r;
            continue;
        }
        if let Some(r) = keyword("orwith", rest) {
            let (r, _) = ws1(r)?;
            let (r, orwith_cond_expr) = condition_expr(r)?;
            let (r, _) = ws(r)?;
            let (r, param, param_def) = if r.starts_with("->") || r.starts_with("<->") {
                let (r, (param, def, params, _, _, _)) = parse_for_params(r)?;
                if !params.is_empty() {
                    return Err(PError::expected_at("single orwith pointy parameter", r));
                }
                (r, param, def)
            } else {
                (r, None, None)
            };
            let (r, body) = block_with_pointy_params(r, param_def.as_slice())?;
            let tmp = crate::with_desugar::next_tmp_name();
            let tmp_var = Expr::Var(tmp.clone());
            let orwith_then =
                with_then_branch(&orwith_cond_expr, &tmp_var, &param, &param_def, body);
            last_orwith_cond = Some(tmp_var);
            let orwith_cond = crate::with_desugar::defined_condition(false, &tmp, orwith_cond_expr);
            let (r, _) = ws(r)?;
            clauses.push(IfChainClause {
                cond: orwith_cond,
                then_branch: orwith_then,
                binding_var: None,
                with_kind: Some(WithBlockKind::Orwith),
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
        let (r, mut binding_params) = parse_if_binding_params(r)?;
        let (r, _) = ws(r)?;
        let (r, mut body) = block_with_pointy_params(r, binding_params.as_deref().unwrap_or(&[]))?;
        // If the last clause was `orwith`, topicalize $_ in the else body via
        // `given` (a fresh topic scope) so it is not blocked by an enclosing `for`'s
        // read-only `$_` (see the orwith branch above).
        if let Some(ref orwith_expr) = last_orwith_cond {
            let defs = binding_params.take().unwrap_or_default();
            if defs.len() > 1 {
                return Err(PError::expected_at("single else pointy parameter", r));
            }
            let def = defs.into_iter().next();
            let param = def.as_ref().map(|def| def.name.clone());
            body = with_then_branch(orwith_expr, orwith_expr, &param, &def, body);
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
            body.push(Stmt::Expr(Expr::Literal(crate::value::Value::NIL)));
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
            is_statement_modifier: false,
            is_unless: false,
            with_kind: clause.with_kind,
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
    let (rest, binding_params) = parse_if_binding_params(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, body) = block_with_pointy_params(rest, binding_params.as_deref().unwrap_or(&[]))?;
    // `unless` cannot have else/elsif/orwith. rakudo rejects this at COMPILE
    // time with `X::Syntax::UnlessElse`, carrying the offending `keyword`
    // (roast S04-statements/unless.t matches on it). mutsu used to lower it to
    // a runtime `Stmt::Die` whose message merely *spelled* the class, which
    // arrived as a plain `X::AdHoc` — and let the rest of the file compile.
    // The `without` twin already goes through `RuntimeError::without_else`.
    let (check, _) = ws(rest)?;
    for kw in &["else", "elsif", "orwith"] {
        if keyword(kw, check).is_some() {
            return Err(PError::from_typed(crate::value::RuntimeError::unless_else(
                kw,
            )));
        }
    }
    Ok((rest, unless_clause(cond, binding_params, body)))
}

/// Expand the written unless clause through the same conditional binder.
// Cost: O(n), n = size of the signature and body.
pub(crate) fn unless_clause(
    cond: Expr,
    binding_params: Option<Vec<ParamDef>>,
    body: Vec<Stmt>,
) -> Stmt {
    let record = binding_params.as_ref().map(|params| {
        Stmt::SourceForm(Box::new(crate::ast::SourceForm::ControlPointy {
            kind: crate::ast::ControlPointyKind::Unless,
            label: None,
            condition: cond.clone(),
            param_defs: params.clone(),
            body: body.clone(),
        }))
    });
    let expanded = expand_unless_clause(cond, binding_params, body);
    match record {
        Some(record) => Stmt::SyntheticBlock(vec![record, expanded]),
        None => expanded,
    }
}

fn expand_unless_clause(
    cond: Expr,
    binding_params: Option<Vec<ParamDef>>,
    body: Vec<Stmt>,
) -> Stmt {
    // `unless COND -> $x { BODY }` binds the condition's OWN value (rakudo:
    // `unless 0 -> $_ { $_.say }` prints `0`, not the negation). Lower it as
    // the *else* branch of an un-negated `if`, which is exactly the machinery
    // `if COND { } else -> $x { }` already uses to hand the else clause the
    // condition value — rather than negating the condition and binding that.
    if let Some(params) = binding_params {
        let clauses = vec![IfChainClause {
            cond,
            then_branch: Vec::new(),
            binding_var: None,
            with_kind: None,
        }];
        let else_clause = ElseClause {
            binding_params: Some(params),
            body,
        };
        return lower_if_chain(clauses, Some(else_clause));
    }
    Stmt::If {
        cond: Expr::Unary {
            op: TokenKind::Bang,
            expr: Box::new(cond),
            word: false,
        },
        then_branch: body,
        else_branch: Vec::new(),
        binding_var: None,
        is_statement_modifier: false,
        is_unless: true,
        with_kind: None,
    }
}
