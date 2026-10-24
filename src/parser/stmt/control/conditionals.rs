use super::*;

use super::conditional_binding::{
    ensure_last_clause_binding_var, lower_else_binding, parse_if_binding_params,
};
use crate::ast::parsed::{
    BlockParams, ClauseKind, Conditional, ConditionalClause, ConditionalKind, ParsedBlock,
};

#[derive(Clone)]
pub(crate) struct IfChainClause {
    pub(crate) cond: Expr,
    pub(crate) then_branch: Vec<Stmt>,
    pub(crate) binding_var: Option<String>,
    /// `Some(WithBlockKind::Orwith)` for an `orwith` clause, which desugars
    /// into the same `.defined` conditional an `elsif` would but which raku
    /// models as its own `Statement::Orwith`. See `Stmt::If`'s `with_kind`.
    pub(crate) with_kind: Option<WithBlockKind>,
}

pub(crate) struct ElseClause {
    pub(crate) binding_params: Option<Vec<ParamDef>>,
    pub(crate) body: Vec<Stmt>,
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
    let (rest, condition) = conditional_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, then) = if_clause_block(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, (clauses, else_block)) = parse_elsif_chain(rest)?;
    let node = Conditional {
        kind: ConditionalKind::If,
        condition,
        then,
        clauses,
        else_block,
    };
    let stmt = super::parsed_block::finish(crate::ast::parsed::ParsedStmt::Conditional(node))
        .map_err(PError::from_typed)?;
    Ok((rest, stmt))
}

/// The block of an `if` / `elsif` / `else` / `unless` clause, with the
/// pointy parameters that bind the tested value (`-> $v { }`).
// Cost: O(n), n = length of the block's source.
fn if_clause_block(input: &str) -> PResult<'_, ParsedBlock> {
    let start = input;
    let (rest, binding_params) = parse_if_binding_params(input)?;
    let (rest, _) = ws(rest)?;
    let params = match binding_params {
        Some(defs) => BlockParams::List(defs),
        None => BlockParams::None,
    };
    super::parsed_block::parsed_block(start, rest, params)
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

/// The `elsif` / `orwith` clauses and the `else` block after a conditional's
/// first block.
pub(crate) fn parse_elsif_chain(
    input: &str,
) -> PResult<'_, (Vec<ConditionalClause>, Option<ParsedBlock>)> {
    let mut rest = input;
    let mut clauses = Vec::new();
    let mut after_orwith = false;

    loop {
        if let Some(r) = keyword("elsif", rest) {
            // Rakudo accepts the condition immediately after the keyword, as
            // in `elsif($condition)`.  The opening parenthesis is still the
            // condition expression here, not a call to `elsif`.
            let (r, _) = ws(r)?;
            let (r, condition) = conditional_expr(r)?;
            let (r, _) = ws(r)?;
            let (r, then) = if_clause_block(r)?;
            let (r, _) = ws(r)?;
            clauses.push(ConditionalClause {
                kind: ClauseKind::Elsif,
                condition,
                then,
            });
            after_orwith = false;
            rest = r;
            continue;
        }
        if let Some(r) = keyword("orwith", rest) {
            let (r, _) = ws1(r)?;
            let (r, condition) = condition_expr(r)?;
            let (r, _) = ws(r)?;
            let (r, then) = topic_clause_block(r, Some("single orwith pointy parameter"))?;
            let (r, _) = ws(r)?;
            clauses.push(ConditionalClause {
                kind: ClauseKind::Orwith,
                condition,
                then,
            });
            after_orwith = true;
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
        let (r, block) = if_clause_block(r)?;
        // The `else` of an `orwith` topicalizes the last tested value, so it
        // takes at most one parameter.
        if after_orwith && matches!(&block.params, BlockParams::List(defs) if defs.len() > 1) {
            return Err(PError::expected_at("single else pointy parameter", r));
        }
        return Ok((r, (clauses, Some(block))));
    }

    Ok((rest, (clauses, None)))
}

/// The block of a `with` / `orwith` / `without` clause: a plain block, or a
/// pointy block with the one parameter the tested value binds to. `several`
/// is the error for a list of parameters (a `with` ignores the rest).
// Cost: O(n), n = length of the block's source.
pub(crate) fn topic_clause_block<'a>(
    input: &'a str,
    several: Option<&'static str>,
) -> PResult<'a, ParsedBlock> {
    let start = input;
    let (rest, params) = if input.starts_with("->") || input.starts_with("<->") {
        let (r, (name, def, params, _, _, _)) = parse_for_params(input)?;
        if let Some(several) = several
            && !params.is_empty()
        {
            return Err(PError::expected_at(several, r));
        }
        (
            r,
            BlockParams::Topic {
                name,
                def: def.map(Box::new),
            },
        )
    } else {
        (input, BlockParams::None)
    };
    super::parsed_block::parsed_block(start, rest, params)
}

pub(crate) fn lower_if_chain(
    mut clauses: Vec<IfChainClause>,
    else_clause: Option<ElseClause>,
) -> Result<Stmt, crate::value::RuntimeError> {
    let mut else_branch = if let Some(else_clause) = else_clause {
        let mut body = lower_else_clause(&mut clauses, else_clause)?;
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
        .ok_or_else(|| crate::value::RuntimeError::new("if chain must have at least one clause"))
}

fn lower_else_clause(
    clauses: &mut [IfChainClause],
    else_clause: ElseClause,
) -> Result<Vec<Stmt>, crate::value::RuntimeError> {
    if else_clause.binding_params.is_none() {
        return Ok(else_clause.body);
    }
    let Some(source_binding) = ensure_last_clause_binding_var(clauses) else {
        return Ok(else_clause.body);
    };
    lower_else_binding(&source_binding, else_clause)
}

/// Parse `unless` statement.
pub(crate) fn unless_stmt(input: &str) -> PResult<'_, Stmt> {
    let rest = keyword("unless", input).ok_or_else(|| PError::expected("unless statement"))?;
    let (rest, _) = ws1(rest)?;
    let (rest, condition) = condition_expr(rest)?;
    let (rest, _) = ws(rest)?;
    let (rest, then) = if_clause_block(rest)?;
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
    let node = Conditional {
        kind: ConditionalKind::Unless,
        condition,
        then,
        clauses: Vec::new(),
        else_block: None,
    };
    let stmt = super::parsed_block::finish(crate::ast::parsed::ParsedStmt::Conditional(node))
        .map_err(|error| PError::fatal(error.message.into_owned()))?;
    Ok((rest, stmt))
}

/// Expand the written unless clause through the same conditional binder.
// Cost: O(n), n = size of the signature and body.
pub(crate) fn unless_clause(
    cond: Expr,
    binding_params: Option<Vec<ParamDef>>,
    body: Vec<Stmt>,
) -> Result<Stmt, crate::value::RuntimeError> {
    if let Some(params) = &binding_params {
        super::source_body::check(&cond, params, &body)?;
    }
    expand_unless_clause(cond, binding_params, body)
}

fn expand_unless_clause(
    cond: Expr,
    binding_params: Option<Vec<ParamDef>>,
    body: Vec<Stmt>,
) -> Result<Stmt, crate::value::RuntimeError> {
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
    Ok(Stmt::If {
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
    })
}
