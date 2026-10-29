use super::*;

use super::conditional_binding::parse_if_binding_params;
use crate::ast::parsed::{
    BlockParams, ClauseKind, Conditional, ConditionalClause, ConditionalKind, ParsedBlock,
};

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
