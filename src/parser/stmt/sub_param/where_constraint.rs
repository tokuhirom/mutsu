use crate::ast::{Expr, Stmt};
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;
use crate::value::Value;
use std::collections::HashMap;

pub(crate) fn parse_where_constraint_expr(input: &str) -> PResult<'_, Expr> {
    let (r, _) = ws(input)?;
    if r.starts_with('{') {
        let (r, body) = crate::parser::primary::parse_block_body(r)?;
        if stmts_lead_with_whatever(&body) {
            return Err(malformed_double_closure_error());
        }
        return Ok((
            r,
            Expr::AnonSub {
                body,
                is_rw: false,
                is_block: true,
            },
        ));
    }
    // Parse the constraint without consuming a trailing `=` default value: in a
    // signature `$x where <constraint> = <default>`, the `=` introduces the
    // parameter default, not an assignment to the constraint expression.
    crate::parser::expr::expression_no_assign(input)
}

pub(crate) fn malformed_double_closure_error() -> PError {
    let msg = "Malformed double closure; WhateverCode is already a closure without curlies, so either remove the curlies or use valid parameter syntax instead of *".to_string();
    let mut attrs = HashMap::new();
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    attrs.insert("what".to_string(), Value::str("closure".to_string()));
    let ex = Value::make_instance(Symbol::intern("X::Syntax::Malformed"), attrs);
    PError::fatal_with_exception(msg, Box::new(ex))
}

/// Reject placeholder (`$:x`, `$^x`) and twigil (`$?x`, `$=x`, `$~x`) variables
/// when they appear as a signature parameter. `input` is the remaining param
/// text, positioned at the sigil. Returns `Ok(())` when the param is not one of
/// these forms (the caller proceeds with normal parsing).
pub(crate) fn reject_placeholder_or_twigil_param(input: &str) -> Result<(), PError> {
    let bytes = input.as_bytes();
    if bytes.len() < 3 || !matches!(bytes[0], b'$' | b'@' | b'%' | b'&') {
        return Ok(());
    }
    let sigil = bytes[0] as char;
    let twigil = bytes[1] as char;
    if !matches!(twigil, ':' | '^' | '?' | '=' | '~') {
        return Ok(());
    }
    // `$::x` is a package-qualified variable, not a `:` placeholder.
    if twigil == ':' && bytes[2] == b':' {
        return Ok(());
    }
    // Must be followed by an identifier character to be a placeholder/twigil var.
    let after = &input[2..];
    let next = after.chars().next();
    if !next.is_some_and(|c| c.is_alphanumeric() || c == '_') {
        return Ok(());
    }
    let end = after
        .find(|c: char| !c.is_alphanumeric() && c != '_')
        .unwrap_or(after.len());
    let name = &after[..end];
    let parameter = format!("{sigil}{twigil}{name}");

    let mut attrs = HashMap::new();
    attrs.insert("parameter".to_string(), Value::str(parameter.clone()));

    let (class, msg) = match twigil {
        ':' => {
            let right = format!(":{sigil}{name}");
            attrs.insert("right".to_string(), Value::str(right.clone()));
            (
                "X::Parameter::Placeholder",
                format!(
                    "Named placeholder variables like '{parameter}' are not allowed in signatures. \nDid you mean: '{right}' ?"
                ),
            )
        }
        '^' => {
            let right = format!("{sigil}{name}");
            attrs.insert("right".to_string(), Value::str(right.clone()));
            (
                "X::Parameter::Placeholder",
                format!(
                    "Positional placeholder variables like '{parameter}' are not allowed in signatures.  Did you mean: '{right}' ?"
                ),
            )
        }
        _ => {
            attrs.insert("twigil".to_string(), Value::str(twigil.to_string()));
            (
                "X::Parameter::Twigil",
                format!(
                    "Parameters with a '{twigil}' twigil, like '{parameter}', are not allowed in signatures."
                ),
            )
        }
    };
    attrs.insert("message".to_string(), Value::str(msg.clone()));
    let ex = Value::make_instance(Symbol::intern(class), attrs);
    Err(PError::fatal_with_exception(msg, Box::new(ex)))
}

/// A `{ ... }` where-constraint is a "malformed double closure" only when its
/// body would itself curry into a WhateverCode — i.e. when the LEADING term
/// (leftmost leaf on the left spine) of a statement is a bare `*`. A `*` nested
/// as a call/method ARGUMENT (`{ .grep: * > 2 }`, `{ $_.map(* + 1) }`) forms its
/// own inner WhateverCode confined to that argument and does NOT make the block a
/// WhateverCode, so it must not trip the guard. Matches raku, which SORRYs on
/// `{ * > 2 }` (leads with `*`) but accepts `{ $_ > * }` / `{ .grep: * > 2 }`.
pub(crate) fn stmts_lead_with_whatever(stmts: &[Stmt]) -> bool {
    stmts.iter().any(stmt_leads_with_whatever)
}

fn stmt_leads_with_whatever(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Expr(expr)
        | Stmt::Return(expr)
        | Stmt::Take(expr, _)
        | Stmt::Die(expr)
        | Stmt::Fail(expr) => expr_leads_with_whatever(expr),
        Stmt::Label { stmt, .. } => stmt_leads_with_whatever(stmt),
        _ => false,
    }
}

/// Walk the LEFT spine (left operand / receiver / base) to the leftmost leaf and
/// report whether the block *leads* with a `*`. Does NOT descend into argument
/// lists or right operands — a `*` there is self-contained and forms its own
/// inner WhateverCode (`{ .grep: * > 2 }`, `{ $_ > * }`).
///
/// Currying rewrites the leading `*` in two shapes, both handled here:
///   `{ * > 2 }`        -> topic-param `Lambda { is_whatever_code }` (the `*` is
///                        the standalone leading operand; always a double closure)
///   `{ * > 2 && * < 9 }`-> `AnonSubParams { is_whatever_code, params:[__wc_N] }`
///                        whose body's leftmost leaf is a `__wc_N` placeholder
/// vs the accepted `{ $_ > * }`, also `AnonSubParams`, but whose leftmost leaf is
/// the genuine `$_` (`Var("_")`), not a `__wc_N` placeholder.
fn expr_leads_with_whatever(expr: &Expr) -> bool {
    match expr {
        Expr::Whatever | Expr::HyperWhatever => true,
        // A curried whatever placeholder reached as the leftmost leaf.
        Expr::Var(name) if name.starts_with("__wc_") => true,
        // Topic-param WhateverCode: only produced by a standalone leading `*`.
        Expr::Lambda {
            is_whatever_code: true,
            ..
        } => true,
        // `__wc_`-param WhateverCode: descend to its body's leftmost leaf, which
        // is a `__wc_N` placeholder iff the block actually led with `*`.
        Expr::AnonSubParams {
            is_whatever_code: true,
            body,
            ..
        } => body.first().is_some_and(stmt_leads_with_whatever),
        Expr::Binary { left, .. }
        | Expr::HyperOp { left, .. }
        | Expr::MetaOp { left, .. }
        | Expr::InfixFunc { left, .. } => expr_leads_with_whatever(left),
        Expr::MethodCall { target, .. }
        | Expr::HyperMethodCall { target, .. }
        | Expr::DynamicMethodCall { target, .. }
        | Expr::HyperMethodCallDynamic { target, .. }
        | Expr::CallOn { target, .. }
        | Expr::Index { target, .. }
        | Expr::MultiDimIndex { target, .. }
        | Expr::HyperSlice { target, .. } => expr_leads_with_whatever(target),
        Expr::Unary { expr: inner, .. }
        | Expr::PostfixOp { expr: inner, .. }
        | Expr::Itemize(inner)
        | Expr::Eager(inner)
        | Expr::Reduction { expr: inner, .. } => expr_leads_with_whatever(inner),
        Expr::Ternary { cond, .. } => expr_leads_with_whatever(cond),
        _ => false,
    }
}
