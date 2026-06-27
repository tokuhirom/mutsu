use crate::ast::{Expr, Stmt};
use crate::parser::helpers::is_non_breaking_space;
use crate::parser::parse_result::PError;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

/// Check if an expression is a negative integer literal (e.g., `-(1)` from `-1`).
/// Returns the negative value string (e.g., "-1") if so.
pub(crate) fn extract_negative_literal(expr: &Expr) -> Option<String> {
    if let Expr::Unary {
        op: TokenKind::Minus,
        expr: inner,
    } = expr
    {
        if let Expr::Literal(Value::Int(n)) = inner.as_ref() {
            return Some(format!("-{}", n));
        }
        if let Expr::Literal(Value::BigInt(n)) = inner.as_ref() {
            return Some(format!("-{}", n));
        }
    }
    None
}

/// Check if an expression is a range ending in a negative literal (e.g., `0..-1`).
/// Returns the negative end value string if so.
pub(crate) fn extract_range_negative_end(expr: &Expr) -> Option<String> {
    if let Expr::Binary {
        op: TokenKind::DotDot | TokenKind::DotDotCaret,
        right,
        ..
    } = expr
    {
        return extract_negative_literal(right);
    }
    None
}

/// Build a fatal X::Obsolete parse error for negative subscripts.
pub(crate) fn make_negative_subscript_error(neg_val: &str) -> PError {
    let old = format!("a negative {} subscript to index from the end", neg_val);
    let replacement = format!("a function such as *{}", neg_val);
    let message = format!(
        "X::Obsolete: Unsupported use of {}. In Raku please use: {}.",
        old, replacement
    );
    let err = crate::value::RuntimeError::obsolete(&old, &replacement);
    let mut attrs = std::collections::HashMap::new();
    attrs.insert("old".to_string(), Value::str(old));
    attrs.insert("replacement".to_string(), Value::str(replacement));
    attrs.insert("message".to_string(), Value::str(err.message.clone()));
    let exception = Value::make_instance(Symbol::intern("X::Obsolete"), attrs);
    PError::fatal_with_exception(message, Box::new(exception))
}

/// Check if a character is valid inside an angle-bracket word key (`<key>`).
/// Raku allows any non-whitespace character that isn't `>` in angle bracket words.
/// We allow: alphanumeric, common ASCII punctuation used in identifiers/keys,
/// non-breaking spaces, and any non-ASCII Unicode character that isn't whitespace.
pub(crate) fn is_angle_key_char(c: char) -> bool {
    c.is_alphanumeric()
        || c == '_'
        || c == '-'
        || c == '!'
        || c == '*'
        || c == '.'
        || c == ':'
        || c == '?'
        || c == '+'
        || c == '/'
        || c == '$'
        || c == '@'
        || c == '%'
        || c == '&'
        || is_non_breaking_space(c)
        || (!c.is_ascii() && !c.is_whitespace())
}

/// When a prefix operator is applied to a WhateverCode (Lambda or AnonSubParams),
/// compose the prefix into the body so that `+(* + 1)` becomes `-> $_ { +($_ + 1) }`
/// instead of trying to numify the closure itself.
pub(crate) fn compose_prefix_into_whatevercode(op: TokenKind, expr: Expr) -> Expr {
    match expr {
        Expr::Lambda {
            param,
            mut body,
            is_whatever_code,
        } => {
            wrap_last_stmt_with_unary(&mut body, op.clone());
            Expr::Lambda {
                param,
                body,
                is_whatever_code,
            }
        }
        Expr::AnonSubParams {
            params,
            param_defs,
            return_type,
            mut body,
            is_rw,
            is_whatever_code,
        } => {
            wrap_last_stmt_with_unary(&mut body, op.clone());
            Expr::AnonSubParams {
                params,
                param_defs,
                return_type,
                body,
                is_rw,
                is_whatever_code,
            }
        }
        other => Expr::Unary {
            op,
            expr: Box::new(other),
        },
    }
}

/// Wrap the last expression statement in a list of statements with a unary operator.
pub(crate) fn wrap_last_stmt_with_unary(stmts: &mut [Stmt], op: TokenKind) {
    if let Some(Stmt::Expr(expr)) = stmts.last_mut() {
        let inner = std::mem::replace(expr, Expr::Literal(Value::Nil));
        *expr = Expr::Unary {
            op,
            expr: Box::new(inner),
        };
    }
}
