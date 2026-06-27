//! Substitution expression building helpers.

use std::sync::Arc;

use crate::ast::{Expr, Stmt};
use crate::parser::expr::expression;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

use super::adverbs::{
    MatchAdverbs, apply_inline_match_adverbs, build_regex_with_adverbs,
    validate_regex_pattern_or_perror,
};

pub(super) fn parse_subst_replacement_expr(input: &str) -> PResult<'_, String> {
    let (input, _) = ws(input)?;
    // Parse the FULL replacement expression (not just the first primary): the
    // literal shortcut is only valid when the entire replacement is a single
    // literal. Using `primary` here would stop after the first term, so a
    // compound replacement like `"<" ~ $/ ~ ">"` would leave `~ $/ ~ ">"` in the
    // stream to be (mis)parsed as an outer binary expression, silently dropping
    // it from the substitution. Requiring `expression` to yield a bare `Literal`
    // routes any compound replacement to the closure/block path instead.
    let (rest, expr) = expression(input)?;
    let replacement = match expr {
        Expr::Literal(value) => value.to_string_value(),
        _ => {
            return Err(PError::expected(
                "literal replacement expression after '=' in substitution",
            ));
        }
    };
    Ok((rest, replacement))
}

/// Try to strip a compound assignment operator (e.g. `+=`, `x=`, `~=`) from the input.
/// Returns the operator string (without `=`) and the remaining input after `=`.
pub(super) fn try_strip_subst_compound_assign(input: &str) -> Option<(&str, &str)> {
    // Multi-char operators first
    for op in &["**", "//", "||", "&&", "+|", "+&", "+^", "~|", "~&", "~^"] {
        if let Some(rest) = input.strip_prefix(op)
            && let Some(after_eq) = rest.strip_prefix('=')
        {
            return Some((op, after_eq));
        }
    }
    // Single-char operators
    for op in &["+", "-", "*", "/", "~", "%"] {
        if let Some(rest) = input.strip_prefix(op)
            && let Some(after_eq) = rest.strip_prefix('=')
            // Make sure it's not `==`
            && !after_eq.starts_with('=')
        {
            return Some((op, after_eq));
        }
    }
    // Word operators: x=, fromplus=, etc. — any identifier followed by =
    let mut i = 0;
    let bytes = input.as_bytes();
    if i < bytes.len() && (bytes[i].is_ascii_alphabetic() || bytes[i] == b'_') {
        i += 1;
        while i < bytes.len()
            && (bytes[i].is_ascii_alphanumeric() || bytes[i] == b'_' || bytes[i] == b'-')
        {
            i += 1;
        }
        if i < bytes.len() && bytes[i] == b'=' && (i + 1 >= bytes.len() || bytes[i + 1] != b'=') {
            return Some((&input[..i], &input[i + 1..]));
        }
    }
    None
}

/// Build an expression for `s[pattern] op= value` compound substitution.
/// This is equivalent to `$_.subst(pattern, { $/ op value })` applied to `$_`.
pub(super) fn build_topic_subst_compound_expr(
    pattern: String,
    op: &str,
    rhs: Expr,
    adverbs: &MatchAdverbs,
) -> Result<Expr, PError> {
    let match_var = Expr::Var("/".to_string());

    let op_token = match op {
        "+" => Some(TokenKind::Plus),
        "-" => Some(TokenKind::Minus),
        "*" => Some(TokenKind::Star),
        "/" => Some(TokenKind::Slash),
        "~" => Some(TokenKind::Tilde),
        "x" => Some(TokenKind::Ident("x".to_string())),
        "%" => Some(TokenKind::Percent),
        "**" => Some(TokenKind::StarStar),
        "//" => Some(TokenKind::SlashSlash),
        "||" => Some(TokenKind::OrOr),
        "&&" => Some(TokenKind::AndAnd),
        _ => None,
    };

    // Build: { $/ op rhs }
    // Note: $/ is stored as Var("/") in the AST (the parser strips the sigil)
    let body_expr = if let Some(op_token) = op_token {
        Expr::Binary {
            left: Box::new(match_var),
            op: op_token,
            right: Box::new(rhs),
        }
    } else {
        // User-defined infix operator: call infix:<op>($/, rhs)
        Expr::Call {
            name: Symbol::intern(&format!("infix:<{op}>")),
            args: vec![match_var, rhs],
        }
    };

    let regex_value = Value::Regex(Arc::new(pattern));

    let mut args = vec![
        Expr::Literal(regex_value),
        Expr::AnonSub {
            body: vec![Stmt::Expr(body_expr)],
            is_rw: false,
            is_block: true,
        },
    ];
    if adverbs.global {
        args.push(Expr::Literal(Value::Pair(
            "g".to_string(),
            Box::new(Value::Bool(true)),
        )));
    }

    Ok(Expr::AssignExpr {
        name: "_".to_string(),
        expr: Box::new(Expr::MethodCall {
            target: Box::new(Expr::Var("_".to_string())),
            name: Symbol::intern("subst"),
            args,
            modifier: None,
            quoted: false,
        }),
        is_bind: false,
    })
}

/// Build a destructive `s[pattern] = expr` substitution lowered to
/// `$_ = $_.subst(pattern, { expr })`. Used for Perl5 substitutions, whose
/// closure replacement must bind Perl5 captures via the `.subst` method.
pub(super) fn build_topic_subst_expr(
    pattern: String,
    replacement: Expr,
    adverbs: &MatchAdverbs,
) -> Result<Expr, PError> {
    let pattern = if adverbs.perl5 {
        pattern
    } else {
        let p = apply_inline_match_adverbs(pattern, adverbs);
        validate_regex_pattern_or_perror(&p)?;
        p
    };

    let regex_value = if adverbs.perl5 {
        build_regex_with_adverbs(pattern, adverbs)
    } else {
        Value::Regex(Arc::new(pattern))
    };

    let mut args = vec![
        Expr::Literal(regex_value),
        Expr::AnonSub {
            body: vec![Stmt::Expr(replacement)],
            is_rw: false,
            is_block: true,
        },
    ];
    if adverbs.global {
        args.push(Expr::Literal(Value::Pair(
            "g".to_string(),
            Box::new(Value::Bool(true)),
        )));
    }

    Ok(Expr::AssignExpr {
        name: "_".to_string(),
        expr: Box::new(Expr::MethodCall {
            target: Box::new(Expr::Var("_".to_string())),
            name: Symbol::intern("subst"),
            args,
            modifier: None,
            quoted: false,
        }),
        is_bind: false,
    })
}

/// Build a non-destructive substitution expression from `S[pattern] = expr`.
/// This is equivalent to `$_.subst(pattern, { expr })` without modifying `$_`.
pub(super) fn build_non_destructive_subst_expr(
    pattern: String,
    replacement: Expr,
    adverbs: &MatchAdverbs,
) -> Result<Expr, PError> {
    let pattern = if adverbs.perl5 {
        pattern
    } else {
        let p = apply_inline_match_adverbs(pattern, adverbs);
        validate_regex_pattern_or_perror(&p)?;
        p
    };

    let regex_value = if adverbs.perl5 {
        build_regex_with_adverbs(pattern, adverbs)
    } else {
        Value::Regex(Arc::new(pattern))
    };

    let mut args = vec![
        Expr::Literal(regex_value),
        Expr::AnonSub {
            body: vec![Stmt::Expr(replacement)],
            is_rw: false,
            is_block: true,
        },
    ];
    if adverbs.global {
        args.push(Expr::Literal(Value::Pair(
            "g".to_string(),
            Box::new(Value::Bool(true)),
        )));
    }
    if adverbs.samecase {
        args.push(Expr::Literal(Value::Pair(
            "samecase".to_string(),
            Box::new(Value::Bool(true)),
        )));
    }
    if adverbs.samemark {
        args.push(Expr::Literal(Value::Pair(
            "samemark".to_string(),
            Box::new(Value::Bool(true)),
        )));
    }
    if adverbs.samespace {
        args.push(Expr::Literal(Value::Pair(
            "samespace".to_string(),
            Box::new(Value::Bool(true)),
        )));
    }

    Ok(Expr::MethodCall {
        target: Box::new(Expr::Var("_".to_string())),
        name: Symbol::intern("subst"),
        args,
        modifier: None,
        quoted: false,
    })
}
