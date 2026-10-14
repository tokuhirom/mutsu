//! Substitution expression building helpers.

use crate::ast::Expr;
use crate::parser::expr::expression_no_word_logical;
use crate::parser::helpers::ws;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;

pub(super) fn parse_subst_replacement_expr(input: &str) -> PResult<'_, String> {
    let (input, _) = ws(input)?;
    // Parse the FULL replacement expression (not just the first primary): the
    // literal shortcut is only valid when the entire replacement is a single
    // literal. Using `primary` here would stop after the first term, so a
    // compound replacement like `"<" ~ $/ ~ ">"` would leave `~ $/ ~ ">"` in the
    // stream to be (mis)parsed as an outer binary expression, silently dropping
    // it from the substitution. Requiring the parse to yield a bare `Literal`
    // routes any compound replacement to the closure/block path instead.
    // The RHS sits at item-assignment precedence, so the loose word-logical
    // operators bind looser than it: `s{^ "row="} = "" and f()` assigns only
    // `""` and leaves `and f()` for the enclosing statement.
    let (rest, expr) = expression_no_word_logical(input)?;
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

/// The per-match replacement of `s[pattern] OP= EXPR`: `$/ OP EXPR`, with a
/// user operator called as `infix:<OP>($/, EXPR)`. The parser and the RakuAST
/// lowering both build it here; [`subst_compound_rhs`] reads `EXPR` back.
// Cost: O(1).
pub(crate) fn subst_compound_thunk(op: &str, rhs: Expr) -> Expr {
    // `$/` is stored as Var("/") in the AST (the parser strips the sigil).
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
    match op_token {
        Some(op) => Expr::Binary {
            left: Box::new(match_var),
            op,
            right: Box::new(rhs),
            form: Default::default(),
        },
        None => Expr::Call {
            name: Symbol::intern(&format!("infix:<{op}>")),
            args: vec![match_var, rhs],
            listop: false,
        },
    }
}

/// The written `EXPR` of a [`subst_compound_thunk`].
// Cost: O(1).
pub(crate) fn subst_compound_rhs(thunk: &Expr) -> Option<&Expr> {
    match thunk {
        Expr::Binary { right, .. } => Some(right),
        Expr::Call { args, .. } if args.len() == 2 => Some(&args[1]),
        _ => None,
    }
}
