use crate::ast::{Expr, Stmt};
use crate::parser::helpers::is_non_breaking_space;
use crate::parser::parse_result::PError;
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::{Value, ValueView};

/// Does the input (which starts at a `:`) begin a bareword/variable colonpair
/// adverb — as opposed to a block/typed-hash/array/angle listop argument?
///
/// A no-space `:` right after a parenless method name is Raku's adverbial
/// colonpair (`$obj.git:so`, `.grep:Str`, `.foo:bar(3)`, `.m:$x`) and binds as a
/// named argument. But `.map:{...}` / `.method:[...]` / `.method:<...>` are
/// colon-listop calls taking a block / array / angle-word argument, so those
/// must NOT be diverted. This distinguishes the two by the character right after
/// the colon: an identifier start, `!name`, or a sigil is an adverb; a brace,
/// bracket, angle, digit, or anything else is left for the listop reading.
pub(crate) fn colonpair_adverb_follows(input: &str) -> bool {
    let Some(rest) = input.strip_prefix(':') else {
        return false;
    };
    // `::` is a package separator, never a colonpair here.
    let mut chars = rest.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => true,
        // `:!name` — a negated boolean colonpair. Require an identifier char to
        // follow so a stray `:!` (prefix `!`) is not mistaken for an adverb.
        Some('!') => chars.next().is_some_and(|c| c.is_alphabetic() || c == '_'),
        // `:$var` / `:@var` / `:%var` / `:&var` — a variable colonpair. Require a
        // name char after the sigil so bare sigils are not swallowed.
        Some('$') | Some('@') | Some('%') | Some('&') => {
            chars.next().is_some_and(|c| c.is_alphabetic() || c == '_')
        }
        _ => false,
    }
}

/// Check if an expression is a negative integer literal (e.g., `-(1)` from `-1`).
/// Returns the negative value string (e.g., "-1") if so.
pub(crate) fn extract_negative_literal(expr: &Expr) -> Option<String> {
    if let Expr::Unary {
        op: TokenKind::Minus,
        expr: inner,
    } = expr
        && let Expr::Literal(lit) = inner.as_ref()
    {
        if let ValueView::Int(n) = lit.view() {
            return Some(format!("-{}", n));
        }
        if let ValueView::BigInt(n) = lit.view() {
            return Some(format!("-{}", *n));
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
        // `=` is an ordinary character inside angle-word quoting, so a subscript
        // key may contain it: `%h<=>` is the string key `=`, `%h<=foo>` is
        // `=foo` (Rakudo). The tightly-bound `<...>` is always a subscript here,
        // never the `<=`/`<=>` infix operator (which needs surrounding space).
        || c == '='
        // Parentheses are ordinary characters inside angle-word quoting, so an
        // angle *subscript* key may contain them too: `%h<(default)>` is the
        // string key `(default)` (App::Rak). The `<...>` word-list literal
        // already accepts them; this aligns the subscript path.
        || c == '('
        || c == ')'
        // `#` is likewise an ordinary character inside angle-word quoting (it is
        // NOT a comment there), so a subscript key may contain it: `%exts<#csv>`
        // is the string key `#csv` (App::Rak). Aligns with the `<...>` literal.
        || c == '#'
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
        let inner = std::mem::replace(expr, Expr::Literal(Value::NIL));
        *expr = Expr::Unary {
            op,
            expr: Box::new(inner),
        };
    }
}
