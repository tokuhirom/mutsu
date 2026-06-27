use crate::ast::Expr;
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;

use super::array::array_literal;
use super::paren::paren_expr;

/// Parse itemized parenthesized expression: `$(...)`.
///
/// In Raku, `$(expr)` creates an item container — the value is evaluated and
/// wrapped in a scalar so that operations like `.flat` treat it as a single
/// opaque element.  We lower this to a method call `.item` on the inner
/// expression, which mirrors what Rakudo does internally.
pub(crate) fn itemized_paren_expr(input: &str) -> PResult<'_, Expr> {
    let Some(rest) = input.strip_prefix('$') else {
        return Err(PError::expected("itemized parenthesized expression"));
    };
    if !rest.starts_with('(') {
        return Err(PError::expected("itemized parenthesized expression"));
    }
    // Try multi-statement block first: $(temp $a = 23; $a)
    let inner = &rest[1..]; // skip '('
    if let Ok((block_rest, block_expr)) =
        crate::parser::primary::var::parse_dollar_paren_block_pub(inner)
    {
        return Ok((
            block_rest,
            Expr::MethodCall {
                target: Box::new(block_expr),
                name: Symbol::intern("item"),
                args: vec![],
                modifier: None,
                quoted: false,
            },
        ));
    }
    let (rest, inner) = paren_expr(rest)?;
    // Lower $(expr) to expr.item — wraps the value in a Scalar container
    Ok((
        rest,
        Expr::MethodCall {
            target: Box::new(inner),
            name: Symbol::intern("item"),
            args: vec![],
            modifier: None,
            quoted: false,
        },
    ))
}

/// Parse an item-context coercion applied to a list/hash contextualizer:
/// `$@(...)` and `$%(...)`.
///
/// In Raku these are the itemizing forms of the list/hash contextualizers:
/// the inner `@(...)` / `%(...)` builds a List / Hash, and the leading `$`
/// itemizes it (wraps it in a Scalar container). We lower `$@(expr)` to
/// `@(expr).item` and `$%(expr)` to `%(expr).item`.
pub(crate) fn itemized_context_paren_expr(input: &str) -> PResult<'_, Expr> {
    let Some(rest) = input.strip_prefix('$') else {
        return Err(PError::expected("itemized context paren expression"));
    };
    let (rest, inner) = if rest.starts_with("@(") {
        list_context_paren_expr(rest)?
    } else if rest.starts_with("%(") {
        hash_context_paren_expr(rest)?
    } else {
        return Err(PError::expected("itemized context paren expression"));
    };
    Ok((
        rest,
        Expr::MethodCall {
            target: Box::new(inner),
            name: Symbol::intern("item"),
            args: vec![],
            modifier: None,
            quoted: false,
        },
    ))
}

/// Parse list-context parenthesized expression: `@(...)`.
///
/// In Raku, `@(expr)` coerces the expression into list context.
/// We lower this to a method call `.list` on the inner expression.
pub(crate) fn list_context_paren_expr(input: &str) -> PResult<'_, Expr> {
    let Some(rest) = input.strip_prefix('@') else {
        return Err(PError::expected("list-context parenthesized expression"));
    };
    if !rest.starts_with('(') {
        return Err(PError::expected("list-context parenthesized expression"));
    }
    let (rest, inner) = paren_expr(rest)?;
    Ok((
        rest,
        Expr::MethodCall {
            target: Box::new(inner),
            name: Symbol::intern("list"),
            args: vec![],
            modifier: None,
            quoted: false,
        },
    ))
}

/// Parse hash-context parenthesized expression: `%(...)`.
///
/// In Raku, `%(expr)` coerces the expression into hash context.
/// We lower this to a method call `.hash` on the inner expression.
pub(crate) fn hash_context_paren_expr(input: &str) -> PResult<'_, Expr> {
    let Some(rest) = input.strip_prefix('%') else {
        return Err(PError::expected("hash-context parenthesized expression"));
    };
    if !rest.starts_with('(') {
        return Err(PError::expected("hash-context parenthesized expression"));
    }
    let (rest, inner) = paren_expr(rest)?;
    Ok((
        rest,
        Expr::MethodCall {
            target: Box::new(inner),
            name: Symbol::intern("hash"),
            args: vec![],
            modifier: None,
            quoted: false,
        },
    ))
}

/// Parse itemized brace expression: `${ }`.
///
/// In Raku, `${ a => 1, b => 2 }` creates an itemized hash — it wraps the
/// hash in a Scalar container so it's treated as a single element.
pub(crate) fn itemized_brace_expr(input: &str) -> PResult<'_, Expr> {
    let Some(rest) = input.strip_prefix('$') else {
        return Err(PError::expected("itemized brace expression"));
    };
    if !rest.starts_with('{') {
        return Err(PError::expected("itemized brace expression"));
    }
    let (rest, inner) = crate::parser::primary::misc::block_or_hash_expr(rest)?;
    // When the inner expression is a Hash literal, ${ } creates an itemized hash
    // (wrapped in a Scalar container), not a Capture.
    if matches!(inner, Expr::Hash(_)) {
        Ok((
            rest,
            Expr::MethodCall {
                target: Box::new(inner),
                name: Symbol::intern("item"),
                args: vec![],
                modifier: None,
                quoted: false,
            },
        ))
    } else {
        // ${expr} where expr is not a hash is Perl 5 scalar dereference syntax
        Err(crate::parser::parse_result::PError::fatal(
            "X::Obsolete: Unsupported use of ${expr}. In Raku please use: $(expr).".to_string(),
        ))
    }
}

/// Parse itemized bracket expression: `$[...]`.
///
/// Rakudo lowers this as a normal bracket constructor followed by `.item`.
pub(crate) fn itemized_bracket_expr(input: &str) -> PResult<'_, Expr> {
    let Some(rest) = input.strip_prefix('$') else {
        return Err(PError::expected("itemized bracket expression"));
    };
    if !rest.starts_with('[') {
        return Err(PError::expected("itemized bracket expression"));
    }
    let (rest, inner) = array_literal(rest)?;
    Ok((
        rest,
        Expr::MethodCall {
            target: Box::new(inner),
            name: Symbol::intern("item"),
            args: vec![],
            modifier: None,
            quoted: false,
        },
    ))
}
