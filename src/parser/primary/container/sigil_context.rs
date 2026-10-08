use crate::ast::{ContextKind, Expr};
use crate::parser::parse_result::{PError, PResult};
use crate::symbol::Symbol;

use super::array::array_literal;
use super::paren::paren_expr;

/// `$(STMT; STMT; ...)`: the statements as one scope-transparent block whose
/// last value is itemized.
///
/// Built here for the parser and for RakuAST lowering, and taken apart again
/// by [`item_statements_parts`] when `.AST` renders the written
/// `Contextualizer::Item(StatementSequence(..))`.
pub(crate) fn item_statements_expr(stmts: Vec<crate::ast::Stmt>) -> Expr {
    Expr::MethodCall {
        target: Box::new(Expr::desugar_block(stmts)),
        name: Symbol::intern("item"),
        args: vec![],
        modifier: None,
        quoted: false,
        sugar: false,
    }
}

/// The statements of an expression [`item_statements_expr`] built.
// Cost: O(1).
pub(crate) fn item_statements_parts(expr: &Expr) -> Option<&[crate::ast::Stmt]> {
    let Expr::MethodCall {
        target,
        name,
        args,
        modifier: None,
        quoted: false,
        sugar: false,
    } = expr
    else {
        return None;
    };
    let Expr::DoBlock {
        body,
        label: None,
        origin: crate::ast::DoBlockOrigin::Desugar,
    } = target.as_ref()
    else {
        return None;
    };
    (args.is_empty() && name.with_str(|n| n == "item")).then_some(body.as_slice())
}

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
        let Expr::DoBlock { body, .. } = block_expr else {
            unreachable!("parse_dollar_paren_block builds a DoBlock")
        };
        return Ok((block_rest, item_statements_expr(body)));
    }
    let (rest, inner) = paren_expr(rest)?;
    // $(expr) compiles to expr.item — wraps the value in a Scalar container
    Ok((
        rest,
        Expr::Contextualizer {
            kind: ContextKind::Item,
            inner: Box::new(inner),
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
        Expr::Contextualizer {
            kind: ContextKind::Item,
            inner: Box::new(inner),
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
        Expr::Contextualizer {
            kind: ContextKind::List,
            inner: Box::new(inner),
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
        Expr::Contextualizer {
            kind: ContextKind::Hash,
            inner: Box::new(inner),
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
    let block_start = rest;
    // rakudo's `special_variable:sym<${ }>` guard decides this before the block
    // is parsed: a `{...}` holding a pair, a `=>` or a `|%` slip is the Raku
    // contextualizer, not the Perl 5 deref (see `is_brace_contextualizer`).
    let is_contextualizer = crate::parser::primary::var::is_brace_contextualizer(
        crate::parser::primary::var::brace_deref_text(block_start),
    );
    let (rest, inner) = crate::parser::primary::misc::block_or_hash_expr(rest)?;
    // When the inner expression is a Hash literal, ${ } creates an itemized hash
    // (wrapped in a Scalar container), not a Capture.
    if is_contextualizer || matches!(inner, Expr::Hash(..)) {
        Ok((
            rest,
            Expr::MethodCall {
                target: Box::new(inner),
                name: Symbol::intern("item"),
                args: vec![],
                modifier: None,
                quoted: false,
                sugar: true,
            },
        ))
    } else {
        // ${expr} where expr is not a hash is Perl 5 scalar dereference syntax
        let block_src = &block_start[..block_start.len() - rest.len()];
        let deref_inner = block_src
            .strip_prefix('{')
            .and_then(|s| s.strip_suffix('}'))
            .unwrap_or("expr");
        Err(crate::parser::parse_result::PError::from_typed(
            crate::value::RuntimeError::obsolete_p5_deref('$', deref_inner),
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
            sugar: true,
        },
    ))
}
