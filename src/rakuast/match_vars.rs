//! Match variables across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09:
//!
//! ```text
//! $<a>     Var::NamedCapture(QuotedString(processors => <words val>, segments => ("a",)), sigil => "$")
//! @<a>     the same with sigil => "@" (and %<a> with "%")
//! $0       Var::PositionalCapture(0)
//! ```
//!
//! The parser keeps `$<a>` as [`Expr::CaptureVar`] and spells `@<a>` / `%<a>`
//! as the `.list` / `.hash` call on it, wrapped in `Spelling::CaptureSigil`
//! so the written sigil reads back. It keeps `$0` as the variable `0` — except inside a
//! string, where its interpolation reads `$/[0]` — so both read back as
//! `Var::PositionalCapture`, and a `$/[0]` written out in a string does too
//! (rakudo renders that one as an index of `$/`).

use super::lower::{leaf_str, named_child_or_positional, unsupported};
use super::view::{leaf_field, node_field, view_expr, word_quote};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::ast::parsed::ParsedExpr;
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value, ValueView};

/// `$<name>` as `Var::NamedCapture`.
// Cost: O(|name|).
pub(super) fn view_node(name: &str) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::VarNamedCapture,
        fields: vec![
            node_field(None, word_quote(name)),
            leaf_field(Some("sigil"), Value::str("$".to_string())),
        ],
    })
}

/// `@<name>` / `%<name>`: the parser's `.list` / `.hash` of `$<name>`, as
/// `Var::NamedCapture` with the written sigil.
// Cost: O(|name|).
pub(super) fn view_sigiled(expr: &Expr, sigil: char) -> Result<RakuAstNode, RuntimeError> {
    let Expr::MethodCall { target, .. } = expr else {
        return Err(super::view::unsupported("sigiled named capture"));
    };
    // `@0` / `%0`: the numbered capture, which keeps the written sigil.
    if let Expr::Var(digits) = target.as_ref()
        && let Some(mut node) = view_positional(digits)
    {
        node.origin.3 = Some(sigil.to_string().into());
        return Ok(node);
    }
    let Expr::CaptureVar(name) = target.as_ref() else {
        return Err(super::view::unsupported("sigiled named capture"));
    };
    let mut node = view_node(name)?;
    node.fields[1] = leaf_field(Some("sigil"), Value::str(sigil.to_string()));
    Ok(node)
}

/// The word a `$<name>` index quotes: a single `StrLiteral` segment.
fn indexed_name(index: &RakuAstNode) -> Option<String> {
    if index.class != RakuAstClass::QuotedString {
        return None;
    }
    let segments = index.fields.iter().find(|f| f.name == Some("segments"))?;
    let RakuAstFieldValue::List(items) = &segments.value else {
        return None;
    };
    let [only] = items.as_slice() else {
        return None;
    };
    let ValueView::RakuAst(segment) = only.view() else {
        return None;
    };
    if segment.class != RakuAstClass::StrLiteral {
        return None;
    }
    let leaf = super::lower::positional_leaf(segment).ok()?;
    match leaf.view() {
        ValueView::Str(s) => Some(s.to_string()),
        _ => None,
    }
}

/// `Var::NamedCapture` as the parser's [`Expr::CaptureVar`] (or its `.list`).
// Cost: O(1).
pub(super) fn lower(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let index = named_child_or_positional(node)?;
    let name = indexed_name(index).ok_or_else(|| unsupported(node))?;
    if let Some(pairs) = lower_colonpairs(names, node)? {
        return adverbed(Expr::Literal(Value::str(name)), false, pairs);
    }
    match leaf_str(node, "sigil")?.as_str() {
        "$" => Ok(Expr::CaptureVar(name)),
        "@" => Ok(Expr::named_capture_call(&name, '@')),
        "%" => Ok(Expr::named_capture_call(&name, '%')),
        _ => Err(unsupported(node)),
    }
}

/// `$0` as `Var::PositionalCapture`.
// Cost: O(1).
fn positional(index: i64) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::VarPositionalCapture,
        fields: vec![leaf_field(None, Value::int(index))],
    }
}

/// A parsed `$0`: the capture variable that remembers its written sigil
/// (`.sigil` answers it; the constructor form does not show it).
// Cost: O(1).
fn parsed_positional(index: i64) -> RakuAstNode {
    let mut node = positional(index);
    node.origin.3 = Some("$".into());
    node
}

/// The variable `0`, `1`, ... as `Var::PositionalCapture`, or `None` for any
/// other name.
// Cost: O(|name|).
pub(super) fn view_positional(name: &str) -> Option<RakuAstNode> {
    if name.is_empty() || !name.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    name.parse().ok().map(parsed_positional)
}

/// A string's interpolated `$0` (`$/[0]`) as `Var::PositionalCapture`, or
/// `None` for any other expression.
// Cost: O(1).
pub(super) fn view_interpolated(expr: &Expr) -> Option<RakuAstNode> {
    let Expr::Index {
        target,
        index,
        is_positional: true,
        ..
    } = expr
    else {
        return None;
    };
    let (Expr::Var(matched), Expr::Literal(index)) = (target.as_ref(), index.as_ref()) else {
        return None;
    };
    match (matched.as_str(), index.view()) {
        ("/", ValueView::Int(n)) if n >= 0 => Some(positional(n)),
        _ => None,
    }
}

/// `Var::PositionalCapture` as the variable the parser reads a capture
/// through.
// Cost: O(1).
pub(super) fn lower_positional(
    names: &NameScope,
    node: &RakuAstNode,
) -> Result<Expr, RuntimeError> {
    let n = match super::lower::positional_leaf(node)?.view() {
        ValueView::Int(n) if n >= 0 => n,
        _ => return Err(unsupported(node)),
    };
    if let Some(pairs) = lower_colonpairs(names, node)? {
        return adverbed(Expr::Literal(Value::int(n)), true, pairs);
    }
    // A parsed `@0` / `%0` is the capture contextualized, as the parser runs it.
    let method = match node.origin.3.as_deref() {
        Some("@") => Some(("list", true)),
        Some("%") => Some(("hash", true)),
        _ => None,
    };
    if let Some((method, sugar)) = method {
        return Ok(Expr::MethodCall {
            target: Box::new(Expr::Var(n.to_string())),
            name: crate::symbol::Symbol::intern(method),
            args: Vec::new(),
            modifier: None,
            quoted: false,
            sugar,
        });
    }
    Ok(Expr::Var(n.to_string()))
}

/// The subscript adverbs (`colonpairs`) of a capture variable, lowered, or
/// `None` when it has none.
// Cost: O(p), p = size of the adverbs.
fn lower_colonpairs(
    names: &NameScope,
    node: &RakuAstNode,
) -> Result<Option<Vec<Expr>>, RuntimeError> {
    let Some(field) = node.fields.iter().find(|f| f.name == Some("colonpairs")) else {
        return Ok(None);
    };
    let RakuAstFieldValue::List(items) = &field.value else {
        return Ok(None);
    };
    if items.is_empty() {
        return Ok(None);
    }
    let mut pairs = Vec::with_capacity(items.len());
    for item in items {
        let ValueView::RakuAst(pair) = item.view() else {
            return Err(unsupported(node));
        };
        pairs.push(super::lower::lower_expr(names, pair)?);
    }
    Ok(Some(pairs))
}

/// `$/[index]` / `$/{index}` with the subscript adverbs `pairs`: what the
/// adverbed capture sugar stands for, expanded as the compiler runs it.
// Cost: O(p), p = adverbs.
fn adverbed(index: Expr, positional: bool, pairs: Vec<Expr>) -> Result<Expr, RuntimeError> {
    super::subscript_adverb::lower_parsed(
        Expr::Index {
            target: Box::new(Expr::Var("/".to_string())),
            index: Box::new(index),
            is_positional: positional,
            spelling: Default::default(),
        },
        pairs,
    )
}

/// `$<x>:exists` / `$0:delete` as the capture variable carrying its subscript
/// adverbs in `colonpairs`, as rakudo keeps them.
// Cost: O(p), p = size of the adverbs.
pub(super) fn view_with_adverbs(
    names: &NameScope,
    expr: &Expr,
) -> Result<RakuAstNode, RuntimeError> {
    let wrong = || RuntimeError::new("RakuAST: capture adverb spelling without its subscript");
    let Expr::Ast(parsed) = expr else {
        return Err(wrong());
    };
    let ParsedExpr::SubscriptAdverbs(adverbs) = parsed.as_ref() else {
        return Err(wrong());
    };
    let Expr::Index {
        index,
        is_positional,
        ..
    } = &adverbs.subscript
    else {
        return Err(wrong());
    };
    let Expr::Literal(index) = index.as_ref() else {
        return Err(wrong());
    };
    let mut node = match (is_positional, index.view()) {
        (true, ValueView::Int(n)) if n >= 0 => parsed_positional(n),
        (false, ValueView::Str(name)) => view_node(name.as_str())?,
        _ => return Err(wrong()),
    };
    let pairs = adverbs
        .pairs
        .iter()
        .map(|pair| view_expr(names, pair).map(|n| Value::rakuast(Box::new(n))))
        .collect::<Result<Vec<_>, _>>()?;
    node.fields.push(RakuAstField {
        name: Some("colonpairs"),
        value: RakuAstFieldValue::List(pairs),
    });
    Ok(node)
}
