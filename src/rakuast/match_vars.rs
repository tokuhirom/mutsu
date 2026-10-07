//! Match variables across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09:
//!
//! ```text
//! $<a>     Var::NamedCapture(QuotedString(processors => <words val>, segments => ("a",)), sigil => "$")
//! @<a>     the same with sigil => "@"
//! $0       Var::PositionalCapture(0)
//! ```
//!
//! The parser keeps `$<a>` as [`Expr::CaptureVar`] and spells `@<a>` as the
//! `.list` call on it, so only the scalar form is rendered here (`@<a>` reads
//! back as `$<a>.list`). It keeps `$0` as the variable `0` — except inside a
//! string, where its interpolation reads `$/[0]` — so both read back as
//! `Var::PositionalCapture`, and a `$/[0]` written out in a string does too
//! (rakudo renders that one as an index of `$/`).

use super::convert::{leaf_field, node_field, word_quote};
use super::lower::{leaf_str, named_child_or_positional, unsupported};
use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::value::{RuntimeError, Value, ValueView};

/// `$<name>` as `Var::NamedCapture`.
// Cost: O(|name|).
pub(super) fn convert(name: &str) -> Result<RakuAstNode, RuntimeError> {
    Ok(RakuAstNode {
        class: RakuAstClass::VarNamedCapture,
        fields: vec![
            node_field(None, word_quote(name)),
            leaf_field(Some("sigil"), Value::str("$".to_string())),
        ],
    })
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
pub(super) fn lower(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let index = named_child_or_positional(node)?;
    let name = indexed_name(index).ok_or_else(|| unsupported(node))?;
    let capture = Expr::CaptureVar(name);
    match leaf_str(node, "sigil")?.as_str() {
        "$" => Ok(capture),
        "@" => Ok(Expr::MethodCall {
            target: Box::new(capture),
            name: crate::symbol::Symbol::intern("list"),
            args: Vec::new(),
            modifier: None,
            quoted: false,
            sugar: false,
        }),
        _ => Err(unsupported(node)),
    }
}

/// `$0` as `Var::PositionalCapture`.
// Cost: O(1).
fn positional(index: i64) -> RakuAstNode {
    RakuAstNode {
        class: RakuAstClass::VarPositionalCapture,
        fields: vec![leaf_field(None, Value::int(index))],
    }
}

/// The variable `0`, `1`, ... as `Var::PositionalCapture`, or `None` for any
/// other name.
// Cost: O(|name|).
pub(super) fn convert_positional(name: &str) -> Option<RakuAstNode> {
    if name.is_empty() || !name.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    name.parse().ok().map(positional)
}

/// A string's interpolated `$0` (`$/[0]`) as `Var::PositionalCapture`, or
/// `None` for any other expression.
// Cost: O(1).
pub(super) fn convert_interpolated(expr: &Expr) -> Option<RakuAstNode> {
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
pub(super) fn lower_positional(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    match super::lower::positional_leaf(node)?.view() {
        ValueView::Int(n) if n >= 0 => Ok(Expr::Var(n.to_string())),
        _ => Err(unsupported(node)),
    }
}
