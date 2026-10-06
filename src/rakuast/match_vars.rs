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
//! back as `$<a>.list`).

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
        }),
        _ => Err(unsupported(node)),
    }
}
