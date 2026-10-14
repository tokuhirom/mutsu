//! Declarator documentation on an anonymous `sub` (`my $f = #| doc
//! anon sub {}`).
//!
//! Rakudo prints the docs as a trailing method call on the node, measured on
//! 2026.09:
//!
//! ```text
//! RakuAST::Sub.new(...).declarator-docs(
//!   leading  => (
//!     "doc",
//!   ),
//!   trailing => (
//!     "more",
//!   )
//! )
//! ```
//!
//! The model keeps the two texts in hidden fields (they are not constructor
//! arguments), which [`suffix`] renders after the node and lowering reads back
//! into the sub's documentation slot.

use super::{RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::decl_doc::{DeclDoc, DocSlot};
use crate::value::{Value, ValueView};

const LEADING: &str = "declarator-docs-leading";
const TRAILING: &str = "declarator-docs-trailing";

/// Whether `field` is one of the hidden documentation fields.
// Cost: O(1).
pub(super) fn is_field(field: &RakuAstField) -> bool {
    matches!(field.name, Some(LEADING | TRAILING))
}

/// Record `doc` on `node`.
// Cost: O(1).
pub(super) fn attach(node: &mut RakuAstNode, doc: Option<&DeclDoc>) {
    let Some(doc) = doc else { return };
    for (name, text) in [(LEADING, &doc.leading), (TRAILING, &doc.trailing)] {
        if let Some(text) = text {
            node.fields.push(RakuAstField {
                name: Some(name),
                value: RakuAstFieldValue::Node(Value::str(text.clone())),
            });
        }
    }
}

fn text(node: &RakuAstNode, name: &str) -> Option<String> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::Str(s) => Some(s.to_string()),
                _ => None,
            },
            _ => None,
        })
}

/// A filled documentation slot for the docs `node` carries, if any.
// Cost: O(1).
pub(super) fn slot(node: &RakuAstNode) -> Option<DocSlot> {
    let doc = DeclDoc {
        leading: text(node, LEADING),
        trailing: text(node, TRAILING),
    };
    if doc.is_empty() {
        return None;
    }
    let slot = DocSlot::pending();
    slot.fill(doc);
    Some(slot)
}

/// The `.declarator-docs(...)` call printed after a node rendered at `indent`.
// Cost: O(1).
pub(super) fn suffix(node: &RakuAstNode, indent: usize) -> String {
    let parts: Vec<(&str, String)> = [("leading", LEADING), ("trailing", TRAILING)]
        .into_iter()
        .filter_map(|(label, key)| text(node, key).map(|t| (label, t)))
        .collect();
    if parts.is_empty() {
        return String::new();
    }
    let width = parts.iter().map(|(l, _)| l.len()).max().unwrap_or(0);
    let pad = " ".repeat(indent + 2);
    let inner = " ".repeat(indent + 4);
    let lines = parts
        .iter()
        .map(|(label, t)| {
            format!(
                "{pad}{label:<width$} => (\n{inner}{},\n{pad})",
                super::render::render_str_literal(t)
            )
        })
        .collect::<Vec<_>>()
        .join(",\n");
    format!(".declarator-docs(\n{lines}\n{})", " ".repeat(indent))
}
