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

/// The leading and trailing docs `node` carries, if it carries any.
// Cost: O(1).
pub(crate) fn docs_of(node: &RakuAstNode) -> Option<(Option<String>, Option<String>)> {
    let (leading, trailing) = (text(node, LEADING), text(node, TRAILING));
    (leading.is_some() || trailing.is_some()).then_some((leading, trailing))
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

/// The lines of one documentation text: the hidden field of a parsed node, or
/// the `leading`/`trailing` list of the `Doc::Declarator` a node was given as
/// its `WHY`.
// Cost: O(f + l), f = fields of the node, l = lines of the text.
fn lines_of(node: &RakuAstNode, label: &str, key: &str) -> Vec<String> {
    if let Some(text) = text(node, key) {
        return vec![text];
    }
    let Some(why) = node.fields.iter().find_map(|f| match (&f.name, &f.value) {
        (Some("WHY"), RakuAstFieldValue::Node(v)) => Some(v),
        _ => None,
    }) else {
        return Vec::new();
    };
    let ValueView::RakuAst(doc) = why.view() else {
        return Vec::new();
    };
    doc.fields
        .iter()
        .filter(|f| f.name == Some(label_name(label)))
        .flat_map(|f| match &f.value {
            RakuAstFieldValue::List(items) => items.iter().collect::<Vec<_>>(),
            RakuAstFieldValue::Node(item) => vec![item],
            RakuAstFieldValue::Adverb(_) => Vec::new(),
        })
        .filter_map(|item| match item.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        })
        .collect()
}

fn label_name(label: &str) -> &'static str {
    if label == "leading" {
        "leading"
    } else {
        "trailing"
    }
}

/// The `.declarator-docs(...)` call printed after a node rendered at `indent`.
// Cost: O(f + l), f = fields of the node, l = lines of its documentation.
pub(super) fn suffix(node: &RakuAstNode, indent: usize) -> String {
    let parts: Vec<(&str, Vec<String>)> = [("leading", LEADING), ("trailing", TRAILING)]
        .into_iter()
        .map(|(label, key)| (label, lines_of(node, label, key)))
        .filter(|(_, lines)| !lines.is_empty())
        .collect();
    if parts.is_empty() {
        return String::new();
    }
    let width = parts.iter().map(|(l, _)| l.len()).max().unwrap_or(0);
    let pad = " ".repeat(indent + 2);
    let inner = " ".repeat(indent + 4);
    let lines = parts
        .iter()
        .map(|(label, texts)| {
            let body = texts
                .iter()
                .map(|t| format!("{inner}{},\n", super::render::render_str_literal(t)))
                .collect::<String>();
            format!("{pad}{label:<width$} => (\n{body}{pad})")
        })
        .collect::<Vec<_>>()
        .join(",\n");
    format!(".declarator-docs(\n{lines}\n{})", " ".repeat(indent))
}
