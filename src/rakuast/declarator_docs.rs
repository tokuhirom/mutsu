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

/// Record `doc` on `node`: one field per text, a list of the comments' lines
/// when the parser kept them.
// Cost: O(l), l = lines of the doc.
pub(super) fn attach(node: &mut RakuAstNode, doc: Option<&DeclDoc>) {
    let Some(doc) = doc else { return };
    for (name, text, lines) in [
        (LEADING, &doc.leading, &doc.leading_lines),
        (TRAILING, &doc.trailing, &doc.trailing_lines),
    ] {
        let Some(text) = text else { continue };
        let value = if lines.is_empty() {
            RakuAstFieldValue::Node(Value::str(text.clone()))
        } else {
            RakuAstFieldValue::List(lines.iter().map(|l| Value::str(l.clone())).collect())
        };
        node.fields.push(RakuAstField {
            name: Some(name),
            value,
        });
    }
}

/// The comments of one hidden field, one entry per line.
// Cost: O(f + l), f = fields of the node, l = lines.
pub(super) fn lines(node: &RakuAstNode, name: &str) -> Vec<String> {
    let Some(field) = node.fields.iter().find(|f| f.name == Some(name)) else {
        return Vec::new();
    };
    let items: Vec<&Value> = match &field.value {
        RakuAstFieldValue::Node(v) => vec![v],
        RakuAstFieldValue::List(items) => items.iter().collect(),
        RakuAstFieldValue::Adverb(_) => Vec::new(),
    };
    items
        .into_iter()
        .filter_map(|v| match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        })
        .collect()
}

/// The joined text of one hidden field (the comments joined by a space).
// Cost: O(f + l).
fn text(node: &RakuAstNode, name: &str) -> Option<String> {
    let lines = lines(node, name);
    (!lines.is_empty()).then(|| lines.join(" "))
}

/// The leading and trailing docs `node` carries, if it carries any.
// Cost: O(1).
pub(crate) fn docs_of(node: &RakuAstNode) -> Option<(Option<String>, Option<String>)> {
    let (leading, trailing) = (text(node, LEADING), text(node, TRAILING));
    (leading.is_some() || trailing.is_some()).then_some((leading, trailing))
}

/// The docs `node` carries as the parser's value, if it carries any.
// Cost: O(f + l).
pub(super) fn doc(node: &RakuAstNode) -> Option<DeclDoc> {
    let doc = DeclDoc {
        leading: text(node, LEADING),
        trailing: text(node, TRAILING),
        leading_lines: lines(node, LEADING),
        trailing_lines: lines(node, TRAILING),
    };
    (!doc.is_empty()).then_some(doc)
}

/// A filled documentation slot for the docs `node` carries, if any.
// Cost: O(1).
pub(super) fn slot(node: &RakuAstNode) -> Option<DocSlot> {
    let doc = DeclDoc {
        leading: text(node, LEADING),
        trailing: text(node, TRAILING),
        leading_lines: lines(node, LEADING),
        trailing_lines: lines(node, TRAILING),
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
    let own = lines(node, key);
    if !own.is_empty() {
        return own;
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
