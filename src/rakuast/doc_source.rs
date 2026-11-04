//! A hand-built `RakuAST::Doc::Block` written back as Pod source (#14880).
//!
//! `$=pod` is built from Pod source text, so `EVAL` of a tree holding
//! `Doc::Block` statements establishes it by writing those blocks as the text
//! they read from and handing that to the same builder as a source string.
//! Only the plain shapes are written (no configuration, no margin, no rows);
//! any other block leaves `$=pod` as it was, as before.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::ValueView;

/// The Pod source of the `Doc::Block` statements of the `StatementList` or
/// block `node` (top level only); `None` when there are none or one is not
/// a shape written here.
// Cost: O(n), n = size of the blocks.
pub(crate) fn pod_source(node: &RakuAstNode) -> Option<String> {
    if node.class == RakuAstClass::DocBlock {
        return block_source(node);
    }
    if node.class != RakuAstClass::StatementList {
        return None;
    }
    let mut out = String::new();
    for field in &node.fields {
        let RakuAstFieldValue::Node(value) = &field.value else {
            continue;
        };
        let ValueView::RakuAst(child) = value.view() else {
            continue;
        };
        if child.class == RakuAstClass::DocBlock {
            out.push_str(&block_source(child)?);
        }
    }
    (!out.is_empty()).then_some(out)
}

fn str_field(node: &RakuAstNode, name: &str) -> Option<String> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => match v.view() {
                ValueView::Str(s) => Some(s.to_string()),
                ValueView::Int(i) => Some(i.to_string()),
                _ => None,
            },
            _ => None,
        })
}

fn flag(node: &RakuAstNode, name: &str) -> bool {
    node.fields.iter().any(|f| f.name == Some(name))
}

// Cost: O(n), n = size of the block.
fn block_source(node: &RakuAstNode) -> Option<String> {
    if flag(node, "config") || flag(node, "margin") || flag(node, "directive") {
        return None;
    }
    let kind = str_field(node, "type")?;
    if kind.is_empty()
        || !kind
            .chars()
            .all(|c| c.is_alphanumeric() || matches!(c, '-' | '_'))
    {
        return None;
    }
    let name = match str_field(node, "level") {
        Some(level) => format!("{kind}{level}"),
        None => kind.clone(),
    };
    let items: &[crate::value::Value] = node
        .fields
        .iter()
        .find(|f| f.name == Some("paragraphs"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::List(items) => Some(items.as_slice()),
            _ => None,
        })
        .unwrap_or(&[]);
    let mut body = String::new();
    for item in items {
        match item.view() {
            ValueView::Str(s) => body.push_str(&paragraph(&s)),
            ValueView::RakuAst(inner) => match inner.class {
                RakuAstClass::DocBlock => body.push_str(&block_source(inner)?),
                RakuAstClass::DocParagraph => {
                    body.push_str(&paragraph(&super::doc_str::text(inner)));
                }
                _ => return None,
            },
            _ => return None,
        }
    }
    if flag(node, "abbreviated") {
        // `=head1 text`: the first line follows the name.
        let first = body.trim_end_matches('\n');
        if first.contains("\n\n") || items.len() > 1 {
            return None;
        }
        return Some(format!("={name} {first}\n\n"));
    }
    if flag(node, "for") {
        return Some(format!("=for {name}\n{body}\n"));
    }
    Some(format!("=begin {name}\n{body}=end {kind}\n"))
}

/// A paragraph ends in a line break and, between paragraphs, a blank line.
// Cost: O(n).
fn paragraph(text: &str) -> String {
    let mut out = text.to_string();
    if !out.ends_with('\n') {
        out.push('\n');
    }
    if !out.ends_with("\n\n") {
        out.push('\n');
    }
    out
}
