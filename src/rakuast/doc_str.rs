//! `.Str` of the `RakuAST::Doc` nodes: the text a Pod block holds, as rakudo
//! 2026.09 answers it (#14880). A block is its paragraphs run together, a
//! paragraph its atoms, a formatting code is rewritten with its delimiters
//! (`B<x>`, `X<a|b>`, `E<gt;lt>`) and a table row is put back as written
//! (cells apart by two spaces, or by `|` when it has dividers).

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// The text of a Doc node.
// Cost: O(n), n = size of the subtree.
pub(super) fn text(node: &RakuAstNode) -> String {
    match node.class {
        RakuAstClass::DocBlock => list_text(node, "paragraphs"),
        RakuAstClass::DocParagraph => positional_text(node),
        RakuAstClass::DocMarkup => markup_text(node),
        RakuAstClass::DocLegacyRow => row_text(node),
        _ => String::new(),
    }
}

// Cost: O(n), n = size of the item.
fn item_text(item: &Value) -> String {
    match item.view() {
        ValueView::Str(s) => s.to_string(),
        ValueView::RakuAst(node) => text(node),
        _ => String::new(),
    }
}

fn field_items<'a>(node: &'a RakuAstNode, name: &str) -> &'a [Value] {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::List(items) => Some(items.as_slice()),
            _ => None,
        })
        .unwrap_or(&[])
}

fn field_str(node: &RakuAstNode, name: &str) -> Option<String> {
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

// Cost: O(n).
fn list_text(node: &RakuAstNode, name: &str) -> String {
    field_items(node, name).iter().map(item_text).collect()
}

// Cost: O(n).
fn positional_text(node: &RakuAstNode) -> String {
    node.fields
        .iter()
        .filter(|f| !super::origin::is_origin(f))
        .map(|f| match &f.value {
            RakuAstFieldValue::Node(v) => item_text(v),
            RakuAstFieldValue::List(items) => items.iter().map(item_text).collect(),
            RakuAstFieldValue::Adverb(_) => String::new(),
        })
        .collect()
}

// Cost: O(n).
fn markup_text(node: &RakuAstNode) -> String {
    let mut out = field_str(node, "letter").unwrap_or_default();
    out.push_str(&field_str(node, "opener").unwrap_or_else(|| "<".to_string()));
    out.push_str(&list_text(node, "atoms"));
    let meta = field_items(node, "meta");
    let pairs: Vec<String> = meta
        .iter()
        .filter_map(|m| match m.view() {
            ValueView::Pair(key, _) => Some(key.to_string()),
            ValueView::ValuePair(key, _) => Some(key.to_string_value()),
            _ => None,
        })
        .collect();
    if !pairs.is_empty() {
        // An entity code: the names between the delimiters.
        out.push_str(&pairs.join(";"));
    } else if !meta.is_empty() {
        out.push('|');
        out.push_str(&meta.iter().map(item_text).collect::<Vec<_>>().join(" "));
    }
    out.push_str(&field_str(node, "closer").unwrap_or_else(|| ">".to_string()));
    out
}

// Cost: O(n).
fn row_text(node: &RakuAstNode) -> String {
    let cells: Vec<String> = field_items(node, "cells").iter().map(item_text).collect();
    if let Some(_dividers) = field_str(node, "column-dividers") {
        let offsets = field_items(node, "column-offsets");
        // The first divider at column 0 puts the first cell at offset 2; a
        // divider after the last cell gives one offset per cell.
        let leading = offsets.first().is_some_and(|o| o.as_int() == Some(2));
        // The dividers are the gaps between cells, plus one at either edge.
        let trailing = offsets.len() > cells.len().saturating_sub(1) + usize::from(leading);
        let mut out = String::new();
        if leading {
            out.push_str("| ");
        }
        out.push_str(&cells.join(" | "));
        if trailing {
            out.push_str(" |");
        }
        out.push('\n');
        return out;
    }
    // A cell may span lines (a header): the lines of the cells go side by side.
    let lines: Vec<Vec<&str>> = cells.iter().map(|c| c.split('\n').collect()).collect();
    let height = lines.iter().map(Vec::len).max().unwrap_or(0);
    let mut out = String::new();
    for k in 0..height {
        let row: Vec<&str> = lines.iter().map(|l| l.get(k).copied().unwrap_or("")).collect();
        out.push_str(&row.join("  "));
        out.push('\n');
    }
    out
}
