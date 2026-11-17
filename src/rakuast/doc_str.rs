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
    out.push_str(&markup_inner(node));
    out.push_str(&field_str(node, "closer").unwrap_or_else(|| ">".to_string()));
    out
}

/// What sits between a formatting code's delimiters: its atoms and, after
/// them, the entity names or the `|` meta text. Also `Markup.flatten`.
// Cost: O(n).
pub(super) fn markup_inner(node: &RakuAstNode) -> String {
    let mut out = list_text(node, "atoms");
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
    out
}

// Cost: O(n).
fn row_text(node: &RakuAstNode) -> String {
    legacy_row_text(node, &mut |divider| Ok(divider.to_string())).unwrap_or_default()
}

/// `LegacyRow.Str(&divider)`: the row as written, line by line (a header cell
/// may span lines), each divider passed through `divider` (the deparser
/// highlights it), as rakudo 2026.09 answers it. A row of dividers is each
/// cell, then a blank and its divider (the empty string past the last one),
/// and a blank before the next cell; a divider at the line's start is written
/// as is. A row of columns is its cells two blanks apart. Each line drops its
/// trailing blanks.
// Cost: O(n) plus `divider` per divider and line, n = size of the cells.
pub(crate) fn legacy_row_text(
    node: &RakuAstNode,
    divider: &mut dyn FnMut(&str) -> Result<String, crate::value::RuntimeError>,
) -> Result<String, crate::value::RuntimeError> {
    let cells: Vec<String> = field_items(node, "cells").iter().map(item_text).collect();
    let lines: Vec<Vec<&str>> = cells.iter().map(|c| c.split('\n').collect()).collect();
    let height = lines.iter().map(Vec::len).max().unwrap_or(0);
    let dividers: Option<Vec<char>> =
        field_str(node, "column-dividers").map(|d| d.chars().collect());
    // The first divider at column 0 puts the first cell at offset 2.
    let leading = dividers.is_some()
        && field_items(node, "column-offsets")
            .first()
            .is_some_and(|o| o.as_int() == Some(2));
    let mut out = String::new();
    for k in 0..height {
        let row: Vec<&str> = lines
            .iter()
            .map(|l| l.get(k).copied().unwrap_or(""))
            .collect();
        let mut line = String::new();
        match &dividers {
            None => line.push_str(&row.join("  ")),
            Some(dividers) => {
                let mut rest = dividers.iter();
                if leading && let Some(first) = rest.next() {
                    line.push(*first);
                    line.push(' ');
                }
                for (i, cell) in row.iter().enumerate() {
                    if i > 0 {
                        line.push(' ');
                    }
                    line.push_str(cell);
                    line.push(' ');
                    let d = rest.next().map(char::to_string).unwrap_or_default();
                    line.push_str(&divider(&d)?);
                }
            }
        }
        out.push_str(line.trim_end_matches(' '));
        out.push('\n');
    }
    Ok(out)
}
