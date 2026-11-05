//! Pod written inside a block of a statement (a class, sub or `if` body) as a
//! `RakuAST::Doc::Block` statement of that block's `StatementList` (#14880).
//!
//! [`super::pod_blocks`] reads the block from the source and finds that it lies
//! inside a top-level statement; here it is put among the statements of the
//! innermost block around it, and that list's range widened to the union of
//! its items, as rakudo 2026.09 does.

use super::infix_origins::byte_range;
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::value::{Value, ValueView};

/// Put the Pod blocks that lie inside a block of a statement into that
/// block's statement list.
// Cost: O(k * n), k = blocks, n = nodes of the tree.
pub(super) fn place_nested(
    list: &mut RakuAstNode,
    nested: Vec<(usize, usize, RakuAstNode)>,
    source: &str,
) {
    for (at, end, block) in nested {
        place(list, at, end, &block, source);
    }
}

/// Place `block` in the innermost `Blockoid` strictly around `at` whose
/// statements do not contain it; `true` when `node`'s subtree took (or
/// refused) it.
// Cost: O(n), n = nodes of the subtree.
fn place(node: &mut RakuAstNode, at: usize, end: usize, block: &RakuAstNode, source: &str) -> bool {
    if let Some((from, to)) = byte_range(node)
        && !(from <= at && at < to)
    {
        return false;
    }
    for field in &mut node.fields {
        let done = match &mut field.value {
            RakuAstFieldValue::Node(child) => place_in_value(child, at, end, block, source),
            RakuAstFieldValue::List(items) => items
                .iter_mut()
                .any(|item| place_in_value(item, at, end, block, source)),
            _ => false,
        };
        if done {
            return true;
        }
    }
    if node.class != RakuAstClass::Blockoid {
        return false;
    }
    let Some((from, to)) = byte_range(node) else {
        return false;
    };
    if !(from < at && at < to) {
        return false;
    }
    let Some(list) = node.fields.iter_mut().find_map(|f| match &mut f.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::RakuAst(n) if n.class == RakuAstClass::StatementList => Some(v),
            _ => None,
        },
        _ => None,
    }) else {
        return false;
    };
    let ValueView::RakuAst(held) = list.view() else {
        return false;
    };
    let mut inner = RakuAstNode::clone(held);
    insert_in_list(&mut inner, at, end, block, source);
    *list = Value::rakuast(Box::new(inner));
    true
}

// Cost: O(size of the subtree).
fn place_in_value(
    child: &mut Value,
    at: usize,
    end: usize,
    block: &RakuAstNode,
    source: &str,
) -> bool {
    let ValueView::RakuAst(held) = child.view() else {
        return false;
    };
    let mut node = RakuAstNode::clone(held);
    if place(&mut node, at, end, block, source) {
        *child = Value::rakuast(Box::new(node));
        true
    } else {
        false
    }
}

/// Insert `block` among the statements of `list` by offset, widening the
/// list's range to the union of its items. A block that a statement of the
/// list contains or starts inside is text to rakudo's reader: left out.
// Cost: O(s), s = statements of the list.
fn insert_in_list(
    list: &mut RakuAstNode,
    at: usize,
    end: usize,
    block: &RakuAstNode,
    source: &str,
) {
    let mut ranges: Vec<Option<(usize, usize)>> = Vec::with_capacity(list.fields.len());
    for f in &list.fields {
        if super::origin::is_origin(f) {
            ranges.push(None);
            continue;
        }
        let RakuAstFieldValue::Node(v) = &f.value else {
            return;
        };
        let ValueView::RakuAst(child) = v.view() else {
            return;
        };
        let Some((from, to)) = byte_range(child) else {
            return;
        };
        if (from < at && at < to) || (at < from && from < end) {
            return;
        }
        ranges.push(Some((from, to)));
    }
    let Some((block_from, block_to)) = byte_range(block) else {
        return;
    };
    let low = ranges
        .iter()
        .flatten()
        .map(|r| r.0)
        .fold(block_from, usize::min);
    let high = ranges
        .iter()
        .flatten()
        // A statement's `;` is part of the list.
        .map(|r| r.1 + usize::from(source[r.1..].starts_with(';')))
        .fold(block_to, usize::max);
    let mut fields = Vec::with_capacity(list.fields.len() + 1);
    let mut pending = Some(block.clone());
    for (f, range) in std::mem::take(&mut list.fields).into_iter().zip(ranges) {
        if let Some((from, _)) = range
            && from > at
            && let Some(node) = pending.take()
        {
            fields.push(RakuAstField {
                name: None,
                value: RakuAstFieldValue::Node(Value::rakuast(Box::new(node))),
            });
        }
        fields.push(f);
    }
    if let Some(node) = pending.take() {
        fields.push(RakuAstField {
            name: None,
            value: RakuAstFieldValue::Node(Value::rakuast(Box::new(node))),
        });
    }
    list.fields = fields;
    list.origin.0 = super::unit_source::span_origin_value(SrcSpan::new(low, high));
}
