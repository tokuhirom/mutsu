//! Dropping the origins a heredoc's body took from another term of its
//! shape (ADR-10723 Stage 2, #15780): `is qq:to/E/, "@l[]\n"` writes a
//! string of the body's shape before the body, and the ranges noted for the
//! one went to the other.

use super::{RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// `qq:to/E/, "@l[]\n"`: when `node`, a heredoc's body written at
/// `written`, has a range outside it, a string of the same shape took the
/// body's ranges; they are all dropped, to be placed again from `written`.
// Cost: O(n), n = nodes under `node`.
pub(super) fn drop_borrowed(node: &mut RakuAstNode, written: crate::ast::SrcSpan) {
    if super::infix_origins::byte_range(node)
        .is_some_and(|(from, to)| from < written.from as usize || to > written.to as usize)
    {
        clear(node);
    }
}

/// Clear the origins of `node` and of every node under it.
// Cost: O(n), n = nodes under `node`.
fn clear(node: &mut RakuAstNode) {
    node.origin.0 = None;
    for field in node.fields.iter_mut() {
        match &mut field.value {
            RakuAstFieldValue::Node(v) => clear_value(v),
            RakuAstFieldValue::List(items) => items.iter_mut().for_each(clear_value),
            _ => {}
        }
    }
}

// Cost: O(n), n = nodes under `value`.
fn clear_value(value: &mut Value) {
    let ValueView::RakuAst(held) = value.view() else {
        return;
    };
    let mut held = RakuAstNode::clone(held);
    clear(&mut held);
    *value = Value::rakuast(Box::new(held));
}
