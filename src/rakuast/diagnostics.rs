//! The check-time problems a RakuAST node collects (`RakuAST::Node`'s
//! `sorries`, `worries`, `add-sorry`, `add-worry`, `clear-worries`,
//! `promote-worries-to-sorries`, `has-check-time-problems`), as rakudo
//! 2026.09 answers them: both are lists of exceptions, empty for a fresh
//! node, and a node has problems when either holds one.
//!
//! The model keeps them in the hidden fields [`SORRIES`] and [`WORRIES`],
//! which no walk, render or lowering reads.

use super::{RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::Value;

const SORRIES: &str = "sorries";
const WORRIES: &str = "worries";

/// Whether `field` is one of the hidden problem lists.
// Cost: O(1).
pub(super) fn is_hidden(_node: &RakuAstNode, field: &RakuAstField) -> bool {
    matches!(field.name, Some(SORRIES | WORRIES))
}

fn held(node: &RakuAstNode, name: &str) -> Vec<Value> {
    node.fields
        .iter()
        .find(|f| f.name == Some(name))
        .map_or_else(Vec::new, |f| match &f.value {
            RakuAstFieldValue::List(items) => items.clone(),
            _ => Vec::new(),
        })
}

fn put(node: &mut RakuAstNode, name: &'static str, items: Vec<Value>) {
    node.fields.retain(|f| f.name != Some(name));
    if !items.is_empty() {
        node.fields.push(RakuAstField {
            name: Some(name),
            value: RakuAstFieldValue::List(items),
        });
    }
}

/// `sorries` (`worries`): the exceptions collected so far, in order.
// Cost: O(f + p), f = fields of the node, p = problems held.
pub(crate) fn list(node: &RakuAstNode, worries: bool) -> Value {
    Value::array(held(node, if worries { WORRIES } else { SORRIES }))
}

/// `has-check-time-problems`.
// Cost: O(f).
pub(crate) fn has_problems(node: &RakuAstNode) -> bool {
    !held(node, SORRIES).is_empty() || !held(node, WORRIES).is_empty()
}

/// `add-sorry($ex)` / `add-worry($ex)`.
// Cost: O(f + p).
pub(crate) fn add(node: &mut RakuAstNode, worry: bool, ex: &Value) -> Value {
    let name = if worry { WORRIES } else { SORRIES };
    let mut items = held(node, name);
    items.push(ex.clone());
    put(node, name, items);
    Value::NIL
}

/// `clear-worries`.
// Cost: O(f).
pub(crate) fn clear_worries(node: &mut RakuAstNode) -> Value {
    put(node, WORRIES, Vec::new());
    Value::NIL
}

/// `promote-worries-to-sorries`: the worries join the sorries, in order.
// Cost: O(f + p).
pub(crate) fn promote(node: &mut RakuAstNode) -> Value {
    let mut sorries = held(node, SORRIES);
    sorries.extend(held(node, WORRIES));
    put(node, SORRIES, sorries);
    put(node, WORRIES, Vec::new());
    Value::NIL
}
