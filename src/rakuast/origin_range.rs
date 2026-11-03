//! The character range a node's origin records.

use super::RakuAstNode;
use crate::value::ValueView;

/// `(from, to)` in characters of the unit's source, for a node the parser
/// placed.
// Cost: O(1).
pub(crate) fn of(node: &RakuAstNode) -> Option<(i64, i64)> {
    let origin = node.origin.0.as_ref()?;
    let ValueView::Instance { attributes, .. } = origin.view() else {
        return None;
    };
    let map = attributes.as_map();
    let at = |name: &str| match map.get(name)?.view() {
        ValueView::Int(n) => Some(n),
        _ => None,
    };
    Some((at("from")?, at("to")?))
}

/// [`of`] for a node held as a value (a subclass instance or a mixin
/// answers through the native node it carries).
// Cost: O(1).
pub(crate) fn of_value(value: &crate::value::Value) -> Option<(i64, i64)> {
    match value.view() {
        ValueView::RakuAst(node) => of(node),
        _ => None,
    }
}
