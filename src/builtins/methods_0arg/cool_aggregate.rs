//! Cool numeric methods on the Cool aggregates.

use crate::value::{Value, ValueView};

/// The element count a Cool aggregate (List/Array, Map/Hash) numifies to for
/// Cool's numeric methods (`{a => 1, b => 2}.round` is 2), or `None` for any
/// other value.
// Cost: O(1).
pub(crate) fn cool_aggregate_elems(target: &Value) -> Option<i64> {
    match target.view() {
        ValueView::Array(..) => target.as_list_items().map(|items| items.len() as i64),
        ValueView::Hash(map) => Some(map.len() as i64),
        _ => None,
    }
}
