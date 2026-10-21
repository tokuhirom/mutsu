//! The `for` loop writeback into a multi-dimensional shaped array (#14248).
//!
//! Such an array iterates its leaves, so the loop's index names a leaf, not
//! a first-dimension row: a mutated topic is written back into that leaf.

use super::*;

impl Interpreter {
    /// The writeback of loop variable `topic` into the `idx`-th leaf of the
    /// multi-dimensional shaped array `source` (a `ContainerRef` cell is read
    /// through). `None` when `source` is not such an array, so the caller's
    /// row writeback applies; `Some(None)` when the leaf is unchanged;
    /// otherwise `Some(Some(updated))`, the array with that leaf replaced and
    /// every level's metadata kept.
    // Cost: O(d) to read the leaf, d = dimensions; O(d * w) to rebuild the
    // path when it changed, w = the widest level's length.
    pub(super) fn shaped_leaf_writeback(
        source: Option<&Value>,
        idx: usize,
        topic: &Value,
    ) -> Option<Option<Value>> {
        let source = source?.deref_container();
        let leaf = crate::value::shaped_array::shaped_array_leaf_at(&source, idx)?;
        if Self::loop_var_unchanged(topic, &leaf) {
            return Some(None);
        }
        Some(crate::value::shaped_array::with_shaped_array_leaf(
            &source,
            idx,
            topic.clone(),
        ))
    }
}
