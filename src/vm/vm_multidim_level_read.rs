//! One level of a multi-dimensional subscript read (`@a[1;1]`, `%h{"a";"b"}`),
//! taken under the ADR-0068 container-structure stripe.
//!
//! The multi-dim reader used to descend by BORROWING the live element
//! (`&items[i]`) and recursing with that reference. Another thread's element
//! store mutates the same node in place (`gc_contents_mut`): an atomic `cas`
//! on `@a[1;1]` promotes the row `@a[1]` to a `ContainerRef` cell and then
//! re-seats the cell's row, dropping the old one. A reader still holding the
//! borrowed slot read a word that was being overwritten and walked a row whose
//! node had been freed -- `Cannot unbox a type object (Nil) to int`, a garbage
//! `Num`, or a wrong element (#17781). The single-dimension read already takes
//! the node's stripe (`vm_var_index_ops`); these helpers give the multi-dim
//! walk the same edge, and hand the next level an OWNED clone (a refcount), so
//! nothing the recursion touches can be freed under it. The stripe is held
//! only around the clone: the recursion itself may run user code (a
//! `WhateverCode` dimension), which must never happen under a stripe.

use super::*;
use crate::value::ValueMap;
use crate::value::container_lock::ContainerStructGuard;

/// What one positional level of a multi-dim read sees.
pub(super) struct LevelElem {
    /// The level's length when the element was read.
    pub(super) len: usize,
    /// The element, cloned under the stripe; `None` past the end.
    pub(super) elem: Option<Value>,
    /// Whether the level is a real `Array` (a miss reads `Any`), not a `List`.
    pub(super) is_real: bool,
}

impl Interpreter {
    /// Element `i` of the array `target`, cloned under its node's stripe.
    /// `None` when `target` is not an array.
    // Cost: O(1).
    pub(super) fn multidim_level_elem(target: &Value, i: usize) -> Option<LevelElem> {
        let ValueView::Array(items, kind) = target.view() else {
            return None;
        };
        let _structure = ContainerStructGuard::acquire_for(None, target);
        Some(LevelElem {
            len: items.len(),
            elem: items.get(i).cloned(),
            is_real: kind.is_real_array(),
        })
    }

    /// Every element of the array `target`, cloned under its node's stripe
    /// (a `*` dimension). `None` when `target` is not an array.
    // Cost: O(n), n = the level's length.
    pub(super) fn multidim_level_items(target: &Value) -> Option<Vec<Value>> {
        let ValueView::Array(items, ..) = target.view() else {
            return None;
        };
        let _structure = ContainerStructGuard::acquire_for(None, target);
        Some(items.to_vec())
    }

    /// The value stored under `key` in the hash `map` (falling back to the
    /// object-hash `.WHICH` spelling), cloned under the stripe of `owner`, the
    /// hash value `map` belongs to. `owner` is `None` for a map no other
    /// thread can reach (the one-entry map a Pair level builds).
    // Cost: O(1) expected, plus the key's stringification.
    pub(super) fn multidim_hash_lookup(
        owner: Option<&Value>,
        map: &ValueMap,
        key: &Value,
    ) -> Option<Value> {
        let primary = key.to_string_value();
        let _structure = owner.and_then(|o| ContainerStructGuard::acquire_for(None, o));
        map.get(primary.as_str())
            .or_else(|| map.get(crate::runtime::utils::value_which_key(key).as_str()))
            .cloned()
    }

    /// Every value of the hash `map`, cloned under the stripe of `owner` (see
    /// [`Self::multidim_hash_lookup`]).
    // Cost: O(n), n = the number of entries.
    pub(super) fn multidim_hash_values(owner: Option<&Value>, map: &ValueMap) -> Vec<Value> {
        let _structure = owner.and_then(|o| ContainerStructGuard::acquire_for(None, o));
        map.values().cloned().collect()
    }
}
