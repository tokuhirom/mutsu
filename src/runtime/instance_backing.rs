//! Which native value backs an `Instance` receiver (ADR-11276 §9.68).
//!
//! A user class that inherits a builtin container (`is Array`, `is Hash`,
//! `is BagHash`, ...) keeps the real data in a reserved attribute of the
//! instance and delegates the protocol methods to it. Whoever dispatches on
//! such an instance has to ask the same question -- "is there a native value
//! behind this receiver, and which one?" -- so the attribute names live here
//! once, instead of in each bridge and in every row that must not answer a
//! receiver the bridge resolves further.

use crate::value::InstanceAttrs;

/// The native storage an instance delegates to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum InstanceBacking {
    /// `is Array` / `is List`: `__mutsu_array_storage`.
    Array,
    /// `is Hash` / `is Map`: `__mutsu_hash_storage`.
    Hash,
    /// `is Set` / `is Bag` / `is Mix` and their mutable forms: `__baggy_data__`.
    Baggy,
}

impl InstanceBacking {
    /// The reserved attribute holding the storage value.
    // Cost: O(1).
    pub(crate) const fn attr(self) -> &'static str {
        match self {
            Self::Array => "__mutsu_array_storage",
            Self::Hash => "__mutsu_hash_storage",
            Self::Baggy => "__baggy_data__",
        }
    }

    /// Whether `attributes` carry this backing.
    // Cost: O(1), one attribute-map probe.
    pub(crate) fn is_in(self, attributes: &InstanceAttrs) -> bool {
        attributes.contains_key(self.attr())
    }
}
