//! The receiver attributes a compiled method call binds from (#8880).
//!
//! Only the full binder in [`crate::runtime::Interpreter::call_compiled_method`] reads the
//! receiver's attributes as a map; the fast path reads them cell-direct. A
//! caller holding the receiver's live attribute cell therefore hands over the
//! cell, and the whole-map clone is paid only when the full binder runs, not
//! on every call (it was ~500 instructions of a ~14,000-instruction `$o.m()`).

use std::borrow::Cow;
use std::sync::atomic::{AtomicBool, Ordering};

use super::vm_method_dispatch::ATTR_ALIAS_META_PREFIX;
use crate::gc::Gc;
use crate::value::{AttrMap, InstanceAttrs};

/// Whether any instance has ever been given sigilless-alias metadata (an
/// `__mutsu_attr_alias::` key, written for a twigil-less `has $x` by the two
/// constructors that build such an instance, which call
/// [`note_alias_meta_written`]). Until one is, no attribute map can hold the
/// key, so [`CallAttrs::has_alias_meta`] answers without scanning one. The
/// flag is monotonic and process-wide (an instance can cross threads), so an
/// over-set only ever costs the scan.
static ALIAS_META_SEEN: AtomicBool = AtomicBool::new(false);

/// Record that an instance is about to carry sigilless-alias metadata.
// Cost: O(1).
#[inline]
pub(crate) fn note_alias_meta_written() {
    ALIAS_META_SEEN.store(true, Ordering::Relaxed);
}

/// Where a method call's receiver attributes come from.
#[derive(Clone, Copy)]
pub(crate) enum CallAttrs<'a> {
    /// No attributes (a type-object or non-instance receiver).
    Empty,
    /// An already-materialized map.
    Map(&'a AttrMap),
    /// The receiver's live attribute cell, snapshotted only on demand.
    Cell(&'a Gc<InstanceAttrs>),
}

impl<'a> CallAttrs<'a> {
    /// The receiver's attributes as a map: borrowed when already one, else a
    /// snapshot of the live cell.
    // Cost: O(1) for `Empty`/`Map`; O(a) for `Cell`, a = attribute count.
    pub(crate) fn materialize(self) -> Cow<'a, AttrMap> {
        match self {
            CallAttrs::Empty => Cow::Owned(AttrMap::new()),
            CallAttrs::Map(m) => Cow::Borrowed(m),
            CallAttrs::Cell(cell) => Cow::Owned(cell.to_map()),
        }
    }

    /// Whether any sigilless-attribute alias metadata key is present.
    // Cost: O(1) until any instance carries alias metadata
    // (`ALIAS_META_SEEN`); then O(a), a = attribute count; no allocation.
    pub(crate) fn has_alias_meta(self) -> bool {
        if !ALIAS_META_SEEN.load(Ordering::Relaxed) {
            return false;
        }
        let has = |m: &AttrMap| m.keys().any(|k| k.starts_with(ATTR_ALIAS_META_PREFIX));
        match self {
            CallAttrs::Empty => false,
            CallAttrs::Map(m) => has(m),
            CallAttrs::Cell(cell) => has(&cell.as_map()),
        }
    }
}
