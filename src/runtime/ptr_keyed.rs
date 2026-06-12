//! Weak-guarded pointer-keyed side tables for container metadata.
//!
//! Several runtime side tables associate metadata with a container value
//! (typed Array/Hash/Set/Bag/Mix metadata, `is default(...)` element
//! defaults, object-hash original keys, shaped-array dimensions, grep
//! rw-view bindings) keyed by the container's `Arc` data pointer. A bare
//! `usize` key is unsound: once every strong reference to the container is
//! dropped, the allocator may place a brand-new container at the same
//! address, and the new container then silently inherits the dead one's
//! metadata. That is the root of the intermittent typed-container failures
//! (allocation-order dependent, e.g. `S02-names-vars/perl.t`).
//!
//! `PtrKeyedMap` closes the hole structurally by storing a `Weak<T>` guard
//! next to each entry:
//!
//! - The `Weak` keeps the `ArcInner` allocation alive (the payload itself is
//!   still dropped with the last strong reference), so **no live `Arc` can
//!   ever occupy a dead entry's address** — stale inheritance becomes
//!   impossible by construction, not by defensive clearing.
//! - Lookups additionally validate `strong_count() > 0`, so an entry whose
//!   container died reads as absent.
//! - Inserts amortize a sweep of dead entries, bounding both the map size
//!   and the amount of pinned (allocation-header) memory.
//!
//! Note: holding a `Weak` makes `Arc::make_mut` relocate the payload even
//! when the strong count is 1. That converts "uniquely-owned in-place
//! mutation" into the already-handled shared-COW class (metadata moves are
//! healed by the existing reattach/migrate/reconcile helpers on mutation
//! paths), at the cost of a fresh allocation for guarded containers only —
//! unguarded (untyped) containers never enter these maps and are unaffected.

use std::collections::HashMap;
use std::sync::{Arc, Weak};

/// Sweep threshold floor: maps smaller than this are never swept.
const MIN_SWEEP: usize = 64;

/// The pointer key for a container's data `Arc`.
pub(crate) fn ptr_key<T>(arc: &Arc<T>) -> usize {
    Arc::as_ptr(arc) as usize
}

/// A map from container identity (`Arc` data pointer) to metadata, guarded
/// against pointer reuse by a `Weak` reference per entry.
pub(crate) struct PtrKeyedMap<T, V> {
    entries: HashMap<usize, (Weak<T>, V)>,
    /// Next size at which an insert triggers a dead-entry sweep.
    sweep_at: usize,
}

impl<T, V> Default for PtrKeyedMap<T, V> {
    fn default() -> Self {
        PtrKeyedMap {
            entries: HashMap::new(),
            sweep_at: MIN_SWEEP,
        }
    }
}

// Manual impl: `Weak<T>` is `Clone` for any `T`, so no `T: Clone` bound.
impl<T, V: Clone> Clone for PtrKeyedMap<T, V> {
    fn clone(&self) -> Self {
        PtrKeyedMap {
            entries: self.entries.clone(),
            sweep_at: self.sweep_at,
        }
    }
}

impl<T, V> PtrKeyedMap<T, V> {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    /// Insert metadata for the container behind `arc`, replacing any
    /// previous entry for the same container.
    pub(crate) fn insert(&mut self, arc: &Arc<T>, value: V) {
        self.maybe_sweep();
        self.entries
            .insert(ptr_key(arc), (Arc::downgrade(arc), value));
    }

    /// Look up metadata by raw pointer key. Entries whose container has
    /// been dropped read as absent.
    pub(crate) fn get(&self, id: usize) -> Option<&V> {
        self.entries
            .get(&id)
            .and_then(|(guard, value)| (guard.strong_count() > 0).then_some(value))
    }

    /// Look up metadata for a live container `Arc`.
    pub(crate) fn get_arc(&self, arc: &Arc<T>) -> Option<&V> {
        self.get(ptr_key(arc))
    }

    /// Remove and return the metadata for `id`, dead or alive. The guard
    /// pins the entry's address, so an entry at `id` is always the container
    /// that registered it — callers that recover metadata from a pre-COW
    /// pointer (whose Arc may already be dropped) rely on this.
    pub(crate) fn remove(&mut self, id: usize) -> Option<V> {
        self.entries.remove(&id).map(|(_, value)| value)
    }

    #[cfg(test)]
    pub(crate) fn raw_len(&self) -> usize {
        self.entries.len()
    }

    /// Drop dead entries once the map has grown past the adaptive
    /// threshold, then set the next threshold to double the live size.
    fn maybe_sweep(&mut self) {
        if self.entries.len() < self.sweep_at {
            return;
        }
        self.entries
            .retain(|_, (guard, _)| guard.strong_count() > 0);
        self.sweep_at = (self.entries.len() * 2).max(MIN_SWEEP);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn live_entry_roundtrip() {
        let mut map: PtrKeyedMap<Vec<i32>, &str> = PtrKeyedMap::new();
        let a = Arc::new(vec![1, 2, 3]);
        map.insert(&a, "typed");
        assert_eq!(map.get_arc(&a), Some(&"typed"));
        assert!(map.get(ptr_key(&a)).is_some());
    }

    #[test]
    fn dead_entry_reads_absent() {
        let mut map: PtrKeyedMap<Vec<i32>, &str> = PtrKeyedMap::new();
        let a = Arc::new(vec![1, 2, 3]);
        let key = ptr_key(&a);
        map.insert(&a, "typed");
        drop(a);
        assert_eq!(map.get(key), None);
        // `remove` still recovers the value (pre-COW migration relies on it).
        assert_eq!(map.remove(key), Some("typed"));
        assert_eq!(map.remove(key), None);
    }

    /// The flaky-killer invariant: after a guarded container dies, no
    /// freshly allocated container can ever observe its metadata — the
    /// `Weak` guard pins the dead entry's address, so a new `Arc` can never
    /// collide with it. Without the guard this loop hits a stale entry
    /// almost immediately (the allocator reuses the freed block).
    #[test]
    fn no_stale_inheritance_after_drop() {
        let mut map: PtrKeyedMap<Vec<i32>, &str> = PtrKeyedMap::new();
        let a = Arc::new(vec![1, 2, 3]);
        map.insert(&a, "stale");
        drop(a);
        for _ in 0..10_000 {
            let fresh = Arc::new(vec![1, 2, 3]);
            assert_eq!(
                map.get_arc(&fresh),
                None,
                "fresh container inherited dead metadata"
            );
        }
    }

    #[test]
    fn sweep_drops_dead_entries() {
        let mut map: PtrKeyedMap<Vec<i32>, usize> = PtrKeyedMap::new();
        // Fill past the sweep threshold with dead entries.
        for i in 0..MIN_SWEEP {
            let a = Arc::new(vec![i as i32]);
            map.insert(&a, i);
        }
        assert_eq!(map.raw_len(), MIN_SWEEP);
        // All previous arcs are dropped; the next insert sweeps them.
        let keeper = Arc::new(vec![-1]);
        map.insert(&keeper, 9999);
        assert_eq!(map.raw_len(), 1);
        assert_eq!(map.get_arc(&keeper), Some(&9999));
    }

    #[test]
    fn reinsert_replaces_value() {
        let mut map: PtrKeyedMap<Vec<i32>, &str> = PtrKeyedMap::new();
        let a = Arc::new(vec![1]);
        map.insert(&a, "first");
        map.insert(&a, "second");
        assert_eq!(map.get_arc(&a), Some(&"second"));
    }
}
