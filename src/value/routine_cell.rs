//! A routine's composition cell (ADR-11827).
//!
//! In rakudo a `Routine` is an object, and `$r does R` reblesses it in place:
//! every alias, every role argument that captured it and the registry entry
//! see `R`. mutsu's `does` builds a `Value::Mixin` around the code object, so
//! the composition has to live somewhere every value of the same routine
//! shares. That is this cell: one per routine, held by every `SubData` of it
//! (and by a named routine's `FunctionDef`, which rebuilds share), written by
//! `does` and read wherever a routine is dispatched on or role-checked.
//!
//! The cell is shared across Raku threads, so it is behind a lock; a routine
//! never mixed into pays one relaxed atomic load (`composed`).

use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};

use super::MixinOverrides;
use crate::gc::Gc;

#[derive(Default)]
struct Inner {
    /// Set once a composition is stored; never cleared.
    composed: AtomicBool,
    overrides: RwLock<Option<Gc<MixinOverrides>>>,
}

/// See the module docs. Cloning shares the cell: a clone of the handle is the
/// same routine. A Raku-level `.clone` of a routine takes [`Self::forked`].
#[derive(Clone, Default)]
pub(crate) struct RoutineCell(Arc<Inner>);

impl std::fmt::Debug for RoutineCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("RoutineCell")
    }
}

impl RoutineCell {
    /// The roles composed into the routine so far, if any.
    // Cost: O(1); one atomic load when nothing was composed.
    pub(crate) fn get(&self) -> Option<Gc<MixinOverrides>> {
        if !self.0.composed.load(Ordering::Acquire) {
            return None;
        }
        self.0.overrides.read().ok().and_then(|g| g.clone())
    }

    /// Record `overrides` as the routine's composition.
    // Cost: O(1).
    pub(crate) fn set(&self, overrides: Gc<MixinOverrides>) {
        if let Ok(mut slot) = self.0.overrides.write() {
            *slot = Some(overrides);
        }
        self.0.composed.store(true, Ordering::Release);
    }

    /// A new, unshared cell starting from this one's composition: what a
    /// Raku-level `.clone` of the routine gets (ADR-11827 §2.3).
    // Cost: O(1).
    pub(crate) fn forked(&self) -> Self {
        let fresh = Self::default();
        if let Some(overrides) = self.get() {
            fresh.set(overrides);
        }
        fresh
    }

    /// Whether two handles are the same routine's cell.
    // Cost: O(1).
    pub(crate) fn same(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}
