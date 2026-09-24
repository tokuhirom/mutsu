//! Per-site inline caches for attribute access (ADR-0121 D3).
//!
//! An attribute access site (a `$!x` / `$.x` local slot of a method body)
//! resolves the same attribute on every execution. Once instances are laid
//! out by a [`ClassLayout`](super::ClassLayout), what that resolution produces
//! for a given layout is a fixed slot index, so the site remembers
//! `(layout id, slot)` and a later access on an instance of the same layout
//! goes straight to the slot.
//!
//! One cache word per site, packed as `layout_id << 32 | slot`. Layout ids
//! start at 1, so 0 means empty. A site that sees several layouts (a method
//! inherited by several classes) keeps only the last one: a miss re-resolves
//! and refills, which is correct, just not faster.

use std::sync::OnceLock;
use std::sync::atomic::{AtomicU64, Ordering};

/// One inline cache word per local slot of a chunk.
#[derive(Debug, Default)]
pub(crate) struct AttrSiteCaches(OnceLock<Box<[AtomicU64]>>);

impl Clone for AttrSiteCaches {
    /// A cloned chunk starts cold: the cache is an optimization, not state.
    fn clone(&self) -> Self {
        Self::default()
    }
}

impl AttrSiteCaches {
    fn words(&self, sites: usize) -> &[AtomicU64] {
        self.0
            .get_or_init(|| (0..sites).map(|_| AtomicU64::new(0)).collect())
    }

    /// The `(layout id, slot)` site `idx` last resolved to, if any. `sites` is
    /// the chunk's number of local slots. The caller checks the layout id
    /// against the instance it holds.
    // Cost: O(1), one atomic load.
    #[inline]
    pub(crate) fn cached(&self, sites: usize, idx: usize) -> Option<(u32, usize)> {
        let word = self.words(sites).get(idx)?.load(Ordering::Relaxed);
        (word != 0).then_some(((word >> 32) as u32, word as u32 as usize))
    }

    /// Remember that site `idx` resolves to `slot` on layout `layout_id`.
    // Cost: O(1), one atomic store.
    #[inline]
    pub(crate) fn fill(&self, sites: usize, idx: usize, layout_id: u32, slot: usize) {
        let Ok(slot) = u32::try_from(slot) else {
            return;
        };
        if let Some(word) = self.words(sites).get(idx) {
            word.store(
                (u64::from(layout_id) << 32) | u64::from(slot),
                Ordering::Relaxed,
            );
        }
    }
}
