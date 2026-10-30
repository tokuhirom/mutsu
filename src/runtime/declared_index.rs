//! The declared-type relation of one candidate family, built once.
//!
//! A candidate that cannot bind a call's arguments is ordered against the
//! others by its declared types alone ([`MultiRank::declared_narrower`](super::multi_rank::MultiRank::declared_narrower)), a
//! function of the two candidates' [`DeclaredKey`](super::multi_rank::DeclaredKey)s. A method family like
//! `RakuAST::Deparse`'s 267 candidates asks that for all 71k pairs on every
//! dispatch, though the answer never depends on the arguments. The index holds,
//! for each candidate of the family in a canonical (sorted-by-key-id) order,
//! the candidates narrower than it, so a dispatch reads rows instead of
//! comparing pairs.

use super::multi_rank::MultiRank;
use crate::symbol::Symbol;
use std::sync::Arc;

/// For each canonical position `p`, the canonical positions `q` with
/// `declared_narrower(q, p)`.
pub(crate) struct DeclaredIndex {
    pub(crate) narrower_than: Vec<Vec<u32>>,
}

/// Most families remembered before the oldest is dropped.
const FAMILIES_CAP: usize = 8;

#[derive(Default)]
pub(crate) struct DeclaredIndexCache {
    families: Vec<(Vec<u32>, Arc<DeclaredIndex>)>,
}

impl DeclaredIndexCache {
    /// The index of the family whose sorted key ids are `sorted_ids`, built
    /// from `ranks` (in the same canonical order) on a miss.
    // Cost: O(n) on a hit (comparing the ids), O(n^2 * r) on a miss, n =
    // candidates, r = one `conforms` call.
    pub(crate) fn get(&self, sorted_ids: &[u32]) -> Option<Arc<DeclaredIndex>> {
        self.families
            .iter()
            .find(|(ids, _)| ids == sorted_ids)
            .map(|(_, index)| Arc::clone(index))
    }

    // Cost: O(1).
    pub(crate) fn insert(&mut self, sorted_ids: Vec<u32>, index: Arc<DeclaredIndex>) {
        if self.families.len() >= FAMILIES_CAP {
            self.families.remove(0);
        }
        self.families.push((sorted_ids, index));
    }

    // Cost: O(1).
    pub(crate) fn clear(&mut self) {
        self.families.clear();
    }
}

/// Build the index over `canonical` (the family's ranks in canonical order).
// Cost: O(n^2 * r), n = candidates, r = one `conforms` call.
pub(crate) fn build(
    canonical: &[&MultiRank],
    conforms: &dyn Fn(Symbol, Symbol) -> bool,
) -> DeclaredIndex {
    let n = canonical.len();
    let narrower_than = (0..n)
        .map(|p| {
            (0..n)
                .filter(|&q| q != p && canonical[q].declared_narrower(canonical[p], conforms))
                .map(|q| q as u32)
                .collect()
        })
        .collect();
    DeclaredIndex { narrower_than }
}

/// The family's items in canonical order (`order[c]` is the item at canonical
/// position `c`) and each item's canonical position (`pos_of[item]`).
// Cost: O(n log n).
pub(crate) fn canonical_order(ids: &[u32]) -> (Vec<usize>, Vec<usize>) {
    let mut order: Vec<usize> = (0..ids.len()).collect();
    order.sort_by_key(|&i| ids[i]);
    let mut pos_of = vec![0usize; ids.len()];
    for (c, &i) in order.iter().enumerate() {
        pos_of[i] = c;
    }
    (order, pos_of)
}

/// The items narrower than `item`, read from `index`.
// Cost: O(row length).
pub(crate) fn preds_of(
    index: &DeclaredIndex,
    order: &[usize],
    pos_of: &[usize],
    item: usize,
) -> Vec<usize> {
    index.narrower_than[pos_of[item]]
        .iter()
        .map(|&q| order[q as usize])
        .collect()
}
