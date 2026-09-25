//! The deduplicated by-name write log of a flattened frame env
//! (`Env::frame_writes`, see `Env::flattened_for_frame`).
//!
//! The log is seeded with every key of the frame tier that was collapsed, so
//! for a frame with many lexicals it holds that many names. It is read as a
//! set, and every logged write first asks "is this name already here?". A
//! plain `Vec` answered that with a linear scan -- once per by-name write, so
//! a `map`/`grep` callback running inside a frame of L lexicals paid O(L) per
//! iteration for binding its topic alone (#9173). Past a small size the log
//! therefore carries a hash index beside the list; below it the scan over a
//! handful of `Symbol`s stays the cheapest check.

use crate::symbol::Symbol;
use rustc_hash::FxHashSet;

/// Length above which the membership index is kept.
const INDEX_AT: usize = 16;

#[derive(Clone, Debug, Default)]
pub(crate) struct FrameWriteLog {
    list: Vec<Symbol>,
    index: Option<FxHashSet<Symbol>>,
}

impl FrameWriteLog {
    /// Build a log from `keys`, dropping repeats.
    // Cost: O(n), n = keys.
    pub(crate) fn from_keys(keys: impl IntoIterator<Item = Symbol>) -> Self {
        let mut log = Self::default();
        for k in keys {
            if !log.contains(k) {
                log.push(k);
            }
        }
        log
    }

    #[inline]
    pub(crate) fn as_slice(&self) -> &[Symbol] {
        &self.list
    }

    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.list.len()
    }

    #[inline]
    pub(crate) fn get(&self, i: usize) -> Symbol {
        self.list[i]
    }

    // Cost: O(1) (hash probe), or O(n <= 16) below the index threshold.
    #[inline]
    pub(crate) fn contains(&self, key: Symbol) -> bool {
        match &self.index {
            Some(set) => set.contains(&key),
            None => self.list.contains(&key),
        }
    }

    /// Append `key`, which the caller has checked is not logged yet.
    // Cost: O(1) amortized (one O(16) index build when the log crosses the threshold).
    pub(crate) fn push(&mut self, key: Symbol) {
        self.list.push(key);
        match &mut self.index {
            Some(set) => {
                set.insert(key);
            }
            None if self.list.len() > INDEX_AT => {
                self.index = Some(self.list.iter().copied().collect());
            }
            None => {}
        }
    }

    /// Remove and return the `i`-th entry; order is not preserved.
    // Cost: O(1).
    pub(crate) fn swap_remove(&mut self, i: usize) -> Symbol {
        let key = self.list.swap_remove(i);
        if let Some(set) = &mut self.index {
            set.remove(&key);
        }
        key
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn membership_across_the_index_threshold() {
        let syms: Vec<Symbol> = (0..40)
            .map(|i| Symbol::intern(&format!("fwl{i}")))
            .collect();
        let mut log = FrameWriteLog::from_keys(syms[..3].iter().copied().chain([syms[0]]));
        assert_eq!(log.len(), 3);
        for &s in &syms[3..] {
            assert!(!log.contains(s));
            log.push(s);
        }
        assert!(syms.iter().all(|&s| log.contains(s)));
        let removed = log.swap_remove(0);
        assert!(!log.contains(removed));
        assert_eq!(log.len(), syms.len() - 1);
    }
}
