//! A memo whose entries are tagged with the registry generation they were
//! computed under, instead of the whole table being thrown away when that
//! generation moves.
//!
//! Every name-keyed dispatch memo answers a question about
//! [`Registry::functions`](crate::runtime::registry::Registry::functions), so
//! it has to be invalidated when that map changes. The cheap way to do that is
//! one generation stamp per *table*: compare it on the way in, `clear()` on a
//! mismatch. That is correct, and it is what these caches used to do — but it
//! means an entry cannot outlive a single generation, which is fatal once the
//! generation moves and comes *back*:
//!
//! ```text
//!   version V0   the program's steady state; memos fill up here
//!   version V1   a routine-local `my sub` is installed on entry to a routine
//!                -> one probe inside the routine wipes every V0 entry
//!   version V0   the routine returns, the scope restore puts the map back
//!                -> one probe wipes every V1 entry, and V0 must be rebuilt
//! ```
//!
//! With `FunctionTable`'s content-identifying version
//! ([`crate::runtime::function_table`]) the two states are genuinely
//! distinguishable and both recur, so tagging each *entry* lets both sets
//! survive: a V0 answer stays in the table, dormant, while the routine runs,
//! and is live again the moment the map is restored. That is what turns a
//! per-call rebuild of six caches into a per-call lookup
//! ([#8314](https://github.com/tokuhirom/mutsu/issues/8314)).
//!
//! **The table cannot grow without bound.** It is keyed by `K` alone, so an
//! entry computed under a new generation replaces — rather than accumulates on
//! top of — the same key's older answer. The size is bounded by the number of
//! distinct keys, exactly as it was before, not by the number of generations.
//! The cost of that choice is that a key probed under two alternating
//! generations recomputes on each switch; a key probed under only one keeps its
//! answer for the life of the program.
//!
//! **A stale tag can never be served.** Generations come from a monotonic
//! process-global counter, so a tag names one map state for the life of the
//! process; an entry is returned only when its tag equals the generation the
//! caller asks with.

use rustc_hash::FxHashMap;
use std::hash::Hash;

/// A [`FxHashMap`] memo whose entries carry the generation they were computed
/// under. See the module docs.
#[derive(Clone)]
pub(crate) struct GenCache<K, V> {
    entries: FxHashMap<K, (u64, V)>,
}

impl<K, V> Default for GenCache<K, V> {
    fn default() -> Self {
        Self {
            entries: FxHashMap::default(),
        }
    }
}

impl<K: Eq + Hash, V> GenCache<K, V> {
    /// The memoized answer for `key` as of `generation`, or `None` when there is
    /// none or the one stored was computed under a different map.
    #[inline]
    pub(crate) fn get(&self, generation: u64, key: &K) -> Option<&V> {
        let found = self.entries.get(key);
        let answer = match found {
            Some((stamp, value)) if *stamp == generation => Some(value),
            _ => None,
        };
        crate::vm::vm_stats::record_gen_cache_probe(answer.is_some(), found.is_some());
        answer
    }

    /// Record `value` as the answer for `key` under `generation`, replacing any
    /// answer this key carried for another generation.
    #[inline]
    pub(crate) fn insert(&mut self, generation: u64, key: K, value: V) {
        self.entries.insert(key, (generation, value));
    }

    /// Drop every entry, whatever its generation.
    ///
    /// Needed where the thing that changed is *not* the functions map — a
    /// wrapped routine, an import scope popping, a proto marker moving — so
    /// there is no version movement that could retire these answers on its own.
    /// See `Interpreter::invalidate_fn_resolution`.
    #[inline]
    pub(crate) fn clear(&mut self) {
        self.entries.clear();
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.entries.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_entry_is_served_only_under_its_own_generation() {
        let mut cache: GenCache<&str, u32> = GenCache::default();
        cache.insert(7, "f", 1);
        assert_eq!(cache.get(7, &"f"), Some(&1));
        assert_eq!(cache.get(8, &"f"), None);
    }

    /// The property the rewrite exists for: an answer computed before a scope
    /// excursion is still there when the excursion puts the map back.
    #[test]
    fn an_entry_survives_an_excursion_and_comes_back() {
        let mut cache: GenCache<&str, u32> = GenCache::default();
        cache.insert(7, "outer", 1);
        // A routine installs a lexical sub: a different map, a different
        // generation, and its own probes.
        cache.insert(8, "inner", 2);
        assert_eq!(cache.get(8, &"outer"), None);
        // The routine returns; the scope restore puts the very same map back,
        // so the generation is 7 again.
        assert_eq!(cache.get(7, &"outer"), Some(&1));
        assert_eq!(cache.get(7, &"inner"), None);
    }

    #[test]
    fn a_key_holds_one_entry_however_many_generations_probe_it() {
        let mut cache: GenCache<&str, u32> = GenCache::default();
        for generation in 0..1000 {
            cache.insert(generation, "f", generation as u32);
        }
        assert_eq!(cache.len(), 1);
    }

    #[test]
    fn clear_drops_entries_of_every_generation() {
        let mut cache: GenCache<&str, u32> = GenCache::default();
        cache.insert(7, "a", 1);
        cache.insert(8, "b", 2);
        cache.clear();
        assert_eq!(cache.get(7, &"a"), None);
        assert_eq!(cache.get(8, &"b"), None);
    }
}
