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
//! **The table cannot grow without bound.** It is keyed by `K` alone, and each
//! key holds at most TWO answers: the one for the generation it was last
//! computed under, and the one before that. An entry computed under a third
//! generation evicts the older of the two rather than accumulating on top, so
//! the size is bounded by the number of distinct keys, exactly as it was
//! before, not by the number of generations.
//!
//! Two, not one, because the excursion above is exactly two alternating
//! generations, and the same key is routinely probed under both: `return` is
//! called from the routine that declares the inner sub *and* from its caller,
//! so a one-slot entry recomputed on every switch — twice per call of the
//! routine (#9073). A key probed under at most two alternating generations
//! keeps both answers for the life of the program.
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
    entries: FxHashMap<K, Slots<V>>,
}

/// The (at most two) answers one key holds: `recent` is the one written last,
/// `older` the one it displaced, if that was for a different generation.
#[derive(Clone)]
struct Slots<V> {
    recent: (u64, V),
    older: Option<(u64, V)>,
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
        let answer = found.and_then(|slots| {
            if slots.recent.0 == generation {
                Some(&slots.recent.1)
            } else {
                match &slots.older {
                    Some((stamp, value)) if *stamp == generation => Some(value),
                    _ => None,
                }
            }
        });
        crate::vm::vm_stats::record_gen_cache_probe(answer.is_some(), found.is_some());
        answer
    }

    /// Record `value` as the answer for `key` under `generation`. The key keeps
    /// the answer it last held for another generation as its second slot and
    /// drops anything older.
    #[inline]
    pub(crate) fn insert(&mut self, generation: u64, key: K, value: V) {
        use std::collections::hash_map::Entry;
        match self.entries.entry(key) {
            Entry::Vacant(entry) => {
                entry.insert(Slots {
                    recent: (generation, value),
                    older: None,
                });
            }
            Entry::Occupied(mut entry) => {
                let slots = entry.get_mut();
                if slots.recent.0 == generation {
                    slots.recent.1 = value;
                } else {
                    let displaced = std::mem::replace(&mut slots.recent, (generation, value));
                    slots.older = Some(displaced);
                }
            }
        }
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

    /// Both sides of an excursion keep their answer for a key probed on both
    /// sides (#9073), and a third generation evicts the older of the two.
    #[test]
    fn a_key_keeps_its_answers_for_two_alternating_generations() {
        let mut cache: GenCache<&str, u32> = GenCache::default();
        cache.insert(7, "return", 1);
        cache.insert(8, "return", 2);
        assert_eq!(cache.get(7, &"return"), Some(&1));
        assert_eq!(cache.get(8, &"return"), Some(&2));
        // Rewriting the recent generation's answer keeps the older one.
        cache.insert(8, "return", 3);
        assert_eq!(cache.get(7, &"return"), Some(&1));
        assert_eq!(cache.get(8, &"return"), Some(&3));
        // A third generation displaces 8 into the older slot and drops 7.
        cache.insert(9, "return", 4);
        assert_eq!(cache.get(7, &"return"), None);
        assert_eq!(cache.get(8, &"return"), Some(&3));
        assert_eq!(cache.get(9, &"return"), Some(&4));
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
