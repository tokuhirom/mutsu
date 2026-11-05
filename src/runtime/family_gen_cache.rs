//! A multi family's own generation, and the memo that is validated by it
//! (ADR-12529 §2.4).
//!
//! The name-keyed dispatch memos are tagged with `fn_resolve_gen`, the version
//! of the whole functions map ([`super::gen_cache`]). That generation moves when
//! *any* routine is registered or goes out of scope, so a block that declares a
//! routine of its own retires every memo of every other name on each entry and
//! exit:
//!
//! ```text
//!   multi f(Int $x) { ... }
//!   for ^N { { multi g(Int $z) { ... }; f($i) + g(1) } }
//! ```
//!
//! Each iteration installs `g` (a map version never seen before) and removes it
//! again, and `f`'s multi caches -- its candidate probe, its winner, its light
//! call -- were rebuilt from a full candidate walk every iteration, although no
//! candidate of `f` ever changed.
//!
//! A multi family's answers depend on the registry keys of its own base name
//! only (the candidates of `f`, in every package the bare name may reach), so
//! each base name carries its own generation: [`FamilyGens`] bumps it whenever
//! a key of that base name is inserted or removed, which is exactly when
//! `invalidate_fn_resolution_for_keys` evicts the base name from the key index.
//! That eviction is already the contract every functions-map write keeps, and
//! the debug-only audit of the key index (`runtime::fn_keys_index`) turns a
//! write that misses it into a located panic. Anything that is not a key change
//! (a `wrap`, an import scope popping) goes through
//! `invalidate_fn_resolution`, which clears these memos outright.
//!
//! [`FamilyGenCache`] keeps the map generation as its fast path: an entry
//! tagged with the running generation is served with no family lookup, so a
//! steady-state program pays exactly what [`super::gen_cache::GenCache`]
//! costs. Only when the map generation moved does a probe read the family's
//! generation, and an entry whose family is unchanged is re-tagged with the
//! running map generation and served.

use rustc_hash::FxHashMap;
use std::hash::Hash;

use crate::symbol::Symbol;

/// The per-base-name generations of the functions map's keys. A base name no
/// key change has touched yet is at generation 0.
#[derive(Default)]
pub(crate) struct FamilyGens {
    gens: FxHashMap<Symbol, u64>,
    next: u64,
}

impl FamilyGens {
    /// Give `base` a generation it has never had: one of its keys changed.
    // Cost: O(1) expected.
    #[inline]
    pub(crate) fn bump(&mut self, base: Symbol) {
        self.next += 1;
        self.gens.insert(base, self.next);
    }

    /// The generation of the family a memo for `name` describes. `name` is the
    /// callsite spelling a memo is keyed by (bare or package-qualified).
    // Cost: O(n) in the length of `name` for a qualified one (the base-name
    // scan), O(1) expected for a bare one.
    pub(crate) fn of_name(&self, name: Symbol) -> u64 {
        let text = name.as_str();
        let base = super::dispatch_resolve::function_key_base_name(text);
        let base_sym = if base.len() == text.len() {
            name
        } else {
            match Symbol::lookup(base) {
                Some(sym) => sym,
                // A base name never interned was never bumped either.
                None => return 0,
            }
        };
        self.gens.get(&base_sym).copied().unwrap_or(0)
    }
}

/// A memo entry: the map generation it was last confirmed under, the family
/// generation it was computed under, and the answer.
struct Tagged<V> {
    generation: u64,
    family: u64,
    value: V,
}

/// The (at most two) answers one key holds, as in `GenCache`: two alternating
/// families (a routine-local candidate of the same name entering and leaving)
/// keep both.
struct Slots<V> {
    recent: Tagged<V>,
    older: Option<Tagged<V>>,
}

/// A memo validated by the map generation first and the family generation
/// second. See the module docs.
pub(crate) struct FamilyGenCache<K, V> {
    entries: FxHashMap<K, Slots<V>>,
}

impl<K, V> Default for FamilyGenCache<K, V> {
    fn default() -> Self {
        Self {
            entries: FxHashMap::default(),
        }
    }
}

impl<K: Eq + Hash, V> FamilyGenCache<K, V> {
    /// The answer for `key` as of map generation `generation`. On a map
    /// generation mismatch, `family` names the current generation of the key's
    /// family, and an answer computed under that family generation is re-tagged
    /// and served.
    ///
    /// Returns a clone: the hit path is a shared-reference lookup, as cheap as
    /// `GenCache::get`, and only a map-generation miss takes the mutable one.
    // Cost: O(1) expected, plus one `family` call when the map generation
    // moved since the key was last confirmed.
    #[inline]
    pub(crate) fn get(&mut self, generation: u64, key: &K, family: impl FnOnce() -> u64) -> Option<V>
    where
        V: Clone,
    {
        let Some(slots) = self.entries.get(key) else {
            crate::vm::vm_stats::record_gen_cache_probe(false, false);
            return None;
        };
        if slots.recent.generation == generation {
            crate::vm::vm_stats::record_gen_cache_probe(true, true);
            return Some(slots.recent.value.clone());
        }
        if let Some(older) = &slots.older
            && older.generation == generation
        {
            crate::vm::vm_stats::record_gen_cache_probe(true, true);
            return Some(older.value.clone());
        }
        self.revalidate(generation, key, family)
    }

    /// [`Self::get`] for a `&self` reader: the same answer, but a hit under a
    /// moved map generation is served without re-tagging the slot, so the
    /// next `&mut` reader still takes the (cold) re-tag.
    // Cost: O(1) expected, plus one `family` call when the map generation moved.
    pub(crate) fn peek(&self, generation: u64, key: &K, family: impl FnOnce() -> u64) -> Option<V>
    where
        V: Clone,
    {
        let slots = self.entries.get(key)?;
        if slots.recent.generation == generation {
            return Some(slots.recent.value.clone());
        }
        if let Some(older) = &slots.older
            && older.generation == generation
        {
            return Some(older.value.clone());
        }
        let fam = family();
        if slots.recent.family == fam {
            return Some(slots.recent.value.clone());
        }
        slots
            .older
            .as_ref()
            .filter(|o| o.family == fam)
            .map(|o| o.value.clone())
    }

    /// [`Self::get`]'s map-generation miss: re-tag the slot computed under the
    /// family's current generation, if any. Kept out of line so the hit path
    /// stays small enough to inline.
    // Cost: O(1) expected, plus the `family` call.
    #[cold]
    #[inline(never)]
    fn revalidate(&mut self, generation: u64, key: &K, family: impl FnOnce() -> u64) -> Option<V>
    where
        V: Clone,
    {
        let slots = self.entries.get_mut(key)?;
        let fam = family();
        let hit = if slots.recent.family == fam {
            slots.recent.generation = generation;
            Some(slots.recent.value.clone())
        } else if let Some(older) = slots.older.as_mut().filter(|o| o.family == fam) {
            older.generation = generation;
            Some(older.value.clone())
        } else {
            None
        };
        crate::vm::vm_stats::record_gen_cache_probe(hit.is_some(), true);
        hit
    }

    /// Record `value` as the answer for `key`, computed under map generation
    /// `generation`; `now` is the map generation at the time of the insert.
    ///
    /// The family generation is read here, not at the miss: most misses never
    /// insert, and reading it per miss cost the multi dispatch benchmark 0.5%.
    /// That is sound only while the map is the one the answer was computed
    /// under: the same map version names the same content, so the family's
    /// current generation describes the keys the answer saw. An answer whose
    /// computation moved the map (`now != generation`) is not recorded.
    // Cost: O(1) expected, plus one `family` call.
    #[inline]
    pub(crate) fn insert(
        &mut self,
        generation: u64,
        now: u64,
        family: impl FnOnce() -> u64,
        key: K,
        value: V,
    ) {
        if now != generation {
            return;
        }
        self.insert_tagged(generation, family(), key, value);
    }

    // Cost: O(1) expected.
    #[inline]
    fn insert_tagged(&mut self, generation: u64, family: u64, key: K, value: V) {
        use std::collections::hash_map::Entry;
        let tagged = Tagged {
            generation,
            family,
            value,
        };
        match self.entries.entry(key) {
            Entry::Vacant(entry) => {
                entry.insert(Slots {
                    recent: tagged,
                    older: None,
                });
            }
            Entry::Occupied(mut entry) => {
                let slots = entry.get_mut();
                if slots.recent.family == family {
                    slots.recent = tagged;
                } else {
                    let displaced = std::mem::replace(&mut slots.recent, tagged);
                    slots.older = Some(displaced);
                }
            }
        }
    }

    /// Whether no key holds an answer.
    // Cost: O(1).
    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Drop every entry: what changed is not a key of the functions map (see
    /// `Interpreter::invalidate_fn_resolution`).
    // Cost: O(k), k = keys held.
    #[inline]
    pub(crate) fn clear(&mut self) {
        self.entries.clear();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn served_under_its_map_generation_without_a_family_read() {
        let mut cache: FamilyGenCache<&str, u32> = FamilyGenCache::default();
        cache.insert_tagged(7, 1, "f", 10);
        assert_eq!(
            cache.get(7, &"f", || panic!("family read on a hit")),
            Some(10)
        );
    }

    /// The property the cache exists for: another family's change moves the
    /// map generation, and the entry survives it.
    #[test]
    fn an_unchanged_family_survives_a_map_generation_move() {
        let mut cache: FamilyGenCache<&str, u32> = FamilyGenCache::default();
        cache.insert_tagged(7, 1, "f", 10);
        assert_eq!(cache.get(8, &"f", || 1), Some(10));
        // Re-tagged: the next probe under 8 is a fast-path hit.
        assert_eq!(cache.get(8, &"f", || panic!("re-tag missed")), Some(10));
    }

    #[test]
    fn a_changed_family_is_not_served() {
        let mut cache: FamilyGenCache<&str, u32> = FamilyGenCache::default();
        cache.insert_tagged(7, 1, "f", 10);
        assert_eq!(cache.get(8, &"f", || 2), None);
    }

    /// A routine-local candidate entering and leaving alternates the family
    /// between two generations' worth of answers; both are kept.
    #[test]
    fn two_families_keep_their_answers() {
        let mut cache: FamilyGenCache<&str, u32> = FamilyGenCache::default();
        cache.insert_tagged(7, 1, "f", 10);
        cache.insert_tagged(8, 2, "f", 20);
        assert_eq!(cache.get(9, &"f", || 1), Some(10));
        assert_eq!(cache.get(10, &"f", || 2), Some(20));
        cache.insert_tagged(11, 3, "f", 30);
        assert_eq!(cache.get(12, &"f", || 3), Some(30));
        assert_eq!(cache.get(13, &"f", || 2), Some(20));
        assert_eq!(cache.get(14, &"f", || 1), None);
    }

    /// An answer whose computation moved the map is not recorded, and the
    /// family is read only for an answer that is.
    #[test]
    fn insert_records_only_under_an_unmoved_map() {
        let mut cache: FamilyGenCache<&str, u32> = FamilyGenCache::default();
        cache.insert(7, 8, || panic!("family read for a dropped insert"), "f", 10);
        assert!(cache.is_empty());
        cache.insert(7, 7, || 1, "f", 10);
        assert_eq!(cache.get(9, &"f", || 1), Some(10));
    }

    #[test]
    fn family_gens_bump_by_base_name() {
        let mut gens = FamilyGens::default();
        let f = Symbol::intern("fgc_test_f");
        assert_eq!(gens.of_name(f), 0);
        gens.bump(f);
        let after = gens.of_name(f);
        assert_ne!(after, 0);
        // A qualified spelling reads its base name's generation.
        assert_eq!(gens.of_name(Symbol::intern("Pkg::fgc_test_f")), after);
        gens.bump(Symbol::intern("fgc_test_g"));
        assert_eq!(gens.of_name(f), after);
    }
}
