//! [`PackageKeyed`]: the `package -> name -> V` symbol tables, with an O(1)
//! "is this name anywhere in the table at all" filter.
//!
//! Every one of these tables is *queried* the same way — `Interpreter::
//! lookup_in_package_chain` takes a candidate owner package and walks up its
//! `::` chain probing `table[pkg][name]` at each tier, and
//! `Interpreter::lookup_in_running_package` runs that walk once per
//! running-package candidate (up to four, each also probed under its
//! unparameterized base name). So a single failed resolution costs up to eight
//! chain walks, each several hash probes deep, and the tables are indexed the
//! wrong way round for the question actually being asked: the walk is over
//! *packages*, but the thing that decides the answer is the *name*.
//!
//! Profiling a `JSON::Fast.from-json` parse ([#8830]) found `lookup_in_
//! package_chain` the single hottest function in the interpreter at 7.34% of
//! instructions retired — roughly 648,000 chain walks to parse 100 flat JSON
//! records, ~6,480 per record. #8811 had already made each walk cheaper; that
//! left it still #1, because the per-call cost was never the problem.
//!
//! The filter is a superset of every inner map's keys: `table[pkg][name]` can
//! only be `Some` for a `name` some inner map holds, so a miss in the filter is
//! a miss in every tier of every candidate, and the whole walk is skipped. A
//! name that was removed may stay in the filter; that only costs the walk the
//! filter would otherwise have skipped, never a wrong answer.
//!
//! **Why it cannot miss a name.** The filter is cached in a [`OnceLock`] that
//! the table's [`DerefMut`] drops, and each package's map is a [`NameMap`]
//! whose own `DerefMut` marks it as not indexed. Every way to add a name —
//! `entry().or_default()`, `get_mut()`, `insert`, `extend`, a whole map moved
//! in from elsewhere — goes through one of the two, and the next query extends
//! the dropped filter with the keys of every map that is not marked as indexed
//! *by this table* (the mark is the table's id, so a map moved in from another
//! table is re-indexed here). The one inherent accessor that deliberately does
//! *not* drop the filter ([`PackageKeyed::get_value_mut`]) cannot change the key
//! set by construction: it hands out `&mut V` for an entry that already exists.
//!
//! **Why it is incremental.** The filter used to be rebuilt from every key of
//! every package after any mutation. A module load alternates writes (each
//! import binds names) with reads (each export lookup asks the filter), so that
//! rebuild made loading O(imports x table size): `use Selkie::UI` (137 modules)
//! spent more than half of its 33 s rebuilding these filters. Extending the
//! dropped filter with only the maps written since costs O(packages) plus the
//! written maps' keys.
//!
//! [#8830]: https://github.com/tokuhirom/mutsu/issues/8830

use rustc_hash::{FxHashMap, FxHashSet};
use std::ops::{Deref, DerefMut};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex, OnceLock};

/// The raw two-level map [`PackageKeyed`] wraps: `package -> name -> V`. Both
/// levels are keyed by plain strings but probed on every free-variable read,
/// so they hash with `FxHash` rather than `SipHash`.
pub(crate) type PackageTable<V> = FxHashMap<String, NameMap<V>>;

/// The id no table has: a [`NameMap`] carrying it is indexed by nobody.
const UNINDEXED: u64 = 0;

/// A fresh table id (never [`UNINDEXED`]).
// Cost: O(1).
fn next_table_id() -> u64 {
    static NEXT: AtomicU64 = AtomicU64::new(UNINDEXED + 1);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

/// One package's `name -> V` map. Derefs to the raw map; a mutable access
/// marks it as not indexed, so the owning table's next filter query adds its
/// keys (see the module docs).
pub(crate) struct NameMap<V> {
    /// Shared between a table and its copy-on-write clones until one of them
    /// writes this package (`Arc::make_mut`), so cloning a table costs one
    /// pointer per package rather than a copy of every entry.
    map: Arc<FxHashMap<String, V>>,
    /// The id of the [`PackageKeyed`] whose filter holds every key of this map,
    /// or [`UNINDEXED`].
    indexed_by: AtomicU64,
}

impl<V: Clone> NameMap<V> {
    /// The raw map, consuming the wrapper.
    // Cost: O(1) when unshared, else O(n) to copy it, n = entries.
    pub(crate) fn into_inner(self) -> FxHashMap<String, V> {
        Arc::unwrap_or_clone(self.map)
    }
}

impl<V> Deref for NameMap<V> {
    type Target = FxHashMap<String, V>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.map
    }
}

impl<V: Clone> DerefMut for NameMap<V> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        *self.indexed_by.get_mut() = UNINDEXED;
        Arc::make_mut(&mut self.map)
    }
}

impl<V> Default for NameMap<V> {
    fn default() -> Self {
        Self::from(FxHashMap::default())
    }
}

impl<V> From<FxHashMap<String, V>> for NameMap<V> {
    fn from(map: FxHashMap<String, V>) -> Self {
        Self {
            map: Arc::new(map),
            indexed_by: AtomicU64::new(UNINDEXED),
        }
    }
}

impl<V> Clone for NameMap<V> {
    /// A copy is indexed by nobody: it is about to live in another table.
    /// The entries stay shared until one side writes.
    fn clone(&self) -> Self {
        self.tagged_clone(UNINDEXED)
    }
}

impl<V> NameMap<V> {
    /// A copy sharing the entries, marked as indexed by `indexed_by`.
    // Cost: O(1).
    fn tagged_clone(&self, indexed_by: u64) -> Self {
        Self {
            map: Arc::clone(&self.map),
            indexed_by: AtomicU64::new(indexed_by),
        }
    }
}

impl<V: std::fmt::Debug> std::fmt::Debug for NameMap<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.map.fmt(f)
    }
}

impl<V: PartialEq> PartialEq for NameMap<V> {
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map
    }
}

impl<'a, V> IntoIterator for &'a NameMap<V> {
    type Item = (&'a String, &'a V);
    type IntoIter = std::collections::hash_map::Iter<'a, String, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.iter()
    }
}

impl<'a, V: Clone> IntoIterator for &'a mut NameMap<V> {
    type Item = (&'a String, &'a mut V);
    type IntoIter = std::collections::hash_map::IterMut<'a, String, V>;

    fn into_iter(self) -> Self::IntoIter {
        (**self).iter_mut()
    }
}

impl<V: Clone> IntoIterator for NameMap<V> {
    type Item = (String, V);
    type IntoIter = std::collections::hash_map::IntoIter<String, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.into_inner().into_iter()
    }
}

/// A [`PackageTable`] plus the name filter described in the module docs.
///
/// Derefs to the raw map, so every existing read (`get`, `iter`, `keys`,
/// `is_empty`) and every existing write (`entry`, `get_mut`, `remove`) works
/// unchanged; the only new operation is [`Self::contains_name`].
pub(crate) struct PackageKeyed<V> {
    table: PackageTable<V>,
    /// This table's id, the mark its [`NameMap`]s carry once indexed.
    id: u64,
    /// A superset of every inner map's keys. Built on the first query after a
    /// mutation and dropped by [`DerefMut`]; see the module docs.
    names: OnceLock<Arc<FxHashSet<Box<str>>>>,
    /// The filter [`DerefMut`] dropped (or a clone's source held), which the
    /// next query extends instead of rebuilding. Touched only on that rebuild,
    /// by `DerefMut` and by `clone`. Shared with the source after a clone
    /// until one side extends it.
    retired: Mutex<Option<Arc<FxHashSet<Box<str>>>>>,
}

impl<V> PackageKeyed<V> {
    /// Whether ANY package in this table may hold an entry called `name`.
    ///
    /// `false` is a definitive miss for every `lookup_in_package_chain` /
    /// `lookup_in_running_package` query over this table, whatever owner
    /// package it would have anchored on.
    // Cost: O(1) while the filter is built; after a mutation, O(p + k) once,
    // p = packages, k = keys of the maps written since the last query.
    pub(crate) fn contains_name(&self, name: &str) -> bool {
        self.names
            .get_or_init(|| {
                // Held for the whole build, so a concurrent `clone` sees
                // either the seed and the marks from before it, or neither.
                let mut retired = self.retired.lock().unwrap_or_else(|e| e.into_inner());
                let mut names = retired.take().unwrap_or_default();
                for map in self.table.values() {
                    if map.indexed_by.load(Ordering::Relaxed) != self.id {
                        if !map.is_empty() {
                            Arc::make_mut(&mut names)
                                .extend(map.keys().map(|key| Box::<str>::from(key.as_str())));
                        }
                        map.indexed_by.store(self.id, Ordering::Relaxed);
                    }
                }
                names
            })
            .contains(name)
    }

    /// `&mut V` for an entry that already exists, WITHOUT dropping the name
    /// cache.
    ///
    /// Sound because the key set cannot change through it: the entry is found
    /// by `(pkg, name)` and only its value is handed out. This exists because
    /// the write-side chokepoints (`unit_lexical_slot_mut` and its callers in
    /// `env_root_descended_mut`) run on every container mutation, and routing
    /// them through `DerefMut` would invalidate the cache on a path that never
    /// adds a name — making the *read* side rebuild the union on the very next
    /// free-variable read.
    // Cost: O(1), plus a copy of the package's map while a clone shares it.
    pub(crate) fn get_value_mut(&mut self, pkg: &str, name: &str) -> Option<&mut V>
    where
        V: Clone,
    {
        let map = &mut self.table.get_mut(pkg)?.map;
        if !map.contains_key(name) {
            return None;
        }
        Arc::make_mut(map).get_mut(name)
    }
}

impl<V> Deref for PackageKeyed<V> {
    type Target = PackageTable<V>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.table
    }
}

impl<V> DerefMut for PackageKeyed<V> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        // Any mutable access can add a name (directly, or by moving a map in),
        // so the filter stops being trustworthy here. It is kept aside as the
        // seed of the next query's filter, which adds the keys of every map
        // not marked as indexed by this table.
        if let Some(names) = self.names.take() {
            *self.retired.get_mut().unwrap_or_else(|e| e.into_inner()) = Some(names);
        }
        &mut self.table
    }
}

impl<V> Default for PackageKeyed<V> {
    fn default() -> Self {
        Self {
            table: PackageTable::default(),
            id: next_table_id(),
            names: OnceLock::new(),
            retired: Mutex::new(None),
        }
    }
}

impl<V> Clone for PackageKeyed<V> {
    /// A clone is a new table (a new id) sharing every package's entries
    /// ([`NameMap`]) and the filter with its source, so a copy-on-write clone
    /// (`cow_table_mut`) costs O(packages), not a copy of every entry plus a
    /// rebuild of the filter. A map the source had indexed is marked as
    /// indexed by the clone, whose filter starts from the source's.
    // Cost: O(p), p = packages.
    fn clone(&self) -> Self {
        let id = next_table_id();
        // Under the lock a filter build holds, so the marks read below agree
        // with the seed (see `contains_name`).
        let retired = self.retired.lock().unwrap_or_else(|e| e.into_inner());
        let seed = self.names.get().cloned().or_else(|| retired.clone());
        let table = self
            .table
            .iter()
            .map(|(pkg, map)| {
                let indexed = seed.is_some() && map.indexed_by.load(Ordering::Relaxed) == self.id;
                (pkg.clone(), map.tagged_clone(if indexed { id } else { UNINDEXED }))
            })
            .collect();
        drop(retired);
        Self {
            table,
            id,
            names: OnceLock::new(),
            retired: Mutex::new(seed),
        }
    }
}

impl<V: std::fmt::Debug> std::fmt::Debug for PackageKeyed<V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.table.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn seeded() -> PackageKeyed<u32> {
        let mut t = PackageKeyed::<u32>::default();
        t.entry("JSON::Fast".to_string())
            .or_default()
            .insert("Bar".to_string(), 1);
        t.entry("JSON::Fast::Inner".to_string())
            .or_default()
            .insert("Baz".to_string(), 2);
        t
    }

    #[test]
    fn contains_name_is_the_union_of_every_packages_keys() {
        let t = seeded();
        assert!(t.contains_name("Bar"));
        assert!(t.contains_name("Baz"));
        assert!(!t.contains_name("Int"));
        assert!(!t.contains_name("JSON::Fast"));
    }

    #[test]
    fn an_insert_through_deref_mut_is_visible_to_the_next_query() {
        let mut t = seeded();
        assert!(!t.contains_name("Quux"));
        t.entry("JSON::Fast".to_string())
            .or_default()
            .insert("Quux".to_string(), 3);
        assert!(t.contains_name("Quux"));
        // A new package after the filter was built.
        t.entry("Other".to_string())
            .or_default()
            .insert("Zot".to_string(), 4);
        assert!(t.contains_name("Zot"));
        assert!(t.contains_name("Bar"));
    }

    #[test]
    fn an_insert_through_an_inner_get_mut_is_visible() {
        let mut t = seeded();
        assert!(!t.contains_name("Late"));
        t.get_mut("JSON::Fast::Inner")
            .unwrap()
            .insert("Late".to_string(), 5);
        assert!(t.contains_name("Late"));
    }

    #[test]
    fn a_map_moved_in_from_another_table_is_reindexed() {
        let mut a = seeded();
        let mut b = PackageKeyed::<u32>::default();
        b.entry("Moved".to_string())
            .or_default()
            .insert("FromB".to_string(), 6);
        // `b` indexes its map; moving that map into `a` must not count as
        // already indexed there.
        assert!(b.contains_name("FromB"));
        assert!(!a.contains_name("FromB"));
        let moved = b.remove("Moved").unwrap();
        a.insert("Moved".to_string(), moved);
        assert!(a.contains_name("FromB"));
    }

    #[test]
    fn a_value_only_update_keeps_the_cache_and_the_answer() {
        let mut t = seeded();
        assert!(t.contains_name("Bar"));
        *t.get_value_mut("JSON::Fast", "Bar").unwrap() = 42;
        assert!(t.contains_name("Bar"));
        assert_eq!(t.get("JSON::Fast").unwrap()["Bar"], 42);
        assert!(t.get_value_mut("JSON::Fast", "Nope").is_none());
        assert!(t.get_value_mut("Nope", "Bar").is_none());
    }

    #[test]
    fn a_removal_leaves_a_superset_not_a_wrong_answer() {
        let mut t = seeded();
        assert!(t.contains_name("Bar"));
        t.get_mut("JSON::Fast").unwrap().remove("Bar");
        // The filter may still say "maybe" for a removed name; the table
        // itself is what answers.
        assert!(t.get("JSON::Fast").unwrap().get("Bar").is_none());
        assert!(t.contains_name("Baz"));
    }

    #[test]
    fn a_clone_answers_the_same_questions() {
        let t = seeded();
        assert!(t.contains_name("Bar"));
        let c = t.clone();
        assert!(c.contains_name("Bar"));
        assert!(!c.contains_name("Int"));
    }
}
