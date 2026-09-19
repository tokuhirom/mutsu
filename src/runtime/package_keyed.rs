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
//! The filter here is a plain union of every inner map's keys. It is exact,
//! not approximate: `table[pkg][name]` can only be `Some` for a `name` some
//! inner map holds, so a miss in the union is a miss in every tier of every
//! candidate, and the whole walk is skipped.
//!
//! **Why it cannot drift.** The union is cached in a [`OnceLock`] that
//! [`DerefMut`] drops. Every mutation of the table — `entry().or_default()`,
//! `get_mut()`, `insert`, `remove`, `extend` — reaches the inner map through
//! `DerefMut`, so there is no way to add a name without invalidating the
//! cache, and the next query rebuilds it. The one inherent accessor that
//! deliberately does *not* invalidate ([`PackageKeyed::get_value_mut`]) cannot
//! change the key set by construction: it hands out `&mut V` for an entry that
//! already exists.
//!
//! [#8830]: https://github.com/tokuhirom/mutsu/issues/8830

use rustc_hash::{FxHashMap, FxHashSet};
use std::ops::{Deref, DerefMut};
use std::sync::OnceLock;

/// The raw two-level map [`PackageKeyed`] wraps: `package -> name -> V`. Both
/// levels are keyed by plain strings but probed on every free-variable read,
/// so they hash with `FxHash` rather than `SipHash`.
pub(crate) type PackageTable<V> = FxHashMap<String, FxHashMap<String, V>>;

/// A [`PackageTable`] plus the name filter described in the module docs.
///
/// Derefs to the raw map, so every existing read (`get`, `iter`, `keys`,
/// `is_empty`) and every existing write (`entry`, `get_mut`, `remove`) works
/// unchanged; the only new operation is [`Self::contains_name`].
pub(crate) struct PackageKeyed<V> {
    table: PackageTable<V>,
    /// The union of every inner map's keys. Built on the first query after a
    /// mutation and dropped by [`DerefMut`]; see the module docs.
    names: OnceLock<FxHashSet<Box<str>>>,
}

impl<V> PackageKeyed<V> {
    /// Whether ANY package in this table holds an entry called `name`.
    ///
    /// `false` is a definitive miss for every `lookup_in_package_chain` /
    /// `lookup_in_running_package` query over this table, whatever owner
    /// package it would have anchored on.
    pub(crate) fn contains_name(&self, name: &str) -> bool {
        self.names
            .get_or_init(|| {
                self.table
                    .values()
                    .flat_map(|entries| entries.keys())
                    .map(|key| Box::<str>::from(key.as_str()))
                    .collect()
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
    pub(crate) fn get_value_mut(&mut self, pkg: &str, name: &str) -> Option<&mut V> {
        self.table.get_mut(pkg)?.get_mut(name)
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
        // Any mutable access can add or remove a name, so the union stops
        // being trustworthy here. Dropping it is what makes the cache
        // impossible to desynchronise; the next query rebuilds it.
        self.names.take();
        &mut self.table
    }
}

impl<V> Default for PackageKeyed<V> {
    fn default() -> Self {
        Self {
            table: PackageTable::default(),
            names: OnceLock::new(),
        }
    }
}

impl<V: Clone> Clone for PackageKeyed<V> {
    /// Deliberately does NOT carry the name cache over. A clone of one of
    /// these tables is a copy-on-write clone (`cow_table_mut`), i.e. the
    /// caller is about to mutate it and would drop the cache anyway — copying
    /// the whole union first would be pure waste.
    fn clone(&self) -> Self {
        Self {
            table: self.table.clone(),
            names: OnceLock::new(),
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
    fn a_removal_is_a_superset_miss_not_a_wrong_answer() {
        let mut t = seeded();
        assert!(t.contains_name("Bar"));
        t.get_mut("JSON::Fast").unwrap().remove("Bar");
        // The cache was dropped by `get_mut`, so this is exact again rather
        // than merely conservative.
        assert!(!t.contains_name("Bar"));
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
