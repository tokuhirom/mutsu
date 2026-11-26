//! [`ClassTable`]: the registry's class table, a copy-on-write overlay over the
//! static catalogue of built-in classes (ADR-19206).
//!
//! The overlay is the sharded copy-on-write table the registry has always
//! used (ADR-17216): user classes, the built-in classes not in the catalogue,
//! and copies of catalogue classes that something wrote to. Under it, a
//! registry built by `build_builtin_registry` sees the catalogue
//! (`builtin_class_catalogue`), whose classes are materialized on first
//! lookup into process-wide slots. Nothing writes a slot after it is filled:
//! [`ClassTable::get_mut`] and [`ClassTable::entry`] copy a catalogue class
//! into the overlay first (a reference-count bump), and [`ClassTable::remove`]
//! hides the row.
//!
//! The interface is the overlay's, except that whole-table walks ([`ClassTable::iter`], [`ClassTable::keys`]) visit the
//! catalogue too, so the first walk materializes every visible row. Start-up
//! must not walk the table; `build_builtin_registry` uses
//! [`ClassTable::iter_overlay`].

use std::sync::Arc;

use rustc_hash::FxHashSet;

use super::ClassDef;
use super::builtin_class_catalogue as catalogue;
use super::registry::Sharded;
use super::registry_cow_table::CowTable;
use super::registry_sharded_map::{ShardedEntry, ShardedMap};

type Entry = CowTable<ClassDef>;

/// See the module docs.
#[derive(Clone, Default)]
pub(crate) struct ClassTable {
    overlay: Sharded<String, Entry>,
    /// Whether the built-in catalogue is visible under the overlay. Only a
    /// registry built by `build_builtin_registry` (and its copies) has it; a
    /// default registry is empty, as before.
    catalogue: bool,
    /// Catalogue rows removed from view, by row index.
    hidden: Option<Arc<FxHashSet<u32>>>,
    /// The registry generation of the last change to `hidden`
    /// (see [`Self::key_generation`]).
    hidden_gen: u64,
}

impl ClassTable {
    /// A table of `overlay` with the built-in catalogue under it.
    // Cost: O(1).
    pub(crate) fn with_builtin_catalogue(overlay: ShardedMap<String, Entry>) -> Self {
        Self {
            overlay: overlay.into(),
            catalogue: true,
            hidden: None,
            hidden_gen: 0,
        }
    }

    /// The visible catalogue row of `name`, whether or not the overlay
    /// shadows it.
    // Cost: O(|name| log R), R = catalogue rows.
    #[inline]
    fn row(&self, name: &str) -> Option<usize> {
        if !self.catalogue {
            return None;
        }
        catalogue::index_of(name).filter(|index| !self.is_hidden(*index))
    }

    // Cost: O(1) expected.
    #[inline]
    fn is_hidden(&self, index: usize) -> bool {
        self.hidden
            .as_ref()
            .is_some_and(|hidden| hidden.contains(&(index as u32)))
    }

    // Cost: O(1) expected, plus the copy of a shared hidden set.
    fn set_hidden(&mut self, index: usize, hide: bool) {
        if self.is_hidden(index) == hide {
            return;
        }
        let set = Arc::make_mut(self.hidden.get_or_insert_with(Default::default));
        if hide {
            set.insert(index as u32);
        } else {
            set.remove(&(index as u32));
        }
        self.hidden_gen = crate::runtime::registry_gen::next_generation();
    }

    /// Visible catalogue rows the overlay does not shadow.
    // Cost: O(1) to create; walking it is O(R log R).
    fn unshadowed_rows(&self) -> impl Iterator<Item = usize> + '_ {
        let rows = if self.catalogue { catalogue::LEN } else { 0 };
        (0..rows).filter(move |index| {
            !self.is_hidden(*index) && !self.overlay.contains_key(catalogue::name(*index))
        })
    }

    // Cost: O(|name|) expected; a catalogue class's first lookup in the
    // process also builds it.
    #[inline]
    pub(crate) fn get(&self, name: &str) -> Option<&Entry> {
        if let Some(entry) = self.overlay.get(name) {
            return Some(entry);
        }
        self.row(name).map(catalogue::materialize)
    }

    // Cost: O(|name|) expected.
    #[inline]
    pub(crate) fn contains_key(&self, name: &str) -> bool {
        self.overlay.contains_key(name) || self.row(name).is_some()
    }

    /// Copy the catalogue class `name` into the overlay, unless the overlay
    /// already has `name`, so a write never reaches a shared slot.
    // Cost: O(|name|) expected, plus the overlay's shard copy when shared.
    fn copy_up(&mut self, name: &str) {
        if self.overlay.contains_key(name) {
            return;
        }
        if let Some(index) = self.row(name) {
            let entry = catalogue::materialize(index).clone();
            self.overlay.insert(name.to_string(), entry);
        }
    }

    // Cost: O(|name|) expected, plus the copies of `copy_up`.
    pub(crate) fn get_mut(&mut self, name: &str) -> Option<&mut Entry> {
        self.copy_up(name);
        if !self.overlay.contains_key(name) {
            return None;
        }
        self.overlay.get_mut(name)
    }

    /// The overlay entry for `key`, starting from the catalogue class when
    /// `key` names one.
    // Cost: O(|key|) expected, plus the copies of `copy_up`.
    pub(crate) fn entry(&mut self, key: String) -> ShardedEntry<'_, String, Entry> {
        self.copy_up(&key);
        self.overlay.entry(key)
    }

    /// Insert `name`, shadowing a catalogue class of that name. Returns the
    /// entry it replaced, as a map does.
    // Cost: O(|name|) expected, plus the overlay's shard copy when shared.
    pub(crate) fn insert(&mut self, name: String, entry: Entry) -> Option<Entry> {
        let row = self.row(&name);
        let old = self.overlay.insert(name, entry);
        old.or_else(|| row.map(|index| catalogue::materialize(index).clone()))
    }

    // Cost: O(|name|) expected, plus the overlay's shard copy when shared.
    pub(crate) fn remove(&mut self, name: &str) -> Option<Entry> {
        let old = self.overlay.remove(name);
        match self.row(name) {
            Some(index) => {
                self.set_hidden(index, true);
                old.or_else(|| Some(catalogue::materialize(index).clone()))
            }
            None => old,
        }
    }

    /// Every class name: the overlay's, then the unshadowed catalogue rows.
    // Cost: O(1) to create; walking it is O(n + R log R), n = overlay entries
    // (the first walk in the process also allocates the catalogue's names).
    pub(crate) fn keys(&self) -> impl Iterator<Item = &String> + '_ {
        self.overlay
            .keys()
            .chain(
                self.unshadowed_rows()
                    .map(|index| -> &String { catalogue::name_string(index) }),
            )
    }

    /// Every class, materializing each catalogue class not built yet.
    // Cost: O(1) to create; walking it is O(n + R log R), plus the first
    // materialization of each catalogue class.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (&String, &Entry)> + '_ {
        self.overlay.iter().chain(
            self.unshadowed_rows()
                .map(|index| -> (&String, &Entry) {
                    (catalogue::name_string(index), catalogue::materialize(index))
                }),
        )
    }

    /// The overlay's classes only: what a walk that must not materialize the
    /// catalogue (start-up) may see.
    // Cost: O(1) to create; walking it is O(n + SHARDS).
    pub(crate) fn iter_overlay(&self) -> impl Iterator<Item = (&String, &Entry)> {
        self.overlay.iter()
    }

    /// Names the current key set: equal values mean the same keys. Both
    /// halves are process-unique registry generations, so the larger one
    /// moves whenever either does.
    // Cost: O(1).
    #[inline]
    pub(crate) fn key_generation(&self) -> u64 {
        self.overlay.key_generation().max(self.hidden_gen)
    }

    /// Whether `self` and `other` are the same, unwritten snapshot.
    // Cost: O(1).
    pub(crate) fn shares_with(&self, other: &Self) -> bool {
        self.overlay.shares_with(&other.overlay)
            && self.catalogue == other.catalogue
            && match (&self.hidden, &other.hidden) {
                (None, None) => true,
                (Some(a), Some(b)) => Arc::ptr_eq(a, b),
                _ => false,
            }
    }

    /// Keep the classes whose name satisfies `keep`; a catalogue class that
    /// fails it is hidden.
    // Cost: O(n + R log R), plus the overlay's shard copies.
    pub(crate) fn retain_names(&mut self, mut keep: impl FnMut(&str) -> bool) {
        self.overlay.retain(|name, _| keep(name));
        let dropped: Vec<usize> = self
            .unshadowed_rows()
            .filter(|index| !keep(catalogue::name(*index)))
            .collect();
        for index in dropped {
            self.set_hidden(index, true);
        }
    }

    /// Restore every class of `snapshot` that `self` no longer has, keeping
    /// the ones it still has (an EVAL rollback).
    // Cost: O(s + R), s = the snapshot's overlay entries.
    pub(crate) fn refill_from(&mut self, snapshot: &ClassTable) {
        for (name, entry) in snapshot.overlay.iter() {
            if !self.contains_key(name) {
                self.overlay.insert(name.clone(), entry.clone());
            }
        }
        if self.catalogue && snapshot.catalogue {
            for index in 0..catalogue::LEN {
                if self.is_hidden(index) && !snapshot.is_hidden(index) {
                    self.set_hidden(index, false);
                }
            }
        }
    }
}

impl<'a> IntoIterator for &'a ClassTable {
    type Item = (&'a String, &'a Entry);
    type IntoIter = Box<dyn Iterator<Item = (&'a String, &'a Entry)> + 'a>;
    // Cost: as for [`ClassTable::iter`].
    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.iter())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn table() -> ClassTable {
        ClassTable::with_builtin_catalogue(ShardedMap::default())
    }

    #[test]
    fn catalogue_classes_are_visible_without_an_overlay_entry() {
        let t = table();
        assert!(t.contains_key("X::Adverb"));
        assert!(t.get("X::Adverb").is_some());
        assert_eq!(t.keys().count(), catalogue::LEN);
        assert!(!ClassTable::default().contains_key("X::Adverb"));
    }

    #[test]
    fn a_write_copies_into_the_overlay_and_leaves_the_slot() {
        let mut t = table();
        t.get_mut("X::Adverb")
            .expect("catalogue class")
            .native_methods
            .insert("probe".to_string());
        assert!(t.get("X::Adverb").unwrap().native_methods.contains("probe"));
        assert!(!table().get("X::Adverb").unwrap().native_methods.contains("probe"));
        assert_eq!(t.keys().count(), catalogue::LEN);
        assert_eq!(t.keys().filter(|k| k.as_str() == "X::Adverb").count(), 1);
    }

    #[test]
    fn remove_hides_and_refill_restores() {
        let snapshot = table();
        let mut t = snapshot.clone();
        let before = t.key_generation();
        assert!(t.remove("X::Adverb").is_some());
        assert!(!t.contains_key("X::Adverb"));
        assert_ne!(t.key_generation(), before);
        assert_eq!(t.keys().count(), catalogue::LEN - 1);
        t.refill_from(&snapshot);
        assert!(t.contains_key("X::Adverb"));

        let mut u = snapshot.clone();
        u.retain_names(|name| name != "X::Bind");
        assert!(!u.contains_key("X::Bind"));
        assert!(u.contains_key("X::Adverb"));
    }
}
