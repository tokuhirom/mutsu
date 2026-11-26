//! [`ComposedRoleTable`]: a composed-role registry of the class registry
//! (`class_composed_roles`, `class_direct_composed_roles`,
//! `class_does_only_roles`), as a copy-on-write overlay over the static class
//! catalogue's `does` lists (ADR-19206 slice 3).
//!
//! `build_builtin_registry` used to write one entry per catalogue row with a
//! non-empty `does` list into each of the three maps: ~220 rows, a `Vec` of
//! `String`s each, about 0.35M instructions of every start. Now a row's entry
//! is computed on its first lookup, once per process, into a shared slot, the
//! same way [`super::registry_class_table::ClassTable`] materializes the
//! classes. The overlay holds every entry written at run time (and the eager
//! built-in ones); a write to a catalogue entry copies it there first, and a
//! removal hides the row.
//!
//! The flattened set (`class_composed_roles`) walks `role_parents` as it was
//! when the built-in registry was finished, frozen by
//! [`freeze_builtin_role_parents`]: that is the table the eager loop walked.

use std::sync::{Arc, OnceLock};

use rustc_hash::FxHashSet;

use super::builtin_class_catalogue as catalogue;
use super::registry::Sharded;
use super::registry_sharded_map::ShardedEntry;

type Roles = Vec<String>;

/// Which of the three registries a table is.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum ComposedRoleKind {
    /// `class_composed_roles`: the flattened set, through `role_parents`.
    Flattened,
    /// `class_direct_composed_roles`: the row's own `does` list.
    Direct,
    /// `class_does_only_roles`: `does` minus the row's parent (ADR-0029 R2).
    DoesOnly,
}

static FLATTENED: [OnceLock<Roles>; catalogue::LEN] = [const { OnceLock::new() }; catalogue::LEN];
static DIRECT: [OnceLock<Roles>; catalogue::LEN] = [const { OnceLock::new() }; catalogue::LEN];
static DOES_ONLY: [OnceLock<Roles>; catalogue::LEN] = [const { OnceLock::new() }; catalogue::LEN];

/// The built-in registry's `role_parents`, as it was when
/// `build_builtin_registry` finished.
static BUILTIN_ROLE_PARENTS: OnceLock<Sharded<String, Roles>> = OnceLock::new();

/// Record the built-in registry's finished `role_parents` for the flattened
/// sets. Called once, by `build_builtin_registry`; a copy-on-write table, so
/// this is a reference-count bump.
// Cost: O(1).
pub(crate) fn freeze_builtin_role_parents(role_parents: &Sharded<String, Roles>) {
    let _ = BUILTIN_ROLE_PARENTS.set(role_parents.clone());
}

/// Whether row `index` has an entry in these registries: it composes a role.
// Cost: O(r), r = the row's length.
fn row_has_entry(index: usize) -> bool {
    catalogue::does(index).next().is_some()
}

/// Row `index`'s entry in the `kind` registry, computed on first use exactly
/// as the eager loop of `build_builtin_registry` computed it.
// Cost: O(1) after the first call; the first is O(d * p), d = roles reached,
// p = their parents.
fn materialize(kind: ComposedRoleKind, index: usize) -> &'static Roles {
    let slots = match kind {
        ComposedRoleKind::Flattened => &FLATTENED,
        ComposedRoleKind::Direct => &DIRECT,
        ComposedRoleKind::DoesOnly => &DOES_ONLY,
    };
    slots[index].get_or_init(|| {
        let does: Roles = catalogue::does(index).map(str::to_string).collect();
        match kind {
            ComposedRoleKind::Direct => does,
            ComposedRoleKind::DoesOnly => {
                let parent = catalogue::parent(index);
                does.into_iter().filter(|role| role != parent).collect()
            }
            ComposedRoleKind::Flattened => {
                let mut flattened = does;
                let mut i = 0;
                while i < flattened.len() {
                    if let Some(parents) = BUILTIN_ROLE_PARENTS
                        .get()
                        .and_then(|rp| rp.get(flattened[i].as_str()))
                    {
                        for p in parents {
                            if !flattened.contains(p) {
                                flattened.push(p.clone());
                            }
                        }
                    }
                    i += 1;
                }
                flattened
            }
        }
    })
}

/// See the module docs.
#[derive(Clone, Default)]
pub(crate) struct ComposedRoleTable {
    overlay: Sharded<String, Roles>,
    /// The catalogue registry under the overlay, if any: only a registry
    /// built by `build_builtin_registry` (and its copies) has one.
    catalogue: Option<ComposedRoleKind>,
    /// Catalogue rows removed from view.
    hidden: Option<Arc<FxHashSet<u32>>>,
}

impl ComposedRoleTable {
    /// Put the catalogue's `kind` registry under this table's overlay.
    // Cost: O(1).
    pub(crate) fn add_builtin_catalogue(&mut self, kind: ComposedRoleKind) {
        self.catalogue = Some(kind);
    }

    /// The visible catalogue row with an entry for `name`, whether or not the
    /// overlay shadows it.
    // Cost: O(|name| log R), R = catalogue rows.
    #[inline]
    fn row(&self, name: &str) -> Option<(ComposedRoleKind, usize)> {
        let kind = self.catalogue?;
        let index = catalogue::index_of(name)?;
        (!self.is_hidden(index) && row_has_entry(index)).then_some((kind, index))
    }

    // Cost: O(1) expected.
    fn is_hidden(&self, index: usize) -> bool {
        self.hidden
            .as_ref()
            .is_some_and(|hidden| hidden.contains(&(index as u32)))
    }

    // Cost: O(1) expected, plus the copy of a shared hidden set.
    fn hide(&mut self, index: usize) {
        if !self.is_hidden(index) {
            Arc::make_mut(self.hidden.get_or_insert_with(Default::default)).insert(index as u32);
        }
    }

    /// Visible catalogue rows with an entry that the overlay does not shadow.
    // Cost: O(1) to create; walking it is O(R log R).
    fn unshadowed_rows(&self) -> impl Iterator<Item = (ComposedRoleKind, usize)> + '_ {
        let rows = if self.catalogue.is_some() { catalogue::LEN } else { 0 };
        (0..rows).filter_map(move |index| {
            let kind = self.catalogue?;
            (!self.is_hidden(index)
                && row_has_entry(index)
                && !self.overlay.contains_key(catalogue::name(index)))
            .then_some((kind, index))
        })
    }

    // Cost: O(|name|) expected; a catalogue entry's first lookup in the process
    // also computes it.
    #[inline]
    pub(crate) fn get(&self, name: &str) -> Option<&Roles> {
        if let Some(roles) = self.overlay.get(name) {
            return Some(roles);
        }
        self.row(name).map(|(kind, index)| materialize(kind, index))
    }

    // Cost: O(|name|) expected.
    pub(crate) fn contains_key(&self, name: &str) -> bool {
        self.overlay.contains_key(name) || self.row(name).is_some()
    }

    /// Copy the catalogue entry `name` into the overlay, unless the overlay
    /// already has `name`, so a write never reaches a shared slot.
    // Cost: O(|name|) expected, plus the entry's copy.
    fn copy_up(&mut self, name: &str) {
        if self.overlay.contains_key(name) {
            return;
        }
        if let Some((kind, index)) = self.row(name) {
            let roles = materialize(kind, index).clone();
            self.overlay.insert(name.to_string(), roles);
        }
    }

    // Cost: O(|name|) expected, plus the copies of `copy_up`.
    pub(crate) fn get_mut(&mut self, name: &str) -> Option<&mut Roles> {
        self.copy_up(name);
        if !self.overlay.contains_key(name) {
            return None;
        }
        self.overlay.get_mut(name)
    }

    // Cost: O(|key|) expected, plus the copies of `copy_up`.
    pub(crate) fn entry(&mut self, key: String) -> ShardedEntry<'_, String, Roles> {
        self.copy_up(&key);
        self.overlay.entry(key)
    }

    /// Insert `name`, shadowing a catalogue entry. Returns the entry it
    /// replaced, as a map does.
    // Cost: O(|name|) expected, plus the overlay's shard copy when shared.
    pub(crate) fn insert(&mut self, name: String, roles: Roles) -> Option<Roles> {
        let row = self.row(&name);
        let old = self.overlay.insert(name, roles);
        old.or_else(|| row.map(|(kind, index)| materialize(kind, index).clone()))
    }

    // Cost: O(|name|) expected, plus the overlay's shard copy when shared.
    pub(crate) fn remove(&mut self, name: &str) -> Option<Roles> {
        let old = self.overlay.remove(name);
        match self.row(name) {
            Some((kind, index)) => {
                self.hide(index);
                old.or_else(|| Some(materialize(kind, index).clone()))
            }
            None => old,
        }
    }

    /// Every entry, computing each catalogue entry not computed yet.
    // Cost: O(1) to create; walking it is O(n + R log R).
    pub(crate) fn iter(&self) -> impl Iterator<Item = (&String, &Roles)> + '_ {
        self.overlay.iter().chain(self.unshadowed_rows().map(
            |(kind, index)| -> (&String, &Roles) {
                (catalogue::name_string(index), materialize(kind, index))
            },
        ))
    }

    /// Keep the entries for which `keep` holds; a catalogue entry that fails
    /// it is hidden.
    // Cost: O(n + R log R), plus the overlay's shard copies.
    pub(crate) fn retain(&mut self, mut keep: impl FnMut(&String, &Roles) -> bool) {
        self.overlay.retain(|name, roles| keep(name, roles));
        let dropped: Vec<usize> = self
            .unshadowed_rows()
            .filter(|(kind, index)| {
                !keep(catalogue::name_string(*index), materialize(*kind, *index))
            })
            .map(|(_, index)| index)
            .collect();
        for index in dropped {
            self.hide(index);
        }
    }

    /// Restore every entry of `snapshot` that `self` no longer has, keeping
    /// the ones it still has (an EVAL rollback).
    // Cost: O(s + R), s = the snapshot's overlay entries.
    pub(crate) fn refill_from(&mut self, snapshot: &ComposedRoleTable) {
        for (name, roles) in snapshot.overlay.iter() {
            if !self.contains_key(name) {
                self.overlay.insert(name.clone(), roles.clone());
            }
        }
        if self.catalogue.is_some() && snapshot.catalogue == self.catalogue {
            for index in 0..catalogue::LEN {
                if self.is_hidden(index) && !snapshot.is_hidden(index) {
                    if let Some(hidden) = self.hidden.as_mut() {
                        Arc::make_mut(hidden).remove(&(index as u32));
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Every row's three entries are what the eager loop of
    /// `build_builtin_registry` wrote, walking the built registry's
    /// `role_parents`.
    #[test]
    fn every_row_computes_what_the_eager_loop_wrote() {
        let registry = crate::runtime::Interpreter::shared_builtin_registry();
        for index in 0..catalogue::LEN {
            let name = catalogue::name(index);
            let does: Roles = catalogue::does(index).map(str::to_string).collect();
            if does.is_empty() {
                assert!(registry.class_composed_roles.get(name).is_none(), "{name}");
                continue;
            }
            let mut flattened = does.clone();
            let mut i = 0;
            while i < flattened.len() {
                if let Some(parents) = registry.role_parents.get(flattened[i].as_str()) {
                    for p in parents {
                        if !flattened.contains(p) {
                            flattened.push(p.clone());
                        }
                    }
                }
                i += 1;
            }
            let parent = catalogue::parent(index);
            let does_only: Roles = does.iter().filter(|r| *r != parent).cloned().collect();
            assert_eq!(registry.class_composed_roles.get(name), Some(&flattened), "{name}");
            assert_eq!(registry.class_direct_composed_roles.get(name), Some(&does), "{name}");
            assert_eq!(registry.class_does_only_roles.get(name), Some(&does_only), "{name}");
        }
        let tlfr = registry
            .class_does_only_roles
            .get("X::TooLateForREPR")
            .expect("row");
        assert!(tlfr.is_empty(), "X::TooLateForREPR's X::Comp is its parent");
    }

    #[test]
    fn writes_copy_up_and_removal_hides() {
        let registry = crate::runtime::Interpreter::shared_builtin_registry();
        let mut table = registry.class_direct_composed_roles.clone();
        let name = "X::Anon::Augment";
        table.get_mut(name).expect("row").push("Probe".to_string());
        assert!(table.get(name).unwrap().contains(&"Probe".to_string()));
        assert!(
            !registry
                .class_direct_composed_roles
                .get(name)
                .unwrap()
                .contains(&"Probe".to_string())
        );
        let snapshot = registry.class_direct_composed_roles.clone();
        let mut t = snapshot.clone();
        assert!(t.remove(name).is_some());
        assert!(!t.contains_key(name));
        t.refill_from(&snapshot);
        assert!(t.contains_key(name));
    }
}
