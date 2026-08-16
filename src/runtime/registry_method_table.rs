//! ADR-0019 F4c: the canonical-method-table reverse index, its read/verify
//! API, and (F4c-2) the mutator surface that maintains both, split out of
//! `registry.rs` (already 1402 lines before this file existed — the
//! 500-line convention forbids growing it further, per the F4c design
//! note's own reasoning for `registry_method_table.rs`).
//!
//! `Registry::owner_method_names` (the field) is declared on `registry.rs`;
//! every read AND write goes through this file's API instead. `registry.rs`'s
//! `sync_user_method_entries` was the first caller (F4c-2, routed through
//! these mutators rather than inlining the retain/re-populate logic it used
//! to); F4c-3 (class-declaration family), F4c-4 (augment family), and F4c-5
//! (role pun / mixin classes) have since moved their own write sites onto
//! this API directly, per the ADR-0019 F4c design note section (3)'s ordered
//! slices. `map_user_methods_in_place` and `restore_user_method_rows` remain
//! `#[allow(dead_code)]` until F4c-3's `compile_class_methods` site and
//! F4c-8's snapshot/rollback land respectively; every mutator (used or not
//! yet) is exercised by this file's own unit tests per design note (5) R2's
//! mitigation ("a unit test per mutator for the `registry.rs:361-365`
//! liveness interaction").
//!
//! Every mutator bumps `method_generation` on its own call, per design note
//! (3) ("Every mutator ... bumps `method_generation`"). This is a
//! deliberately accepted, watched behavior change from the old single
//! bump-per-statement cadence (design note (5) R6: "Per-mutation bumps
//! replace per-statement bumps ... batch behind a `bump_once` guard if they
//! move" — a reactive mitigation, not a prerequisite for this slice).

#[cfg(debug_assertions)]
use rustc_hash::FxHashSet as HashSet;

use crate::symbol::Symbol;

use super::MethodDef;
use super::registry::{MethodEntry, MethodEntryKey, Registry};
#[cfg(test)]
use super::{ClassAttributeDef, ClassDef};

/// The row-liveness predicate every mutator drops a row under once it stops
/// holding true (`registry.rs`'s former inline retain condition, now shared
/// -- ADR-0019 F4c design note (3)).
fn entry_is_live(entry: &MethodEntry) -> bool {
    entry.builtin.is_some()
        || !entry.user_candidates.is_empty()
        || entry.accessor.is_some()
        || entry.proto.is_some()
}

impl Registry {
    /// F4c-1 read accessor for [`owner_method_names`](Registry::owner_method_names):
    /// every user-declared method/attribute-accessor name `owner` currently
    /// has a non-empty candidate row for, i.e. the F4c replacement for
    /// `ClassDef::methods.keys()`. Empty for an owner with no user rows
    /// (including any owner that is only a role -- `RoleDef::methods` is
    /// explicitly out of scope for this index, see the ADR-0019 F4c design
    /// note section (1)).
    pub(crate) fn owner_method_names(&self, owner: &str) -> Vec<Symbol> {
        self.owner_method_names
            .get(&Symbol::intern(owner))
            .cloned()
            .unwrap_or_default()
    }

    /// Shared F4c-1 shadow check for the eight `class_def.methods.keys()`
    /// full-name-enumeration read sites the ADR-0019 F4c design note's
    /// ground-truth correction (0)(i) named. `old_names` is today's read (a
    /// borrow, so the caller pays nothing when `MUTSU_VM_STATS` is
    /// disabled); compared against [`owner_method_names`](Registry::
    /// owner_method_names) as a SET (declaration order is not a meaningful
    /// invariant here -- see that field's own doc). Zero behavior change:
    /// this only records a counter, the caller's existing read is untouched.
    pub(crate) fn shadow_check_owner_method_names<'a>(
        &self,
        site: &str,
        owner: &str,
        old_names: impl Iterator<Item = &'a str>,
    ) {
        if !crate::vm::vm_stats::enabled() {
            return;
        }
        let mut old: Vec<&str> = old_names.collect();
        old.sort_unstable();
        old.dedup();
        let mut new: Vec<String> = self
            .owner_method_names(owner)
            .iter()
            .map(Symbol::resolve)
            .collect();
        new.sort_unstable();
        new.dedup();
        let matched = old.iter().copied().eq(new.iter().map(String::as_str));
        crate::vm::vm_stats::record_owner_method_names_shadow_check(site, matched, || {
            format!("owner={owner} old={old:?} new={new:?}")
        });
    }

    /// Full-table consistency check between [`owner_method_names`](Registry::
    /// owner_method_names) and the `user_candidates` half of `method_entries`:
    /// every indexed name must have a live row and every row with a
    /// non-empty `user_candidates` must be indexed under its owner. No-op
    /// unless built with `debug_assertions` AND `MUTSU_CHECK_METHOD_INDEX`
    /// is set (ADR-0019 F4c design note section (2)) -- this is an O(total
    /// rows) full scan, deliberately not run on every build.
    #[cfg(debug_assertions)]
    pub(super) fn debug_verify_owner_method_names_index(&self) {
        use std::sync::OnceLock;
        static ENABLED: OnceLock<bool> = OnceLock::new();
        if !*ENABLED.get_or_init(|| std::env::var_os("MUTSU_CHECK_METHOD_INDEX").is_some()) {
            return;
        }
        for (owner, names) in &self.owner_method_names {
            let mut seen = HashSet::default();
            for name in names {
                assert!(
                    seen.insert(*name),
                    "MUTSU_CHECK_METHOD_INDEX: owner_method_names[{owner:?}] lists {name:?} twice"
                );
                let live = self
                    .method_entries
                    .get(&MethodEntryKey {
                        owner: *owner,
                        name: *name,
                    })
                    .is_some_and(|entry| !entry.user_candidates.is_empty());
                assert!(
                    live,
                    "MUTSU_CHECK_METHOD_INDEX: owner_method_names[{owner:?}] lists {name:?} but its row has no live user_candidates"
                );
            }
        }
        for (key, entry) in &self.method_entries {
            if entry.user_candidates.is_empty() {
                continue;
            }
            let indexed = self
                .owner_method_names
                .get(&key.owner)
                .is_some_and(|names| names.contains(&key.name));
            assert!(
                indexed,
                "MUTSU_CHECK_METHOD_INDEX: method_entries[{key:?}] has live user_candidates but is missing from owner_method_names"
            );
        }
    }

    #[cfg(not(debug_assertions))]
    pub(super) fn debug_verify_owner_method_names_index(&self) {}

    /// Adds or removes `name` from `owner`'s slot in the reverse index to
    /// match `live` (whether `(owner, name)`'s `user_candidates` is
    /// currently non-empty). Internal -- every mutator below calls this
    /// after touching `user_candidates`, never the raw field.
    fn reindex_user_method_name(&mut self, owner: Symbol, name: Symbol, live: bool) {
        if live {
            let names = self.owner_method_names.entry(owner).or_default();
            if !names.contains(&name) {
                names.push(name);
            }
        } else if let Some(names) = self.owner_method_names.get_mut(&owner) {
            names.retain(|n| *n != name);
            if names.is_empty() {
                self.owner_method_names.remove(&owner);
            }
        }
    }

    /// Replaces `(owner, name)`'s user candidate list wholesale -- the F4c-2
    /// mutator behind `sync_user_method_entries`'s per-name re-derivation,
    /// and (from F4c-3 onward) individual class-body method declarations.
    /// An empty `defs` is equivalent to [`remove_user_methods`](Self::
    /// remove_user_methods); the row is dropped from `method_entries`
    /// entirely once no column keeps it alive (ADR-0019 F4c design note
    /// (3)).
    pub(crate) fn set_user_methods(&mut self, owner: Symbol, name: Symbol, defs: Vec<MethodDef>) {
        let live = !defs.is_empty();
        let key = MethodEntryKey { owner, name };
        let entry = self.method_entries.entry(key).or_default();
        entry.user_candidates = defs;
        if !entry_is_live(entry) {
            self.method_entries.remove(&key);
        }
        self.reindex_user_method_name(owner, name, live);
        self.bump_method_generation();
        self.debug_verify_owner_method_names_index();
    }

    /// Appends one candidate to `(owner, name)`'s user candidate list -- the
    /// `multi` declaration case, where a later declaration adds a candidate
    /// rather than replacing the row.
    pub(crate) fn push_user_method(&mut self, owner: Symbol, name: Symbol, def: MethodDef) {
        let key = MethodEntryKey { owner, name };
        self.method_entries
            .entry(key)
            .or_default()
            .user_candidates
            .push(def);
        self.reindex_user_method_name(owner, name, true);
        self.bump_method_generation();
        self.debug_verify_owner_method_names_index();
    }

    /// Filters `(owner, name)`'s user candidate list in place -- the
    /// privacy-preserving non-`multi` replace
    /// (`registration_class_body_method.rs:219-222`'s current shape). A
    /// no-op if the row does not exist.
    pub(crate) fn retain_user_methods(
        &mut self,
        owner: Symbol,
        name: Symbol,
        pred: impl FnMut(&MethodDef) -> bool,
    ) {
        let key = MethodEntryKey { owner, name };
        let Some(entry) = self.method_entries.get_mut(&key) else {
            return;
        };
        entry.user_candidates.retain(pred);
        let live = !entry.user_candidates.is_empty();
        if !entry_is_live(entry) {
            self.method_entries.remove(&key);
        }
        self.reindex_user_method_name(owner, name, live);
        self.bump_method_generation();
        self.debug_verify_owner_method_names_index();
    }

    /// Clears `(owner, name)`'s user candidate list entirely.
    pub(crate) fn remove_user_methods(&mut self, owner: Symbol, name: Symbol) {
        self.set_user_methods(owner, name, Vec::new());
    }

    /// Clears every `(owner, *)` row's user candidate list -- the
    /// redeclaration reset `publish_class_shell`
    /// (`registration_class_validate.rs:406-409`) gets today for free from
    /// the old `sync_user_method_entries`'s combined retain step. A row
    /// that still has a live `builtin`/`accessor`/`proto` column survives
    /// with an empty `user_candidates`; a row with nothing else live is
    /// dropped.
    pub(crate) fn clear_user_methods_for_owner(&mut self, owner: Symbol) {
        let names = self.owner_method_names.remove(&owner).unwrap_or_default();
        for name in names {
            let key = MethodEntryKey { owner, name };
            if let Some(entry) = self.method_entries.get_mut(&key) {
                entry.user_candidates.clear();
                if !entry_is_live(entry) {
                    self.method_entries.remove(&key);
                }
            }
        }
        self.bump_method_generation();
        self.debug_verify_owner_method_names_index();
    }

    /// Moves every user-owned row from `old` to `new` -- `withdraw_role_
    /// pun`'s rename half (`rename_generic_composed_class`,
    /// `registration_class_compose_body.rs:42-46`). `old`'s rows (if any)
    /// are gone afterward; any pre-existing `new`-owned row with the same
    /// name is overwritten, matching `set_user_methods`' own replace
    /// semantics.
    pub(crate) fn rename_method_owner(&mut self, old: Symbol, new: Symbol) {
        let rows = self.user_method_rows_for_owner(old);
        self.clear_user_methods_for_owner(old);
        for (name, defs) in rows {
            self.set_user_methods(new, name, defs);
        }
    }

    /// Mutates every user candidate `owner` currently owns in place (e.g.
    /// `compile_class_methods` filling in `compiled_code` post-compilation,
    /// `accessors_resolve.rs:116-122`). Does not change which names are
    /// live, so the reverse index is untouched.
    #[allow(dead_code)] // ADR-0019 F4c-3 wires this into `compile_class_methods`.
    pub(crate) fn map_user_methods_in_place(
        &mut self,
        owner: Symbol,
        mut f: impl FnMut(&mut MethodDef),
    ) {
        let names = self
            .owner_method_names
            .get(&owner)
            .cloned()
            .unwrap_or_default();
        for name in names {
            if let Some(entry) = self.method_entries.get_mut(&MethodEntryKey { owner, name }) {
                for def in &mut entry.user_candidates {
                    f(def);
                }
            }
        }
        self.bump_method_generation();
    }

    /// Snapshots every user-owned row `owner` currently has, for rollback
    /// (ADR-0019 F4c design note (4)'s `ClassRegSnapshot`/EVAL-rollback
    /// mechanisms). `MethodDef` clones are shallow (`body` is an `Arc`).
    pub(crate) fn user_method_rows_for_owner(
        &self,
        owner: Symbol,
    ) -> Vec<(Symbol, Vec<MethodDef>)> {
        self.owner_method_names
            .get(&owner)
            .into_iter()
            .flatten()
            .filter_map(|name| {
                self.method_entries
                    .get(&MethodEntryKey { owner, name: *name })
                    .map(|entry| (*name, entry.user_candidates.clone()))
            })
            .collect()
    }

    /// Inverse of [`user_method_rows_for_owner`](Self::
    /// user_method_rows_for_owner): replaces `owner`'s entire user-owned row
    /// set with `rows`, clearing anything not present in `rows` first.
    #[allow(dead_code)] // ADR-0019 F4c-8 wires this into snapshot/rollback.
    pub(crate) fn restore_user_method_rows(
        &mut self,
        owner: Symbol,
        rows: Vec<(Symbol, Vec<MethodDef>)>,
    ) {
        self.clear_user_methods_for_owner(owner);
        for (name, defs) in rows {
            self.set_user_methods(owner, name, defs);
        }
    }

    /// Re-derives `owner`'s `accessor` column from `ClassDef::attributes` --
    /// the "surviving half" of the old `sync_user_method_entries` (ADR-0019
    /// F4c design note (3)): type-structure metadata stays on `ClassDef` by
    /// design, so this keeps its pre-existing O(total table) shape
    /// (deliberately not index-accelerated -- accessor-only rows are not
    /// covered by `owner_method_names`, which is scoped to the user-method
    /// column only) rather than getting the O(names) treatment the method
    /// half got. A later same-name attribute overrides an earlier one:
    /// iterating `ClassDef::attributes` in declaration order and letting
    /// each write clobber the last gives "most recent wins".
    pub(crate) fn sync_accessor_entries(&mut self, owner: Symbol) {
        let stale: Vec<MethodEntryKey> = self
            .method_entries
            .iter()
            .filter(|(key, entry)| key.owner == owner && entry.accessor.is_some())
            .map(|(key, _)| *key)
            .collect();
        for key in stale {
            if let Some(entry) = self.method_entries.get_mut(&key) {
                entry.accessor = None;
                if !entry_is_live(entry) {
                    self.method_entries.remove(&key);
                }
            }
        }
        if let Some(class_def) = self.classes.get(Symbol::resolve(&owner).as_str()) {
            let attributes = class_def.attributes.clone();
            for attr in &attributes {
                self.method_entries
                    .entry(MethodEntryKey {
                        owner,
                        name: Symbol::intern(&attr.name),
                    })
                    .or_default()
                    .accessor = Some(attr.is_public);
            }
        }
        self.bump_method_generation();
        self.debug_verify_owner_method_names_index();
    }
}

#[cfg(test)]
#[path = "registry_method_table_tests.rs"]
mod tests;
