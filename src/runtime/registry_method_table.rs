//! ADR-0019 F4c: the canonical-method-table reverse index and its read/verify
//! API, split out of `registry.rs` (already 1402 lines before this file
//! existed — the 500-line convention forbids growing it further, per the F4c
//! design note's own reasoning for `registry_method_table.rs`). This starts
//! as a second `impl Registry` block holding just the F4c-1 read side
//! (`owner_method_names`, the shadow check, and the debug verifier); F4c-2
//! adds the mutator surface (`set_user_methods`, `push_user_method`, ...)
//! here too.
//!
//! `Registry::owner_method_names` (the field) is declared and maintained on
//! `registry.rs`'s `sync_user_method_entries`, since that is where the data
//! it derives from (`ClassDef::methods`) is read — the split is call-site
//! surface, not data ownership.

#[cfg(debug_assertions)]
use rustc_hash::FxHashSet as HashSet;

use crate::symbol::Symbol;

#[cfg(debug_assertions)]
use super::registry::MethodEntryKey;
use super::registry::Registry;

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
}
