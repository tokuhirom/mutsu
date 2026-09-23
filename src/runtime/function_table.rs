//! The registry's function map and the version stamp that travels with it.
//!
//! [`Registry::functions`](crate::runtime::registry::Registry::functions) is a
//! copy-on-write `Arc`: a block- or routine-scope snapshot clones the `Arc`
//! (a refcount bump), every write goes through `Arc::make_mut`, and a scope
//! restore puts the *saved `Arc` itself* back. Name-keyed dispatch caches are
//! memos over that map, so each one needs to know "is the map still the map I
//! computed this answer under".
//!
//! The answer used to be a counter on the interpreter that every registry
//! mutation incremented (`Interpreter::fn_resolve_gen`). Counting steps makes
//! the answer wrong in one specific, extremely common direction: a map that
//! goes away and *comes back* — which is exactly what a routine-local `my sub`
//! does on every single call — reads as two different states, so every memo
//! built before the excursion is discarded even though the map it described is
//! back, byte for byte
//! ([#8314](https://github.com/tokuhirom/mutsu/issues/8314)).
//!
//! So the version lives **in the map**, not beside it. [`FunctionTable`] is the
//! map plus a `version` drawn from a process-global counter; the stamp is
//! renewed by [`FunctionTable::map_mut`], which
//! [`Registry::functions_mut`](crate::runtime::registry::Registry::functions_mut)
//! is the only caller of, and which is in turn the only way to write the map.
//! Two properties follow *by construction*, without any call site having to
//! cooperate:
//!
//! 1. **A version identifies map content.** The counter never repeats, so a
//!    version is minted for one content state and never re-used for another.
//!    A memo tagged with version `V` can only ever be served while the map is
//!    the same map it was computed from — no "generation `G+1` reached by a
//!    different route" aliasing, which is the silent mis-dispatch the naive
//!    "restore the saved counter" fix would introduce.
//! 2. **A restore restores the version.** `registry.functions = saved` moves
//!    the whole `Arc` back, stamp included, so the pre-excursion version
//!    returns with the pre-excursion content and memos tagged with it become
//!    live again. Nothing has to notice that a restore happened.
//!
//! Reads are unaffected: [`FunctionTable`] derefs to the map, so
//! `registry.functions.get(&key)` and friends read exactly as they did when the
//! field was a bare `Arc<HashMap<..>>`.
//!
//! # Recognising a state the map has been in before
//!
//! Minting a fresh stamp per write covers the *restore* direction for free, but
//! not the other half of the same cycle. A routine-local `my sub` is installed
//! on entry to its routine and taken away again on exit, so the map alternates
//! between two contents on every call — and the install re-creates a content the
//! map has held on every previous call, out of the very same `Arc<FunctionDef>`
//! (`prepared_fn_defs` caches the derived definition so the re-install is a
//! refcount bump). A fresh stamp each time says "new state" about a state that
//! is a call old.
//!
//! [`FunctionTableTransitions`] closes that half. It memoizes the one fact a
//! stamp cannot carry: *installing this key with this definition, into the map
//! named `V`, yields the map named `W`*. On the next call the map is named `V`
//! again (the restore saw to that), the same install runs, and the stamp goes
//! back to `W` instead of to something new — so the memos taken inside the
//! routine are live again too.
//!
//! The reuse is sound by induction rather than by comparing content: a version
//! names one content (base case: fresh stamps are unique), a recorded
//! transition was observed to take that content to the content named `W`, and
//! the same content under the same write yields the same content. A
//! debug-only audit ([`FunctionTableTransitions::audit`]) re-derives a content
//! hash for every stamp it hands out and panics if one ever names two different
//! maps, so a mistake in that reasoning fails CI instead of mis-dispatching.
//!
//! "The same content" is decided by an `Arc<FunctionDef>`'s address, which is
//! only sound while that address cannot be handed to a different, unrelated
//! `Arc<FunctionDef>` -- so [`FunctionTableTransitions::seen`] keeps a clone of
//! every definition it has memoized alive for as long as its entry lives,
//! rather than trusting the bare address to keep meaning the same thing after
//! every other owner drops it.

use std::ops::Deref;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::runtime::FunctionDef;
use crate::symbol::Symbol;
use rustc_hash::FxHashMap;

/// The map behind [`FunctionTable`]: registry function key -> definition.
pub(crate) type FunctionMap = FxHashMap<Symbol, std::sync::Arc<FunctionDef>>;

/// Source of [`FunctionTable::version`] stamps.
///
/// Process-global and monotonic, so a stamp is unique across every
/// `Interpreter` in the process — a thread interpreter clones the registry
/// (`runtime_thread.rs`) and would otherwise be able to mint a version that
/// collides with the parent's for different content. Starts at 1 so that 0 can
/// stay the "never observed a map" value for a freshly built interpreter.
static NEXT_FUNCTION_TABLE_VERSION: AtomicU64 = AtomicU64::new(1);

fn next_version() -> u64 {
    NEXT_FUNCTION_TABLE_VERSION.fetch_add(1, Ordering::Relaxed)
}

/// The registry's function map, carrying the version stamp of its content.
///
/// See the module docs for why the stamp lives here rather than on the
/// interpreter.
#[derive(Clone)]
pub(crate) struct FunctionTable {
    map: FunctionMap,
    version: u64,
}

impl Default for FunctionTable {
    fn default() -> Self {
        Self {
            map: FunctionMap::default(),
            // Even the empty map gets a real stamp: two independently built
            // registries hold different (if equal) maps, and a shared 0 would
            // let one's memos be served under the other's version.
            version: next_version(),
        }
    }
}

impl FunctionTable {
    /// The version of the content currently in this table.
    #[inline]
    pub(crate) fn version(&self) -> u64 {
        self.version
    }

    /// Mutable access to the map, renewing the version stamp.
    ///
    /// The stamp is renewed once for the whole borrow, not per write — nothing
    /// can read the map while the `&mut` is outstanding, so the intermediate
    /// states inside the borrow are unobservable and the stamp correctly names
    /// whatever the map holds when the borrow ends.
    ///
    /// A borrow that turns out not to write anything only burns a version: the
    /// memos built under the old one go unused, which costs a rebuild and is
    /// never wrong. Under-stamping is the direction that would be wrong, and it
    /// cannot happen from here.
    #[inline]
    pub(crate) fn map_mut(&mut self) -> &mut FunctionMap {
        self.version = next_version();
        &mut self.map
    }

    /// Mint a fresh version for content that has not changed.
    ///
    /// For an announcement that something *other* than this map changed —
    /// a routine wrapped, an import scope popped — where the change still has
    /// to retire every memo hanging off the version. See
    /// `Interpreter::invalidate_fn_resolution`.
    #[inline]
    pub(crate) fn renew_version(&mut self) {
        self.version = next_version();
    }

    /// Re-stamp the table with a version that has already named this exact
    /// content.
    ///
    /// The only caller is [`FunctionTableTransitions::install`], which supplies
    /// a version it recorded as the outcome of this very write from this very
    /// starting version. Handing it anything else breaks the invariant that a
    /// version names one map, which is what every generation-tagged memo relies
    /// on — hence the private-to-the-module visibility and the audit.
    #[inline]
    fn set_version(&mut self, version: u64) {
        self.version = version;
    }
}

/// What installing one key did to the map's version, last time.
///
/// Lives on the `Registry` (cloned with it into a thread interpreter, which is
/// sound: versions are process-global, so a cloned entry still names the same
/// content). See the module docs for why it exists and why reuse is sound.
#[derive(Clone, Default)]
pub(crate) struct FunctionTableTransitions {
    /// `(version before, key, definition identity) -> (definition kept alive, version after)`.
    ///
    /// "Definition identity" is the `Arc<FunctionDef>`'s address. An address is
    /// only a sound stand-in for "this exact definition" while the allocation
    /// behind it cannot be freed and handed to an unrelated `Arc<FunctionDef>`
    /// — otherwise a later, different definition can land on the same address
    /// and collide with a stale entry here
    /// ([#8932](https://github.com/tokuhirom/mutsu/issues/8932),
    /// [#8934](https://github.com/tokuhirom/mutsu/issues/8934)). Retaining a
    /// clone of the `Arc` alongside its entry pins that allocation for as long
    /// as the entry is live, so no other `Arc<FunctionDef>` can ever be
    /// allocated at the same address in the meantime: the address a fresh
    /// `Arc::new` receives is therefore guaranteed to differ from every
    /// address already claimed by a live entry, and a memo hit's address can
    /// only ever mean "this is a clone of the very `Arc` recorded here".
    seen: FxHashMap<TransitionKey, (std::sync::Arc<FunctionDef>, u64)>,
    /// The whole table a *recurring* transition produced, keyed like `seen`.
    ///
    /// Knowing the resulting version is not enough to make the re-install
    /// cheap: the install still went through `Arc::make_mut` on a map the
    /// scope snapshot shares, which copies the entire functions map — once per
    /// call of any routine that declares an inner `my sub`, O(registry) each
    /// time, and the dominant per-call cost once a module is loaded (#9073).
    /// The resulting table is immutable content named by its version, so the
    /// second and later installs just hand this `Arc` back. Only a transition
    /// seen at least twice is stored, and the table is capped far below
    /// `seen`, because each entry pins one full copy of the map.
    tables: FxHashMap<TransitionKey, std::sync::Arc<FunctionTable>>,
    /// `version after -> (version before, key installed)` for every transition
    /// recorded in `seen`: what one install did, read backwards. A scope
    /// restore uses it to name the keys it gives back without diffing two
    /// whole maps (see [`Self::keys_installed_since`]).
    parents: FxHashMap<u64, (u64, Symbol)>,
    /// Debug-only: the content hash each version has been observed to name.
    /// Empty (and untouched) in a release build.
    #[cfg(debug_assertions)]
    audit: FxHashMap<u64, u64>,
}

/// `(version before, key, definition identity)`: one install, as memoized.
type TransitionKey = (u64, Symbol, usize);

/// How many resulting tables [`FunctionTableTransitions::tables`] keeps before
/// starting over. Each one is a full copy of the functions map, so this is the
/// number of distinct inner-`my sub` routines whose steady state is cheap at
/// once, not a bound on correctness.
const TABLE_MEMO_CAP: usize = 64;

/// How far [`FunctionTableTransitions::keys_installed_since`] walks back
/// before giving up and letting the caller diff the maps.
const PARENT_WALK_LIMIT: usize = 8;

/// How many transitions to remember before starting over.
///
/// A steady-state program cycles through a handful; the table only grows while
/// new routines are being declared, and dropping it merely costs fresh stamps.
const TRANSITION_MEMO_CAP: usize = 4096;

impl FunctionTableTransitions {
    /// Insert `key -> def` into the table behind `functions`, reusing the
    /// version — and, for a transition that recurs, the very table — this
    /// same write produced last time it was made from this same starting
    /// version.
    pub(crate) fn install(
        &mut self,
        functions: &mut std::sync::Arc<FunctionTable>,
        key: Symbol,
        def: std::sync::Arc<FunctionDef>,
    ) {
        let memo_key = (
            functions.version(),
            key,
            std::sync::Arc::as_ptr(&def) as usize,
        );
        let known = self.seen.get(&memo_key).map(|(_, version)| *version);
        crate::vm::vm_stats::record_fn_table_transition(known.is_some());
        match known {
            Some(version) => {
                if let Some(table) = self.tables.get(&memo_key) {
                    // Sound by the same induction as the version reuse: the
                    // stored table is the content `version` names (it was the
                    // table that version was stamped on, and a shared `Arc` is
                    // never written in place — `cow_table_mut` copies it and
                    // mints a new version first).
                    debug_assert_eq!(table.version(), version);
                    *functions = std::sync::Arc::clone(table);
                } else {
                    let table = crate::runtime::cow_table_mut(functions);
                    table.map_mut().insert(key, def);
                    table.set_version(version);
                    if self.tables.len() >= TABLE_MEMO_CAP {
                        self.tables.clear();
                    }
                    self.tables
                        .insert(memo_key, std::sync::Arc::clone(functions));
                }
            }
            None => {
                if self.seen.len() >= TRANSITION_MEMO_CAP {
                    self.seen.clear();
                    self.tables.clear();
                    self.parents.clear();
                }
                // Retain a clone so this address can never be freed and
                // reused by an unrelated `Arc<FunctionDef>` while this entry
                // is live -- see the field doc on `seen`.
                let retained = def.clone();
                let table = crate::runtime::cow_table_mut(functions);
                table.map_mut().insert(key, def);
                self.seen.insert(memo_key, (retained, table.version()));
                self.parents.insert(table.version(), (memo_key.0, key));
            }
        }
        self.audit(functions);
    }

    /// The keys that installs recorded here added to get from the map named
    /// `from` to the map named `to`, or `None` when `to` was not reached from
    /// `from` by recorded installs alone (any other write in between mints a
    /// version with no parent, so the walk stops there).
    ///
    /// A scope restore that puts `from` back uses it to learn which keys it is
    /// taking away without diffing both maps key by key — which, like the
    /// install this undoes, was O(registry) per call of a routine declaring an
    /// inner `my sub` (#9073). A recorded install may have *replaced* a key
    /// rather than added it; either way the key's binding differs between the
    /// two maps, which is all the caller needs to know.
    pub(crate) fn keys_installed_since(&self, from: u64, to: u64) -> Option<Vec<Symbol>> {
        let mut keys = Vec::new();
        let mut version = to;
        for _ in 0..PARENT_WALK_LIMIT {
            if version == from {
                return Some(keys);
            }
            let (parent, key) = *self.parents.get(&version)?;
            keys.push(key);
            version = parent;
        }
        (version == from).then_some(keys)
    }

    /// Debug-only: assert that no version ever names two different maps.
    ///
    /// This is the safety net for the inductive argument in the module docs. It
    /// hashes the whole map, so it is `debug_assertions`-only — but it runs over
    /// the entire `prove t/` suite in CI (the `gc-stress-tap` / `jit-stress-tap`
    /// jobs build debug), which is where a broken transition would show up.
    #[cfg(debug_assertions)]
    fn audit(&mut self, table: &FunctionTable) {
        use std::hash::{Hash, Hasher};
        let mut content = 0u64;
        for (key, def) in table.iter() {
            // XOR of per-entry hashes: order-independent, which a HashMap
            // iteration order demands.
            let mut hasher = rustc_hash::FxHasher::default();
            key.hash(&mut hasher);
            (std::sync::Arc::as_ptr(def) as usize).hash(&mut hasher);
            content ^= hasher.finish();
        }
        let version = table.version();
        match self.audit.get(&version) {
            Some(&recorded) => assert_eq!(
                recorded, content,
                "function-table version {version} names two different maps -- a reused \
                 transition was not equivalent (runtime::function_table)"
            ),
            None => {
                if self.audit.len() >= TRANSITION_MEMO_CAP {
                    self.audit.clear();
                }
                self.audit.insert(version, content);
            }
        }
    }

    #[cfg(not(debug_assertions))]
    #[inline]
    fn audit(&mut self, _table: &FunctionTable) {}
}

impl Deref for FunctionTable {
    type Target = FunctionMap;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.map
    }
}

impl FromIterator<(Symbol, std::sync::Arc<FunctionDef>)> for FunctionTable {
    fn from_iter<I: IntoIterator<Item = (Symbol, std::sync::Arc<FunctionDef>)>>(iter: I) -> Self {
        Self {
            map: iter.into_iter().collect(),
            version: next_version(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn def() -> std::sync::Arc<FunctionDef> {
        std::sync::Arc::new(FunctionDef {
            is_cached: false,
            package: Symbol::intern("GLOBAL"),
            name: Symbol::intern("f"),
            params: Vec::new(),
            param_defs: Vec::new(),
            body: Vec::new(),
            is_test_assertion: false,
            is_rw: false,
            is_raw: false,
            declarator: crate::ast::RoutineDeclarator::Sub,
            empty_sig: false,
            is_stub: false,
            return_type: None,
            is_default: false,
            deprecated_message: None,
            source_file: None,
            source_line: None,
            decl_order: 0,
            compiled: None,
            body_fp_cache: std::sync::OnceLock::new(),
            body_facts_cache: std::sync::OnceLock::new(),
        })
    }

    #[test]
    fn a_write_renews_the_version() {
        let mut table = FunctionTable::default();
        let before = table.version();
        table.map_mut().insert(Symbol::intern("GLOBAL::f"), def());
        assert_ne!(table.version(), before);
    }

    #[test]
    fn versions_are_never_reused() {
        let mut table = FunctionTable::default();
        let mut seen = vec![table.version()];
        for i in 0..8 {
            table
                .map_mut()
                .insert(Symbol::intern(&format!("GLOBAL::f{i}")), def());
            seen.push(table.version());
        }
        let unique: std::collections::HashSet<u64> = seen.iter().copied().collect();
        assert_eq!(unique.len(), seen.len(), "a version stamp was re-used");
    }

    /// The property the whole design rests on: a snapshot/restore round trip
    /// gives the *same* version back, so memos taken before the excursion stay
    /// valid after it. A step-counting generation cannot do this.
    /// The other half of the cycle: re-installing the same definition under the
    /// same key, into the same starting map, lands on the state that write
    /// produced before — so the routine's *inside* has a stable generation too,
    /// not just its outside.
    #[test]
    fn repeating_an_install_reuses_the_version_it_produced() {
        let mut transitions = FunctionTableTransitions::default();
        let mut table = std::sync::Arc::new(FunctionTable::default());
        let base = table.version();
        let inner = def();
        let key = Symbol::intern("GLOBAL::inner");

        let snapshot = std::sync::Arc::clone(&table);
        transitions.install(&mut table, key, inner.clone());
        let installed = table.version();
        assert_ne!(installed, base);

        // The routine returns: the scope restore puts the snapshot back, `base`
        // and all.
        table = snapshot;
        assert_eq!(table.version(), base);

        let snapshot = std::sync::Arc::clone(&table);
        transitions.install(&mut table, key, inner);
        assert_eq!(
            table.version(),
            installed,
            "the same write from the same map names the same state"
        );

        // A *different* definition under the same key is a different map, and
        // must not borrow the name of the first one.
        table = snapshot;
        transitions.install(&mut table, key, def());
        assert_ne!(table.version(), installed);
        assert_ne!(table.version(), base);
    }

    /// Directly pins the mechanism the fix in #8935 (issues #8932, #8934)
    /// landed: a live `seen` entry keeps its definition's allocation alive by
    /// retaining its own clone, not by relying on some other owner (the
    /// table, a local variable) to still be around. Unlike
    /// `repeating_an_install_reuses_the_version_it_produced`, this does not
    /// depend on the allocator actually reusing a freed address -- it
    /// observes the retention directly through a `Weak` reference, so it
    /// fails deterministically if the retained clone is ever dropped early.
    #[test]
    fn a_live_memo_entry_keeps_its_definitions_allocation_alive() {
        let mut transitions = FunctionTableTransitions::default();
        let mut table = std::sync::Arc::new(FunctionTable::default());
        let inner = def();
        let weak = std::sync::Arc::downgrade(&inner);

        transitions.install(&mut table, Symbol::intern("GLOBAL::inner"), inner);

        // Every other owner is gone: `inner` was moved into `install`, and
        // the only clone `table.map` held goes away with `table` itself.
        drop(table);

        assert!(
            weak.upgrade().is_some(),
            "the memo's retained clone should keep the allocation alive even \
             after every other owner drops its reference"
        );
    }

    /// The retention `a_live_memo_entry_keeps_its_definitions_allocation_alive`
    /// observes is bounded, not a permanent leak: once the memo fills past
    /// `TRANSITION_MEMO_CAP` and clears (`install`'s `None` branch), the
    /// retained clones go with it and the allocations they were pinning can
    /// finally be freed.
    #[test]
    fn evicting_the_memo_releases_its_retained_allocations() {
        let mut transitions = FunctionTableTransitions::default();
        let mut table = std::sync::Arc::new(FunctionTable::default());
        let inner = def();
        let weak = std::sync::Arc::downgrade(&inner);

        transitions.install(&mut table, Symbol::intern("GLOBAL::inner"), inner);
        // Drop the table's own reference too, so afterwards the *only* thing
        // keeping `inner` alive is the memo's retained clone -- otherwise the
        // fillers below would just prove the table itself still holds it.
        drop(table);
        assert!(
            weak.upgrade().is_some(),
            "sanity: still pinned before eviction"
        );

        // Force enough distinct entries that some install along the way
        // clears `seen` (each key/table-version pair here is unique, so
        // every one of these is a fresh `None`-branch insert). A separate,
        // fresh table for the fillers ensures none of them can accidentally
        // keep `inner`'s key reachable.
        let mut filler_table = std::sync::Arc::new(FunctionTable::default());
        for i in 0..TRANSITION_MEMO_CAP {
            transitions.install(
                &mut filler_table,
                Symbol::intern(&format!("GLOBAL::filler{i}")),
                def(),
            );
        }

        assert!(
            weak.upgrade().is_none(),
            "clearing the memo at its cap should release every retained \
             clone, not leak them"
        );
    }

    /// The steady state the whole change is aiming at: a routine entered and
    /// left repeatedly makes the version alternate between exactly two values,
    /// so memos on both sides of the boundary keep their tags.
    #[test]
    fn an_entry_exit_cycle_settles_on_two_versions() {
        let mut transitions = FunctionTableTransitions::default();
        let mut table = std::sync::Arc::new(FunctionTable::default());
        let inner = def();
        let mut seen = std::collections::HashSet::new();

        for _ in 0..16 {
            let snapshot = std::sync::Arc::clone(&table);
            transitions.install(&mut table, Symbol::intern("GLOBAL::inner"), inner.clone());
            seen.insert(table.version());
            table = snapshot;
            seen.insert(table.version());
        }
        assert_eq!(seen.len(), 2, "the cycle names two states, not thirty-two");
    }

    /// The third and later installs of a recurring transition copy nothing:
    /// they hand back the table the second one built (#9073).
    #[test]
    fn a_recurring_install_reuses_the_table_it_produced() {
        let mut transitions = FunctionTableTransitions::default();
        let mut table = std::sync::Arc::new(FunctionTable::default());
        let inner = def();
        let key = Symbol::intern("GLOBAL::inner");
        let mut installed = Vec::new();
        for _ in 0..3 {
            let snapshot = std::sync::Arc::clone(&table);
            transitions.install(&mut table, key, inner.clone());
            installed.push(std::sync::Arc::clone(&table));
            table = snapshot;
        }
        assert_eq!(installed[0].version(), installed[1].version());
        assert!(
            std::sync::Arc::ptr_eq(&installed[1], &installed[2]),
            "the steady-state install is an Arc hand-back, not a map copy"
        );
        assert!(std::sync::Arc::ptr_eq(&installed[2].map[&key], &inner));

        // The memoized table is never written in place: a write through the
        // registry copies it and names a new version, leaving the memo intact.
        let snapshot = std::sync::Arc::clone(&table);
        transitions.install(&mut table, key, inner.clone());
        let memo_version = table.version();
        crate::runtime::cow_table_mut(&mut table)
            .map_mut()
            .insert(Symbol::intern("GLOBAL::other"), def());
        assert_ne!(table.version(), memo_version);
        table = snapshot;
        transitions.install(&mut table, key, inner);
        assert_eq!(table.version(), memo_version);
        assert!(!table.contains_key(&Symbol::intern("GLOBAL::other")));
    }

    #[test]
    fn keys_installed_since_walks_recorded_installs_only() {
        let mut transitions = FunctionTableTransitions::default();
        let mut table = std::sync::Arc::new(FunctionTable::default());
        let base = table.version();
        let (a, b) = (Symbol::intern("GLOBAL::a"), Symbol::intern("GLOBAL::b"));
        transitions.install(&mut table, a, def());
        transitions.install(&mut table, b, def());
        assert_eq!(
            transitions.keys_installed_since(base, table.version()),
            Some(vec![b, a])
        );
        assert_eq!(
            transitions.keys_installed_since(table.version(), table.version()),
            Some(vec![])
        );
        // An ordinary write in between leaves no parent link to walk.
        crate::runtime::cow_table_mut(&mut table)
            .map_mut()
            .insert(Symbol::intern("GLOBAL::c"), def());
        assert_eq!(
            transitions.keys_installed_since(base, table.version()),
            None
        );
    }

    #[test]
    fn a_restored_snapshot_restores_its_version() {
        let mut table = std::sync::Arc::new(FunctionTable::default());
        std::sync::Arc::make_mut(&mut table)
            .map_mut()
            .insert(Symbol::intern("GLOBAL::outer"), def());
        let base_version = table.version();

        let snapshot = std::sync::Arc::clone(&table);
        std::sync::Arc::make_mut(&mut table)
            .map_mut()
            .insert(Symbol::intern("GLOBAL::inner"), def());
        assert_ne!(table.version(), base_version);
        assert!(table.contains_key(&Symbol::intern("GLOBAL::inner")));

        table = snapshot;
        assert_eq!(table.version(), base_version);
        assert!(!table.contains_key(&Symbol::intern("GLOBAL::inner")));
    }
}
