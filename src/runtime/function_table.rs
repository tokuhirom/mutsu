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
//! mutation incremented ([`Interpreter::fn_resolve_gen`]). Counting steps makes
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
    /// `(version before, key, definition identity) -> version after`.
    seen: FxHashMap<(u64, Symbol, usize), u64>,
    /// Debug-only: the content hash each version has been observed to name.
    /// Empty (and untouched) in a release build.
    #[cfg(debug_assertions)]
    audit: FxHashMap<u64, u64>,
}

/// How many transitions to remember before starting over.
///
/// A steady-state program cycles through a handful; the table only grows while
/// new routines are being declared, and dropping it merely costs fresh stamps.
const TRANSITION_MEMO_CAP: usize = 4096;

impl FunctionTableTransitions {
    /// Insert `key -> def` into `table`, reusing the version this same write
    /// produced last time it was made from this same starting version.
    pub(crate) fn install(
        &mut self,
        table: &mut FunctionTable,
        key: Symbol,
        def: std::sync::Arc<FunctionDef>,
    ) {
        let memo_key = (table.version(), key, std::sync::Arc::as_ptr(&def) as usize);
        let known = self.seen.get(&memo_key).copied();
        table.map_mut().insert(key, def);
        crate::vm::vm_stats::record_fn_table_transition(known.is_some());
        match known {
            Some(version) => table.set_version(version),
            None => {
                if self.seen.len() >= TRANSITION_MEMO_CAP {
                    self.seen.clear();
                }
                self.seen.insert(memo_key, table.version());
            }
        }
        self.audit(table);
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
        transitions.install(std::sync::Arc::make_mut(&mut table), key, inner.clone());
        let installed = table.version();
        assert_ne!(installed, base);

        // The routine returns: the scope restore puts the snapshot back, `base`
        // and all.
        table = snapshot;
        assert_eq!(table.version(), base);

        let snapshot = std::sync::Arc::clone(&table);
        transitions.install(std::sync::Arc::make_mut(&mut table), key, inner.clone());
        assert_eq!(
            table.version(),
            installed,
            "the same write from the same map names the same state"
        );

        // A *different* definition under the same key is a different map, and
        // must not borrow the name of the first one.
        table = snapshot;
        transitions.install(std::sync::Arc::make_mut(&mut table), key, def());
        assert_ne!(table.version(), installed);
        assert_ne!(table.version(), base);
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
            transitions.install(
                std::sync::Arc::make_mut(&mut table),
                Symbol::intern("GLOBAL::inner"),
                inner.clone(),
            );
            seen.insert(table.version());
            table = snapshot;
            seen.insert(table.version());
        }
        assert_eq!(seen.len(), 2, "the cycle names two states, not thirty-two");
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
