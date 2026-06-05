use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, OnceLock};

use crate::symbol::Symbol;
use crate::value::Value;

/// Process-wide immutable "base" tier of the environment.
///
/// Holds the built-in enum constants (`Order`, `Endian`, `ProtocolFamily`,
/// `Signal` and their bare/qualified variant names) -- ~70 entries that are
/// constant for the whole process. They are looked up by name but never
/// mutated, removed, or iterated-over as part of the lexical environment.
///
/// Keeping them out of every per-frame `Env` overlay is the point: each
/// compiled call clones the env and the first write `Arc::make_mut`-deep-copies
/// it, an O(env_size) cost. With ~70 of the ~119 entries hoisted into this
/// shared, never-copied base, the per-call deep copy shrinks to the handful of
/// real lexicals. See docs/vm-dual-store.md (Slice 4b).
static GLOBAL_BASE: OnceLock<HashMap<Symbol, Value>> = OnceLock::new();

/// Install the immutable base tier. Idempotent: the first caller wins and
/// later calls are ignored (the base is identical for every interpreter in a
/// process, so re-installation is a no-op rather than an error).
pub(crate) fn set_global_base(map: HashMap<Symbol, Value>) {
    let _ = GLOBAL_BASE.set(map);
}

#[inline(always)]
fn global_base() -> Option<&'static HashMap<Symbol, Value>> {
    GLOBAL_BASE.get()
}

/// Monotonic, process-global flag: set the first time any closure-writeback
/// metadata key is inserted into *any* env. These keys --
/// `__mutsu_sigilless_readonly::*`, `__mutsu_sigilless_alias::*`,
/// `__mutsu_state_key::*`, `__mutsu_predictive_seq_iter::*` -- drive the
/// per-call sigilless/alias/state-var write-back scans in the closure exit path
/// (`call_compiled_closure`). The common program never creates any of them, so
/// the closure path consults [`closure_meta_keys_possible`] to skip those
/// scans (and their `format!` allocations) entirely.
///
/// Soundness: every such key is created via the String-keyed [`Env::insert`]
/// (always a `format!` result), so detecting them there catches *every*
/// creation site regardless of which of the ~20 scattered callers inserts it --
/// the robustness the per-site approach could not guarantee. The flag is
/// monotonic and global: once `true` it stays `true`, and `true` only ever
/// makes the (correct) scan run, so an over-set is conservative, never wrong.
/// A program's metadata lives in its own (per-thread) env, and the creating
/// `insert` runs earlier in that thread's program order than any closure that
/// reads it, so `Relaxed` ordering suffices.
static CLOSURE_META_KEY_SEEN: AtomicBool = AtomicBool::new(false);

/// True if any closure-writeback metadata key may exist in some env. See
/// [`CLOSURE_META_KEY_SEEN`].
#[inline]
pub(crate) fn closure_meta_keys_possible() -> bool {
    CLOSURE_META_KEY_SEEN.load(Ordering::Relaxed)
}

/// Flip [`CLOSURE_META_KEY_SEEN`] if `key` is a closure-writeback metadata key.
/// The outer `__mutsu_` gate keeps this ~1 byte compare for ordinary lexical
/// names (which never start with `_`).
#[inline]
fn note_closure_meta_key(key: &str) {
    if key.as_bytes().starts_with(b"__mutsu_")
        && (key.starts_with("__mutsu_sigilless_")
            || key.starts_with("__mutsu_state_key::")
            || key.starts_with("__mutsu_predictive_seq_iter::"))
    {
        CLOSURE_META_KEY_SEEN.store(true, Ordering::Relaxed);
    }
}

/// Copy-on-write environment wrapper.
///
/// Wraps `Arc<HashMap<Symbol, Value>>` so that cloning is O(1) (just an Arc bump).
/// Mutation goes through `Arc::make_mut`, triggering a deep clone only when
/// the Arc is shared.  Symbol keys make the deep clone cheaper: key clone is
/// O(1) (Copy) instead of O(n) heap allocation for String keys.
///
/// Name lookups (`get`/`contains_key`/`get_mut`) fall back to the shared
/// immutable [`GLOBAL_BASE`] tier on an overlay miss. Iteration, removal, and
/// in-place value iteration operate on the overlay only -- the base tier is a
/// read-only constant pool (built-in enums), not part of the mutable lexical
/// environment, so it is invisible to `iter`/`keys`/`values`/`len`/`remove`.
#[derive(Clone)]
pub struct Env {
    inner: Arc<HashMap<Symbol, Value>>,
    /// Optional read-through "parent" tier: a snapshot of an enclosing call
    /// frame's overlay. When present (a *scoped* env), name lookups fall through
    /// overlay -> parent -> [`GLOBAL_BASE`], but `insert`/`remove`/`get_mut` and
    /// iteration (`iter`/`keys`/`values`/`len`) operate on the overlay only.
    ///
    /// `parent=None` is the flat env: byte-identical to the pre-scoped behavior,
    /// so every non-converted dispatch path and the ~80 env-iteration consumers
    /// are unaffected. A scoped env is transient -- it is only ever the live
    /// `self.env` during a converted call frame's own opcode execution; anything
    /// that captures/clones the env across a boundary (`clone_env`,
    /// `clone_for_thread`) flattens it first via [`Env::flattened`]. See
    /// docs/vm-dual-store.md (Slice 6).
    parent: Option<Arc<HashMap<Symbol, Value>>>,
}

impl Env {
    pub(crate) fn new() -> Self {
        Self {
            inner: Arc::new(HashMap::new()),
            parent: None,
        }
    }

    /// Create a *scoped child* env: an empty overlay that reads through to
    /// `parent_overlay` (the caller frame's overlay snapshot) and then to
    /// [`GLOBAL_BASE`]. Writes land in the (initially empty) overlay, so the
    /// inherited entries are never `make_mut`-deep-copied. See docs/vm-dual-store.md.
    pub(crate) fn scoped_child(parent_overlay: Arc<HashMap<Symbol, Value>>) -> Self {
        Self {
            inner: Arc::new(HashMap::new()),
            parent: Some(parent_overlay),
        }
    }

    /// True if this env has a parent tier (i.e. it is a scoped child).
    #[inline(always)]
    pub(crate) fn is_scoped(&self) -> bool {
        self.parent.is_some()
    }

    /// O(1) handle to the overlay Arc, for installing a scoped child over it.
    #[inline(always)]
    pub(crate) fn overlay_arc(&self) -> Arc<HashMap<Symbol, Value>> {
        Arc::clone(&self.inner)
    }

    /// Collapse a scoped env into a flat (`parent=None`) env. For a flat env
    /// this is the O(1) `Arc` clone; for a scoped env it materializes
    /// `parent` merged under `overlay` (overlay shadows parent) into a fresh flat
    /// overlay. The base tier is never materialized (it stays shared). Used at
    /// every boundary that captures/clones the env so no full-view iteration
    /// consumer is starved of parent lexicals.
    pub(crate) fn flattened(&self) -> Self {
        match &self.parent {
            None => self.clone(),
            Some(parent) => {
                let mut merged: HashMap<Symbol, Value> = (**parent).clone();
                for (k, v) in self.inner.iter() {
                    merged.insert(*k, v.clone());
                }
                Self {
                    inner: Arc::new(merged),
                    parent: None,
                }
            }
        }
    }

    /// Overlay-only iterator: yields exactly this frame's own writes (the
    /// callee's overlay), excluding parent and base tiers. This is what a
    /// merge-back wants -- the keys the callee actually wrote.
    pub(crate) fn overlay_iter(&self) -> std::collections::hash_map::Iter<'_, Symbol, Value> {
        self.inner.iter()
    }

    /// Check whether two `Env` values point to the same underlying overlay map.
    /// Note: this compares the overlay only; two scoped envs sharing an overlay
    /// but differing in parent would compare equal (callers that rely on ptr_eq
    /// to detect "no writes happened" only ever use it on flat envs).
    #[allow(dead_code)]
    pub(crate) fn ptr_eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }

    #[inline(always)]
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.get_sym(Symbol::intern(key))
    }

    #[inline(always)]
    pub fn get_sym(&self, key: Symbol) -> Option<&Value> {
        if let Some(v) = self.inner.get(&key) {
            return Some(v);
        }
        if let Some(parent) = &self.parent
            && let Some(v) = parent.get(&key)
        {
            return Some(v);
        }
        global_base().and_then(|b| b.get(&key))
    }

    #[inline]
    pub fn contains_key(&self, key: &str) -> bool {
        self.contains_key_sym(Symbol::intern(key))
    }

    #[inline]
    pub fn contains_key_sym(&self, key: Symbol) -> bool {
        self.inner.contains_key(&key)
            || self.parent.as_ref().is_some_and(|p| p.contains_key(&key))
            || global_base().is_some_and(|b| b.contains_key(&key))
    }

    /// Copy-on-write access to the inner map for mutation. Equivalent to
    /// `Arc::make_mut`, but when stats are enabled it records an actual
    /// O(env_size) deep copy whenever the env is shared (the real dual-store
    /// cost; see docs/vm-dual-store.md and `vm_stats::record_env_deep_copy`).
    #[inline]
    fn cow_mut(&mut self) -> &mut HashMap<Symbol, Value> {
        if crate::vm::vm_stats::enabled() && Arc::strong_count(&self.inner) > 1 {
            crate::vm::vm_stats::record_env_deep_copy();
        }
        Arc::make_mut(&mut self.inner)
    }

    #[inline]
    pub fn insert(&mut self, key: String, value: Value) -> Option<Value> {
        note_closure_meta_key(&key);
        let sym = Symbol::intern(&key);
        self.cow_mut().insert(sym, value)
    }

    #[inline]
    pub fn insert_sym(&mut self, key: Symbol, value: Value) -> Option<Value> {
        self.cow_mut().insert(key, value)
    }

    pub fn remove(&mut self, key: &str) -> Option<Value> {
        let sym = Symbol::intern(key);
        self.cow_mut().remove(&sym)
    }

    pub fn remove_sym(&mut self, key: Symbol) -> Option<Value> {
        self.cow_mut().remove(&key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value> {
        self.get_mut_sym(Symbol::intern(key))
    }

    pub fn get_mut_sym(&mut self, key: Symbol) -> Option<&mut Value> {
        // Promote a parent-tier or base-only key into the overlay before handing
        // out a mutable reference, so a write to a caller lexical (scoped env) or
        // a built-in constant lands in this frame's overlay and is not silently
        // lost. Common case (overlay hit, or absent) pays nothing extra.
        if !self.inner.contains_key(&key) {
            let promote = self
                .parent
                .as_ref()
                .and_then(|p| p.get(&key))
                .or_else(|| global_base().and_then(|b| b.get(&key)))
                .cloned();
            if let Some(v) = promote {
                self.cow_mut().insert(key, v);
            }
        }
        self.cow_mut().get_mut(&key)
    }

    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&Symbol, &mut Value) -> bool,
    {
        self.cow_mut().retain(f);
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, Symbol, Value> {
        self.inner.iter()
    }

    pub fn keys(&self) -> std::collections::hash_map::Keys<'_, Symbol, Value> {
        self.inner.keys()
    }

    pub fn values(&self) -> std::collections::hash_map::Values<'_, Symbol, Value> {
        self.inner.values()
    }

    pub fn values_mut(&mut self) -> std::collections::hash_map::ValuesMut<'_, Symbol, Value> {
        self.cow_mut().values_mut()
    }

    /// Full name->value snapshot, merging the immutable base tier under the
    /// overlay (overlay shadows base). Used where a complete view of every
    /// reachable name is required (serialization / cross-context copy), unlike
    /// `iter`/`keys`/`values`, which expose only the mutable overlay.
    pub fn flatten(&self) -> HashMap<String, Value> {
        let mut out: HashMap<String, Value> = global_base()
            .map(|b| b.iter().map(|(k, v)| (k.resolve(), v.clone())).collect())
            .unwrap_or_default();
        if let Some(parent) = &self.parent {
            for (k, v) in parent.iter() {
                out.insert(k.resolve(), v.clone());
            }
        }
        for (k, v) in self.inner.iter() {
            out.insert(k.resolve(), v.clone());
        }
        out
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Insert only if key is not present (in overlay or the base tier).
    pub fn entry_or_insert(&mut self, key: String, value: Value) {
        self.entry_or_insert_sym(Symbol::intern(&key), value);
    }

    /// Insert only if key is not present, keyed directly by an interned Symbol.
    /// Avoids the `resolve()` (Symbol -> String) + re-intern round trip that
    /// `entry_or_insert` pays when the caller already holds a Symbol.
    pub fn entry_or_insert_sym(&mut self, key: Symbol, value: Value) {
        if !self.contains_key_sym(key) {
            self.cow_mut().insert(key, value);
        }
    }

    /// Insert only if key is not present (lazy value).
    pub fn entry_or_insert_with<F: FnOnce() -> Value>(&mut self, key: String, f: F) {
        let sym = Symbol::intern(&key);
        if !self.contains_key_sym(sym) {
            self.cow_mut().insert(sym, f());
        }
    }

    /// Direct access to the inner HashMap (for bulk mutation).
    #[allow(dead_code)]
    pub(crate) fn inner_mut(&mut self) -> &mut HashMap<Symbol, Value> {
        self.cow_mut()
    }

    /// Direct read access to the inner HashMap.
    #[allow(dead_code)]
    pub(crate) fn inner(&self) -> &HashMap<Symbol, Value> {
        &self.inner
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl From<HashMap<String, Value>> for Env {
    fn from(map: HashMap<String, Value>) -> Self {
        let sym_map: HashMap<Symbol, Value> = map
            .into_iter()
            .map(|(k, v)| (Symbol::intern(&k), v))
            .collect();
        Self {
            inner: Arc::new(sym_map),
            parent: None,
        }
    }
}

impl From<HashMap<Symbol, Value>> for Env {
    fn from(map: HashMap<Symbol, Value>) -> Self {
        Self {
            inner: Arc::new(map),
            parent: None,
        }
    }
}

impl fmt::Debug for Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<'a> IntoIterator for &'a Env {
    type Item = (&'a Symbol, &'a Value);
    type IntoIter = std::collections::hash_map::Iter<'a, Symbol, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl IntoIterator for Env {
    type Item = (Symbol, Value);
    type IntoIter = std::collections::hash_map::IntoIter<Symbol, Value>;

    fn into_iter(self) -> Self::IntoIter {
        Arc::try_unwrap(self.inner)
            .unwrap_or_else(|arc| (*arc).clone())
            .into_iter()
    }
}
