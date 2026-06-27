use std::collections::{HashMap, HashSet};
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

/// True if `name` is a *plain user lexical* env key — a `my`/`our`-declared
/// scalar/array/hash/sub whose name is an ordinary lowercase identifier (stored
/// scalar-sigilless, e.g. `$x` → `"x"`, `@a` → `"@a"`). These are the only env
/// keys a closure body reaches exclusively through a `GetGlobal`-family opcode,
/// so the compiler's `free_var_syms` set already lists every one a closure can
/// reference. The closure-capture upvalue path
/// ([`crate::runtime::Interpreter::capture_closure_env`], single-store Slice E)
/// uses this to drop the *non-free* plain lexicals from a closure's captured env
/// while keeping everything else.
///
/// Everything that is NOT a plain user lexical is kept by the capture, because a
/// closure body may read it through a dedicated opcode the free-var scan cannot
/// see: `self` (attribute access), special vars (`$_`→`"_"`, `$/`→`"/"`,
/// `$!`→`"!"`, `$?FILE`→`"?FILE"`, …), dynamic vars (`$*x`→`"*x"`/`"$*x"`), match
/// captures (`$0`→`"0"`, `$<n>`→`"<n>"`), `&?ROUTINE`/`&?BLOCK`, the `__mutsu_*`
/// shadow-meta, and type names (uppercase-initial). The classification is
/// therefore deliberately conservative: it returns `true` (droppable) only for a
/// lowercase-identifier-shaped name (optionally `@`/`%`/`&`-sigiled), and `self`
/// — the one lowercase system name read via a dedicated opcode — is excluded.
#[inline]
pub(crate) fn is_plain_user_lexical(name: &str) -> bool {
    if name == "self" {
        return false;
    }
    let b = name.as_bytes();
    let Some(&first) = b.first() else {
        return false;
    };
    // The character that decides the name's "kind": for a sigiled array/hash/sub
    // key it is the char after the sigil; otherwise the first char (scalars are
    // stored sigil-less). A plain user lexical starts there with a lowercase
    // ASCII letter. Anything else — uppercase (types), `?`/`*`/`/`/`!`/`<`
    // (specials/dynamics/captures), a digit (positional captures), `_` (`$_`,
    // `@_`), or `__mutsu_*` — is a system name the capture must keep.
    let decider = if matches!(first, b'@' | b'%' | b'&') {
        b.get(1).copied()
    } else {
        Some(first)
    };
    matches!(decider, Some(c) if c.is_ascii_lowercase())
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
    /// Optional read-through "parent" tier: the enclosing call frame's whole env
    /// (itself possibly scoped, forming a *chain*). When present (a *scoped* env),
    /// name lookups fall through overlay -> parent-chain -> [`GLOBAL_BASE`], but
    /// `insert`/`remove`/`get_mut` and iteration (`iter`/`keys`/`values`/`len`)
    /// operate on this frame's overlay only.
    ///
    /// `parent=None` is the flat env: byte-identical to the pre-scoped behavior,
    /// so every non-converted dispatch path and the ~80 env-iteration consumers
    /// are unaffected. Holding the *whole* parent `Env` (not just its overlay
    /// `HashMap`) is what makes nesting free: a method already under a scoped
    /// overlay that calls another method just chains a new child over it, instead
    /// of `flattened()`-merging the 2-tier env into a flat map per nested call
    /// (the O(env) cost measured in PR #2683 / docs/vm-dual-store.md). The chain
    /// always bottoms out at a flat (`parent=None`) env, so [`GLOBAL_BASE`] is
    /// consulted exactly once at the chain's tail.
    ///
    /// A scoped env is transient -- it is only ever the live `self.env` during a
    /// converted call frame's own opcode execution and the `saved_env` slots that
    /// restore it. Anything that captures the env into a long-lived structure
    /// (a `Value::Sub` closure, an END phaser, a thread) flattens it first via
    /// [`Env::flattened`] / `clone_env`, so an overlay-only iteration consumer is
    /// never starved of parent-chain lexicals. See docs/vm-dual-store.md (Slice 6).
    parent: Option<Arc<Env>>,
    /// Tombstones: keys `remove`d in this scoped overlay that still exist in the
    /// `parent`/base tier. Because the overlay can only *add* shadowing entries,
    /// a plain overlay `remove` cannot hide a parent key; a tombstone records
    /// "this key is deleted in this scope" so `get`/`contains_key` stop falling
    /// through to the parent. This is what makes "clear inherited state" idioms
    /// (e.g. `my $x` clearing an outer `\x`'s `__mutsu_sigilless_readonly::x`)
    /// behave correctly under a scoped overlay. `None` for flat envs (no scope to
    /// shadow). Tombstones are dropped with the overlay on return and are never
    /// merged back (a callee-local removal must not delete the caller's key).
    tombstones: Option<HashSet<Symbol>>,
    /// Number of parent tiers below this env (0 for a flat env). Used to bound
    /// the chain length: a recursive function would otherwise grow the chain one
    /// tier per call, making `get`/`Drop`/`flattened` recurse to the recursion
    /// depth. [`scoped_child`] flattens the parent once the chain reaches
    /// [`MAX_OVERLAY_DEPTH`], so the chain length (hence lookup cost and `Drop`
    /// recursion) stays O(1) while shallow nesting (methods, ~2-5 deep) pays no
    /// flatten.
    depth: u16,
}

/// Maximum overlay chain length before [`Env::scoped_child`] flattens the parent.
/// Typical method/function nesting is a handful of tiers, well under this; only
/// deep recursion ever hits it, paying one O(env) flatten per this many frames.
const MAX_OVERLAY_DEPTH: u16 = 16;

impl Env {
    pub(crate) fn new() -> Self {
        Self {
            inner: Arc::new(HashMap::new()),
            parent: None,
            tombstones: None,
            depth: 0,
        }
    }

    /// Build a flat (`parent=None`) env directly from a pre-built overlay map.
    /// Name lookups still fall through to [`GLOBAL_BASE`] at the tail, so the
    /// built-in enum constants remain reachable without copying them in. Used by
    /// the closure-capture upvalue path (single-store Slice E) to materialize a
    /// snapshot holding only a closure's free variables, the shadow-meta, and the
    /// system names a body may read through a dedicated opcode — not a flatten of
    /// every plain user lexical in scope.
    pub(crate) fn from_symbol_map(map: HashMap<Symbol, Value>) -> Self {
        Self {
            inner: Arc::new(map),
            parent: None,
            tombstones: None,
            depth: 0,
        }
    }

    /// Create a *scoped child* env: an empty overlay that reads through to
    /// `parent` (the whole caller-frame env, itself possibly scoped) and then to
    /// [`GLOBAL_BASE`]. Writes land in the (initially empty) overlay, so the
    /// inherited entries are never `make_mut`-deep-copied, and -- because the
    /// parent is the *whole* env rather than a flattened overlay -- chaining over
    /// an already-scoped env is O(1) (no per-nested-call flatten). To keep the
    /// chain from growing unbounded under deep recursion, the parent is flattened
    /// to a single tier once it reaches [`MAX_OVERLAY_DEPTH`]. See
    /// docs/vm-dual-store.md.
    pub(crate) fn scoped_child(parent: Env) -> Self {
        let parent = if parent.depth >= MAX_OVERLAY_DEPTH {
            parent.flattened()
        } else {
            parent
        };
        Self {
            inner: Arc::new(HashMap::new()),
            depth: parent.depth + 1,
            parent: Some(Arc::new(parent)),
            tombstones: None,
        }
    }

    /// True if `key` is tombstoned (removed) in this scoped overlay.
    #[inline(always)]
    fn is_tombstoned(&self, key: Symbol) -> bool {
        self.tombstones.as_ref().is_some_and(|t| t.contains(&key))
    }

    /// True if this env has a parent tier (i.e. it is a scoped child).
    #[inline(always)]
    pub(crate) fn is_scoped(&self) -> bool {
        self.parent.is_some()
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
                // Recursively collapse the parent chain to a flat overlay first
                // (the base tier stays shared, never materialized), then layer
                // this frame's tombstones and overlay on top.
                let parent_flat = parent.flattened();
                let mut merged: HashMap<Symbol, Value> = (*parent_flat.inner).clone();
                // Apply tombstones: a key removed in this scope must not survive
                // into the flattened (flat) env.
                if let Some(tomb) = &self.tombstones {
                    for k in tomb {
                        merged.remove(k);
                    }
                }
                for (k, v) in self.inner.iter() {
                    merged.insert(*k, v.clone());
                }
                Self {
                    inner: Arc::new(merged),
                    parent: None,
                    tombstones: None,
                    depth: 0,
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

    #[inline]
    pub fn get_sym(&self, key: Symbol) -> Option<&Value> {
        if let Some(v) = self.inner.get(&key) {
            return Some(v);
        }
        // A tombstoned key is deleted in this scope: do not fall through to the
        // parent/base tier.
        if self.is_tombstoned(key) {
            return None;
        }
        // Walk the parent chain (each tier may itself be scoped). The chain
        // bottoms out at a flat env, which consults GLOBAL_BASE; intermediate
        // tiers do not, so the base is checked exactly once.
        if let Some(parent) = &self.parent {
            return parent.get_sym(key);
        }
        global_base().and_then(|b| b.get(&key))
    }

    #[inline]
    pub fn contains_key(&self, key: &str) -> bool {
        self.contains_key_sym(Symbol::intern(key))
    }

    #[inline]
    pub fn contains_key_sym(&self, key: Symbol) -> bool {
        if self.inner.contains_key(&key) {
            return true;
        }
        if self.is_tombstoned(key) {
            return false;
        }
        if let Some(parent) = &self.parent {
            return parent.contains_key_sym(key);
        }
        global_base().is_some_and(|b| b.contains_key(&key))
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

    /// Clear a tombstone for `key` (it is being re-inserted, so it is no longer
    /// deleted in this scope). No-op for flat envs / un-tombstoned keys.
    #[inline(always)]
    fn untombstone(&mut self, key: Symbol) {
        if let Some(t) = &mut self.tombstones {
            t.remove(&key);
        }
    }

    #[inline]
    pub fn insert(&mut self, key: String, value: Value) -> Option<Value> {
        note_closure_meta_key(&key);
        let sym = Symbol::intern(&key);
        self.untombstone(sym);
        self.cow_mut().insert(sym, value)
    }

    #[inline]
    pub fn insert_sym(&mut self, key: Symbol, value: Value) -> Option<Value> {
        self.untombstone(key);
        self.cow_mut().insert(key, value)
    }

    pub fn remove(&mut self, key: &str) -> Option<Value> {
        self.remove_sym(Symbol::intern(key))
    }

    pub fn remove_sym(&mut self, key: Symbol) -> Option<Value> {
        let from_overlay = self.cow_mut().remove(&key);
        // Scoped env: if the key still exists in the parent/base tier, record a
        // tombstone so it stops shadowing through. The visible value before
        // removal is the overlay value if present, else the parent/base value.
        if self.parent.is_some() {
            // Chain-aware lookup (covers the base tier at the chain tail).
            let parent_val = self
                .parent
                .as_ref()
                .and_then(|p| p.get_sym(key))
                .or_else(|| global_base().and_then(|b| b.get(&key)))
                .cloned();
            if parent_val.is_some() {
                let visible = from_overlay.or(parent_val);
                self.tombstones.get_or_insert_with(HashSet::new).insert(key);
                return visible;
            }
        }
        from_overlay
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value> {
        self.get_mut_sym(Symbol::intern(key))
    }

    pub fn get_mut_sym(&mut self, key: Symbol) -> Option<&mut Value> {
        // Promote a parent-tier or base-only key into the overlay before handing
        // out a mutable reference, so a write to a caller lexical (scoped env) or
        // a built-in constant lands in this frame's overlay and is not silently
        // lost. Common case (overlay hit, or absent) pays nothing extra. A
        // tombstoned key is deleted in this scope, so it is not promoted.
        if !self.inner.contains_key(&key) {
            if self.is_tombstoned(key) {
                return None;
            }
            // Promote from the parent chain (chain-aware get_sym already covers
            // the base tier at the chain tail) so a write to a caller lexical
            // lands in this frame's overlay and is not silently lost.
            let promote = self
                .parent
                .as_ref()
                .and_then(|p| p.get_sym(key))
                .or_else(|| global_base().and_then(|b| b.get(&key)))
                .cloned();
            if let Some(v) = promote {
                self.untombstone(key);
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
        // Build the parent view first (the chain tail seeds GLOBAL_BASE), then
        // layer this frame's tombstones and overlay on top.
        let mut out: HashMap<String, Value> = match &self.parent {
            Some(parent) => parent.flatten(),
            None => global_base()
                .map(|b| b.iter().map(|(k, v)| (k.resolve(), v.clone())).collect())
                .unwrap_or_default(),
        };
        if let Some(tomb) = &self.tombstones {
            for k in tomb {
                out.remove(&k.resolve());
            }
        }
        for (k, v) in self.inner.iter() {
            out.insert(k.resolve(), v.clone());
        }
        out
    }

    /// Full symbol-keyed snapshot filtered during collection, without first
    /// materializing every reachable entry into an intermediate flat env.
    pub(crate) fn filtered_symbol_map<F>(&self, keep: &mut F) -> HashMap<Symbol, Value>
    where
        F: FnMut(Symbol, &Value) -> bool,
    {
        let mut out: HashMap<Symbol, Value> = match &self.parent {
            Some(parent) => parent.filtered_symbol_map(keep),
            None => global_base()
                .map(|b| {
                    b.iter()
                        .filter_map(|(k, v)| keep(*k, v).then(|| (*k, v.clone())))
                        .collect()
                })
                .unwrap_or_default(),
        };
        if let Some(tomb) = &self.tombstones {
            for k in tomb {
                out.remove(k);
            }
        }
        for (k, v) in self.inner.iter() {
            if keep(*k, v) {
                out.insert(*k, v.clone());
            } else {
                out.remove(k);
            }
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
            tombstones: None,
            depth: 0,
        }
    }
}

impl From<HashMap<Symbol, Value>> for Env {
    fn from(map: HashMap<Symbol, Value>) -> Self {
        Self {
            inner: Arc::new(map),
            parent: None,
            tombstones: None,
            depth: 0,
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

#[cfg(test)]
mod tests {
    use super::*;

    fn s(name: &str) -> Symbol {
        Symbol::intern(name)
    }

    #[test]
    fn plain_user_lexical_classification() {
        // Plain user lexicals (droppable by the closure upvalue capture): a
        // lowercase-identifier scalar (stored sigil-less) or @/%/&-sigiled var.
        for k in ["x", "c", "count", "@arr", "%h", "&helper", "longname"] {
            assert!(
                is_plain_user_lexical(k),
                "{k} should be a plain user lexical"
            );
        }
        // System names the capture must keep: self, topic/match/error, compile-time
        // `?` vars, dynamic `*` vars, match captures, &?ROUTINE, types, meta.
        for k in [
            "self",
            "_",
            "/",
            "!",
            "?FILE",
            "?LINE",
            "?CLASS",
            "*REPO",
            "@*ARGS",
            "%*ENV",
            "$*HOME",
            "0",
            "1",
            "<name>",
            "&?ROUTINE",
            "&?BLOCK",
            "@_",
            "Cache",
            "Any",
            "__mutsu_type::x",
        ] {
            assert!(
                !is_plain_user_lexical(k),
                "{k} should be kept (not a plain user lexical)"
            );
        }
        // Empty string is not a plain user lexical.
        assert!(!is_plain_user_lexical(""));
    }

    fn scoped_with(parent: Env, writes: &[(&str, i64)]) -> Env {
        let mut e = Env::scoped_child(parent);
        for (k, v) in writes {
            e.insert(k.to_string(), Value::Int(*v));
        }
        e
    }

    #[test]
    fn chain_lookup_reads_through_all_tiers() {
        let mut root = Env::new();
        root.insert("a".into(), Value::Int(1));
        let mid = scoped_with(root, &[("b", 2)]);
        let leaf = scoped_with(mid, &[("c", 3)]);
        // Each tier's key is visible from the leaf.
        assert_eq!(leaf.get_sym(s("a")), Some(&Value::Int(1)));
        assert_eq!(leaf.get_sym(s("b")), Some(&Value::Int(2)));
        assert_eq!(leaf.get_sym(s("c")), Some(&Value::Int(3)));
        assert!(leaf.get_sym(s("missing")).is_none());
        assert!(leaf.contains_key_sym(s("a")));
        assert!(!leaf.contains_key_sym(s("missing")));
    }

    #[test]
    fn overlay_shadows_parent() {
        let mut root = Env::new();
        root.insert("x".into(), Value::Int(1));
        let leaf = scoped_with(root, &[("x", 99)]);
        assert_eq!(leaf.get_sym(s("x")), Some(&Value::Int(99)));
    }

    #[test]
    fn iter_is_overlay_only() {
        let mut root = Env::new();
        root.insert("a".into(), Value::Int(1));
        let leaf = scoped_with(root, &[("b", 2)]);
        let keys: Vec<String> = leaf.iter().map(|(k, _)| k.resolve()).collect();
        // Only the leaf's own write, not the parent's `a`.
        assert_eq!(keys, vec!["b".to_string()]);
    }

    #[test]
    fn tombstone_hides_parent_key_through_chain() {
        let mut root = Env::new();
        root.insert("a".into(), Value::Int(1));
        let mut leaf = Env::scoped_child(root);
        let removed = leaf.remove("a");
        assert_eq!(removed, Some(Value::Int(1)));
        // Hidden in this scope, but not deleted from the parent tier.
        assert!(leaf.get_sym(s("a")).is_none());
        assert!(!leaf.contains_key_sym(s("a")));
        // Re-inserting clears the tombstone.
        leaf.insert("a".into(), Value::Int(5));
        assert_eq!(leaf.get_sym(s("a")), Some(&Value::Int(5)));
    }

    #[test]
    fn flattened_preserves_full_view_and_tombstones() {
        let mut root = Env::new();
        root.insert("a".into(), Value::Int(1));
        root.insert("gone".into(), Value::Int(7));
        let mid = scoped_with(root, &[("b", 2)]);
        let mut leaf = mid;
        leaf.insert("c".into(), Value::Int(3));
        leaf.remove("gone");
        let flat = leaf.flattened();
        assert!(!flat.is_scoped());
        assert_eq!(flat.depth, 0);
        assert_eq!(flat.get_sym(s("a")), Some(&Value::Int(1)));
        assert_eq!(flat.get_sym(s("b")), Some(&Value::Int(2)));
        assert_eq!(flat.get_sym(s("c")), Some(&Value::Int(3)));
        assert!(flat.get_sym(s("gone")).is_none());
    }

    #[test]
    fn depth_is_bounded_under_deep_nesting() {
        // Chaining far past MAX_OVERLAY_DEPTH must keep the chain length bounded
        // (scoped_child flattens the parent at the limit) while still reading the
        // deepest lexical correctly.
        let mut env = Env::new();
        env.insert("root".into(), Value::Int(42));
        for i in 0..(MAX_OVERLAY_DEPTH as i64 * 4) {
            env = scoped_with(env, &[("tmp", i)]);
            assert!(
                env.depth <= MAX_OVERLAY_DEPTH,
                "depth {} exceeded bound {}",
                env.depth,
                MAX_OVERLAY_DEPTH
            );
        }
        // The original root lexical is still reachable through the flattened tiers.
        assert_eq!(env.get_sym(s("root")), Some(&Value::Int(42)));
    }
}
