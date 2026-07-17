//! The cross-thread lexical store, scoped to a spawn lineage (ADR-0010).
//!
//! `clone_for_thread` gives each spawned thread a store that **chains to its
//! parent's** instead of `Arc::clone`-ing one process-wide map. Sibling
//! lineages therefore have no path to each other: two hyper workers that each
//! declare `my $uri` hold two different entries and cannot clobber one another,
//! while a `start` nested inside one worker still chains up to it and sees that
//! worker's `$uri`.
//!
//! Resolution rules (ADR-0010):
//! - **read** — `own`, else walk up the parent chain.
//! - **write** — the nearest ancestor that already has the name (so a child's
//!   mutation of the parent's `$counter` reaches the parent); this lineage's
//!   `own` if no ancestor has it.
//! - **declare** — always this lineage's `own`, shadowing any ancestor entry.

use crate::value::Value;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// One lineage's slice of the cross-thread store, plus a link to the lineage
/// that spawned it.
/// Runtime-internal keys are not lexicals: the atomic element stores, the
/// `state`-variable cells, and the dirty markers are all **explicitly
/// process-wide** shared state whose whole purpose is that every thread sees one
/// of them. `clone_for_thread` never migrates them from the env either. They are
/// therefore resolved at the root lineage, not scoped like a `my` variable —
/// lineage-scoping them would give each sibling thread its own `atomicint`
/// counter or its own `state` cell and silently lose updates.
fn is_internal_key(key: &str) -> bool {
    key.starts_with("__mutsu_")
}

#[derive(Debug)]
pub(crate) struct SharedStore {
    own: RwLock<HashMap<String, Value>>,
    parent: Option<Arc<SharedStore>>,
    /// The root of the chain. `None` when this store *is* the root.
    root: Option<Arc<SharedStore>>,
}

impl SharedStore {
    /// A root store — the main thread's lineage.
    pub(crate) fn root() -> Arc<Self> {
        Arc::new(Self {
            own: RwLock::new(HashMap::new()),
            parent: None,
            root: None,
        })
    }

    /// A child lineage of `parent`. Spawned threads get one of these, so their
    /// own declarations stay private to them while the parent's entries stay
    /// visible and writable through the chain.
    pub(crate) fn child_of(parent: &Arc<Self>) -> Arc<Self> {
        Arc::new(Self {
            own: RwLock::new(HashMap::new()),
            parent: Some(Arc::clone(parent)),
            root: Some(parent.root_ref()),
        })
    }

    /// An `Arc` to the root of this chain (self when this is the root).
    fn root_ref(self: &Arc<Self>) -> Arc<Self> {
        self.root.clone().unwrap_or_else(|| Arc::clone(self))
    }

    /// The lineage an operation on `key` belongs to: the root for a
    /// runtime-internal key, this lineage for a user lexical.
    fn scope_for(&self, key: &str) -> &SharedStore {
        if is_internal_key(key) {
            self.root.as_deref().unwrap_or(self)
        } else {
            self
        }
    }

    /// The lineage at the top of the chain — the process's root store.
    ///
    /// Atomic primitives (`atomicint`, `cas`, the `__mutsu_atomic_*` element
    /// stores) are **explicitly process-wide shared state**, not the lexical
    /// sharing this store scopes: `my atomicint $x; await (^4).map: { start {
    /// $x⚛++ } }` must have all four threads hit one counter. Lineage-scoping
    /// them would give each sibling its own counter and silently lose updates,
    /// so they resolve here instead. This keeps their semantics exactly as they
    /// were before ADR-0010.
    pub(crate) fn root_store(self: &Arc<Self>) -> Arc<Self> {
        let mut cur = Arc::clone(self);
        while let Some(p) = cur.parent.clone() {
            cur = p;
        }
        cur
    }

    /// This lineage's own map. Only for callers that need to hold the lock
    /// across a read-modify-write on the **root** store (the atomic ops), where
    /// it is exactly the single global map they locked before ADR-0010.
    pub(crate) fn own_map(&self) -> &RwLock<HashMap<String, Value>> {
        &self.own
    }

    /// Read a name: this lineage first, then up the chain. An internal key
    /// resolves at the root instead (see `is_internal_key`).
    pub(crate) fn get(&self, key: &str) -> Option<Value> {
        if is_internal_key(key) {
            return self.scope_for(key).own_get(key);
        }
        self.own_get(key)
            .or_else(|| self.parent.as_ref().and_then(|p| p.get(key)))
    }

    fn own_get(&self, key: &str) -> Option<Value> {
        self.own.read().unwrap().get(key).cloned()
    }

    pub(crate) fn contains_key(&self, key: &str) -> bool {
        if is_internal_key(key) {
            return self.scope_for(key).owns(key);
        }
        self.owns(key) || self.parent.as_ref().is_some_and(|p| p.contains_key(key))
    }

    /// True when this lineage itself holds `key` (not an ancestor). A name this
    /// lineage declared is its own, and must not be written through to a parent.
    fn owns(&self, key: &str) -> bool {
        self.own.read().unwrap().contains_key(key)
    }

    /// Write to the nearest lineage that already has the name, so a child's
    /// mutation of a lexical the parent shared with it reaches the parent.
    /// Falls back to this lineage when the name is new (a first write with no
    /// prior declaration behaves as a declaration here).
    pub(crate) fn set(&self, key: &str, value: Value) {
        let target = self.owner_of(key).unwrap_or_else(|| self.scope_for(key));
        target.own.write().unwrap().insert(key.to_string(), value);
    }

    /// Bind the name into THIS lineage, shadowing any ancestor entry. Used by
    /// `my`-declaration seeding: a re-declared name is a fresh binding whose
    /// writes must not leak to the lineage that shared the old one.
    pub(crate) fn declare(&self, key: &str, value: Value) {
        self.scope_for(key)
            .own
            .write()
            .unwrap()
            .insert(key.to_string(), value);
    }

    /// Seed a name only if neither this lineage nor an ancestor already has it.
    /// `clone_for_thread` migrates the parent env this way: an entry an earlier
    /// thread already updated must not be reset to the spawning env's copy.
    pub(crate) fn seed_if_absent(&self, key: &str, value: impl FnOnce() -> Value) {
        if self.contains_key(key) {
            return;
        }
        self.scope_for(key)
            .own
            .write()
            .unwrap()
            .insert(key.to_string(), value());
    }

    /// Fetch `key` when it already holds a cell, else install `make()` in this
    /// lineage and return that. Atomic against a concurrent initialiser in the
    /// *same* lineage (double-checked under `own`'s write lock). A racing
    /// initialiser in an ancestor is not a concern in practice: `state` cells are
    /// seeded into the spawning lineage by `clone_for_thread` *before* the child
    /// starts, so children resolve them through the chain and never re-init.
    pub(crate) fn get_or_init_cell(&self, key: &str, make: impl FnOnce() -> Value) -> Value {
        if let Some(existing) = self.get(key)
            && existing.is_container_ref()
        {
            return existing;
        }
        let mut own = self.scope_for(key).own.write().unwrap();
        if let Some(existing) = own.get(key)
            && existing.is_container_ref()
        {
            return existing.clone();
        }
        let cell = make();
        own.insert(key.to_string(), cell.clone());
        cell
    }

    /// Walk to the lineage that holds `key`, if any.
    fn owner_of(&self, key: &str) -> Option<&SharedStore> {
        if is_internal_key(key) {
            let root = self.scope_for(key);
            return root.owns(key).then_some(root);
        }
        if self.owns(key) {
            return Some(self);
        }
        let mut cur = self.parent.as_deref()?;
        loop {
            if cur.owns(key) {
                return Some(cur);
            }
            cur = cur.parent.as_deref()?;
        }
    }

    /// Mutate the value under `key` in place, in whichever lineage owns it.
    /// Returns None when the name is nowhere in the chain.
    pub(crate) fn with_entry_mut<R>(
        &self,
        key: &str,
        f: impl FnOnce(&mut Value) -> R,
    ) -> Option<R> {
        let target = self.owner_of(key)?;
        let mut guard = target.own.write().unwrap();
        guard.get_mut(key).map(f)
    }

    pub(crate) fn remove(&self, key: &str) {
        if let Some(target) = self.owner_of(key) {
            target.own.write().unwrap().remove(key);
        }
    }

    /// Every name visible from this lineage (own entries shadow ancestors').
    pub(crate) fn visible_keys(&self) -> Vec<String> {
        let mut out: Vec<String> = self.own.read().unwrap().keys().cloned().collect();
        if let Some(p) = &self.parent {
            for k in p.visible_keys() {
                if !out.contains(&k) {
                    out.push(k);
                }
            }
        }
        out
    }

    /// Every value in this lineage AND every ancestor — including entries an
    /// own key shadows. For GC rooting: a shadowed ancestor entry is still
    /// reachable from the lineage that owns it, so it must be visited.
    pub(crate) fn chain_values(&self) -> Vec<Value> {
        let mut out: Vec<Value> = self.own.read().unwrap().values().cloned().collect();
        if let Some(p) = &self.parent {
            out.extend(p.chain_values());
        }
        out
    }

    /// Snapshot every visible (name, value) pair.
    pub(crate) fn visible_entries(&self) -> Vec<(String, Value)> {
        self.visible_keys()
            .into_iter()
            .filter_map(|k| self.get(&k).map(|v| (k, v)))
            .collect()
    }

    /// Drop own entries whose key is not in `keep`. Only touches this lineage —
    /// an ancestor's entries are not this store's to retract.
    pub(crate) fn retain_own<F: Fn(&str) -> bool>(&self, keep: F) {
        self.own.write().unwrap().retain(|k, _| keep(k));
    }
}
