use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use crate::symbol::Symbol;
use crate::value::Value;

/// Copy-on-write environment wrapper.
///
/// Wraps `Arc<HashMap<Symbol, Value>>` so that cloning is O(1) (just an Arc bump).
/// Mutation goes through `Arc::make_mut`, triggering a deep clone only when
/// the Arc is shared.  Symbol keys make the deep clone cheaper: key clone is
/// O(1) (Copy) instead of O(n) heap allocation for String keys.
#[derive(Clone)]
pub struct Env {
    inner: Arc<HashMap<Symbol, Value>>,
}

impl Env {
    pub(crate) fn new() -> Self {
        Self {
            inner: Arc::new(HashMap::new()),
        }
    }

    /// Check whether two `Env` values point to the same underlying HashMap.
    #[allow(dead_code)]
    pub(crate) fn ptr_eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }

    #[inline(always)]
    pub fn get(&self, key: &str) -> Option<&Value> {
        let sym = Symbol::intern(key);
        self.inner.get(&sym)
    }

    #[inline(always)]
    pub fn get_sym(&self, key: Symbol) -> Option<&Value> {
        self.inner.get(&key)
    }

    #[inline]
    pub fn contains_key(&self, key: &str) -> bool {
        let sym = Symbol::intern(key);
        self.inner.contains_key(&sym)
    }

    #[inline]
    pub fn contains_key_sym(&self, key: Symbol) -> bool {
        self.inner.contains_key(&key)
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
        let sym = Symbol::intern(key);
        self.cow_mut().get_mut(&sym)
    }

    pub fn get_mut_sym(&mut self, key: Symbol) -> Option<&mut Value> {
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

    pub fn flatten(&self) -> HashMap<String, Value> {
        self.inner
            .iter()
            .map(|(k, v)| (k.resolve(), v.clone()))
            .collect()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Insert only if key is not present.
    pub fn entry_or_insert(&mut self, key: String, value: Value) {
        let sym = Symbol::intern(&key);
        if !self.inner.contains_key(&sym) {
            self.cow_mut().insert(sym, value);
        }
    }

    /// Insert only if key is not present (lazy value).
    pub fn entry_or_insert_with<F: FnOnce() -> Value>(&mut self, key: String, f: F) {
        let sym = Symbol::intern(&key);
        if !self.inner.contains_key(&sym) {
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
        }
    }
}

impl From<HashMap<Symbol, Value>> for Env {
    fn from(map: HashMap<Symbol, Value>) -> Self {
        Self {
            inner: Arc::new(map),
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
