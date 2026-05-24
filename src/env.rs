use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use crate::value::Value;

/// Copy-on-write environment wrapper.
///
/// Wraps `Arc<HashMap<String, Value>>` so that cloning is O(1) (just an Arc bump).
/// Mutation goes through `Arc::make_mut`, triggering a deep clone only when
/// the Arc is shared.
///
/// Previously exposed `HashMap` via `Deref`/`DerefMut`. Now uses explicit
/// methods to allow future scope-chain extensions without changing callers.
#[derive(Clone)]
pub struct Env {
    inner: Arc<HashMap<String, Value>>,
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
        self.inner.get(key)
    }

    #[inline]
    pub fn contains_key(&self, key: &str) -> bool {
        self.inner.contains_key(key)
    }

    #[inline]
    pub fn insert(&mut self, key: String, value: Value) -> Option<Value> {
        Arc::make_mut(&mut self.inner).insert(key, value)
    }

    pub fn remove(&mut self, key: &str) -> Option<Value> {
        Arc::make_mut(&mut self.inner).remove(key)
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value> {
        Arc::make_mut(&mut self.inner).get_mut(key)
    }

    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&String, &mut Value) -> bool,
    {
        Arc::make_mut(&mut self.inner).retain(f);
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, String, Value> {
        self.inner.iter()
    }

    pub fn keys(&self) -> std::collections::hash_map::Keys<'_, String, Value> {
        self.inner.keys()
    }

    pub fn values(&self) -> std::collections::hash_map::Values<'_, String, Value> {
        self.inner.values()
    }

    pub fn values_mut(&mut self) -> std::collections::hash_map::ValuesMut<'_, String, Value> {
        Arc::make_mut(&mut self.inner).values_mut()
    }

    pub fn flatten(&self) -> HashMap<String, Value> {
        (*self.inner).clone()
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
        if !self.inner.contains_key(&key) {
            Arc::make_mut(&mut self.inner).insert(key, value);
        }
    }

    /// Insert only if key is not present (lazy value).
    pub fn entry_or_insert_with<F: FnOnce() -> Value>(&mut self, key: String, f: F) {
        if !self.inner.contains_key(&key) {
            Arc::make_mut(&mut self.inner).insert(key, f());
        }
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl From<HashMap<String, Value>> for Env {
    fn from(map: HashMap<String, Value>) -> Self {
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
    type Item = (&'a String, &'a Value);
    type IntoIter = std::collections::hash_map::Iter<'a, String, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl IntoIterator for Env {
    type Item = (String, Value);
    type IntoIter = std::collections::hash_map::IntoIter<String, Value>;

    fn into_iter(self) -> Self::IntoIter {
        Arc::try_unwrap(self.inner)
            .unwrap_or_else(|arc| (*arc).clone())
            .into_iter()
    }
}
