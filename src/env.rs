use std::collections::HashMap;
use std::fmt;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;

use crate::value::Value;

/// Copy-on-write environment wrapper.
///
/// Wraps `Arc<HashMap<String, Value>>` so that cloning is O(1) (just an Arc bump).
/// Mutation goes through `DerefMut` which calls `Arc::make_mut`, triggering a
/// deep clone only when the Arc is shared.
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

impl Deref for Env {
    type Target = HashMap<String, Value>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Env {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Arc::make_mut(&mut self.inner)
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
        // Unwrap Arc if unique, otherwise clone the HashMap
        Arc::try_unwrap(self.inner)
            .unwrap_or_else(|arc| (*arc).clone())
            .into_iter()
    }
}
