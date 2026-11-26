//! [`DynBase`]: an env base tier — the immutable map of built-in dynamics and
//! constants every lookup falls through to — plus *lazy entries*, whose value
//! is built on the first lookup (#19224, the rule of ADR-19206).
//!
//! `%*ENV` is the motivating entry: building it reads every environment
//! variable and runs `val()` on each value, ~15% of a `say "hello"`'s
//! instructions, for a hash most programs never name. A lazy entry is a key, a
//! builder injected by the runtime (this layer must not name it), and a
//! `OnceLock` for the value.
//!
//! The tier keeps the shape callers relied on when it was a plain map:
//!
//! - [`DynBase::get`] answers from the map, then from the lazy entries,
//!   building one on its first lookup. A hit in the map costs what it did; a
//!   miss pays one emptiness test, and only a base with lazy entries (the
//!   per-interpreter one) looks further.
//! - [`DynBase::contains_key`] and [`DynBase::keys`] answer without building.
//! - [`DynBase::iter`] yields every entry and so builds the lazy ones; the
//!   walks that need values (`flatten`, the capture merge, the `DYNAMIC::`
//!   listing) are not on the start-up path.
//! - [`DynBase::built_values`] is for the GC: an unbuilt entry holds nothing.
//!
//! A clone shares a built value (a `Value` clone is a handle) and carries an
//! unbuilt entry unbuilt, as the `Arc`'d map shared its values before.

use std::sync::OnceLock;

use crate::env_tier::SymMap;
use crate::symbol::Symbol;
use crate::value::Value;

/// A base-tier entry built on first lookup.
#[derive(Clone)]
struct LazyEntry {
    key: Symbol,
    build: fn() -> Value,
    value: OnceLock<Value>,
}

impl LazyEntry {
    // Cost: O(1) once built; the first call is the builder's cost.
    fn value(&self) -> &Value {
        self.value.get_or_init(self.build)
    }
}

/// See the module docs.
#[derive(Clone, Default)]
pub(crate) struct DynBase {
    map: SymMap,
    lazy: Vec<LazyEntry>,
}

impl From<SymMap> for DynBase {
    fn from(map: SymMap) -> Self {
        Self {
            map,
            lazy: Vec::new(),
        }
    }
}

impl DynBase {
    // Cost: O(1) expected; a lazy entry's first lookup also builds it.
    #[inline]
    pub(crate) fn get(&self, key: &Symbol) -> Option<&Value> {
        match self.map.get(key) {
            Some(value) => Some(value),
            None if self.lazy.is_empty() => None,
            None => self.lazy_get(*key),
        }
    }

    // Cost: O(l), l = lazy entries (a handful); builds a matching one.
    #[cold]
    fn lazy_get(&self, key: Symbol) -> Option<&Value> {
        self.lazy
            .iter()
            .find(|entry| entry.key == key)
            .map(LazyEntry::value)
    }

    // Cost: O(1) expected, plus O(l); never builds.
    #[inline]
    pub(crate) fn contains_key(&self, key: &Symbol) -> bool {
        self.map.contains_key(key) || self.lazy.iter().any(|entry| entry.key == *key)
    }

    /// Whether `key` is a lazy entry that has not been built yet.
    // Cost: O(l), l = lazy entries.
    pub(crate) fn is_unbuilt(&self, key: &Symbol) -> bool {
        !self.map.contains_key(key)
            && self
                .lazy
                .iter()
                .any(|entry| entry.key == *key && entry.value.get().is_none())
    }

    /// Every key, lazy ones included, without building any.
    // Cost: O(1) to create; walking it is O(n + l).
    pub(crate) fn keys(&self) -> impl Iterator<Item = &Symbol> {
        self.map
            .keys()
            .chain(self.lazy.iter().map(|entry| &entry.key))
    }

    /// Every entry, building the lazy ones.
    // Cost: O(1) to create; walking it is O(n + l), plus each lazy entry's
    // first build.
    pub(crate) fn iter(&self) -> impl Iterator<Item = (&Symbol, &Value)> {
        self.map
            .iter()
            .chain(self.lazy.iter().map(|entry| (&entry.key, entry.value())))
    }

    /// The values that exist: every map entry and the lazy entries already
    /// built (the GC's roots; an unbuilt entry holds nothing yet).
    // Cost: O(1) to create; walking it is O(n + l).
    pub(crate) fn built_values(&self) -> impl Iterator<Item = &Value> {
        self.map
            .values()
            .chain(self.lazy.iter().filter_map(|entry| entry.value.get()))
    }

    /// Set `key`, replacing a lazy entry of that name.
    // Cost: O(1) expected, plus O(l).
    pub(crate) fn insert(&mut self, key: Symbol, value: Value) {
        self.lazy.retain(|entry| entry.key != key);
        self.map.insert(key, value);
    }

    /// Set `key` unless the tier already has it.
    // Cost: O(1) expected, plus O(l).
    pub(crate) fn insert_if_absent(&mut self, key: Symbol, value: impl FnOnce() -> Value) {
        if !self.contains_key(&key) {
            self.map.insert(key, value());
        }
    }

    /// Give `key` a value built by `build` on its first lookup, unless the
    /// tier already has `key`.
    // Cost: O(1) expected, plus O(l).
    pub(crate) fn insert_lazy(&mut self, key: Symbol, build: fn() -> Value) {
        if !self.contains_key(&key) {
            self.lazy.push(LazyEntry {
                key,
                build,
                value: OnceLock::new(),
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static BUILDS: AtomicUsize = AtomicUsize::new(0);

    fn build() -> Value {
        BUILDS.fetch_add(1, Ordering::Relaxed);
        Value::int(7)
    }

    #[test]
    fn a_lazy_entry_is_built_once_on_first_lookup() {
        let key = Symbol::intern("%*__dyn_base_test_lazy");
        let mut base = DynBase::default();
        base.insert_lazy(key, build);
        let before = BUILDS.load(Ordering::Relaxed);
        assert!(base.contains_key(&key));
        assert_eq!(base.keys().filter(|k| **k == key).count(), 1);
        assert_eq!(base.built_values().count(), 0);
        assert_eq!(BUILDS.load(Ordering::Relaxed), before);
        assert_eq!(
            base.get(&key).map(|v| v.to_string_value()),
            Some("7".into())
        );
        let copy = base.clone();
        assert_eq!(
            copy.get(&key).map(|v| v.to_string_value()),
            Some("7".into())
        );
        assert_eq!(BUILDS.load(Ordering::Relaxed), before + 1);
        assert_eq!(base.built_values().count(), 1);
    }

    #[test]
    fn insert_replaces_a_lazy_entry() {
        let key = Symbol::intern("%*__dyn_base_test_replace");
        let mut base = DynBase::default();
        base.insert_lazy(key, build);
        base.insert(key, Value::int(1));
        assert_eq!(base.iter().filter(|(k, _)| **k == key).count(), 1);
        assert_eq!(
            base.get(&key).map(|v| v.to_string_value()),
            Some("1".into())
        );
    }
}
