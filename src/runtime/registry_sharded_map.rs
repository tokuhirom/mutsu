//! A registry table split into copy-on-write shards (ADR-17216 §4).
//!
//! The registry is world state shared by every thread, published by
//! read-copy-update: a write copies what another thread's snapshot still
//! shares. A [`CowTable`](super::registry_cow_table::CowTable) copies the
//! whole table on such a write, which is `O(table)` per declaration. The type
//! tables of a large program (Red: thousands of classes and roles) are
//! written on every query that declares a type (a `my role`, a `class ::`, a
//! `but` mixin, a MOP `new_type`), so with several threads declaring at once
//! every write paid for a copy of the whole class table (#17217).
//!
//! A [`ShardedMap`] keeps the same `HashMap` interface but splits the entries
//! over [`SHARDS`] shards by key hash, each behind its own `Arc`. Cloning the
//! map is `SHARDS` reference-count increments; a write copies the one shard it
//! touches (when shared), so a write costs `O(n / SHARDS)` instead of `O(n)`.
//! A read is one hash plus the shard's own lookup.

use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::hash::{BuildHasher, Hash};
use std::sync::Arc;

use rustc_hash::{FxBuildHasher, FxHashMap};

/// The number of shards. A power of two, so the shard index is a mask.
pub(crate) const SHARDS: usize = 32;

/// See the module docs.
#[derive(Debug)]
pub(crate) struct ShardedMap<K, V> {
    shards: Box<[Arc<FxHashMap<K, V>>; SHARDS]>,
    len: usize,
    /// Replaced by a process-unique value whenever a key is added or
    /// removed: an index derived from the key set is current exactly while
    /// this has not moved (see [`Self::key_generation`]).
    key_gen: u64,
}

impl<K, V> Clone for ShardedMap<K, V> {
    // Cost: O(SHARDS), one reference-count increment per shard.
    fn clone(&self) -> Self {
        Self {
            shards: Box::new(std::array::from_fn(|i| Arc::clone(&self.shards[i]))),
            len: self.len,
            key_gen: self.key_gen,
        }
    }
}

impl<K, V> Default for ShardedMap<K, V> {
    // Cost: O(SHARDS).
    fn default() -> Self {
        Self {
            shards: Box::new(std::array::from_fn(|_| Arc::new(FxHashMap::default()))),
            len: 0,
            key_gen: crate::runtime::registry_gen::next_generation(),
        }
    }
}

/// The shard holding `key`.
// Cost: O(|key|), one hash.
#[inline]
fn shard_of<Q: Hash + ?Sized>(key: &Q) -> usize {
    // The upper bits: FxHash mixes them best, and the shard's own table
    // indexes by the lower ones.
    (FxBuildHasher.hash_one(key) >> 59) as usize & (SHARDS - 1)
}

impl<K: Hash + Eq, V> ShardedMap<K, V> {
    // Cost: O(|key|) expected.
    #[inline]
    pub(crate) fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.shards[shard_of(key)].get(key)
    }

    // Cost: O(|key|) expected.
    #[inline]
    pub(crate) fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.shards[shard_of(key)].contains_key(key)
    }

    /// Names the current key set: equal values mean the same keys (values
    /// may differ).
    // Cost: O(1).
    #[inline]
    pub(crate) fn key_generation(&self) -> u64 {
        self.key_gen
    }

    // Cost: O(1).
    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.len
    }

    // Cost: O(1).
    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.len == 0
    }

    // Cost: O(1) to create; walking it is O(n + SHARDS).
    pub(crate) fn iter(&self) -> impl Iterator<Item = (&K, &V)> {
        self.shards.iter().flat_map(|shard| shard.iter())
    }

    // Cost: O(1) to create; walking it is O(n + SHARDS).
    pub(crate) fn keys(&self) -> impl Iterator<Item = &K> {
        self.shards.iter().flat_map(|shard| shard.keys())
    }

    // Cost: O(1) to create; walking it is O(n + SHARDS).
    pub(crate) fn values(&self) -> impl Iterator<Item = &V> {
        self.shards.iter().flat_map(|shard| shard.values())
    }
}

impl<K: Hash + Eq + Clone, V: Clone> ShardedMap<K, V> {
    /// The shard holding `key`, copied first when another version shares it.
    // Cost: O(1), or O(n / SHARDS) when the shard is shared (the copy).
    #[inline]
    fn shard_mut(&mut self, index: usize) -> &mut FxHashMap<K, V> {
        Arc::make_mut(&mut self.shards[index])
    }

    // Cost: O(|key|) expected, plus the shard copy when shared.
    pub(crate) fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let index = shard_of(key);
        // Probe first: a miss must not copy the shard.
        if !self.shards[index].contains_key(key) {
            return None;
        }
        self.shard_mut(index).get_mut(key)
    }

    // Cost: O(|key|) expected, plus the shard copy when shared.
    pub(crate) fn insert(&mut self, key: K, value: V) -> Option<V> {
        let old = self.shard_mut(shard_of(&key)).insert(key, value);
        if old.is_none() {
            self.len += 1;
            self.key_gen = crate::runtime::registry_gen::next_generation();
        }
        old
    }

    // Cost: O(|key|) expected, plus the shard copy when shared.
    pub(crate) fn remove<Q>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let index = shard_of(key);
        if !self.shards[index].contains_key(key) {
            return None;
        }
        let old = self.shard_mut(index).remove(key);
        if old.is_some() {
            self.len -= 1;
            self.key_gen = crate::runtime::registry_gen::next_generation();
        }
        old
    }

    /// The entry for `key` (see [`ShardedEntry`]).
    // Cost: O(|key|) expected, plus the shard copy when shared.
    pub(crate) fn entry(&mut self, key: K) -> ShardedEntry<'_, K, V> {
        let shard = Arc::make_mut(&mut self.shards[shard_of(&key)]);
        ShardedEntry {
            entry: shard.entry(key),
            len: &mut self.len,
            key_gen: &mut self.key_gen,
        }
    }

    // Cost: O(n + SHARDS); copies every shared shard `f` keeps or drops from.
    pub(crate) fn retain(&mut self, mut f: impl FnMut(&K, &mut V) -> bool) {
        let mut len = 0;
        for shard in self.shards.iter_mut() {
            if shard.is_empty() {
                continue;
            }
            Arc::make_mut(shard).retain(|k, v| f(k, v));
            len += shard.len();
        }
        if len != self.len {
            self.key_gen = crate::runtime::registry_gen::next_generation();
        }
        self.len = len;
    }

}

impl<K: Hash + Eq + Clone, V: Clone> Extend<(K, V)> for ShardedMap<K, V> {
    // Cost: O(m) expected for m items, plus the shard copies.
    fn extend<I: IntoIterator<Item = (K, V)>>(&mut self, iter: I) {
        for (k, v) in iter {
            self.insert(k, v);
        }
    }
}

impl<K: Hash + Eq + Clone, V: Clone> FromIterator<(K, V)> for ShardedMap<K, V> {
    // Cost: O(m) expected for m items.
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let mut map = Self::default();
        map.extend(iter);
        map
    }
}

impl<'a, K: Hash + Eq, V> IntoIterator for &'a ShardedMap<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = Box<dyn Iterator<Item = (&'a K, &'a V)> + 'a>;
    // Cost: O(1) to create.
    fn into_iter(self) -> Self::IntoIter {
        Box::new(self.iter())
    }
}

impl<K: Hash + Eq + Clone, V: Clone> IntoIterator for ShardedMap<K, V> {
    type Item = (K, V);
    type IntoIter = std::iter::Flatten<std::vec::IntoIter<FxHashMap<K, V>>>;
    // Cost: O(SHARDS), plus a copy of each shard another version shares.
    fn into_iter(self) -> Self::IntoIter {
        let shards: Vec<FxHashMap<K, V>> = Vec::from(self.shards as Box<[_]>)
            .into_iter()
            .map(Arc::unwrap_or_clone)
            .collect();
        shards.into_iter().flatten()
    }
}

/// An entry of a [`ShardedMap`]: the shard's own entry, plus the map's
/// length, which an insert through it counts.
pub(crate) struct ShardedEntry<'a, K, V> {
    entry: Entry<'a, K, V>,
    len: &'a mut usize,
    key_gen: &'a mut u64,
}

impl<'a, K, V> ShardedEntry<'a, K, V> {
    // Cost: O(1).
    pub(crate) fn or_insert_with(self, f: impl FnOnce() -> V) -> &'a mut V {
        if matches!(self.entry, Entry::Vacant(_)) {
            *self.len += 1;
            *self.key_gen = crate::runtime::registry_gen::next_generation();
        }
        self.entry.or_insert_with(f)
    }

    // Cost: O(1).
    pub(crate) fn or_insert(self, value: V) -> &'a mut V {
        self.or_insert_with(|| value)
    }

    // Cost: O(1).
    pub(crate) fn or_default(self) -> &'a mut V
    where
        V: Default,
    {
        self.or_insert_with(V::default)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_write_copies_only_the_shared_shard_it_touches() {
        let mut a: ShardedMap<String, u32> = ShardedMap::default();
        for i in 0..200 {
            a.insert(format!("k{i}"), i);
        }
        let b = a.clone();
        a.insert("new".to_string(), 1);
        *a.get_mut("k3").unwrap() = 99;
        assert_eq!(a.len(), 201);
        assert_eq!(b.len(), 200);
        assert_eq!(b.get("k3"), Some(&3));
        assert_eq!(a.get("k3"), Some(&99));
        assert!(!b.contains_key("new"));
        let shared = (0..SHARDS)
            .filter(|&i| Arc::ptr_eq(&a.shards[i], &b.shards[i]))
            .count();
        assert!(shared >= SHARDS - 2, "only the written shards were copied");
        *a.entry("k5".to_string()).or_insert(0) += 1;
        *a.entry("fresh".to_string()).or_insert(7) += 1;
        assert_eq!(a.get("k5"), Some(&6));
        assert_eq!(a.get("fresh"), Some(&8));
        assert_eq!(a.len(), 202);
        let keys = a.key_generation();
        *a.get_mut("k5").unwrap() = 1;
        *a.entry("k5".to_string()).or_insert(0) += 1;
        assert_eq!(a.key_generation(), keys, "a value write keeps the key set");
        a.insert("k5".to_string(), 3);
        assert_eq!(a.key_generation(), keys);
        a.insert("k-new".to_string(), 3);
        assert_ne!(a.key_generation(), keys, "a new key moves it");
        a.remove("k-new");
        a.retain(|k, _| k.starts_with("k1"));
        assert_eq!(a.len(), a.iter().count());
        assert_eq!(a.remove("k1"), Some(1));
        assert_eq!(a.len(), a.iter().count());
    }
}
