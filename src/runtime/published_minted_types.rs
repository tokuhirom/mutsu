//! Process-wide publication of types minted by `Metamodel::*HOW.new_type`
//! (#17697, ADR-17697).
//!
//! Every thread runs on its own interpreter, whose declaration registry is a
//! copy-on-write snapshot of its parent's, taken when the thread spawns. A
//! type minted after that on any thread lands in that thread's registry only,
//! yet the type object (a by-name package value) crosses threads freely: a
//! program that mints a type under a `Lock` and caches it in a shared object
//! hands it to siblings whose registries have never heard of it.
//!
//! So once `compose` finishes on a minted type, the minting thread copies the
//! type's registry entries (and those of the parents and roles it was built
//! from) into a bundle published here, and a thread that misses the name in
//! its own registry copies the bundle's entries in
//! (`Interpreter::adopt_published_minted_type`). Only complete, post-compose
//! snapshots are published, the first publication of a name wins, and
//! adoption copies data without running any user code. This is the sibling of
//! [`super::published_mixins`], which publishes mixin types as recipes.

use std::collections::HashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, RwLock};

use super::registry::Registry;

/// One published type: the registry entries of the type and of the names it
/// was built from, and the per-interpreter state kept outside the registry.
pub(crate) struct MintedTypeEntries {
    /// The names `registry` holds entries for: the type, its MRO and the roles
    /// composed into it. An adopting thread takes only the names it lacks.
    pub(crate) names: Vec<String>,
    /// A registry holding only those names' entries.
    pub(crate) registry: Registry,
    /// The names' `.^add_fallback` handlers (`TypeState::method_fallbacks`).
    pub(crate) method_fallbacks: Vec<(String, Vec<(crate::value::Value, crate::value::Value)>)>,
    /// The names' class- or role-body lexicals (`package_lexicals`).
    pub(crate) package_lexicals: Vec<(String, Vec<(String, crate::value::Value)>)>,
}

/// The published minted types of this process, keyed by type name. One
/// instance is shared (not snapshotted) by an interpreter and every thread
/// forked from it.
#[derive(Default)]
pub(crate) struct PublishedMintedTypes {
    /// Set when the first thread is forked. A type composed before that is in
    /// every fork's snapshot already, so nothing needs publishing.
    forked: AtomicBool,
    /// Set once the first type is published, so the miss-path probe of a
    /// program that never mints a type after a fork costs one relaxed load.
    any: AtomicBool,
    types: RwLock<HashMap<String, Arc<MintedTypeEntries>>>,
}

impl PublishedMintedTypes {
    /// Note that a thread was forked from an interpreter sharing this table.
    // Cost: O(1).
    pub(crate) fn note_fork(&self) {
        self.forked.store(true, Ordering::Relaxed);
    }

    /// Whether a type composed now has to be published: a thread exists that
    /// did not get it through its spawn snapshot.
    // Cost: O(1).
    #[inline]
    pub(crate) fn publishing(&self) -> bool {
        self.forked.load(Ordering::Relaxed)
    }

    /// Whether `name` is already published.
    // Cost: O(n) expected, n = name.len() (hash + read-locked probe).
    pub(crate) fn contains(&self, name: &str) -> bool {
        let types = self.types.read().unwrap_or_else(|e| e.into_inner());
        types.contains_key(name)
    }

    /// Record `name`'s entries. The first publication of a name wins.
    // Cost: O(n) expected, n = name.len() (hash + one write-locked insert).
    pub(crate) fn publish(&self, name: &str, entries: MintedTypeEntries) {
        let mut types = self.types.write().unwrap_or_else(|e| e.into_inner());
        types
            .entry(name.to_string())
            .or_insert_with(|| Arc::new(entries));
        self.any.store(true, Ordering::Relaxed);
    }

    /// Whether any type was ever published: the miss paths' O(1) negative
    /// answer.
    // Cost: O(1).
    #[inline]
    pub(crate) fn may_hold(&self) -> bool {
        self.any.load(Ordering::Relaxed)
    }

    /// The published entries of `name`.
    // Cost: O(1) when nothing was published; otherwise O(n) expected,
    // n = name.len() (hash + read-locked probe).
    pub(crate) fn lookup(&self, name: &str) -> Option<Arc<MintedTypeEntries>> {
        if !self.may_hold() {
            return None;
        }
        let types = self.types.read().unwrap_or_else(|e| e.into_inner());
        types.get(name).cloned()
    }
}
