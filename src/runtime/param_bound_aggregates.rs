//! Every live `@`/`%` **parameter** binding, per name — see
//! [`Interpreter::param_bound_aggregates`](super::Interpreter).
//!
//! `clone_for_thread_for_block` keeps a parameter-bound aggregate off the
//! name-keyed `shared_vars` lane when the container the spawning frame holds
//! under that name is one a parameter binding of that name produced. A
//! `name -> Value` map could only answer that for the *latest* binding: two
//! closures made by one routine, each capturing its `%g` bound to a different
//! caller hash, had the older one fail the check, get seeded onto the lane
//! under `%g`, and from then on merge both bindings' stores into one copy
//! (#10076).
//!
//! So this remembers every binding, but only **weakly**: an entry is a
//! [`WeakGc`] to the bound container, keyed by its address. That keeps the
//! table from pinning every argument ever passed to a `@`/`%` parameter
//! alive, and it cannot give a false positive through address reuse — an
//! outstanding weak handle keeps the node's *allocation* reserved even after
//! its value is dropped, so no other container can sit at a recorded address
//! while the entry exists. Dead entries are swept on an amortized schedule.

use std::collections::HashMap;

use crate::gc::WeakGc;
use crate::value::{ArrayData, HashData, Value, ValueView};

/// A weak handle to one recorded `@`/`%` container.
#[derive(Clone)]
enum Bound {
    Array(WeakGc<ArrayData>),
    Hash(WeakGc<HashData>),
}

impl Bound {
    fn is_alive(&self) -> bool {
        match self {
            Bound::Array(w) => w.strong_count() > 0,
            Bound::Hash(w) => w.strong_count() > 0,
        }
    }
}

/// The identity of an `Array`/`Hash` container: its payload address, plus a
/// weak handle to it. `None` for every other value — only those two have the
/// container identity the spawn-time check compares
/// (`Interpreter::same_container_arc`).
fn identify(value: &Value) -> Option<(usize, Bound)> {
    let value = value.deref_container();
    match value.view() {
        ValueView::Array(items, _) => Some((
            crate::gc::Gc::as_ptr(&*items) as usize,
            Bound::Array(crate::gc::Gc::downgrade(&*items)),
        )),
        ValueView::Hash(map) => Some((
            crate::gc::Gc::as_ptr(&*map) as usize,
            Bound::Hash(crate::gc::Gc::downgrade(&*map)),
        )),
        _ => None,
    }
}

/// The live parameter-bound containers of one name.
#[derive(Clone, Default)]
struct NameBindings {
    by_addr: HashMap<usize, Bound>,
    /// Entry count at which dead entries are swept, doubled after each sweep
    /// so the amortized cost per recorded binding stays constant.
    sweep_at: usize,
}

const MIN_SWEEP_AT: usize = 16;

/// Name -> every live container a parameter of that name was bound to.
#[derive(Clone, Default)]
pub(crate) struct ParamBoundAggregates {
    names: HashMap<String, NameBindings>,
}

impl ParamBoundAggregates {
    /// Record that a parameter named `name` was bound to `value`. A value that
    /// is not an `Array`/`Hash` container is not recorded.
    // Cost: O(1) amortized — a hash probe plus a sweep of this name's entries
    // every time their count doubles.
    pub(crate) fn note(&mut self, name: &str, value: &Value) {
        let Some((addr, bound)) = identify(value) else {
            return;
        };
        let entry = match self.names.get_mut(name) {
            Some(entry) => entry,
            None => self.names.entry(name.to_string()).or_default(),
        };
        // A dead entry at this address cannot exist (its weak handle keeps the
        // allocation reserved), so an existing entry is this very container.
        entry.by_addr.entry(addr).or_insert(bound);
        if entry.by_addr.len() >= entry.sweep_at.max(MIN_SWEEP_AT) {
            entry.by_addr.retain(|_, b| b.is_alive());
            entry.sweep_at = (entry.by_addr.len() * 2).max(MIN_SWEEP_AT);
        }
    }

    /// Whether `value` (looked up under `name` in the spawning frame) is a
    /// container some live parameter binding of `name` produced.
    // Cost: O(1) — one hash probe per lookup.
    pub(crate) fn holds(&self, name: &str, value: &Value) -> bool {
        let Some(entry) = self.names.get(name) else {
            return false;
        };
        let Some((addr, _)) = identify(value) else {
            return false;
        };
        entry.by_addr.get(&addr).is_some_and(Bound::is_alive)
    }

    /// The names with at least one recorded binding.
    // Cost: O(n), n = distinct parameter names recorded.
    pub(crate) fn names(&self) -> impl Iterator<Item = &String> {
        self.names.keys()
    }
}

#[cfg(test)]
mod tests {
    use super::ParamBoundAggregates;
    use crate::value::Value;

    fn hash() -> Value {
        Value::hash(crate::value::HashData::default())
    }

    #[test]
    fn remembers_every_live_binding_of_a_name() {
        let (one, two) = (hash(), hash());
        let mut t = ParamBoundAggregates::default();
        t.note("%g", &one);
        t.note("%g", &two);
        assert!(t.holds("%g", &one), "the older binding is still recorded");
        assert!(t.holds("%g", &two));
        assert!(!t.holds("%h", &one), "bindings are per name");
        assert!(!t.holds("%g", &hash()), "an unrelated container is not");
    }

    #[test]
    fn does_not_keep_a_bound_container_alive() {
        let mut t = ParamBoundAggregates::default();
        for _ in 0..1000 {
            t.note("%g", &hash());
        }
        let entry = t.names.get("%g").unwrap();
        assert!(
            entry.by_addr.len() < 64,
            "dead bindings are swept, got {}",
            entry.by_addr.len()
        );
    }
}
