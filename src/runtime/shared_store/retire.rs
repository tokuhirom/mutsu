//! Retiring a re-declared lane binding into a box its live children keep
//! (ADR-0129), and the child registry that makes it possible.

use super::{HashMap, SharedStore, atomic_lane_str_key, is_internal_key};
use std::sync::atomic::Ordering;
use std::sync::{Arc, Weak};

/// The spawn-ordered child list of one lineage plus the bookkeeping that keeps
/// both of its walks proportional to what changed since the last one.
#[derive(Debug, Default)]
pub(super) struct ChildList {
    next_seq: u64,
    live: Vec<(u64, Weak<SharedStore>)>,
    /// Length at which the next registration prunes dead entries.
    prune_at: usize,
    /// Per name, the spawn sequence number at the last retirement of that name:
    /// every child older than that was already given a redirect then.
    retired_upto: HashMap<String, u64>,
}

/// The lineage holding an entry: either a store in the chain itself or a
/// retired-binding box reached through a redirect.
pub(super) enum Holder<'a> {
    Chain(&'a SharedStore),
    Boxed(Arc<SharedStore>),
}

impl std::ops::Deref for Holder<'_> {
    type Target = SharedStore;
    fn deref(&self) -> &SharedStore {
        match self {
            Holder::Chain(s) => s,
            Holder::Boxed(b) => b,
        }
    }
}

impl SharedStore {
    /// A child lineage of `parent`. Spawned threads get one of these, so their
    /// own declarations stay private to them while the parent's entries stay
    /// visible and writable through the chain.
    ///
    /// The child is registered with `parent` so a later re-declaration there
    /// can hand it the binding it captured (see [`Self::retire_binding`]).
    /// Cost: O(1) amortized — dead registrations are pruned in bulk when the
    /// list doubles.
    pub(crate) fn child_of(parent: &Arc<Self>) -> Arc<Self> {
        let mut children = parent.children.lock().unwrap();
        let seq = children.next_seq + 1;
        children.next_seq = seq;
        let child = Arc::new(Self::detached(
            Some(Arc::clone(parent)),
            Some(parent.root_ref()),
        ));
        if children.live.len() >= children.prune_at {
            children.live.retain(|(_, w)| w.strong_count() > 0);
            children.prune_at = (children.live.len() * 2).max(64);
        }
        children.live.push((seq, Arc::downgrade(&child)));
        child
    }

    /// The retired-binding box this lineage redirects `key` to, if any.
    pub(super) fn redirect(&self, key: &str) -> Option<Arc<Self>> {
        if !self.has_redirects.load(Ordering::Acquire) {
            return None;
        }
        self.redirects.read().unwrap().get(key).cloned()
    }

    /// Retire this lineage's entry for the user lexical `key` before a
    /// re-declaration replaces or clears it (ADR-0129).
    ///
    /// The store is keyed by bare name, so it holds one binding per name per
    /// lineage — but a child spawned while the old binding was current captured
    /// *that* binding, and may still be running when the spawning routine is
    /// called again and re-declares the name (`sub f($n) { my @p = ^$n; start
    /// { @p } }; await f(1), f(2)`). Such a child must keep resolving the old
    /// binding. So the entry (and its `__mutsu_atomic_*` lane twin, which
    /// resolves wherever the base name lives) moves into a binding box, every
    /// live child spawned since the last retirement of `key` gets a redirect to
    /// it, and this lineage no longer holds the name. The children of one old
    /// binding all share the one box, so they keep seeing each other's writes.
    ///
    /// With no live child there is nobody to preserve the binding for, and the
    /// entry is left exactly as before (the caller overwrites or clears it).
    ///
    /// Cost: O(c), c = children spawned from this lineage since `key` was last
    /// retired here.
    pub(crate) fn retire_binding(&self, key: &str) {
        if is_internal_key(key) || !self.owns(key) {
            return;
        }
        let mut children = self.children.lock().unwrap();
        let since = children.retired_upto.get(key).copied().unwrap_or(0);
        let heirs: Vec<Arc<Self>> = children
            .live
            .iter()
            .rev()
            .take_while(|(seq, _)| *seq > since)
            .filter_map(|(_, w)| w.upgrade())
            .filter(|c| !c.owns(key) && c.redirect(key).is_none())
            .collect();
        let upto = children.next_seq;
        children.retired_upto.insert(key.to_string(), upto);
        drop(children);
        if heirs.is_empty() {
            return;
        }
        // A box is a leaf lineage of its own: it owns the name and its lane
        // twin, so every lookup routed to it resolves there.
        let bx = Arc::new(Self::detached(None, None));
        // `own` stays write-locked until every heir is redirected, so a heir's
        // concurrent write cannot land in this lineage after the move.
        let mut own = self.own.write().unwrap();
        {
            let mut box_own = bx.own.write().unwrap();
            if let Some(v) = own.remove(key) {
                box_own.insert(key.to_string(), v);
            }
            for hash_lane in [false, true] {
                let lane = atomic_lane_str_key(key, hash_lane);
                if let Some(v) = own.remove(lane) {
                    box_own.insert(lane.to_string(), v);
                }
            }
        }
        for heir in heirs {
            heir.redirects
                .write()
                .unwrap()
                .insert(key.to_string(), Arc::clone(&bx));
            heir.has_redirects.store(true, Ordering::Release);
        }
        drop(own);
    }

    /// The holder of user lexical `key` at THIS level of the chain: this
    /// lineage when it owns the name, else the retired-binding box it
    /// redirects the name to (ADR-0129).
    pub(super) fn holder_here(&self, key: &str) -> Option<Holder<'_>> {
        if self.owns(key) {
            return Some(Holder::Chain(self));
        }
        self.redirect(key).map(Holder::Boxed)
    }

    /// The retired-binding boxes this lineage redirects to (ADR-0129).
    pub(super) fn redirect_boxes(&self) -> Vec<Arc<Self>> {
        if !self.has_redirects.load(Ordering::Acquire) {
            return Vec::new();
        }
        self.redirects.read().unwrap().values().cloned().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn live_child_keeps_the_binding_a_redeclaration_replaces() {
        let parent = SharedStore::root();
        parent.declare("@p", Value::int(1));
        let first = SharedStore::child_of(&parent);
        parent.declare("@p", Value::int(2));
        let second = SharedStore::child_of(&parent);
        assert_eq!(first.get("@p"), Some(Value::int(1)));
        assert_eq!(second.get("@p"), Some(Value::int(2)));
        assert_eq!(parent.get("@p"), Some(Value::int(2)));
        // The first child's write lands in its own binding.
        first.set("@p", Value::int(10));
        assert_eq!(first.get("@p"), Some(Value::int(10)));
        assert_eq!(parent.get("@p"), Some(Value::int(2)));
    }

    #[test]
    fn siblings_of_one_binding_share_its_box() {
        let parent = SharedStore::root();
        parent.declare("@p", Value::int(1));
        let a = SharedStore::child_of(&parent);
        let b = SharedStore::child_of(&parent);
        parent.declare("@p", Value::int(2));
        a.set("@p", Value::int(7));
        assert_eq!(b.get("@p"), Some(Value::int(7)));
        // A grandchild resolves through its parent's redirect.
        let grandchild = SharedStore::child_of(&a);
        assert_eq!(grandchild.get("@p"), Some(Value::int(7)));
    }

    #[test]
    fn lane_twin_moves_with_the_binding() {
        let parent = SharedStore::root();
        parent.declare("@p", Value::int(1));
        let lane = atomic_lane_str_key("@p", false);
        parent.set(lane, Value::int(5));
        let child = SharedStore::child_of(&parent);
        parent.retire_binding("@p");
        assert_eq!(child.get(lane), Some(Value::int(5)));
        assert!(!Arc::ptr_eq(&child.atomic_lane_scope("@p"), &parent));
        assert_eq!(parent.get(lane), None);
    }

    #[test]
    fn no_live_child_leaves_the_entry_in_place() {
        let parent = SharedStore::root();
        parent.declare("@p", Value::int(1));
        drop(SharedStore::child_of(&parent));
        parent.retire_binding("@p");
        assert_eq!(parent.get("@p"), Some(Value::int(1)));
    }
}
