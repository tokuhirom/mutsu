//! The named-capture axis of a capture level (ADR-10488 D1).
//!
//! A level's named captures were a hash map from capture name to a vector of
//! capture nodes. Both halves allocate: the map's table on the first name a
//! level files, and each name's vector on its first node. A grammar files one
//! node per subrule call, so a parse paid two allocations per call for storage
//! that, for almost every level, held one to three names with one node each
//! (#10488: 44k of a 10 KB document's 165k parse allocations were these maps,
//! copied by the delta merges that moved nodes from level to level).
//!
//! [`NamedCaptureMap`] is a small map kept in filing order: one vector of
//! `(name, slot)` pairs, searched linearly. A level has a handful of names, so
//! a linear probe is cheaper than hashing, and the order a caller sees is the
//! order the captures were taken in, not a hash order. [`CapNodes`] holds one
//! node inline and spills to a vector only for a name captured twice or more.

use super::CapNode;
use crate::symbol::Symbol;
use std::sync::Arc;

/// One named capture's entries (ADR-0016 P4): span-bearing capture nodes and
/// the quantified flag in one axis.
#[derive(Clone, Default)]
pub(crate) struct NamedSlot {
    pub(crate) nodes: CapNodes,
    /// The name was captured under a quantifier (or `@<name>=` forced list):
    /// the Match presents it as an Array even for zero or one entries.
    pub(crate) quantified: bool,
}

impl NamedSlot {
    /// A slot holding one span-only leaf entry.
    pub(crate) fn leaf(from: usize, to: usize) -> Self {
        NamedSlot {
            nodes: CapNodes::one(Arc::new(CapNode {
                from,
                to,
                ..Default::default()
            })),
            quantified: false,
        }
    }

    /// An empty slot that renders as a list (a quantified name nothing bound).
    pub(crate) fn empty_list() -> Self {
        NamedSlot {
            nodes: CapNodes::default(),
            quantified: true,
        }
    }

    /// Fold another slot's entries into this one (capture-merge semantics:
    /// entries append, the quantified flag is sticky).
    // Cost: O(k) amortized, k = the entries appended.
    pub(crate) fn merge(&mut self, other: NamedSlot) {
        self.nodes.extend(other.nodes);
        self.quantified |= other.quantified;
    }
}

/// The capture nodes filed under one name, in filing order. One node is held
/// inline; a second spills them all into a vector.
#[derive(Clone, Default)]
pub(crate) struct CapNodes(Repr);

#[derive(Clone, Default)]
enum Repr {
    #[default]
    Empty,
    One(Arc<CapNode>),
    Many(Vec<Arc<CapNode>>),
}

impl CapNodes {
    pub(crate) fn one(node: Arc<CapNode>) -> Self {
        CapNodes(Repr::One(node))
    }

    // Cost: O(1) amortized.
    pub(crate) fn push(&mut self, node: Arc<CapNode>) {
        self.0 = match std::mem::take(&mut self.0) {
            Repr::Empty => Repr::One(node),
            Repr::One(first) => Repr::Many(vec![first, node]),
            Repr::Many(mut nodes) => {
                nodes.push(node);
                Repr::Many(nodes)
            }
        };
    }

    // Cost: O(1).
    pub(crate) fn pop(&mut self) -> Option<Arc<CapNode>> {
        match std::mem::take(&mut self.0) {
            Repr::Empty => None,
            Repr::One(node) => Some(node),
            Repr::Many(mut nodes) => {
                let last = nodes.pop();
                self.0 = Repr::Many(nodes);
                last
            }
        }
    }

    /// Keep the first `len` nodes.
    // Cost: O(d), d = the nodes dropped.
    pub(crate) fn truncate(&mut self, len: usize) {
        match &mut self.0 {
            Repr::Empty => {}
            Repr::One(_) => {
                if len == 0 {
                    self.0 = Repr::Empty;
                }
            }
            Repr::Many(nodes) => nodes.truncate(len),
        }
    }
}

impl std::ops::Deref for CapNodes {
    type Target = [Arc<CapNode>];

    fn deref(&self) -> &[Arc<CapNode>] {
        match &self.0 {
            Repr::Empty => &[],
            Repr::One(node) => std::slice::from_ref(node),
            Repr::Many(nodes) => nodes,
        }
    }
}

impl std::ops::DerefMut for CapNodes {
    fn deref_mut(&mut self) -> &mut [Arc<CapNode>] {
        match &mut self.0 {
            Repr::Empty => &mut [],
            Repr::One(node) => std::slice::from_mut(node),
            Repr::Many(nodes) => nodes,
        }
    }
}

impl Extend<Arc<CapNode>> for CapNodes {
    fn extend<I: IntoIterator<Item = Arc<CapNode>>>(&mut self, iter: I) {
        for node in iter {
            self.push(node);
        }
    }
}

impl FromIterator<Arc<CapNode>> for CapNodes {
    fn from_iter<I: IntoIterator<Item = Arc<CapNode>>>(iter: I) -> Self {
        let mut nodes = CapNodes::default();
        nodes.extend(iter);
        nodes
    }
}

impl IntoIterator for CapNodes {
    type Item = Arc<CapNode>;
    type IntoIter =
        std::iter::Chain<std::option::IntoIter<Arc<CapNode>>, std::vec::IntoIter<Arc<CapNode>>>;

    fn into_iter(self) -> Self::IntoIter {
        let (one, many) = match self.0 {
            Repr::Empty => (None, Vec::new()),
            Repr::One(node) => (Some(node), Vec::new()),
            Repr::Many(nodes) => (None, nodes),
        };
        one.into_iter().chain(many)
    }
}

impl<'a> IntoIterator for &'a CapNodes {
    type Item = &'a Arc<CapNode>;
    type IntoIter = std::slice::Iter<'a, Arc<CapNode>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut CapNodes {
    type Item = &'a mut Arc<CapNode>;
    type IntoIter = std::slice::IterMut<'a, Arc<CapNode>>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

/// A capture level's named captures: a small map in filing order. See the
/// module doc.
#[derive(Clone, Default)]
pub(crate) struct NamedCaptureMap {
    slots: Vec<(Symbol, NamedSlot)>,
}

impl NamedCaptureMap {
    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.slots.is_empty()
    }

    /// How many distinct names.
    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.slots.len()
    }

    // Cost: O(n), n = the distinct names.
    #[inline]
    fn index_of(&self, key: &Symbol) -> Option<usize> {
        self.slots.iter().position(|(k, _)| k == key)
    }

    // Cost: O(n), n = the distinct names.
    pub(crate) fn get(&self, key: &Symbol) -> Option<&NamedSlot> {
        self.index_of(key).map(|i| &self.slots[i].1)
    }

    // Cost: O(n), n = the distinct names.
    pub(crate) fn get_mut(&mut self, key: &Symbol) -> Option<&mut NamedSlot> {
        self.index_of(key).map(|i| &mut self.slots[i].1)
    }

    /// The slot for `key`, appended empty when the name is new.
    // Cost: O(n) amortized, n = the distinct names.
    pub(crate) fn slot_mut(&mut self, key: Symbol) -> &mut NamedSlot {
        let i = match self.index_of(&key) {
            Some(i) => i,
            None => {
                self.slots.push((key, NamedSlot::default()));
                self.slots.len() - 1
            }
        };
        &mut self.slots[i].1
    }

    /// [`Self::slot_mut`], also saying whether the name was already present.
    // Cost: O(n) amortized, n = the distinct names.
    pub(crate) fn slot_entry(&mut self, key: Symbol) -> (bool, &mut NamedSlot) {
        match self.index_of(&key) {
            Some(i) => (true, &mut self.slots[i].1),
            None => {
                self.slots.push((key, NamedSlot::default()));
                let last = self.slots.len() - 1;
                (false, &mut self.slots[last].1)
            }
        }
    }

    pub(crate) fn clear(&mut self) {
        self.slots.clear();
    }

    /// Replace (or append) `key`'s slot, handing back the one it replaced.
    // Cost: O(n) amortized, n = the distinct names.
    pub(crate) fn insert(&mut self, key: Symbol, slot: NamedSlot) -> Option<NamedSlot> {
        match self.index_of(&key) {
            Some(i) => Some(std::mem::replace(&mut self.slots[i].1, slot)),
            None => {
                self.slots.push((key, slot));
                None
            }
        }
    }

    /// Remove `key`'s slot, keeping the other names in filing order.
    // Cost: O(n), n = the distinct names.
    pub(crate) fn remove(&mut self, key: &Symbol) -> Option<NamedSlot> {
        self.index_of(key).map(|i| self.slots.remove(i).1)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = (&Symbol, &NamedSlot)> {
        self.slots.iter().map(|(k, v)| (k, v))
    }

    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = (&Symbol, &mut NamedSlot)> {
        self.slots.iter_mut().map(|(k, v)| (&*k, v))
    }

    pub(crate) fn keys(&self) -> impl Iterator<Item = &Symbol> {
        self.slots.iter().map(|(k, _)| k)
    }

    pub(crate) fn values(&self) -> impl Iterator<Item = &NamedSlot> {
        self.slots.iter().map(|(_, v)| v)
    }

    /// Move every slot out, in filing order.
    pub(crate) fn drain(&mut self) -> std::vec::Drain<'_, (Symbol, NamedSlot)> {
        self.slots.drain(..)
    }
}

impl<'a> IntoIterator for &'a NamedCaptureMap {
    type Item = (&'a Symbol, &'a NamedSlot);
    type IntoIter = std::iter::Map<
        std::slice::Iter<'a, (Symbol, NamedSlot)>,
        fn(&'a (Symbol, NamedSlot)) -> (&'a Symbol, &'a NamedSlot),
    >;

    fn into_iter(self) -> Self::IntoIter {
        self.slots.iter().map(|(k, v)| (k, v))
    }
}

impl std::ops::Index<&Symbol> for NamedCaptureMap {
    type Output = NamedSlot;

    fn index(&self, key: &Symbol) -> &NamedSlot {
        self.get(key).expect("no capture of that name")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn node(from: usize) -> Arc<CapNode> {
        Arc::new(CapNode {
            from,
            to: from + 1,
            ..Default::default()
        })
    }

    #[test]
    fn cap_nodes_spill_and_shrink() {
        let mut nodes = CapNodes::default();
        assert!(nodes.is_empty());
        nodes.push(node(0));
        assert_eq!(nodes.len(), 1);
        nodes.push(node(1));
        nodes.push(node(2));
        assert_eq!(nodes.iter().map(|n| n.from).collect::<Vec<_>>(), [0, 1, 2]);
        nodes.truncate(1);
        assert_eq!(nodes.last().map(|n| n.from), Some(0));
        assert_eq!(nodes.pop().map(|n| n.from), Some(0));
        assert!(nodes.is_empty());
        let mut one = CapNodes::one(node(5));
        one.truncate(0);
        assert!(one.is_empty());
    }

    #[test]
    fn map_keeps_filing_order() {
        let (a, b, c) = (
            Symbol::intern("a"),
            Symbol::intern("b"),
            Symbol::intern("c"),
        );
        let mut map = NamedCaptureMap::default();
        map.slot_mut(b).nodes.push(node(0));
        map.slot_mut(a).nodes.push(node(1));
        map.slot_mut(b).nodes.push(node(2));
        assert_eq!(map.keys().copied().collect::<Vec<_>>(), [b, a]);
        assert_eq!(map[&b].nodes.len(), 2);
        assert!(map.insert(c, NamedSlot::empty_list()).is_none());
        assert!(map.remove(&a).is_some());
        assert_eq!(map.keys().copied().collect::<Vec<_>>(), [b, c]);
        assert!(
            map.get(&c)
                .is_some_and(|s| s.quantified && s.nodes.is_empty())
        );
    }
}
