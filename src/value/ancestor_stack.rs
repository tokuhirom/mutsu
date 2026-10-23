//! The ancestors of the node a renderer is on (`.gist`, `.raku`, `.Str`):
//! an ordered stack with a membership test by node address.
//!
//! Each renderer keeps the nodes it is currently inside so that meeting one of
//! them again (a cycle) can be named instead of recursed into. The stack used
//! to be a `Vec` searched linearly on every node, which made rendering a
//! structure of depth `d` O(d^2): `.gist` of 200,000 nested arrays took 9 s,
//! `.raku` 32 s (#12714). A hash index of address -> position makes the test
//! O(1) however deep the walk is.
//!
//! A shallow stack is scanned instead of hashed: nearly every render is a few
//! levels deep, where a linear scan over a handful of addresses beats a hash
//! insert and remove per node. The index is built the first time the stack
//! passes [`INDEX_THRESHOLD`] and dropped when the stack empties.
//!
//! An address is on the stack at most once: a revisit is a cycle and is not
//! pushed. That is what makes "address -> position" a function.

use rustc_hash::FxHashMap;

/// Stack depth from which membership is answered by the hash index.
const INDEX_THRESHOLD: usize = 32;

/// Ancestors in render order, each with a payload `T` the renderer keeps per
/// node (the "was looped back to" flag, the node's back-reference name).
#[derive(Debug)]
pub(crate) struct AncestorStack<T> {
    stack: Vec<(usize, T)>,
    /// address -> position in `stack`, present once the stack is deep.
    index: Option<FxHashMap<usize, usize>>,
}

impl<T> Default for AncestorStack<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> AncestorStack<T> {
    // Cost: O(1).
    pub(crate) fn new() -> Self {
        Self {
            stack: Vec::new(),
            index: None,
        }
    }

    // Cost: O(1).
    pub(crate) fn is_empty(&self) -> bool {
        self.stack.is_empty()
    }

    // Cost: O(1) once the stack is deep; O(min(depth, INDEX_THRESHOLD)) before.
    fn position(&self, ptr: usize) -> Option<usize> {
        match &self.index {
            Some(index) => index.get(&ptr).copied(),
            None => self.stack.iter().rposition(|(p, _)| *p == ptr),
        }
    }

    /// The payload of `ptr` when it is an ancestor.
    // Cost: as `position`.
    pub(crate) fn get(&self, ptr: usize) -> Option<&T> {
        self.position(ptr).map(|pos| &self.stack[pos].1)
    }

    /// The payload of `ptr` when it is an ancestor, mutably.
    // Cost: as `position`.
    pub(crate) fn get_mut(&mut self, ptr: usize) -> Option<&mut T> {
        let pos = self.position(ptr)?;
        Some(&mut self.stack[pos].1)
    }

    /// Enter `ptr`, which must not be an ancestor already (test with
    /// [`Self::get`] first: a revisit is a cycle, not a node to push).
    // Cost: O(1) amortized; O(depth) once, when the stack first gets deep.
    pub(crate) fn push(&mut self, ptr: usize, payload: T) {
        debug_assert!(self.position(ptr).is_none(), "ancestor pushed twice");
        let pos = self.stack.len();
        self.stack.push((ptr, payload));
        match &mut self.index {
            Some(index) => {
                index.insert(ptr, pos);
            }
            None if self.stack.len() > INDEX_THRESHOLD => {
                self.index = Some(
                    self.stack
                        .iter()
                        .enumerate()
                        .map(|(pos, (p, _))| (*p, pos))
                        .collect(),
                );
            }
            None => {}
        }
    }

    /// Leave `ptr`, returning its payload. `ptr` is the innermost ancestor in
    /// every real walk (enter and leave are paired), which is the O(1) case; a
    /// node left out of order shifts the ones above it.
    // Cost: O(1) for the innermost node; O(depth above it) otherwise.
    pub(crate) fn pop(&mut self, ptr: usize) -> Option<T> {
        let pos = self.position(ptr)?;
        let (_, payload) = self.stack.remove(pos);
        if let Some(index) = &mut self.index {
            index.remove(&ptr);
            for (shifted, (p, _)) in self.stack.iter().enumerate().skip(pos) {
                index.insert(*p, shifted);
            }
        }
        if self.stack.is_empty() {
            self.index = None;
        }
        Some(payload)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn membership_before_and_after_the_index_is_built() {
        let mut s: AncestorStack<usize> = AncestorStack::new();
        for ptr in 1..=100 {
            assert!(s.get(ptr).is_none());
            s.push(ptr, ptr * 10);
        }
        for ptr in 1..=100 {
            assert_eq!(s.get(ptr), Some(&(ptr * 10)));
        }
        assert!(s.get(101).is_none());
        *s.get_mut(50).unwrap() = 7;
        assert_eq!(s.pop(100), Some(1000));
        assert!(s.get(100).is_none());
        assert_eq!(s.get(50), Some(&7));
        for ptr in (1..=99).rev() {
            assert!(s.pop(ptr).is_some());
        }
        assert!(s.is_empty());
        assert!(s.index.is_none());
    }

    #[test]
    fn leaving_a_node_out_of_order_keeps_the_rest_findable() {
        let mut s: AncestorStack<()> = AncestorStack::new();
        for ptr in 1..=40 {
            s.push(ptr, ());
        }
        assert!(s.pop(10).is_some());
        for ptr in (1..=40).filter(|p| *p != 10) {
            assert!(s.get(ptr).is_some(), "{ptr} lost");
        }
        assert!(s.get(10).is_none());
        assert!(s.pop(40).is_some());
        assert!(s.pop(40).is_none());
    }
}
