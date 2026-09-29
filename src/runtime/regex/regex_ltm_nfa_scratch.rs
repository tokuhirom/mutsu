//! The working storage of an [`LtmNfa`] run (`regex_ltm_nfa_run`): the
//! interned call stacks, the per-position seen set and the thread queues,
//! pooled per thread so a grammar parse's tens of thousands of measurement
//! runs do not each allocate them afresh (#10008).

use super::super::*;
#[cfg(doc)]
use super::regex_ltm_nfa::{LtmNfa, NfaNode};
use rustc_hash::{FxHashMap, FxHashSet};
use std::cell::RefCell;
use std::cmp::Reverse;
use std::collections::BinaryHeap;

/// Past this many distinct call stacks in one run, a further call is a fate
/// instead of a new stack. The recursion cut already bounds a stack's depth
/// by the number of rules; this bounds their number for a grammar whose
/// rules call each other along very many distinct paths.
const MAX_STACKS: usize = 1 << 16;

/// A run's working storage past this many stacks is dropped instead of going
/// back to [`SCRATCH`], so one pathological run does not pin its tables.
const POOLED_STACKS: usize = 1 << 12;

/// The call stacks of one run. Id 0 is the empty stack; every other id is a
/// `(parent id, return node, rule name)` entry, shared by every thread that
/// made the same calls.
pub(super) struct Stacks {
    pub(super) entries: Vec<(u32, u32, Symbol)>,
    index: FxHashMap<(u32, u32), u32>,
    /// Rule names already being inlined by the enclosing run, for a
    /// [`NfaNode::Sub`] or [`NfaNode::DynCall`] region's own run.
    outer: Vec<Symbol>,
}

impl Stacks {
    fn new() -> Self {
        Stacks {
            entries: vec![(0, 0, Symbol::intern(""))],
            index: FxHashMap::default(),
            outer: Vec::new(),
        }
    }

    /// Back to the empty stack alone, for a run under `outer`.
    // Cost: O(s + o), s = stacks the previous run made, o = `outer.len()`.
    fn reset(&mut self, outer: &[Symbol]) {
        self.entries.truncate(1);
        self.index.clear();
        self.outer.clear();
        self.outer.extend_from_slice(outer);
    }

    /// Is `name` being called on `stack`, or by an enclosing run?
    // Cost: O(d), d = the stack's depth (at most the number of rules).
    pub(super) fn calls(&self, mut stack: u32, name: Symbol) -> bool {
        while stack != 0 {
            let (parent, _, called) = self.entries[stack as usize];
            if called == name {
                return true;
            }
            stack = parent;
        }
        self.outer.contains(&name)
    }

    /// The names on `stack` and the enclosing runs', for a nested run.
    pub(super) fn names(&self, mut stack: u32) -> Vec<Symbol> {
        let mut names = self.outer.clone();
        while stack != 0 {
            let (parent, _, called) = self.entries[stack as usize];
            names.push(called);
            stack = parent;
        }
        names
    }

    pub(super) fn push(&mut self, stack: u32, ret: u32, name: Symbol) -> Option<u32> {
        if let Some(&id) = self.index.get(&(stack, ret)) {
            return Some(id);
        }
        if self.entries.len() >= MAX_STACKS {
            return None;
        }
        let id = self.entries.len() as u32;
        self.entries.push((stack, ret, name));
        self.index.insert((stack, ret), id);
        Some(id)
    }
}

/// The threads already expanded at the current position. Almost every node
/// is reached with one stack per position, so the first stack is kept in a
/// flat array and only the rest go to a hash set.
///
/// A slot belongs to the current position when its stamp is `now`. Stamps
/// only grow, so a `Seen` reused by a later run (of any NFA) needs no clearing:
/// every slot an earlier position or run wrote is stale by construction.
pub(super) struct Seen {
    stamp: Vec<u64>,
    stack: Vec<u32>,
    now: u64,
    more: FxHashSet<(u32, u32)>,
}

impl Seen {
    fn new() -> Self {
        Seen {
            stamp: Vec::new(),
            stack: Vec::new(),
            now: 0,
            more: FxHashSet::default(),
        }
    }

    /// Start a run over an NFA of `nodes` nodes.
    // Cost: O(1) amortized; O(nodes) only when this `Seen` grows.
    fn reset(&mut self, nodes: usize) {
        if self.stamp.len() < nodes {
            self.stamp.resize(nodes, 0);
            self.stack.resize(nodes, 0);
        }
        self.advance();
    }

    /// Record `(node, stack)` at the current position; `false` when it
    /// already was.
    pub(super) fn insert(&mut self, node: u32, stack: u32) -> bool {
        let n = node as usize;
        if self.stamp[n] != self.now {
            self.stamp[n] = self.now;
            self.stack[n] = stack;
            return true;
        }
        self.stack[n] != stack && self.more.insert((node, stack))
    }

    pub(super) fn advance(&mut self) {
        self.now += 1;
        if !self.more.is_empty() {
            self.more.clear();
        }
    }
}

pub(super) type Thread = (u32, u32);

/// The working storage of one run. Runs nest (a [`NfaNode::Sub`] or
/// [`NfaNode::DynCall`] region runs an NFA of its own), so each run takes one
/// from [`SCRATCH`] and puts it back when it ends; a grammar parse makes tens
/// of thousands of runs, and allocating these tables afresh for each was most
/// of the measurement's allocations (#10008).
pub(super) struct Scratch {
    pub(super) stacks: Stacks,
    pub(super) seen: Seen,
    /// Threads still to expand at the current position.
    pub(super) work: Vec<Thread>,
    /// Threads reached one position further (almost every leaf consumes one
    /// grapheme of one char).
    pub(super) step: Vec<Thread>,
    /// Threads reached further still.
    pub(super) far: BinaryHeap<Reverse<(usize, u32, u32)>>,
}

thread_local! {
    /// Idle [`Scratch`]es: as many as runs have ever been nested at once.
    static SCRATCH: RefCell<Vec<Scratch>> = const { RefCell::new(Vec::new()) };
}

impl Scratch {
    // Cost: O(s + o) amortized, as `Stacks::reset`.
    pub(super) fn take(nodes: usize, outer: &[Symbol]) -> Self {
        let mut scratch = SCRATCH
            .with(|pool| pool.borrow_mut().pop())
            .unwrap_or_else(|| Scratch {
                stacks: Stacks::new(),
                seen: Seen::new(),
                work: Vec::new(),
                step: Vec::new(),
                far: BinaryHeap::new(),
            });
        scratch.stacks.reset(outer);
        scratch.seen.reset(nodes);
        scratch
    }

    pub(super) fn give_back(mut self) {
        if self.stacks.entries.len() > POOLED_STACKS {
            return;
        }
        self.work.clear();
        self.step.clear();
        self.far.clear();
        SCRATCH.with(|pool| pool.borrow_mut().push(self));
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn reused_scratch_starts_clean() {
        let a = Symbol::intern("a");
        let b = Symbol::intern("b");
        let mut scratch = Scratch::take(8, &[a]);
        assert!(scratch.seen.insert(3, 0));
        assert!(!scratch.seen.insert(3, 0));
        assert!(scratch.seen.insert(3, 1));
        let called = scratch.stacks.push(0, 5, b).unwrap();
        assert!(scratch.stacks.calls(called, b));
        assert!(scratch.stacks.calls(0, a));
        scratch.work.push((1, called));
        scratch.give_back();

        // The next run, of a larger NFA with no enclosing names, reuses the
        // pooled storage and sees none of the previous run's state.
        let mut scratch = Scratch::take(16, &[]);
        assert!(scratch.work.is_empty());
        assert!(!scratch.stacks.calls(0, a));
        assert_eq!(scratch.stacks.entries.len(), 1);
        assert!(scratch.seen.insert(3, 0));
        assert!(scratch.seen.insert(3, 1));
        assert!(scratch.seen.insert(15, 0));
        scratch.seen.advance();
        assert!(scratch.seen.insert(3, 1));
        scratch.give_back();
    }

    #[test]
    fn nested_runs_take_distinct_scratch() {
        let outer = Scratch::take(4, &[]);
        let mut inner = Scratch::take(4, &[]);
        // The inner run's marks must not be visible to the outer one.
        assert!(inner.seen.insert(0, 0));
        let mut outer = outer;
        assert!(outer.seen.insert(0, 0));
        inner.give_back();
        outer.give_back();
    }
}
