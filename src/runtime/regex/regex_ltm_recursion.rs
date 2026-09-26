//! A recursive subrule call ends an LTM declarative-prefix path
//! ([#9617](https://github.com/tokuhirom/mutsu/issues/9617)).
//!
//! Rakudo builds a rule's NFA by inlining the NFAs of the subrules it calls,
//! and it keeps the names already being inlined in a `seen` set: a call to a
//! rule that is already being inlined becomes a fate instead of a second
//! copy. So in `regex A { '{' [ <A> | . ]*? '}' }` the declarative prefix of
//! the branch `<A>` inlines `A` once, and the inner `<A>` ends its path where
//! it stands. Verified against `raku`:
//!
//! ```raku
//! grammar H { token A { 'a' <A>? 'b' | 'q' }; token T { [ <A> | 'aab' ] } }
//! H.subparse("aabb", :rule<T>)   # 「aab」: `<A>` measures 1 ('a', then a fate)
//! ```
//!
//! Following the recursion instead measured `<A>` as 4 and picked it. The
//! set starts empty at every measurement: a `|` inside `A` ranks its `<A>`
//! branch by inlining `A` once more (`token A { 'x' [ <A> | 'x' ] }` matches
//! all of "xxx"), and a proto's dispatch inlines each candidate's call to the
//! proto itself once.
//!
//! mutsu measures by walking the matcher under `LTM_DECLARATIVE_MODE`. The
//! three atom matchers every subrule call funnels through call
//! [`ltm_enter_subrule`], which keeps the names on a thread-local stack for as
//! long as the call's body is walked. Following the recursion made every
//! ranking walk the rule's whole nesting below it, which is what made
//! `[ <A> | . ]*?` cubic in the subject length.
//!
//! The stack is part of what a measurement depends on, so the measurement
//! memo (`regex_ltm_memo`) keys its entries by [`ltm_subrule_stack_id`]: an id
//! for the exact sequence of names on the stack, handed out by a trie so that
//! pushing and popping stay O(1).

use super::super::*;
use super::regex_helpers::LTM_DECLARATIVE_MODE;
use super::regex_ltm_fate::ltm_record_fate;
use rustc_hash::FxHashMap as HashMap;
use std::cell::{Cell, RefCell};

/// The id of the empty stack.
const EMPTY_STACK_ID: u32 = 0;

thread_local! {
    /// The subrules being walked by the measurement in progress, innermost
    /// last, each with the id of the stack that ends in it.
    static LTM_SUBRULE_STACK: RefCell<Vec<(Symbol, u32)>> = const { RefCell::new(Vec::new()) };

    /// `(parent stack id, pushed name) -> stack id`.
    static LTM_STACK_TRIE: RefCell<HashMap<(u32, Symbol), u32>> =
        RefCell::new(HashMap::default());

    /// The next id [`LTM_STACK_TRIE`] hands out.
    static LTM_STACK_NEXT_ID: Cell<u32> = const { Cell::new(EMPTY_STACK_ID + 1) };

    /// Stacks set aside by [`LtmMeasurementStack`] frames still open.
    static LTM_STACKS_SET_ASIDE: Cell<usize> = const { Cell::new(0) };
}

/// Opened by a measurement entry point. A measurement nested in another one's
/// walk (a `|` ranked while a subrule body is being walked) is part of the same
/// NFA and keeps the stack; one started from a real match starts from an empty
/// stack, whatever an enclosing measurement left on it.
pub(super) struct LtmMeasurementStack(Option<Vec<(Symbol, u32)>>);

impl LtmMeasurementStack {
    /// `already_measuring` is `LTM_DECLARATIVE_MODE` as the entry point found it.
    // Cost: O(1).
    pub(super) fn open(already_measuring: bool) -> Self {
        if already_measuring {
            return Self(None);
        }
        let outer = LTM_SUBRULE_STACK.with(|s| std::mem::take(&mut *s.borrow_mut()));
        if outer.is_empty() {
            return Self(None);
        }
        LTM_STACKS_SET_ASIDE.with(|n| n.set(n.get() + 1));
        Self(Some(outer))
    }
}

impl Drop for LtmMeasurementStack {
    // Cost: O(1).
    fn drop(&mut self) {
        if let Some(outer) = self.0.take() {
            LTM_SUBRULE_STACK.with(|s| *s.borrow_mut() = outer);
            LTM_STACKS_SET_ASIDE.with(|n| n.set(n.get() - 1));
        }
    }
}

/// The id of the current subrule stack; equal ids mean equal stacks.
// Cost: O(1).
pub(super) fn ltm_subrule_stack_id() -> u32 {
    LTM_SUBRULE_STACK.with(|s| s.borrow().last().map_or(EMPTY_STACK_ID, |&(_, id)| id))
}

/// Forget every stack id, when no measurement holds one: called as the
/// measurement memo — the only place ids are stored — is dropped.
// Cost: O(t), t = trie nodes created since the last reset.
pub(super) fn ltm_subrule_stack_reset_ids() {
    let idle = LTM_SUBRULE_STACK.with(|s| s.borrow().is_empty())
        && LTM_STACKS_SET_ASIDE.with(Cell::get) == 0;
    if idle {
        LTM_STACK_TRIE.with(|t| t.borrow_mut().clear());
        LTM_STACK_NEXT_ID.with(|n| n.set(EMPTY_STACK_ID + 1));
    }
}

/// Keeps one subrule on the stack while its body is walked.
pub(super) struct LtmSubruleFrame(());

impl Drop for LtmSubruleFrame {
    // Cost: O(1).
    fn drop(&mut self) {
        LTM_SUBRULE_STACK.with(|s| s.borrow_mut().pop());
    }
}

/// What a subrule call does under measurement.
pub(super) enum LtmSubruleEntry {
    /// Not a subrule call, or not measuring: match as usual.
    Plain,
    /// Measuring, and the rule is already being walked: a fate was recorded at
    /// the call's position, and the caller must fail this path.
    Recursive,
    /// Measuring: the rule stays on the stack until the frame drops.
    Entered(LtmSubruleFrame),
}

/// Classify a matcher's `atom` at `pos` (see the module docs).
// Cost: O(d) expected, d = subrule nesting depth of the measurement (one scan
// of the stack plus one trie probe).
pub(super) fn ltm_enter_subrule(atom: &RegexAtom, pos: usize) -> LtmSubruleEntry {
    let RegexAtom::Named(name) = atom else {
        return LtmSubruleEntry::Plain;
    };
    if !LTM_DECLARATIVE_MODE.with(Cell::get) {
        return LtmSubruleEntry::Plain;
    }
    let sym = name.spec().lookup_sym;
    let parent = LTM_SUBRULE_STACK.with(|s| {
        let s = s.borrow();
        if s.iter().any(|&(entered, _)| entered == sym) {
            None
        } else {
            Some(s.last().map_or(EMPTY_STACK_ID, |&(_, id)| id))
        }
    });
    let Some(parent) = parent else {
        ltm_record_fate(pos);
        return LtmSubruleEntry::Recursive;
    };
    let id = LTM_STACK_TRIE.with(|t| {
        *t.borrow_mut().entry((parent, sym)).or_insert_with(|| {
            LTM_STACK_NEXT_ID.with(|n| {
                let id = n.get();
                n.set(id + 1);
                id
            })
        })
    });
    LTM_SUBRULE_STACK.with(|s| s.borrow_mut().push((sym, id)));
    LtmSubruleEntry::Entered(LtmSubruleFrame(()))
}
