//! A declarative prefix, measured by a compiled NFA
//! ([ADR-0125](../../../docs/adr/0125-ltm-declarative-prefix-nfa.md)).
//!
//! Every LTM measurement in mutsu runs here: the rank of a `|` branch, the
//! ranking of a proto's candidates, and the `:rule<...>` / outermost proto
//! entry point. Rakudo compiles each rule's declarative prefix into an NFA
//! once and runs it; so does this module:
//!
//! - [`super::regex_ltm_nfa_build`] compiles a pattern into [`LtmNfa`]. A
//!   subrule call compiles to a [`NfaNode::Call`] into the callee's body,
//!   compiled once per NFA, so the NFA grows with the grammar and not with the
//!   number of paths through it;
//! - [`super::regex_ltm_nfa_run`] simulates it over the subject, keeping the
//!   call stack of each thread;
//! - the result is cached on the pattern per `(package, :i, TOKEN_DEFS_GEN)`.
//!
//! The backtracking matcher takes no part in a measurement beyond answering
//! single atoms (the NFA's leaves), which it does under
//! `LTM_DECLARATIVE_MODE` so that nothing it reaches runs user code
//! (ADR-0009).

use super::super::*;
use std::sync::Arc;

/// One node of the NFA. Edges point at node indices.
pub(super) enum NfaNode {
    /// ε-edges to every target. A placeholder (a procedure's entry, a loop
    /// head) is an empty split until the builder patches it.
    Split(Vec<u32>),
    /// The entry of an ordered alternation (`||`): ε-edges like a split, but
    /// reaching it makes a `None` measurement unsound to filter on (see
    /// [`LtmMeasure::stopped`]).
    SeqAlt(Vec<u32>),
    /// One atom, answered by the existing matcher (see [`LeafKind`]).
    Leaf {
        atom: Box<RegexAtom>,
        pkg: Symbol,
        ic: bool,
        kind: LeafKind,
        next: u32,
    },
    /// `<.ws>`: a fate, except at the very start of the subject where a rule's
    /// leading whitespace is transparent (`ltm_leading_ws_is_transparent`).
    WsLead {
        atom: Box<RegexAtom>,
        pkg: Symbol,
        ic: bool,
        next: u32,
    },
    /// A pattern's `^`: only at position 0.
    AtStart(u32),
    /// A pattern's trailing `$`: only at the end of the subject.
    AtEnd(u32),
    /// A `<name>` call: push `ret` and continue at `body`, the callee's
    /// procedure. A call to a rule already on the thread's stack is a fate
    /// (Rakudo's `%seen`, #9617); a call whose left-recursion activation is
    /// live reads that activation's seed instead of the body, as the matcher
    /// would.
    Call { name: Symbol, body: u32, ret: u32 },
    /// The end of a procedure: pop the stack and continue at its return node.
    Return,
    /// A `<name>` call whose callee can only be found at run time: a rule
    /// whose body's parse depends on runtime values. Each candidate found is
    /// measured by its own NFA.
    DynCall {
        atom: Box<RegexAtom>,
        pkg: Symbol,
        ic: bool,
        next: u32,
    },
    /// A region measured by an NFA of its own, over a different subject or
    /// in a different scope.
    Sub {
        nfa: Arc<LtmNfa>,
        kind: SubKind,
        next: u32,
    },
    /// A fate: the path ends here, and here counts toward the prefix.
    Fate,
    /// The end of the measured pattern.
    Accept,
}

/// Which existing matcher answers a leaf.
#[derive(Clone, Copy)]
pub(super) enum LeafKind {
    /// A one-grapheme atom (literal, `.`, class, property):
    /// `match_consuming_atom`, the prober's own tail.
    Consume,
    /// Any other atom with at most one end (a zero-width test):
    /// the single-end prober `regex_match_atom_in_pkg`.
    Probe,
    /// A builtin `<name>`, which may have several ends: the plural atom
    /// matcher.
    Plural,
}

/// Why a [`NfaNode::Sub`] region is measured on its own.
pub(super) enum SubKind {
    /// `:m` (ignoremark): the region runs over the subject with its combining
    /// marks stripped, and its positions are mapped back.
    StripMarks,
    /// An interpolated regex that closed over its defining scope (#8951):
    /// the scope is installed while the region runs.
    Scoped(Arc<crate::value::ValueMap>),
}

pub(crate) struct LtmNfa {
    pub(super) nodes: Vec<NfaNode>,
    pub(super) start: u32,
}

/// One entry of `PatternDerived::ltm_nfa`.
pub(crate) struct LtmNfaSlot {
    pkg: Symbol,
    ignore_case: bool,
    generation: u64,
    nfa: Arc<LtmNfa>,
}

/// What a measurement found.
pub(crate) struct LtmMeasure {
    /// The furthest place any path got, an accept or a fate, as a length
    /// from the start position; `None` when no path got anywhere.
    pub(crate) len: Option<usize>,
    /// `true` when the measurement was cut short: some path ended in a fate,
    /// or went through a `||`. A `None` length then proves nothing, because
    /// a `||`'s ε bypass continues at the group's start, so an atom after the
    /// group can fail where the real match (taking a later branch) would not
    /// (ADR-0022 §4.2; Cro::Uri's `IPv6address`, ADR-0046 Slice 4). A `None`
    /// with `false` is a sound "cannot match here" verdict.
    pub(crate) stopped: bool,
}

impl Interpreter {
    /// The declarative prefix of `pattern` at `pos`, walked in `pkg` from a
    /// real match (no inherited `:i`, nothing on the call stack).
    // Cost: O(n * s) for the simulation, n = characters the prefix can reach
    // past `pos`, s = NFA states live at a position (a node and a call stack);
    // plus one build per (pattern, package, generation). Rakudo: the same
    // order (an NFA run per ranking).
    pub(crate) fn ltm_measure(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
    ) -> LtmMeasure {
        let nfa = self.ltm_nfa_for(pattern, pkg, false);
        let run = nfa.run(self, chars, pos, &[]);
        let furthest = run.ends.iter().copied().max().max(run.fate);
        LtmMeasure {
            len: furthest.map(|end| end - pos),
            stopped: run.seqalt || run.fate.is_some(),
        }
    }

    /// The cached NFA of `pattern` in `pkg` (with an inherited `:i` when
    /// `ignore_case`), building it on first use.
    // Cost: O(k) for a cached hit, k = (package, :i) pairs the pattern was
    // measured under; a miss costs one build.
    pub(super) fn ltm_nfa_for(
        &mut self,
        pattern: &RegexPattern,
        pkg: Symbol,
        ignore_case: bool,
    ) -> Arc<LtmNfa> {
        let generation =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        if let Ok(mut slots) = pattern.derived.ltm_nfa.lock() {
            if slots.iter().any(|slot| slot.generation != generation) {
                slots.clear();
            }
            if let Some(slot) = slots
                .iter()
                .find(|slot| slot.pkg == pkg && slot.ignore_case == ignore_case)
            {
                return slot.nfa.clone();
            }
        }
        let nfa = Arc::new(super::regex_ltm_nfa_build::NfaBuilder::new(self, 0).build(
            pattern,
            pkg,
            ignore_case,
        ));
        if let Ok(mut slots) = pattern.derived.ltm_nfa.lock() {
            slots.push(LtmNfaSlot {
                pkg,
                ignore_case,
                generation,
                nfa: nfa.clone(),
            });
        }
        nfa
    }
}
