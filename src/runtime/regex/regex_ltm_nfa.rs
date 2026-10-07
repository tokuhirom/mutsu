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
        /// A literal that counts toward `litlen` (NQP's `_LL` edge; see
        /// `regex_ltm_litend`).
        ll: bool,
        next: u32,
    },
    /// `<.ws>`: a fate, except at the very start of the subject where a rule's
    /// leading whitespace is transparent (`ltm_leading_ws_is_transparent`).
    /// `lead` is false when a non-literal atom came first in the pattern
    /// (`\s* <.ws>`): that whitespace is then no longer leading, even though
    /// the atom before it matched nothing, so it is a fate at position 0 too.
    WsLead {
        lead: bool,
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
    /// The end of the `usize`th candidate of a proto's NFA ([`LtmNfa::roots`]).
    AcceptAt(u32),
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
    /// A proto's NFA has no single start: each candidate is entered through a
    /// root of its own and ends at its own [`NfaNode::AcceptAt`], so the one
    /// run measures every candidate at once and keeps their results apart
    /// (`NfaRun::origins`). Empty for any other NFA.
    pub(super) roots: Vec<NfaRoot>,
    /// Per node, what a thread arriving there at a character the guard rejects
    /// cannot go on from (`regex_ltm_nfa_guard`): the run drops it unasked.
    pub(super) guards: Vec<Option<super::regex_prefilter_firstset::FirstSet>>,
}

impl LtmNfa {
    /// An NFA of `nodes`, with the guards its nodes have.
    // Cost: O(s), s = nodes.
    pub(super) fn new(nodes: Vec<NfaNode>, start: u32, roots: Vec<NfaRoot>) -> Self {
        let guards = super::regex_ltm_nfa_guard::node_guards(&nodes);
        LtmNfa {
            nodes,
            start,
            roots,
            guards,
        }
    }
}

/// One candidate of a proto's NFA.
pub(super) struct NfaRoot {
    /// The candidate's body.
    pub(super) entry: u32,
    /// The candidate's [`NfaNode::AcceptAt`].
    pub(super) accept: u32,
}

/// The NFAs built for one pattern, or one compiled `|`'s branches: one entry
/// per (package, `:i`) they were measured from, stamped with the
/// `TOKEN_DEFS_GEN` they were built under (building one resolves rule names).
pub(crate) type LtmNfaSlots = std::sync::Mutex<Vec<LtmNfaSlot>>;

/// One entry of [`LtmNfaSlots`].
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
    /// The longest-literal tie-break (ADR-0022 §2): the length, from the
    /// start position, just past the furthest `_LL` literal any path crossed
    /// that is no further than `len` — MoarVM's `longlit` for the fate.
    pub(crate) litlen: usize,
}

impl LtmMeasure {
    /// The `(prefix_len, litlen)` a `|` branch ranks by (ADR-0022 §4.4). A
    /// nested sequential alternation can expose its epsilon bypass to the
    /// prefix measurement even when its first branch has already consumed a
    /// declarative literal. `litlen` still records that consumed literal, so
    /// keep the two measurements ordered consistently. Otherwise a later
    /// branch with a directly visible literal (for example `atom` after a
    /// nested `func` subrule) outranks the earlier branch despite both
    /// matching the same full text.
    // Cost: O(1).
    pub(crate) fn branch_rank(&self) -> (usize, usize) {
        (self.len.unwrap_or(0).max(self.litlen), self.litlen)
    }

    /// What a path set that got as far as `furthest` (an accept or a fate)
    /// from `pos` measures: `ll_ends` are the places the `_LL` literals it
    /// crossed ended. The one definition of a measurement, whether one NFA
    /// measures one pattern or a proto's NFA measures all its candidates.
    // Cost: O(l), l = `ll_ends`.
    pub(super) fn of(
        pos: usize,
        furthest: Option<usize>,
        stopped: bool,
        ll_ends: impl Iterator<Item = usize>,
    ) -> LtmMeasure {
        let litlen = furthest.map_or(0, |furthest| {
            ll_ends
                .filter(|&end| end <= furthest)
                .max()
                .map_or(0, |end| end - pos)
        });
        LtmMeasure {
            len: furthest.map(|end| end - pos),
            stopped,
            litlen,
        }
    }
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
        let measure = LtmMeasure::of(
            pos,
            furthest,
            run.seqalt || run.fate.is_some(),
            run.ll_ends.iter().copied(),
        );
        run.recycle();
        measure
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
        let generation = token_generation();
        if let Some(nfa) = cached_ltm_nfa(&pattern.derived.ltm_nfa, pkg, ignore_case, generation) {
            return nfa;
        }
        let nfa = Arc::new(super::regex_ltm_nfa_build::NfaBuilder::new(self, 0).build(
            pattern,
            pkg,
            ignore_case,
        ));
        store_ltm_nfa(&pattern.derived.ltm_nfa, pkg, ignore_case, generation, &nfa);
        nfa
    }
}

/// The token-definition generation an NFA is built under.
// Cost: O(1).
pub(super) fn token_generation() -> u64 {
    crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed)
}

/// The NFA in `slots` for (`pkg`, `ignore_case`) built under `generation`. A
/// slot of another generation is stale and is dropped.
// Cost: O(k), k = the slots.
pub(super) fn cached_ltm_nfa(
    slots: &LtmNfaSlots,
    pkg: Symbol,
    ignore_case: bool,
    generation: u64,
) -> Option<Arc<LtmNfa>> {
    let mut slots = slots.lock().ok()?;
    if slots.iter().any(|slot| slot.generation != generation) {
        slots.clear();
    }
    slots
        .iter()
        .find(|slot| slot.pkg == pkg && slot.ignore_case == ignore_case)
        .map(|slot| slot.nfa.clone())
}

/// Keep `nfa` in `slots` for the next measurement.
// Cost: O(1).
pub(super) fn store_ltm_nfa(
    slots: &LtmNfaSlots,
    pkg: Symbol,
    ignore_case: bool,
    generation: u64,
    nfa: &Arc<LtmNfa>,
) {
    if let Ok(mut slots) = slots.lock() {
        slots.push(LtmNfaSlot {
            pkg,
            ignore_case,
            generation,
            nfa: nfa.clone(),
        });
    }
}
