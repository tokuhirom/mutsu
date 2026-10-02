//! Ranking several patterns that are measured side by side with one NFA run
//! (ADR-0046, ADR-0125): a proto's candidates, or the branches of a compiled
//! `|`.
//!
//! A call to a proto picks the candidate whose declarative prefix reaches
//! furthest at the position, ties broken by the longest literal and then by
//! declaration order; a `|` ranks its branches the same way (ADR-0022).
//! Rakudo builds one NFA for the whole proto and runs it once; so does this:
//! [`NfaBuilder::build_roots`] compiles every candidate (or branch) into one
//! [`LtmNfa`] (the rules they call once, not once per candidate), and its run
//! reports what each root found on its own (`NfaRun::origins`). A root's
//! measurement is the one it would get from an NFA of its own, which is
//! `Interpreter::ltm_measure`'s.

use super::super::*;
use super::regex_ltm_nfa::{
    LtmMeasure, LtmNfa, LtmNfaSlots, cached_ltm_nfa, store_ltm_nfa, token_generation,
};
use super::regex_ltm_nfa_build::NfaBuilder;
use super::regex_token_candidates::TokenCandidates;
use std::cmp::Reverse;
use std::sync::Arc;

impl Interpreter {
    /// The indexes of a proto's candidates in the order the call tries them:
    /// the walk's rank-then-match dispatch (ADR-0046), written to `out`.
    /// Ranking measures each candidate's declarative prefix, so it runs nothing
    /// (ADR-0009); a candidate that cannot match here is left out, and ties
    /// keep declaration order. `keys` is scratch (the caller's, reused per
    /// call).
    // Cost: O(n * t + c log c) for one run of the proto's NFA (as
    // `ltm_measure`, over the paths of all the candidates), c = the
    // candidates; plus one build per (candidate list, generation).
    pub(super) fn ltm_rank_proto(
        &mut self,
        candidates: &TokenCandidates,
        chars: &[char],
        pos: usize,
        keys: &mut Vec<(usize, (usize, usize))>,
        out: &mut Vec<usize>,
    ) {
        keys.clear();
        out.clear();
        if candidates.is_empty() {
            return;
        }
        let nfa = self.ltm_proto_nfa_for(candidates);
        self.ltm_measure_roots(&nfa, chars, pos, |idx, measured| {
            // ADR-0022 §4.1's contract: `(None, false)` is a sound "this
            // candidate cannot match here" verdict and may filter; `(None,
            // true)` only means the measurement was cut short, so the
            // candidate is kept, ranked at 0.
            if measured.len.is_none() && !measured.stopped {
                return;
            }
            keys.push((idx, (measured.len.unwrap_or(0), measured.litlen)));
        });
        keys.sort_by_key(|(_, rank)| Reverse(*rank));
        out.extend(keys.iter().map(|(idx, _)| *idx));
    }

    /// Every branch of a compiled `|` with its rank key at `pos`, best first
    /// (`order`, the caller's scratch): `ltm_branch_rank_key` of each branch,
    /// from one run of the NFA of all of them, which `slots` (the `|`'s own)
    /// keeps. Ties keep declaration order, as `drive_alternation_candidates`
    /// sorts them; a branch that cannot match here is ranked, not left out.
    // Cost: O(n * t + b log b) for one run of the branches' NFA (as
    // `ltm_measure`, over the paths of all the branches), b = the branches;
    // plus one build per (package, generation).
    pub(super) fn ltm_rank_alternation(
        &mut self,
        slots: &LtmNfaSlots,
        branches: &[RegexPattern],
        chars: &[char],
        pos: usize,
        pkg: Symbol,
        order: &mut Vec<(usize, (usize, usize))>,
    ) {
        order.clear();
        let generation = token_generation();
        let nfa = match cached_ltm_nfa(slots, pkg, false, generation) {
            Some(nfa) => nfa,
            None => {
                let nfa = Arc::new(NfaBuilder::new(self, 0).build_alternation(branches, pkg));
                store_ltm_nfa(slots, pkg, false, generation, &nfa);
                nfa
            }
        };
        self.ltm_measure_roots(&nfa, chars, pos, |idx, measured| {
            order.push((idx, measured.branch_rank()));
        });
        order.sort_by_key(|(_, rank)| Reverse(*rank));
    }

    /// Run `nfa`, a [`NfaNode::AcceptAt`] NFA of several roots, once at `pos`
    /// and hand `each` the measurement of every root, in root order.
    // Cost: O(n * t + r) for the run (as `ltm_measure`, over the paths of all
    // the roots), r = the roots.
    fn ltm_measure_roots(
        &mut self,
        nfa: &LtmNfa,
        chars: &[char],
        pos: usize,
        mut each: impl FnMut(usize, LtmMeasure),
    ) {
        let mut run = nfa.run(self, chars, pos, &[]);
        // Each root's `_LL` literals, together.
        run.origin_ll.sort_unstable();
        let mut cursor = 0;
        for (idx, found) in run.origins.iter().enumerate() {
            let group = &run.origin_ll[cursor..];
            let own = group
                .iter()
                .take_while(|&&(origin, _)| origin as usize == idx)
                .count();
            cursor += own;
            each(
                idx,
                LtmMeasure::of(
                    pos,
                    found.furthest(),
                    found.stopped(),
                    group[..own].iter().map(|&(_, end)| end),
                ),
            );
        }
        run.recycle();
    }

    /// The NFA of all of `candidates`, built on first use under the current
    /// token generation and kept with the list.
    // Cost: O(1) for a cached hit; a miss costs one build.
    fn ltm_proto_nfa_for(&mut self, candidates: &TokenCandidates) -> Arc<LtmNfa> {
        let generation = token_generation();
        if let Some(nfa) = candidates.cached_proto_nfa(generation) {
            return nfa;
        }
        let nfa = Arc::new(NfaBuilder::new(self, 0).build_proto(candidates));
        candidates.store_proto_nfa(generation, &nfa);
        nfa
    }
}
