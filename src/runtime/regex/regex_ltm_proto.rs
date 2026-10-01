//! Ranking a proto's candidates with one NFA run (ADR-0046, ADR-0125).
//!
//! A call to a proto picks the candidate whose declarative prefix reaches
//! furthest at the position, ties broken by the longest literal and then by
//! declaration order. Rakudo builds one NFA for the whole proto and runs it
//! once; so does this: [`NfaBuilder::build_proto`] compiles every candidate
//! into one [`LtmNfa`] (the rules they call once, not once per candidate), and
//! its run reports what each candidate found on its own
//! (`NfaRun::origins`). A candidate's measurement is the one it would get from
//! an NFA of its own, which is `Interpreter::ltm_measure`'s.

use super::super::*;
use super::regex_ltm_nfa::{LtmMeasure, LtmNfa};
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
            let measured = LtmMeasure::of(
                pos,
                found.furthest(),
                found.stopped(),
                group[..own].iter().map(|&(_, end)| end),
            );
            // ADR-0022 §4.1's contract: `(None, false)` is a sound "this
            // candidate cannot match here" verdict and may filter; `(None,
            // true)` only means the measurement was cut short, so the
            // candidate is kept, ranked at 0.
            if measured.len.is_none() && !measured.stopped {
                continue;
            }
            keys.push((idx, (measured.len.unwrap_or(0), measured.litlen)));
        }
        run.recycle();
        keys.sort_by_key(|(_, rank)| Reverse(*rank));
        out.extend(keys.iter().map(|(idx, _)| *idx));
    }

    /// The NFA of all of `candidates`, built on first use under the current
    /// token generation and kept with the list.
    // Cost: O(1) for a cached hit; a miss costs one build.
    fn ltm_proto_nfa_for(&mut self, candidates: &TokenCandidates) -> Arc<LtmNfa> {
        let generation =
            crate::runtime::regex_parse::TOKEN_DEFS_GEN.load(std::sync::atomic::Ordering::Relaxed);
        if let Some(nfa) = candidates.cached_proto_nfa(generation) {
            return nfa;
        }
        let nfa = Arc::new(NfaBuilder::new(self, 0).build_proto(candidates));
        candidates.store_proto_nfa(generation, &nfa);
        nfa
    }
}
