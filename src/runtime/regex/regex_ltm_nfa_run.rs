//! Simulating an [`LtmNfa`] over the subject (ADR-0125).
//!
//! Positions are processed in increasing order. Each position keeps the set
//! of nodes reached there; a node is expanded at most once per position, so a
//! run costs O(positions × nodes) plus the leaves' own matching. A leaf may
//! jump more than one character (a grapheme, a builtin `<ident>`), so the
//! positions still to visit are kept in an ordered map rather than a
//! lockstep frontier.
//!
//! The run happens under `LTM_DECLARATIVE_MODE`, inside a fate frame of its
//! own, and from an empty subrule stack, exactly as `ltm_prefix_len_at`
//! measures: a leaf that records a fate (a builtin that turns out to be a
//! method, say) records it into this run's frame.

use super::super::*;
use super::regex_helpers::{LTM_DECLARATIVE_MODE, LTM_PREFIX_TERMINATED, LTM_SEQALT_EPSILON};
use super::regex_ltm_fate::{ltm_fate_frame_close, ltm_fate_frame_open};
use super::regex_ltm_nfa::{LtmNfa, NfaNode};
use std::collections::BTreeMap;

/// The simulation handed the measurement back to the walker.
struct HandBack;

impl LtmNfa {
    /// The branch's prefix length at `start`: the furthest accept or fate,
    /// minus `start`. `None` inside the result means no path got anywhere;
    /// `None` outside it means the walker must measure instead (a live
    /// left-recursion activation, see [`NfaNode::Enter`]).
    // Cost: O(n * s) plus leaf matching, n = positions reached past `start`,
    // s = nodes.
    pub(super) fn run(
        &self,
        interp: &mut Interpreter,
        chars: &[char],
        start: usize,
    ) -> Option<Option<usize>> {
        let saved_mode = LTM_DECLARATIVE_MODE.with(|f| f.replace(true));
        let saved_terminated = LTM_PREFIX_TERMINATED.with(|f| f.replace(false));
        let saved_epsilon = LTM_SEQALT_EPSILON.with(|f| f.replace(false));
        let stack = super::regex_ltm_recursion::LtmMeasurementStack::open(false);
        let enclosing_fate = ltm_fate_frame_open();
        let walked = self.walk(interp, chars, start);
        let leaf_fate = ltm_fate_frame_close(enclosing_fate);
        drop(stack);
        LTM_DECLARATIVE_MODE.with(|f| f.set(saved_mode));
        LTM_PREFIX_TERMINATED.with(|f| f.set(saved_terminated));
        LTM_SEQALT_EPSILON.with(|f| f.set(saved_epsilon));
        let furthest = walked.ok()?;
        Some(furthest.max(leaf_fate).map(|end| end - start))
    }

    /// The furthest accept or NFA fate reached from `start`.
    fn walk(
        &self,
        interp: &mut Interpreter,
        chars: &[char],
        start: usize,
    ) -> Result<Option<usize>, HandBack> {
        let mut furthest: Option<usize> = None;
        // `seen[node]` is the last position the node was expanded at.
        let mut seen = vec![usize::MAX; self.nodes.len()];
        let mut pending: BTreeMap<usize, Vec<u32>> = BTreeMap::new();
        pending.insert(start, vec![self.start]);
        let mut work: Vec<u32> = Vec::new();
        while let Some((pos, nodes)) = pending.pop_first() {
            work.extend(nodes);
            while let Some(node) = work.pop() {
                let slot = &mut seen[node as usize];
                if *slot == pos {
                    continue;
                }
                *slot = pos;
                let mut reach = |end: usize, next: u32, work: &mut Vec<u32>| {
                    if end == pos {
                        work.push(next);
                    } else {
                        pending.entry(end).or_default().push(next);
                    }
                };
                match &self.nodes[node as usize] {
                    NfaNode::Split(targets) => work.extend(targets.iter().copied()),
                    NfaNode::Leaf {
                        atom,
                        pkg,
                        ic,
                        plural,
                        next,
                    } => {
                        if *plural {
                            for end in plural_ends(interp, atom, chars, pos, *pkg, *ic) {
                                reach(end, *next, &mut work);
                            }
                        } else if let Some(end) =
                            interp.regex_match_atom_in_pkg(atom, chars, pos, *pkg, *ic)
                        {
                            reach(end, *next, &mut work);
                        }
                    }
                    NfaNode::WsLead {
                        atom,
                        pkg,
                        ic,
                        next,
                    } => {
                        if pos != 0 {
                            furthest = furthest.max(Some(pos));
                        } else if matches!(**atom, RegexAtom::WsRule) {
                            if let Some(end) =
                                interp.regex_match_atom_in_pkg(atom, chars, pos, *pkg, *ic)
                            {
                                reach(end, *next, &mut work);
                            }
                        } else {
                            for end in plural_ends(interp, atom, chars, pos, *pkg, *ic) {
                                reach(end, *next, &mut work);
                            }
                        }
                    }
                    NfaNode::AtStart(next) => {
                        if pos == 0 {
                            work.push(*next);
                        }
                    }
                    NfaNode::AtEnd(next) => {
                        if pos == chars.len() {
                            work.push(*next);
                        }
                    }
                    NfaNode::Enter { name, next } => {
                        if super::regex_lr_state::lr_call_is_live(*name, chars.len() - pos) {
                            return Err(HandBack);
                        }
                        work.push(*next);
                    }
                    NfaNode::Fate | NfaNode::Accept => furthest = furthest.max(Some(pos)),
                }
            }
        }
        Ok(furthest)
    }
}

/// Every end of `atom` at `pos`, from the plural atom matcher.
fn plural_ends(
    interp: &mut Interpreter,
    atom: &RegexAtom,
    chars: &[char],
    pos: usize,
    pkg: Symbol,
    ic: bool,
) -> Vec<usize> {
    interp
        .regex_match_atom_all_with_capture_in_pkg(
            atom,
            chars,
            pos,
            &RegexCaptures::default(),
            pkg,
            ic,
        )
        .into_iter()
        .map(|(end, _)| end)
        .collect()
}
