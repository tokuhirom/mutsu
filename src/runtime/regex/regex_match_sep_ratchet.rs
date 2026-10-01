//! The ratcheted separated quantifier (`token`/`rule` `atom +% sep`): a
//! possessive linear scan that yields at most one candidate.

use super::super::*;
use super::regex_helpers::count_capture_groups;
use super::regex_match_sep::{separated_capture_delta, separator_stride, with_iteration_capture};

impl Interpreter {
    /// Ratcheted (`token`/`rule`) separated quantifier: possessive linear scan.
    /// Ratchet forbids backtracking into the quantifier, so each step commits
    /// to the separator's and the atom's single highest-priority match and the
    /// whole quantifier yields at most one candidate. This matches Rakudo:
    /// `my token T { <[ab]>+ % ',' ',b' }` does NOT match "a,b" (the chain
    /// possessively consumes all of it) while the backtracking `regex` variant
    /// does. It is also what keeps grammar rules linear: the general DFS in
    /// `enumerate_separated_chains` goes exponential when sigspace turns the
    /// atom/separator into groups with several same-end candidates (a 6-pair
    /// JSON object under `rule pairlist { <pair> * % \, }` took ~8s to parse;
    /// this scan parses it in microseconds).
    pub(super) fn match_separated_quantifier_ratchet(
        &mut self,
        token: &RegexToken,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        pattern: &RegexPattern,
        current_caps: &RegexCaptures,
    ) -> Vec<(usize, RegexCaptures)> {
        let sep = token.separator.as_ref().expect("separator present");
        let Some((min, max)) = self.separated_quantifier_bounds(token, current_caps) else {
            return Vec::new();
        };
        // Frugal (`*? %`) under ratchet commits to the minimal count; greedy
        // extends to `max` (or as far as the input allows).
        let limit = if token.frugal { Some(min) } else { max };
        let can_extend = |count: usize| limit.is_none_or(|m| count < m);

        let mut atom_caps: Vec<RegexCaptures> = Vec::new();
        let mut sep_caps: Vec<RegexCaptures> = Vec::new();
        let mut cur = start;
        // Highest-priority atom match = the LAST candidate (the atom
        // enumeration returns lowest priority first), mirroring the
        // `RegexQuant::One` ratchet case. Deliberately the `_all_` enumeration
        // and NOT the singular `regex_match_atom_with_capture_in_pkg`: the
        // singular matcher's Named-atom path spawns a scratch sub-interpreter
        // (plus a tail-text copy) per candidate per call, which is ~300x
        // slower on nested grammar rules like `rule arraylist { <value> * %
        // [\,] }` over `[[1,2,3],[4,5,6],[7,8,9]]`.
        //
        // A zero-width FIRST atom is a genuine empty element (Rakudo:
        // `<-[;]>* % ';'` on ";b" is `("", "b")`, not zero iterations), so it
        // is accepted here. The infinite-loop risk lives only in the extension
        // loop below, which is bounded by its own `atom_end <= cur`
        // no-progress guard: after a zero-width atom, the separator must
        // advance `cur` or the loop breaks.
        let atom_stride = count_capture_groups(&token.atom);
        let sep_stride = separator_stride(&sep.pattern);
        let names = Self::collect_quantified_names_for_token(token);
        // What code in the next iteration sees of the chain so far
        // (`regex_match_sep_view`).
        let fold = |atoms: &[RegexCaptures], seps: &[RegexCaptures]| {
            separated_capture_delta(&names, atoms, seps, None, atom_stride, sep_stride)
        };
        let ic = pattern.ignore_case;
        if can_extend(0)
            && let Some((end, caps)) =
                self.sep_atom_first_seeing_chain(token, chars, start, pkg, ic, current_caps, || {
                    fold(&[], &[])
                })
        {
            atom_caps.push(with_iteration_capture(token, start, end, caps));
            super::regex_helpers::record_regex_farthest_position(end);
            cur = end;
            while can_extend(atom_caps.len()) {
                let Some((sep_end, scaps)) = self
                    .sep_ends_seeing_chain(
                        &sep.pattern,
                        chars,
                        cur,
                        pkg,
                        current_caps,
                        || fold(&atom_caps, &sep_caps),
                        atom_stride,
                        true,
                    )
                    .pop()
                else {
                    break;
                };
                super::regex_helpers::record_regex_farthest_position(sep_end);
                // The separator is folded before the atom after it matches,
                // so code in that atom sees it.
                sep_caps.push(scaps);
                let Some((atom_end, acaps)) = self.sep_atom_first_seeing_chain(
                    token,
                    chars,
                    sep_end,
                    pkg,
                    ic,
                    current_caps,
                    || fold(&atom_caps, &sep_caps),
                ) else {
                    sep_caps.pop();
                    break;
                };
                super::regex_helpers::record_regex_farthest_position(atom_end);
                if atom_end <= cur {
                    sep_caps.pop();
                    break;
                }
                atom_caps.push(with_iteration_capture(token, sep_end, atom_end, acaps));
                cur = atom_end;
            }
        }
        if atom_caps.len() < min {
            // Ratchet cannot backtrack to satisfy `min`: the quantifier fails.
            return Vec::new();
        }
        if atom_caps.is_empty() {
            // Zero iterations still marks the quantified names, so `$/<name>` is
            // an empty list rather than one empty Match (see the twin comment in
            // `match_separated_quantifier`).
            let mut caps = RegexCaptures::default();
            for n in Self::collect_quantified_names_for_token(token) {
                caps.named.slot_mut(Symbol::intern(&n)).quantified = true;
            }
            return vec![(start, caps)];
        }
        // Trailing separator for `%%`: Rakudo consumes it greedily, and
        // ratchet commits to that single choice.
        let mut end = cur;
        let mut trailing: Option<RegexCaptures> = None;
        if sep.allow_trailing
            && let Some((ts_end, ts_caps)) = self
                .sep_ends_seeing_chain(
                    &sep.pattern,
                    chars,
                    cur,
                    pkg,
                    current_caps,
                    || fold(&atom_caps, &sep_caps),
                    atom_stride,
                    true,
                )
                .pop()
            && ts_end >= cur
        {
            end = ts_end;
            trailing = Some(ts_caps);
        }
        let caps = separated_capture_delta(
            &names,
            &atom_caps,
            &sep_caps,
            trailing.as_ref(),
            atom_stride,
            sep_stride,
        );
        vec![(end, caps)]
    }
}
