//! Demand-driven atom candidate production (ADR-0073).
//!
//! The token walk (`regex_match_core.rs`) is already a continuation: it tries
//! one candidate, descends into the rest of the pattern, and only comes back
//! for the next candidate when the continuation rejected the current one. Its
//! *atoms* were not — an atom that is itself a sub-pattern (a group, an
//! alternation) was handed to `regex_match_atom_all_with_capture_in_pkg`, which
//! materialises **every** candidate before the walk descends into any of them.
//!
//! Computing a candidate runs the embedded `{ ... }` blocks inside it for real
//! (ADR-0009), so a block fired once per candidate *computed* rather than once
//! per candidate *entered*, and a `die` in a block on a candidate raku never
//! enters aborted a match that should have succeeded.
//!
//! `for_each_atom_candidate` closes that: for the atom kinds that recurse into
//! a sub-pattern it drives that sub-pattern's walk through a `MatchSink::Cont`,
//! so candidate *k+1* is computed only after candidate *k* has been rejected by
//! the real continuation. Every other atom kind falls back to the eager
//! producer, iterated highest-priority-first — byte-identical behaviour, which
//! is what confines the change to the atoms that can contain code.

use super::super::*;
use super::regex_helpers::{LTM_DECLARATIVE_MODE, alternation_capture_slots, merge_regex_captures};
use super::regex_match_core::MatchSink;
use super::regex_match_delta::{GroupShape, alternation_branch_delta};
use super::regex_trail::CapStore;
use std::cell::Cell;

/// What the walk does with one atom candidate: merge its delta, descend, and
/// report whether the whole walk should stop.
pub(in crate::runtime::regex) type AtomCandidateCont<'f> =
    dyn FnMut(&mut Interpreter, &mut CapStore, usize, RegexCaptures) -> bool + 'f;

impl Interpreter {
    /// Feed `on` this atom's candidates at `pos`, HIGHEST PRIORITY FIRST,
    /// producing each one only when the previous was rejected. Returns `true`
    /// when `on` asked the walk to stop.
    ///
    /// `store` is passed through rather than borrowed for a `current_caps`
    /// argument because the continuation runs *inside* this call and needs the
    /// store mutably; the enclosing level's lexicals/backrefs are published to
    /// the nested walks through the take-once seed instead, exactly as the
    /// eager producer does.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn for_each_atom_candidate(
        &mut self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
        store: &mut CapStore,
        pkg: &str,
        ignore_case: bool,
        ratchet: bool,
        on: &mut AtomCandidateCont<'_>,
    ) -> bool {
        // Measurement mode has no continuation to be driven by, and must stay
        // side-effect free (ADR-0009); leave it on the eager producer.
        if !LTM_DECLARATIVE_MODE.with(Cell::get) {
            match atom {
                RegexAtom::Group(pattern) => {
                    return self.drive_subpattern_candidates(
                        atom,
                        pattern,
                        chars,
                        pos,
                        store,
                        pkg,
                        ratchet,
                        on,
                        GroupShape::Merge,
                    );
                }
                RegexAtom::CaptureGroup(pattern) => {
                    return self.drive_subpattern_candidates(
                        atom,
                        pattern,
                        chars,
                        pos,
                        store,
                        pkg,
                        ratchet,
                        on,
                        GroupShape::Capture,
                    );
                }
                RegexAtom::CaptureIsolatedGroup(pattern) => {
                    return self.drive_subpattern_candidates(
                        atom,
                        pattern,
                        chars,
                        pos,
                        store,
                        pkg,
                        ratchet,
                        on,
                        GroupShape::Isolated,
                    );
                }
                RegexAtom::Conjunction(branches) => {
                    return self.drive_conjunction_candidates(
                        branches, chars, pos, store, pkg, ratchet, on,
                    );
                }
                RegexAtom::Alternation(alternatives) => {
                    return self.drive_alternation_candidates(
                        atom,
                        alternatives,
                        chars,
                        pos,
                        store,
                        pkg,
                        ratchet,
                        on,
                    );
                }
                RegexAtom::Named(name) => {
                    if let Some(stop) = self
                        .drive_named_subrule_candidates(name, chars, pos, store, pkg, ratchet, on)
                    {
                        return stop;
                    }
                }
                _ => {}
            }
        }
        let mut candidates = self.regex_match_atom_all_with_capture_opts(
            atom,
            chars,
            pos,
            store.caps(),
            pkg,
            ignore_case,
            // ADR-0073 Slice 2: under a ratcheted token the walk cannot come
            // back for a second candidate, so a `<subrule>` atom is asked for
            // its highest-priority end alone instead of its whole end set.
            // Measurement is exempt (ADR-0073 Decision 5): LTM ranking wants the
            // declarative prefix of the WHOLE atom, not of one chosen end.
            ratchet && !LTM_DECLARATIVE_MODE.with(Cell::get),
        );
        if ratchet && candidates.len() > 1 {
            // Ratchet (`:`) commits to the atom's highest-priority match and
            // forbids backtracking into it. Candidates come lowest-priority
            // first, so the preferred one is last.
            candidates.drain(..candidates.len() - 1);
        }
        for (next, delta) in candidates.into_iter().rev() {
            if on(self, store, next, delta) {
                return true;
            }
        }
        false
    }

    /// Drive one sub-pattern's walk, turning each of its ends into this atom's
    /// candidate delta as it is produced.
    #[allow(clippy::too_many_arguments)]
    fn drive_subpattern_candidates(
        &mut self,
        atom: &RegexAtom,
        pattern: &RegexPattern,
        chars: &[char],
        pos: usize,
        store: &mut CapStore,
        pkg: &str,
        ratchet: bool,
        on: &mut AtomCandidateCont<'_>,
        shape: GroupShape,
    ) -> bool {
        // `:m` remaps positions across the whole result set, so it cannot be
        // streamed; fall back to the eager producer for it.
        if pattern.ignore_mark {
            let mut candidates = self.regex_match_atom_all_with_capture_in_pkg(
                atom,
                chars,
                pos,
                store.caps(),
                pkg,
                pattern.ignore_case,
            );
            if ratchet && candidates.len() > 1 {
                candidates.drain(..candidates.len() - 1);
            }
            for (next, delta) in candidates.into_iter().rev() {
                if on(self, store, next, delta) {
                    return true;
                }
            }
            return false;
        }
        let _seed = Self::arm_inline_vars_seed(atom, store.caps());
        // A capturing group's candidates are deduplicated by end position: two
        // internal paths that stop at the same offset are one candidate for the
        // caller. Streaming keeps the FIRST (highest-priority) of them, which is
        // the one the walk would have tried first anyway.
        let mut seen: Vec<usize> = Vec::new();
        // The inner walk's stop signal means two different things and they must
        // not be conflated: `true` from `on` unwinds the WHOLE enclosing DFS,
        // while a ratchet only exhausts THIS atom's candidates. Both stop the
        // inner walk, so which one happened is recorded separately.
        let mut unwind = false;
        {
            let unwind = &mut unwind;
            let mut cont = |interp: &mut Interpreter, end: usize, inner: RegexCaptures| -> bool {
                if shape.dedups_ends() {
                    if seen.contains(&end) {
                        return false;
                    }
                    seen.push(end);
                }
                let delta = shape.delta(pos, end, inner);
                if on(interp, store, end, delta) {
                    *unwind = true;
                    return true;
                }
                // Ratchet (`:`) commits to the atom's highest-priority match and
                // forbids backtracking into it, so there is no second candidate.
                ratchet
            };
            self.regex_walk_ends_in_pkg(
                pattern,
                chars,
                pos,
                pkg,
                false,
                false,
                &mut MatchSink::Cont(&mut cont),
            );
        }
        unwind
    }

    /// Drive a `&`/`&&` conjunction: every branch has to match the SAME
    /// substring, so the atom's candidates are the FIRST branch's ends that
    /// every other branch also reaches. The eager producer collected the first
    /// branch's whole end set before probing any of them, which ran the code
    /// blocks inside it once per computed end (`( \w* {B} & \w* )` on `"aaa"`
    /// fired `B` four times against raku's one). Driving the first branch
    /// through a `MatchSink::Cont` instead means end *k+1* is computed only
    /// once end *k* has been rejected -- either by a sibling branch that cannot
    /// reach it, or by the real continuation.
    ///
    /// The other branches keep the eager probe: `regex_match_branch_ending_at`
    /// asks a yes/no question about ONE end, so there is no candidate set to
    /// stream, and raku evaluates them for the end under test too.
    #[allow(clippy::too_many_arguments)]
    fn drive_conjunction_candidates(
        &mut self,
        branches: &[RegexPattern],
        chars: &[char],
        pos: usize,
        store: &mut CapStore,
        pkg: &str,
        ratchet: bool,
        on: &mut AtomCandidateCont<'_>,
    ) -> bool {
        // An empty conjunction matches zero-width, as the eager producer does.
        let Some((first, rest)) = branches.split_first() else {
            return on(self, store, pos, RegexCaptures::default());
        };
        let mut unwind = false;
        {
            let unwind = &mut unwind;
            let mut cont =
                |interp: &mut Interpreter, end: usize, first_caps: RegexCaptures| -> bool {
                    // Raku keeps the captures from EVERY side of `&`, in written
                    // order -- same merge the eager arm performs.
                    let mut merged = merge_regex_captures(RegexCaptures::default(), first_caps);
                    for branch in rest {
                        match interp.regex_match_branch_ending_at(branch, chars, pos, end, pkg) {
                            Some(bcaps) => merged = merge_regex_captures(merged, bcaps),
                            // This end is not a candidate at all, so a ratchet has
                            // nothing to commit to yet: keep walking.
                            None => return false,
                        }
                    }
                    if on(interp, store, end, merged) {
                        *unwind = true;
                        return true;
                    }
                    // Ratchet (`:`) commits to the highest-priority candidate and
                    // forbids backtracking into the atom.
                    ratchet
                };
            self.regex_walk_ends_in_pkg(
                first,
                chars,
                pos,
                pkg,
                false,
                false,
                &mut MatchSink::Cont(&mut cont),
            );
        }
        unwind
    }

    /// Drive a `|` alternation: rank the branches by declarative prefix first
    /// (ADR-0022 — measurement, so it executes nothing), then walk them in rank
    /// order, entering branch *k+1* only once branch *k*'s candidates have all
    /// been rejected by the continuation. Mirrors `walk_seq_alternation`, which
    /// does the same for `||` in source order.
    #[allow(clippy::too_many_arguments)]
    fn drive_alternation_candidates(
        &mut self,
        atom: &RegexAtom,
        alternatives: &[RegexPattern],
        chars: &[char],
        pos: usize,
        store: &mut CapStore,
        pkg: &str,
        ratchet: bool,
        on: &mut AtomCandidateCont<'_>,
    ) -> bool {
        let capture_slots = alternation_capture_slots(alternatives);
        // Ranking is measurement, so it executes nothing (ADR-0009): the whole
        // point is to decide the branch ORDER without entering any of them.
        // A side-effect-only alternative (`| { die ... }`) matches zero-width,
        // so it can only win when nothing else matched; it is deferred to a
        // second pass rather than entered speculatively.
        let mut ranked: Vec<(usize, (usize, usize))> = Vec::new();
        for (i, alt) in alternatives.iter().enumerate() {
            if Self::is_pure_code_block_alt(alt) {
                continue;
            }
            let rank = self.ltm_branch_rank_key(alt, chars, pos, pkg);
            ranked.push((i, rank));
        }
        // Stable sort by rank descending: ties keep declaration order.
        ranked.sort_by_key(|(_, rank)| std::cmp::Reverse(*rank));
        let mut any = false;
        for (i, _) in ranked {
            let (matched, unwind) = self.drive_alternation_branch(
                atom,
                &alternatives[i],
                capture_slots,
                chars,
                pos,
                store,
                pkg,
                ratchet,
                on,
            );
            any |= matched;
            if unwind {
                return true;
            }
            if ratchet && matched {
                // `:ratchet` commits to the first branch that matched (and, in
                // `drive_alternation_branch`, to that branch's first candidate).
                return false;
            }
        }
        if any {
            return false;
        }
        for alt in alternatives.iter() {
            if !Self::is_pure_code_block_alt(alt) {
                continue;
            }
            let (_, unwind) = self.drive_alternation_branch(
                atom,
                alt,
                capture_slots,
                chars,
                pos,
                store,
                pkg,
                ratchet,
                on,
            );
            if unwind {
                return true;
            }
        }
        false
    }

    /// Walk one `|` branch, reporting `(the branch matched at all, the whole
    /// enclosing DFS should unwind)`. The two flags are distinct: a ratchet
    /// stops this branch's walk without unwinding anything.
    #[allow(clippy::too_many_arguments)]
    fn drive_alternation_branch(
        &mut self,
        atom: &RegexAtom,
        alt: &RegexPattern,
        capture_slots: usize,
        chars: &[char],
        pos: usize,
        store: &mut CapStore,
        pkg: &str,
        ratchet: bool,
        on: &mut AtomCandidateCont<'_>,
    ) -> (bool, bool) {
        let _seed = Self::arm_inline_vars_seed(atom, store.caps());
        let mut matched = false;
        let mut unwind = false;
        {
            let matched = &mut matched;
            let unwind = &mut unwind;
            let mut cont = |interp: &mut Interpreter, end: usize, inner: RegexCaptures| -> bool {
                *matched = true;
                let delta = alternation_branch_delta(capture_slots, inner);
                if on(interp, store, end, delta) {
                    *unwind = true;
                    return true;
                }
                ratchet
            };
            self.regex_walk_ends_in_pkg(
                alt,
                chars,
                pos,
                pkg,
                false,
                false,
                &mut MatchSink::Cont(&mut cont),
            );
        }
        (matched, unwind)
    }
}
