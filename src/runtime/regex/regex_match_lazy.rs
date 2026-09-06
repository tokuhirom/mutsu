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
use super::regex_helpers::{LTM_DECLARATIVE_MODE, alternation_capture_slots};
use super::regex_match_core::MatchSink;
use super::regex_trail::CapStore;
use std::cell::Cell;

/// What the walk does with one atom candidate: merge its delta, descend, and
/// report whether the whole walk should stop.
pub(super) type AtomCandidateCont<'f> =
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
                _ => {}
            }
        }
        let mut candidates = self.regex_match_atom_all_with_capture_in_pkg(
            atom,
            chars,
            pos,
            store.caps(),
            pkg,
            ignore_case,
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

/// How a group atom turns one inner match into this level's capture delta.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum GroupShape {
    /// `[ ... ]` — the inner captures join the caller's numbering.
    Merge,
    /// `( ... )` — the inner captures become this group's sub-Match.
    Capture,
    /// `<$rx>` and friends — the inner captures are discarded entirely.
    Isolated,
}

impl GroupShape {
    fn dedups_ends(self) -> bool {
        matches!(self, GroupShape::Capture)
    }

    fn delta(self, pos: usize, end: usize, inner: RegexCaptures) -> RegexCaptures {
        match self {
            GroupShape::Merge => group_merge_delta(inner),
            GroupShape::Capture => capture_group_delta(pos, end, inner),
            GroupShape::Isolated => RegexCaptures::default(),
        }
    }
}

/// `[ ... ]`: named captures merge into the caller's map, positionals append,
/// an inline `make` and any `:my`/`:let` write leave the group with it, and a
/// `<(` / `)>` marker inside sets the whole pattern's match boundaries.
pub(super) fn group_merge_delta(mut inner_caps: RegexCaptures) -> RegexCaptures {
    let mut new_caps = RegexCaptures::default();
    for (k, v) in inner_caps.named.drain() {
        new_caps.named.entry(k).or_default().merge(v);
    }
    new_caps.positional.append(&mut inner_caps.positional);
    super::regex_helpers::adopt_inline_ast(&mut new_caps, &mut inner_caps);
    new_caps
        .regex_vars
        .extend(std::mem::take(&mut inner_caps.regex_vars));
    if inner_caps.capture_start.is_some() {
        new_caps.capture_start = inner_caps.capture_start;
    }
    if inner_caps.capture_end.is_some() {
        new_caps.capture_end = inner_caps.capture_end;
    }
    new_caps
}

/// `( ... )`: the inner captures become this group's sub-Match (`$/[0]<name>`),
/// deliberately NOT merged into the parent's top-level named map.
pub(super) fn capture_group_delta(
    pos: usize,
    end: usize,
    inner_caps: RegexCaptures,
) -> RegexCaptures {
    let mut new_caps = RegexCaptures::default();
    let mut inner_caps = inner_caps;
    super::regex_helpers::adopt_inline_ast(&mut new_caps, &mut inner_caps);
    new_caps.regex_vars.extend(inner_caps.regex_vars.clone());
    let mut subcap = inner_caps;
    subcap.from = pos;
    subcap.to = end;
    new_caps.positional.push(PosSlot {
        from: pos,
        to: end,
        subcap: Some(std::sync::Arc::new(subcap.into_cap_node())),
        ..Default::default()
    });
    new_caps
}

/// One `|` / `||` branch's inner match, padded into the alternation's shared
/// positional slot space.
pub(super) fn alternation_branch_delta(
    capture_slots: usize,
    mut inner_caps: RegexCaptures,
) -> RegexCaptures {
    if !super::regex_helpers::IN_QUANTIFIED_ALTERNATION_MATCH.with(Cell::get) {
        inner_caps
            .positional
            .resize(capture_slots, PosSlot::alternation_padding());
    }
    let mut new_caps = RegexCaptures::default();
    for (k, v) in inner_caps.named.drain() {
        new_caps.named.entry(k).or_default().merge(v);
    }
    new_caps.positional.append(&mut inner_caps.positional);
    super::regex_helpers::adopt_inline_ast(&mut new_caps, &mut inner_caps);
    new_caps
        .regex_vars
        .extend(std::mem::take(&mut inner_caps.regex_vars));
    new_caps
}
