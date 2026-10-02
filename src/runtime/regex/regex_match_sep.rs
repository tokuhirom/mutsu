//! Separator quantifiers (`atom +% sep`, `atom **N..M %% sep`, ...): the atom
//! is matched repeatedly with `sep` interleaved between iterations, each
//! side's captures accumulated into its own folded group.
//!
//! Candidates are returned as *deltas* — `RegexCaptures` relative to an empty
//! baseline (ADR-0007); the engine merges the chosen candidate into its
//! capture store and rewinds on backtrack.

use super::super::*;
use super::regex_helpers::{count_capture_groups, count_pattern_capture_groups};
use super::regex_trail::CapStore;
use std::collections::HashSet;

/// How many positional slots one match of a separator pattern takes.
// Cost: O(1) once the pattern's count is memoized.
pub(super) fn separator_stride(sep: &RegexPattern) -> usize {
    count_pattern_capture_groups(sep)
}

/// A separated quantifier's capture delta for one chain: every name under the
/// quantifier marked quantified, then the atoms' and the separators' captures
/// folded side by side (`append_separated_captures`). Every engine that
/// matches a separated quantifier builds its delta here.
// Cost: O(n + c), n = the names, c = the captures across the chain.
pub(super) fn separated_capture_delta(
    names: &HashSet<String>,
    atom_caps: &[RegexCaptures],
    sep_caps: &[RegexCaptures],
    trailing: Option<&RegexCaptures>,
    atom_stride: usize,
    sep_stride: usize,
) -> RegexCaptures {
    separated_capture_delta_syms(
        names.iter().map(|n| Symbol::intern(n)),
        atom_caps,
        sep_caps,
        trailing,
        atom_stride,
        sep_stride,
    )
}

/// [`separated_capture_delta`] for names already interned (the compiled
/// engine's, interned when the pattern compiled).
// Cost: O(n + c), n = the names, c = the captures across the chain.
pub(super) fn separated_capture_delta_syms(
    names: impl IntoIterator<Item = Symbol>,
    atom_caps: &[RegexCaptures],
    sep_caps: &[RegexCaptures],
    trailing: Option<&RegexCaptures>,
    atom_stride: usize,
    sep_stride: usize,
) -> RegexCaptures {
    let mut caps = RegexCaptures::default();
    for n in names {
        caps.named.slot_mut(n).quantified = true;
    }
    Interpreter::append_separated_captures(
        &mut caps,
        atom_caps,
        sep_caps,
        trailing,
        atom_stride,
        sep_stride,
    );
    caps
}

/// Apply a separated token's own capture name to ONE iteration's atom match
/// (`from..to`, captures `caps`). A capture name left on a separated token
/// (a builtin subrule `<digit>+ % ','`, an angle alias, an aliased capture
/// group or subrule call) names each item, so `$<digit>` is a List with one
/// Match per item, exactly like the unseparated `<digit>+`. A sigil alias of
/// the whole quantified span (`$<x>=\d+ % ','`) is wrapped in a group by the
/// parser and never reaches here.
// Cost: O(c), c = the iteration's captures.
pub(super) fn with_iteration_capture(
    token: &RegexToken,
    from: usize,
    to: usize,
    caps: RegexCaptures,
) -> RegexCaptures {
    if token.named_capture.is_none() {
        return caps;
    }
    let mut store = CapStore::new(caps);
    Interpreter::store_apply_named_capture(&mut store, token, from, to, 0);
    store.into_caps()
}

impl Interpreter {
    /// Resolve a separator quantifier's bounds once for the current match
    /// state. Block quantifiers use the same evaluator as their non-separated
    /// counterparts; treating `RepeatCode` as the fallback one-item case
    /// silently discarded a block's minimum and maximum.
    pub(super) fn separated_quantifier_bounds(
        &mut self,
        token: &RegexToken,
        current_caps: &RegexCaptures,
    ) -> Option<(usize, Option<usize>)> {
        match &token.quant {
            RegexQuant::OneOrMore => Some((1, None)),
            RegexQuant::ZeroOrMore => Some((0, None)),
            RegexQuant::Repeat(lo, hi) => Some((*lo, *hi)),
            RegexQuant::RepeatCode(code) => self.eval_regex_repeat_code(code, current_caps),
            // `?` / exact-one don't form a separator list; treat as one.
            _ => Some((1, Some(1))),
        }
    }

    /// Match a separator quantifier at `start`. Returns `(end, delta)` pairs in
    /// LOWEST-priority-first order (the engine iterates them in reverse):
    /// shortest match first, longest (greedy) last.
    pub(super) fn match_separated_quantifier(
        &mut self,
        token: &RegexToken,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        pattern: &RegexPattern,
        current_caps: &RegexCaptures,
    ) -> Vec<(usize, RegexCaptures)> {
        if token.ratchet {
            return self.match_separated_quantifier_ratchet(
                token,
                chars,
                start,
                pkg,
                pattern,
                current_caps,
            );
        }
        let sep = token.separator.as_ref().expect("separator present");
        let Some((min, max)) = self.separated_quantifier_bounds(token, current_caps) else {
            return Vec::new();
        };
        let atom_stride = count_capture_groups(token);
        let sep_stride = separator_stride(&sep.pattern);
        let names = Self::collect_quantified_names_for_token(token);

        // Zero iterations still reserve the atom's and the separator's
        // positional slots, as empty lists (rakudo: `"" ~~ / (\d)* % ',' /`
        // has `$/.list` `([],)`), like the unseparated quantifier does.
        let zero = (min == 0).then(|| {
            let caps = separated_capture_delta(&names, &[], &[], None, atom_stride, sep_stride);
            (start, caps)
        });

        // Enumerate every valid `atom (sep atom)*` chain via DFS, backtracking
        // the separator. A purely greedy linear scan (match the first atom, then
        // repeatedly take the separator's single highest-priority match followed
        // by an atom) cannot admit more atoms when a frugal separator's minimal
        // match blocks the next atom: e.g. `( 'a' || 'b' )* %% (.+?)` on "a x b"
        // would stop after "a" because the frugal `(.+?)` matched just " " and
        // "x b" is not an atom. Real backtracking expands the separator (" x ")
        // so the following atom ("b") can match. `enumerate_separated_chains`
        // performs that backtracking, returning chains highest-priority first
        // (most atoms first for a greedy quantifier; within a step, the
        // separator's own priority order from `regex_match_ends_from_caps_in_pkg`).
        let chains =
            self.enumerate_separated_chains(token, chars, start, pkg, pattern, max, current_caps);

        // Turn each chain into result candidates (with optional trailing
        // separator for `%%`), highest-priority first, then reverse so the
        // engine (iterating in reverse) tries the highest-priority candidate
        // first.
        let mut out: Vec<(usize, RegexCaptures)> = Vec::new();
        if token.frugal
            && let Some(candidate) = &zero
        {
            out.push(candidate.clone());
        }
        for (atom_caps, sep_caps, end) in &chains {
            let count = atom_caps.len();
            if count < min {
                continue;
            }
            if let Some(m) = max
                && count > m
            {
                continue;
            }
            let assemble = |trailing: Option<&RegexCaptures>, end: usize| {
                let caps = separated_capture_delta(
                    &names,
                    atom_caps,
                    sep_caps,
                    trailing,
                    atom_stride,
                    sep_stride,
                );
                (end, caps)
            };
            // Optional trailing separator for `%%`: prefer "with trailing" over
            // "without" (Rakudo greedily consumes a trailing separator). Each
            // trailing length is a separate candidate so an outer anchor can pick
            // the length that lets the whole pattern match (`... %% (.+?) $`).
            if sep.allow_trailing {
                for (ts_end, ts_caps) in
                    self.regex_match_ends_from_caps_in_pkg(&sep.pattern, chars, *end, pkg)
                {
                    if ts_end >= *end {
                        out.push(assemble(Some(&ts_caps), ts_end));
                    }
                }
            }
            out.push(assemble(None, *end));
        }
        // Zero iterations (when `min == 0`) is the lowest-priority outcome for a
        // greedy quantifier, so it goes last in highest-priority-first order.
        // The names still have to be marked quantified — `<pair>* %% ','` that
        // matched nothing captures an EMPTY list under `$/<pair>`, not a single
        // empty Match (`load-yaml("{}")` is an empty Hash, not `{"" => Any}`).
        if !token.frugal
            && let Some(candidate) = zero
        {
            out.push(candidate);
        }
        out.reverse();
        out
    }

    /// Enumerate every `atom (sep atom)*` chain rooted at `start`, backtracking
    /// the separator at each step. Each chain is `(atom_caps, sep_caps, end)`
    /// with `sep_caps.len() == atom_caps.len() - 1`. Chains are returned
    /// highest-priority first: for a greedy quantifier the longest chains come
    /// first, and within a step separator matches follow their own priority
    /// order. `max` bounds the atom count (atom count never exceeds `max`).
    #[allow(clippy::too_many_arguments)]
    fn enumerate_separated_chains(
        &mut self,
        token: &RegexToken,
        chars: &[char],
        start: usize,
        pkg: Symbol,
        pattern: &RegexPattern,
        max: Option<usize>,
        current_caps: &RegexCaptures,
    ) -> Vec<(Vec<RegexCaptures>, Vec<RegexCaptures>, usize)> {
        let mut chains: Vec<(Vec<RegexCaptures>, Vec<RegexCaptures>, usize)> = Vec::new();
        // First atom: enumerate EVERY match length (highest-priority first), not
        // just the single highest-priority one. A frugal atom (`[[.]+?]`) matches
        // as few chars as possible, but an outer anchor / goalpost following the
        // separated quantifier (e.g. `'<' ~ '>' [<( [[.]+?]* %% SEP )>]`) may
        // require the atom to expand. Taking only the shortest match would leave
        // no candidate for that anchor and the whole pattern would fail to match.
        let first_matches = self.regex_match_atom_all_with_capture_in_pkg(
            &token.atom,
            chars,
            start,
            current_caps,
            pkg,
            pattern.ignore_case,
        );
        // `regex_match_atom_all_with_capture_in_pkg` returns lowest-priority
        // first; iterate highest-priority first so the chains come out in
        // highest-priority order for the engine.
        //
        // A zero-width first atom (`end == start`) is a genuine empty leading
        // element (Rakudo: `<-[;]>* % ';'` on ";b" is `("", "b")`). It is the
        // LOWEST-priority first match, so `.rev()` places it last — the greedy
        // longest chain is still found first, and the empty-first chains are
        // only lower-priority backtracking options. The recursion is bounded by
        // `extend_separated_chain`'s `atom_end <= cur` no-progress guard (and
        // the 20_000 chain cap), so a zero-width atom cannot loop forever.
        for (end, caps) in first_matches.into_iter().rev() {
            super::regex_helpers::record_regex_farthest_position(end);
            let mut atom_caps = vec![with_iteration_capture(token, start, end, caps)];
            let mut sep_caps: Vec<RegexCaptures> = Vec::new();
            self.extend_separated_chain(
                token,
                chars,
                end,
                pkg,
                pattern,
                max,
                &mut atom_caps,
                &mut sep_caps,
                &mut chains,
                current_caps,
            );
        }
        chains
    }

    /// Recursive worker for `enumerate_separated_chains`. Extends the chain at
    /// `cur` by trying every separator match (in priority order) followed by an
    /// atom, recursing greedily first, then records the chain that stops at
    /// `cur`. Pushing the deeper (longer) chains before the shorter one yields a
    /// highest-priority-first ordering for a greedy quantifier.
    #[allow(clippy::too_many_arguments)]
    fn extend_separated_chain(
        &mut self,
        token: &RegexToken,
        chars: &[char],
        cur: usize,
        pkg: Symbol,
        pattern: &RegexPattern,
        max: Option<usize>,
        atom_caps: &mut Vec<RegexCaptures>,
        sep_caps: &mut Vec<RegexCaptures>,
        out: &mut Vec<(Vec<RegexCaptures>, Vec<RegexCaptures>, usize)>,
        current_caps: &RegexCaptures,
    ) {
        // Bound the chain count to avoid catastrophic backtracking on a frugal
        // separator over a long string.
        if out.len() > 20_000 {
            return;
        }
        if token.frugal {
            out.push((atom_caps.clone(), sep_caps.clone(), cur));
        }
        let can_extend = max.is_none_or(|m| atom_caps.len() < m);
        if can_extend {
            for (sep_end, scaps) in self.regex_match_ends_from_caps_in_pkg(
                &token.separator.as_ref().unwrap().pattern,
                chars,
                cur,
                pkg,
            ) {
                super::regex_helpers::record_regex_farthest_position(sep_end);
                // Enumerate every atom-match length after this separator
                // (highest-priority first), mirroring the first-atom enumeration
                // so a frugal atom can expand to satisfy a following anchor.
                let atom_matches = self.regex_match_atom_all_with_capture_in_pkg(
                    &token.atom,
                    chars,
                    sep_end,
                    current_caps,
                    pkg,
                    pattern.ignore_case,
                );
                for (atom_end, acaps) in atom_matches.into_iter().rev() {
                    super::regex_helpers::record_regex_farthest_position(atom_end);
                    if atom_end <= cur {
                        continue;
                    }
                    atom_caps.push(with_iteration_capture(token, sep_end, atom_end, acaps));
                    sep_caps.push(scaps.clone());
                    self.extend_separated_chain(
                        token,
                        chars,
                        atom_end,
                        pkg,
                        pattern,
                        max,
                        atom_caps,
                        sep_caps,
                        out,
                        current_caps,
                    );
                    atom_caps.pop();
                    sep_caps.pop();
                }
            }
        }
        if !token.frugal {
            out.push((atom_caps.clone(), sep_caps.clone(), cur));
        }
    }

    /// Append captures from a separated quantifier into `caps`, folding each
    /// side into its own positional/named group lists.
    pub(super) fn append_separated_captures(
        caps: &mut RegexCaptures,
        atom_caps: &[RegexCaptures],
        sep_caps: &[RegexCaptures],
        trailing_sep: Option<&RegexCaptures>,
        atom_stride: usize,
        sep_stride: usize,
    ) {
        // Positional captures: atom groups occupy the first `atom_stride` slots,
        // separator groups the next `sep_stride`. The folded slot keeps the
        // last iteration's span/subcap as its representative values.
        // An iteration's slot that an inner quantifier already folded
        // (`[ [ (\d) ] +% '.' ] +% ';'`) contributes all its entries: raku has
        // one flat list for a capture group under nested quantifiers.
        let fold_group = |sources: &[&RegexCaptures], g: usize| -> PosSlot {
            let mut list: Vec<QuantifiedCaptureEntry> = Vec::new();
            for src in sources {
                if let Some(slot) = src.positional.get(g) {
                    slot.push_entries_to(&mut list);
                }
            }
            PosSlot::folded(list)
        };
        let atom_refs: Vec<&RegexCaptures> = atom_caps.iter().collect();
        for g in 0..atom_stride {
            let slot = fold_group(&atom_refs, g);
            caps.positional.push(slot);
        }
        let mut all_sep: Vec<&RegexCaptures> = sep_caps.iter().collect();
        if let Some(ts) = trailing_sep {
            all_sep.push(ts);
        }
        for g in 0..sep_stride {
            let slot = fold_group(&all_sep, g);
            caps.positional.push(slot);
        }
        // Named captures: merge every iteration's named captures (as arrays)
        // in match order -- a0, s0, a1, s1, ..., then a trailing separator --
        // so a name both sides capture lists its entries as rakudo does
        // (#10574). Positional slots above stay side by side: they are
        // numbered by source position.
        let interleaved = (0..atom_caps.len().max(sep_caps.len()))
            .flat_map(|i| atom_caps.get(i).into_iter().chain(sep_caps.get(i)))
            .chain(trailing_sep);
        for src in interleaved {
            for (k, v) in &src.named {
                let slot = caps.named.slot_mut(*k);
                slot.merge(v.clone());
                slot.quantified = true;
            }
            for (k, v) in src.hash_captures() {
                caps.hash_captures_mut()
                    .entry(k.clone())
                    .or_default()
                    .extend(v.clone());
            }
        }
    }
}
