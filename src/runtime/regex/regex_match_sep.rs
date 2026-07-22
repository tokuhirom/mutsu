//! Separator quantifiers (`atom +% sep`, `atom **N..M %% sep`, ...): the atom
//! is matched repeatedly with `sep` interleaved between iterations, each
//! side's captures accumulated into its own folded group.
//!
//! Candidates are returned as *deltas* — `RegexCaptures` relative to an empty
//! baseline (ADR-0007); the engine merges the chosen candidate into its
//! capture store and rewinds on backtrack.

use super::super::*;
use super::regex_helpers::count_capture_groups;

impl Interpreter {
    /// Match a separator quantifier at `start`. Returns `(end, delta)` pairs in
    /// LOWEST-priority-first order (the engine iterates them in reverse):
    /// shortest match first, longest (greedy) last.
    pub(super) fn match_separated_quantifier(
        &mut self,
        token: &RegexToken,
        chars: &[char],
        start: usize,
        pkg: &str,
        pattern: &RegexPattern,
    ) -> Vec<(usize, RegexCaptures)> {
        if token.ratchet {
            return self.match_separated_quantifier_ratchet(token, chars, start, pkg, pattern);
        }
        let sep = token.separator.as_ref().expect("separator present");
        let (min, max) = match &token.quant {
            RegexQuant::OneOrMore => (1usize, None),
            RegexQuant::ZeroOrMore => (0usize, None),
            RegexQuant::Repeat(lo, hi) => (*lo, *hi),
            // `?` / exact-one don't form a separator list; treat as one.
            _ => (1usize, Some(1usize)),
        };
        let atom_stride = count_capture_groups(&token.atom);
        let sep_stride: usize = sep
            .pattern
            .tokens
            .iter()
            .map(|t| count_capture_groups(&t.atom))
            .sum();
        let names = Self::collect_quantified_names_for_token(token);

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
        let chains = self.enumerate_separated_chains(token, chars, start, pkg, pattern, max);

        // Turn each chain into result candidates (with optional trailing
        // separator for `%%`), highest-priority first, then reverse so the
        // engine (iterating in reverse) tries the highest-priority candidate
        // first.
        let mut out: Vec<(usize, RegexCaptures)> = Vec::new();
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
                let mut caps = RegexCaptures::default();
                caps.named_quantified.extend(names.iter().cloned());
                Self::append_separated_captures(
                    &mut caps,
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
        if min == 0 {
            out.push((start, RegexCaptures::default()));
        }
        out.reverse();
        out
    }

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
    fn match_separated_quantifier_ratchet(
        &mut self,
        token: &RegexToken,
        chars: &[char],
        start: usize,
        pkg: &str,
        pattern: &RegexPattern,
    ) -> Vec<(usize, RegexCaptures)> {
        let sep = token.separator.as_ref().expect("separator present");
        let (min, max) = match &token.quant {
            RegexQuant::OneOrMore => (1usize, None),
            RegexQuant::ZeroOrMore => (0usize, None),
            RegexQuant::Repeat(lo, hi) => (*lo, *hi),
            // `?` / exact-one don't form a separator list; treat as one.
            _ => (1usize, Some(1usize)),
        };
        // Frugal (`*? %`) under ratchet commits to the minimal count; greedy
        // extends to `max` (or as far as the input allows).
        let limit = if token.frugal { Some(min) } else { max };
        let can_extend = |count: usize| limit.is_none_or(|m| count < m);

        let empty = RegexCaptures::default();
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
        if can_extend(0)
            && let Some((end, caps)) = self
                .regex_match_atom_all_with_capture_in_pkg(
                    &token.atom,
                    chars,
                    start,
                    &empty,
                    pkg,
                    pattern.ignore_case,
                )
                .pop()
        {
            atom_caps.push(caps);
            cur = end;
            while can_extend(atom_caps.len()) {
                let Some((sep_end, scaps)) =
                    self.regex_match_end_from_caps_in_pkg(&sep.pattern, chars, cur, pkg)
                else {
                    break;
                };
                let Some((atom_end, acaps)) = self
                    .regex_match_atom_all_with_capture_in_pkg(
                        &token.atom,
                        chars,
                        sep_end,
                        &empty,
                        pkg,
                        pattern.ignore_case,
                    )
                    .pop()
                else {
                    break;
                };
                if atom_end <= cur {
                    break;
                }
                sep_caps.push(scaps);
                atom_caps.push(acaps);
                cur = atom_end;
            }
        }
        if atom_caps.len() < min {
            // Ratchet cannot backtrack to satisfy `min`: the quantifier fails.
            return Vec::new();
        }
        if atom_caps.is_empty() {
            return vec![(start, RegexCaptures::default())];
        }
        let atom_stride = count_capture_groups(&token.atom);
        let sep_stride: usize = sep
            .pattern
            .tokens
            .iter()
            .map(|t| count_capture_groups(&t.atom))
            .sum();
        let names = Self::collect_quantified_names_for_token(token);
        let mut caps = RegexCaptures::default();
        caps.named_quantified.extend(names.iter().cloned());
        // Trailing separator for `%%`: Rakudo consumes it greedily, and
        // ratchet commits to that single choice.
        let mut end = cur;
        let mut trailing: Option<RegexCaptures> = None;
        if sep.allow_trailing
            && let Some((ts_end, ts_caps)) =
                self.regex_match_end_from_caps_in_pkg(&sep.pattern, chars, cur, pkg)
            && ts_end >= cur
        {
            end = ts_end;
            trailing = Some(ts_caps);
        }
        Self::append_separated_captures(
            &mut caps,
            &atom_caps,
            &sep_caps,
            trailing.as_ref(),
            atom_stride,
            sep_stride,
        );
        vec![(end, caps)]
    }

    /// Enumerate every `atom (sep atom)*` chain rooted at `start`, backtracking
    /// the separator at each step. Each chain is `(atom_caps, sep_caps, end)`
    /// with `sep_caps.len() == atom_caps.len() - 1`. Chains are returned
    /// highest-priority first: for a greedy quantifier the longest chains come
    /// first, and within a step separator matches follow their own priority
    /// order. `max` bounds the atom count (atom count never exceeds `max`).
    fn enumerate_separated_chains(
        &mut self,
        token: &RegexToken,
        chars: &[char],
        start: usize,
        pkg: &str,
        pattern: &RegexPattern,
        max: Option<usize>,
    ) -> Vec<(Vec<RegexCaptures>, Vec<RegexCaptures>, usize)> {
        let mut chains: Vec<(Vec<RegexCaptures>, Vec<RegexCaptures>, usize)> = Vec::new();
        let empty = RegexCaptures::default();
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
            &empty,
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
            let mut atom_caps = vec![caps];
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
        pkg: &str,
        pattern: &RegexPattern,
        max: Option<usize>,
        atom_caps: &mut Vec<RegexCaptures>,
        sep_caps: &mut Vec<RegexCaptures>,
        out: &mut Vec<(Vec<RegexCaptures>, Vec<RegexCaptures>, usize)>,
    ) {
        // Bound the chain count to avoid catastrophic backtracking on a frugal
        // separator over a long string.
        if out.len() > 20_000 {
            return;
        }
        let empty = RegexCaptures::default();
        let can_extend = max.is_none_or(|m| atom_caps.len() < m);
        if can_extend {
            for (sep_end, scaps) in self.regex_match_ends_from_caps_in_pkg(
                &token.separator.as_ref().unwrap().pattern,
                chars,
                cur,
                pkg,
            ) {
                // Enumerate every atom-match length after this separator
                // (highest-priority first), mirroring the first-atom enumeration
                // so a frugal atom can expand to satisfy a following anchor.
                let atom_matches = self.regex_match_atom_all_with_capture_in_pkg(
                    &token.atom,
                    chars,
                    sep_end,
                    &empty,
                    pkg,
                    pattern.ignore_case,
                );
                for (atom_end, acaps) in atom_matches.into_iter().rev() {
                    if atom_end <= cur {
                        continue;
                    }
                    atom_caps.push(acaps);
                    sep_caps.push(scaps.clone());
                    self.extend_separated_chain(
                        token, chars, atom_end, pkg, pattern, max, atom_caps, sep_caps, out,
                    );
                    atom_caps.pop();
                    sep_caps.pop();
                }
            }
        }
        out.push((atom_caps.clone(), sep_caps.clone(), cur));
    }

    /// Append captures from a separated quantifier into `caps`, folding each
    /// side into its own positional/named group lists.
    fn append_separated_captures(
        caps: &mut RegexCaptures,
        atom_caps: &[RegexCaptures],
        sep_caps: &[RegexCaptures],
        trailing_sep: Option<&RegexCaptures>,
        atom_stride: usize,
        sep_stride: usize,
    ) {
        // Positional captures: atom groups occupy the first `atom_stride` slots,
        // separator groups the next `sep_stride`.
        for g in 0..atom_stride {
            let mut list: Vec<QuantifiedCaptureEntry> = Vec::new();
            let mut last_text = String::new();
            let mut last_sub: Option<std::sync::Arc<RegexCaptures>> = None;
            for ac in atom_caps {
                if let Some(text) = ac.positional.get(g) {
                    let (from, to) = ac.positional_offsets.get(g).copied().unwrap_or((0, 0));
                    let sub = ac.positional_subcaps.get(g).cloned().flatten();
                    list.push((text.clone(), from, to, sub.clone()));
                    last_text = text.clone();
                    last_sub = sub;
                }
            }
            caps.positional.push(last_text);
            caps.positional_subcaps.push(last_sub);
            caps.positional_quantified.push(Some(list));
            caps.positional_offsets.push((0, 0));
        }
        let mut all_sep: Vec<&RegexCaptures> = sep_caps.iter().collect();
        if let Some(ts) = trailing_sep {
            all_sep.push(ts);
        }
        for g in 0..sep_stride {
            let mut list: Vec<QuantifiedCaptureEntry> = Vec::new();
            let mut last_text = String::new();
            let mut last_sub: Option<std::sync::Arc<RegexCaptures>> = None;
            for sc in &all_sep {
                if let Some(text) = sc.positional.get(g) {
                    let (from, to) = sc.positional_offsets.get(g).copied().unwrap_or((0, 0));
                    let sub = sc.positional_subcaps.get(g).cloned().flatten();
                    list.push((text.clone(), from, to, sub.clone()));
                    last_text = text.clone();
                    last_sub = sub;
                }
            }
            caps.positional.push(last_text);
            caps.positional_subcaps.push(last_sub);
            caps.positional_quantified.push(Some(list));
            caps.positional_offsets.push((0, 0));
        }
        // Named captures: merge every iteration's named captures (as arrays).
        for src in atom_caps.iter().chain(all_sep.iter().copied()) {
            for (k, v) in &src.named {
                caps.named.entry(k.clone()).or_default().extend(v.clone());
                caps.named_quantified.insert(k.clone());
            }
            for (k, v) in &src.named_subcaps {
                caps.named_subcaps
                    .entry(k.clone())
                    .or_default()
                    .extend(v.clone());
            }
            for (k, v) in &src.hash_captures {
                caps.hash_captures
                    .entry(k.clone())
                    .or_default()
                    .extend(v.clone());
            }
        }
    }
}
