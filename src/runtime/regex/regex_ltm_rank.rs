//! ADR-0022 Slice 1: declarative-prefix LTM measurement infrastructure.
//!
//! This module adds the *measurement* primitive (`ltm_prefix_len_at`) and the
//! shared atom classifier (`ltm_atom_mode`) that the three atom matchers
//! consult under `LTM_DECLARATIVE_MODE`. It intentionally does NOT change how
//! `|` alternation ranks its branches — that is ADR-0022 Slice 3. Nothing in
//! this file is wired into the alternation-ranking consumer arms yet; the new
//! API is exercised only by this module's own unit tests, plus indirectly by
//! whatever already calls into `LTM_DECLARATIVE_MODE` (protoregex dispatch's
//! `declarative_prefix_match_len`, `regex_resolve.rs`), whose *measurements*
//! now see through more atom kinds than before (ADR-0009 previously handled
//! only code atoms).

use super::super::*;
use super::regex_helpers::{LTM_DECLARATIVE_MODE, LTM_PREFIX_TERMINATED, named_lookup_is_ws};
use std::cell::Cell;
use std::collections::HashSet;

/// Recursion cap for `ltm_litlen_at`'s subrule/group descent (ADR-0022 §4.3),
/// mirroring the ADR's suggested bound. Guards against pathological grammars
/// even though `seen` already cuts direct cycles.
const LTM_LITLEN_MAX_DEPTH: usize = 16;

/// How an atom participates in LTM declarative-prefix measurement
/// (ADR-0022 §4.2's prefix-construction table). `CodeAssertion` and
/// `SequentialAlternation` are deliberately NOT covered here: `CodeAssertion`
/// already has its own inline mode handling (ADR-0009), and
/// `SequentialAlternation` needs a full ε-bypass measurement (candidates =
/// ends(first branch) ∪ {pos}), not a single yes/no verdict — see
/// `Interpreter::ltm_seqalt_candidates` / `ltm_seqalt_best`.
pub(super) enum LtmAtomMode<'a> {
    /// Measure exactly as a real match would (consuming/transparent).
    Normal,
    /// Zero-width success; the caller must set `LTM_PREFIX_TERMINATED` so the
    /// walk unwinds and the length measured so far stands.
    Terminate,
    /// Measure the inner pattern's ends from the current position as if
    /// consuming (positive lookahead: `<?before X>` inlines `X` then stops),
    /// then terminate.
    TerminateAfter(&'a RegexPattern),
    /// Zero-width success that must NOT run and must NOT stop the walk: the
    /// atom consumes nothing and contributes nothing, and measurement keeps
    /// going past it at full strength. Distinct from `Terminate`, which also
    /// returns `pos` but declares everything after it non-declarative.
    SkipZeroWidth,
}

/// Classify `atom` for LTM declarative-prefix measurement. Only meaningful
/// while `LTM_DECLARATIVE_MODE` is set; callers must check that themselves
/// (this function does not consult the thread-local) so the check happens
/// once per atom match, not once per classification.
pub(super) fn ltm_atom_mode(atom: &RegexAtom) -> LtmAtomMode<'_> {
    match atom {
        // <.ws> / <ws> / implicit sigspace: Rakudo's NFA special-cases `ws` ->
        // fate (terminate), same as a subrule that names it in any lookup form.
        RegexAtom::WsRule => LtmAtomMode::Terminate,
        RegexAtom::Named(name) if named_lookup_is_ws(name) => LtmAtomMode::Terminate,
        // Backreferences depend on a capture made so far in THIS match, not on
        // the pattern's declarative structure — Rakudo's NFA has no method for
        // them, so they terminate.
        RegexAtom::Backref(_) | RegexAtom::NamedBackref(_) => LtmAtomMode::Terminate,
        // A bare `$var` interpolating an in-regex lexical is not a compile-time
        // literal; terminate (constants are inlined as literals before this
        // atom kind is ever produced, so they never reach here — see §2).
        RegexAtom::VarInterp(_) => LtmAtomMode::Terminate,
        // `:my $x = …;` / `:our` / `:constant` — a zero-width *declaration*.
        // Rakudo's NFA walks straight past it (validated: reordering two proto
        // candidates whose only difference is a leading `:my` flips the winner
        // purely by declaration order, i.e. their prefixes tie — ADR-0046
        // Slice 3), so it neither consumes nor terminates. It must be skipped
        // rather than measured normally, because the real `VarDecl` arm
        // *evaluates* the initializer, and measuring must never execute
        // (ADR-0009). This replaces the string-level declarator strip in
        // `regex_match_with_captures`, which only saw a declarator sitting at
        // the very start of a pattern's source text.
        RegexAtom::VarDecl { .. } => LtmAtomMode::SkipZeroWidth,
        // `<{ code }>` — the interpolated pattern is not known without running
        // code, so it cannot participate in a declarative prefix.
        RegexAtom::ClosureInterpolation { .. } => LtmAtomMode::Terminate,
        // `&` / `&&` conjunction: no NFA method in Rakudo -> fate (terminate).
        RegexAtom::Conjunction(_) => LtmAtomMode::Terminate,
        RegexAtom::Lookaround {
            pattern,
            negated,
            is_behind,
        } => {
            if *negated || *is_behind {
                // `<!before X>`, `<?after>`/`<!after>`, and any other negated
                // zero-width lookaround: terminate (matches Rakudo's own
                // `#?rakudo todo` on this quirk).
                LtmAtomMode::Terminate
            } else {
                // `<?before X>`: inline X's measurement, then terminate.
                LtmAtomMode::TerminateAfter(pattern)
            }
        }
        _ => LtmAtomMode::Normal,
    }
}

impl Interpreter {
    /// ADR-0022 §4.1: the longest declarative-prefix match of `pattern` at
    /// `pos`, plus whether the measurement was cut short by a
    /// non-declarative atom (`true` => the `None`/short length proves
    /// nothing about whether the real match would go further, so a caller
    /// may use the length to ORDER branches but must never use it to FILTER
    /// one out — same contract as `declarative_prefix_match_len`).
    ///
    /// Saves/restores `LTM_DECLARATIVE_MODE` / `LTM_PREFIX_TERMINATED`
    /// exactly like `declarative_prefix_match_len` (`regex_resolve.rs`) —
    /// they must nest, since a measurement can occur inside a real match
    /// inside another measurement (a subrule's own pattern may be measured
    /// while an outer measurement is still live). Never executes user code
    /// (ADR-0009 discipline): every code-bearing / non-declarative atom kind
    /// is neutralized by `ltm_atom_mode` or the `CodeAssertion` arm's
    /// existing mode check.
    ///
    pub(crate) fn ltm_prefix_len_at(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        pos: usize,
        pkg: &str,
    ) -> (Option<usize>, bool) {
        let saved_mode = LTM_DECLARATIVE_MODE.with(|f| f.replace(true));
        let saved_terminated = LTM_PREFIX_TERMINATED.with(|f| f.replace(false));
        let ends = self.regex_match_ends_from_caps_in_pkg(pattern, chars, pos, pkg);
        let stopped_at_non_declarative = LTM_PREFIX_TERMINATED.with(Cell::get);
        LTM_DECLARATIVE_MODE.with(|f| f.set(saved_mode));
        LTM_PREFIX_TERMINATED.with(|f| f.set(saved_terminated));
        let max_end = ends.into_iter().map(|(end, _)| end).max();
        (max_end.map(|end| end - pos), stopped_at_non_declarative)
    }

    /// ADR-0022 §4.2's `SequentialAlternation` special case for the PLURAL
    /// (all-candidates) atom matcher: in LTM mode, only the FIRST branch of
    /// `X || Y ...` participates in the declarative prefix, plus a
    /// zero-width epsilon bypass at `pos` (Rakudo `NFA.nqp::method altseq`
    /// builds child 0, then an epsilon edge straight from entry to exit).
    /// Deliberately does NOT set `LTM_PREFIX_TERMINATED` itself — the
    /// epsilon keeps the measurement alive past the group, so `X || Y` can
    /// never be the SOLE reason a fully-declarative measurement returns
    /// `None` (a caller filtering on `(None, false)` must not drop a branch
    /// just because its `||` group's first branch failed to match).
    ///
    /// Returned lowest-priority-first (the plural atom-matcher convention:
    /// the engine iterates the result in reverse): the epsilon first, then
    /// the first branch's own ends from lowest to highest.
    pub(super) fn ltm_seqalt_candidates(
        &mut self,
        alternatives: &[RegexPattern],
        chars: &[char],
        pos: usize,
        pkg: &str,
    ) -> Vec<(usize, RegexCaptures)> {
        let mut out = vec![(pos, RegexCaptures::default())];
        if let Some(first) = alternatives.first() {
            let mut ends = self.regex_match_ends_from_caps_in_pkg(first, chars, pos, pkg);
            ends.reverse(); // highest-first -> lowest-first
            for (end, mut inner_caps) in ends {
                let mut new_caps = RegexCaptures::default();
                for (k, v) in inner_caps.named.drain() {
                    new_caps.named.entry(k).or_default().merge(v);
                }
                new_caps.positional.append(&mut inner_caps.positional);
                new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                out.push((end, new_caps));
            }
        }
        out
    }

    /// ADR-0022 §4.3: length of the leading-literal region of `pattern` at
    /// `pos` — the longest run of leading declarative-literal content, used
    /// (only) to break `prefix_len` ties between `|` branches. NOT a matcher
    /// run: a direct char-comparison walk over `pattern`'s own token list, per
    /// the construction table in ADR-0022 §2 (concatenated literals extend it;
    /// capture groups end it even though their own content is literal;
    /// quantifiers end it; non-capturing groups and subrule calls descend and
    /// keep extending only if their own chain reaches their own end; nested
    /// alternation extends only when every branch is itself pure-literal).
    /// `seen` cycle-guards subrule recursion by lookup name; `depth` is capped
    /// by `LTM_LITLEN_MAX_DEPTH`. Never executes user code and never runs the
    /// real matcher (so it cannot itself set `LTM_PREFIX_TERMINATED`).
    pub(crate) fn ltm_litlen_at(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        pos: usize,
        pkg: &str,
        seen: &mut HashSet<String>,
        depth: usize,
    ) -> usize {
        self.ltm_litlen_walk(pattern, chars, pos, pkg, seen, depth)
            .0
    }

    /// Internal walk for [`Self::ltm_litlen_at`]: returns `(consumed_len,
    /// reached_end)`, where `reached_end` is true only when the walk consumed
    /// every token in `pattern` without hitting a region-ender. Callers that
    /// descend into a sub-pattern (`Group`, `Alternation` branch, subrule
    /// candidate) use `reached_end` to decide whether their OWN outer chain
    /// may keep extending past the sub-pattern, per ADR-0022 §4.3.
    fn ltm_litlen_walk(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        pos: usize,
        pkg: &str,
        seen: &mut HashSet<String>,
        depth: usize,
    ) -> (usize, bool) {
        if depth > LTM_LITLEN_MAX_DEPTH {
            return (0, false);
        }
        let mut acc = 0usize;
        for token in &pattern.tokens {
            // ADR-0022 Slice 5: a literal token born from a non-constant
            // runtime variable's interpolation contributes nothing to
            // litlen and ends the chain, same as any other non-literal
            // construct ("everything else ends litlen" below) — it is not
            // a compile-time-known character.
            if token.from_runtime_interpolation {
                return (acc, false);
            }
            // Quantifiers (and their separators) always end the litlen chain,
            // even around otherwise-literal content (ADR-0022 §2 table).
            if !matches!(token.quant, RegexQuant::One) || token.separator.is_some() {
                return (acc, false);
            }
            // A capture alias on this token — `(...)`'s own token-level
            // capture, `$<x>=...`, or `%<x>=...` — ends litlen unconditionally,
            // even for a token whose atom is otherwise pure-literal ("capture
            // kills litlen", validated by the `'a' \w\w | ('abc')` probe).
            if token.named_capture.is_some()
                || token.secondary_named_capture.is_some()
                || token.hash_capture.is_some()
            {
                return (acc, false);
            }
            match &token.atom {
                RegexAtom::Literal(ch) => {
                    let idx = pos + acc;
                    if idx >= chars.len() {
                        return (acc, false);
                    }
                    let hit = if pattern.ignore_case {
                        ch.to_lowercase().eq(chars[idx].to_lowercase())
                    } else {
                        *ch == chars[idx]
                    };
                    if !hit {
                        return (acc, false);
                    }
                    acc += 1;
                }
                RegexAtom::Group(inner) => {
                    let (len, full) =
                        self.ltm_litlen_walk(inner, chars, pos + acc, pkg, seen, depth + 1);
                    acc += len;
                    if !full {
                        return (acc, false);
                    }
                }
                // `( … )` — a capture group is transparent for prefix LENGTH
                // but always ends litlen, contributing nothing at all (not
                // even its own leading-literal content), matching Rakudo's
                // NFA (`subcapture` is not in the litlen-exempt set).
                RegexAtom::CaptureGroup(_) => {
                    return (acc, false);
                }
                RegexAtom::Alternation(alts) => {
                    let mut all_pure = true;
                    let mut best = 0usize;
                    for alt in alts {
                        let (len, full) =
                            self.ltm_litlen_walk(alt, chars, pos + acc, pkg, seen, depth + 1);
                        all_pure &= full;
                        best = best.max(len);
                    }
                    acc += best;
                    // Only continue the outer chain past the nested `|` when
                    // EVERY branch was itself pure-literal-to-its-end
                    // (mirrors NFA.nqp `method alt`'s "stop litlen at
                    // recombination unless all alts are pure literal").
                    if !all_pure {
                        return (acc, false);
                    }
                }
                RegexAtom::Named(name) => {
                    if depth >= LTM_LITLEN_MAX_DEPTH || seen.contains(name) {
                        return (acc, false);
                    }
                    let spec = Self::parse_named_regex_lookup_spec(name);
                    if !spec.arg_exprs.is_empty() {
                        return (acc, false);
                    }
                    let (candidates, raw_empty) = self.parsed_subrule_candidates(&spec, pkg, &[]);
                    if raw_empty {
                        return (acc, false);
                    }
                    seen.insert(name.clone());
                    let mut best = 0usize;
                    let mut all_full = true;
                    for (cand_pattern, cand_pkg, _sym) in candidates.iter() {
                        let (len, full) = self.ltm_litlen_walk(
                            cand_pattern,
                            chars,
                            pos + acc,
                            cand_pkg,
                            seen,
                            depth + 1,
                        );
                        best = best.max(len);
                        all_full &= full;
                    }
                    seen.remove(name);
                    acc += best;
                    if !all_full {
                        return (acc, false);
                    }
                }
                // Everything else (char classes, quantified-in-spirit atoms,
                // ws, code, backrefs, lookaround, anchors, …) ends litlen.
                _ => return (acc, false),
            }
        }
        (acc, true)
    }

    /// ADR-0022 §4.4: the `(prefix_len, litlen)` rank key for one `|` branch
    /// at `pos` — the two-part tie-break the three alternation-ranking
    /// consumer arms sort branches by (declaration order, the third and
    /// final tie-break, comes for free from a stable sort over the branches
    /// in their original written order — no index needs to travel with this
    /// key). Descending on both fields wins: `unwrap_or(0)` on a `None`
    /// prefix measurement is safe here because a `None` with `stopped ==
    /// false` (a sound "never matches" verdict) is filtered out by each
    /// caller before ranking ever sees it — see ADR-0022 §4.1's contract.
    pub(super) fn ltm_branch_rank_key(
        &mut self,
        alt: &RegexPattern,
        chars: &[char],
        pos: usize,
        pkg: &str,
    ) -> (usize, usize) {
        let (plen, _stopped) = self.ltm_prefix_len_at(alt, chars, pos, pkg);
        let mut seen = HashSet::new();
        let litlen = self.ltm_litlen_at(alt, chars, pos, pkg, &mut seen, 0);
        (plen.unwrap_or(0), litlen)
    }

    /// ADR-0046 Decision 1, mechanism 1: rank one proto-token candidate that is
    /// still in *pattern-source* form, as `eval_token_call_values_at` (the
    /// `:rule<...>` / outermost proto entry point) holds it.
    ///
    /// Parses the candidate once and delegates to the shared
    /// [`Self::ltm_branch_rank_key`] primitive, so this call site gets ADR-0022's
    /// `litlen` tie-break instead of ranking on `prefix_len` alone. Returns the
    /// same `(rank, stopped)` contract as [`Self::ltm_prefix_len_at`]: a `None`
    /// rank with `stopped == false` is a sound "cannot match here" verdict the
    /// caller may filter on, while `stopped == true` means the measurement was
    /// cut short and proves nothing.
    ///
    /// Falls back to the string-based `declarative_prefix_match_len` (with a
    /// zero litlen) when the source does not parse — that path also covers the
    /// `parse_anchored_single_subrule` shortcut, which only `regex_match_with_captures`
    /// implements.
    pub(in crate::runtime) fn ltm_rank_token_candidate_source(
        &mut self,
        pattern: &str,
        text: &str,
    ) -> (Option<(usize, usize)>, bool) {
        let Some(parsed) = self.parse_regex(pattern) else {
            let (plen, stopped) = self.declarative_prefix_match_len(pattern, text);
            return (plen.map(|p| (p, 0)), stopped);
        };
        let chars: Vec<char> = text.chars().collect();
        let pkg = self.current_package();
        let (plen, stopped) = self.ltm_prefix_len_at(&parsed, &chars, 0, &pkg);
        let Some(plen) = plen else {
            return (None, stopped);
        };
        let mut seen = HashSet::new();
        let litlen = self.ltm_litlen_at(&parsed, &chars, 0, &pkg, &mut seen, 0);
        (Some((plen, litlen)), stopped)
    }

    /// [`Self::ltm_seqalt_candidates`] collapsed to the single longest
    /// candidate, for atom matchers that return one candidate rather than a
    /// backtracking set (the singular capture-bearing matcher and the
    /// no-capture prober).
    pub(super) fn ltm_seqalt_best(
        &mut self,
        alternatives: &[RegexPattern],
        chars: &[char],
        pos: usize,
        pkg: &str,
    ) -> (usize, RegexCaptures) {
        let mut best: (usize, RegexCaptures) = (pos, RegexCaptures::default());
        if let Some(first) = alternatives.first() {
            for (end, mut inner_caps) in
                self.regex_match_ends_from_caps_in_pkg(first, chars, pos, pkg)
            {
                if end > best.0 {
                    let mut new_caps = RegexCaptures::default();
                    for (k, v) in inner_caps.named.drain() {
                        new_caps.named.entry(k).or_default().merge(v);
                    }
                    new_caps.positional.append(&mut inner_caps.positional);
                    new_caps.code_blocks.append(&mut inner_caps.code_blocks);
                    best = (end, new_caps);
                }
            }
        }
        best
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::runtime::regex_parse::RegexParseMode;

    /// Measure `pattern` (raw regex source, no delimiters) against `text` at
    /// position 0 in the empty package, returning `ltm_prefix_len_at`'s result.
    fn measure(pattern: &str, text: &str) -> (Option<usize>, bool) {
        let mut interp = Interpreter::new();
        let parsed = interp
            .parse_regex_with_mode(pattern, RegexParseMode::Match)
            .expect("pattern should parse");
        let chars: Vec<char> = text.chars().collect();
        interp.ltm_prefix_len_at(&parsed, &chars, 0, "")
    }

    #[test]
    fn plain_literal_measures_full_length_not_terminated() {
        let (len, stopped) = measure("abc", "abcabc");
        assert_eq!(len, Some(3));
        assert!(!stopped);
    }

    #[test]
    fn ws_rule_terminates_prefix() {
        // `\w+ '-'` with an implicit <.ws> injected by sigspace would need
        // `:s`/`rule`; here we spell the ws rule explicitly to keep the test
        // independent of sigspace wiring: prefix should stop AT the ws call,
        // not continue past it.
        let (len, stopped) = measure(r"a <.ws> b", "a   b");
        assert!(stopped, "ws should terminate the declarative prefix");
        // Terminated at the position right after 'a' (before <.ws> consumes
        // anything) — the ws call itself is zero-width in mode.
        assert_eq!(len, Some(1));
    }

    #[test]
    fn named_ws_lookup_terminates_like_wsrule() {
        let (len_dot, stopped_dot) = measure(r"a <.ws> b", "a   b");
        let (len_plain, stopped_plain) = measure(r"a <ws> b", "a   b");
        assert!(stopped_dot);
        assert!(stopped_plain);
        assert_eq!(len_dot, len_plain);
    }

    #[test]
    fn backref_terminates_prefix() {
        let (len, stopped) = measure(r"(a) $0", "aa");
        assert!(stopped, "a backreference should terminate the prefix");
        // Prefix includes the capture group's own consumption (1 char).
        assert_eq!(len, Some(1));
    }

    #[test]
    fn named_backref_terminates_prefix() {
        let (len, stopped) = measure(r"$<x>=(a) $<x>", "aa");
        assert!(stopped);
        assert_eq!(len, Some(1));
    }

    #[test]
    fn positive_lookahead_extends_prefix_then_terminates() {
        // `'ab' <?before c> \w\w` on "abcd": prefix is 'ab' (2) + the
        // lookahead's own inner match 'c' (1, measured as consuming) = 3,
        // then terminates (the trailing \w\w does not extend it further).
        let (len, stopped) = measure(r"ab <?before c> \w\w", "abcd");
        assert!(stopped);
        assert_eq!(len, Some(3));
    }

    #[test]
    fn negative_lookahead_terminates_without_extending() {
        let (len, stopped) = measure(r"ab <!before x> \w\w", "abcd");
        assert!(stopped);
        // Terminates right after 'ab' (2) — the negated lookahead does not
        // extend the prefix at all, unlike the positive case.
        assert_eq!(len, Some(2));
    }

    #[test]
    fn lookbehind_terminates_without_extending() {
        let (len, stopped) = measure(r"ab <?after ab> \w\w", "abcd");
        assert!(stopped);
        assert_eq!(len, Some(2));
    }

    #[test]
    fn conjunction_terminates_prefix() {
        // The conjunction group itself terminates as soon as it is reached
        // (zero-width, per §2's "no NFA method -> fate"), so the measured
        // prefix is exactly the declarative content BEFORE it — the leading
        // 'a' (1 char). Wrapped in a non-capturing group (`[...]`) rather
        // than `(...)` to keep the position slot bookkeeping out of the way.
        let (len, stopped) = measure(r"a [a & a] \w", "aab");
        assert!(stopped);
        assert_eq!(len, Some(1));
    }

    #[test]
    fn var_interp_terminates_prefix() {
        let (len, stopped) = measure(r":my $x = 'a'; a $x \w", "aab");
        assert!(stopped);
        // 'a' (1) then the VarDecl + VarInterp: the interpolation atom itself
        // terminates before consuming, so the prefix stops at 1.
        assert_eq!(len, Some(1));
    }

    #[test]
    fn closure_interpolation_terminates_prefix() {
        let (len, stopped) = measure(r"a <{ 'b' }> \w", "abc");
        assert!(stopped);
        assert_eq!(len, Some(1));
    }

    #[test]
    fn plain_code_block_still_terminates_per_adr_0009() {
        // Pre-existing ADR-0009 behavior, unchanged by this slice.
        let (len, stopped) = measure(r"a { ; } \w\w", "abcd");
        assert!(stopped);
        assert_eq!(len, Some(1));
    }

    #[test]
    fn code_assertion_true_stays_zero_width_and_transparent() {
        // Pre-existing ADR-0009 behavior, unchanged by this slice: <?{ ... }>
        // does NOT terminate, and keeps measuring past it.
        let (len, stopped) = measure(r"a <?{ 1 }> \w\w", "aaa");
        assert!(!stopped);
        assert_eq!(len, Some(3));
    }

    #[test]
    fn sequential_alternation_epsilon_bypass_when_first_branch_fails() {
        // `['doof' || 'food']` on "food": the first branch ('doof') does not
        // match at all, but the epsilon bypass means the group still measures
        // as zero-width instead of poisoning the whole prefix to None.
        let (len, stopped) = measure(r"['doof' || 'food']", "food");
        // Not stopped: SequentialAlternation's ε-bypass does not set
        // TERMINATED itself, and nothing else in this pattern does either.
        assert!(!stopped);
        assert_eq!(len, Some(0));
    }

    #[test]
    fn sequential_alternation_measures_first_branch_when_it_matches() {
        let (len, stopped) = measure(r"['food' || 'doof']", "food");
        assert!(!stopped);
        assert_eq!(len, Some(4));
    }

    #[test]
    fn sequential_alternation_never_the_sole_reason_for_none() {
        // A pattern that is ENTIRELY `X || Y` must still measure as Some(0)
        // (the epsilon), never None, even when no branch matches at all.
        let (len, stopped) = measure(r"['zzz' || 'yyy']", "food");
        assert!(!stopped);
        assert_eq!(len, Some(0));
    }

    #[test]
    fn repeat_code_terminates_without_evaluating() {
        // `** {code}`: must terminate WITHOUT evaluating the code block (it
        // could have side effects / rely on runtime-only state). Measured
        // length stops right before the quantified atom's contribution.
        let (len, stopped) = measure(r"a 'b' ** {1}", "abbb");
        assert!(stopped);
        assert_eq!(len, Some(1));
    }

    #[test]
    fn nested_subrule_sees_terminator_through_recursion() {
        // The declarative prefix must descend into a subrule and see a
        // terminator nested inside it (mirrors `declarative_prefix_match_len`'s
        // existing subrule-descent behavior for code atoms). Uses the real
        // grammar/token declaration path (`Interpreter::run`) rather than
        // poking registry internals directly, so the test tracks whatever
        // storage `token`/`rule` declarations actually use.
        let mut interp = Interpreter::new();
        interp
            .run("grammar G { token item { a <.ws> b } }")
            .expect("grammar declaration should run");
        let outer = interp
            .parse_regex_with_mode("<item>", RegexParseMode::Match)
            .expect("outer pattern should parse");
        let chars: Vec<char> = "a   b".chars().collect();
        let (len, stopped) = interp.ltm_prefix_len_at(&outer, &chars, 0, "G");
        assert!(stopped);
        assert_eq!(len, Some(1));
    }

    /// Measure `pattern`'s `ltm_litlen_at` against `text` at position 0 in
    /// the empty package.
    fn litlen(pattern: &str, text: &str) -> usize {
        let mut interp = Interpreter::new();
        let parsed = interp
            .parse_regex_with_mode(pattern, RegexParseMode::Match)
            .expect("pattern should parse");
        let chars: Vec<char> = text.chars().collect();
        let mut seen = HashSet::new();
        interp.ltm_litlen_at(&parsed, &chars, 0, "", &mut seen, 0)
    }

    #[test]
    fn pure_literal_chain_measures_full_length() {
        assert_eq!(litlen("abc", "abcdef"), 3);
    }

    #[test]
    fn literal_chain_stops_at_mismatch() {
        assert_eq!(litlen("abc", "abx"), 2);
    }

    #[test]
    fn capture_group_kills_litlen_even_when_pure_literal() {
        // `('abc')` as the whole pattern: capture ends litlen immediately,
        // contributing nothing at all — ADR-0022 §2/§4.3.
        assert_eq!(litlen("('abc')", "abc"), 0);
    }

    #[test]
    fn capture_group_kills_litlen_after_leading_literal() {
        // `'a' (\w\w)`: the leading 'a' still counts; the capture group ends
        // the chain right after it.
        assert_eq!(litlen(r"a (\w\w)", "abc"), 1);
    }

    #[test]
    fn quantifier_ends_litlen() {
        // NOT `'ab' ** 2`: a captureless, separator-less, single-atom fixed-
        // count `**N` is string-unrolled into literal repeated text by the
        // pre-existing `expand_ltm_pattern` engine pass BEFORE the token
        // parser ever runs (`regex_parse_core.rs`'s `mode ==
        // RegexParseMode::Match` branch) — so by the time this walk sees it,
        // it is indistinguishable from a hand-written `'abab'` and the
        // quantifier-boundary information this rule depends on is already
        // gone. `+`/`*`/`?` are NOT touched by that pass (its trigger regex
        // matches literal `**` only), so they exercise the real check.
        assert_eq!(litlen("'ab'+", "abab"), 0);
    }

    #[test]
    fn non_capturing_group_descends_and_continues() {
        // `[ab]c`: the group is pure literal and reaches its own end, so the
        // outer chain continues past it into the trailing 'c'.
        assert_eq!(litlen("[ab] c", "abc"), 3);
    }

    #[test]
    fn nested_alternation_all_pure_literal_extends_chain() {
        // `"/c/" [ 'tree' | 'x' ]`: both nested branches are pure literal, so
        // litlen continues through the longest one that actually matches.
        assert_eq!(litlen(r#""/c/" [ 'tree' | 'x' ]"#, "/c/tree"), 7);
    }

    #[test]
    fn nested_alternation_non_pure_branch_stops_chain_after_contribution() {
        // One branch is not pure-literal (`\w+`); the nested `|` still
        // contributes its best matching length, but does not let the OUTER
        // chain continue past it.
        assert_eq!(litlen(r"a [ 'b' | \w+ ] c", "abc"), 2);
    }

    #[test]
    fn char_class_ends_litlen() {
        assert_eq!(litlen(r"a \w b", "aab"), 1);
    }

    #[test]
    fn case_insensitive_literal_extends_via_pattern_flag() {
        assert_eq!(litlen("abc", "ABC"), 0); // :i not set -> no match at all
        let mut interp = Interpreter::new();
        let parsed = interp
            .parse_regex_with_mode("abc", RegexParseMode::Match)
            .expect("pattern should parse");
        // Simulate `:i` by constructing the pattern with ignore_case set —
        // the parser's own `:i` plumbing is exercised elsewhere; this test
        // only pins that ltm_litlen_at honors `pattern.ignore_case`.
        let mut ci_pattern = parsed;
        ci_pattern.ignore_case = true;
        let chars: Vec<char> = "ABC".chars().collect();
        let mut seen = HashSet::new();
        let len = interp.ltm_litlen_at(&ci_pattern, &chars, 0, "", &mut seen, 0);
        assert_eq!(len, 3);
    }

    #[test]
    fn subrule_descent_extends_litlen_through_pure_literal_callee() {
        let mut interp = Interpreter::new();
        interp
            .run("grammar G { token abb { 'abb' } }")
            .expect("grammar declaration should run");
        let pattern = interp
            .parse_regex_with_mode("<abb>", RegexParseMode::Match)
            .expect("pattern should parse");
        let chars: Vec<char> = "abb".chars().collect();
        let mut seen = HashSet::new();
        let len = interp.ltm_litlen_at(&pattern, &chars, 0, "G", &mut seen, 0);
        assert_eq!(len, 3);
    }

    #[test]
    fn subrule_descent_stops_at_non_literal_callee_content() {
        let mut interp = Interpreter::new();
        interp
            .run(r"grammar G { token item { a \w } }")
            .expect("grammar declaration should run");
        let pattern = interp
            .parse_regex_with_mode("<item>", RegexParseMode::Match)
            .expect("pattern should parse");
        let chars: Vec<char> = "ab".chars().collect();
        let mut seen = HashSet::new();
        let len = interp.ltm_litlen_at(&pattern, &chars, 0, "G", &mut seen, 0);
        assert_eq!(len, 1);
    }

    #[test]
    fn direct_left_recursive_subrule_cycle_guard_terminates() {
        // A token whose body calls itself must not blow the stack: `seen`
        // cuts the cycle and the chain simply stops there.
        let mut interp = Interpreter::new();
        interp
            .run("grammar G { token loopy { 'a' <loopy> } }")
            .expect("grammar declaration should run");
        let pattern = interp
            .parse_regex_with_mode("<loopy>", RegexParseMode::Match)
            .expect("pattern should parse");
        let chars: Vec<char> = "aaaa".chars().collect();
        let mut seen = HashSet::new();
        // Must terminate (not stack-overflow / infinite-loop) and return SOME
        // bounded length; the exact value is an implementation detail of
        // where the cycle guard cuts in, so only assert boundedness.
        let len = interp.ltm_litlen_at(&pattern, &chars, 0, "G", &mut seen, 0);
        assert!(len <= chars.len());
    }
}
