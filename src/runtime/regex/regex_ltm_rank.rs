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
    /// Not yet called from non-test code: this is ADR-0022 Slice 1
    /// (measurement infrastructure only). Slice 3 wires it into the three
    /// alternation-ranking consumer arms, at which point this attribute goes
    /// away. TODO(ADR-0022 Slice 3): remove `#[allow(dead_code)]` once wired.
    #[allow(dead_code)]
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
}
