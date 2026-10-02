//! ADR-0022: LTM ranking of `|` branches and proto candidates.
//!
//! The measurement primitive (`ltm_prefix_len_at`) runs the pattern's NFA
//! (ADR-0125, `regex_ltm_nfa`); `ltm_atom_mode` is the shared classifier the
//! NFA builder, and the atom matchers answering its leaves, consult for what
//! is a fate. The rank key that combines the prefix length with the
//! `litlen` tie-break (ADR-0022 §2), which the same NFA run measures
//! (`regex_ltm_litend`), lives here too.

use super::super::*;
use super::regex_helpers::named_lookup_is_ws;

/// How an atom participates in LTM declarative-prefix measurement
/// (ADR-0022 §4.2's prefix-construction table). `CodeAssertion` and
/// `SequentialAlternation` are deliberately NOT covered here: the NFA builder
/// gives each its own construction (a plain block is a fate, `<?{ }>` a pass,
/// `||` its first branch plus an ε bypass).
pub(super) enum LtmAtomMode<'a> {
    /// Measure exactly as a real match would (consuming/transparent).
    Normal,
    /// A fate: the path ends here, and here counts toward the prefix.
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

/// Classify `atom` for LTM declarative-prefix measurement: by the NFA builder,
/// and by the atom matchers while they answer an NFA leaf under
/// `LTM_DECLARATIVE_MODE` (callers check the mode themselves, so the check
/// happens once per atom match, not once per classification).
pub(super) fn ltm_atom_mode(atom: &RegexAtom) -> LtmAtomMode<'_> {
    match atom {
        // <.ws> / <ws> / implicit sigspace: Rakudo's NFA special-cases `ws` ->
        // fate (terminate), same as a subrule that names it in any lookup form.
        RegexAtom::WsRule => LtmAtomMode::Terminate,
        RegexAtom::Named(name) if named_lookup_is_ws(name) => LtmAtomMode::Terminate,
        // `<!>` — the always-fail assertion (parsed as `Named("!")`). Rakudo
        // dispatches it as a Cursor method, so its NFA has no edge for it and it
        // becomes a fate: the declarative prefix ends *before* it. Measuring it
        // normally instead lets it fail the whole measurement, which reports the
        // branch's prefix as 0 and mis-ranks it against its `|` siblings —
        // `/ 'foo' | ( 'food' <!> || { ... } ) /` must rank the group first
        // (prefix "food"), enter it, and reach the `||` branch after `<!>` fails
        // for real (verified against `raku`).
        RegexAtom::Named(name) if name == "!" => LtmAtomMode::Terminate,
        // A package-qualified subrule call (`<CSS::Grammar::Core::_arg>`,
        // `<G::x>`): Rakudo's NFA looks the rule up by name as a method of the
        // cursor, finds none, and puts a fate there -- even when the package is
        // the grammar's own (verified against `raku`; issue #9053).
        RegexAtom::Named(name) if crate::qualified::is_qualified(name.spec().lookup_sym) => {
            LtmAtomMode::Terminate
        }
        // Backreferences depend on a capture made so far in THIS match, not on
        // the pattern's declarative structure — Rakudo's NFA has no method for
        // them, so they terminate.
        RegexAtom::Backref(_) | RegexAtom::NamedBackref(_) => LtmAtomMode::Terminate,
        // A bare `$var` interpolating an in-regex lexical is not a compile-time
        // literal; terminate (constants are inlined as literals before this
        // atom kind is ever produced, so they never reach here — see §2).
        RegexAtom::VarInterp(_) | RegexAtom::QqInterp { .. } => LtmAtomMode::Terminate,
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
        RegexAtom::ClosureInterpolation { .. } | RegexAtom::CodeInterp { .. } => {
            LtmAtomMode::Terminate
        }
        // A character class built with set SUBTRACTION (`<[\x1F..\xFF] - [;]>`,
        // `<+alpha - [q]>`, `<-[;] - [q]>`): Rakudo's NFA has no single edge
        // kind for "this set minus that set", so the class becomes a fate arc
        // and terminates the declarative prefix — while every subtraction-free
        // class (`<-[;]>`, `<[A..z]>`, `<+alpha>`, `\w`, `.`) participates
        // normally. Validated against `raku` across all of those shapes; note
        // the rule is about the class's *written structure*, not its resulting
        // character set (`<-[;] - [q]>` terminates, the equivalent `<-[;q]>`
        // does not) and not about the quantifier (a single unquantified
        // subtracted class terminates too). A `CompositeClass` with an empty
        // `negative` is a subtraction-free union and keeps participating.
        RegexAtom::CompositeClass { negative, .. } if !negative.is_empty() => {
            LtmAtomMode::Terminate
        }
        // `<:L>` / `<-:L>` / `<:!L>`: a Unicode-property atom has no NFA edge
        // in Rakudo, so it is a fate (ADR-0111 §5).
        RegexAtom::UnicodeProp { .. } => LtmAtomMode::Terminate,
        // `<-alpha>`: a negated *named* class has no NFA edge either, while
        // `\W`/`\D`/`\S` and `<-[..]>` do.
        RegexAtom::CharClass(class)
            if class.negated
                && class.items.iter().any(|item| {
                    matches!(
                        item,
                        ClassItem::NamedBuiltin(_) | ClassItem::UnicodePropItem { .. }
                    )
                }) =>
        {
            LtmAtomMode::Terminate
        }
        // `&` / `&&` conjunction: no NFA method in Rakudo -> fate (terminate).
        RegexAtom::Conjunction(_) => LtmAtomMode::Terminate,
        // `<~~>` recurses into the enclosing regex, which is exactly the
        // structure being measured — inlining it would not terminate. Rakudo's
        // NFA has no method for it either, so it is a fate.
        RegexAtom::RecurseSelf(_) => LtmAtomMode::Terminate,
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

/// A rule's implicit leading `<.ws>` is zero-width when the subject starts at
/// a non-whitespace character. It must not erase the rule's useful prefix
/// during LTM measurement; explicit/interior whitespace still terminates the
/// prefix at the position where it occurs.
pub(super) fn ltm_leading_ws_is_transparent(atom: &RegexAtom, pos: usize) -> bool {
    if pos != 0 {
        return false;
    }
    match atom {
        RegexAtom::WsRule => true,
        RegexAtom::Named(name) => named_lookup_is_ws(name),
        _ => false,
    }
}

impl Interpreter {
    /// ADR-0022 §4.1: the longest declarative-prefix match of `pattern` at
    /// `pos`, plus whether a `None` length is unsound to filter on (see
    /// [`super::regex_ltm_nfa::LtmMeasure::stopped`]): a caller may drop a
    /// candidate on `(None, false)` only. Measured by the pattern's NFA
    /// (ADR-0125), which never executes user code (ADR-0009).
    // Cost: see `ltm_measure`.
    pub(crate) fn ltm_prefix_len_at(
        &mut self,
        pattern: &RegexPattern,
        chars: &[char],
        pos: usize,
        pkg: Symbol,
    ) -> (Option<usize>, bool) {
        let measured = self.ltm_measure(pattern, chars, pos, pkg);
        (measured.len, measured.stopped)
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
        pkg: Symbol,
    ) -> (usize, usize) {
        self.ltm_measure(alt, chars, pos, pkg).branch_rank()
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
    pub(in crate::runtime) fn ltm_rank_token_candidate_source(
        &mut self,
        pattern: &str,
        text: &str,
    ) -> (Option<(usize, usize)>, bool) {
        // A source that does not parse proves nothing: the real match
        // reports it.
        let Some(parsed) = self.parse_regex(pattern) else {
            return (None, true);
        };
        let target = MatchTarget::new(text);
        let _target_scope = super::regex_helpers::MatchTargetScope::enter(target.clone());
        let chars = target.chars();
        let pkg = self.current_package_sym();
        let measured = self.ltm_measure(&parsed, chars, 0, pkg);
        let (plen, stopped) = (measured.len, measured.stopped);
        // A prefix that ends in a fate ranks by where the fate is, as in
        // Rakudo: `t:sym<a> { 'abc' {} 'd' }` (prefix 3) outranks
        // `t:sym<b> { 'ab' }` on "abcd".
        let Some(plen) = plen else {
            return (None, stopped);
        };
        (Some((plen, measured.litlen)), stopped)
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
        interp.ltm_prefix_len_at(&parsed, &chars, 0, Symbol::intern(""))
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
        // `stopped` is the "do NOT filter on this result" flag. The ε-bypass
        // continues the walk at the group's START position, so anything after
        // the group is measured against text the real match would have
        // consumed and can fail spuriously — the whole measurement is
        // therefore unsound to filter on (ADR-0046 Slice 4). It does not
        // truncate the measured length.
        assert!(stopped);
        assert_eq!(len, Some(0));
    }

    #[test]
    fn sequential_alternation_measures_first_branch_when_it_matches() {
        let (len, stopped) = measure(r"['food' || 'doof']", "food");
        // Unsound-to-filter for the same reason as the test above, even though
        // the first branch matched: the ε alternative is still offered, so a
        // later atom could have been measured at the wrong position.
        assert!(stopped);
        assert_eq!(len, Some(4));
    }

    #[test]
    fn sequential_alternation_never_the_sole_reason_for_none() {
        // A pattern that is ENTIRELY `X || Y` must still measure as Some(0)
        // (the epsilon), never None, even when no branch matches at all.
        let (len, stopped) = measure(r"['zzz' || 'yyy']", "food");
        assert!(stopped); // unsound to filter on -- see the two tests above
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
        let (len, stopped) = interp.ltm_prefix_len_at(&outer, &chars, 0, Symbol::intern("G"));
        assert!(stopped);
        assert_eq!(len, Some(1));
    }

    /// Measure `pattern`'s `litlen` against `text` at position 0 in the
    /// empty package.
    fn litlen(pattern: &str, text: &str) -> usize {
        let mut interp = Interpreter::new();
        let parsed = interp
            .parse_regex_with_mode(pattern, RegexParseMode::Match)
            .expect("pattern should parse");
        let chars: Vec<char> = text.chars().collect();
        interp
            .ltm_measure(&parsed, &chars, 0, Symbol::intern(""))
            .litlen
    }

    /// Measure the `litlen` of `pattern` in grammar `G`, declared by `grammar`.
    fn litlen_in_g(grammar: &str, pattern: &str, text: &str) -> usize {
        let mut interp = Interpreter::new();
        interp.run(grammar).expect("grammar declaration should run");
        let parsed = interp
            .parse_regex_with_mode(pattern, RegexParseMode::Match)
            .expect("pattern should parse");
        let chars: Vec<char> = text.chars().collect();
        interp
            .ltm_measure(&parsed, &chars, 0, Symbol::intern("G"))
            .litlen
    }

    #[test]
    fn pure_literal_chain_measures_full_length() {
        assert_eq!(litlen("abc", "abcdef"), 3);
    }

    #[test]
    fn a_branch_that_cannot_match_has_no_litlen() {
        // MoarVM records `longlit` for a fate only when the fate is reached.
        assert_eq!(litlen("abc", "abx"), 0);
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
        // NQP's `_I_LL` edge: a `:i` literal counts like any other.
        assert_eq!(litlen(":i abc", "ABC"), 3);
    }

    #[test]
    fn subrule_descent_extends_litlen_through_pure_literal_callee() {
        let grammar = "grammar G { token abb { 'abb' } }";
        assert_eq!(litlen_in_g(grammar, "<abb>", "abb"), 3);
    }

    #[test]
    fn subrule_descent_stops_at_non_literal_callee_content() {
        let grammar = r"grammar G { token item { a \w } }";
        assert_eq!(litlen_in_g(grammar, "<item>", "ab"), 1);
    }

    #[test]
    fn direct_left_recursive_subrule_cycle_guard_terminates() {
        // A token whose body calls itself must not blow the stack: the
        // recursion cut ends the path there.
        let grammar = "grammar G { token loopy { 'a' <loopy> } }";
        assert!(litlen_in_g(grammar, "<loopy>", "aaaa") <= 4);
    }

    #[test]
    fn a_literal_after_a_subrule_call_does_not_count() {
        // `regex_nfa` closes `$!LITEND` for the `subrule` node: only the
        // callee's own leading literals count, not the caller's `'bc'`.
        let grammar = "grammar G { token a { 'a' } }";
        assert_eq!(litlen_in_g(grammar, "<a> 'bc'", "abc"), 1);
    }

    #[test]
    fn a_subrule_counts_its_own_literals_under_a_quantifier() {
        // ANTLR4::Grammar's `[<e> '-']? <e>`: the callee's `'x'` counts even
        // though the call is quantified, while a literal written in the
        // quantified group itself, or after it, does not.
        let grammar = "grammar G { token e { 'x' <[0..9]> } }";
        assert_eq!(litlen_in_g(grammar, "[<e> '-']? 'x'", "x1-x"), 1);
        assert_eq!(litlen_in_g(grammar, "[ 'x' '1' '-' ]? 'x'", "x1-x"), 0);
        // The furthest crossing counts: the second call's `'x'` ends at 4.
        assert_eq!(litlen_in_g(grammar, "[<e> '-']? <e>", "x1-x2"), 4);
    }
}
