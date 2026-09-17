//! Unit tests for [`super::regex_prefilter_chain`], in their own file for the
//! same 500-line-budget reason `regex_prefilter_tests.rs` is split out.

use super::*;
use crate::runtime::regex::regex_prefilter::regex_scan_positions;

fn positions(pattern: &str, subject: &str) -> Vec<usize> {
    let mut interp = crate::runtime::Interpreter::new();
    let parsed = interp
        .parse_regex(pattern)
        .expect("pattern should parse for this test");
    let chars: Vec<char> = subject.chars().collect();
    let pkg = interp.current_package_sym();
    regex_scan_positions(&mut interp, &parsed, &chars, 0, pkg).collect()
}

/// Like [`positions`], but running `grammar_src` (declaring `grammar G {
/// ... }`) first, so `pattern`'s `<G::name>` references resolve against a
/// real rule registry entry — the package-keyed path
/// [`super::super::regex_prefilter_memo::pattern_prefilter_in_pkg`] takes.
fn positions_in_grammar(grammar_src: &str, pattern: &str, subject: &str) -> Vec<usize> {
    let mut interp = crate::runtime::Interpreter::new();
    interp
        .run(grammar_src)
        .expect("grammar declaration should run for this test");
    let parsed = interp
        .parse_regex(pattern)
        .expect("pattern should parse for this test");
    let chars: Vec<char> = subject.chars().collect();
    regex_scan_positions(&mut interp, &parsed, &chars, 0, Symbol::intern("G")).collect()
}

/// The chain [`super::build_chain`] actually surfaces to a [`Prefilter`] —
/// `None` (reported as 0) both when the walk cannot say anything at all AND
/// when it pins fewer than two steps, since [`super::build_chain`]
/// deliberately does not surface a chain that says nothing beyond the
/// existing first-character set (see [`a_chain_of_two_or_more_steps_is_required_to_engage`]).
fn chain_len(pattern: &str) -> usize {
    let interp = crate::runtime::Interpreter::new();
    let parsed = interp
        .parse_regex(pattern)
        .expect("pattern should parse for this test");
    build_chain(&mut Analyzer::pattern_only(), &parsed, Symbol::intern(""))
        .map(|c| c.steps.len())
        .unwrap_or(0)
}

/// The RAW step count [`chain_pattern`] itself produces, bypassing
/// [`build_chain`]'s "fewer than two steps is not worth surfacing" cutoff —
/// what the tests below that want to distinguish "stops after exactly one
/// step" from "declines outright" actually need, since both read as `0`
/// through [`chain_len`].
fn raw_chain_len(pattern: &str) -> usize {
    let interp = crate::runtime::Interpreter::new();
    let parsed = interp
        .parse_regex(pattern)
        .expect("pattern should parse for this test");
    chain_pattern(
        &mut Analyzer::pattern_only(),
        &parsed,
        Symbol::intern(""),
        0,
    )
    .map(|p| p.steps.len())
    .unwrap_or(0)
}

/// Like [`raw_chain_len`], but through an interpreter-backed [`Analyzer`]
/// with NO invocant package — needed for a composite class's `NamedBuiltin`
/// item, since `Analyzer::pattern_only()` has no interpreter to ask at all
/// and so declines it unconditionally regardless of the package. An empty
/// package is the engine's own "no rule registry to fall back to" guard
/// (`composite_item_may_dispatch_token`), the same convention
/// `regex_prefilter_memo::pattern_prefilter` uses for a pattern with no rule
/// name; the case where a REAL grammar shadows the built-in name is pinned at
/// the integration level in `tests/regex_prefilter_engagement.rs`, which can
/// declare one.
fn chain_len_no_pkg(pattern: &str) -> usize {
    let mut interp = crate::runtime::Interpreter::new();
    let parsed = interp
        .parse_regex(pattern)
        .expect("pattern should parse for this test");
    build_chain(
        &mut Analyzer::with_interpreter(&mut interp),
        &parsed,
        Symbol::intern(""),
    )
    .map(|c| c.steps.len())
    .unwrap_or(0)
}

/// Like [`raw_chain_len`], but running `grammar_src` first and resolving
/// against package `G` — for the shapes that need a real rule registry entry
/// (a `<subrule>` with a bounded, separator-carrying quantifier chief among
/// them, since the parser cannot unroll THAT one into flat literal tokens the
/// way it does for a plain literal/class atom — see
/// [`a_separator_prevents_unrolling_a_subrules_second_mandatory_copy`]).
fn raw_chain_len_in_grammar(grammar_src: &str, pattern: &str) -> usize {
    let mut interp = crate::runtime::Interpreter::new();
    interp
        .run(grammar_src)
        .expect("grammar declaration should run for this test");
    let parsed = interp
        .parse_regex(pattern)
        .expect("pattern should parse for this test");
    chain_pattern(
        &mut Analyzer::with_interpreter(&mut interp),
        &parsed,
        Symbol::intern("G"),
        0,
    )
    .map(|p| p.steps.len())
    .unwrap_or(0)
}

#[test]
fn three_consecutive_digits_chain_to_three_steps() {
    // The whole motivating gap: a single first-character set admits every
    // isolated digit, but the second and third character of `\d\d\d` are
    // just as much a necessary condition as the first.
    assert_eq!(chain_len(r"\d\d\d"), 3);
}

#[test]
fn a_lone_digit_does_not_survive_a_three_digit_chain() {
    // "a1b22c333d": the only run of three consecutive digits starts at 6.
    assert_eq!(positions(r"\d\d\d", "a1b22c333d"), vec![6]);
}

#[test]
fn a_repeated_fixed_count_class_chains_too() {
    assert_eq!(chain_len("<:Lu> ** 4"), 4);
    // "AbCDEFg HIJK": two genuine runs of four consecutive uppercase letters
    // -- C-D-E-F at offset 2 and H-I-J-K at offset 8 -- so both are real
    // candidates a sound chain must offer, not just the second.
    assert_eq!(positions("<:Lu> ** 4", "AbCDEFg HIJK"), vec![2, 8]);
}

#[test]
fn a_plain_literal_run_chains_one_step_per_character() {
    assert_eq!(chain_len("'abc'"), 3);
}

#[test]
fn a_composite_class_chains_when_it_cannot_dispatch_a_token() {
    // No invocant package at all, so the engine's own guard means the
    // grammar-token fallback can never engage -- the same "no rule registry
    // to fall back to" case `composite_item_may_dispatch_token` special-cases.
    assert_eq!(chain_len_no_pkg("<+digit> <+digit>"), 2);
    assert_eq!(positions("<+digit> <+digit>", "a1 b22 c3"), vec![4]);
}

#[test]
fn a_class_with_a_grapheme_item_does_not_extend_past_it() {
    // The Devanagari conjunct U+0915 U+094D U+0937 ("kSa") has no single-codepoint
    // NFC form, so it stays a `ClassItem::Grapheme` rather than collapsing to
    // `ClassItem::Char` (see `regex_types.rs`'s own doc comment for this exact
    // example) -- and a `Grapheme` entry can consume more than one codepoint,
    // so the position right after this atom cannot be pinned.
    assert_eq!(raw_chain_len("<[\\x[915]\\x[94d]\\x[937]]> 'x'"), 1);
}

#[test]
fn a_class_matching_newline_does_not_extend_past_it() {
    // `\r\n` is one grapheme cluster in Raku, and a class matching `\n` can
    // therefore consume two characters at a `\r`.
    assert_eq!(raw_chain_len(r"<[\n a]> 'x'"), 1);
}

#[test]
fn a_quantified_leading_atom_stops_the_chain_at_zero_steps() {
    // The leading atom might not be consumed at all, so nothing about
    // position 0 can be pinned -- matches the existing first-set decline.
    assert_eq!(chain_len("a? 'bc'"), 0);
}

#[test]
fn an_optional_atom_after_a_pinned_run_just_stops_the_chain_there() {
    assert_eq!(chain_len("'ab' c?"), 2);
    // "xxaycxxabxx": position 2 is 'a' but followed by 'y', not 'b' -- only
    // position 7 ('a' then 'b') is a real candidate.
    assert_eq!(positions("'ab' c?", "xxaycxxabxx"), vec![7]);
}

#[test]
fn alternation_of_equal_length_branches_chains_the_union() {
    assert_eq!(chain_len("'aa' | 'bb'"), 2);
    // "zaazabzbbz": positions 1 ("aa") and 7 ("bb") are real matches of one
    // branch each. Position 4 ("ab") is neither branch, but this module
    // deliberately does not correlate WHICH branch's character occupied
    // offset 0 with what offset 1 must then be -- each offset's set is the
    // union across every branch independently (see the module doc comment's
    // "sound by construction" paragraph), so position 4 survives the chain
    // too. That is over-approximation, not a wrong answer: the full engine
    // still rejects "ab" as neither "aa" nor "bb" once it is tried there.
    assert_eq!(positions("'aa' | 'bb'", "zaazabzbbz"), vec![1, 4, 7]);
}

#[test]
fn alternation_of_unequal_length_branches_truncates_to_the_shorter() {
    // "a" | "bcd": only offset 0 is guaranteed by BOTH branches, since the
    // shorter one may already have handed control to whatever follows.
    assert_eq!(raw_chain_len("'a' | 'bcd'"), 1);
}

#[test]
fn ignorecase_never_extends_past_the_first_step() {
    assert_eq!(raw_chain_len(":i 'abc'"), 1);
    // Still correct, just not narrowed beyond position 0.
    assert_eq!(positions(":i 'abc'", "xxABCxx"), vec![2]);
}

#[test]
fn ignoremark_only_ever_offers_one_step() {
    assert_eq!(raw_chain_len("[:m 'abc']"), 1);
}

#[test]
fn a_leading_code_block_declines_the_whole_chain() {
    assert_eq!(chain_len(r"{ 1 } 'ab'"), 0);
}

#[test]
fn a_code_block_after_a_pinned_run_only_stops_the_chain_there() {
    // ADR-0009: the code block still runs once per surviving start position
    // -- this only pins that the chain itself does not decline outright.
    assert_eq!(chain_len(r"'ab' { 1 } 'c'"), 2);
}

#[test]
fn a_separator_prevents_unrolling_a_subrules_second_mandatory_copy() {
    // A plain literal/class's bounded repeat-with-separator is fully
    // unrolled by the PARSER into flat, separately-visible tokens (the
    // separator becomes its own literal token in between), which this
    // module's ordinary sequence-concatenation already handles correctly
    // with no special case. A `<subrule>`'s length is not known at parse
    // time, so `<G::kw> ** 2..3 % ','` instead stays ONE token whose
    // `RegexQuant::Repeat` carries the separator directly
    // (`token.separator.is_some()`). Naively concatenating two copies of
    // `kw`'s own chain back to back would silently assume no comma sits
    // between them, and reject a real "ab,ab" at the position right after
    // the first copy.
    let len = raw_chain_len_in_grammar("grammar G { token kw { 'ab' } }", "<G::kw> ** 2..3 % ','");
    assert!(
        len <= 2,
        "unrolled past the separator between iterations: {len} steps"
    );
}

#[test]
fn an_unresolvable_subrule_reference_declines_rather_than_panicking() {
    // `Analyzer::pattern_only()` has no interpreter to resolve `<kw>`
    // through at all, matching the single-position analysis's own decline
    // for a bare pattern-keyed derivation that mentions a rule name.
    assert_eq!(chain_len(r"<kw> 'z'"), 0);
    // Confirmed narrowed once a real grammar and package are in the picture
    // -- pinned end-to-end (with `positions()`, going through the real
    // package-keyed dispatch `regex_scan_positions` performs) rather than
    // through a hand-built `Analyzer`, since building a rule registry entry
    // needs running a `grammar` declaration. "xaazbby": only the "aa"
    // occurrence (offset 1) is followed by 'z'; the "bb" occurrence
    // (offset 4) is followed by 'y', not 'z'.
    assert_eq!(
        positions_in_grammar(
            "grammar G { token kw { 'aa' | 'bb' } }",
            "<G::kw> 'z'",
            "xaazbby"
        ),
        vec![1]
    );
}

#[test]
fn a_chain_of_two_or_more_steps_is_required_to_engage() {
    // A single-step chain says nothing the existing first-character set does
    // not already say, so it is not built at all.
    assert_eq!(chain_len(r"\d"), 0);
}

#[test]
fn the_chain_length_is_bounded() {
    let long = "a".repeat(MAX_CHAIN_LEN + 10);
    let pattern = long
        .chars()
        .map(|c| format!("'{c}'"))
        .collect::<Vec<_>>()
        .join(" ");
    assert!(chain_len(&pattern) <= MAX_CHAIN_LEN);
}
