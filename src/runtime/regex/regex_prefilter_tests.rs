//! Unit tests for [`super::regex_prefilter`], in their own file so the module
//! they cover stays inside the repository's 500-line limit (the same split
//! `registry_method_table.rs` uses).

use super::*;

fn parse(pattern: &str) -> std::sync::Arc<RegexPattern> {
    let interp = crate::runtime::Interpreter::new();
    interp
        .parse_regex(pattern)
        .expect("pattern should parse for this test")
}

fn positions(pattern: &str, subject: &str) -> Vec<usize> {
    let mut interp = crate::runtime::Interpreter::new();
    let parsed = interp
        .parse_regex(pattern)
        .expect("pattern should parse for this test");
    let chars: Vec<char> = subject.chars().collect();
    let pkg = interp.current_package_sym();
    regex_scan_positions(&mut interp, &parsed, &chars, 0, pkg).collect()
}

#[test]
fn plain_literal_is_a_usable_prefix() {
    assert_eq!(
        required_literal_prefix(&parse("'hello'")),
        Some("hello".to_string())
    );
    assert_eq!(
        required_literal_prefix(&parse("hello")),
        Some("hello".to_string())
    );
}

#[test]
fn a_trailing_non_literal_still_yields_the_leading_prefix() {
    // "abc" must occur, whatever \d+ then requires -- the required
    // prefix is a valid (if not tight) necessary condition either way.
    assert_eq!(
        required_literal_prefix(&parse(r"abc \d+")),
        Some("abc".to_string())
    );
}

#[test]
fn a_quoted_group_extends_the_prefix_before_the_trailing_pattern() {
    // A quoted literal is represented as a transparent `Group` once it
    // appears in a larger regex. Its entire body is still required at the
    // start, so the substring prefilter must not fall back to the weaker
    // required-inner-literal path (#8449).
    assert_eq!(
        required_literal_prefix(&parse(r"'67-8' \s $")),
        Some("67-8".to_string())
    );
}

#[test]
fn quantified_leading_literal_declines() {
    // "a" is optional here, so it is not REQUIRED at all.
    assert_eq!(required_literal_prefix(&parse("a? bc")), None);
}

#[test]
fn case_insensitive_declines() {
    assert_eq!(required_literal_prefix(&parse(":i 'ABC'")), None);
}

#[test]
fn ignoremark_declines() {
    assert_eq!(required_literal_prefix(&parse(":m 'cafe'")), None);
}

#[test]
fn alternation_has_no_top_level_literal_prefix() {
    assert_eq!(required_literal_prefix(&parse("'foo' | 'bar'")), None);
}

#[test]
fn a_named_capture_ends_the_chain_immediately() {
    assert_eq!(required_literal_prefix(&parse("$<x>=[a] bc")), None);
}

#[test]
fn scan_positions_finds_the_literal_and_nothing_else() {
    assert_eq!(positions("'ab'", "xxabxxabxx"), vec![2, 6]);
}

#[test]
fn scan_positions_respects_the_kill_switch() {
    // `prefilter_enabled` memoizes process-wide on first read, so a test
    // cannot flip it; this only pins that a real occurrence is never
    // missed either way.
    assert_eq!(positions("'zz'", "no zz here at all"), vec![3]);
}

#[test]
fn a_subrule_call_declines_so_every_position_is_kept() {
    // A prefix derived through `<foo>` would have to be keyed by invocant
    // package and `TOKEN_DEFS_GEN` to survive a dynamic override
    // (ADR-0099 §4 constraint 3), so the analysis declines instead.
    assert_eq!(positions("<foo>", "abc"), vec![0, 1, 2, 3]);
}

#[test]
fn a_character_class_narrows_the_scan_to_its_members() {
    assert_eq!(positions(r"\d+", "ab1cd2"), vec![2, 5]);
    assert_eq!(positions(r"<[xyz]>", "axbycz"), vec![1, 3, 5]);
}

#[test]
fn a_negated_class_narrows_to_its_complement() {
    assert_eq!(positions(r"<-[abc]>", "abXcd"), vec![2, 4]);
}

#[test]
fn an_alternation_unions_its_branches_first_characters() {
    // Only the two branch-leading characters are candidates -- and the
    // last two positions are ruled out by the 3-character minimum both
    // branches share.
    assert_eq!(positions("'foo' | 'bar'", "zfoozbarz"), vec![1, 5]);
}

#[test]
fn ignorecase_uses_the_fold_closure_not_a_folded_needle() {
    // Both cases of the leading letter are candidates, and nothing else.
    assert_eq!(positions(":i 'z'", "aZbzc"), vec![1, 3]);
}

#[test]
fn a_leading_zero_width_assertion_is_passed_through() {
    // `^^` consumes nothing, so the first-set is still the literal's.
    assert_eq!(positions("^^ 'a'", "xaya"), vec![1, 3]);
}

#[test]
fn a_nullable_pattern_keeps_every_position() {
    assert_eq!(positions(r"\d*", "ab"), vec![0, 1, 2]);
}

#[test]
fn a_leading_code_block_declines_so_it_still_runs_per_position() {
    // ADR-0009: a leading `{ … }` runs once per start position in both
    // mutsu and rakudo, so the prefilter must not skip any.
    assert_eq!(positions(r"{ 1 } 'z'", "ab"), vec![0, 1, 2]);
}

#[test]
fn a_minimum_length_truncates_the_tail_of_the_range() {
    // `. . .` has a universal first-set (so no character filtering) but
    // still cannot start within two characters of the end.
    assert_eq!(positions(". . .", "abcde"), vec![0, 1, 2]);
    assert!(positions(". . .", "ab").is_empty());
}

#[test]
fn a_literal_prefix_longer_than_the_subject_yields_nothing() {
    assert!(positions("'abcdef'", "abc").is_empty());
}

#[test]
fn a_unicode_property_narrows_the_scan_to_its_members() {
    // Derived by calling the engine's own property predicate over ASCII,
    // the same way a character class is, rather than widening to
    // "anything" because the atom spells its members as a property.
    assert_eq!(positions("<:Lu>", "aBcD"), vec![1, 3]);
    assert_eq!(positions("<:Nd>", "a1b2"), vec![1, 3]);
}

#[test]
fn a_negated_unicode_property_narrows_to_its_complement() {
    assert_eq!(positions("<:!Nd>", "1a2b"), vec![1, 3]);
}

#[test]
fn a_property_with_no_ascii_member_still_admits_every_non_ascii_one() {
    // The ASCII half comes out empty; non-ASCII is admitted wholesale
    // rather than enumerated, which is what keeps the derivation cheap.
    assert_eq!(positions("<:Greek>", "ab\u{3a9}cd"), vec![2]);
}

#[test]
fn a_scoped_ignoremark_is_derived_from_the_stripped_pattern() {
    // The engine matches `[:m 'x']` as the mark-stripped pattern against
    // the mark-stripped subject, so the set is `{x}` -- and on an ASCII
    // subject that is exactly the positions holding an `x`.
    assert_eq!(positions("[:m 'x']", "axbxc"), vec![1, 3]);
}

#[test]
fn a_mark_skewed_set_admits_a_position_it_cannot_speak_for() {
    // Stripping skips a position that does not start a grapheme cluster,
    // and the derived set then says nothing about the character sitting
    // there -- so every such position must still be offered. Position 1 is
    // the combining mark, which stripping removes entirely; it is admitted
    // because a mark-skewed set takes every non-ASCII character. Position 0
    // is NOT skipped: an ASCII character always starts its own cluster and
    // is always its first base, so the stripped subject there begins with
    // that same `e` and the set may rule it out.
    assert_eq!(positions("[:m 'x']", "e\u{301}x"), vec![1, 2]);
    // The same pattern over a pure-ASCII subject keeps its precision: no
    // position is inside a cluster, so the bitmap decides all of them.
    assert_eq!(positions("[:m 'x']", "eex"), vec![2]);
}

#[test]
fn a_scoped_ignoremark_claims_no_length_bound() {
    // Stripping is not injective on positions (both halves of a `\r\n`
    // cluster map to its start), so a sub-pattern consuming two stripped
    // characters can cover zero original ones. A length bound derived in
    // stripped space would not be a lower bound in the original.
    let prefilter = Prefilter::build(
        &mut Analyzer::pattern_only(),
        &parse("[:m 'abc']"),
        Symbol::intern(""),
    );
    assert_eq!(prefilter.min_len, 0);
    // The same pattern without `:m` does claim one.
    let plain = Prefilter::build(
        &mut Analyzer::pattern_only(),
        &parse("[ 'abc' ]"),
        Symbol::intern(""),
    );
    assert_eq!(plain.min_len, 3);
}

#[test]
fn a_composite_class_narrows_to_its_positive_items() {
    // `<+alpha>` is a `CompositeClass`, not a `CharClass`, and every slice
    // before the sixth widened the whole atom to "anything".
    assert_eq!(positions("<+xdigit>", "g1hZi"), vec![1]);
}

#[test]
fn a_composite_class_subtracts_its_negative_items() {
    // A negative item may narrow with no resolution at all: the engine runs
    // the character half first and short-circuits, so a character it matches
    // is one `pos_match && !neg_match` rejects outright.
    assert_eq!(positions("<+xdigit -[1]>", "1x2y3"), vec![2, 4]);
    assert_eq!(positions("<[a..z] - [aeiou]>", "aeixou"), vec![3]);
}

#[test]
fn a_purely_negated_composite_class_starts_from_every_character() {
    // An empty `positive` means "any character" to the engine, so the
    // derivation starts universal and lets the negatives carve it down.
    assert_eq!(positions("<-[;] - [q]>", ";qaq;b"), vec![2, 5]);
}

#[test]
fn a_composite_class_admits_non_ascii_only_where_it_must() {
    // A built-in name is evaluated over the whole of Unicode by a predicate
    // this analysis only samples over ASCII, so every non-ASCII character is
    // admitted; a plain range item keeps the exact answer `<[a..z]>` gets.
    assert_eq!(positions("<+alpha -[a..z]>", "ab\u{e9}cd"), vec![2]);
    assert!(positions("<[a..z] - [aeiou]>", "\u{e9}\u{3a9}").is_empty());
}

#[test]
fn a_composite_class_reads_the_registry_only_through_a_positive_named_item() {
    // Two statements of the same condition have to agree: the memo picks the
    // package-keyed slot from `mentions_subrule`, and the analysis reads the
    // rule registry from `composite_class_reads_registry`. A registry-dependent
    // set that reached the pattern-keyed slot would answer for every package —
    // a wrong answer, not a slow scan.
    //
    // (That a defined `token xdigit` then actually makes the atom decline is
    // pinned end-to-end in `tests/regex_prefilter_engagement.rs`, which can
    // declare a grammar.)
    assert!(mentions_subrule(&parse("<+xdigit -[1]>")));
    // No `NamedBuiltin` anywhere: a pure function of the pattern.
    assert!(!mentions_subrule(&parse("<[a..z] - [aeiou]>")));
}
