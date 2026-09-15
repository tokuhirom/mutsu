//! The ADR-0099 Stage 1 prefilter's *engagement* test (#8272).
//!
//! The differential test next door (`regex_prefilter_differential.rs`) pins
//! that the prefilter never changes an answer. This one pins the other
//! failure mode the issue names: that it silently stops applying, so a failing
//! scan quietly goes back to being linear in the subject length with nothing
//! but a benchmark to notice.
//!
//! It asserts on the `MUTSU_VM_STATS` counters rather than on wall-clock,
//! which makes it an exact, load-independent statement of the same property:
//! `positions_offered` is what the unfiltered scan would have had to walk and
//! `position_hits` is what the prefilter actually handed the engine, so
//! "sub-linear in subject length" is `position_hits` staying flat while
//! `positions_offered` grows with the subject. A timing assertion would say
//! the same thing far less reliably on a loaded CI box.

use std::process::Command;

/// Run `src` under `MUTSU_VM_STATS` and return its `regex-prefilter:` counters.
fn prefilter_stats(src: &str) -> Stats {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(src);
    cmd.env("MUTSU_VM_STATS", "1");
    let output = cmd.output().expect("failed to spawn mutsu");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "program failed: {stderr}\nprogram: {src}"
    );
    let line = stderr
        .lines()
        .find(|line| line.contains("regex-prefilter:"))
        .unwrap_or_else(|| panic!("no regex-prefilter stats line in stderr: {stderr}"))
        .to_string();
    let counter = |name: &str| {
        line.split(|c: char| c.is_whitespace() || c == '(' || c == ')')
            .find_map(|word| word.strip_prefix(name))
            .and_then(|value| value.parse::<u64>().ok())
            .unwrap_or_else(|| panic!("missing {name} in: {line}"))
    };
    Stats {
        literal_prefix: counter("literal_prefix="),
        inner_literal: counter("inner_literal="),
        first_char_set: counter("first_char_set="),
        positions_offered: counter("positions_offered="),
        position_hits: counter("position_hits="),
        line,
    }
}

struct Stats {
    literal_prefix: u64,
    inner_literal: u64,
    first_char_set: u64,
    positions_offered: u64,
    position_hits: u64,
    line: String,
}

/// A subject of `repeats` copies of a 42-character unit that contains no `z`
/// or `q` in either case and exactly one digit, so every pattern below scans
/// it to the end and fails.
fn failing_scan(pattern: &str, repeats: usize) -> String {
    format!(
        r#"my $big = "the5wild_brown-fox+jumps/over=the,lame.dog" x {repeats};
say ($big ~~ {pattern}).Bool;"#
    )
}

#[test]
fn a_failing_literal_scan_is_sublinear_in_subject_length() {
    let small = prefilter_stats(&failing_scan("/ 'zzzq-not-here' /", 100));
    let large = prefilter_stats(&failing_scan("/ 'zzzq-not-here' /", 800));

    assert!(
        small.literal_prefix >= 1 && large.literal_prefix >= 1,
        "the literal-prefix prefilter did not engage at all:\n{}\n{}",
        small.line,
        large.line
    );
    // The subject grew 8x, so the unfiltered scan's work did too...
    assert!(
        large.positions_offered > small.positions_offered * 4,
        "positions_offered did not grow with the subject: {} vs {}",
        small.positions_offered,
        large.positions_offered
    );
    // ...while the engine was entered zero times either way, because the
    // needle never occurs. That is the whole asymptotic claim.
    assert_eq!(
        small.position_hits, 0,
        "a needle that does not occur still reached the engine: {}",
        small.line
    );
    assert_eq!(
        large.position_hits, 0,
        "a needle that does not occur still reached the engine: {}",
        large.line
    );
}

#[test]
fn a_failing_ignorecase_literal_scan_uses_the_first_character_set() {
    // `:i` cannot use a literal needle (multi-character folds make a folded
    // needle variable-length), so this is the path that has to be carried by
    // the fold-closure first-character set instead.
    let stats = prefilter_stats(&failing_scan("/ :i 'ZZZQ' /", 400));
    assert!(
        stats.first_char_set >= 1,
        "the first-character-set prefilter did not engage under :i: {}",
        stats.line
    );
    assert_eq!(
        stats.position_hits, 0,
        "no subject character folds to 'z', so no position should have reached the engine: {}",
        stats.line
    );
}

#[test]
fn an_alternation_is_narrowed_by_the_union_of_its_branches() {
    let stats = prefilter_stats(&failing_scan("/ 'zzzq' | 'qqqz' /", 400));
    assert!(
        stats.first_char_set >= 1,
        "an alternation of literals got no first-character set: {}",
        stats.line
    );
    assert_eq!(
        stats.position_hits, 0,
        "neither branch's leading character occurs in the subject: {}",
        stats.line
    );
}

#[test]
fn a_character_class_scan_only_offers_class_members() {
    // The unit has exactly one digit (`5`) per repeat, so a `\d\d` scan is
    // offered one position per repeat and every other position is rejected by
    // the bitmap without entering the engine.
    let repeats = 200;
    let stats = prefilter_stats(&failing_scan(r"/ \d\d /", repeats));
    assert!(
        stats.first_char_set >= 1,
        "a leading character class got no first-character set: {}",
        stats.line
    );
    assert_eq!(
        stats.position_hits, repeats as u64,
        "expected exactly one candidate per repeat: {}",
        stats.line
    );
}

#[test]
fn a_subrule_led_scan_is_narrowed_by_the_rules_own_first_set() {
    // ADR-0099 §4 constraint 3's "keyed by invocant package and
    // `TOKEN_DEFS_GEN`, or decline", taken by its first half: the same
    // alternation written inline is narrowed to two characters, and hiding it
    // behind a rule name must not put the scan back to linear.
    let grammar = r#"grammar G { token kw { 'zzzq' | 'qqqz' } }"#;
    let scan = |repeats: usize| {
        prefilter_stats(&format!(
            "{grammar}\n{}",
            failing_scan("/ <G::kw> /", repeats)
        ))
    };
    let small = scan(100);
    let large = scan(800);

    assert!(
        small.first_char_set >= 1 && large.first_char_set >= 1,
        "a subrule-led pattern got no first-character set:\n{}\n{}",
        small.line,
        large.line
    );
    assert!(
        large.positions_offered > small.positions_offered * 4,
        "positions_offered did not grow with the subject: {} vs {}",
        small.positions_offered,
        large.positions_offered
    );
    // Neither branch's leading character occurs in the subject, so the engine
    // is never entered however long the subject gets.
    assert_eq!(
        small.position_hits, 0,
        "a rule whose every branch starts with an absent character still reached the engine: {}",
        small.line
    );
    assert_eq!(
        large.position_hits, 0,
        "a rule whose every branch starts with an absent character still reached the engine: {}",
        large.line
    );
}

#[test]
fn a_rule_reached_through_another_rule_is_still_narrowed() {
    // The walk follows the call chain, and each body resolves its own
    // unqualified references against the package that DEFINED it.
    let stats = prefilter_stats(&format!(
        "grammar G {{ token outer {{ <inner> }} token inner {{ 'zzzq' }} }}\n{}",
        failing_scan("/ <G::outer> /", 200)
    ));
    assert!(
        stats.first_char_set >= 1,
        "a rule reached through another rule got no first-character set: {}",
        stats.line
    );
    assert_eq!(
        stats.position_hits, 0,
        "the inner rule's leading character does not occur in the subject: {}",
        stats.line
    );
}

#[test]
fn a_left_recursive_rule_declines_rather_than_unrolling() {
    // The first-set must be a SUPERSET of what can match, and a rule reached
    // from itself before anything is consumed can only be walked to a subset
    // -- the one unsound direction. So it is answered "unknown", which walks
    // every position.
    let stats = prefilter_stats(
        r#"grammar G { token thing { <thing> 'a' | 'b' } }
say ("xbz" ~~ / <G::thing> /).Bool;"#,
    );
    assert!(
        stats.first_char_set == 0 && stats.literal_prefix == 0 && stats.inner_literal == 0,
        "a left-recursive rule must not be narrowed: {}",
        stats.line
    );
}

#[test]
fn a_right_recursive_rule_is_still_narrowed_by_what_it_consumes_first() {
    // The complement: recursion only blocks the derivation while it can still
    // be reached without consuming anything. `'a' <thing>` has already fixed
    // the first character by the time the recursive call is seen, so the
    // decline on the call costs nothing and the set is exactly {a, b}.
    let stats = prefilter_stats(&format!(
        "grammar G {{ token thing {{ 'z' <thing> | 'q' }} }}\n{}",
        failing_scan("/ <G::thing> /", 200)
    ));
    assert!(
        stats.first_char_set >= 1,
        "a right-recursive rule got no first-character set: {}",
        stats.line
    );
    assert_eq!(
        stats.position_hits, 0,
        "neither 'z' nor 'q' occurs in the subject: {}",
        stats.line
    );
}

#[test]
fn a_pattern_the_analysis_declines_on_still_walks_every_position() {
    // The control for the tests above -- if `declined` were what every case
    // reported, they would prove nothing. A rule body that opens with a code
    // block is the shape that must still decline: ADR-0009 makes that block
    // run once per start position in both mutsu and rakudo, so no position may
    // be skipped before it.
    let stats = prefilter_stats(
        r#"grammar G { token thing { { 1 } \w } }
say ("abc" ~~ / <G::thing> /).Bool;"#,
    );
    assert!(
        stats.first_char_set == 0 && stats.literal_prefix == 0 && stats.inner_literal == 0,
        "a rule whose body opens with a code block must not be narrowed (ADR-0009): {}",
        stats.line
    );
}

#[test]
fn a_subrule_call_with_arguments_still_walks_every_position() {
    // A parameterized call resolves per call against values the memo key does
    // not carry, so it declines however narrow the body looks.
    let stats = prefilter_stats(
        r#"grammar G { token thing($x) { 'zzzq' } }
say ("abc" ~~ / <G::thing(1)> /).Bool;"#,
    );
    assert!(
        stats.first_char_set == 0 && stats.literal_prefix == 0 && stats.inner_literal == 0,
        "a parameterized subrule call must not be narrowed: {}",
        stats.line
    );
}

#[test]
fn a_failing_inner_literal_scan_is_sublinear_in_subject_length() {
    // `\w+` leads, so there is no literal prefix and the first-character set is
    // as wide as `\w` -- every position in the subject passes it. Only the
    // required INNER literal can answer this scan without entering the engine.
    let small = prefilter_stats(&failing_scan(r"/ \w+ 'zzzq' /", 100));
    let large = prefilter_stats(&failing_scan(r"/ \w+ 'zzzq' /", 800));

    assert!(
        small.inner_literal >= 1 && large.inner_literal >= 1,
        "the inner-literal prefilter did not engage at all:\n{}\n{}",
        small.line,
        large.line
    );
    assert!(
        large.positions_offered > small.positions_offered * 4,
        "positions_offered did not grow with the subject: {} vs {}",
        small.positions_offered,
        large.positions_offered
    );
    assert_eq!(
        small.position_hits, 0,
        "an inner literal that does not occur still reached the engine: {}",
        small.line
    );
    assert_eq!(
        large.position_hits, 0,
        "an inner literal that does not occur still reached the engine: {}",
        large.line
    );
}

#[test]
fn an_inner_literal_carries_a_pattern_with_a_universal_first_set() {
    // `.+` admits every character, so the first-set derivation declines
    // outright and this scan was completely unfiltered before the inner
    // literal existed -- the shape where the win is largest.
    let stats = prefilter_stats(&failing_scan(r"/ .+ 'zzzq' /", 200));
    assert!(
        stats.inner_literal >= 1,
        "a universal-first-set pattern got no inner literal: {}",
        stats.line
    );
    assert_eq!(
        stats.position_hits, 0,
        "the needle does not occur, so no position should have reached the engine: {}",
        stats.line
    );
}

#[test]
fn a_bounded_lead_in_narrows_each_occurrence_to_a_short_window() {
    // A lead-in of bounded width (`'e'?` is a literal atom, so it consumes
    // exactly zero or one character -- unlike a character class, which matches
    // a whole grapheme cluster and so has no upper bound) turns each
    // occurrence of the literal into a two-position window rather than
    // "everything up to here". The unit contains exactly one `=`, preceded by
    // `r`, so the first-character set then rejects one of the two and the
    // engine is entered once per repeat.
    let repeats = 200;
    let stats = prefilter_stats(&failing_scan(r"/ 'e'? '=' \d /", repeats));
    assert!(
        stats.inner_literal >= 1,
        "a bounded lead-in got no inner literal: {}",
        stats.line
    );
    assert_eq!(
        stats.position_hits, repeats as u64,
        "expected exactly one candidate per occurrence: {}",
        stats.line
    );
    assert!(
        stats.positions_offered > 40 * repeats as u64,
        "the unfiltered scan should have had far more to walk: {}",
        stats.line
    );
}

#[test]
fn a_code_block_after_the_literal_declines_so_it_keeps_running() {
    // ADR-0009: skipping a start position on a later literal's account would
    // skip a `{ ... }` block that WOULD have run there, which is exactly why
    // the inner-literal analysis declines on any pattern that runs code --
    // a stronger decline than the first-set analysis needs.
    let stats = prefilter_stats(r#"say ("xyz" ~~ / \w+ { 1 } 'q' /).defined;"#);
    assert_eq!(
        stats.inner_literal, 0,
        "a pattern that runs user code must not be narrowed by an inner literal: {}",
        stats.line
    );
}
