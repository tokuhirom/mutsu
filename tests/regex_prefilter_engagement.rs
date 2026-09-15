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
        first_char_set: counter("first_char_set="),
        positions_offered: counter("positions_offered="),
        position_hits: counter("position_hits="),
        line,
    }
}

struct Stats {
    literal_prefix: u64,
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
fn a_pattern_the_analysis_declines_on_still_walks_every_position() {
    // The complement of the property above: declining is always allowed, and
    // this pins that it is what happens for a subrule call rather than some
    // narrowing nobody checked. Also the control for the tests above -- if
    // `declined` were what every case reported, they would prove nothing.
    let stats = prefilter_stats(
        r#"grammar G { token thing { \w } }
say ("abc" ~~ / <G::thing> /).Bool;"#,
    );
    assert!(
        stats.first_char_set == 0 && stats.literal_prefix == 0,
        "a subrule-led pattern must not be narrowed (ADR-0099 §4 constraint 3): {}",
        stats.line
    );
}
