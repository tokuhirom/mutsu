//! Differential property test for the ADR-0099 Stage 1 scan prefilter
//! (#8272): for every program in the corpus below, the result with the
//! prefilter enabled (the default) must equal the result with it forced off
//! via `MUTSU_REGEX_PREFILTER=off`, byte for byte.
//!
//! Per the ADR's own §7 consequence: this is the first piece of mutsu regex
//! machinery that can be *wrong without being incorrect* -- an over-promising
//! prefix analysis silently skips a valid match, and no ordinary correctness
//! test shape catches that (the unfiltered engine would answer the same way
//! either way, since a `.t` file only ever runs with the prefilter ON). The
//! kill switch exists exactly so this comparison is cheap: run the same
//! program twice, diff stdout.
//!
//! The corpus deliberately includes shapes the prefilter DECLINES on (`:i`,
//! quantified prefixes, alternation, no-literal patterns) alongside the ones
//! it applies to -- both must still agree, since a regression could just as
//! easily be "declines when it shouldn't" (no perf win, still correct) as
//! "applies when it shouldn't" (the dangerous direction).

use std::process::Command;

/// Run a Raku snippet through the built `mutsu`, with the prefilter forced to
/// `on` or `off`. Returns (stdout, success).
fn run(src: &str, prefilter: &str) -> (String, bool) {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(src);
    cmd.env("MUTSU_REGEX_PREFILTER", prefilter);
    let out = cmd.output().expect("failed to spawn mutsu");
    (
        String::from_utf8_lossy(&out.stdout).into_owned(),
        out.status.success(),
    )
}

fn assert_same_with_and_without_prefilter(label: &str, src: &str) {
    let (on_out, on_ok) = run(src, "on");
    let (off_out, off_ok) = run(src, "off");
    assert_eq!(
        on_ok, off_ok,
        "{label}: success differs between prefilter on/off\nprogram: {src}\non: {on_out}\noff: {off_out}"
    );
    assert_eq!(
        on_out, off_out,
        "{label}: output differs between prefilter on/off -- the prefilter is dropping or adding a match\nprogram: {src}"
    );
}

macro_rules! differential_case {
    ($name:ident, $src:expr) => {
        #[test]
        fn $name() {
            assert_same_with_and_without_prefilter(stringify!($name), $src);
        }
    };
}

differential_case!(literal_found_once, r#"say "hello world" ~~ / 'world' /;"#);
differential_case!(literal_not_found, r#"say "hello world" ~~ / 'xyz' /;"#);
differential_case!(literal_at_very_start, r#"say "worldhello" ~~ / 'world' /;"#);
differential_case!(literal_at_very_end, r#"say "helloworld" ~~ / 'world' /;"#);
differential_case!(empty_subject, r#"say "" ~~ / 'x' /;"#);
differential_case!(
    literal_prefix_with_trailing_pattern,
    r#"say "abc123" ~~ / abc \d+ /;"#
);
differential_case!(
    literal_prefix_with_trailing_pattern_no_match,
    r#"say "abcxyz" ~~ / abc \d+ /;"#
);
differential_case!(
    overlapping_occurrences_global,
    r#"say "aaaa" ~~ m:g/ 'aa' /;"#
);
differential_case!(global_comb, r#"say ("banana").comb(/ 'an' /).join(",");"#);
differential_case!(
    global_subst,
    r#"say "one two one two".subst(/ 'one' /, 'ONE', :g);"#
);
differential_case!(
    quantified_prefix_declines_but_still_correct,
    r#"say "bc" ~~ / a? bc /;"#
);
differential_case!(
    alternation_has_no_prefix,
    r#"say "xyz-bar" ~~ / 'foo' | 'bar' /;"#
);
differential_case!(
    ignorecase_declines_but_still_correct,
    r#"say "Hello WORLD" ~~ / :i 'world' /;"#
);
differential_case!(
    ignoremark_declines_but_still_correct,
    r#"say "cafe\x[301]" ~~ / :m 'cafe' /;"#
);
differential_case!(no_literal_at_all, r#"say "abc123" ~~ / \d+ /;"#);
differential_case!(
    unicode_literal_prefix,
    r#"say "héllo wörld" ~~ / 'wörld' /;"#
);
differential_case!(
    named_capture_after_literal_prefix,
    r#"my $m = "key=value" ~~ / 'key=' $<v>=(.+) /; say $m<v>;"#
);
differential_case!(
    grammar_parse_is_anchored_and_unaffected,
    r#"
grammar G { token TOP { 'hello' \s+ 'world' } }
say G.parse('hello world') ?? "matched" !! "no match";
say G.parse('hello there') ?? "matched" !! "no match";
"#
);
differential_case!(
    contains_and_index_use_the_same_engine_path,
    r#"say "the quick brown fox" ~~ / 'brown' /; say "the quick brown fox".contains(/ 'brown' /);"#
);
differential_case!(
    from_pos_scan_min_pos_offset,
    r#"say "aXaXaX".match(/ 'aX' /, :g).join(",");"#
);

// --- first-character-set / minimum-length shapes (the second Stage 1 slice) ---
//
// Everything below is narrowed by the derived first-character set or the
// minimum match length rather than by a literal prefix, so these are the cases
// where an over-promising analysis would silently drop a match.

differential_case!(
    class_led_scan,
    r#"say ("abc123def456" ~~ m:g/ \d+ /).join(",");"#
);
differential_case!(
    negated_class_led_scan,
    r#"say ("aaabaaac" ~~ m:g/ <-[a]> /).join(",");"#
);
differential_case!(
    alternation_first_set_union,
    r#"say ("zzfoozzbarzz" ~~ m:g/ 'foo' | 'bar' /).join(",");"#
);
differential_case!(
    alternation_with_a_nullable_branch,
    r#"say ("xyz" ~~ / 'a' | '' /).Bool;"#
);
differential_case!(
    ignorecase_literal_uses_the_fold_closure,
    r#"say ("say STRASSE now" ~~ m:g/ :i 'strasse' /).join(",");"#
);
differential_case!(
    ignorecase_class,
    r#"say ("aXbYc" ~~ m:g/ :i <[xy]> /).join(",");"#
);
differential_case!(
    ignorecase_kelvin_sign_folds_onto_ascii,
    r#"say ("a\c[KELVIN SIGN]b" ~~ / :i 'k' /).Bool;"#
);
differential_case!(
    ignorecase_long_s_folds_onto_ascii,
    r#"say ("a\c[LATIN SMALL LETTER LONG S]b" ~~ / :i 's' /).Bool;"#
);
differential_case!(
    minimum_length_prunes_the_tail,
    r#"say ("abcde" ~~ m:g/ . . . /).join(",");"#
);
differential_case!(
    minimum_length_with_a_repeat_quantifier,
    r#"say ("aaaa" ~~ / a ** 3..5 /).Str;"#
);
differential_case!(
    leading_zero_width_assertion,
    r#"say ("foo\nbar" ~~ m:g/ ^^ \w /).join(",");"#
);
differential_case!(
    leading_word_boundary,
    r#"say ("one two three" ~~ m:g/ << \w /).join(",");"#
);
differential_case!(
    leading_lookahead,
    r#"say ("a1b2" ~~ m:g/ <?before \d> . /).join(",");"#
);
differential_case!(
    leading_code_block_must_still_run_per_position,
    r#"my $n = 0; my $m = "xxxz" ~~ / { $n++ } 'z' /; say $m.Str; say $n > 1;"#
);
differential_case!(
    newline_atom_and_crlf,
    r#"say ("a\r\nb" ~~ m:g/ \n /).elems;"#
);
differential_case!(
    class_containing_newline_matches_at_cr,
    r#"say ("a\r\nb" ~~ / <[\n]> /).Bool;"#
);
differential_case!(
    grapheme_class_entry,
    r#"say ("xe\x[301]y" ~~ / <[e\x[301]]> /).Bool;"#
);
differential_case!(
    combining_mark_does_not_start_a_match,
    r#"say ("a\x[094D]b" ~~ / \w /).Str;"#
);
differential_case!(
    subrule_led_pattern_declines,
    r#"
grammar G { token thing { \d+ } }
say ("ab123" ~~ / <G::thing> /).Str;
"#
);
differential_case!(
    backreference_after_a_class,
    r#"say ("xabab" ~~ / (\w) (\w) $0 $1 /).Str;"#
);
differential_case!(
    scoped_ignoremark_inside_a_plain_pattern,
    r#"say ("xcafe\x[301]" ~~ / [:m 'cafe'] /).Bool;"#
);
differential_case!(
    split_on_a_class,
    r#"say "a1b22c".split(/ \d+ /).join("|");"#
);
differential_case!(
    subst_with_a_class_pattern,
    r#"say "a1b2".subst(/ \d /, "X", :g);"#
);
differential_case!(
    non_ascii_subject_with_an_ascii_class,
    r#"say ("日本語abc" ~~ / <[a..z]>+ /).Str;"#
);
differential_case!(
    non_ascii_class_member,
    r#"say ("abc日本" ~~ / <[日本]> /).Str;"#
);
differential_case!(
    unicode_property_atom_declines_to_anything,
    r#"say ("abcÀ" ~~ / <:Lu> /).Str;"#
);
