//! ADR-0135 D6 for Slice A (#10251): the compiled regex engine against the
//! tree walk.
//!
//! Every corpus program is run three ways: with the compiled engine off
//! (`MUTSU_RX_VM=off`, the walk alone), with it on, and with it on under
//! `MUTSU_RX_DIFF=1`, which re-runs every compiled match through the walk and
//! aborts on any disagreement in the match, its end or a capture span. The
//! first two must print the same thing and the third must not abort.
//!
//! A last test pins engagement: the ADR-0135 §2.3 shapes, Slice A's kill
//! criterion, must actually take the compiled engine.

use std::process::Command;

fn run(src: &str, env: &[(&str, &str)]) -> (bool, String, String) {
    let mut cmd = Command::new(env!("CARGO_BIN_EXE_mutsu"));
    cmd.arg("-e").arg(src);
    for (k, v) in env {
        cmd.env(k, v);
    }
    let out = cmd.output().expect("failed to spawn mutsu");
    (
        out.status.success(),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

fn assert_same(src: &str) {
    let walk = run(src, &[("MUTSU_RX_VM", "off")]);
    let vm = run(src, &[]);
    assert_eq!(
        (walk.0, &walk.1),
        (vm.0, &vm.1),
        "compiled engine changed the output of:\n{src}\nwalk stderr: {}\nvm stderr: {}",
        walk.2,
        vm.2
    );
    let diff = run(src, &[("MUTSU_RX_DIFF", "1")]);
    assert!(
        !diff.2.contains("MUTSU_RX_DIFF"),
        "compiled engine and walk disagree on:\n{src}\n{}",
        diff.2
    );
    assert_eq!(
        diff.1, vm.1,
        "MUTSU_RX_DIFF run printed differently for:\n{src}"
    );
}

macro_rules! differential_case {
    ($name:ident, $src:expr) => {
        #[test]
        fn $name() {
            assert_same($src);
        }
    };
}

differential_case!(
    greedy_give_back,
    r#"for <aaab abab ab b ""> -> $s { say ($s ~~ / a+ b /).gist; say ($s ~~ / ^ a* b $ /).gist }"#
);
differential_case!(
    frugal_quantifiers,
    r#"say ("<a><b>" ~~ / '<' .+? '>' /).gist; say ("xaaay" ~~ / a+? /).gist; say ("abc" ~~ / a .*? c /).gist; say ("ab" ~~ / a b?? /).gist"#
);
differential_case!(
    ratchet_quantifiers,
    r#"say so "aaa" ~~ / :r a+ a /; say so "aab" ~~ / :r a+ b /; say ("ab" ~~ / :r a? b /).gist; say so "a" ~~ / :r a? a /"#
);
differential_case!(
    counted_quantifiers,
    r#"for ^8 -> $n { my $s = "x" x $n; say ($s ~~ / ^ x ** 2..4 /).gist, " ", ($s ~~ / x ** 3 /).gist, " ", so $s ~~ / ^ x ** 2..4 $ / }"#
);
differential_case!(
    groups_and_nested_quantifiers,
    r#"say ("ab ab ab c" ~~ / [ a b \s ]+ c /).gist; say ("xyxyx" ~~ / [xy]* x $ /).gist; say ("the quick brown 12 fox" ~~ / [ \w+ \s ] ** 2 \d+ /).gist"#
);
differential_case!(
    positional_captures,
    r#"my $m = "key=42 other=7" ~~ / (\w+) '=' (\d+) /; say $m[0].Str, " ", $m[1].Str, " ", $m.from, " ", $m.to; say ("abc" ~~ / (b) /)[0].from; say ("aXb" ~~ / a (<[A..Z]>) b /).list.map(*.Str)"#
);
differential_case!(
    named_aliases,
    r#"my $m = "k1=v-2;" ~~ / $<key> = [ \w+ ] '=' $<val> = [ <[\w\-]>+ ] /; say $m<key>.Str, " ", $m<val>.Str, " ", $m<key>.from; say ("ab" ~~ / $<x>=(a) (b) /).gist; say ("ab" ~~ / $1=(a) b /).gist; say ("zz9" ~~ / <alpha>+ /).gist"#
);
differential_case!(
    global_matching,
    r##"say "a1 b22 c333".match(/ (\w) (\d+) /, :g).map({ .[0] ~ ":" ~ .[1] }).join(","); say "one two  three".comb(/ \w+ /).join("|"); say "a-b-c".split(/ '-' /).join("+"); say "x1y22z".subst(/ \d+ /, "#", :g)"##
);
differential_case!(
    anchors_and_boundaries,
    r#"my $t = "ab cd\nef gh\n"; say $t.match(/ ^^ \w+ /, :g).join(","); say $t.match(/ \w+ $$ /, :g).join(","); say $t.match(/ << \w /, :g).join(","); say $t.match(/ \w >> /, :g).join(","); say ("abc" ~~ / b $ /).gist; say ("ab" ~~ / <?wb> b /).gist; say ("ab" ~~ / <!ww> b /).gist"#
);
differential_case!(
    graphemes_and_newlines,
    r#"my $s = "e\x[301]x\r\ny"; say ($s ~~ / . x /).gist; say ($s ~~ / x \n y /).gist; say ($s ~~ / \N+ /).chars; say ("a\x[5B4]b" ~~ / a /).gist; say ($s ~~ / ^ . ** 3 /).chars"#
);
differential_case!(
    unicode_properties,
    r#"say ("abcDEFghi" ~~ / <:Lu>+ /).gist; say ("a1b2" ~~ / <:N> /).gist; say ("αβγ" ~~ / <:Greek>+ /).gist"#
);
differential_case!(
    declined_shapes_still_agree,
    r#"say ("ab" ~~ / a | ab /).gist; say ("ab" ~~ / a || ab /).gist; say ("aa" ~~ / (a) $0 /).gist; say ("a,b,c" ~~ / \w+ % ',' /).gist; say ("AB" ~~ / :i ab /).gist; say ("ab" ~~ / a <?before b> /).gist; say ("abab" ~~ / (ab)+ /)[0].elems"#
);

differential_case!(
    quantified_captures,
    r#"my $m = "abcab" ~~ / (\w)+ /; say $m[0].elems, " ", $m[0].map(*.Str).join(","); say ("a1b2c3" ~~ / [ (\w) (\d) ]+ /).list.map({ .elems }).join(","); say ("xyz" ~~ / (x)* y /)[0].elems; say ("abc" ~~ / (b) ** 1..2 /)[0].elems; say ("aaa" ~~ / :r (a)+ /)[0].elems; say ("ab" ~~ / (c)* ab /)[0].elems; say ("aab" ~~ / (a)+? b /)[0].elems"#
);
differential_case!(
    optional_captures,
    r#"say ("b" ~~ / (a)? b /).gist; say ("ab" ~~ / (a)? b /).gist; say ("b" ~~ / (a)? (b) /)[1].from; say ("b" ~~ / $<x>=[a]? b /).gist; say ("b" ~~ / $<x>=(a)? b /).gist; say ("ab" ~~ / $<x>=(a)?? ab /).gist; say ("b" ~~ / [ (a) (c) ]? b /).gist; say ("ab" ~~ / :r (a)? b /).gist"#
);
differential_case!(
    sequential_alternation,
    r#"say ("abcd" ~~ / a [ bc || b ] cd /).gist; say ("b1" ~~ / [ (a) || (b) ] (\d) /).gist; say ("b1" ~~ / [ (a) (x) || (b) ] (\d) /)[2].Str; say ("aab" ~~ / [ a || aa ]+ b /).gist; say ("aab" ~~ / :r [ a || aa ] b /).gist; say ("aab" ~~ / :r [ aa || a ] b /).gist; say ("q" ~~ / [ a || b ]? q /).gist; say ("ab" ~~ / $<x>=a [ $<x>=b || c ] /)<x>.elems; say ("cc" ~~ / [ <[ab]> || c ] ** 2 /).gist; say ("abc" ~~ / :r [ \w+ || \d ] 'c' /).gist; say ("x" ~~ / [ a || (b)+ || x ] /)[0].raku; say ("ab" ~~ / $<y>=[ a || b ] b /)<y>.Str; say ("adx" ~~ / [ a || b ]+ [ (c) || d ] (x) /).list.elems; say ("adx" ~~ / [ a || b ]+? [ (c) || d ] (x) /)[1].Str; say ("aaa" ~~ / ^ [ a || aa ] ** 2 $ /).gist; say ("abab" ~~ / ^ [ a || ab ] **? 2..3 b $ /).gist"#
);
differential_case!(
    nullable_loop_bodies,
    r#"say ("aab" ~~ / [ a? ]* b /).gist; say ("aab" ~~ / ( a? )+ b /)[0].elems; say ("xb" ~~ / [ a* ]+ b /).gist; say ("ab" ~~ / :r [ a? ]* b /).gist; say ("ab" ~~ / [ a? ]*? b /).gist; say ("abab" ~~ / ^ [ a b? ]* $ /).gist; say ("aaa" ~~ / ^ [ a? a? ] ** 2 $ /).gist; say ("aaa" ~~ / ^ ( a? a? ) ** 2..3 $ /)[0].elems; say ("abc" ~~ / [ \w* ]* c /).gist; say so "" ~~ / ^ [ x? ]+ $ /; say ("ab" ~~ / [ a || b? ]+ $ /).gist; say ("a b" ~~ / ^^ ** 2 a /).gist; say "a b c".comb(/ [ \s* \w ]+? /).join("|")"#
);
differential_case!(
    composite_classes,
    r#"say ("abc1x" ~~ / <+alpha -[b]>+ /).gist; say ("Hello World" ~~ / <+upper +digit>+ /).gist; say ("ab12cd" ~~ / <[a..z] - [c]>+ /).gist; say so "x\r\ny" ~~ / x <+[\n]> y /; say ("é1" ~~ / <+alpha>+ /).gist; say ("a^b" ~~ / <+graph -punct>+ /).gist; say "abc".comb(/ <+alnum -[b]> /).join("|"); grammar G { token alpha { 'Z' }; token TOP { <+alpha -[q]>+ } }; say G.parse("aZb").gist; grammar H { token vowel { <[aeiou]> }; token TOP { <-vowel>+ } }; say H.parse("xyz").gist; say H.parse("xay").gist"#
);

/// The ADR-0135 §2.3 shapes must take the compiled engine: this is Slice A's
/// kill criterion, and a silently declined pattern would pass every
/// differential case above while measuring nothing.
#[test]
fn kill_criterion_shapes_are_compiled() {
    let src = r#"my $big = "the quick brown fox 12345 jumps over 67-8 lazy\n" x 20;
say so $big ~~ / \w+ \s \d ** 6 /;
say so $big ~~ / [ \w+ \s ] ** 3 \d ** 6 /;
say +$big.match(/ (\w+) \s (\d+) /, :g);"#;
    let (ok, out, err) = run(src, &[("MUTSU_VM_STATS", "1")]);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "False\nFalse\n40\n");
    let line = err
        .lines()
        .find_map(|l| l.split("regex-vm: ").nth(1))
        .unwrap_or_else(|| panic!("no regex-vm stats line: {err}"));
    let compiled: u64 = line
        .split_whitespace()
        .find_map(|w| w.strip_prefix("compiled="))
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("no compiled= in: {line}"));
    assert!(compiled >= 3, "the §2.3 shapes were not compiled: {line}");
}
