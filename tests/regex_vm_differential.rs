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
    separated_and_conjunction_code_views,
    r#"my @l; "x1,2,3" ~~ / (x) [ (\d) { @l.push: $/.list.map(~*).join('|') } ] +% [ (',') { @l.push: ~$/ } ] /; "1;2" ~~ / :r [ (\d) { @l.push: +$/[1] } ] +%% [ (';') ] /; "ab" ~~ / (a) [ (\w) { @l.push: +$/.list } & \w { @l.push: ~$0 } ] /; "aa" ~~ / :my $x = 'a'; [ \w & $x ] $x /; @l.push: ~$/; .say for @l"#
);
differential_case!(
    greedy_give_back,
    r#"for <aaab abab ab b ""> -> $s { say ($s ~~ / a+ b /).gist; say ($s ~~ / ^ a* b $ /).gist }"#
);
differential_case!(
    frugal_quantifiers,
    r#"say ("<a><b>" ~~ / '<' .+? '>' /).gist; say ("xaaay" ~~ / a+? /).gist; say ("abc" ~~ / a .*? c /).gist; say ("ab" ~~ / a b?? /).gist"#
);
differential_case!(
    frugal_separated_quantifiers,
    r#"say ("a,a,a" ~~ / a+? % ',' /).gist; say ("a,a,a" ~~ / a*? % ',' /).gist; say ("a,a,a" ~~ / a**?2..3 % ',' /).gist; say ("a,a,a" ~~ / ^ a+? % ',' ',a' /).gist; say ("a,a,a" ~~ / ^ a**?2..3 % ',' $ /).gist"#
);
differential_case!(
    sigspace_separated_quantifiers,
    r#"say ("a, a, a" ~~ / :s a*? % "," /).gist; say ("a, a, a" ~~ / :s a**?2..3 % "," /).gist; say ("a , a , a" ~~ / :s a+ % "," /).gist; say ("a , a , a" ~~ / :s a +% "," /).gist; say ("a, a, a," ~~ / :s a+? %% "," $ /).gist; say ("1, 2" ~~ / :s <digit>+ % "," /).gist; say ("1,2" ~~ / <digit>+ % "," /).gist; say ("1, 2" ~~ / :s $<x>=\d ** 2 % "," /).gist"#
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
differential_case!(
    separated_quantifiers,
    "say \"a,b,c\" ~~ / \\w+ % ',' /; say \"a,b,c,\" ~~ / \\w+ %% ',' /; say \"a,b,c,x\" ~~ / \\w+ % ',' ',x' /; say \"a,b\" ~~ / ^ \\w* % ',' $ /; say \"\" ~~ / ^ \\w* % ',' $ /; say \";b\" ~~ / ^ <-[;]>* % ';' $ /; say \"a,b,c\" ~~ / \\w ** 2 % ',' /; say \"a,b,c\" ~~ / ^ \\w ** 1..2 % ',' /; say \"a, b ,c\" ~~ / \\w+ % [ \\s* ',' \\s* ] /; say \"a,b,c\" ~~ / :r \\w+ % ',' /; say \"a,b,c,\" ~~ / :r \\w+ %% ',' /; say \"a,b,c,x\" ~~ / :r \\w+ % ',' ',x' /; say \"a,b,c\" ~~ / :r \\w+ % ',' ',' /; say \"ab,cd\" ~~ / [ a || ab ]+ % ',' cd /; say \"aa,aa\" ~~ / ^ [ a+ ]+ % ',' $ /; say \"1-2--3\" ~~ / \\d+ % '-'+ /; say \"x\" ~~ / :r \\d* % ',' x /; say \"a,,b\" ~~ / ^ \\w* % ',' $ /;"
);

differential_case!(
    backreferences,
    r#"say ("xyzzy" ~~ / (.) $0 /).gist; say ("abcabc" ~~ / (\w+) $0 /).gist; say ("abba" ~~ / (.)(.) $1 $0 /).gist; say ("aba" ~~ / (a) [ (b) $0 ] /).gist; say ("abb" ~~ / (a) [ (b) $0 ] /).gist; say ("aXa" ~~ / $<q>=. X $<q> /).gist; say ("aa" ~~ / $<x>=(\w) ( $<x> ) /).gist; say ("abab" ~~ / (a)(b) $0 $1 /).gist; say ("aa bb" ~~ / (\w) $0 ' ' (\w) $1 /).gist; say ("abcab" ~~ / (a)(b) .* $0 $1 /).gist; say ("aab" ~~ / (a)+ $0 /).gist"#
);
differential_case!(
    capture_markers,
    r#"say ("foobar" ~~ / foo <( bar )> /).gist; say ("foobar" ~~ / foo <( bar /).gist; say ("foobar" ~~ / foo )> bar /).gist; say "a1 b2".match(/ \w <( \d /, :g).join(","); say ("xab" ~~ / x [ a <( b ] /).gist; say "a-b".subst(/ a <( '-' )> b /, "+"); say ~("xab" ~~ / x [ c || a <( b ] /); say ~("xab" ~~ / [ c || x a )> ] b /)"#
);
differential_case!(
    nested_captures,
    r#"say ("ab ab" ~~ / ((a)(b)) ' ' $0 /).gist; say ("abab" ~~ / ((a)(b))+ /).gist; say ("aaa" ~~ / ( (a)* ) /).gist; say ("a1b2" ~~ / ( (\w) (\d) )+ /)[0][1][0].Str; say ("xay" ~~ / x ( $<in>=a ) y /)[0]<in>.Str; say ("ab" ~~ / $<o>=( (a) b ) /)<o>[0].Str; say ("aab" ~~ / ( (a)+? ) b /).gist; say ("ab" ~~ / :r ( (a) ) b /).gist; say ("abc" ~~ / ( [ (a) || (b) ] ) /).gist; say ("b" ~~ / ( (a) )? b /).gist"#
);
differential_case!(
    quantified_aliases,
    r#"say ("123" ~~ / $<x>=(\d)+ /).gist; say ("a1b2" ~~ / [ $<x>=\w $<y>=(\d) ]+ /).gist; say ("aaab" ~~ / $<x>=[a]+ b /).gist; say ("ab" ~~ / $<x>=(a)* b /)<x>.elems; say ("b" ~~ / $<x>=(a)* b /)<x>.elems; say ("aab" ~~ / $<x>=(a)+? b /)<x>.elems; say ("aa" ~~ / :r $<x>=(a) ** 2 /).gist; say ("abc" ~~ / $<x>=[ (\w) ]+ /).gist"#
);
differential_case!(
    separated_captures,
    r#"say "a,b,c" ~~ / (\w)+ % ',' /; say "a,b,c," ~~ / (\w)+ %% (',') /; say "a1-b2-c3" ~~ / [ (\w)(\d) ]+ % '-' /; say "x=1;y=2" ~~ / [ $<k>=\w '=' $<v>=\d ]+ % ';' /; say "a, b ,c" ~~ / (\w) ** 2..3 % [ \s* (',') \s* ] /; say "ab" ~~ / :r (\w)* % ',' b /; say "a,b" ~~ / :r (\w)+ %% ',' /; say "" ~~ / (\w)* % ',' /; say "a;;b" ~~ / (\w*)+ % ';' /; say "1,2,3" ~~ / ^ (\d)+ % (',') $ /; say "aXbXc" ~~ / ((\w))+ % X /; say "a,b,c,x" ~~ / (\w)+ % ',' ',x' /"#
);

differential_case!(
    alias_forms_and_position_assertions,
    r#"say ("ab12" ~~ / $<a>=<alpha>+ /)<a>.Str; say ("abc" ~~ / @<x>=(.(.)) /)<x>.elems; say ("abc" ~~ / @<x>=[ \w ] /)<x>.elems; say ("aab" ~~ / a <?same> a /).gist; say ("abc" ~~ / <at(1)> b /).Str; say ("abc" ~~ / \w <!same> \w /).Str"#
);

differential_case!(
    ltm_alternation,
    r#"say ("ab" ~~ / a | ab /).gist; say ("abc" ~~ / [ a | ab ] c /).gist; say ("aaab" ~~ / [ a+ | q ] ab /).gist; say ("foobar" ~~ / foo | foobar | fo /).gist; say ("ab" ~~ / (a) | (a)(b) /).gist; say ("xy" ~~ / $<k>=x | $<k>=xy /).gist; say ("abab" ~~ / [ a | ab ]+ $ /).gist; say ("ab" ~~ / :r [ a | ab ] b /).gist; say ("ab" ~~ / :r [ ab | a ] b /).gist; say "a bb ccc".comb(/ \w ** 2 | \w /).join("|"); say ("ab" ~~ / [ (a) | b ]+ /).gist; say ("abc" ~~ / a [ b | bc ]? /).gist; say ("xyz" ~~ / [ x | xy ] [ yz | z ] /).gist; say ("aab" ~~ / (a) [ $0 b | a ] /).gist; say ("abcd" ~~ / [ [ ab | a ] | abc ] [ cd | d ] /).gist; say ("x" ~~ / ^ [ [ a | b | x ] | y ] $ /).gist"#
);

differential_case!(
    lookaround,
    r#"say ("ab" ~~ / a <?before b> /).gist; say ("ac" ~~ / a <!before b> /).gist; say ("ab" ~~ / <?after a> b /).gist; say ("xb" ~~ / <!after a> b /).gist; say "foo1 bar2 baz".comb(/ \w+ <?before \d> /).join("|"); say "a1b2c3".comb(/ <?after \d> \w /).join("|"); say ("aab" ~~ / a+ <?before b> /).gist; say ("abab" ~~ / <?after ab> ab /).gist; say ("ab" ~~ / a <?before [ b | c ]> /).gist; say ("x" ~~ / <?before x> <!before y> x /).gist; say ("ab" ~~ / :r a <?before b>? b /).gist; say ("aa" ~~ / $<x>=(\w) <?before $<x>> . /).gist"#
);

differential_case!(
    ignorecase,
    r#"say ("ABC" ~~ / :i abc /).gist; say ("xABCy" ~~ / :i [a|b]+ c /).gist; say ("FOO bar" ~~ / :i foo \s BAR /).gist; say ("aBC" ~~ / a [:i bc] /).gist; say ("ABC" ~~ / a [:i bc] /).gist; say ("Straße" ~~ / :i strasse /).gist; say "Hello HELLO hello".comb(/ :i hello /).elems; say ("ÉCOLE" ~~ / :i école /).gist; say ("aA" ~~ / :i (a) $0 /).gist; say ("xY" ~~ / :i <[a..z]>+ /).gist; say ("ﬁ" ~~ / :i fi /).gist; say "A-b-C".comb(/ :i <[abc]> /).join; say ("xAy" ~~ / :i x <?before a> . y /).gist"#
);
differential_case!(
    ignoremark,
    r#"say ("café" ~~ / :m cafe /).gist; say ("cafe" ~~ / :m café /).gist; say ("ÀB" ~~ / :m :i ab /).gist; say ("naïve x" ~~ / :m (naive) \s (x) /).gist; say "résumé resume".match(/ :m resume /, :g).elems; say ("e\x[301]x" ~~ / :m ex /).gist; say ("xé" ~~ / x [:m e] /).gist; say "ÀÉÎ".subst(/ :m e /, "E")"#
);

differential_case!(
    conjunction,
    r#"say ("abc" ~~ / \w+ & ab /).gist; say ("abc" ~~ / <[a..c]>+ & .* c /).gist; say ("ab12" ~~ / (\w+) & (\w\w) /).gist; say ("foobar" ~~ / [ \w+ & foo ] bar /).gist; say ("aaa" ~~ / a+ & a ** 2 /).gist; say "ab cd".match(/ \w+ & <[a..c]>+ /, :g).join("|"); say ("abc" ~~ / $<x>=\w+ & $<y>=[ab] c /)<x y>.join(","); say ("aXb" ~~ / a [ . & <:Lu> ] b /).gist; say ("abc" ~~ / a && ab /).gist; say ("aa" ~~ / $<x>=(\w) [ $<x> & . ] /).gist; say ("ab" ~~ / ( <alpha> & . )+ /).gist; say ("abab" ~~ / [ \w+ & ab ]+ /).gist"#
);

differential_case!(
    code_blocks,
    r#"my @log; say so "abc" ~~ / a { @log.push("blk@" ~ $/.Str) } b c /; say @log.join(","); @log = (); say so "aab" ~~ / a+ { @log.push("n" ~ $/.chars) } b /; say @log.join(","); @log = (); say so "aax" ~~ / a+ { @log.push("n" ~ $/.chars) } b /; say @log.join(","); @log = (); say so "aaa" ~~ / a* { @log.push("n" ~ $/.chars) } a /; say @log.join(","); @log = (); say ("ab" ~~ / (a) { @log.push("c0=" ~ $0) } (b) /).gist; say @log.join(","); @log = (); say so "abc" ~~ / a [ b { @log.push($/.Str) } || c ] c /; say @log.join(",")"#
);
differential_case!(
    code_assertions,
    r#"my @log; say so "abc" ~~ / a <?{ @log.push("as1"); True }> b <?{ @log.push("as2"); False }> c /; say @log.join(","); @log = (); say so "abc" ~~ / a <!{ @log.push("neg"); False }> b c /; say @log.join(","); my $n = 0; say so "aaaa" ~~ / [ <?{ $n++; True }> . ]+ /; say $n; @log = (); say ("abd" ~~ / a [ b <?{ @log.push($/.Str); True }> | c ] d /).gist; say @log.join(","); say ("12" ~~ / (\d) <?{ +$0 == 1 }> \d /).gist; say ("22" ~~ / (\d) <?{ +$0 == 1 }> \d /).gist"#
);
differential_case!(
    code_scopes,
    r#""abc" ~~ / a [ b { say "grp: ", $/.Str } ] c /; "abc" ~~ / a ( b { say "cap: ", $/.Str } ) c /; "abc" ~~ / (a) [ b { say "grp0: ", $0.Str } ] c /; "abc" ~~ / (a) [ (b) { say "grp1: ", $0.Str, $1.Str } ] c /; "abc" ~~ / (a) ( (b) { say "cap1: ", $0.Str } ) c /; "aaab" ~~ / [ a { say "it: ", $/.Str } ]+ b /"#
);
differential_case!(
    code_my_declarations,
    r#"my @log; say so "abc" ~~ / :my $x = 3; a { @log.push("x=$x") } b <?{ $x == 3 }> c /; say @log.join(","); say ("abab" ~~ / :my $i = 0; [ ab { $i++ } ]+ <?{ $i == 2 }> /).gist; say ("ab" ~~ / :my @l = 1, 2; a <?{ @l.elems == 2 }> b /).gist; say ("ab" ~~ / :my $y = 'b'; a $y /).gist"#
);
differential_case!(
    code_block_dies,
    r#"my $r = do { "abc" ~~ / a { die "boom" } b /; "no error" }; CATCH { default { say "caught: ", .message } }; say $r"#
);
differential_case!(
    code_in_lookaround_and_conjunction,
    r#"my @log; say ("ab" ~~ / a <?before b { @log.push("la") }> b /).gist; say @log.join(","); @log = (); say ("ab" ~~ / <?after a { @log.push("lb") }> b /).gist; say @log.join(","); @log = (); say ("abc" ~~ / \w+ { @log.push("c1") } & ab /).gist; say @log.join(",")"#
);

differential_case!(
    isolated_groups,
    r#"my $re = /\d+/; say ("a12b" ~~ / a <$re> b /).gist; say ("a12b" ~~ / a $re b /).gist; my $r2 = /(\d)(\d)/; say ("a12b" ~~ / a <$r2> b /).gist; say ("a12b" ~~ / (a) <$r2> (b) /).gist; my @log; my $r3 = /x { @log.push("r3@" ~ $/.Str) }/; say so "ax" ~~ / a <$r3> /; say @log.join(","); say ("aaa1" ~~ / [ <$re> | a ]+ /).gist; say ("12ab" ~~ / <$re>+ ab /).gist; say ("1234" ~~ / <$re> <$re> /).gist; say ("1234" ~~ / :r <$re> \d /).gist"#
);
differential_case!(
    closure_and_variable_interpolation,
    r#"say ("aab" ~~ / <{ 'a+' }> b /).gist; my $n = 2; say ("aaab" ~~ / <{ 'a' x $n }> a? b /).gist; say ("ab" ~~ / :my $x = 'a'; $x b /).gist; say ("aab" ~~ / :my $x = 'a'; $x+ b /).gist; say ("abab" ~~ / :my $x = 'ab'; $x ** 2 /).gist; my @log; say so "abc" ~~ / a <{ @log.push("closure@" ~ $/.Str); 'b' }> c /; say @log.join(",")"#
);
differential_case!(
    code_that_matches_a_regex_with_code,
    r#"my @log; say so "ab" ~~ / a <?{ "x" ~~ / x { @log.push("inner") } /; @log.push("outer"); True }> b /; say @log.join(","); @log = (); say so "ab" ~~ / a { so "yy" ~~ / y <?{ @log.push("assert"); True }> y / } b { @log.push("tail") } /; say @log.join(",")"#
);

differential_case!(
    lexicals_in_nested_levels,
    r#"my @log; say so "xab" ~~ / x :my $v = 'a'; ( $v { @log.push("in:" ~ $v) } b ) /; say @log.join(","); say ("ab" ~~ / :my $v = 'a'; ( $v ) b /).gist; say ("aab" ~~ / :my $v = 'a'; [ $v ]+ b /).gist; say ("a,a" ~~ / :my $v = 'a'; ( $v )+ % ',' /).gist; say ("ab" ~~ / :my $v = 'a'; [ <?{ $v eq 'a' }> a ] b /).gist"#
);

differential_case!(
    code_interpolation_candidates,
    r#"my @alts = <ab a>; say ("abc" ~~ / @(@alts) c /).gist; say ("abc" ~~ / $( 'ab' ) c /).gist; say ("aab" ~~ / @( <a aa> ) b /).gist; say ("xab" ~~ / x [ @(<a ab>) ]+ /).gist; say ("xaab" ~~ / x @(<a aa>) b /).gist; say ("abab" ~~ / :r @(<a ab>) <[ab]>+ /).gist; my @log; say so "ab" ~~ / a $( @log.push("interp"); 'b' ) /; say @log.join(","); @log = (); say ("aab" ~~ / @( @log.push("pick"); <aa a> ) b /).gist; say @log.join(",")"#
);
differential_case!(
    code_repeat_counts,
    r#"my $n = 3; say ("aaaa" ~~ / a ** {$n} /).gist; say ("aaaa" ~~ / ^ a ** {$n} $ /).gist; say ("aaaa" ~~ / a ** {2..3} a /).gist; say ("abab" ~~ / [ab] ** {2} /).gist; say ("aaa" ~~ / :r a ** {1..*} a /).gist; say ("aaaa" ~~ / a **? {1..*} /).gist; say ("abcabc" ~~ / [ <alpha> ** {2} ]+ /).gist; my @log; say so "aaab" ~~ / a ** { @log.push("again"); 1..3 } b /; say @log.join(","); @log = (); say so "xaab" ~~ / x [ a ** { @log.push("it"); 1 } ]+ b /; say @log.join(",")"#
);

// Slice D (#10254): `<subrule>` calls. A plain rule or a proto runs as a frame in
// the caller's loop; the Match tree it files must be the walk's.
differential_case!(
    subrule_calls_plain_and_ratchet,
    r#"grammar G { token TOP { <a> <b> } token a { \d+ } token b { <c> | 'x' } token c { <[a..c]>+ } }; my $m = G.parse("12abc"); say $m.gist; say $m<a>.Str, $m<b>.Str, $m<b><c>.Str; say G.parse("12x").gist; say G.parse("12d").so"#
);
differential_case!(
    subrule_calls_resume_into_a_non_ratchet_callee,
    r#"grammar H { regex TOP { <w> <w> } regex w { \w+ } }; say H.parse("abcd")<w>.map(*.Str).join(","); say ("abcd" ~~ / <H::w> 'd' /).gist; say ("abcd" ~~ / <H::w> <H::w> /).gist; my @log; grammar K { regex TOP { <r> 'c' } regex r { \w* { @log.push($/.Str) } } }; say K.parse("abc").so; say @log.join(",")"#
);
differential_case!(
    subrule_calls_dedup_a_callees_ends,
    r#"my @log; grammar D { regex TOP { <d> 'b'? { @log.push("t") } } regex d { a || a || ab } }; say D.parse("ab").gist; say D.parse("a").gist; say @log.join(",")"#
);
differential_case!(
    proto_dispatch_through_frames,
    r#"grammar P { token TOP { <value>+ % ',' } proto token value {*} token value:sym<num> { \d+ } token value:sym<word> { <[a..z]>+ } token value:sym<list> { '[' ~ ']' <value>* % ',' } token value:sym<t> { 'true' } }; class A { method TOP($/) { make $<value>.map(*.made).join('|') } method value:sym<num>($/) { make "N$/" } method value:sym<word>($/) { make "W$/" } method value:sym<list>($/) { make "L(" ~ $<value>.map(*.made).join(",") ~ ")" } method value:sym<t>($/) { make "T" } }; my $m = P.parse("12,ab,[1,2,[x]],true", :actions(A.new)); say $m.so; say $m.made; say $m<value>.elems; say P.parse("12,,3").so; say P.subparse("12,ab,").Str; say P.parse("trueish,1").so"#
);
differential_case!(
    quantified_subrule_calls,
    r#"grammar R { token TOP { <a>+ <b>* <c>? <d> } token a { 'a' } token b { 'b' } token c { 'c' } token d { 'd' } }; my $r = R.parse("aaabbcd"); say $r.so; say $r<a>.elems, " ", $r<b>.elems, " ", ($r<c>.defined ?? "c" !! "-"); say R.parse("aad")<c>.defined; say R.parse("ad")<b>.elems; grammar S { regex TOP { <w> <w> <w> } regex w { \w ** 1..3 } }; say S.parse("abcdefgh")<w>.map(*.Str).join(","); say S.parse("abcde")<w>.map(*.Str).join(",")"#
);
differential_case!(
    goal_matches,
    r#"grammar Q { token TOP { <list> } rule list { '[' ~ ']' <item>* % ',' } token item { <num> | <word> | <list> } token num { \d+ } token word { <[a..z]>+ } }; my $m = Q.parse("[1, ab, [2,3], c]"); say $m.so; say $m<list><item>.elems; say $m<list><item>[2]<list><item>.map(*.Str).join("+"); say Q.parse("[1, ab").so; say ("x(a)y" ~~ / '(' ~ ')' (\w) /).gist; say ("x(a" ~~ / '(' ~ ')' (\w) /).gist; say ("((a))" ~~ / '(' ~ ')' [ <-[()]>+ | <?before '('> $<in>=[ '(' ~ ')' <-[()]>+ ] ] /).gist"#
);
differential_case!(
    subrule_recursion_and_left_recursion,
    r#"grammar N { token TOP { <list> } token list { '[' <list>* ']' } }; say N.parse("[" x 200 ~ "]" x 200).so; say N.parse("[[]" ~ "]").so; grammar L { token TOP { <e> } token e { <e> '+' <n> | <n> } token n { \d } }; say L.parse("1+2+3").so; say L.parse("1+2+")"#
);
differential_case!(
    subrule_captures_in_groups_and_aliases,
    r#"grammar G { token TOP { (<a> <b>) <x=a> $<y>=<b> [ <a> <b> ]+ } token a { 'a' } token b { 'b' } }; my $m = G.parse("ababababab"); say $m.so; say $m[0]<a>.Str, $m[0]<b>.Str; say $m<x>.Str, $m<y>.Str; say $m<a>.elems, $m<b>.elems; say $m.gist"#
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

/// Slice C (#10253): a pattern holding a code atom must take the compiled
/// engine. A silently declined `{ … }` / `<?{ … }>` / `:my` pattern would pass
/// every differential case above while measuring nothing.
#[test]
fn code_atoms_are_compiled() {
    let src = r#"my $n = 0;
say so "abc" ~~ / a { $n++ } b /;
say so "abc" ~~ / a <?{ $n++; True }> b /;
say so "abc" ~~ / :my $x = 1; a <?{ $x == 1 }> b /;
say $n;"#;
    let (ok, out, err) = run(src, &[("MUTSU_VM_STATS", "1")]);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "True\nTrue\nTrue\n2\n");
    let line = err
        .lines()
        .find_map(|l| l.split("regex-vm: ").nth(1))
        .unwrap_or_else(|| panic!("no regex-vm stats line: {err}"));
    assert!(
        !line.contains("code="),
        "a code atom declined to the walk: {line}"
    );
    let compiled: u64 = line
        .split_whitespace()
        .find_map(|w| w.strip_prefix("compiled="))
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("no compiled= in: {line}"));
    assert!(compiled >= 3, "the code patterns were not compiled: {line}");
}

/// #10456: code inside a separated quantifier's atom or separator, or in a
/// `&` conjunction's branches, must take the compiled engine — each reads the
/// enclosing captures through an inline level's view rather than declining.
#[test]
fn separated_and_conjunction_code_is_compiled() {
    let src = r#"my @log;
"x1,2" ~~ / (x) [ (\d) { @log.push: +$/[1] } ] +% [ ',' { @log.push: 's' ~ $/[1].elems } ] /;
"ab" ~~ / (a) [ (\w) { @log.push: +$/.list } & \w { @log.push: ~$0 } ] /;
say @log.join(',');"#;
    let (ok, out, err) = run(src, &[("MUTSU_VM_STATS", "1")]);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "1,s1,2,2,a\n");
    let line = err
        .lines()
        .find_map(|l| l.split("regex-vm: ").nth(1))
        .unwrap_or_else(|| panic!("no regex-vm stats line: {err}"));
    assert!(
        !line.contains("separator-code") && !line.contains("conjunction-code"),
        "a separated or conjunction code pattern declined to the walk: {line}"
    );
}

/// Slice D (#10254): a grammar with plain, proto, quantified and goal-matched
/// `<subrule>` calls must take the compiled engine whole. A silently declined
/// call would pass every differential case above while measuring nothing.
#[test]
fn subrule_calls_are_compiled() {
    let src = r#"grammar G {
    token TOP { <value>+ % ',' }
    proto token value {*}
    token value:sym<num>  { <digits> }
    token value:sym<list> { '[' ~ ']' <value>* % ',' }
    token digits { \d+ }
}
say G.parse("1,[2,3],4").so;
say so "ab" ~~ / <G::digits> | 'a' /;"#;
    let (ok, out, err) = run(src, &[("MUTSU_VM_STATS", "1")]);
    assert!(ok, "run failed: {err}");
    assert_eq!(out, "True\nTrue\n");
    let line = err
        .lines()
        .find_map(|l| l.split("regex-vm: ").nth(1))
        .unwrap_or_else(|| panic!("no regex-vm stats line: {err}"));
    assert!(
        !line.contains("subrule"),
        "a subrule call declined to the walk: {line}"
    );
    let runs: u64 = line
        .split_whitespace()
        .find_map(|w| w.strip_prefix("runs="))
        .and_then(|v| v.parse().ok())
        .unwrap_or_else(|| panic!("no runs= in: {line}"));
    assert!(runs >= 1, "the grammar parse did not run compiled: {line}");
}
