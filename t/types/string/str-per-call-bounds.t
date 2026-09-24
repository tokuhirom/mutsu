use Test;
use MONKEY-SEE-NO-EVAL;

# Pins the observable results of the Str operations whose per-call cost was
# brought down to Rakudo's bound (#9147): the invocant is now shared instead of
# copied, and a `$limit` stops the scan. Every answer must be what it was.

plan 12;

subtest 'Str.Str / .Stringy / prefix ~ / interpolation share a plain Str', {
    plan 8;
    my $s = "héllo wörld";
    is $s.Str, $s, '.Str';
    is $s.Stringy, $s, '.Stringy';
    is ~$s, $s, 'prefix ~';
    is "$s", $s, 'interpolation';
    is $s.Str.WHAT.^name, 'Str', '.Str stays a Str';
    my $i = IntStr.new(5, "five");
    is ~$i, 'five', 'prefix ~ on an allomorph is its Str part';
    is ~$i.WHAT.^name, 'IntStr', 'the allomorph itself is untouched';
    multi prefix:<~>(Str $x where * eq 'magic') { 'overloaded' }
    is ~'magic', 'overloaded', 'a user prefix:<~> candidate still wins';
}

subtest 'Str.WHICH', {
    plan 11;
    my $s = "abc";
    is $s.WHICH, 'Str|abc', '.WHICH text';
    is $s.WHICH.^name, 'ValueObjAt', 'a ValueObjAt';
    ok $s.WHICH === "abc".WHICH, 'equal strings are ===';
    nok $s.WHICH === "abd".WHICH, 'different strings are not ===';
    ok $s.WHICH === ValueObjAt.new("Str|abc"), 'matches a constructed ValueObjAt';
    is $s.WHICH.raku, 'ValueObjAt.new("Str|abc")', '.raku';
    is $s.WHICH.gist, 'Str|abc', '.gist';
    ok $s.WHICH eq "Str|abc", 'eq against the text';
    is "é".WHICH, 'Str|é', 'non-ASCII';
    is-deeply $s.WHICH.raku.EVAL, $s.WHICH, '.raku.EVAL round-trips (eqv)';
    ok $s.WHICH eqv "abc".WHICH, 'eqv between two .WHICH results';
}

subtest 'chomp', {
    plan 7;
    is "abc".chomp, 'abc', 'nothing to chomp (method)';
    is chomp("abc"), 'abc', 'nothing to chomp (sub)';
    is "abc\n".chomp, 'abc', 'LF';
    is "abc\r\n".chomp, 'abc', 'CRLF';
    is "abc\r".chomp, 'abc', 'CR';
    is "abc\n\n".chomp, "abc\n", 'only one';
    is IntStr.new(5, "5\n").chomp.^name, 'Str', 'an allomorph chomps to a Str';
}

subtest 'eq / ne', {
    plan 8;
    ok "abc" eq "abc", 'equal';
    nok "abc" eq "abcd", 'different lengths';
    ok "abc" ne "ab", 'ne on different lengths';
    nok "abc" ne "abc", 'ne on equal';
    ok "é" eq "é", 'non-ASCII';
    nok "é" eq "e", 'non-ASCII vs ASCII';
    ok 42 eq "42", 'a non-Str operand is stringified';
    ok "abc".encode eq "abc", 'a utf8 operand is decoded';
}

subtest 'lt / gt / le / ge', {
    plan 8;
    ok "abc" lt "abd", 'lt';
    ok "ab" lt "abc", 'lt: prefix is smaller';
    nok "abc" lt "ab", 'lt: longer is not smaller';
    ok "b" gt "abc", 'gt';
    ok "abc" le "abc", 'le equal';
    ok "abc" ge "abc", 'ge equal';
    ok "é" gt "z", 'codepoint order for non-ASCII';
    ok 10 lt 9, 'numbers compare as strings';
}

subtest 'leg / cmp / before / after / min / max', {
    plan 10;
    is "abc" leg "abd", Less, 'leg Less';
    is "abd" leg "abc", More, 'leg More';
    is "abc" leg "abc", Same, 'leg Same';
    is "ab" cmp "abc", Less, 'cmp prefix';
    is "é" cmp "e", More, 'cmp non-ASCII';
    ok "a" before "b", 'before';
    ok "b" after "a", 'after';
    is "pear" min "apple", 'apple', 'min';
    is "pear" max "apple", 'pear', 'max';
    is <b c a>.sort.join, 'abc', 'sort';
}

subtest 'lines($limit)', {
    plan 7;
    my $s = "a\nb\r\nc\rd\n";
    is-deeply $s.lines(2), ("a", "b").Seq, 'first two';
    is-deeply $s.lines(0), ().Seq, 'zero';
    is-deeply $s.lines(10), ("a", "b", "c", "d").Seq, 'more than there are';
    is-deeply $s.lines(*), ("a", "b", "c", "d").Seq, 'Whatever';
    is-deeply $s.lines(Inf), ("a", "b", "c", "d").Seq, 'Inf';
    is-deeply "x\ny".lines(2), ("x", "y").Seq, 'last line without a newline';
    is-deeply "é\nü\nö".lines(2), ("é", "ü").Seq, 'non-ASCII';
}

subtest 'words($limit)', {
    plan 5;
    my $s = "  one two\tthree\nfour ";
    is-deeply $s.words(2), ("one", "two").Seq, 'first two';
    is-deeply $s.words(0), ().Seq, 'zero';
    is-deeply $s.words(9), ("one", "two", "three", "four").Seq, 'more than there are';
    is-deeply $s.words(*), ("one", "two", "three", "four").Seq, 'Whatever';
    is-deeply "é ü ö".words(2), ("é", "ü").Seq, 'non-ASCII';
}

subtest 'comb(Int, $limit)', {
    plan 6;
    is-deeply "abcdefg".comb(3, 2), ("abc", "def").Seq, 'two chunks of three';
    is-deeply "abcdefg".comb(3, 5), ("abc", "def", "g").Seq, 'short last chunk';
    is-deeply "abcdefg".comb(3), ("abc", "def", "g").Seq, 'no limit';
    is-deeply "abc".comb(1, 0), ().Seq, 'zero limit';
    is-deeply "e\x[301]xy".comb(1, 2), ("e\x[301]", "x").Seq, 'graphemes, not codepoints';
    is-deeply "abc".comb(0, 2), ("a", "b").Seq, 'a non-positive size is 1';
}

subtest 'comb(Str, $limit)', {
    plan 3;
    is-deeply "abab".comb("ab", 1), ("ab",).Seq, 'needle';
    is-deeply "héllo".comb("", 3), ("h", "é", "l").Seq, 'empty needle';
    is-deeply "héllo".comb(""), ("h", "é", "l", "l", "o").Seq, 'empty needle, no limit';
}

subtest 'comb(Regex, $limit)', {
    plan 7;
    is-deeply "a1b22c333".comb(/\d+/, 2), ("1", "22").Seq, 'first two matches';
    is-deeply "a1b22c333".comb(/\d+/), ("1", "22", "333").Seq, 'all matches';
    is-deeply "ab".comb(/x/, 3), ().Seq, 'no match';
    is-deeply "éàü".comb(/./, 2), ("é", "à").Seq, 'non-ASCII';
    is-deeply "aXbXc".comb(/<[a..z]>/, 5), ("a", "b", "c").Seq, 'limit above the count';
    is "a1b2".comb(/\d/, 1, :match).map(*.Str).join, '1', ':match with a limit';
    my @seen;
    my @r = "a1b2c3".comb(/\d { @seen.push: ~$/ }/, 2);
    is-deeply @r, ["1", "2"], 'a code-block regex stops at the limit';
}

subtest 'Uni.codes', {
    plan 3;
    my $u = "e\x[301]x".NFD;
    is $u.codes, 3, '.codes';
    is $u.elems, 3, '.elems';
    is +$u, 3, 'numeric';
}
