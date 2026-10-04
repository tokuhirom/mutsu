use Test;

# ADR-11276 slice 3: `comb`, `words`, `lines` and `ords` with no arguments
# are rows owned by `Str` and `Cool`. Calls on a variable are answered by the
# call-site lane; the loops run each site repeatedly.

plan 8;

subtest 'comb', {
    plan 3;
    my $s = "ab\x[301]c";
    is-deeply $s.comb.List, ("a", "b\x[301]", "c"), 'graphemes';
    is-deeply "".comb.List, (), 'empty';
    is-deeply (^3).map({ $s.comb.elems }).List, (3, 3, 3), 'one site, repeated';
}

subtest 'words', {
    plan 2;
    my $s = "  a b\n\tc  ";
    is-deeply $s.words.List, <a b c>, 'split on whitespace runs';
    is-deeply (^3).map({ $s.words.elems }).List, (3, 3, 3), 'one site, repeated';
}

subtest 'lines', {
    plan 3;
    my $s = "l1\r\nl2\rl3\n";
    is-deeply $s.lines.List, <l1 l2 l3>, 'each line ending is dropped';
    is-deeply "a\n\nb".lines.List, ("a", "", "b"), 'an empty line';
    is-deeply (^3).map({ $s.lines.elems }).List, (3, 3, 3), 'one site, repeated';
}

subtest 'ords', {
    plan 3;
    is-deeply "e\x[301]".ords.List, (233,), 'the NFC codepoints';
    is-deeply "ab".ords.WHAT, Seq, 'a Seq';
    my $s = "xy";
    is-deeply (^3).map({ $s.ords.List }).List, ((120, 121) xx 3).List, 'one site, repeated';
}

subtest 'Cool receivers stringify', {
    plan 4;
    is-deeply 12345.comb.List, ("1", "2", "3", "4", "5"), 'Int.comb';
    is-deeply 1.5.ords.List, (49, 46, 53), 'Rat.ords';
    is-deeply [1, "a b"].words.List, ("1", "a", "b"), 'Array.words reads "1 a b"';
    is-deeply %(k => "v w").words.List, <k v w>, 'Hash.words reads "k\tv w"';
}

is-deeply "a b".words.WHAT, Seq, 'words is a Seq';

{
    my $s = "q r";
    my $w = $s.words;
    is-deeply $w.List, <q r>, 'a Seq is read once';
}

ok Str.^can('lines') && Cool.^can('comb'), 'introspection sees the methods';
