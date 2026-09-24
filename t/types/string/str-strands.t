use v6;
use Test;

# ADR-0120: `x`, `~` with a shared operand, and interpolation build their
# result as a strand list (references to the operands, flattened once on
# first read) once it is at least 1 KiB. None of that may be observable: every
# result here must read exactly as the flat string it stands for, and a strand
# join is only taken where NFC cannot compose across it.

plan 37;

# -- x ------------------------------------------------------------------------

{
    my $r = "ab" x 1000;
    is $r.chars, 2000, 'x: .chars of a strand result';
    is $r.substr(0, 5), 'ababa', 'x: leading characters';
    is $r.substr(*-3), 'bab', 'x: trailing characters';
    is $r, "ab" x 1000, 'x: eq another repetition';
    ok $r eq ("abab" x 500), 'x: eq a repetition of a different unit';
}

{
    my $r = "あい" x 700;
    is $r.chars, 1400, 'x: multi-byte source';
    is $r.substr(699 * 2, 2), 'あい', 'x: last unit of a multi-byte source';
}

{
    # A source starting with a combining mark composes with the copy before
    # it, so it must be built flat and renormalized.
    my $r = "e\x[301]" x 600;
    is $r.chars, 600, 'x: a composing source still counts graphemes';
    is $r.NFC.elems, 600, 'x: and is NFC-composed';
    is ("\x[308]" x 5).chars, 1, 'x: a run of combining marks is one grapheme';
}

is ("a" x 0), '', 'x: zero count';
is ("" x 100000), '', 'x: empty source';
is ("abc" x 1).chars, 3, 'x: count of one';

{
    my $r = "a" x 5000;
    $r ~= "!";
    is $r.chars, 5001, 'x then ~=: an in-place append onto a strand result';
    is $r.substr(*-2), 'a!', 'x then ~=: the appended text is at the end';
}

throws-like { my $s = "ab" x 2**31 }, Exception,
    message => /'required number of graphemes'/,
    'x: a result over the grapheme cap dies catchably';
throws-like { my $s = "ab" x 2**32 }, Exception,
    message => /'Repeat count (4294967296) cannot be greater'/,
    'x: a count over the cap dies catchably';
lives-ok { my str $n = "a" x 2**32 - 1 }, 'x: a result at the cap is never materialized';

# -- ~ ------------------------------------------------------------------------

{
    my $a = "a" x 3000;
    my $r = $a ~ "b";
    is $r.chars, 3001, '~: shared left operand';
    is $r.substr(*-2), 'ab', '~: the right operand follows';
    is $a.chars, 3000, '~: the left operand is untouched';
    my $r2 = $r ~ "c";
    is $r2.substr(*-3), 'abc', '~: a strand result as the left operand';
    is $r.chars, 3001, '~: and that operand is untouched too';
}

{
    my $a = "e" x 2000;
    my $r = $a ~ "\x[301]";
    is $r.chars, 2000, '~: a combining right operand composes with the join';
    is $r.substr(*-1).NFD.elems, 2, '~: the last grapheme is the composed one';
}

{
    my $a = "x" x 2000;
    is (7 ~ $a).chars, 2001, '~: a non-Str left operand';
    is ($a ~ 7).substr(*-2), 'x7', '~: a non-Str right operand';
}

{
    my $acc = "";
    my @keep;
    for ^100 {
        $acc = $acc ~ ("z" x 20);
        @keep.push: $acc;
    }
    is $acc.chars, 2000, '~: repeated concatenation of a shared accumulator (past the strand limit)';
    is @keep[50].chars, 1020, '~: every intermediate result keeps its own value';
}

{
    my %h;
    my $k = ("k" x 1500) ~ "!";
    %h{$k} = 1;
    ok %h{"k" x 1500 ~ "!"}:exists, '~: a strand result works as a hash key';
    is-deeply ($k => 1).key.chars, 1501, '~: and as a Pair key';
}

# -- interpolation ------------------------------------------------------------

{
    my $a = "a" x 2000;
    my $b = "b" x 300;
    my $r = "[$a|$b|{ 42 }]";
    is $r.chars, 2306, 'interpolation: several shared parts';
    is $r.substr(0, 2), '[a', 'interpolation: the leading literal';
    is $r.substr(2000, 4), 'a|bb', 'interpolation: a join between two shared parts';
    is $r.substr(*-4), '|42]', 'interpolation: a non-Str part at the end';
}

{
    my $a = "o" x 2000;
    my $r = "$a\x[308]";
    is $r.chars, 2000, 'interpolation: a combining literal composes with the part before it';
}

{
    my @parts = "p" x 300 xx 100;
    my $r = @parts.map({ "$_" }).join;
    is $r.chars, 30000, 'join of interpolated strand parts';
}
