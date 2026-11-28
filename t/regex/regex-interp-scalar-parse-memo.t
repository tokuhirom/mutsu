# A regex that names a plain scalar (`/ $needle \d+ /`) reads it at match time,
# so its parse no longer depends on the value and is memoized by the pattern
# text (#19296). These pin that every execution still sees the variable's
# current value and kind, and that a pattern whose pre-pass does read state
# is not memoized. Values are rakudo's.
use Test;

plan 12;

{
    my @lines = 'host3 a', 'node7 b', 'host12 c';
    my @hits;
    for <host node host> -> $needle {
        @hits.push: @lines.grep({ $_ ~~ / $needle \d+ / }).elems;
    }
    is @hits.join(','), '2,1,2', 'the scalar is read on every execution';
}

{
    my $v = 'b';
    my @r;
    for ^3 {
        @r.push: ('abc' ~~ / $v /) ?? ~$/ !! 'none';
        $v = $_ == 0 ?? / <[a..b]>+ / !! 'zz';
    }
    is @r.join(','), 'b,ab,none', 'a scalar that changes between Str and Regex';
}

{
    my $v = 'a';
    ok 'xa' ~~ / x $v /, 'Str value matches';
    $v = %(a => 1);
    throws-like { 'xa' ~~ / x $v / }, Exception, 'a hash value is still rejected';
    $v = 'a';
    ok 'xa' ~~ / x $v /, 'and a Str value matches again';
}

{
    my $n = 3;
    my @r;
    for 1..3 -> $k {
        $n = $k;
        @r.push: so 'aaa' ~~ / ^ a ** {$n} $ /;
    }
    is @r.join(','), 'False,False,True', 'a code block quantifier with a changing value';
}

{
    my $x = 'q';
    ok 'pq' ~~ / p $x /, 'outer scalar';
    ok 'pz' ~~ / :my $y = 'z'; p $y /, 'a :my scalar';
    $x = 'r';
    ok 'pr' ~~ / p $x /, 'the outer one is read again with its new value';
}

{
    my @alts = <cat dog>;
    my @r;
    for ^2 {
        @r.push: so 'a dog' ~~ / @alts /;
        @alts = <bird>;
    }
    is @r.join(','), 'True,False', 'an array interpolation is re-read';
}

{
    my $w = 'b';
    my @r;
    for <abc xyz> -> $s {
        @r.push: so $s ~~ / $w /;
        $w = 'y';
    }
    is @r.join(','), 'True,True', 'the same pattern text with a new value';
}

{
    my $sep = ',';
    my @r;
    for ',', ';' -> $s {
        $sep = $s;
        @r.push: 'a,b;c'.comb(/ <-[,;]> $sep? /).join('|');
    }
    is @r.join(' '), 'a,|b|c a|b;|c', 'an interpolated scalar in .comb';
}
