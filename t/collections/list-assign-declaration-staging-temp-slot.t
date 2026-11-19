use Test;

# `my ($a, $b) = EXPR` stages EXPR in a temp array that the declaration's
# targets then read. The temp is read through its own slot rather than by
# name (#16897), so every shape that reuses or re-enters the declaration
# must still see its own values. Checked against rakudo.
plan 12;

sub pair-of($x) { ("v$x", $x + 1) }

{
    my @seen;
    for ^5 -> $i {
        my ($name, $next) = pair-of($i);
        @seen.push: "$name:$next";
    }
    is @seen.join(','), 'v0:1,v1:2,v2:3,v3:4,v4:5', 'a loop re-declares from each staging';
}

{
    sub depth($n) {
        return 0 if $n == 0;
        my ($here, $rest) = ($n, depth($n - 1));
        $here + $rest
    }
    is depth(5), 15, 'recursion keeps each frame its own staging';
}

{
    my @closures;
    for 1..3 -> $i {
        my ($a, $b) = $i, $i * 10;
        @closures.push: { $a + $b };
    }
    is @closures.map({ .() }).join(','), '11,22,33', 'a closure captures the destructured values';
}

{
    my ($first, $second) = (gather { take 'x'; take 'y'; take 'z' });
    is "$first$second", 'xy', 'a Seq right-hand side';
}

{
    my ($lo, $hi) = 3..*;
    is "$lo $hi", '3 4', 'a lazy Range right-hand side';
}

{
    my %h = a => 1;
    my ($p, $q) = %h;
    is $p.key, 'a', 'a Hash right-hand side yields its pairs';
    nok $q.defined, 'a missing element leaves the target undefined';
}

{
    my ($only, $none) = 42;
    is $only, 42, 'a single value fills the first target';
    nok $none.defined, 'and the rest stay undefined';
}

{
    my ($x, @rest) = 1, 2, 3;
    is "$x|{@rest.join(',')}", '1|2,3', 'a slurpy array target';
}

{
    my @values = gather for ^3 {
        my ($k, $v) = "k$_", $_ * 2;
        take "$k=$v";
    }
    is @values.join(','), 'k0=0,k1=2,k2=4', 'inside a gather body';
}

{
    my ($u, $w) = 'outer', 'w';
    {
        my ($u, $w) = 'inner', $u;
        is "$u/{$w.raku}", 'inner/Any', 'the right-hand side sees the new, still undefined target';
    }
}
