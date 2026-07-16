use v6;
use Test;

plan 14;

# Lazy pull/resume of a gather whose body contains NESTED or SEQUENTIAL for
# loops, or several takes per iteration. The single resume slot used to be
# overwritten by each enclosing loop as the take-limit signal propagated out,
# losing the inner loop's position (and statements after a take were skipped
# on resume). Root cause of roast/integration/99problems-51-to-60.t tests 8/21
# (P55 cbal-tree / P59 hbal-tree).

# (1) body loop var must not leak into the consumer scope on pull
# (.gist comparison: the itemization of the pulled element in .raku is a
# separate open issue and not what this pins)
sub leak() { gather { for [Any] -> $a { take ['y', $a] } } }
my @pairs;
for leak() -> $a {
    for leak() -> $b {
        push @pairs, ($a.gist ~ '|' ~ $b.gist);
    }
}
is @pairs.elems, 1, 'nested pull over same sub: one pair';
is @pairs[0], '[y (Any)]|[y (Any)]', 'consumer loop vars not clobbered by body loop var';

# (2) multiple takes per iteration survive lazy pull
sub multi-take() { gather { for 1, 2 -> $x { take $x; take $x * 10 } } }
is multi-take().join(','), '1,10,2,20', 'multi-take: full force';
my @mt; for multi-take() -> $v { push @mt, $v }
is @mt.join(','), '1,10,2,20', 'multi-take: lazy pull keeps takes after the suspending take';

# (3) nested eager loops inside a pulled gather
sub nested-eager() { gather { for 1, 2 -> $a { for 3, 4 -> $b { take $a * 10 + $b } } } }
my @ne; for nested-eager() -> $v { push @ne, $v }
is @ne.join(','), '13,14,23,24', 'nested eager loops: inner position survives suspend';

# (4) recursive gather: nested lazy-gather loops
sub u(Int $n) {
    return (1, 2) if $n == 0;
    gather {
        for u($n - 1) -> $a {
            for u($n - 1) -> $b {
                take $a * 10 + $b;
            }
        }
    }
}
is u(1).join(','), '11,12,21,22', 'recursive gather depth 1';
is +u(2), 16, 'recursive gather depth 2: all 16 combinations';

# (5) conditional multi-take in the second sequential loop (P59 shape)
sub g6(Int $n) {
    return (1, 2) if $n == 0;
    gather {
        for g6($n - 1) -> $a {
            for g6($n - 1) -> $b {
                take $a * 10 + $b;
            }
            for g6($n - 1) -> $b {
                if $b % 2 == 0 {
                    take $a * 100 + $b;
                    take $b * 100 + $a;
                }
            }
        }
    }
}
is g6(1).join(','), '11,12,102,201,21,22,202,202', 'sequential loops + conditional double take: depth 1';
is +g6(2), 144, 'sequential loops + conditional double take: depth 2';

# (6) P55 cbal-tree shape: gather over recursion, is-deeply on structure
sub cbal-tree(Int $n) {
    return [Any] if $n == 0;
    gather {
        if $n % 2 == 1 {
            my $k = ($n - 1) div 2;
            for cbal-tree($k) -> $a {
                for cbal-tree($k) -> $b {
                    take ['x', $a, $b],;
                }
            }
        } else {
            my $k = $n div 2;
            for cbal-tree($k) -> $a {
                for cbal-tree($k - 1) -> $b {
                    take ['x', $a, $b],;
                }
            }
            for cbal-tree($k - 1) -> $a {
                for cbal-tree($k) -> $b {
                    take ['x', $a, $b],;
                }
            }
        }
    }
}
is-deeply [ cbal-tree(3) ], [['x', ['x', Any, Any], ['x', Any, Any]],],
    'P55 cbal-tree(3): outer loop var intact through recursion';
is +cbal-tree(4), 4, 'P55 cbal-tree(4) count';

# (7) int-range loop nested in a pulled gather
sub ir() { gather { for 1..2 -> $a { for 1..2 -> $b { take $a * 10 + $b } } } }
my @ir; for ir() -> $v { push @ir, $v }
is @ir.join(','), '11,12,21,22', 'nested int-range loops under lazy pull';

# (8) a take inside a SUB called from the gather's for-body must NOT make the
# enclosing for-loop re-enter its current iteration (which would re-run the sub
# and duplicate its side effects). Regression guard for roast advent2010-day11.t.
sub push-take(@values, $new, $n) {
    @values.push($new);
    @values.shift if +@values > $n;
    if +@values == $n {
        for @values { take $_ }
    }
}
sub sliding(@a, $n) {
    my @values;
    gather for @a -> $x { push-take(@values, $x, $n) }
}
my @windows;
for sliding(<a b c d e>, 3) -> $p, $q, $r {
    push @windows, "$p$q$r";
}
is @windows.join(' '), 'abc bcd cde', 'take inside a sub called from gather-for: no duplicated side effects';
is @windows.elems, 3, 'sliding-window produced the right number of windows';
