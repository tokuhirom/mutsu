use Test;

# ADR-0113: a frame-lexical `my sub` stays frame-lexical when the only calls
# to it sit inside closures of its routine body (`map`, `grep`, `reduce`,
# `sort`, ...). Several of those paths do not run the closure's compiled
# chunk but compile its AST again, so the fresh chunk must inherit the call
# table: the inner sub is never in the routine registry.

plan 20;

# Shadowing a same-named outer sub from inside a closure.
sub shade($x) { "outer $x" }
sub shaded(@a) {
    my sub shade($x) { "inner $x" }
    @a.map({ shade($_) }).join(',')
}
is shaded([1, 2]), 'inner 1,inner 2', 'map closure calls the inner sub';
is shade(3), 'outer 3', 'outer sub untouched';

sub via-map(@a) { my sub inc($x) { $x + 1 }; my @r = @a.map({ inc($_) }); @r }
is-deeply via-map([1, 2, 3]), [2, 3, 4], 'map with a bare block';

sub via-pointy(@a) { my sub dbl($x) { $x * 2 }; my @r = @a.map(-> $v { dbl($v) }); @r }
is-deeply via-pointy([1, 2]), [2, 4], 'map with a pointy block';

sub via-grep(@a) { my sub even($x) { $x %% 2 }; my @r = @a.grep({ even($_) }); @r }
is-deeply via-grep([1, 2, 3, 4]), [2, 4], 'grep';

sub via-first(@a) { my sub big($x) { $x > 1 }; @a.first({ big($_) }) }
is via-first([1, 2, 3]), 2, 'first';

sub via-sort(@a) { my sub desc($a, $b) { $b <=> $a }; @a.sort({ desc($^a, $^b) }).List }
is-deeply via-sort([3, 1, 2]), (3, 2, 1), 'sort comparator';

sub via-reduce(@a) { my sub plus($a, $b) { $a + $b }; @a.reduce({ plus($^a, $^b) }) }
is via-reduce([1, 2, 3]), 6, 'reduce method';

sub via-reduce-sub() { my sub t($n) { $n + 1 }; reduce -> $b, $i { t($b) + $i }, 0, |^3 }
is via-reduce-sub(), 6, 'reduce sub with a pointy block';

sub via-for(@a) { my sub sq($x) { $x * $x }; my @r; @r.push: sq($_) for @a; @r }
is-deeply via-for([1, 2, 3]), [1, 4, 9], 'statement-modifier for';

sub via-seq() { my sub nxt($x) { $x * 2 }; (1, { nxt($_) } ... * > 20).List }
is-deeply via-seq(), (1, 2, 4, 8, 16, 32), 'sequence generator';

sub via-classify() {
    my sub k($x) { $x %% 2 ?? 'e' !! 'o' }
    (1..4).classify({ k($_) }).sort.map({ .key ~ '=' ~ .value.join('') }).join(' ')
}
is via-classify(), 'e=24 o=13', 'classify';

sub via-deepmap() { my sub dm($x) { $x + 1 }; [[1, 2], [3]].deepmap({ dm($_) }) }
is-deeply via-deepmap(), [[2, 3], [4]], 'deepmap';

sub via-start() { my sub s1($x) { $x * 3 }; await start { s1(4) } }
is via-start(), 12, 'start block';

sub via-gather() { my sub g($x) { $x + 10 }; my @r = gather { for 1..3 { take g($_) } }; @r }
is-deeply via-gather(), [11, 12, 13], 'gather body';

sub via-nested(@a) {
    my sub nn($x) { $x + 1 }
    my @r = @a.map({ [+] (1..2).map({ nn($_) }) });
    @r
}
is-deeply via-nested([1, 2]), [5, 5], 'closure nested in a closure';

sub via-recursion($n) {
    my sub fact($k) { $k <= 1 ?? 1 !! $k * fact($k - 1) }
    my @r = (1..$n).map({ fact($_) });
    @r
}
is-deeply via-recursion(5), [1, 2, 6, 24, 120], 'recursive inner sub from a closure';

sub via-capture() {
    my $acc = 0;
    my sub add-it($x) { $acc += $x }
    (1..3).map({ add-it($_) }).eager;
    $acc
}
is via-capture(), 6, 'inner sub writes a captured lexical';

sub via-supply() { my sub ss($x) { $x * 5 }; my @r; Supply.from-list(1..3).tap({ @r.push: ss($_) }); @r }
is-deeply via-supply(), [5, 10, 15], 'Supply tap';

# Repeated calls keep working (the derived definition is memoized).
is (via-map([5]) for ^3).map(*.[0]).join(','), '6,6,6', 'repeat calls';
