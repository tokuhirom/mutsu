use Test;

# #18248 shape C (ADR-12529 phase 3): a `sub` a loop body declares closes over
# that iteration's lexicals, and a recursive call it makes to itself must reach
# the same iteration's routine. mutsu bound the routine's siblings in its
# activation capture but not the routine itself, so the recursion resolved by
# name: to the registry's latest iteration (`(24 24)`), or, after a top-level
# loop, to nothing (`Unknown function`). Every expectation was checked against
# rakudo 2026.11.

plan 6;

{
    my @a;
    for 3, 4 -> $v {
        my $q = $v;
        sub f($n) { $n <= 1 ?? $q !! $n * f($n - 1) }
        @a.push(-> { f(3) });
    }
    is-deeply @a.map({ $_() }).List, (18, 24), 'a closure calling the routine recurses in its iteration';
}

{
    my @b;
    for 3, 4 -> $v {
        my $q = $v;
        sub g($n) { $n <= 1 ?? $q !! $n * g($n - 1) }
        @b.push(&g);
    }
    is-deeply @b.map({ $_(3) }).List, (18, 24), 'the routine taken out of its iteration recurses there';
}

my @top;
for 3, 4 -> $v {
    my $q = $v;
    sub g2($n) { $n <= 1 ?? $q !! $n * g2($n - 1) }
    @top.push(&g2);
}
is-deeply @top.map({ $_(3) }).List, (18, 24), 'a top-level loop body\'s routine recurses after the loop';

# Mutual recursion between two routines of one iteration.
{
    my @c;
    for 10, 20 -> $v {
        my $base = $v;
        sub ev($n) { $n == 0 ?? $base !! od($n - 1) }
        sub od($n) { $n == 0 ?? -$base !! ev($n - 1) }
        @c.push(&ev);
    }
    is-deeply @c.map({ $_(3) }).List, (-10, -20), 'mutual recursion stays in its iteration';
}

# A routine that reads nothing of the iteration recurses as before.
{
    my @d;
    for 1, 2 -> $v {
        sub fact($n) { $n <= 1 ?? 1 !! $n * fact($n - 1) }
        @d.push(&fact);
    }
    is-deeply @d.map({ $_(5) }).List, (120, 120), 'a routine closing over nothing recurses';
}

# Called inside the iteration that declared it.
{
    my @e;
    for 2, 3 -> $v {
        my $q = $v;
        sub pw($n) { $n == 0 ?? 1 !! $q * pw($n - 1) }
        @e.push(pw(3));
    }
    is-deeply @e.List, (8, 27), 'recursion during the iteration itself';
}
