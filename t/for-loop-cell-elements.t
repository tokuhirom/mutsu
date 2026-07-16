use v6;
use Test;

# Pin for the ContainerRef arms in loop_var_unchanged (vm_loop_writeback.rs):
# `.grep` leaves shared rw-alias ContainerRef cells in its source array, and a
# subsequent `for` over that array must (a) keep element-alias semantics and
# (b) stay O(n) — the unchanged-value guard must recognize a cell whose content
# still equals the loop var, instead of falling through to the O(n) source
# rebuild every iteration (O(n^2): a 20k-element loop took ~9s instead of ~10ms).

plan 6;

# (a) semantics: read-only for over a cell-holding array
{
    my @a = 1, 2, 3, 4;
    @a.grep(*.so).elems;
    my $sum = 0;
    for @a -> $x { $sum += $x }
    is $sum, 10, 'read-only for over cell-holding array iterates values';
    is-deeply @a, [1, 2, 3, 4], 'source array unchanged after read-only for';
}

# (a) semantics: topic rebind still writes back through/over the cell
{
    my @b = 1, 2, 3;
    @b.grep(*.so).elems;
    for @b { $_ = $_ + 100 }
    is-deeply @b, [101, 102, 103], 'topic rebind writes back over cell elements';
}

# (a) semantics: named-param container mutation still propagates
{
    my @c = [1, 2], [3, 4];
    @c.grep(*.so).elems;
    for @c -> @row { @row.push(8) }
    is @c[0][2], 8, 'container mutation through named param propagates (row 0)';
    is @c[1][2], 8, 'container mutation through named param propagates (row 1)';
}

# (b) perf: the loop must be O(n), not O(n^2). With the guard broken this
# takes ~9s release / much longer debug for 20k elements; healthy runs are
# ~10ms. 15s leaves a wide margin for slow CI machines while still failing
# decisively on an O(n^2) regression.
{
    my @big = (1..20000).map({ "s" ~ $_ });
    @big.grep(*.so).elems;
    my $t0 = now;
    my $n = 0;
    for @big -> $s { $n++ }
    my $elapsed = now - $t0;
    ok $elapsed < 15, "for over 20k cell-holding elements is O(n) ({$elapsed.round(0.01)}s)";
}

done-testing;
