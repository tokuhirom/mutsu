use Test;

# Regression test: `$_[N] = ...` inside a `for @aggregate` statement modifier
# must write back to the per-index element slot, not clobber the whole
# aggregate with the last topic value.

plan 6;

{
    my @a = ([1], [2], [3]);
    $_[1] = 9 for @a;
    is-deeply @a, [[1, 9], [2, 9], [3, 9]], 'topic element writeback into array of arrays';
}

{
    my @a = ([1], [2], [3]);
    for @a { $_[1] = 9 }
    is-deeply @a, [[1, 9], [2, 9], [3, 9]], 'topic element writeback in block for';
}

{
    # classify into an existing filled hash: each value array grows
    my %r = (5 => [1], 10 => [2]);
    my @vals = %r.values;
    $_[1] = $_[0] for @vals;
    is %r<5>.elems, 2, 'value array via temp grows correctly (5)';
    is %r<10>.elems, 2, 'value array via temp grows correctly (10)';
}

{
    # nested topic indexing keeps the aggregate intact
    my @a = ([10, 20], [30, 40]);
    $_[0] = $_[0] + 1 for @a;
    is-deeply @a, [[11, 20], [31, 40]], 'increment first element of each sub-array';
}

{
    my @a = ('a', 'b', 'c');
    $_ = $_.uc for @a;
    is-deeply @a, ['A', 'B', 'C'], 'plain topic writeback still works';
}
