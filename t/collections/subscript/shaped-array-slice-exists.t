use v6;
use Test;

plan 16;

# `@a[0,1]` and `@a[0;1]` are different subscripts: a slice of two indices, and
# one multidimensional index. `:exists` used to decide between them by asking
# whether the *target* was shaped, so every `,` slice on a shaped array
# collapsed into a single Bool. The separator the user wrote is what decides --
# and it already survives to the compiler, which routes `Expr::MultiDimIndex`
# to a different path entirely.

{
    my @z[3];

    is-deeply (@z[0, 1]:exists), (False, False),
        'shaped array: a comma slice answers one Bool per index';
    is-deeply (@z[0; 1]:exists), False,
        'shaped array: a semicolon subscript answers one Bool';

    @z[1] = 9;
    is-deeply (@z[0, 1, 2]:exists), (False, True, False),
        'shaped array: the slice tracks each index separately';
    is-deeply (@z[1]:exists), True, 'shaped array: a single index still answers one Bool';
}

# The slice agrees with the value adverbs on the same array.
{
    my @z[3];
    @z[1] = 9;

    is-deeply (@z[0, 1, 2]:v), (9,), 'shaped array slice: :v keeps only real values';
    is-deeply (@z[0, 1, 2]:k), (1,), 'shaped array slice: :k keeps only real indices';
    is-deeply (@z[0, 1, 2]:kv), (1, 9), 'shaped array slice: :kv keeps only real pairs';
}

# A fully-assigned shaped array, across the adverbs and their negations.
{
    my @z[3];
    @z[0] = 1; @z[1] = 2; @z[2] = 3;

    is-deeply (@z[0, 2]:exists), (True, True), 'shaped array slice: assigned slots exist';
    is-deeply (@z[0, 1]:!exists), (False, False), 'shaped array slice: :!exists negates per index';
    is-deeply (@z[0, 2]:p), (0 => 1, 2 => 3), 'shaped array slice: :p pairs each index';
    is-deeply (@z[0, 1]:!v), (1, 2), 'shaped array slice: :!v keeps every slot';
    is-deeply (@z[*]:exists), (True, True, True), 'shaped array: a zen-ish `*` slice is per index';
}

# A multidimensional shaped array: a comma slice indexes the first dimension,
# a semicolon subscript addresses one cell.
{
    my @m[2; 2];
    @m[0; 0] = 1;
    @m[1; 1] = 4;

    is-deeply (@m[0; 0]:exists), True, '2-D shaped array: an assigned cell exists';
    is-deeply (@m[1; 0]:exists), False, '2-D shaped array: an unassigned cell does not exist';
    is-deeply (@m[0, 1]:exists), (True, True),
        '2-D shaped array: a comma slice indexes the rows, one Bool each';
}

# An unshaped array is unchanged.
{
    my @a;
    @a[0] = 1; @a[2] = 3;
    is-deeply (@a[0, 1, 2]:exists), (True, False, True),
        'unshaped array: a comma slice is still one Bool per index';
}
