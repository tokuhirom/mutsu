use Test;

# From-end (`*-1`) index assignment through a `:=`-bound array must resolve the
# real length via the shared cell (regression: it read len 0 → negative
# effective index → X::OutOfRange).

plan 7;

{
    my @b = (1, 2, 3);
    my @a := @b;
    @a[*-1] = 9;
    is @b.gist, '[1 2 9]', 'from-end assign reaches bind source';
}

{
    my @b = (1, 2, 3, 4, 5);
    my @a := @b;
    is @a[*-1], 5, 'from-end read through bound array';
    is @a[*-2], 4, 'from-end read (-2) through bound array';
}

{
    my @b = (1, 2, 3, 4, 5);
    my @a := @b;
    @a[*-2, *-1] = 8, 9;
    is @b.gist, '[1 2 3 8 9]', 'multi from-end assign through bind';
}

{
    my @b = (1, 2, 3, 4, 5);
    my @a := @b;
    @a[*-3 .. *-1] = 0, 0, 0;
    is @b.gist, '[1 2 0 0 0]', 'range from-end assign through bind';
}

# plain (non-bound) from-end assign must stay correct
{
    my @a = (1, 2, 3);
    @a[*-1] = 7;
    is @a.gist, '[1 2 7]', 'plain from-end assign';

    my @z = (1, 2, 3, 4);
    @z[*-2 .. *-1] = 8, 9;
    is @z.gist, '[1 2 8 9]', 'plain range from-end assign';
}
