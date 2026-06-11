use Test;

plan 7;

# `take-rw <lvalue>` must capture the *source container* (a shared cell), so the
# gathered value keeps container identity (`=:=`) with the original element and a
# write through one is observed by the other. Mirrors roast/S04-statements/gather.t
# test 38 (take-rw reference equality through nested indexing).

# Single-level element, stored into an array element (item context preserves the
# gathered Seq's container elements).
{
    my @spot = 10, 20, 30;
    my @n;
    @n[0] = eager gather { take-rw @spot[1] };
    is @n[0][0], 20, 'take-rw gathered the element value';
    ok @n[0][0] =:= @spot[1], 'take-rw keeps container identity (=:=)';
    @n[0][0] = 999;
    is @spot[1], 999, 'writing through the taken cell updates the source element';
}

# Nested element through a for-gather (the gather.t shape).
{
    my @spot = [10, 20, 30], [40, 50, 60];
    my @neighbors;
    @neighbors[0] = eager gather for 0, 1, 2 { take-rw @spot[0][$_] };
    is @neighbors[0][1], 20, 'nested take-rw gathered the right value';
    ok @neighbors[0][1] =:= @spot[0][1], 'nested take-rw keeps container identity';
}

# A plain `take` must NOT alias — it snapshots the value (no container identity).
{
    my @spot = 10, 20, 30;
    my @n;
    @n[0] = eager gather { take @spot[1] };
    nok @n[0][0] =:= @spot[1], 'plain take is a snapshot, not a live alias';
}

# Out-of-range element under `// next`: the cell wraps an undefined value, so the
# `//` fallback must still fire (value_is_defined looks through the cell).
{
    my @spot = 10, 20, 30;
    my @got = eager gather for 0, 5 { take-rw @spot[$_] // next };
    is @got.elems, 1, '// next skips the undefined (out-of-range) element';
}
