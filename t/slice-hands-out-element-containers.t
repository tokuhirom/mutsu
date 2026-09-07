use Test;

# A positional/associative SLICE hands out the container's own element
# containers, not copies of their values, so a topic or parameter bound to a
# slice element writes through to the source.
#
# `todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` section B
# producer 2: `for @a[0..1] { $_ = 5 }` silently dropped the write, where the
# plain-array `for` (which writes back by source name) did not.
#
# The POSITIONAL half is PARKED and its rows are `todo`-marked. Handing out the
# array's own element containers from a positional slice corrupts
# `Text::CSV`'s `csv(in => $aoa.iterator, out => $fno)`: every row of `$aoa`
# becomes `IterationEnd`, which its whitelisted `t/90_csv.t` catches (test 503,
# "AOA parse out"). Measured on a clean run with nothing else building --
# the ASSOCIATIVE half and everything else in this file is unaffected and
# ships. See the ticket's section B for the split.

plan 16;

{
    my @a = 1, 2, 3;
    for @a[0..1] { $_ = 5 }
    todo 'positional-slice element containers are parked (Text::CSV 90_csv.t)';
    is-deeply @a, [5, 5, 3], 'for over a Range slice writes through';
}

{
    my @a = 1, 2, 3;
    for @a[0, 1] { $_ = 5 }
    todo 'positional-slice element containers are parked (Text::CSV 90_csv.t)';
    is-deeply @a, [5, 5, 3], 'for over a comma-list slice writes through';
}

{
    my @a = 1, 2, 3;
    for @a[^2] { $_ = 5 }
    todo 'positional-slice element containers are parked (Text::CSV 90_csv.t)';
    is-deeply @a, [5, 5, 3], 'for over an exclusive-Range slice writes through';
}

{
    my @a = 1, 2, 3;
    @a[0..1].map({ $_ = 5 }).eager;
    todo 'positional-slice element containers are parked (Text::CSV 90_csv.t)';
    is-deeply @a, [5, 5, 3], '.map over a slice writes through';
}

{
    my @a = 1, 2, 3;
    @a[0, 2].grep({ $_ = 5 }).eager;
    todo 'positional-slice element containers are parked (Text::CSV 90_csv.t)';
    is-deeply @a, [5, 2, 5], '.grep over a slice writes through';
}

{
    my %h = a => 1, b => 2, c => 3;
    for %h<a b> { $_ = 5 }
    is-deeply %h, {a => 5, b => 5, c => 3}, 'for over an associative slice writes through';
}

# ... and nothing else about a slice changes.

{
    my @a = 1, 2, 3;
    is @a[0, 1].WHAT.^name, 'List', 'a slice is still a List';
    is-deeply @a[0, 1].List, (1, 2), 'a slice still reads as its values';
    is @a[0..1].join(','), '1,2', 'a slice still stringifies its values';
    is @a[0..1].sum, 3, 'a slice still numifies its values';
}

{
    # A slice ASSIGNMENT copies: the new array does not alias the source.
    my @a = 1, 2, 3;
    my @b = @a[0, 1];
    @b[0] = 9;
    is-deeply @a, [1, 2, 3], 'list-assigning a slice copies the values out';
    is-deeply @b, [9, 2], '...into an independent array';
}

{
    # An out-of-range slice index reads the typed default and must NOT grow the
    # source array to produce a container for it.
    my @a = 1, 2, 3;
    my $slice = @a[1..4];
    is @a.elems, 3, 'an out-of-range slice does not grow the array';
    is $slice.elems, 4, '...and still yields one entry per index';
    ok !$slice[3].defined, '...whose past-the-end entries are undefined';
}

{
    # An immutable source has no slots to hand out, so its elements stay bare.
    my @a := (1, 2, 3);
    is-deeply @a[0, 1].List, (1, 2), 'a List slice still reads its values';
}
