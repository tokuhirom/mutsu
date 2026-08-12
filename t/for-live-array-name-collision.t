use v6;
use Test;

# `for @in` live-array iteration (RT113026) re-reads its source after each
# pass to pick up growth. The source slot was resolved bare-name-first, so a
# same-named scalar (`$in` — scalar locals are stored sigil-less) was read
# instead of `@in`: its larger length was misread as "the array grew" and the
# loop re-ran the final iteration (Text::CSV 90_csv.t emitted the last row
# twice through Block/Channel/Supplier outputs).

plan 4;

{
    my $in = [1, 2, 3];
    my @in = $in.list;
    @in.shift;
    my @seen;
    for @in -> $row { @seen.push: $row }
    is-deeply @seen, [2, 3], 'for @in iterates the array, not the same-named scalar';
}

{
    sub f(:$in!) {
        my @in = $in.list;
        @in.shift;
        my $n = 0;
        for @in -> $r { $n++ }
        $n;
    }
    is f(in => [[1], [2], [3]]), 2, 'named param $in does not confuse a for over @in';
}

{
    # Live-array growth must still work (RT113026).
    my @a = 1, 2;
    for @a -> $n { @a.push($n + 10) if $n < 3 }
    is-deeply @a, [1, 2, 11, 12], 'live-array growth during for still iterates the new tail';
}

{
    # Shrinking the same-named scalar's array must not truncate the loop.
    my $xs = [1, 2];
    my @xs = |$xs, 3, 4;
    my $n = 0;
    for @xs -> $x { $n++ }
    is $n, 4, 'loop count comes from @xs, not $xs';
}

done-testing;
