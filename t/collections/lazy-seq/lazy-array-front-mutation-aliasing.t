use Test;

# A lazy Array is one shared list: shift/unshift on it reach every holder.
# From JSON::RPC t/00-client.rakutest (a stateful id sequencer built with
# `.assuming(:@ids)` over `[1 .. *]`), #16895.

plan 9;

# 1. A scalar holding a lazy array, and a Pair's value (no variable to rebuild).
{
    my $d = [1 .. *];
    is $d.shift, 1, 'shift on a scalar-held lazy array';
    is $d.shift, 2, 'and again';
    my $p = :a($d);
    $p.value.unshift(0);
    is $d[0, 1, 2].join(','), '0,3,4', 'unshift through a Pair value is seen by the array';
}

# 2. An `@` parameter binds the lazy Array itself.
{
    sub gg(:@a) { @a.shift }
    my @y = [1 .. *];
    is (gg(:a(@y)), gg(:a(@y))).join(','), '1,2', 'named @ parameter aliases a lazy array';
    is @y[0], 3, 'the caller sees both shifts';

    sub hh(@a) { @a.shift }
    my @z = 1 .. *;
    hh(@z); hh(@z);
    is @z[0], 3, 'positional @ parameter aliases a lazy array';
}

# 3. A callable that keeps the array: `.assuming(:@ids)`.
{
    sub sequencer(:@ids) { return @ids.shift }
    sub spec(:@ids = [1 .. *]) {
        my &q = &sequencer.assuming(:@ids);
        (q(), q(), q()).join(',')
    }
    is spec(), '1,2,3', '.assuming over a lazy array advances';
}

# 4. A finite array still copies on assignment.
{
    my @a = 1 .. *;
    my @b = @a[^3];
    @a.shift;
    is @b.join(','), '1,2,3', 'a copy made before the shift is unaffected';
    is @a[0], 2, 'the original advanced';
}
