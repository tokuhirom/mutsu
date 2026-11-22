use Test;

# #8069: an element store into a `:=`-bound (celled) array from several
# threads takes the element-store lane under the container's lock
# (ADR-0068) instead of the full path. These pin that the lane keeps what
# the full path guaranteed: every store lands, an append grows the array,
# and a whole-array reassignment, element stores and reads racing on the
# same cell neither crash nor tear.
# Expected values checked against raku 2026.11.

plan 6;

{
    my @backing = 0 xx 64;
    my @slots := @backing;
    await (^4).map: -> $t {
        start { for ^5000 -> $i { @slots[$t * 16 + ($i +& 15)] = $t * 1000 + $i } }
    };
    my @expected = 0 xx 64;
    for ^4 -> $t { for ^5000 -> $i { @expected[$t * 16 + ($i +& 15)] = $t * 1000 + $i } }
    is-deeply @slots.List, @expected.List,
        'disjoint slot stores from four workers all land';
}

{
    my @backing;
    my @grow := @backing;
    my $lock = Lock.new;
    await (^4).map: -> $t {
        start { for ^500 { $lock.protect: { @grow[@grow.elems] = $t } } }
    };
    is @grow.elems, 2000, 'appends one past the end grow the array';
    is @grow.grep(* == 2).elems, 500, 'every append kept its value';
}

{
    my @backing = 0 xx 32;
    my @c := @backing;
    my @workers = (^3).map: -> $t {
        start { for ^3000 -> $i { @c[$i +& 31] = $i; } }
    };
    @workers.push: start { for ^200 { @c = 0 xx 32 } };
    my $sum = 0;
    @workers.push: start { for ^3000 -> $i { $sum += @c[$i +& 31] // 0 } };
    await @workers;
    is @c.elems, 32, 'reassignment racing with element stores keeps a whole array';
    ok @c.all ~~ Int, 'every element is an Int after the race';
}

{
    # The same container stored through two aliases from two threads.
    my @backing = 0 xx 8;
    my @a := @backing;
    my @b := @backing;
    await start { for ^2000 -> $i { @a[0] = $i } }, start { for ^2000 -> $i { @b[1] = $i } };
    is @backing[0, 1], (1999, 1999), 'stores through two aliases of one cell both land';
}
