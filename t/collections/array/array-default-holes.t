use Test;

# #10360: a hole in an `is default(...)` array -- a `:delete`d slot or a gap an
# out-of-range store grew -- reads as the default through every whole-array
# view and iteration, not only through `@a[$i]`; and the hole record itself
# (`:exists`) follows push/append/shift/unshift/splice, copies and iteration.
# Expected values are rakudo's.

plan 42;

# Deleted slot, untyped.
{
    my @m is default(7) = 1, 2, 3;
    @m[0]:delete;
    is @m.values.raku, '(7, 2, 3).Seq', '.values';
    is @m.kv.raku, '(0, 7, 1, 2, 2, 3).Seq', '.kv';
    is @m.pairs.raku, '(0 => 7, 1 => 2, 2 => 3).Seq', '.pairs';
    is @m.map({ $_ }).raku, '(7, 2, 3).Seq', '.map';
    is (for @m { $_ }).raku, '(7, 2, 3)', 'for';
    is @m[*].raku, '(7, 2, 3)', 'whatever slice';
    is @m[0, 1].raku, '(7, 2)', 'list slice';
    is @m[^2].raku, '(7, 2)', 'range slice';
    is "@m[]", '7 2 3', 'zen slice interpolation';
    is @m.reverse.raku, '(3, 2, 7).Seq', '.reverse';
    is @m.rotate.raku, '(2, 3, 7).Seq', '.rotate';
    is @m.unique.raku, '(7, 2, 3).Seq', '.unique';
    is @m.flat.raku, '(7, 2, 3).Seq', '.flat';
    is @m.Seq.raku, '(7, 2, 3).Seq', '.Seq';
    is @m.eager.raku, '[7, 2, 3]', '.eager';
    is @m.clone.raku, '[7, 2, 3]', '.clone';
    is @m.iterator.pull-one, 7, '.iterator.pull-one';
    is (@m Z 1, 2, 3).raku, '((7, 1), (2, 2), (3, 3)).Seq', 'Z';
    nok @m[0]:exists, 'the deleted slot is still a hole';
    is @m.raku, '[7, 2, 3]', '.raku';
}

# A copy holds real containers: the holes become present elements, and the
# source's default does not travel with them.
{
    my @m is default(7) = 1, 2, 3;
    @m[0]:delete;
    my @c = @m;
    is @c.raku, '[7, 2, 3]', 'my @c = @m copies the default into the hole';
    ok @c[0]:exists, 'the copied hole exists';
    is-deeply @c[10], Any, 'the copy does not inherit the default';
    my @e is default(6) = @m;
    is @e[10], 6, 'a target keeps its own default';
    sub f(@a is copy) { (@a[0]:exists, @a[10]) }
    is-deeply f(@m), (True, Any), 'an is copy parameter is a list-assignment copy';
    my @f; @f[2] = 1;
    my @g = @f;
    ok @g[0]:exists, 'a copy of an untyped gap exists';
}

# Grown gaps, untyped and typed.
{
    my @g is default(7) = 1;
    @g[3] = 4;
    is @g.values.raku, '(1, 7, 7, 4).Seq', 'a grown gap reads the default';
    nok @g[1]:exists, 'a grown gap is a hole';
    my Int @t is default(7) = 1, 2, 3;
    @t[1]:delete;
    is @t.values.raku, '(1, 7, 3).Seq', 'typed array, deleted slot';
    nok @t[1]:exists, 'typed array, deleted slot is a hole';
    my @i is default(7);
    @i[2]++;
    is @i.raku, '[7, 7, 8]', 'a gap grown by ++ reads the default';
    nok @i[0]:exists, 'a gap grown by ++ is a hole';
}

# Iteration leaves the array alone.
{
    my @q is default(7) = 1, 2, 3;
    @q[5] = 9;
    for @q { }
    is @q[4], 7, 'a hole still reads the default after for';
    is @q.raku, '[1, 2, 3, 7, 7, 9]', '.raku after for';
    my $s = @q.Seq;
    my $v = @q.values;
    nok @q[4]:exists, 'a hole stays a hole after .Seq / .values';
    my @k = 1, 2, 3;
    @k[1]:delete;
    for @k { }
    nok @k[1]:exists, 'iterating does not vivify a hole';
    for @k { $_ = 5 }
    ok @k[1]:exists, 'writing through the loop variable fills it';
}

# The hole record follows the element moves.
{
    my @a; @a[2] = 1; @a.push(Any);
    ok @a[3]:exists, 'a pushed Any exists';
    my @p is default(7); @p[2] = 1; @p.push(7);
    ok @p[3]:exists, 'a pushed default value exists';
    my @b is default(7) = 1, 2, 3;
    @b[1]:delete;
    @b.unshift(0);
    is-deeply (@b[1]:exists, @b[2]:exists), (True, False), 'unshift moves the hole';
    @b.shift;
    is-deeply (@b[0]:exists, @b[1]:exists), (True, False), 'shift moves the hole';
    @b.splice(0, 1);
    is-deeply (@b[0]:exists, @b[1]:exists), (False, True), 'splice moves the hole';
}
