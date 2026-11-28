use Test;

# `@a[$i] = $v` into a declared `@` local takes a lean lane (#19285) that
# answers from the slot, the container's own metadata and process latches.
# These pin the shapes it must decline or get right: typed, defaulted and
# readonly arrays, `:=`-bound and rebound arrays, bound elements, append past
# the end, holes, aliasing through captures and threads, and the store's
# result value.

plan 24;

{
    my @a = 0 xx 4;
    @a[1] = 5;
    is-deeply @a, [0, 5, 0, 0], 'in-range store';
    @a[4] = 9;
    is-deeply @a, [0, 5, 0, 0, 9], 'store one past the end appends';
    @a[7] = 1;
    is @a.elems, 8, 'store past the end extends the array';
    nok @a[6].defined, 'the gap is a hole';
    is (@a[2] = 3), 3, 'the store evaluates to the stored value';
}

{
    my @a;
    for ^5 -> $i { @a[$i] = $i * 2 }
    is-deeply @a, [0, 2, 4, 6, 8], 'stores into an empty array in a loop';
    loop (my $j = 0; $j < 100; $j++) { @a[$j +& 3] = $j }
    is-deeply @a, [96, 97, 98, 99, 8], 'overwrites in a C-style loop';
}

{
    my Int @t = 1, 2;
    @t[0] = 7;
    is-deeply @t, Array[Int].new(7, 2), 'typed array takes a fitting value';
    throws-like { @t[1] = 'x' }, X::TypeCheck::Assignment, 'typed array rejects a wrong value';
}

{
    my @d is default(42) = 1, 2;
    @d[1] = Nil;
    is @d[1], 42, 'a Nil store into an `is default` array reads the default';
    @d[0] = 5;
    is @d[0], 5, 'an `is default` array takes a plain store';
}

{
    my @backing = 0 xx 4;
    my @alias := @backing;
    @alias[2] = 'z';
    is @backing[2], 'z', 'a store through a `:=` alias reaches the original';
    @backing[3] = 'w';
    is @alias[3], 'w', 'and the other way round';
}

{
    my @a = 1, 2;
    my @b = 10, 20;
    @a := @b;
    @a[0] = 99;
    is @b[0], 99, 'a store into a rebound array reaches its new container';
}

{
    my @a = 1, 2;
    my @b := @a;
    my @c = 7, 8;
    @a := @c;
    @b[0] = 'b0';
    is-deeply @b, ['b0', 2], 'a store through an alias whose source was rebound keeps its node';
    is-deeply @a, [7, 8], 'and leaves the rebound source alone';
}

{
    my @a = 1, 2, 3;
    my $x = 0;
    @a[1] := $x;
    @a[1] = 'bound';
    is $x, 'bound', 'a store into a bound element writes through to the variable';
}

{
    my @a = 1, 2, 3;
    my $r = @a;
    @a[0] = 'seen';
    is $r[0], 'seen', 'an itemized reference sees the store';
    my &f = { @a[2] = 'closure' };
    f();
    is @a[2], 'closure', 'a closure store is seen by the declaring scope';
}

{
    my @a = 0 xx 4;
    await start { @a[1] = 'worker' };
    is @a[1], 'worker', 'a worker store is seen by the main thread';
    @a[2] = 'main';
    is (await start { @a[2] }), 'main', 'a main store is seen by a worker';
}

{
    my @a = 1, 2;
    @a[0] = [3, 4];
    is @a[0].elems, 2, 'an array value is stored as one itemized element';
    @a[1] = (5, 6).Seq;
    is @a[1].elems, 2, 'a Seq value is stored as one element';
}

{
    my @a = 0 xx 3;
    my $n = 0;
    @a[$n++] = 'first';
    is-deeply @a, ['first', 0, 0], 'the index expression is evaluated once';
}
