use Test;

plan 8;

# Concurrent `@a.push` from `start` blocks must not lose updates: each thread's
# push has to land in the single shared array, not clobber a stale snapshot.
{
    my @a;
    my @p;
    for 1..10 -> $i {
        @p.push: start { @a.push($i) }
    }
    await @p;
    is @a.elems, 10, 'all concurrent pushes land';
    is @a.sum, 55, 'no values lost (1..10 sum)';
}

# Larger thread count.
{
    my @a;
    my @p;
    for 1..30 -> $i {
        @p.push: start { @a.push($i) }
    }
    await @p;
    is @a.elems, 30, '30 concurrent pushes all land';
    is @a.sum, 465, '30-thread sum intact';
}

# A parent-seeded array keeps its initial elements.
{
    my @a = 100, 200;
    my @p;
    for 1..5 -> $i {
        @p.push: start { @a.push($i) }
    }
    await @p;
    is @a.elems, 7, 'seeded array keeps its initial elements plus pushes';
    ok 100 (elem) @a, 'initial element preserved';
}

# Concurrent unshift.
{
    my @a;
    my @p;
    for 1..8 -> $i {
        @p.push: start { @a.unshift($i) }
    }
    await @p;
    is @a.elems, 8, 'all concurrent unshifts land';
}

# Single-threaded unshift still prepends after a thread has run (the shared
# fast path used to append for unshift).
{
    await (start { 1 });
    my @c = 1, 2, 3;
    @c.unshift(0);
    is-deeply @c, [0, 1, 2, 3], 'unshift prepends in shared context';
}
