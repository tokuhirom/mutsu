use Test;

plan 3;

# Two sibling `start` blocks each declaring their own `my @a` must not merge
# through the `__mutsu_atomic_arr::@a` shared-store lane. A nested spawn
# inside the worker (here, `await fetch(...)`, which itself does a `start`)
# drops the worker's redeclared-name mask, so `push @a, ...` routes through
# the name-keyed atomic lane -- and that lane used to resolve unconditionally
# at the process ROOT store, so both threads' unrelated `@a` bindings funneled
# into ONE entry and each thread's final read saw the other's pushes
# interleaved in. The lane must resolve at the lineage that owns `@a` (the
# thread that declared it), not at root.

sub fetch($n, $i) { start { sleep 0.01; "$n$i" } }

{
    my $pa = start {
        my @a;
        for 1..5 -> $i { push @a, await fetch('A', $i) }
        @a.join(',')
    }
    my $pb = start {
        my @a;
        for 1..5 -> $i { push @a, await fetch('B', $i) }
        @a.join(',')
    }
    is await($pa), 'A1,A2,A3,A4,A5', 'thread A pushes only its own array';
    is await($pb), 'B1,B2,B3,B4,B5', 'thread B pushes only its own array';
}

# A genuinely shared array (declared by the parent, pushed to by children)
# must still merge -- the fix must not regress the intentional root-scoped
# case (see t/lock.t).
{
    my @shared;
    await do for 1..4 -> $i {
        start { @shared.push($i) }
    };
    is @shared.sort.join(','), '1,2,3,4',
        'a parent-declared array shared with children still merges their pushes';
}
