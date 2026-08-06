use Test;

# A `state` scalar written with plain `=` (not `++`) must accumulate across
# calls. The scalar used to live in the plain state store while the write hit
# only the local slot, so the exit persist read a stale copy and every call
# saw the initializer again. `state` scalars now live in a ContainerRef cell
# like aggregates do, making slot, env and the store one location
# (todo/tickets/state-scalar-plain-assignment-is-lost-across-calls.md).
# Expected values verified against raku.

plan 8;

{
    sub f() { state $n = 0; $n = $n + 1; $n }
    is (f(), f(), f()).join(','), '1,2,3', 'plain = accumulates across calls';
}

{
    sub f() { state $n = 0; $n += 1; $n }
    is (f(), f()).join(','), '1,2', '+= accumulates across calls';
}

{
    sub f() { state $s = ""; $s ~= "x"; $s }
    is (f(), f()).join(','), 'x,xx', '~= accumulates across calls';
}

{
    sub f() { state $n = 0; $n = $n + 1; }
    is (f(), f()).join(','), '1,2', 'implicit return of the assignment accumulates';
}

{
    sub f() { state $n = 0; $n = $n + 1; return $n }
    is (f(), f()).join(','), '1,2', 'explicit return sees the accumulated value';
}

{
    sub f() { state $n = 0; $n = $n + 1; my $r = $n; $r }
    is (f(), f()).join(','), '1,2', 'a plain read after the assignment sees the write';
}

{
    sub f() { state $n = 0; $n++; $n }
    is (f(), f()).join(','), '1,2', '++ still accumulates';
}

{
    # Per-clone identity must survive the cell: a nested named sub
    # re-initializes its state per enclosing call.
    sub outer() {
        sub inner() { state $n = 0; $n = $n + 1; $n }
        (inner(), inner()).join(',')
    }
    is (outer(), outer()).join('|'), '1,2|1,2',
        'nested named sub state still re-initializes per enclosing call';
}
