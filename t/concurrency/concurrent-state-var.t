use Test;

# Track C (concurrency / shared live cells), slice 2: a scalar `state` variable
# is shared across threads. A routine's `state $n` lives on the routine, so
# concurrent calls from `start` blocks must accumulate into one shared cell
# instead of each thread initializing its own snapshot. mutsu stores the state
# in a shared `ContainerRef` cell and increments it under the cell lock, so the
# result is deterministic (unlike a plain racy `++`).

plan 5;

# Canonical case: state counter incremented from spawned threads.
{
    sub f { state $n = 0; $n++ }
    await (^3).map: { start f() };
    is f(), 3, 'state $n accumulates across start blocks';
}

# State mutated BEFORE the first thread spawns must carry into the threads.
{
    sub g { state $n = 0; $n++ }
    g(); g();                 # n -> 2 (pre-thread)
    await (start { g() });    # thread: n 2 -> 3
    is g(), 3, 'pre-thread state value carries into the shared cell';
}

# Larger fan-out stays exact thanks to the atomic read-modify-write.
{
    sub h { state $n = 0; $n++ }
    await (^100).map: { start h() };
    is h(), 100, 'atomic state increment keeps a large fan-out exact';
}

# Decrement through the shared state cell works too.
{
    sub d { state $n = 50; $n-- }
    await (^10).map: { start d() };
    is d(), 40, 'shared state decrement accumulates';
}

# A non-threaded state variable is completely unaffected (no cell).
{
    sub plain { state $c = 10; $c++ }
    my @r = (plain(), plain(), plain());
    is @r, [10, 11, 12], 'non-threaded state still increments normally';
}
