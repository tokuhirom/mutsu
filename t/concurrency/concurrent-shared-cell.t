use Test;

# Track C (concurrency / shared live cells): a lexical scalar captured and
# mutated by a `start` block must be a SHARED container cell, so increments from
# the spawned threads accumulate into the parent's variable instead of each
# thread mutating its own snapshot copy. mutsu performs the in-place `++` under
# the cell's lock, so the result is deterministic (unlike a plain racy `++`).

plan 7;

# Single start block writes back through the shared cell.
{
    my $counter = 0;
    await (start { $counter++ });
    is $counter, 1, 'single start block increment is visible to parent';
}

# Serialized start blocks accumulate (no race, pure sharing).
{
    my $counter = 0;
    for ^4 {
        await (start { $counter++ });
    }
    is $counter, 4, 'serialized start blocks accumulate into shared cell';
}

# `start` directly in a loop body: all spawned threads share one cell.
{
    my $counter = 0;
    my @p;
    for ^10 { @p.push: start { $counter++ } }
    await @p;
    is $counter, 10, 'parallel start blocks in loop body share one cell';
}

# Canonical reproducer: `start` nested inside an immediately-invoked `map`
# block. The escape signal must bubble up through the (non-escaping) map block
# so the outer `$counter` still becomes a shared cell.
{
    my $counter = 0;
    await (^10).map: { start { $counter++ } };
    is $counter, 10, 'start nested in map block shares the outer cell';
}

# Larger fan-out stays exact thanks to the atomic read-modify-write.
{
    my $counter = 0;
    await (^200).map: { start { $counter++ } };
    is $counter, 200, 'atomic increment keeps a large fan-out exact';
}

# Decrement through the shared cell works too.
{
    my $counter = 100;
    await (^10).map: { start { $counter-- } };
    is $counter, 90, 'shared-cell decrement accumulates';
}

# A non-thread sibling-closure capture (escape analysis baseline) is unaffected.
{
    my $v = 0;
    my @cbs = (^5).map: { -> { $v++ } };
    $_() for @cbs;
    is $v, 5, 'sibling closures still share their captured cell';
}
