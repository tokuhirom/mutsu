use lib 't/lib';
use Test;
use ModuleStateSub;

plan 6;

# Sequential calls accumulate the module sub's `state` (routine-scoped cell).
is tick(), 1, 'module sub state: first sequential call';
is tick(), 2, 'module sub state: second sequential call';
is tick(), 3, 'module sub state: third sequential call';

# A module sub's `state` cell is SHARED across `start` threads: 100 concurrent
# increments accumulate into one cell, so the post-await call observes 100.
# Before the compiled_fns expansion each worker re-OTF-compiled a distinct body
# with its own cell, so this returned 0/1 instead of 100.
await (^100).map: { start bump() };
is bump(), 100, 'module sub scalar state accumulates across start threads';

# Aggregate `state` (a `state @`) shares its cell across threads too.
await (^50).map: -> $i { start collect($i) };
is collect(999), 51, 'module sub aggregate state accumulates across start threads';

# The shared body still behaves correctly for a plain single-threaded call after
# concurrency has run.
is tick(), 4, 'module sub state continues sequentially after threaded use';
