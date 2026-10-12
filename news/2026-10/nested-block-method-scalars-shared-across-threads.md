# A nested-block method's scalars are shared across threads

`class Cnt { { my $n = 0; method inc { ++$n } } }; await start { Cnt.inc }; say Cnt.inc` now
prints `2`, as Rakudo does (#13044). A method declared in a nested block of a class or role body
used to keep that block's scalars per thread. A worker's write went into the worker's own copy
of the method table and was lost to every other thread.

When the block runs, its scalars become shared container cells, as #12825 did for a body's
top-level statics. The block's methods and every thread's copy of the method table share one
cell, so a write made anywhere is seen everywhere, and `⚛++` on an `atomicint` stays atomic.
The class body also stops recording the block's stale plain copy of such a name as a body
static, because method code would read that copy before the cell.
