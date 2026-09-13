# IP::Random passes its ecosystem suite

The `IP::Random` 0.1.0 test suite now passes under mutsu exactly as under
Rakudo: 6/6 files and 20/20 assertions. The fixes cover Hash pair iteration
in `grep`, deferred MapGrep truthiness, flattening batched sequence results,
and aggregate-parameter scope isolation in worker callbacks.

Pinned by `t/collections/hash/hash-predicate-iteration.t`,
`t/collections/transform/flat-seq-nested-array.t`, and
`t/concurrency/race-aggregate-param-shadow.t`.
