# A closure call skips re-installing a captured cell the frame already resolves to

A closure call writes each box-on-capture cell (`ContainerRef`) it captured over
whatever the calling frame holds, so a stale value cannot hide the closure's own
cell. When the frame already resolves the name to the very same cell, which is the
common case for an `our` variable or an outer lexical shared with the caller, that
write changes nothing a read can see, and it cost about 1.5k Ir per captured cell per call
(an env insert, key marking and the drop at exit). The call now compares the cell first
and only inserts when the frame answers with something else.

Measured on #7565's loop (callgrind Ir slope between 200 and 1000 iterations,
`MUTSU_JIT=off MUTSU_GC=off`, `profiling` builds of the parent and this branch, the same day):
with 60 mainline `our` variables 146.1k -> 125.3k Ir/iteration (-14%); the `use Test` loop
-0.5k Ir/iteration. The remaining cost of that shape is the per-call snapshot of every
captured free variable that the writeback test compares (a `Symbol -> i64` table and a
`Vec<(Symbol, Value)>` per call), recorded on #7565.
