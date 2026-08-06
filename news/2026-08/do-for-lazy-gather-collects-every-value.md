# `do for` over a lazy gather collects every iteration's value

The expression form `my @a = do for gather f() { $_ }` returned only the
last iteration's value (`[10]` where raku says `[5 10]`), while the
statement form iterated correctly. Filed during the ADR-0019 C6e-2a work as
`todo/tickets/do-for-over-lazy-gather-drops-first-value.md` (the symptom
read as "the first value is dropped"; it was really "only the top of the
stack survives").

Root cause: the lazy-gather for-loop (`exec_for_loop_lazy_gather_from`,
which iterates gather/sequence/lazy-pipe `LazyList` sources by pulling one
chunk at a time) implemented no `collect` protocol at all. The eager path
(`exec_for_loop_body`) tracks a stack base, pops each iteration's body
value into a collection vector, and pushes one array at the end; the lazy
path just left every iteration's value on the VM stack, so the consumer of
the loop's result saw whichever value happened to be on top — the last one
— and the earlier values leaked into the surrounding stack frame.

Fix: the lazy loop now mirrors the eager collect protocol — a stack base
recorded at entry, each `Ok` iteration's value popped into the collection
(with the surplus truncated), a `LABEL.leave($v)` value joining the
collection, and a single array pushed on exit. `next` contributes no value
and `last` stops the collection, matching raku.

Pinned by `t/do-for-lazy-gather-collect.t` (plain collection, `next`,
`last`, an infinite closure sequence with `last`, and the statement form) —
expected values verified against raku.
