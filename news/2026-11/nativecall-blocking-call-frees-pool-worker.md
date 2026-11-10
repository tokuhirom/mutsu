# A native call that blocks in C no longer starves the worker pool

A `start` worker sitting in a blocking native call (`accept`, `recv`,
`connect`, `usleep` through NativeCall) is now counted as blocked for its
duration, as MoarVM marks a thread blocked around a native call. Before, the
worker pool (ADR-0123) still counted such a worker as running: once the
address-space stack budget was spent, every new task was queued for workers
that would never come back, and the program hung. TCP::LowLevel's
`t/01-basic-compat.t` hit exactly this under the ecosystem sandbox's
`ulimit -v`: each subtest leaves a worker parked in a native `accept`/`recv`,
and the twelfth task — the `Promise.in` timeout of the MD5 failure subtest —
never ran.

The same boundary also counts the thread quiescent for the cycle collector,
so a thread parked in C no longer holds up another thread's stop-the-world.
A `&callback` that C calls back into Raku leaves quiescence for the callback's
body and re-enters it afterwards (`gc::foreign_callback_mutator`), so Raku
code never mutates the object graph while the collector counts its thread
stopped. The libffi call itself is now made by a helper that returns the raw
result, and the `Value` is built only after the thread is a mutator again.

The blocked mark is the pool's and the collector's only: a native call does
not *park* the reacts whose events the thread is handling, so an `emit` keeps
waiting for a `whenever` body that calls `close` or `write` through NativeCall
(ADR-11268), as a Rakudo emitter waits on the react's lock. Net::BGP's
`t/90-basic-bgp.t`, which also used to time out under the sandbox, now passes
too; it depends on that ordering, since its socket's `recv` loop and the
handler would otherwise both close the same descriptor.
