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
