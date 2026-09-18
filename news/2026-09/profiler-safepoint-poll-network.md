# The safepoint poll network now serves more than one consumer

mutsu already had a poll network that reaches every point where a mutator can be interrupted — the
thirteen bytecode dispatch loops, the call/return/nested-run boundaries, the async machinery, and,
through a Cranelift-emitted shim, the JIT's native backedges. It could only ever do one thing: both
the gate and the callee were spelled "GC", so a second consumer could not arm the network without
turning the collector on, and could not learn *where* the mutator was, because the JIT's shim took no
instruction pointer.

ADR-0106 Slice 1 ([#8701](https://github.com/tokuhirom/mutsu/issues/8701)) generalizes it. The gate
is now `vm_poll::armed()` — one cached bool whose value is the union of the consumers' arming states,
computed once at arm time rather than OR'd per poll — and `gc_safepoint(kind)` becomes
`vm_poll::poll(kind, site)`, with GC as its first consumer and the collector's own trigger policy
unchanged behind `gc_safepoint_armed`. Every existing call site keeps its `SafepointKind`; the sites
that have a bytecode instruction pointer (the dispatch backedges, the JIT's native ones) now pass it,
and the rest pass `NO_SITE`.

The interesting part is what the JIT does with that ip. It is a compile-time immediate, so passing it
is nearly free — but "nearly free" on every native backedge is the hottest code mutsu emits, paid by
every user who never profiles. So the emitter chooses between two shims once per process:
`safepoint(interp)`, byte-for-byte the call a native backedge made before this change, and
`safepoint_at(interp, site)`, emitted only when the profiler is armed. That specialization is sound
because arming is a process-lifetime decision read from the environment at the first poll, while a
chunk is compiled later, at the hotness threshold — the arming state is already fixed by the time any
code is generated.

Two things fell out of building the test for it. First, Raku's own loops never produce a backward
jump *inside* a JIT-compiled chunk: every loop form compiles to a compound opcode whose body is a
separate compiled range, so the native backedge poll is reached through `nqp::while`, which emits a
plain backward `Jump` into the enclosing chunk. That is the fixture `tests/jit_diff.rs` uses, and with
it the poll report shows 4000 native polls (200 calls x 20 iterations) carrying the ip of the loop's
backward jump, against zero for the same program run with `MUTSU_JIT=off` — which is what pins those
polls to native code rather than the interpreter loop. Second, the slice ships a deliberately
minimal profiler consumer: `MUTSU_PROFILE=1` arms the gate and prints one `profiler-poll:` line at
exit, in the shape `MUTSU_VM_STATS` established. There is no sampler yet — that is Slice 2
([#8702](https://github.com/tokuhirom/mutsu/issues/8702)) — but the network it will ride now exists,
and the run that does not profile pays exactly what it paid before.
