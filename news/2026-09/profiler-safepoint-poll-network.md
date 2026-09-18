# The safepoint poll network now serves more than one consumer

mutsu already had a poll network reaching every point where a mutator can be interrupted — the
thirteen bytecode dispatch loops, the call/return/nested-run boundaries, the async machinery, and,
through a Cranelift-emitted shim, the JIT's native backedges. It could only ever do one thing: both
the gate and the callee were spelled "GC", so a second consumer could not arm the network without
turning the collector on, and could not learn *where* the mutator was, because the JIT's shim took no
instruction pointer.

ADR-0106 Slice 1 ([#8701](https://github.com/tokuhirom/mutsu/issues/8701)) generalizes it. The gate is
now `vm_poll::armed()` — one cached bool whose value is the union of the consumers' arming states,
computed once at arm time rather than OR'd per poll — and `gc_safepoint(kind)` becomes
`vm_poll::poll(kind, site)`, with GC as its first consumer and the collector's trigger policy unchanged
behind `gc_safepoint_armed`. Every existing call site keeps its `SafepointKind`, and the sites that
have a bytecode instruction pointer now pass it.

The part that needed care is what the JIT does with that ip. It is a compile-time immediate, so
passing it is nearly free — but "nearly free" on every native backedge is the hottest code mutsu
emits, paid by every user who never profiles. So the emitter picks the ip-carrying shim only when the
profiler is armed, and every other run keeps exactly the call it made before. That is sound because
arming is a process-lifetime decision read from the environment at the first poll, while a chunk is
compiled later, at the hotness threshold.

**The disarmed gates are measured, not asserted.** `perf` was unavailable, so gates 1 and 1c were
measured with callgrind, whose `Ir` count is deterministic and load-independent. Against the release
binary of `d559d288` (main immediately before the slice), with `MUTSU_GC=off`: `bench-fib`,
`bench-tak` and `bench-mandelbrot` move by at most +0.085% with the JIT on and +0.047% with it off,
and a fixture driving 4,000,000 native backedges moves +0.069% — all inside the ADR's 0.5%. The full
table is in ADR-0106 §8.1.

Building that last fixture turned up something worth recording. **No Raku loop form puts a backward
jump inside a JIT-compiled range**: `while`, `for`, `loop`, C-style `loop` and `repeat` all compile to
compound opcodes whose body is a separate compiled range, so the range has no backedge of its own, and
the emitted backedge hook never fires for them. The shape that does reach it is `nqp::while`, which
emits a plain backward `Jump` into the enclosing chunk — which is why the gate's 4,000,000-backedge
fixture had to be written in it. Line coverage is a different hook and is unaffected: Slice 3 emits it
at chunk entry, jump targets and line transitions inside the compiled body.

(An earlier revision of this entry drew a further conclusion from the same measurement — that the
JIT's per-line coverage therefore came only from a once-per-body-entry poll. That was wrong; see
`news/2026-09/profiler-exact-line-counts.md`.)
