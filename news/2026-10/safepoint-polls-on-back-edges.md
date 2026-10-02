# Safepoint polls moved from every opcode to back-edges and loop iterations

Every bytecode dispatch loop used to poll the VM's safepoint network (the GC consumer, and the
profiler when armed) before **every** opcode: about five polls per iteration of a
`while $i < $n { $i = $i + 1 }` loop, to answer questions whose answers change on a timescale
of milliseconds (#8821). #8833 made each declined poll cheap; this change makes them rare.

With only the GC consumer armed — every default run — a dispatch loop now polls at its entry
and after a **backward** control transfer (an op that leaves `ip` at or before its own index,
such as the plain `Jump(loop_start)` a sunk `nqp::while` compiles to). A compound loop op polls
once per iteration, when its body enters `run_range`; condition and step ranges skip the entry
poll because the body's covers the same iteration. Calls keep their own safepoints. Under
`MUTSU_VM_STATS=1` the new `poll: polls=N` line counts them, and every loop shape now polls
exactly once per iteration.

The stop-the-world bound is argued in `vm_poll::DispatchPolls`: between two polls a dispatch
loop moves `ip` only forward, so it runs at most one chunk of straight-line code, and anything
that repeats does so through a back-edge, a loop iteration or a call, all of which poll.
Writing that argument down turned up a gap that predates this change. The call fast paths'
body loops (`vm_call_fast`, `vm_call_light`, method and closure dispatch, the lazy-pull loops)
never polled at all, so a sunk `nqp::while` inside a called sub ran its whole loop without a
safepoint. Those loops now poll on backward transfers too.

Profile runs keep the old placement exactly. The profiler's exact line counts and ADR-0106
§D4's region attribution both depend on seeing every opcode, so when the profiler is armed
every opcode still polls.

`$*VM.request-garbage-collection` used to get its collect from the per-opcode poll that happened
to run just before it under `MUTSU_GC_EVERY_SAFEPOINT`. It now runs the collect itself, as the
method's documentation says it should ("perform a garbage collect run when possible"), so a
reclaimed cycle's `DESTROY` fires on request in a default run too.

Measured with callgrind on the #8819 loop (`--profile profiling`, second run, 30k vs 60k
iterations): 1,923.0 Ir per iteration on `main` and 1,899.0 after, −1.25%. Most of the
per-poll cost had already gone in #8833. What changes here is how the cost scales: it now
grows with back-edges and calls, not with executed opcodes.
