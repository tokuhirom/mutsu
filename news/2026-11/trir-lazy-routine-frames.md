# A call between TRIR routines pushes its routine-stack entry only when something looks

Every call from one TRIR routine to another used to push a full routine-stack
entry and switch the current compilation unit. That cost ~280 instructions per
call, and in a JSON::Fast decode nothing ever looked at most of those entries.

Now an inner call only records the entry (ADR-8673 stage 1). Ops are dispatched
in two groups:

- The ops proved not to look at the routine stack (arithmetic, slot loads and
  stores, `ordat`, the typed list ops) run in the main loop as before.
- Every other op reaches a separate dispatch through one arm that first pushes
  the recorded entries. Those are calls out of TRIR, `nqp::die`, a user
  `.Bool` or `.Str`, type-name resolution and generic `nqp::` ops.

So an op added later counts as an observer until someone lists it as pure.

`callframe`, backtraces, `CALLER::`, warnings, `EVAL` and wrapped routines see
the same stack as before. A new test checks each of them against rakudo, with
the entries pushed lazily, pushed eagerly (`MUTSU_TRIR_LAZY_FRAMES=0`), and with
TRIR off. `MUTSU_VM_STATS` now also reports how many entries were deferred and
how many had to be pushed.

The microbenchmark's per-call overhead went from 1,293 to 1,016 instructions,
and the 100-record SPDX decode from 61.6 M to 60.3 M. Stage 2 inlines small
callees, which removes the call altogether.
