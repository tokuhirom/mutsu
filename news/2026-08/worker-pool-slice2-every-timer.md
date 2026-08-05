# `cue(:every)` cues are timer entries now, not threads (ADR-0020 slice 2)

A repeating `$*SCHEDULER.cue(:every)` no longer owns a dedicated OS thread
sleeping through a repeat loop for its whole lifetime. It is now an entry on
the shared deadline-heap timer (`interval_timer.rs`): each tick enqueues one
iteration onto the ADR-0020 worker pool, skipping the tick while the previous
iteration still runs. `scheduler_run_every_loop` is retired. This closes the
worst resource row in ADR-0020 §1.1: 50 idle `cue(:every(60))` used to cost 52
threads and 16.7 GB of VmSize; they are now 4 threads and 761 MB (raku on the
same host idles at 5 threads / 1.2 GB).

Semantics preserved and one Rakudo behavior gained:

- `:in`/`:at` initial delays, `:times` (exact count), `:stop`, `:catch`, and
  the `:every(Inf)` run-once / `-Inf` special cases all behave as before
  (`roast/S17-scheduler/every.t` stays green).
- Sub-1ms (zero/negative/`-Inf`) intervals now clamp to Rakudo's 1ms minimum
  timer resolution with the same warning ("Minimum timer resolution is 1ms;
  using that instead of Nms"). mutsu previously busy-spun at ~49k iterations/s
  where raku runs ~890/s.
- `.cancel` now waits (bounded, 100ms cap, self-cancel-safe) for an in-flight
  iteration to finish before returning. The pool's dispatch-to-execution
  window is far wider than the retired loop's check-to-call gap, and a
  callback completing after `.cancel` raced the *next* same-named cue's fresh
  lexical through the bare-name atomic lane — a dead cue's final
  `cas $a, {.succ}` resurrected its count into the successor's `$a`
  (intermittent `roast/S17-scheduler/every.t` failures, "seen 21 runs"). The
  underlying bare-name-lane residue is a pre-existing bug that also
  reproduces on thread-per-cue builds (see
  `todo/tickets/cue-loop-lexical-shared-lane-residue.md`); the bounded wait
  restores the old race odds at their source for the cancel path.
