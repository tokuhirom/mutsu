# Elastic worker pool: `start` and one-shot `cue` now reuse warm threads (ADR-0020 slice 1)

mutsu's hottest task spawners — `start` / `Promise.start`
(`spawn_callable_promise`) and one-shot `$*SCHEDULER.cue` — now run on a
process-wide elastic worker pool (`src/runtime/worker_pool.rs`) instead of
spawning a fresh 256 MiB-stack OS thread per task. This implements slice 1 of
[ADR-0020](../../docs/adr/0020-shared-worker-pool.md).

The pool follows the ADR's decisions exactly:

- **Elastic growth, blocking `await` stays.** `submit` grows the pool whenever
  there are more queued tasks than parked workers — a busy worker may be
  *blocked* on an `await` the queued task resolves, so waiting for one would
  deadlock. Nested `start`+`await` at depth N still materializes ~N threads
  (same as before, by design).
- **STW-aware idle wait.** The task-queue park runs inside
  `gc::block_quiescent`, so idle workers count as quiescent and never starve a
  stop-the-world (the queue pop provably touches no `Gc` state).
- **Task-boundary hygiene.** `drop_thread_local_gc_state()` runs between tasks
  so one task's pending DESTROY queue / failure registry never leaks into the
  next; a panicking task is caught and the worker survives with correct
  accounting.
- **Sizing.** Starts at 0 workers; a soft floor of `min(cores, 8)` workers
  stays alive; beyond the floor an idle worker exits after a 1s grace period.
  `:every` cues keep their dedicated thread for now — that shape is exactly
  what slice 2 moves onto the deadline-heap timer.
- **Escape hatch.** `MUTSU_POOL=off` restores thread-per-task for A/B runs and
  flake triage. `MUTSU_VM_STATS=1` reports `worker-pool: tasks/spawns/warm_reuses`.

Measured on the ADR's probes (release, median of 3, same host): the ripemd
shape (2000 × `await map { start { $k*2 } }, 1, 2`) improved 1.94s → 1.73s
(−11%) and 500 × trivial `start {}` 0.190s → 0.172s (−9%), matching the ADR §5
expectation of 10–15% — the pool removes thread-machinery and GC-registration
churn, while the dominant per-task `clone_for_thread` cost remains the
companion lever tracked in
`todo/tickets/digest-ripemd-start-per-block-overhead.md`.
