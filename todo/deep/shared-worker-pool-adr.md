# Write a Proposed ADR for a shared worker pool

Extracted from PLAN.md §6 (2026-08-02). The groundwork below was measured 2026-07-17 on release
builds of main `159a30cb0`, 12 cores, raku 2026.06 on the same host. The deliverable is a *Proposed
ADR*, not an implementation — the central question is a design fork, not a tuning parameter.

## The gap

mutsu has no thread pool at all: it spawns a thread per task at each of the **19 `spawn_user_thread`
sites**, and `ThreadPoolScheduler` is a bare type name in `runtime_init.rs` with nothing behind it.
What that costs:

| probe | mutsu | raku |
|---|---|---|
| 500 × trivial `start {}` | 0.232–0.262s | 0.051–0.07s |
| 50 idle `cue(:every(60))` | RSS +20.7 MB, **VmSize +16.4 GB**, threads 2→**52** | RSS +4.3 MB, VmSize +25 MB, threads 2→**5** |
| 200 × `start { sleep 2 }` | 2.09s (unbounded concurrency) | 6.1s (bounded, 3 batches) |
| nested `start`+`await`, depth 500 | 0.99s (500 real OS threads) | **0.12s** |

## The central question: what does `await` do to a pooled worker?

raku's `max_threads` defaults to 96 here (8 × cpu-cores) and genuinely-blocking tasks *do* serialize
against it (200 × `sleep 2` takes 6.1s, not 2s) — yet nested `await` at depth 500 does **not**
deadlock on those 96 workers: Rakudo's `await` yields a MoarVM continuation (`$*AWAITER`) and hands
the worker back.

mutsu has no continuations, so a **bounded pool + blocking `await` deadlocks** (depth-500 pins every
worker). The ADR must choose between:

- **(a) an elastic pool** that grows on starvation, Rakudo-supervisor-style — which still re-explodes
  to ~500 threads on that shape, so it wins for idle `cue` / short tasks but not there; or
- **(b) continuation-ifying `await`** — a VM-scale project.

## Other decisions the ADR must record

- **Stack tiering.** `spawn_user_thread` reserves 256 MiB (`builtins_system.rs`) for deep-recursion
  headroom. Five sites (`Proc::Async` ×4, `signal_watcher.rs`) take that stack while running **no
  user VM code** — they only need GC registration, so reclassifying them to
  `spawn_gc_helper_thread` is free. Conversely, 256 MiB *reserved* per pooled worker makes the
  steady-state pool size an address-space decision.
- **Task-boundary invariants.** `clone_for_thread` (`runtime_thread.rs`) is per-*task*, not
  per-thread — a pooled worker cannot reuse the previous task's `Interpreter`. Likewise
  `drop_thread_local_gc_state` (`value/mod.rs`) must run **between tasks**, or task N's pending
  DESTROYs leak into task N+1 while the thread stays registered. `WorkerGuard`'s drop order
  (drain → `mark_thread_registered(false)` → `exit_mutator_worker`) becomes a task-boundary rule
  rather than a thread-exit one.
- **★The biggest correctness risk**: an idle pooled worker parked on a raw `recv()` is permanently
  non-quiescent and would defeat **every** STW in the process — strictly worse than today. The
  task-queue wait must use `stw_aware_wait` / `block_quiescent`.
- **An argument in favour**: `preregister_worker_quiescent` and `notify_worker_exit` (`stw.rs`) exist
  *only* to survive spawn/exit churn; a pool makes `mutator_worker_count()` near-constant and both
  near-moot.
- The shape to mirror is `interval_timer.rs` (leaked `OnceLock` state + one long-lived registered
  driver + actions run with the heap lock released). Its stated contract is that actions must never
  run user VM code on the driver thread, and its escape hatch is "spawn a worker" — exactly where the
  pool slots in.

## What it unblocks

Only once the ADR lands does `cue(:every)` become a timer entry that enqueues onto the pool (skipping
a tick while the previous iteration still runs). Today an `:every` cue owns a thread for its whole
lifetime (`scheduler.rs` → `scheduler_run_every_loop`), which is what the 16.4 GB / 52-thread row
above measures; `:in`/`:at` delays already moved onto the deadline heap in #4638. Moving `:every`
onto the timer *without* a pool would be a regression: every iteration runs user VM code, so the heap
would have to spawn a fresh 256 MiB worker plus a `clone_for_thread` per tick.

Context: [ADR-0008](../../docs/adr/0008-push-based-supply-event-delivery.md) (push delivery; the core
landed in #4636, follow-up slices #4638 / #4639).
