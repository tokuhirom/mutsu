# ADR-0123: User-thread stacks are reserved against an address-space budget, and the pool queues rather than grows past it

- **Status**: Accepted (user approval 2026-09-25; implemented with #9377)
- **Deciders**: tokuhirom, Claude
- **Context**: [#9377](https://github.com/tokuhirom/mutsu/issues/9377). Supersedes the growth rule
  of [ADR-0020](0020-shared-worker-pool.md) §3.2 ("grow whenever there are more queued tasks than
  parked workers") and amends [ADR-0100](0100-deep-recursion-raises-on-native-stack-headroom.md)
  point 7 ("every thread that runs user VM code already has the same 256 MiB stack") and its fixed
  16 MiB reserve. Compatible with [ADR-0105](0105-promise-resolution-dispatches-through-the-promise-scheduler.md),
  whose D3/D4 hook the same blocking chokepoint this ADR introduces.

## 1. Context

`Concurrent::PriorityQueue` 0.0.2's `t/02-concurrent.rakutest` starts 64 `start` blocks, each
pushing 500 items. It passes under rakudo, and under mutsu outside the ecosystem sandbox. Inside
the sandbox (`ulimit -v 6000000`, 6 GB of address space) mutsu died:

```
thread 'mutsu-main' panicked at src/runtime/thread_compat.rs:48:33:
failed to spawn worker thread: Os { code: 11, kind: WouldBlock, ... }
```

Three separate things lined up:

1. **Every user-code thread reserved 256 MiB of stack** (ADR-0020 §3.5, ADR-0100 point 7). The
   reservation costs no physical memory, but under `RLIMIT_AS` it is real: 6 GB fits barely twenty
   such stacks next to the heap.
2. **The pool grew on every burst.** ADR-0020 §3.2 grew whenever no worker was idle, because
   mutsu's `await` holds its thread and a queued task may be exactly what a busy worker waits for.
   The rule could not tell a worker that is *blocked* from one that is merely *busy*, so 64
   CPU-bound tasks got 64 threads — 16 GiB of reservations — where rakudo (whose
   `ThreadPoolScheduler` caps at `max_threads`, 8 × cores) queues.
3. **A refused thread was a panic.** `thread_compat::spawn_thread` `.expect()`ed the spawn, so
   `pthread_create`'s `EAGAIN` took down the whole process, with nothing for `try` to catch.

Waiting for `pthread_create` to fail is also the wrong signal on its own: the address space left
at that moment is what the heap needs too, and the next `malloc` failure is a Rust *abort*. (It was
observed while building the regression test: at a few MB of headroom, `memory allocation of 1 bytes
failed` killed the process.) The limit has to be respected *before* the thread is attempted.

## 2. Decision

### D1. Thread stacks are reserved against a budget

`runtime::stack_budget` keeps a count of the stack bytes reserved by live user-code threads and a
budget: **half of the soft `RLIMIT_AS`**, or no budget when the address space is unlimited (the
normal case outside sandboxes and containers with `ulimit -v`). `MUTSU_STACK_BUDGET_MB` overrides
it, for tests and for limits the process cannot see. A reservation is released when its thread
exits. The main thread's stack predates the budget and is not counted.

Half, not all: the rest is for the heap, the JIT, malloc's per-thread arenas and the binary. The
exact fraction is not load-bearing — anything that leaves the heap a comparable share works — and
it is the one number here to revisit with data.

### D2. Stacks come in tiers: 256, 64 and 32 MiB

`STACK_TIERS = [256 MiB, 64 MiB, 32 MiB]`. Optional growth (D3) takes only the full 256 MiB
tier — so which worker a task lands on never changes how deeply it may recurse. A thread that
*has to* exist steps down the tiers, past the budget if it must: the budget is the line for
optional growth, not a hard cap.

ADR-0100's guard keeps working on every tier: its reserve becomes `min(16 MiB, stack / 4)` (8 MiB
on the 32 MiB tier, still above the ~5 MiB worst-case check-interval overshoot of a debug build),
and 32 MiB becomes the smallest guardable stack. On a smaller stack, deep recursion raises its
catchable `Too deep recursion` earlier. Nothing crashes.

### D3. The pool grows by a blocked-aware rule

The pool now counts **blocked** workers: a pool worker inside a task that enters
`gc::block_quiescent` or `gc::wait_until` — the chokepoints every blocking wait in mutsu already
goes through (`await`, channel receive, `sleep`, a join, lock waits) — is blocked until it leaves.
With *unclaimed* meaning "queued tasks no idle or starting worker is about to take":

| state | action |
|---|---|
| no unclaimed task | nothing |
| no worker is **running** (all blocked, or none exist) | grow, `Required` (D2 step-down) |
| fewer than `8 × cores` workers are active (not blocked), and a full stack fits the budget | grow, `Budgeted` |
| otherwise | **queue**: a running worker will come back for it |

The rule is evaluated on every `submit` *and* the moment a worker blocks, so the deadlock
ADR-0020 §3.2 guarded against is still impossible: if a running worker blocks on something only a
queued task can resolve, its own block is what grows the pool. The `8 × cores` soft cap is
rakudo's `max_threads` default, so past it mutsu queues where rakudo queues.

The same burst that died in #9377 now runs on a handful of workers. Under a 3 GB limit, a 64-task
CPU-bound burst uses 8 workers; under `MUTSU_STACK_BUDGET_MB=0` it uses one.

### D4. A refused thread is a catchable error, never a panic

`thread_compat::spawn_thread` returns `io::Result`; the GC registration taken for an unborn worker
is handed back (`gc::abort_unborn_worker`) when the OS refuses it. Callers raise `X::AdHoc`
(`Could not create a new Thread: <reason>` — MoarVM reports a refused thread as an ad-hoc
exception too):

- `start` / `Promise.start`: the pool rejects the task, and its promise breaks with the error;
- `Thread.start` / `Thread.run`, `Supply.Promise`'s drive thread, slang activation and
  `Promise.allof`'s combinator thread: the error is raised at the call.

A pool task is rejected only when a `Required` growth fails — every tier refused by the OS, and
no running worker left to take it. A fire-and-forget task with no waiter to tell (a supply
callback) is dropped with a diagnostic on stderr.

## 3. Consequences

- `Concurrent::PriorityQueue`'s burst, and any `start` burst under `ulimit -v`, completes.
- A burst of CPU-bound `start` blocks no longer spawns a thread per task on an unlimited machine
  either: it caps at `8 × cores` active workers. That is also what rakudo does.
- **Busy-waiting across pool tasks can hang past the cap.** A task that spins, without any
  blocking call, on something only a *queued* task will do is now stuck once the cap (or budget)
  is reached and every worker spins. This is rakudo's behaviour at `max_threads` too, and a
  spin with a `sleep` in it is fine (sleep is a blocking chokepoint). ADR-0105 D4's 10 ms tick,
  when it lands, is the natural place for a spin-escape if one is ever needed.
- **Stack depth is no longer uniform across user threads** (ADR-0100 point 7). It is uniform for
  every thread created by optional growth, and only a thread created under pressure (every
  worker blocked, over budget; or an explicit `Thread` over budget) gets a smaller stack.
- **Remaining panics.** The default-stack service threads (timer driver, socket pumps,
  `Proc::Async` readers, signal reader, `IO::Path.watch`) still `.expect()` their spawn through
  `spawn_gc_helper_thread`. They reserve a few MiB each and are not what exhausts an address
  space, but their call sites should move to `try_spawn_gc_helper_thread` as they are touched.

## 4. Alternatives considered

- **Smaller stacks for everyone.** Moves the cliff (a 256-task burst hits the same limit at
  64 MiB) and costs every program recursion depth for a problem only limited address spaces have.
- **Wait for `pthread_create` to fail, then degrade.** Rejected in §1: the heap needs that same
  headroom, and a failed allocation is an uncatchable abort.
- **Queue whenever the budget is spent, regardless of blocked workers.** Deadlocks nested
  `start` + `await` (every worker blocked on a queued task); D3's `Required` row is what prevents
  it.
- **Cap the pool at `cores`.** Tighter for CPU-bound bursts, but it widens the busy-wait hazard
  above from rakudo's threshold to a much lower one; `8 × cores` matches the reference.
- **A supervisor thread (rakudo-style) that grows on stalled queues.** Unneeded for correctness:
  every blocking wait is already observed at the chokepoint. ADR-0105 D4 plans a 10 ms tick for an
  unrelated ordering reason; it can take over that role if the busy-wait case ever matters.
