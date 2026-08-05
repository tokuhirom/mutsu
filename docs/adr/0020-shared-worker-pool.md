# ADR-0020: Shared worker pool — elastic growth, blocking `await`

- Status: **Proposed**
- Date: 2026-08-05
- Context: extracted from PLAN.md §5 via `todo/deep/shared-worker-pool-adr.md` (2026-08-02);
  groundwork measured 2026-07-17 on main `159a30cb0` and re-measured 2026-08-05 on main
  `a85d464a3` (release builds, 12 cores, raku 2026.06 on the same host).
- Related: [ADR-0008](0008-push-based-supply-event-delivery.md) (push supply delivery),
  [ADR-0010](0010-cross-thread-lexical-sharing-scope.md) (spawn-lineage lexical sharing),
  `todo/tickets/digest-ripemd-start-per-block-overhead.md` (the motivating battery gap).

## 1. Context

mutsu has no thread pool: every task spawns a fresh OS thread at each of the **20
`spawn_user_thread` call sites**, and `ThreadPoolScheduler` is a bare type name with a
thread-per-cue implementation behind it. Each spawn pays, per *task*:

1. an OS thread create/destroy with a **256 MiB reserved stack** (`USER_THREAD_STACK_SIZE`,
   `builtins_system.rs` — needed for deep VM recursion);
2. GC mutator registration churn (`enter_mutator_worker` / `preregister_worker_quiescent` on the
   parent, `WorkerGuard` drain + `exit_mutator_worker` on exit — `gc/stw.rs` grew
   `preregister_worker_quiescent` and `notify_worker_exit` *solely* to survive this churn);
3. `clone_for_thread` (`runtime_thread.rs`): flatten + iterate the whole parent env, seed the
   ADR-0010 lineage store, clone the env into the child interpreter.

### 1.1 What that costs (measured)

In-process elapsed time (`now`-clamped, median of 3, quiet machine, 2026-08-05):

| probe | mutsu | raku | ratio |
|---|---|---|---|
| 500 × trivial `start {}` | 0.14s | 0.029s | ~5× |
| ripemd shape: 2000 × `await map { start { $k*2 } }, 1, 2` | 1.50s | 0.089s | ~17× |
| nested `start`+`await`, depth 500 | 1.37s (RSS 305 MB) | 0.026s | ~50× |
| 50 idle `cue(:every(60))` | 52 threads, VmSize **16.7 GB**, RSS 42 MB | 5 threads, VmSize 1.2 GB, RSS 164 MB | — |

The ~17× on the ripemd shape reproduces the Digest ticket's measurement; per `start`,
mutsu pays ~0.3–0.4 ms where raku pays ~20 µs.

### 1.2 Why now — the motivation upgraded from polish to a battery gap

When the groundwork was measured (2026-07-17) this was resource hygiene. Since then the
bundled-`Digest` battery gate turned it into a **§1 (main-effort) blocker**:
`t/ripemd.t` is the one upstream `Digest` file that cannot be whitelisted — ~513s vs raku's
~46s, over the gate's 120s per-file budget — and the measured lever is exactly per-`start`
overhead (~17× raku on the 2-task `await map … start` shape that `rmd160` runs 15,625 times
per MB). See `todo/tickets/digest-ripemd-start-per-block-overhead.md`.

### 1.3 Where the per-start money actually goes (decomposition, 2026-08-05)

Three independent measurements, all on the ripemd shape:

1. **Raw OS thread machinery is only ~10%.** A Rust microbench running the *exact*
   thread-per-task shape (4000 threads, 256 MiB reserved stacks, joined in pairs) costs
   **~155 ms** total (~39 µs/thread; ~95 ms with default stacks). mutsu spends **1.50 s** on
   the same shape — so thread create/destroy + stack reservation account for roughly a tenth
   of the per-start cost.
2. **The per-task `Interpreter` clone is the dominant, env-proportional share.** Adding 200
   do-nothing lexicals to the spawning scope takes the same 4000-start loop from ~1.5 s to
   ~2.7 s (**+80%**): `clone_for_thread` flattens and iterates the whole parent env, seeds
   the ADR-0010 lineage store, and clones the env map, per task.
3. **The `perf` profile agrees** (`profiling` build, dwarf call graphs). Flat: malloc/free
   ~30%, SipHash + `hash_one` + hashbrown insert/clone ~20–25%, `clone_for_thread_excluding`
   the top direct mutsu symbol. Children mode: the `pthread_create`/`clone3` subtree is
   ~10–14% — matching the Rust bench — while the per-*task* work dominates:
   `clone_for_thread_excluding` ~15%, `Registry::clone` ~14%, `drop_in_place<Interpreter>`
   ~14%, `drop_thread_local_gc_state` ~19%, `init_io_environment` ~10% (all of which a
   pooled worker still pays per task).

The load-bearing consequence: **a pool removes the thread-create + stack-reserve + GC
register/exit churn, but `clone_for_thread` is a per-*task* contract and stays** (a pooled
worker cannot reuse the previous task's `Interpreter`; ADR-0010 lineage seeding is per
spawn). The pool alone therefore does NOT close the ripemd gap — it recovers the ~10%
thread-machinery share plus the register churn, and it fixes the resource pathologies
(§1.1's thread-count/VmSize rows). Closing the ripemd gap needs the companion lever:
**make the per-task clone cheap** (lazy/COW env snapshot, or seed-once lineage reuse across
same-lineage spawns). That work is scoped in
`todo/tickets/digest-ripemd-start-per-block-overhead.md` and is deliberately OUT of this
ADR's decision — but the pool is its prerequisite: a warm worker is what makes a cheap
clone observable at all, and the pool's task boundary is where a reusable clone cache
would live.

## 2. The central fork: what does `await` do on a pooled worker?

Rakudo bounds its pool (`max_threads`, 8 × cores = 96 here; 200 × `start { sleep 2 }`
genuinely serializes into 6.1s) yet nested `await` at depth 500 does not deadlock on 96
workers: its `await` is **non-blocking** — `$*AWAITER` takes a MoarVM continuation, parks the
`Promise`, and hands the worker back to the queue.

mutsu's `await` is a blocking condvar wait (`SharedPromise::wait`, `value_async.rs`). With a
*bounded* pool that combination deadlocks: depth-500 nested `start`+`await` pins every worker
on a wait whose resolution needs a worker. The fork:

- **(a) Elastic pool, blocking `await` stays.** Grow on starvation
  (Rakudo-supervisor-style); a blocked worker triggers a new spawn when queued work exists
  and no worker is idle. Nested-await depth N still materializes ~N threads — the same as
  today, not worse — but short-task churn (ripemd, `hyper`, supply callbacks) reuses warm
  workers, and idle `cue(:every)` stops owning threads.
- **(b) Continuation-ify `await`.** Matches Rakudo semantics and caps the pool for real, but
  it is a VM-scale project: every blocking point (`await`, channel receive, lock, sleep,
  supply `react`) must be able to unwind and re-enter VM frames, on a VM whose frames are
  native Rust stack frames. This is the "Semaphore / non-blocking await" axis PLAN.md §5
  already tracks as separate and hard.

## 3. Decision

**Choose (a): one process-wide elastic worker pool with blocking `await`, grown on
starvation, shrunk on idle.** Record (b) as the future path that would *supersede* the
elasticity (not the pool): if non-blocking `await` ever lands, the pool it feeds is this one.

Concretely:

1. **Placement.** The pool lives behind `spawn_user_thread` — the same façade
   (`thread_compat.rs`) that already swaps OS threads for the cooperative queue on wasm32.
   Call sites do not change; `spawn_user_thread(f)` becomes "enqueue task, wake or grow the
   pool". The wasm build is untouched (its cooperative scheduler *is* already a pool of one).
2. **Sizing.** Start at 0 workers (keep startup at ~5ms), grow on demand up to a soft floor
   of `min(cores, 8)` kept-alive workers; beyond that, an idle worker parks for a grace
   period (~1s) and exits. Growth past the floor happens whenever the queue is non-empty and
   every live worker is busy or blocked — the supervisor check that keeps blocking `await`
   deadlock-free. 256 MiB *reserved* stack per pooled worker makes the kept-alive floor an
   address-space budget: 8 × 256 MiB of `VmSize` is fine (raku idles at 1.2 GB VmSize here);
   an *unbounded* keep-alive would not be.
3. **★The task-queue wait must be STW-aware.** An idle pooled worker parked on a raw
   `recv()` is permanently non-quiescent and would starve **every** stop-the-world in the
   process — strictly worse than today. The queue wait uses `gc::wait_until` /
   `stw_aware_wait` (`gc/stw.rs`) so idle workers count as quiescent, exactly like a blocked
   `await` does today.
4. **Task-boundary invariants** (what "reuse a thread" must NOT reuse):
   - `clone_for_thread` stays per-task; the pool ships the cloned `Interpreter` to the
     worker inside the task closure, unchanged.
   - `drop_thread_local_gc_state` (`value/mod.rs`) runs **between tasks**: task N's pending
     DESTROY queue and failure registry must not leak into task N+1 while the thread stays
     GC-registered. `WorkerGuard`'s drop order (drain → unregister → exit) becomes a
     task-boundary rule; the thread-exit path keeps the existing guard.
   - `$*THREAD` / thread-id-derived state must be per-task where Raku semantics demand it
     (a `Thread.start` thread keeps a stable id for its lifetime — `Thread.start` therefore
     does NOT pool; see §3.6).
5. **Stack tiering.** The five `spawn_user_thread` sites that run no user VM code
   (`native_proc_async.rs` ×4, `signal_watcher.rs`) are reclassified to
   `spawn_gc_helper_thread` (default stack) independently of the pool — a free preliminary
   slice. Pooled workers keep the 256 MiB reservation; that is what the keep-alive floor
   budgets for.
6. **What routes through the pool, in slices.**
   - Slice 1: `spawn_callable_promise` (`start` / `Promise.start`) and `cue` one-shots —
     the measured hot spawners.
   - Slice 2: `cue(:every)` becomes a deadline-heap timer entry (`interval_timer.rs`) that
     *enqueues onto the pool* per tick, skipping a tick while the previous iteration still
     runs. This retires `scheduler_run_every_loop`'s thread-per-cue (the 52-thread /
     16.7 GB VmSize row) and honours the timer's contract that actions never run user VM
     code on the driver thread — "enqueue" is exactly its sanctioned escape hatch.
   - Slice 3: supply emitters / socket pumps / hyper-race workers, case by case.
   - Never pooled: `Thread.start` (user-visible thread identity), `Proc::Async` waiters and
     the signal watcher (become gc-helper threads per §3.5).
7. **Churn-only STW machinery gets simpler, not load-bearing.** With a near-constant
   `mutator_worker_count()`, `preregister_worker_quiescent` / `notify_worker_exit` stay
   correct but stop being hot; no change is required, and removing them is NOT a goal of
   this ADR.

## 4. Alternatives considered

- **(b) Continuation-ified `await` first** — rejected as the opening move: VM-scale rework
  of every blocking point for a benefit (a truly bounded pool) that no current battery or
  roast target needs. Recorded as the future ADR that supersedes §3's elasticity.
- **Bounded pool + blocking `await`** — rejected: deadlocks on nested `start`+`await`
  (depth 500 pins all workers), and "bounded but deadlocks" is a correctness regression
  against today's thread-per-task.
- **Do nothing** — rejected: `t/ripemd.t` stays un-whitelistable (§1.2), idle `cue(:every)`
  keeps owning a thread each, and the ADR-0008 follow-up (moving `:every` onto the timer)
  stays blocked — without a pool, a timer tick would have to cold-spawn a 256 MiB worker
  plus `clone_for_thread` per iteration, a regression over the dedicated thread.
- **An async runtime (tokio-style) under the VM** — rejected: user VM code blocks freely
  (sleep, IO, locks); an async executor under a blocking VM is fork (b) with more moving
  parts and a foreign scheduler in the build.

## 5. Consequences

- Per-task cost drops from thread-create + register churn + clone to queue-push + clone on
  the warm path. Honest expectation from §1.3: **~10–15% off the ripemd-shape microbench**,
  not a fix — that benchmark is the regression guard, while `t/ripemd.t` under the 120s
  gate budget is the exit criterion of the *combined* campaign (pool + per-task clone
  slimming, the latter tracked in the Digest ticket).
- Idle `cue(:every)` stops costing a thread + 256 MiB VmSize each (the 52-thread / 16.7 GB
  probe row collapses to timer entries).
- Nested-await depth N still costs ~N threads (unchanged from today, by design). If that
  shape ever matters, fork (b) is the recorded escalation path.
- New failure surface: pool bugs are cross-task state leaks (missed
  `drop_thread_local_gc_state`, a stale `$*THREAD`) and STW starvation from a non-quiescent
  idle wait — §3.3/§3.4 are the review checklist for every pool PR, and both have
  deterministic detectors (`MUTSU_GC_VERIFY`, STW timeout logging).

## 6. Implementation status

- [x] Preliminary slice: reclassify the 5 no-user-code sites to `spawn_gc_helper_thread`.
- [x] Slice 1: pool behind `spawn_user_thread`; `start` / one-shot `cue` pooled; probes re-run
      (release A/B, 2026-08-05: ripemd shape 1.94s → 1.73s (−11%), 500 × trivial `start {}`
      0.190s → 0.172s (−9%), nested-500 unchanged — matching §5's 10–15% expectation).
- [x] Slice 2: `cue(:every)` → timer entry + pool enqueue; retire `scheduler_run_every_loop`
      (2026-08-05: 50 idle `cue(:every(60))` went from 52 threads / 16.7 GB VmSize to
      4 threads / 761 MB. Adds Rakudo's 1ms minimum-resolution clamp + warning, and a
      bounded `.cancel` wait for the in-flight iteration — the pool's wider
      dispatch-to-execution window otherwise let a dead cue's last `cas` resurrect a
      successor cue's same-named lexical through the bare-name atomic lane).
- [ ] Slice 3: supply emitters / socket pumps / hyper-race, case by case.
