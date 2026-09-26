# ADR-0105: Promise resolution dispatches through the promise's scheduler, and a woken awaiter borrows the resuming worker's slot

- Status: **Accepted** (design final 2026-09-16 — the user delegated the two
  open points to the design session under the premise "we are building the
  fastest Raku interpreter", and §7 records how they were settled;
  implemented 2026-09-26, slices and implementation notes in §8)
- Date: 2026-09-16
- Context: [#8380](https://github.com/tokuhirom/mutsu/issues/8380) (the
  `Test::Time` / `Test::Scheduler` deadlock), whose 2026-09-16 direction
  comment asked for a Proposed ADR scoping
  [ADR-0020](0020-shared-worker-pool.md) §2 fork (b), "continuation-ify
  `await`". This ADR is the result of that design session. Its central
  finding is that fork (b) is the wrong tool for this gap — see §2 and §4.
- Related: [ADR-0020](0020-shared-worker-pool.md) (the pool; §5.1 records
  the correctness upgrade this ADR answers),
  [ADR-0043](0043-scheduled-delivery-hop-belongs-to-the-tapped-supply.md)
  (the Supply-side scheduler hop; the `Supply.interval` ticket in §8 is its
  territory), [ADR-0010](0010-cross-thread-lexical-sharing-scope.md),
  [ADR-0096](0096-batteries-adoption-policy.md) (why patching the
  distributions is not an option).

## 1. Context

`Test::Time` 0.0.2 `t/01-tdd.t` deadlocks under mutsu's default pool (raku
6/6 in 2.5s; mutsu 0/6, 240s sweep timeout), and `Test::Scheduler` 1.2 is
78/83. `SSH::LibSSH`, `SSH::LibSSH::Tunnel` and `Concurrent::Progress` test
against `Test::Scheduler` too. #8380's root-cause comment established, with
gdb thread dumps and an instrumented `Test::Scheduler`, that the awaiting
`start` block resumes *unordered* with respect to the thread that kept its
promise, and that `Test::Scheduler.advance-by` returns before the resumed
block has registered its next `sleep`. It framed the fix as ADR-0020 fork
(b): give `await` a continuation so a parked frame can be resumed inside the
keeper's own scheduling step.

Before scoping that VM-scale rework, this session measured what Rakudo
actually does with an `await`ed promise's resolution. The measurements (all
in Appendix A, reproducible with `raku` 2026.07 and the two vendored
distributions) show that **Rakudo's continuation is not what gives
`Test::Scheduler` its ordering**. Two other properties do, both observable
from user space and both absent in mutsu. Fork (b) would not supply either;
it is neither necessary nor sufficient here.

## 2. What Rakudo actually does (measured 2026-09-16)

**F1 — every promise subscriber is dispatched through
`$promise.scheduler.cue(&callback, :catch)`.** That includes `.then`
callbacks *and* the wake-up of a parked `await`, on a pool worker and on the
main thread alike (Appendix A, experiment 1: all three sections log a `cue:`
call on the logging scheduler between `keep:` and `resumed`). mutsu never
calls `.cue` on resolution: `SharedPromise::keep` broadcasts the condvar the
awaiter parks on and runs `.then` waiters on a pool task (`dispatch_waiters`,
`src/value/value_async.rs`) — a user scheduler cannot see either.

**F2 — the cued callback does not run the awaiter's continuation inline.**
`cue: task end` is logged *before* `await: resumed`: the callback only
re-queues the continuation on the pool. So in Rakudo, "the wake-up cue task
completed" means "the awaiter has been queued", nothing stronger.

**F3 — a task cued from a running pool worker does not start until that
worker yields, or the supervisor's ~10ms tick adds a worker.** With a 50ms
busy submitter and an otherwise idle pool, the cued task started after 14–19ms
in three Rakudo runs; mutsu starts it within 2–8ms (a debug build; the pool
grows immediately on submit). The user-visible consequence (experiment 3): a
`start` block that does `$init.keep; $flag = 1` is observed with `$flag == 1`
by the thread that `await`ed `$init` in 30/30 Rakudo runs, and in 28/30 mutsu
runs. The keeper's straight-line code reliably runs before the thread it woke.

**F4 — the scheduler is a property of the promise.** `Promise.new(:scheduler)`,
`Promise.in(:scheduler)`, a `.then` result promise, and `start` (via
`$*SCHEDULER`) bind it; `.scheduler` reads it; `Supply.interval` under a user
`$*SCHEDULER` calls its `.cue(:every, :in)`. mutsu: `Promise.in` honours only
`$*SCHEDULER` and drops an explicit `:scheduler` argument, there is no
`.scheduler` accessor, and neither `start` nor `Supply.interval` reaches a
user scheduler's `.cue` (Appendix A, probe).

### 2.1 How `Test::Scheduler` builds its guarantee out of F1–F3

`Test::Scheduler.cue` with no delay, called while an event is already being
run, delegates to the wrapped (real) scheduler and records the delegated
task's promise in the dynamic `@*TEST-SCHEDULER-NESTED`; `!run-due` awaits
that list before keeping the event's `$done` promise, and `advance-by` awaits
every `$done` before returning:

```raku
$!wrapped-scheduler.cue({
    my @*TEST-SCHEDULER-NESTED = ();
    to-run();                          # keeps the sleep's Promise.in promise
    await @*TEST-SCHEDULER-NESTED;     # ... whose awaiter wake-up was cued HERE (F1)
    LEAVE $done.keep(True);
});
```

F1 is what makes the wake-up of `await Promise.in(...)` *land in that list at
all*. F2 means the awaited delegate only guarantees "the sleeping block has
been re-queued". The rest — the resumed block running up to its next `sleep`
and registering the next `FutureEvent` before `advance-by` returns — is F3:
with one general worker the re-queued continuation is the queue head, and the
whole wake chain that lets `advance-by` return (delegate ends → `$p.keep` →
`!run-due` resumes → `$done.keep` → main resumes) is queued behind it, each
hop a cue from a running worker. A second worker arrives only on a ~10ms tick.

### 2.2 The mutsu failure, restated in those terms

The trace in #8380 shows two distinct losses. (a) `advance-by(10)` returns
before the block's first `sleep 10` is registered: the `$init.keep` wake-up
reached the main thread immediately (no F3), and main won the race against
the block's straight-line `sleep → Promise.in → cue` — the experiment-3 shape.
(b) After the resume, `advance-by` #2 *and* #3 both return before `sleep 20`
is registered: the wake-up bypassed the scheduler (no F1, so
`@*TEST-SCHEDULER-NESTED` never saw it) and the resumed thread ran
concurrently with the wake chain (no F3). The second registration then lands
after the last `advance-by`, computed against a virtual time that has moved
on, and nothing ever advances to it. The reduction in the issue passes only
because its `advance-by` budget has 20s of slack.

## 3. Decision

Give mutsu F1, a stronger-than-F2 substitute for the continuation, and the
ordering-bearing half of F3. Keep fork (a) and the elastic pool.

### D1. A promise carries its scheduler

`SharedPromise` gains `scheduler: Option<Value>`, holding a *user*
scheduler; `None` means a built-in one and keeps today's deadline-heap /
pool paths (the rationale already recorded on `user_scheduler()` in
`methods_promise_class.rs`: the built-ins are observationally identical and
much cheaper driven natively). It is bound at every site Rakudo binds it:
`Promise.new(:scheduler // $*SCHEDULER)`, `Promise.in` / `.at` (the explicit
`:scheduler` argument honoured, `$*SCHEDULER` otherwise), `start` /
`Promise.start` (under a user `$*SCHEDULER` the body itself is cued through
it, as Rakudo does — `Test::Time`'s `:auto-advance` is written around exactly
that and passes `:scheduler($orig)` to escape it), `.then` / `.andthen` /
`.orelse` result promises (inherit), `Promise.allof` / `.anyof`, and
`Supply.Promise`. `.scheduler` becomes an accessor.

### D2. Resolution dispatches every subscriber through that scheduler

`keep` / `break` drain the waiter list and hand it to the resolving site.
With `None` it is `dispatch_waiters` as today: one pool task, registration
order preserved. With a user scheduler the resolving site — which always has
an `Interpreter` in hand (a user `.keep`, `Promise::Vow.keep`, `start`
completion, `.then` resolution) — calls `.cue(&dispatcher, :catch)` once, the
dispatcher running the drained waiters in order.

`await` on a Planned promise registers a *wake waiter* and parks until that
waiter fires, not until the status flips: the condvar broadcast in `keep`
stops being the wake path. With a user scheduler the wake-up is therefore a
`.cue` hop for every awaiter — main thread included, which is what Rakudo
does (Appendix A, experiment 1, section 2). An already-resolved promise
returns without a hop (Rakudo's `$handle.already`).

**On a built-in scheduler the wake-up is not a pool task.** Nobody can
observe a built-in cue task's completion, so a task exists only to order the
wake behind the keeper's yield — and that ordering (D4) is cheaper to deliver
directly: a keep on a pool worker appends the wake to that worker's
*pending-wake list*, flushed as direct condvar notifies at the worker's next
park, its task end, or the D4 tick; a keep from a non-worker thread notifies
immediately, exactly as today. The `await` hot path thus pays no extra hop
and no extra thread over the current implementation; only `.then`-style
subscribers, which run user code and need a worker, stay pool tasks.

### D3. A woken awaiter borrows the resuming worker's slot — on user-scheduler wake-ups only, unbounded

The wake waiter of a promise that has a **user** scheduler, running inside
the `.cue` task that scheduler dispatched, signals the parked thread and
then blocks — counted quiescent — until that thread next parks in *any*
quiescent blocking wait, or finishes its task. "Parks" is detected at the
chokepoint every blocking wait in mutsu already goes through for
stop-the-world (`block_quiescent` / `stw_aware_wait` in `src/gc/stw.rs`); the
hook fires whether or not `gc_enabled()`. Task end fires it from the pool's
task boundary and from thread exit.

This is the thread-backed substitute for "the continuation runs on the
worker": the awaiter's frames stay on their own thread, and the cue task
lends that thread its turn. Its completion then means "the resumed code
reached its next blocking point" — strictly stronger than Rakudo's F2, and
exactly what closes §2.2(b) *deterministically* rather than by winning a
race (mutsu's interpreted straight-line code is slower than Rakudo's, so the
race it would otherwise have to win is one it loses under load). On wasm32
the rendezvous is a no-op: the cooperative pump is already sequential.

Two scoping decisions, both made under the fastest-interpreter premise (§7):

- **Only user-scheduler wake-ups rendezvous.** A cue task's completion is
  observable only when a user scheduler ran it; that is where the rendezvous
  buys a guarantee. A built-in wake (D2's pending-wake list) signals and
  moves on: lending a worker there would park one thread per in-flight
  resumed awaiter — doubling the thread count of a fan-in burst — for an
  ordering Rakudo does not promise either (with idle workers, its pool runs
  queued work in parallel with a resumed continuation).
- **Unbounded.** The wait ends only at the resumed thread's next park or
  task end. A resumed thread that busy-waits, with no `sleep`/`await`/lock
  in the loop, for something that only happens after the cue task returns
  hangs deterministically rather than flaking; `MUTSU_TRACE=pool` names the
  lent worker and the borrowing thread after 5s. §5 records why a time bound
  was rejected.

### D4. A task submitted from a running pool worker does not overtake its submitter — worker-submitted work only

`submit` from a pool worker with no idle worker enqueues without spawning.
The task starts when the submitter yields: at its next park (the D3
chokepoint — spawn for the whole queued backlog, today's eager starvation
rule, so a worker that fans out and then `await`s gets its children started
at the `await` with no delay) or at its task end (the worker dequeues it
itself). A supervisor tick (10ms, the cadence Rakudo uses; hosted on the
existing timer-driver thread if practical, else a gc-helper thread) spawns
for a CPU-bound submitter that does neither, and flushes D2's pending-wake
lists on the same tick. Submits from a non-worker thread — main, the timer
driver, a `Thread.start` thread — keep today's immediate growth.

This is F3 narrowed to the property that carries the ordering ("nothing
overtakes its still-running submitter"), not a port of Rakudo's supervisor:
main-thread fan-outs (`hyper`/`race` batches, `await map { start … }`) and
nested `start`+`await` chains keep their current parallelism, and the
deferral applies to every kind of worker-submitted work — `cue`, `start`,
`.then` dispatch and D2's wakes alike — because each is a user-visible
ordering of the same class as experiment 3 (`$p.then({ is $flag, 1 })`
after `$p.keep; $flag = 1` is the `.then` spelling of it). It is what turns
§2.2(a) from a 2-in-30 loss into Rakudo's 30/30. The full supervisor was
rejected (§5): it would tax exactly the fan-outs a fast interpreter is
measured on.

### D5. ADR-0020 stands; fork (b) reverts to a perf question

ADR-0020 §3 (elastic pool, blocking `await`) is unchanged. Its §5.1 recorded
that blocking `await` is also a *correctness* gap; this ADR is the answer to
that gap and §5.1 gets a pointer here. Fork (b) — real continuations — goes
back to being the thread-count/perf axis PLAN.md tracks, with no open
correctness driver.

## 4. Why fork (b) is not the fix

- **F2.** Rakudo's continuation does not deliver the ordering; F1 and F3 do.
  A continuation-ified `await` on mutsu's eagerly growing pool would still
  lose experiment 3 and still race §2.2(b).
- **It would still need D1, D2 and D4.** Continuations replace only D3 —
  "run the frames on the worker" instead of "lend the worker to the thread"
  — and D3 is the *smallest* of the four.
- **Cost.** Every blocking point unwinding and re-entering native Rust frames
  is VM-scale (ADR-0020 §2); there is no measured thread-count trigger
  (PLAN.md), and roast has none (`TODO_roast/BLOCKERS.md`).

## 5. Alternatives considered

- **Run the waiters inline on the keeper thread** (synchronous resumption in
  `keep`). Rejected: Rakudo does not (F1/F2); `keep` would block on
  arbitrary user code; a keep under a user `Lock` whose awaiter takes the
  same lock deadlocks; it changes `.then` ordering that #7811 pinned.
- **D1 + D2 without D3.** Rejected as insufficient: §2.2(b) stays a race
  between the resumed thread's straight-line code and a three-hop wake chain,
  which mutsu loses under load.
- **D3 with a time bound.** A resumed thread that busy-waits for something
  that only happens after the cue task returns (a spin on a flag, no
  `sleep`/`await` in the loop) hangs the rendezvous; a bound would degrade
  that to Rakudo's own unordered behaviour after, say, 100ms. Rejected by the
  "sound over fast" rule: a bound makes the ordering guarantee itself
  load-dependent (a resumed block slower than the bound under CI load loses
  it), which is precisely the flaky class this ADR exists to remove. The spin
  shape is a deterministic hang instead, diagnosable via a `MUTSU_TRACE=pool`
  line after 5s. Settled in §7.
- **Rendezvous on built-in wake-ups too** (one invariant, "a resumed awaiter
  always occupies a slot"). Rejected under the fastest-interpreter premise:
  it parks one worker per in-flight resumed awaiter on the hottest
  concurrency path, for an ordering nobody can observe on the built-in pool
  and that Rakudo does not promise there. Settled in §7.
- **Built-in wake-ups as pool tasks** (D2 uniform for every scheduler).
  Rejected: an extra queue hop on every `await` wake-up buys nothing a
  deferred direct notify does not, and costs a worker dequeue per wake.
- **Port Rakudo's whole supervisor** (start at zero workers, grow only by
  tick). Rejected: it also delays main-thread fan-outs and every level of a
  nested `start` chain by ~10ms — ADR-0020 §2's 200 × `start { sleep 2 }`
  taking 6.1s *is* Rakudo paying for that. D4 keeps only the half that
  carries the ordering.
- **Patch `Test::Time` / `Test::Scheduler`, or measure them with
  `MUTSU_POOL=off`.** Banned (ADR-0096's spirit; the issue says so).
  `MUTSU_POOL=off` only passes the reduction by luck — it does not make F1
  exist, and the full `t/01-tdd.t` still has no ordering guarantee under it.
- **One runnable thread at a time** (a GIL-style scheduler). Deterministic,
  but it kills parallelism. Rejected.

## 6. Consequences

- **The `await` hot path is unchanged in hops and threads.** A built-in
  wake-up is a direct notify, deferred to the keeper's yield only when the
  keeper is a pool worker (D2); the `.cue` hop and the D3 lent worker exist
  only on user-scheduler promises. `nested-500` and the S17 suite times are
  the regression guard; the implementing PR measures before/after (numbers
  into documents only from the bench CI).
- **One parked worker per in-flight resumed awaiter of a user-scheduler
  promise** (D3) — the slot Rakudo's continuation would occupy. The queue
  never starves on it: D4's tick and the park-spawn rule cover queued work.
- **Up to 10ms of latency** for a task submitted from a CPU-bound worker
  that neither parks nor finishes — parity with Rakudo (F3).
- **New failure surface.** The D3 hang on a spin-without-park (§5); lock
  order at the chokepoint hooks (never take the pool lock while holding a
  promise's state lock); the supervisor tick must be a gc-helper thread and
  must spawn through `spawn_pool_worker`'s registration protocol; the wake
  waiter must be the *only* wake path or the old broadcast re-introduces the
  race silently.
- **Expected ecosystem effect.** `Test::Time` `t/01-tdd.t` 0/6 → 6/6
  (subtests 3 and 5 need D2–D4, subtest 4 needs D2–D3). `Test::Scheduler`
  stays 78/83 under this ADR: its five failures are two unrelated tickets
  (§8). `Concurrent::Progress` and `SSH::LibSSH` re-measured after S5.

## 7. The two open points, settled (2026-09-16)

The user delegated both to the design session with one premise: mutsu is
being built as the fastest Raku interpreter. Under it the rule is "buy
determinism only where it is observable, and add nothing to the hot path".

1. **D3 is unbounded, and only user-scheduler wake-ups rendezvous.** A time
   bound would make the guarantee load-dependent (flaky), so a spin-without-
   park is a deterministic, traceable hang instead; and lending a worker on
   built-in wake-ups would park a thread per resumed awaiter on the hottest
   concurrency path for an ordering nobody can observe there. A bound remains
   a one-constant change if a real distribution ever hits the spin shape.
2. **D4 covers worker-submitted work only, with a 10ms tick.** The full
   supervisor would tax main-thread fan-outs and nested `start` chains by
   ~10ms per level — the cost Rakudo pays on ADR-0020 §2's 200 ×
   `start { sleep 2 }` row — for no ordering the tests need. Built-in
   wake-ups ride the same yield points as a deferred notify (D2), so the
   ordering comes for free where a keeper yields promptly (the common case:
   a keep is usually the last thing a task does) and costs at most one tick
   where it does not.

## 8. Implementation plan

Slices, each its own PR off `main`; S1–S4 are independent enough to land in
order without stacking.

- **S0 — pin the oracle.** A `t/` test with a logging user scheduler
  asserting F1 (a `.cue` call on `keep` for both an `await` wake-up and a
  `.then`), F4 (`.scheduler`, `Promise.in(:scheduler)`, `start` under a user
  `$*SCHEDULER` cues through it), and the experiment-3 invariant. All fail
  today.
- **S1 — D1.** The field, every binding site, the accessor. *(Landed:
  `PromiseState::scheduler`, `src/runtime/methods_promise_scheduler.rs`,
  pinned by `t/concurrency/promise/promise-scheduler-binding.t`. The
  `start` thunk breaks its own promise rather than passing Rakudo's
  `:catch`, since a synthesized block has no parameter list.)*
- **S2–S5 — D2, D3, D4, verification.** *(Landed together, 2026-09-26.)*
  - D2: `src/value/promise_wake.rs`. A promise's subscriber list holds
    `.then` callbacks and parked awaiters (`Subscriber::Wake(ticket)`) in
    registration order; `wait` parks until its ticket is granted, so the
    condvar broadcast is no longer a wake path. The interpreter-aware
    resolving sites (`.keep`/`.break`, `Promise::Vow`, a cued `start` body,
    `.then`-family results) go through
    `Interpreter::resolve_promise_dispatching`, which cues one
    `Promise::Vow.__mutsu_run_dispatch($id)` block through the user
    scheduler as `.cue(&dispatcher, :catch)`; every other site takes the
    built-in path, so nothing is ever stranded.
  - D2's pending-wake list and D4 live in
    `src/runtime/worker_pool/yield_points.rs`: the yield hook runs from
    `worker_pool::enter_blocking` (reached from `gc::block_quiescent` and
    `gc::wait_until`; `wait_until` now returns without yielding when its
    condition already holds, so an uncontended wait is not a yield) and at
    the pool's task boundary. **Implementation note:** the 10ms tick starts
    deferred *tasks*, but delivers a deferred *wake-up* only once it has
    waited 100ms (`WAKE_GRACE`). Flushing wake-ups on the first tick, as D4
    first described it, lost `Test::Time` subtest 6 in 1 of 9 debug-build
    runs: an interpreted keeper's straight-line code between its `keep` and
    its next `sleep` can outlast 10ms. The tick exists only for keepers that
    never yield, which pay the longer grace.
  - D3: the woken awaiter records the rendezvous in a thread-local list,
    released at its next yield or by `drop_thread_local_gc_state` (task end,
    thread exit). `MUTSU_TRACE=pool` names a rendezvous open for 5s.
  - Verification surfaced two pre-existing cross-thread variable bugs on the
    same path, fixed here: a `start` inside a method seeded its attribute
    aliases (`@!x`, `!lock`) into the bare-name store, so an inline
    `$!lock.protect: { @!x ... }` read a spawn-time snapshot
    (`t/oo/attribute/start-in-method-attribute-alias.t`); and
    `sync_shared_vars_for_names` pulled a re-declared aggregate's stale lane
    entry into a protect block (`Test::Scheduler.run-due` re-queued a
    cancelled `:every` event forever;
    `t/vm/protect-block-redeclared-aggregate.t`).
  - Result: the issue's reduction, `Test::Time` `t/01-tdd.t` (6/6) and all
    three `Test::Scheduler` files pass under the default pool; the S0 oracle
    carries no `todo`.

Unrelated findings to file as `todo:ticket`s (not this ADR's scope):

- `Cancellation.new` / `.cancel` / `.cancelled` as a user-constructible class
  (`Test::Scheduler` tests 56, 57, 62, 63: `No such method 'cancelled'`).
- `Supply.interval` under a user `$*SCHEDULER` must route through its
  `.cue(:every, :in)` (test 55, "Certainly scheduling using virtual time":
  it ticks in real time today, 31s for the file) — ADR-0043 territory.

## Appendix A — the measurements

All run 2026-09-16 on this box: `raku` 2026.07 (moar), mutsu debug build at
`26e8afb7`. Scripts are short enough to be reproduced from here.

### Experiment 1 — who dispatches an awaiter's wake-up

```raku
class LogSched does Scheduler {
    has $.wrapped = $*SCHEDULER;
    method cue(&code, *%opts) {
        note "cue: called on thread {$*THREAD.id} opts=({%opts.keys.sort})";
        $!wrapped.cue({ note "cue: task start"; code(); note "cue: task end" }, |%opts)
    }
    method uncaught_handler(|c) is raw { $!wrapped.uncaught_handler(|c) }
    method handle_uncaught(|c) is raw { $!wrapped.handle_uncaught(|c) }
    method loads(|c) is raw { $!wrapped.loads(|c) }
}
# pooled awaiter
my $s = LogSched.new; my $p = Promise.new(scheduler => $s); my $started = Promise.new;
my $t = start { $started.keep; note "await: parking"; await $p;
                note "await: resumed"; sleep 0.3; note "await: resumed code done" };
await $started; sleep 0.2; note "keep: from main"; $p.keep(1); note "keep: returned"; await $t;
# (the main-thread-awaiter and .then sections are the same shape)
```

Rakudo, pooled awaiter section:

```text
await: parking on thread 4
keep: from thread 1
cue: called on thread 1 opts=(catch)     # F1
keep: returned
cue: task start
cue: task end                            # F2: the task ends BEFORE the resume
await: resumed on thread 4
await: resumed code done
```

The main-thread awaiter section logs the same `cue: called ... opts=(catch)`
before `await: resumed on thread 1`; the `.then` section logs it before
`then: running`. mutsu logs no `cue:` line in any section.

### Experiment 2 — does a worker-submitted task overtake its submitter

```raku
my $t = start {
    my $started = now;
    $*SCHEDULER.cue({ note "cued task started after {(now - $started).round(0.001)}s" });
    my $x = 0; $x++ while now - $started < 0.05;   # busy, never parks
    note "submitter finished busy loop at {(now - $started).round(0.001)}s";
    sleep 0.2;
};
await $t;
```

| run | Rakudo | mutsu (debug) |
|---|---|---|
| 1 | 0.016s | 0.008s |
| 2 | 0.019s | 0.005s |
| 3 | 0.014s | 0.002s |

(Rakudo's "cued task started after" is the supervisor tick plus a spawn;
mutsu's is the immediate spawn.)

### Experiment 3 — the keeper's continuation vs. the woken awaiter

```raku
my $init = Promise.new; my $flag = 0;
my $p = start { $init.keep; $flag = 1; sleep 0.1 };
await $init; print $flag; await $p;
```

30 runs each, one character per run: Rakudo `111111111111111111111111111111`
(30/30); mutsu `011111111110111111111111111111` (28/30).

### Probe — scheduler binding surface (F4)

Under `my $*SCHEDULER = <logging scheduler>`: Rakudo logs `cue opts=(every in)`
for `Supply.interval(10)`, `cue opts=(catch)` for `start { 1 }`, and
`cue opts=(in)` for `Promise.in(5)`, whose `.scheduler` is the logging class;
`Promise.new(scheduler => 42).scheduler` is `42`. mutsu logs only the
`Promise.in` cue, ticks `Supply.interval` in real time, runs `start` on the
pool, and has no `.scheduler` method.
