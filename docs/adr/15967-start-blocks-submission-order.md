# ADR-15967: mutsu's thread model: what a `start` is, what it costs, and what it does not promise

- **Status**: Accepted (user decision 2026-11-09: §6's four recommendations as written)
- **Revision**: 2026-11-09 widens the first draft (2026-11-08), which asked only "must two
  `start` blocks begin in submission order?" and rested on a premise §2 refutes. A second
  revision the same day withdrew a scheduler change that ADR-0105 §5 had already rejected.
- **Date**: 2026-11-08, revised 2026-11-09
- **Deciders**: tokuhirom (maintainer)
- **Issue**: [#15967](https://github.com/tokuhirom/mutsu/issues/15967); the issue map in §5
  places every open `lane:threads` issue and the related ones.
- **Related**: [ADR-0020](0020-shared-worker-pool.md) (shared worker pool, blocking `await`),
  [ADR-0105](0105-promise-resolution-dispatches-through-the-promise-scheduler.md) (promise
  resolution through the scheduler; no task overtakes its submitter),
  [ADR-0123](0123-bounded-user-thread-stack-reservations.md) (pool growth and stack budget),
  [ADR-12814](12814-concurrency-memory-safety-follows-rakudo.md) (memory safety vs. races),
  [ADR-16595](16595-threads-reach-lexicals-through-their-captured-cells.md) (lexicals reach
  threads through cells), [ADR-12529](12529-frames-resolve-names-through-the-static-link.md)
  (static vs. dynamic link), #10779 (Interpreter subsystems), PLAN.md §5 (non-blocking `await`).

## 1. Context

MCP::Server `t/21-elicitation-dispatch.rakutest`, subtest "A round that answers only some of the
questions", runs a tool whose handler starts two `start` blocks (`alpha`, `beta`) that each ask an
elicitation question, while the dispatcher waits on `Promise.anyof($work, $broker.round-ready)`
and snapshots the pending questions as soon as the first one arrives. The test assumes that both
questions are in that snapshot and that `alpha`'s question got the first key. #15967 asked mutsu
to make two `start` blocks begin in submission order. A first attempt (branch
`fix/15967-start-blocks-begin-in-submission-order`, no PR) gated the pool and halved the failure
rate at a ~20% cost on a burst of tiny tasks, but never reached zero, because the only way to
order the first *statement* is to serialise the bodies.

Rather than decide that one question, this ADR asks what mutsu's `start` and its threads should
be, in three layers: what a program may rely on (§3, contract), what a `start` costs and what
state it runs against (§4.1, execution model), and how blocking and scheduling work (§4.2-§4.3).

## 2. Measurements (2026-11-09)

Release build of main @ 05d87609, 4-core container, raku 2026.09 on the same box.

### 2.1 The MCP::Server test depends on rakudo's pool state

| run | passes |
|---|---|
| `raku t/21-elicitation-dispatch.rakutest` | 10/10 |
| the same file under raku, after `await (^16).map({ start { sleep 0.2 } })` warmed the pool (`EVALFILE`, file unmodified) | **7/10** (the same subtest fails) |
| mutsu | 6/10 |

On a cold pool rakudo has a single general worker. The handler's two `start`s queue behind it;
the handler's `await` frees the worker (a continuation), which runs `alpha` until its own
`await`, then `beta`; and the dispatcher's wake-up is itself a task cued on that worker (ADR-0105
F1), queued *behind* `beta`. FIFO order on one worker, not timing, is what makes it pass. Once
the pool has several workers, rakudo fails it too. The test encodes an implementation state of
rakudo, not a language guarantee.

The #15967 repro (300 rounds of two `start`s pushing `a`/`b` under a lock) says the same:

| situation | rakudo: bad / 300 | threads used | mutsu: bad / 300 | threads used |
|---|---|---|---|---|
| cold pool | 0, 0, 0 | **1** | 5, 7, 7 | 2 |
| pool warmed by 16 sleeping `start`s | **22, 46, 69** | 16 | 11, 43, 18 | 16 |

The first draft's premise (rakudo loses the race less often because its hand-off is faster) is
wrong: with several workers rakudo loses it *more* often than mutsu.

### 2.2 A `start` costs O(program state)

Median first-statement latency of `await start { now - $t }` (#17217):

| situation | mutsu | raku |
|---|---|---|
| baseline | 79-93 µs | 51-59 µs |
| after `EVAL`ing 300 small classes | 131 µs | 31 µs |
| 300 `my` lexicals in the spawning scope | 231 µs | 33 µs |
| 2000 × `await (1,2).map({ start { $k*2 } })` | 0.478 s | 0.173 s |

#15077's "1.5-10 ms vs 0.1-3 ms" was a debug-build figure. The baseline is within 2x; the
scaling is the defect. Its cause is the spawn contract: `clone_for_thread_for_block` →
`spawn_clone` builds a whole `Interpreter` per task, cloning the env and forking the type
registry, module, dispatch, IO and cache state.

### 2.3 That clone is also a semantic divergence

Because each task gets a fork of the world (#17216): a class `EVAL`'d inside a `start` is not
visible to the main thread; a class declared on the main thread after the spawn is not visible to
the running task; an `&f.wrap` done in a task does not affect `f` elsewhere; a handle opened in a
task is `Invalid IO::Handle` on another thread until the promise is awaited (the awaiter then
receives it through `ThreadPromisePayload`). All four work on rakudo.

### 2.4 A pending `await` costs an OS thread

| probe | rakudo OS threads | mutsu OS threads |
|---|---|---|
| nested `await start { ... }`, depth 300 | 4 | 303 |
| 200 `start { await Promise.in(0.3) }` in flight | 9 | 304 |
| 100 × `start { sleep 0.5 }` wall time | 1.40 s (sleep holds a worker; pool ramps by tick) | 0.51 s |
| 100 × `start { await Promise.in(0.5) }` wall time | 0.51 s | 0.51 s |

Rakudo's `await` on a pool worker parks a continuation and frees the worker; `sleep` and `Lock`
hold it. mutsu's `await` holds its OS thread (ADR-0020 §2 fork (a)), so the pool grows by one
thread per pending `await`, within ADR-0123's stack budget. Neither rakudo nor mutsu moved a task
to another thread across `await` in the probe (20/20 resumed on the same thread id).

## 3. Contract: what a program may rely on

As rakudo and the docs (`Type/Promise.rakudoc`, `Language/concurrency.rakudoc`) state it:

- `start BLOCK` is `Promise.start(BLOCK)`: it cues the block on `$*SCHEDULER` and returns a vowed
  Promise that the block's result keeps or its exception breaks; a sunk broken promise nobody
  observes kills the program (6.d).
- The block runs eventually, on some thread, with fresh `$/` and `$!`; dynamic variables resolve
  through the context of the `start` (6.d).
- The block reaches lexicals through its closure's captures (ADR-16595) and shares **one program
  world** (packages, types, routines and their wrappers, handles) with every other thread.
- **Nothing orders the beginning of two tasks, or their bodies, or a task against the thread an
  `await` wakes**, beyond what the program synchronises (`await`, `Lock`, `Channel`, a promise
  hand-off) and the ordering ADR-0105 established (resolution dispatches through the promise's
  scheduler; a worker-submitted task does not overtake its running submitter).
- Memory safety holds for every program; a program racing on one container without
  synchronisation may see torn logical state (ADR-12814).

mutsu adds no guarantee to this. A test that passes on rakudo only because rakudo's pool happened
to have one worker (§2.1) is a test assumption, and mutsu does not chase it: such a guarantee
would make no portable program more correct, and every way to provide it taxes or serialises
every `start` (§1).

## 4. Execution model

### 4.1 One world, long-lived execution contexts, small tasks (the target)

A `start` costs **O(1) in program state**: independent of the lexicals in scope, the declared
types, the loaded modules and the frame depth, with a fixed part within ~2x of rakudo.

The shape that meets it, and that also removes §2.3, is rakudo's:

- **The world is shared.** Packages, the type registry, routines (and their wrappers), the IO
  handle table and module state exist once per process; a write to them is synchronised at the
  structure (ADR-0068's rule, ADR-12814's bar). There is no fork and no handle transfer.
- **A pool worker owns a long-lived execution context**: VM registers, the frame stack, its
  per-thread caches and GC mutator state. It is created when the worker starts, not per task.
- **A task carries only its own context**: the closure (whose captures are cells, ADR-16595), the
  promise, the dynamic link to the `start` point (ADR-12529), fresh `$/`/`$!`, and its output
  target. Between tasks the worker resets what is task-scoped (frames unwound, task-local GC state
  dropped, as `worker_loop` does today).

`clone_for_thread_*` then has no job on the `start` path. This is the decomposition #10779 is
heading for, with the cut drawn as "world vs. execution". It is the prerequisite of §4.2 too: a
suspended task can only be small if a task is small.

Prerequisites, each its own carrier: ADR-16595 (no lexical lane; slice 4 removes the seeding walk
but not the env clone), ADR-12529 (the dynamic link), #10779 (the split), and a synchronisation
audit of every world structure that becomes shared (the type-hook cache of #17149 is the first
instance of that class, already reachable today through shared objects).

### 4.2 Blocking: keep blocking `await` now; continuations are the long-term axis

ADR-0020 chose a blocking `await` on an elastic pool; ADR-0105 showed that rakudo's continuation
is not what orders promise resolution, and PLAN.md §5 keeps non-blocking `await` as a standing axis
"started only on a measured trigger". §2.4 is that measurement: one OS thread per pending `await`
(303 vs 4 at depth 300). It is a resource cost (stack address space, thread creation under
fan-out, ADR-0123's budget), not a wrong answer, and nothing in roast or the known ecosystem
failures needs it.

When it is taken up, the recommended mechanism is **stackful tasks** (each task on its own
pooled stack; `await` on an unresolved promise parks the task and the worker switches to the next
queued one), not a rewrite of the VM into heap frames: mutsu's frames are native Rust frames, and
a stack switch keeps them as they are. What it needs, and why it is not now:

- §4.1 first: a parked task must be a small context over the shared world, not a cloned
  `Interpreter`.
- A switch point discipline: a task may park only where it holds no Rust lock guard and no
  `RefCell` borrow of thread-local state (the same kind of discipline as `gc::block_quiescent`'s
  "touches no `Gc` state"); tasks stay pinned to the worker that started them, so thread-locals
  stay valid (rakudo did not migrate either, §2.4).
- `unsafe` stack switching, under the `docs/security.md` rules (a `// SAFETY:` argument per
  block, guard pages on every task stack, no weakening of any hardening).
- `sleep` and `Lock` stay blocking, as on rakudo; `await`, `react` and `Lock::Async` become the
  suspension points. The `Awaitable` protocol (`get-await-handle`, #15519) is the natural seam.

This ADR records the direction; the decision to start it gets its own ADR.

### 4.3 Scheduling: ADR-0105 stands; no supervisor port

The pool keeps ADR-0020's elastic shape, ADR-0123's growth rule and ADR-0105's D1-D4. In
particular a submit from the main thread (or any non-worker thread) still activates an idle
worker at once.

The first revision of this ADR proposed rakudo-style lazy activation (extra workers only on a
supervisor tick) to reproduce §2.1's cold-pool behaviour. That is withdrawn: ADR-0105 §5 already
rejected porting the supervisor, because it delays every main-thread fan-out and every level of a
nested `start` chain by a tick (rakudo's 1.40 s against mutsu's 0.51 s in §2.4 is that cost), and
§2.1 shows it would buy only a rakudo pool state, not a guarantee. Reproducing rakudo's cold-pool
order would additionally need built-in `await` wake-ups to be queued as pool tasks, which
ADR-0105 §5 also rejected.

## 5. Issue map

| issue | layer | consequence of this ADR |
|---|---|---|
| #15967 (this) | §3 | close: no begin-order guarantee; keep the gate branch as the A/B reference, do not merge |
| #14684 MCP::Server `t/21` | §3 | a test assumption (§2.1): record as an accepted divergence in the ecosystem ledger once its failure shape is pinned, not a mutsu fix |
| #16898 Object::Delayed | §3 + bug | part (1), `nqp::assign(SELF, ...)` on another thread not reaching the captured variable, is a real bug; part (2), winning the timing race, is a test assumption like §2.1 |
| #15077 `start` latency | §4.1 | the baseline is within 2x on release; re-scope or close in favour of #17217 |
| #17217 `start` cost O(program state) | §4.1 | the perf close condition of the execution model |
| #17216 world is a per-thread snapshot | §4.1 | the correctness side of the same change |
| #10779 Interpreter decomposition | §4.1 | gains a concrete cut: world vs. execution |
| #16595 and slices #16596-#16600 | §4.1 prerequisite | unchanged; their completion removes the reason ADR-0020 §1.3 gave for a per-task clone |
| #12204, #15020, #9939, #16417, #16469 | §4.1 prerequisite | lane-era bugs and ADR-12204 slices; resolved by, or superseded by, ADR-16595 |
| #17149 Monad type-hook cache race (memory safety) | §4.1 | first instance of "a shared world structure needs its own lock"; fix independently now, and make the audit part of §4.1 |
| #16806 `cas-int.t`, #16811 SSL `thread-stress`, #7609 `procasync/stress` | memory safety | independent of this ADR (ADR-12814 bar); not caused by the scheduling or the clone |
| #14896 100 ms late wake after a NativeCall | unrelated | a lazy parse/hash on the main thread, not the pool |
| #9932 blocking transform on a `.schedule-on` Supply | §4.2 | becomes simpler with suspension points; stays iceboxed |
| #15519 `Awaitable::Handle` | §4.2 | the `await` protocol; implement it as the seam a future suspension uses |
| PLAN.md §5 non-blocking `await` | §4.2 | the measured trigger now exists (§2.4); the start decision stays a separate ADR |

## 6. Recommendation

1. **Contract (§3)**: adopt it; close #15967 as "no ordering guarantee"; treat MCP::Server `t/21`
   and the race half of #16898 as test assumptions.
2. **Execution model (§4.1)**: adopt "one world, long-lived execution contexts, small tasks" as
   the target of the `start` path and of the #10779 split, with #17217 and #17216 as its close
   conditions. Write the design ADR for the world/execution cut when ADR-16595 slice 4 has landed.
3. **Blocking (§4.2)**: no change now. Record stackful tasks as the chosen mechanism for when
   the non-blocking-`await` axis is opened, and §2.4 as its trigger measurement.
4. **Scheduling (§4.3)**: no change; ADR-0105 stands.

## 7. Consequences

- `start` gains no mutsu-only semantic, so nothing has to be kept forever for portability's sake.
- The `start` path converges on rakudo's model by removing mechanism (the env clone, the world
  fork, the handle transfer), not by adding gates.
- A program that relied on the thread-local world snapshot changes behaviour when #17216 lands;
  rakudo never behaved that way, so no rakudo-portable program does.
- MCP::Server `t/21` stays a nondeterministic `partial` on mutsu, as it is on a warm rakudo; the
  flaky-test policy's quarantine is not used for it (the failure is the test's assumption).

## 8. Rejected

- **Submission-order gates** (with a grace timer, or released at the first statement by a VM
  hook): a guarantee no other Raku gives, a tax on every `start` (+20% measured), and still not
  a guarantee without serialising first statements.
- **Porting rakudo's supervisor / lazy worker activation**: rejected by ADR-0105 §5; §4.3.
- **Helping while waiting** (a blocked `await` runs queued tasks inline on its own stack): the
  waiting frame cannot resume until the inlined task returns, so a task that waits on something
  the buried frame produces deadlocks. Stackful tasks (§4.2) are the sound form.
- **Keeping the per-task `Interpreter` clone as the permanent contract** (ADR-0020 §1.3): that was
  true while lexicals reached threads by name; ADR-16595 removes that reason, and §2.2/§2.3 show
  the clone is both the cost and a divergence.
- **A single-threaded pool for `start` bursts**, or a GIL: removes real parallelism.

## 9. Amendment (2026-11-09): MCP::Server 0.7.2 is not a test target

User decision after acceptance: because `t/21`'s outcome depends on the test environment (§2.1),
the current MCP::Server release (0.7.2) is excluded from the ecosystem sweep and queue rather
than kept as a nondeterministic `partial` (§5's #14684 row and §7's last bullet). The exclusion
is pinned to that release (`MCP::Server@0.7.2` in `ecosystem/exclude.txt`, a version-pinned
entry form added for it), so a new release is measured again automatically.
