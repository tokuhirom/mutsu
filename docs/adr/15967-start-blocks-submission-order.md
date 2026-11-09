# ADR-15967: What a `start` is: contract, cost and scheduling (and why submission order is not promised)

- **Status**: Proposed (needs the maintainer's call; recommendation in §5)
- **Revision**: 2026-11-09 widens the first draft (2026-11-08), which asked only "must two
  `start` blocks begin in submission order?" and rested on a premise §2 refutes.
- **Date**: 2026-11-08, revised 2026-11-09
- **Deciders**: the maintainer
- **Issue**: [#15967](https://github.com/tokuhirom/mutsu/issues/15967); related
  [#14684](https://github.com/tokuhirom/mutsu/issues/14684) (same MCP::Server test),
  [#15077](https://github.com/tokuhirom/mutsu/issues/15077) (`start` latency),
  [#17216](https://github.com/tokuhirom/mutsu/issues/17216) (a spawned thread runs on a snapshot
  of the program's world), [#17217](https://github.com/tokuhirom/mutsu/issues/17217) (a `start`
  costs O(program state)), [#16898](https://github.com/tokuhirom/mutsu/issues/16898)
  (Object::Delayed, another cross-thread ordering assumption).
- **Related**: [ADR-0020](0020-shared-worker-pool.md) (shared worker pool),
  [ADR-0123](0123-bounded-user-thread-stack-reservations.md) (pool growth rule),
  [ADR-0105](0105-promise-resolution-dispatches-through-the-promise-scheduler.md),
  [ADR-12814](12814-concurrency-memory-safety-follows-rakudo.md),
  [ADR-16595](16595-threads-reach-lexicals-through-their-captured-cells.md),
  [ADR-12529](12529-frames-resolve-names-through-the-static-link.md),
  #10779 (Interpreter subsystems).

## 1. Context

MCP::Server `t/21-elicitation-dispatch.rakutest`, subtest "A round that answers only some of the
questions", starts two `start` blocks back to back and assumes the first one asks first. Under
mutsu it fails now and then; rakudo has not been seen to fail it. #15967 asked mutsu to make two
`start` blocks begin in submission order. A first attempt (branch
`fix/15967-start-blocks-begin-in-submission-order`, no PR) gated the pool and halved the failure
rate at a ~20% cost on a burst of tiny tasks, but never reached zero, because the only way to
order the first *statement* is to serialise the bodies.

The first draft of this ADR recommended "no guarantee, as rakudo" on the grounds that rakudo has
the same race and loses it less often because its thread hand-off is faster, and named #15077
(start latency) as the mitigation. Before deciding, the question was widened to: what is a `start`
supposed to be in mutsu at all? That needs three separate answers: what a program may rely on
(contract), what a `start` may cost (cost model), and how tasks are mapped to threads
(scheduling policy). §2 is the measurement each answer rests on.

## 2. Measurements (2026-11-09)

Release build of main @ 05d87609, 4-core container, raku 2026.09 on the same box. Scripts:
the #15967 repro plus `$*THREAD.id` bookkeeping, and `await start { now - $t }` latency loops.

### 2.1 Rakudo's "never fails" is a pool-size artefact, not a faster hand-off

The #15967 repro (300 rounds of two `start` blocks that push `a`/`b` under a lock), counting how
many distinct threads ran the tasks:

| situation | rakudo: bad / 300 | rakudo threads used | mutsu: bad / 300 | mutsu threads used |
|---|---|---|---|---|
| cold pool (the repro as written) | 0, 0, 0 | **1** | 5, 7, 7 | 2 |
| pool warmed first by 16 sleeping `start`s | **22, 46, 69** | 16 | 11, 43, 18 | 16 |
| 3 busy background `start`s | 9, 0, 0 | 1-2 | (not run) | |

Rakudo's `ThreadPoolScheduler` starts with one general worker and adds workers from a supervisor
tick when the queue is not draining. A burst of tiny tasks therefore runs on *one* worker, in FIFO
order, and the "race" never happens. Once the pool has several workers (any program that ran a
parallel phase before), rakudo loses the race *more* often than mutsu does (7-23% vs 4-14%).
So: rakudo gives no ordering guarantee, and its common-case behaviour comes from its **growth
policy**, not from hand-off speed.

### 2.2 The start latency is no longer the gap; its scaling is

Median first-statement latency of `await start { now - $t }`:

| situation | mutsu | raku |
|---|---|---|
| baseline | 79-93 µs | 51-59 µs |
| after `EVAL`ing 300 small classes | 131 µs | 31 µs |
| 300 `my` lexicals in the spawning scope | 231 µs | 33 µs |
| 2000 × `await (1,2).map({ start { $k*2 } })` | 0.478 s | 0.173 s |

#15077's "1.5-10 ms vs 0.1-3 ms" was a debug-build figure; on release the baseline is within 2x.
What is wrong is that a mutsu `start` costs more the more program state exists: the spawn builds a
whole `Interpreter` (`clone_for_thread_for_block` → `spawn_clone`), cloning the env and forking the
type registry, module, dispatch, IO and cache state (#17217).

### 2.3 The per-task clone is also a semantic divergence

Because the child gets a fork of the world, changes to the world made after the spawn do not
cross threads (#17216): a class `EVAL`'d inside a `start` is not visible to the main thread, a
class declared on the main thread after the spawn is not visible to the running task, an
`&f.wrap` done in a task does not affect `f` elsewhere, and a handle opened in a task is
`Invalid IO::Handle` on another thread until the promise is awaited (the awaiter receives it by
`ThreadPromisePayload` handle transfer). All four work on rakudo.

## 3. What a `start` is

### 3.1 Contract (what a program may rely on)

As rakudo and the docs (`Type/Promise.rakudoc`, `Language/concurrency.rakudoc`) state it:

- `start BLOCK` is `Promise.start(BLOCK)`: it cues the block on `$*SCHEDULER` and returns a
  vowed Promise that the block's result keeps or its exception breaks; a broken promise that is
  sunk and never observed kills the program (6.d).
- The block runs eventually, on some thread, with fresh `$/` and `$!`; dynamic variables
  resolve through the context of the `start` (6.d).
- The block sees lexicals through its closure capture (ADR-16595) and **the one program world**
  (packages, types, routines and their wrappers, handles) that every thread shares.
- **Nothing orders the beginning of two tasks**, nor their bodies. A program that needs an order
  synchronises (`await`, `Lock`, `Channel`, a `Promise` hand-off). Memory safety and races follow
  ADR-12814.

mutsu promises exactly this, no more: no submission-order guarantee (it would be a guarantee no
other Raku gives, so no portable program could use it, and it costs every `start`).

### 3.2 Cost model

A `start` costs **O(1) in program state**: independent of the number of lexicals in scope, of
declared types, of loaded modules and of frame depth. Its fixed part should be within ~2x of
rakudo's. Today's spawn violates this (§2.2).

The shape that meets it is rakudo's: a pool worker is a long-lived thread that owns a long-lived
*execution context* (VM registers, frame stack, per-thread caches) and runs tasks against the
*shared world*. A task carries only its own context: the closure (whose captures are cells,
ADR-16595), the promise, the dynamic link to the `start` point (ADR-12529), fresh `$/`/`$!`, and
its output target. Per-task reset is bounded by what the previous task left in the execution
context (frames unwound, task-local GC state dropped as today). `clone_for_thread_*` then has no
job left on the `start` path, and the handle transfer in `spawn_callable_promise` goes away.

Prerequisites, each already a carrier of its own: ADR-16595 (no lexical lane), ADR-12529 (the
dynamic link), #10779 (split `Interpreter` into a shared world and per-thread execution state),
#17216 (the world is shared and its writes synchronised as ADR-0068/ADR-12814 require).

### 3.3 Scheduling policy

The pool keeps ADR-0020's shape (one elastic pool, blocking `await`, grow when every worker is
blocked). What changes is when an extra worker joins in: **demand-driven, as rakudo's
supervisor does**. Queued tasks go to the worker(s) already active; another (warm, parked)
worker is activated when the queue has not drained within a short tick, or at once when every
active worker is blocked. Thread *creation* and the keep-alive floor are unchanged; only the
*activation* of parallelism is lazy.

This is a policy, not a guarantee (§3.1 still holds; with a warm busy pool the order is as random
as on rakudo). It is chosen because:

- it reproduces rakudo's observable common case (§2.1), which is the behaviour ecosystem code was
  written and tested against (the ecosystem oracle is rakudo);
- a burst of tiny tasks runs on one warm worker instead of waking one thread per task, the shape
  where mutsu is ~2.8x rakudo (§2.2's burst row, the ADR-0020 ripemd motivation);
- the cost, a ramp of up to one tick before a CPU-bound burst gets its second worker, is the
  same one rakudo pays.

It amends ADR-0123's "grow freely below the soft cap" rule and must be measured before it lands:
the burst row, a 4 × CPU-bound `start` speedup (must stay ≥ 3x on 4 cores), the
`bench-threads` rows, and MCP::Server `t/21` 20/20.

## 4. Options for #15967 itself

- **(a) Submission-order gates plus a grace timer.** Rejected: invents a guarantee (§3.1),
  taxes every `start` (+20% measured), and still is not a guarantee.
- **(b) A VM hook that releases the gate at the first statement or blocking point.** Rejected
  for the same contract reason; it is the only sound form of (a), but serialises CPU-bound first
  statements.
- **(c) No ordering guarantee, as rakudo.** Chosen, with §3.3 as the mechanism that reproduces
  rakudo's common case and §3.2 as the cost model. The first draft's rationale for (c) ("rakudo's
  hand-off is faster, so drive #15077") is withdrawn: §2.1/§2.2 refute it.

## 5. Recommendation

1. Accept §3.1 as the contract: close #15967 as "no ordering guarantee"; its close condition
   (`bad=0` on the repro) is not a goal. Keep the gate branch as the A/B reference; do not merge.
2. Accept §3.2 as the cost model. #17217 (perf, close condition: O(1) in program state) and
   #17216 (wrong answers: the world is shared) carry it; the per-worker execution context gets its
   own ADR once ADR-16595 slice 4 and the #10779 world/execution split are far enough along to
   say which fields are which.
3. Accept §3.3 as the scheduling direction, as an amendment to ADR-0123, implemented behind an
   A/B switch and landed only on the measurements listed there. MCP::Server `t/21` (#14684) is
   closed by it if it passes 20/20, and is a `partial` caused by a test assumption otherwise.
4. Re-scope #15077: the baseline latency is within 2x on release; its remaining gap is #17217.

## 6. Consequences

- No new semantic is added to `start`, so nothing mutsu-specific has to be kept forever.
- The `start` path converges on rakudo's model in all three layers, which removes, rather than
  adds, mechanism: the env clone, the world fork and the handle transfer.
- A program that relied on mutsu's thread-local world snapshot (none is known; rakudo does not
  behave that way) changes behaviour when #17216 lands.
- Until §3.3 lands, MCP::Server `t/21` stays a flaky-looking `partial` (handled by
  `docs/flaky-test-policy.md`, not quarantined: the failure is a test assumption, not a mutsu bug).

## 7. Rejected

- Making the pool single-threaded for `start` bursts outright: removes real parallelism; §3.3's
  tick-driven activation is the version that keeps it.
- Treating the per-task `Interpreter` clone as the permanent `start` contract (ADR-0020 §1.3's
  "`clone_for_thread` is a per-task contract and stays"): that was true while lexicals reached
  threads by name; ADR-16595 removes that reason, and §2.2/§2.3 show the clone is both the cost
  and a divergence.
