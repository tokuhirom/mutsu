# S17 scheduler / supply drive model

> **Status:** DRAFT (2026-06-28). This document captures the design work needed
> for the remaining S17 cluster that `TODO_roast/BLOCKERS.md` now splits into
> shared scalar coherence, scheduler lifecycle, Supply timer-driven operators,
> and async IO. The unifying question is: **who owns event delivery, and when is
> user code allowed to run?**

## 0. Scope

This document is about the runtime drive model behind:

- `Scheduler.cue`, `:in`, `:at`, `:every`, `:times`
- `start` / Promise completion / uncaught async exceptions
- timer-driven Supply operators such as `batch(:seconds)`
- `Proc::Async` and socket supplies as event sources

This document is **not** primarily about:

- GC of Promise/Supply cycles
- container identity in ordinary single-thread code
- parser issues in S17 tests

## 1. Why a dedicated design is needed

Current S17 failures look unrelated on the surface:

- `Semaphore.acquire/release` around a shared scalar races
- `$*SCHEDULER.uncaught_handler` is incomplete
- `batch(:seconds)` is flaky because flush is emit-driven
- `Proc::Async` and socket supplies still have uneven bridges

But these collapse onto one runtime question:

> what is the authoritative path from **event source** to **scheduled callback**
> to **shared-state reconciliation**?

Today mutsu has working islands, but they do not yet share one drive model.

## 2. Current substrate

Useful pieces already exist.

### 2.1 Scheduler / cancellation substrate

- `src/runtime/native_methods/scheduler.rs`
  - parses `:in`, `:at`, `:every`, `:times`
  - owns `Cancellation`
- `src/runtime/native_methods/state_scheduler.rs`
  - stores global `uncaught_handler`

This means the public scheduler surface is present. The issue is lifecycle and
integration, not the total absence of a scheduler.

### 2.2 Shared-state reconciliation substrate

- `src/runtime/resolution_eval.rs::call_protect_block`
- `src/runtime/runtime_shared_vars.rs::sync_shared_vars_to_env`
- `shared_vars_dirty`

This is the model that currently makes `Lock.protect`-style critical sections
reconcile shared scalar state correctly. Semaphore does not yet participate in
the same model.

### 2.3 Supply / async source substrate

- `src/runtime/native_supply_mut_methods.rs`
- `src/runtime/native_proc_async.rs`
- `src/runtime/native_methods/socket_async_conn.rs`
- global registries in `src/runtime/native_methods/state*.rs`

The system already has:

- event channels
- supply ids
- background threads for external sources
- per-source callback bridges

The missing part is a more explicit ownership model for delivery and flushing.

## 3. Root cause

The runtime currently mixes three styles of progress:

1. **synchronous user-driven progress**
   - the next emit / method call happens, so work gets flushed
2. **background thread progress**
   - external source threads push events into channels
3. **scheduler-owned progress**
   - timed callbacks should fire because time elapsed

Many bugs come from using style 1 where style 3 is required.

Examples:

- `batch(:seconds)` only flushes when a later emit arrives
- detached task failures do not always flow through one authoritative reporting path
- semaphore critical sections use the primitive, but not the same shared-state
  reconciliation protocol as `Lock.protect`

## 4. Design target

### 4.1 Normalize around event source -> queue -> callback

Every async source should conceptually feed:

1. an event source
2. a queue/channel
3. a scheduler-owned callback runner
4. a reconciliation boundary before/after user code

This does not mean "everything must run on one OS thread". It means the
**ownership boundaries** must be the same.

### 4.2 Separate time-driven work from emit-driven work

A Supply operator with `:seconds` in its semantics is not allowed to depend on a
future emit to make progress.

That means timer-driven operators need a scheduler cue, not just bookkeeping on
the next value arrival.

### 4.3 Reuse one reconciliation model for async critical sections

`Semaphore.acquire/release` should not invent a second coherence protocol if
`Lock.protect` already has the right shape.

The target is:

- enter critical section
- reconcile shared state into the current runtime
- run user code
- write back modified shared state

Semaphore may differ in ownership and blocking behavior, but not in the user
visible coherence contract.

## 5. Four subtracks

## 5.1 Shared scalar coherence

Problem:

- thread-cloned lexical `$x` can be stale in a semaphore-guarded region

Current code entry points:

- `src/runtime/native_methods/concurrency.rs`
- `src/runtime/resolution_eval.rs::call_protect_block`
- `src/runtime/runtime_shared_vars.rs::sync_shared_vars_to_env`
- `src/runtime/native_methods/state_lock.rs`

Design direction:

- do not special-case semaphore mutations ad hoc
- instead, make semaphore-guarded user code pass through the same reconciliation
  shape as protected lock code

First slice:

- factor a reusable "async/shared critical section" helper from
  `call_protect_block`-adjacent logic
- use it for semaphore acquire/release blocks

Exit criterion:

- semaphore-guarded read-modify-write on captured scalars is stable without
  relying on explicit `Lock.protect`

## 5.2 Scheduler lifecycle / uncaught failure path

Problem:

- detached tasks, cue callbacks, and async exception reporting are not yet fully
  unified

Current code entry points:

- `src/runtime/native_methods/scheduler.rs`
- `src/runtime/native_methods/state_scheduler.rs`
- `src/runtime/methods_promise.rs`
- `src/runtime/builtins_system_async.rs`

Design direction:

- define one canonical path for async callback failure:
  callback throws -> task/supply context captures -> uncaught handler or promise
  break path runs
- avoid separate "thread happened to print" behavior

First slice:

- finish `$*SCHEDULER.uncaught_handler` semantics for detached task failures
- ensure start-thread exceptions flow through the same reporting surface

Exit criterion:

- `start`, `cue`, and scheduler-driven callbacks use one failure reporting model

## 5.3 Supply timer-driven operators

Problem:

- operators like `batch(:seconds)` are currently emit-driven in places where the
  spec expects time-driven progress

Current code entry points:

- `src/runtime/native_supply_mut_methods.rs`
- scheduler cue APIs
- supply registries in `state_supplier` / related state modules

Design direction:

- split operator state into:
  - value accumulation state
  - timer/cancellation state
- timer flushes should be scheduled explicitly, not inferred from the next emit

First slice:

- `batch(:seconds)` and `batch(:elems, :seconds)` get an explicit scheduler cue
  that flushes the current window even if no later emit arrives

Exit criterion:

- time-windowed operators progress because time passed, not because an unrelated
  later value arrived

## 5.4 Async IO / Proc::Async / socket supplies

Problem:

- external IO sources already have channels and supply ids, but their bridge
  model is still uneven across process output, sockets, encoding, and shutdown

Current code entry points:

- `src/runtime/methods_object_native_ctors_io.rs`
- `src/runtime/native_proc_async.rs`
- `src/runtime/native_methods/socket_async_conn.rs`
- `src/runtime/native_methods/encoding.rs`

Design direction:

- standardize on:
  - source thread or OS event
  - `SupplyEvent`/channel bridge
  - scheduler-visible consumer path
  - explicit quit/done/error delivery

First slice:

- get `Proc::Async` happy-path fully onto this bridge without encoding
  complexity first
- only then layer `:enc`, per-stream decode, and error-on-invalid-bytes behavior

Exit criterion:

- `Proc::Async` and socket supplies are instances of the same drive model, not
  two unrelated feature clusters

## 6. Cross-cutting rules

These should hold across all subtracks.

### 6.1 User callbacks should run at explicit boundaries

Do not let arbitrary background threads directly "be the runtime". They may
produce events, but runtime callback execution should occur through an explicit
bridge.

### 6.2 Cancellation state must be first-class

Timer-driven operators and detached tasks need cancellable scheduled work.
Ad-hoc booleans buried in operator code make lifecycle bugs hard to reason
about.

### 6.3 Reconciliation should be symmetric

If a callback can observe shared state, then there must be a matching write-back
story after it mutates that state.

## 7. Recommended implementation order

1. **Semaphore/shared scalar coherence**
   - smallest user-visible correctness win
   - directly reuses existing reconciliation ideas
2. **Scheduler uncaught-handler lifecycle**
   - gives one authoritative async failure path
3. **`batch(:seconds)` explicit timer drive**
   - proves the scheduler is not merely a thin wrapper around `sleep`
4. **Proc::Async happy-path bridge**
   - standardize the external-source model before adding encodings and edge cases

## 8. Validation plan

We should not rely on broad roast alone.

### Pins for shared-state coherence

- semaphore-guarded scalar accumulation
- start-thread captured scalar mutation

### Pins for scheduler lifecycle

- detached callback exception reaches uncaught handler
- cancellation prevents later callback execution

### Pins for Supply timing

- `batch(:seconds)` flushes with no trailing emit
- `batch(:elems, :seconds)` does not depend on racey wall-clock alignment

### Pins for async IO bridge

- `Proc::Async.start` -> stdout/stderr -> done/quit path
- socket supply emits data and done/quit through the same scheduler-facing path

## 9. Exit criteria

This track is done when:

- S17 is no longer described as "many unrelated async bugs"
- each remaining async failure clearly belongs to one of the four subtracks
- timer-driven Supply behavior and detached-task lifecycle share one explicit
  scheduler model
