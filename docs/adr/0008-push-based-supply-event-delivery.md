# 0008. Push-based supply event delivery (ReactWaker sinks)

Date: 2026-07-17
Status: Accepted

## Context

The react/supply drive loop (`drive_react_subscriptions_*`) and every other
supply consumer (control waits, `supply_list_values`, `Promise.anyof`) were
built on **snapshot polling**: consumers repeatedly cloned the supplier
registry state (`supplier_snapshot_seqs`) or slept in 1ms/10ms
`thread::sleep` loops, looking for new values. This architecture — one of the
oldest concurrency mechanisms in the codebase — had four structural defects,
all observed on real workloads (S17 CI flakes, `zef`, Cro-style pipelines):

1. **Busy spin.** A react whose only sources were supplier-backed
   subscriptions had *no blocking point at all*: the loop spun at 100% CPU
   between snapshots (`react whenever $supplier.Supply { }` burned a full
   core for its whole lifetime, and hammered the global supplier-registry
   mutex, slowing the emitting threads too). Under CI load this is what
   pushed `S17-supply/syntax.t` past the 30s roast timeout.
2. **Lost events.** `Supplier.done` fires its callbacks and then
   `supplier_reset`s the registry entry (so the supplier can be reused).
   A polling consumer that had not yet observed the `done` flag — or the
   values emitted just before it — lost them permanently:
   `start { $s.emit(1); $s.emit(2); $s.done }` + `react whenever $s.Supply`
   delivered only `1` and then hung forever. Poll-based readers cannot be
   made sound against writer-side resets.
3. **Quadratic churn.** `supplier_snapshot_seqs` cloned the *entire* emit
   history per delivered value per subscription; a supply carrying 4000
   emits cloned ~16M `Value`s inside the global registry lock.
4. **Latency amplification under load.** Every 1ms/10ms `thread::sleep` poll
   overshoots badly on an oversubscribed host, compounding across the many
   loops involved in one supply pipeline (mutsu ran S17 tests 1.5–2× slower
   than raku under CPU contention, flaking their fixed-sleep timing budgets).

## Decision

Supply events are **pushed to consumers**, never polled:

- A new primitive, `crate::value::waker::ReactWaker`, is a cloneable
  event queue + condvar. Producers `push(key, SinkEvent::{Emit,Done,Quit})`
  or `notify()` (bare wake); the consumer `drain()`s and, when idle,
  `wait_activity(cap)`s (GC-quiescent via `block_quiescent`).
- The supplier registry keeps a list of **sinks** per supplier
  (`supplier_sink_register/unregister`). `supplier_emit/done/quit` push to
  every registered sink *under the registry lock*, so (a) push order equals
  emit order across suppliers, and (b) a later `supplier_reset` cannot
  un-publish an event. Registration replays the currently-buffered values
  (and a pending done/quit) into the queue under the same lock acquisition,
  so nothing falls between replay and subscription. The old cross-supplier
  `emit_seqs` merge machinery is deleted — queue order *is* emit order.
- The react drive loop consumes its waker queue for supplier-backed
  subscriptions, and blocks on the waker when a round makes no progress.
  `SharedPromise` sources wake it via `on_resolve`; `SharedChannel` sources
  via a registered-waker poke on `send/close/fail`. The remaining polled
  sources (mpsc receivers from tap/Proc/zip threads, pending tap-close
  promises) bound the idle wait to 10ms — the same worst-case latency as
  before, with zero spin and instant wake for the dominant sources.
- `supply_list_values`, the throttle control waits, and `Promise.anyof`
  drop their sleep-poll loops for the same sink/`on_resolve` mechanisms.
- `Supply.interval` no longer spawns a sleep-loop OS thread per instance:
  a single process-wide driver thread (`interval_timer.rs`) fires every
  interval off a deadline min-heap, waiting on a condvar until the earliest
  deadline (Rakudo-style single timer queue). S17-supply/syntax.t creates
  2000 short-lived 1ms intervals; per-instance threads turned that into
  ~195k `clock_nanosleep` calls per run (~42k/s), each wake-up paying
  scheduler latency under load. With the shared timer the file makes zero
  `clock_nanosleep` calls on interval-heavy sections.
- `Supply.throttle(0, &block, :control)` runs its block with
  `effective_limit`-way **real concurrency** (worker threads pulling from a
  shared index, delivery serialized per Raku's per-tap guarantee, emission
  order = completion order) instead of serially; the non-block path vents
  its first batch immediately on receiving the control limit (Rakudo
  semantics), doubling the timing margin of throttle.t's fixed-sleep tests.

Lock ordering: supplier registry lock → waker mutex, everywhere (producers
push under the registry lock; the consumer holds only the waker mutex while
draining/waiting and never calls into the registry under it). Waker queues
are GC root containers, visited via the supplier sinks in
`visit_supply_state_roots`.

## Consequences

- `react whenever $supplier` waits at ~0% CPU and wakes in the emit's
  lock-handoff time. The done/emit-loss race is structurally impossible.
- Cross-thread `emit`s before a consumer registers are still delivered
  (replay), matching the previous mutsu semantics that the late-registering
  drive loop depends on (`react { whenever $sup {...}; $s.emit(...) }`).
  Live-supply drops of pre-tap emits (Rakudo semantics) remain a known,
  pre-existing divergence.
- mpsc-receiver sources still poll at a 10ms cap; migrating them onto the
  waker (routing tap channels through sinks) is the natural follow-up if
  their latency ever matters.
- Pinned by `t/react-supplier-done.t`; S17 roast (99 files) is the
  regression net.
