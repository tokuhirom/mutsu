# React promise-whenever arming now happens after sink registration: Cro::TCP::Connector.establish delivers, Cro::Core tcp.rakutest 44/44

`Cro::Core`'s `t/tcp.rakutest` hung at test 33 ("Response supply emits a TCP
message") even after the `pending_react_subscriptions` early-break fix
(PR #6124): `Cro::TCP::Connector.establish`'s response supply never delivered
a message to the consuming `react`, which idled forever in
`ReactWaker::wait_activity` with a single registered subscription.

## Root cause: arm-vs-sink-registration ordering, not cross-thread consumer state

The original deep-ticket hypothesis blamed `Interpreter::supply_stream_consumers`
being per-instance (invisible across `clone_for_thread()`). The per-instance
delivery list is real, but it was not the load-bearing bug. The actual failure
was an ordering race in the react drive machinery:

1. A `whenever <Promise>` nested in a `supply { }` body is rewritten into a
   stand-in supplier (`normalize_promise_whenever_markers`), and the promise is
   armed later via `arm_pending_promise_whenevers` — whose `on_resolve` closure
   emits the result into the stand-in and then calls `done` on it.
2. When the promise is **already resolved at arm time** (a loopback TCP connect
   resolves in microseconds; `Promise.kept` immediately), `on_resolve` fires the
   closure **synchronously** — before the drive loop has registered any sink for
   the stand-in supplier (`supplier_sinks_register_batch` runs inside
   `drive_react_subscriptions_inner`, *after* `run_react_event_loop` armed the
   promises).
3. The emit found no sink and was merely buffered in the global supplier state —
   which would still have been recovered by the sink-registration replay, except
   the closure's follow-up `done` runs the Supplier `"done"` handler, which ends
   with `supplier_reset` — **clearing the buffered value and the done flag**.
4. The later sink registration replayed nothing, the `whenever`'s body never
   ran, no inner subscriptions were ever adopted, and the react hung forever.

The trap: this looked exactly like a cross-thread delivery gap in gdb (the
resolution closure runs on a `clone_for_thread()` interpreter with empty
`supply_stream_consumers` and `react_active == 0`), and the previous session's
ticket "confirmed" that diagnosis. The differentiator was found by minimizing:
a socket-free repro with `whenever start { ... }` (promise resolves *after* sink
registration) passed, while the identical shape with an **already-kept** promise
hung — pinning the bug on the arm/sink ordering window, not on which thread the
emit ran on.

## Fix

Arm `whenever <Promise>` stand-ins only **after** their sinks are wired to the
drive loop's waker:

- `run_react_event_loop` (`src/vm/vm_react_loop.rs`) no longer arms before
  entering the drive loop; only the already-finished early-return path still
  arms there (no drive loop will run).
- `drive_react_subscriptions_inner` (`src/vm/vm_react_subscriptions.rs`) arms
  right after `supplier_sinks_register_batch` + waker publication.
- `adopt_newly_registered_subscriptions` (mid-loop nested-`whenever` adoption)
  moved its arm call after the new batch's sink registration for the same
  reason.

With sinks registered first, a synchronously-firing arm closure's emit pushes
straight into the react loop's waker queue (the already-cross-thread supplier
sink registry), and the `done` is delivered as a `SinkEvent::Done` before
`supplier_reset` clears anything. Once `establish`'s outer whenever body runs on
the main react loop, the whole nested chain (transformer, socket supplies) is
adopted by that loop too, so the per-`Interpreter` `StreamConsumer` fast path
sees every downstream `emit` — the cross-thread fallback never needs to fire in
this flow.

## Results

- `Cro::Core` `t/tcp.rakutest`: 44/44, exit 0 (3× stable) — `Cro::Core` is now
  9/9 files.
- New pin: `t/react-whenever-kept-promise-nested-supply.t` (already-kept promise
  guarding a nested live-supplier whenever; direct emit+done from an
  already-kept promise body; late-resolving control case).
- `make test` (28051 tests) and the S17 supply/react roast subset
  (`syntax.t`, `Promise.t`, `basic.t`, `act.t`, `on-demand.t`,
  `subscription-drain-in-react.t`, `then.t`, `start.t`) all pass.

## Note for future work

`supply_stream_consumers` (and `react_active`, `current_react_waker`,
`pending_react_subscriptions`) remain per-`Interpreter` state, invisible across
`clone_for_thread()`. In flows where a supply body's `emit` genuinely runs on a
foreign thread clone with no sink registered for the emitter, the value still
falls back to the global tap registry and can be dropped. No known failing case
remains after this fix (the Cro chain stays on the main loop), but if a future
hang shows an emit on a cloned interpreter to an emitter with no sink, the
consistent fix direction is a global (supplier_id-keyed) consumer registry
mirroring the sink registry — see the analysis preserved in this file's git
history (`todo/deep/stream-consumer-delivery-not-cross-thread-safe.md`).
