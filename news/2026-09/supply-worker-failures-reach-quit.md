# A Supply worker's failure — Rust panic included — now reaches QUIT

A failure raised by a Supply's *producer* code had two ways of going missing,
and issue [#8185](https://github.com/tokuhirom/mutsu/issues/8185) (`PLAN.md` §5)
tracked the worse of them.

**The silent one.** `run_supply_act_loop` is the detached driver every
channel-backed Supply tap runs on, and since ADR-0020 it runs on a pooled
worker. `worker_pool`'s `worker_loop` catches an escaping panic and discards it
by design — a panicking task must not take the worker's `live` accounting with
it. So a Rust panic raised outside the VM's own `run_inner_guarded` frames
(native method plumbing, dispatch helpers) unwound straight past the tap into
that `let _ = catch_unwind`: no diagnostic, no exception, no exit code, just a
Supply that stopped producing while the consumer waited forever. A silently dead
Supply is the worst shape a concurrency bug can take, and it also undercuts
`QUIT` as a mechanism — a handler that cannot observe the most severe failure
mode teaches users not to rely on it.

**The loud-but-misrouted one.** Even a plain `die` in a `supply { }` body
consumed by `react`/`whenever` never reached the subscription's `QUIT` phasers;
it became an `X::React::Died` that killed the whole react. `raku` hands it to
`QUIT` and only dies the react when nothing handles it.

## What changed

The panic boundary is per-worker, not pool-wide. A `catch_unwind` in
`worker_pool::submit` would have changed the failure semantics of every pooled
task — hyper/race batches, throttle workers, `start {}` bodies — to fix one of
them. Instead `run_supply_act_loop` wraps its own user-code dispatch in
`vm::guard_worker_panic`, the same boundary `start {}`/`Promise` workers already
install, so a panic becomes the catchable `X::AdHoc` the VM boundary would have
produced (`Internal error: <payload>`). No new exception type: the mapping
already existed, and the `Internal error:` prefix is what keeps a mutsu bug
distinguishable from a user-level failure once it is sitting in a `QUIT` block.

From there it is an ordinary failure, and three routes were opened for one:

- `run_supply_act_loop` learned a `producer_supplier_id`. It is `Some` at
  exactly one of its four call sites — the branch driving a `supply { }` block's
  own `whenever` body over a live channel-backed source, which is producer code
  rather than a downstream tap handler. A failure there is delivered to the
  enclosing supply's registered `quit =>` handlers through the serialize-group
  link, for the same reason `invoke_supply_done_callback_for_supplier` reaches
  them that way (ADR-0031 Decision A), and the emitter is marked quit so the
  supply's other sinks and pending promises see the terminal state.
- `run_react_consumer` now attributes a body failure on an
  `emitter_supplier_id`-owning subscription to that on-demand supply's quit.
  The field and the attribution already existed for a LAST-phaser die; this is
  its body-side twin.
- The two on-demand branches that ran a `supply { }` body inline
  (`vm_react_loop.rs`'s `build_react_subscriptions` and
  `vm_react_supply_helpers.rs`'s nested-stage registration) now give the
  subscribing `whenever`'s `QUIT` phasers first refusal before wrapping the
  failure in `X::React::Died`.

Consumer-side callbacks are deliberately untouched: `raku` does not route an
exception thrown by a tap body to that same tap's quit handler, so the other
three act-loop call sites pass `None` and keep today's loud
"Unhandled exception in code scheduled on thread" report. An unhandled supply
failure with no `QUIT` anywhere still dies the react, so nothing became quieter —
a failure that used to vanish is now either handled or reported.

## Pin

`t/concurrency/supply/supply-worker-panic-reaches-quit.t` — twelve assertions
covering the panic and the `die` for both the inline `supply { }` body and the
detached act-loop worker, that a healthy supply still completes without running
`QUIT`, that an unhandled body die still dies the react, and that a panic in an
ordinary non-Supply pooled task (`start {}`) still breaks its own Promise
unchanged. The panic trigger is the `@a[2**64 - 1] = 1` index-OOB that
`t/vm/start-panic-boundary.t` already uses; a merely-large index is guarded by a
fallible reservation and would not reach the boundary at all.
