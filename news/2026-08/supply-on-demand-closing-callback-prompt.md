# `Supply.on-demand`'s `closing` callback now fires promptly per tap

`Supply.on-demand(..., closing => { ... })`'s `closing` callback is supposed
to fire shortly after each individual tap's producer finishes, not batched
together only when the enclosing `react` block finally tears down. mutsu had
this backwards: every pending `closing` callback across every on-demand tap
sat queued until the whole react loop exited, then fired back-to-back in one
burst — even though the *final* value each callback produced was always
correct (which is why this went unnoticed for a while; only the timing was
wrong).

## Root cause

The react/supply drive loop (`src/vm/vm_react_subscriptions.rs`) tracks each
async on-demand tap (e.g. `Supply.on-demand(-> $s { start { $s.emit(42);
$s.done } }, closing => { ... })`) as a source-less `ReactSubscription`
carrying `close_callbacks` and an `on_demand_done` promise that resolves once
the tap's producer calls `.done`/`.quit` on its emitter. The only code that
ever actually invoked `close_callbacks` was `run_react_close_callbacks`,
called exactly once — after the drive loop's `'react_loop` had already
broken. Nothing fired them while the loop was still running.

Making it worse, a separate fallback in the same loop — meant to retire a
source-less subscription with nothing left to wait for — marked every such
subscription `done` on its very first poll unless it still had pending
`last_callbacks`. It never checked `close_callbacks`, so an on-demand tap's
subscription got marked `done` (and thus skipped on every subsequent
iteration) almost immediately, typically well before its `on_demand_done`
promise had even resolved. Once marked done, the subscription was invisible
to any future poll, so its `closing` callback had no chance to fire until the
unconditional loop-exit sweep.

## Fix

`drive_react_subscriptions_loop` now fires a subscription's `close_callbacks`
(draining them via `std::mem::take` so the loop-exit catch-all does not
refire them) at the exact point it observes completion:

- In the `on_demand_done` promise poll (both the normal "Kept" path and the
  "Broken"/quit path — real Raku fires `closing` on either), right when the
  promise resolves.
- In the waker-driven `SinkEvent::Done` handler, for any supplier-backed
  subscription that also carries close callbacks.

The early-retirement fallback for source-less subscriptions was also
corrected: it now treats a pending `close_callbacks` list the same way it
already treated `last_callbacks` — as a reason to keep polling instead of
marking the subscription done prematurely.

## Verification

`t/react-nested-whenever-on-demand-close.t` gained a new subtest that would
have caught this: it runs a react block with a much longer backstop (2s) than
any single on-demand tap/close cycle should need, checked at an earlier
checkpoint. Confirmed (via a temporary revert) that the new subtest fails
against the old code (`closed=0` at the checkpoint, since every `closing`
callback was still queued for the eventual backstop) and passes with the fix.
