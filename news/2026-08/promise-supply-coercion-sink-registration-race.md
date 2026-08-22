# `Promise(supply {...})` registered its supplier sinks on the wrong thread — a load-sensitive lost-`done` race

`t/promise-supply-coercion-async-drive.t` test 3 failed at random under CPU
oversubscription (measured 5/20 with 16 busy-loop processes and the jit-stress
env, 0/10 unloaded), reddening the `jit-stress` CI job at random — most
recently on the unrelated PR #6841. The ticket
(`todo/tickets/promise-supply-coercion-async-drive-load-sensitive.md`) had
already established it was not the test's 5-second budget: raising it to 30s
did not help.

## Root cause

A diagnostic run under load showed the exact failure shape: the `start`
block's `await $p` never returned — the coerced promise stayed `Planned`
(status=Planned, `$done` never kept), so the main thread's
`Promise.anyof($done, Promise.in(5))` lost to the timer.

`supply_promise_on_demand` (the `Promise(supply {...})` / `await $supply`
coercion) collects the body's `whenever` subscriptions on the calling thread,
then spawns a background thread to run the react drive loop — and it was that
*spawned* thread that registered the push sinks on the source `Supplier`'s
registry entry (`supplier_sinks_register_batch` inside
`drive_react_subscriptions_inner`). The registration/replay is atomic under the
registry lock, and buffered events replay to a late sink — but the
`Supplier."done"` method dispatch ends with `supplier_reset`, which clears the
buffered values AND the done flag (so a Supplier can be re-tapped, matching
Rakudo's live-hub semantics). So under load:

1. `Promise(supply { whenever $s.Supply ... })` returns a Planned promise;
   the drive thread is spawned but not yet scheduled.
2. The producer runs `$s.emit('x'); $s.emit('y'); $s.done` — the events are
   buffered (no sink yet), and the `done` dispatch then `supplier_reset`s the
   state.
3. The drive thread finally registers its sink: the state is empty and
   not-done — the replay delivers nothing, and the loop waits out its whole
   30s internal deadline before keeping the promise with Nil.

Unloaded, the spawn wins the race and the live sink pushes work, which is why
the failure needed contention to show. In Rakudo, `Supply.Promise` taps the
supply *synchronously* — only event processing is asynchronous — so a producer
is entitled to emit the moment the coercion returns.

## Fix

Register the supplier sinks on the **calling thread**, inside the coercion,
before it returns (`supply_promise_on_demand` now creates the `ReactWaker` and
calls `supplier_sinks_register_batch` itself), and hand the pre-wired waker +
sink registrations to the background drive loop
(`drive_react_subscriptions_prewired` →
`drive_react_subscriptions_nested_prewired`). Registered sinks survive
`supplier_reset`, and the waker queue buffers every pushed event until the
loop drains it, so the producer can emit/done at any point after the coercion
returns. The non-prewired react path is unchanged.

## Verification

Same harness as the ticket (16 CPU burners, `MUTSU_JIT=on
MUTSU_JIT_THRESHOLD=2`, debug build, `scripts/flake-repro.sh -l 16`):

- Before: 5/20 failures (always test 3; ticket measured 8/20 the day before).
- After: **0/50 failures** across two runs (20 + 30), plus the full
  supply/react/promise `t/` sweep green.
