# `Tap.close` on a `whenever` is ordered against the emit, not against the pump

This closes the last row of the long-running "`whenever` in expression position"
finding, and with it [ADR-0053](../../docs/adr/0053-do-whenever-produces-a-tap-on-the-stack.md)'s
slice 2. Slice 1 had already made `do whenever` answer a real `Tap`; what
survived was that closing that `Tap` threw away values the react loop had not
got to yet.

## What rakudo actually does

Delivery inside a `react` is deferred: `say`-instrumenting the body prints
`A B C got 1 got 2`, and reading the accumulator halfway through the body shows
it still empty. Both implementations agree on that. The close, however, is
**not** ordered against the pump that delivers — it is ordered against the
`emit`. A value emitted while the tap was live is delivered even if the close
happens before the pump ever runs; a value emitted afterwards is dropped. One
rule explains every measured shape:

| # | Shape (Supplier source unless noted) | rakudo v2026.07 | mutsu before | mutsu after |
|---|---|---|---|---|
| A1 | two emits, no close | `[1, 2]` | `[1, 2]` | `[1, 2]` |
| A2 | close, then two emits | `[]` | `[]` | `[]` |
| A3 | emit, close, emit | `[1]` | **`[]`** | `[1]` |
| A4 | emit, emit, close | `[1, 2]` | **`[]`** | `[1, 2]` |
| B1/B3/B4 | the same three with a plain `.tap` outside `react` | `[1,2]` / `[1]` / `[1,2]` | same | same |
| C1 | three emits, tap closes itself in its own callback | `[1, 2, 3]` | **`[1]`** | `[1, 2, 3]` |
| C2 | `Supply.interval`, self-close on the second tick | `[0, 1]` | `[0, 1]` | `[0, 1]` |
| D1 | sibling `whenever` closes this tap; emit after the sibling's emit | `[1, 2]` | **`[1]`** | `[1, 2]` |
| D2 | two emits, close, then an emit from a later pump round | `[1, 2]` | **`[]`** | `[1, 2]` |
| E1 | a `whenever` registered, fed and closed inside a sibling's body | `[1, 2]` | **`[]`** | `[1, 2]` |
| J1 | does an explicit `.close` fire the subscription's `LAST` phaser? | no | no | no |

`Tap` has no `.closed` method in rakudo at all (`.^methods` gives
`BUILD POPULATE close new`), which retires that question from ADR-0053's slice-3
residue list without any code.

## Root cause — and why the recorded one was wrong

The finding, and ADR-0053 §8 after it, blamed `Interpreter::dispatch_waker_events`
(`src/vm/vm_react_subscriptions.rs`): it drains the whole FIFO batch and *then*
consults `is_whenever_closed` per event, so it cannot tell an event queued
before the close from one queued after. A `rust-gdb -batch` run with breakpoints
on both that per-event check and the drive loop's top-of-round retirement shows
the per-event check **never fires**. The blame was on the wrong line.

The actual droppers were two *blanket* retirements that ran before any event was
looked at:

- `drive_react_subscriptions_loop` sets `sub.done = true` at the top of every
  round for any subscription whose id is in the closed set. `dispatch_waker_events`
  then skips every event for a `done` subscription — which is why its own check
  never got a chance to be wrong.
- `adopt_newly_registered_subscriptions` filtered a pending `whenever` marker out
  entirely if its id was already closed, so a subscription registered, fed and
  closed inside a sibling `whenever`'s body never got a sink at all and its
  source's backlog was never replayed (row E1).

Underneath both: `close_whenever` recorded the close as a *timeless* bit in a
process-global set. A bit has no position in the event order, so no consumer of
it could ever distinguish "emitted before" from "emitted after".

## The fix

Give the close a position in the same total order the events already carry.
The supplier registry already stamped every buffered value with a global
`next_emit_seq()` (it uses it to replay sibling supplies merged in true emit
order); that counter moves to `value::waker::next_event_seq` and now serves
three things at once:

- every event queued on a `ReactWaker` carries its sequence — `push_at` lets a
  producer that stamped the value under its own registry lock, or a backlog
  replayed to a late-registered sink, keep the sequence it was really emitted
  at rather than getting a fresh one at push time;
- `close_whenever(id)` records the sequence it was called at, so
  `whenever_closed_seq(id)` answers "closed, and at what point";
- the drive loop compares the two. `dispatch_waker_events` delivers an event
  sequenced at or before the close and retires the subscription only on a later
  one; the top-of-round retirement fires only once the waker holds no event for
  that subscription from before the close (`ReactWaker::has_event_upto`).

The marker filter in `adopt_newly_registered_subscriptions` is deleted rather
than made order-aware: with one ordered retirement rule, a closed marker is
adopted like any other, registers its sink (which replays the pre-close backlog
with real sequences) and is retired by that same rule as soon as it owes
nothing. That leaves exactly one mechanism deciding when a closed subscription
goes away, instead of three that disagreed.

## A local test was pinning the wrong answer

`t/react-whenever-tap-close.t`, written as ADR-0053's slice-2 pin, asserted that
a self-close stops queued later emissions (`$seen == 1` for three emits) and
that a sibling close suppresses an already-emitted value (`$left-seen == 0`).
Run under `raku` verbatim, both fail: rakudo answers `3` and `1`. The file now
pins rakudo's answers, plus the two genuine "emitted after the close" shapes
that do demonstrate the close working. `t/whenever-tap-close-ordering.t` loses
its two `todo` markers, and `t/whenever-tap-close-control-table.t` pins the
whole table above — all twelve rows verified green under `raku` first.

## Residue

`Channel.send` in mutsu eagerly `supplier_emit`s into the Supply the channel is
bridged to, so a channel value counts as emitted at `send` time; rakudo's
channel-to-Supply pump is asynchronous, so the same value is still sitting in
the channel when a close in the same body runs. That makes two channel-source
shapes disagree in the opposite direction now (mutsu delivers where rakudo does
not) — previously masked, because the blanket drop discarded them too. It is a
property of the bridge, not of the close, and is recorded as
`todo/tickets/channel-supply-bridge-emits-at-send-time.md`.
