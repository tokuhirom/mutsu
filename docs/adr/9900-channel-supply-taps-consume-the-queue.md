# ADR-9900: A `Channel.Supply` tap is a consumer of the channel queue

- **Status**: Accepted (implemented)
- **Date**: 2026-10-03
- **Deciders**: tokuhirom, Claude
- **Issue**: [#9900](https://github.com/tokuhirom/mutsu/issues/9900)
- **Related**: [ADR-0053](0053-do-whenever-produces-a-tap-on-the-stack.md) §8 (named this
  residue), [ADR-0074](0074-a-channel-backed-supply-broadcasts-to-its-taps.md) (the
  *Rust-producer* channel registry; a different mechanism, untouched here), #7604 (competing
  taps), #11237 (an on-demand ordering bug found on the way)

## 1. Context

rakudo defines `Channel.Supply` as an on-demand supply:

```raku
method Supply(Channel:D:) {
    supply {
        whenever $!async-notify.unsanitized-supply.schedule-on($*SCHEDULER) {
            my \got = self.poll;
            if nqp::eqaddr(got, Nil) { done / die $!closed_promise.cause once closed }
            else { emit got }
        }
        loop { my \got = self.poll; last if nqp::eqaddr(got, Nil); emit got }
    }
}
```

Every tap therefore runs its own body: it drains what is already queued, then polls once per
send notification. A tap is one more consumer of the queue, competing with `receive`, `poll`
and the other taps; a value sent before any tap existed stays queued for the first one.

mutsu instead built a *live* Supply over a supplier "bridged" to the channel, and `Channel.send`
did three things: emit into every bridged supplier (sinks and the `emitted` log), hand the value
to one tap callback round-robin, **and** enqueue it. Measured against rakudo v2026.07, twelve
shapes, eleven diverged:

| Shape | raku | mutsu (before) |
|---|---|---|
| tap after two sends (the issue) | `[1 2 3]` | `[3]` |
| tap after `close` | `[1 DONE]` | `[]` |
| `.map` / `.grep` after sends | backlog included | backlog lost |
| `.list` / `await` of a closed channel's Supply | `(1 2)` / `2` | hang |
| `whenever` in a `supply` block | backlog included | lost |
| tap, then `poll` | `Nil` | the value again (delivered twice) |
| tap after `fail` | `[1 QUIT]` | `[]` |
| two taps over a backlog | first takes all | nothing |
| react `whenever` | `[1 2]` | `[1 2]` |

The react `whenever` was right only because it ignores the bridge and drains the queue itself.
The bridge is the defect: it is a second delivery path beside the queue, so a value could be
emitted without leaving the queue (delivered twice), or emitted with nobody listening (lost).

## 2. Decision

**`Channel.Supply` is an on-demand Supply with a native producer, and each tap's emitter is
attached to the channel as a consumer of its queue.** There is one delivery path: a value leaves
the queue for exactly one consumer.

- `$channel.Supply` returns an ordinary on-demand Supply whose `on_demand_callback` is a
  `__ChannelSupply` shim (`src/runtime/native_methods/channel_supply.rs`), exactly like the
  `__SupplyDerive` producers. Per tap, the producer emits the backlog (`poll` until empty), then
  either completes the emitter (channel already closed: `done`; failed: the body dies with the
  failure, as rakudo's block does) or attaches it (`SharedChannel::attach_tap`). Closing the Tap
  detaches it through the emitter's close callbacks.
- `Channel.send` only enqueues, then runs `pump_channel_taps`; `close`/`fail` likewise. The pump
  moves queued values to the attached taps round robin and, once the channel is closed and
  drained, completes them.
- A tap is **ready** only once its `.tap` call has registered it. Between the producer
  attaching the emitter and the `.tap` call registering the callback, a value emitted would
  reach nobody, so it stays queued. `Supply.tap`/`.act` (`native_supply_mut`, now a thin
  wrapper) marks the taps its call attached ready and pumps once more, which closes the window
  for a value or a close arriving from another thread meanwhile. (A consumer that runs the
  producer without going through `.tap` counts as ready once something listens on the emitter.)
- A react `whenever` keeps draining the queue directly through the Supply's `channel`
  attribute — the same competing consumption, already pumped by the drive loop.

Because the Supply is a plain on-demand one, `map`/`grep`/`head`, `.list`, `await`, `.Promise`
and a `whenever` inside `supply { }` all tap it per use with no channel-specific code.

### Emissions on a foreign supplier

Delivering from `send` meant emitting on a tap's emitter from wherever `send` was called,
including from inside some *other* supply's body. `run_on_demand_body`'s emit buffer collected
every `Supplier.emit` made on that thread, whoever the supplier was, so such a value was also
replayed to that other supply's tap — a pre-existing bug for any supplier
(`supply { $other.emit(5); emit 1 }` delivered `5` to the outer tap). A body's buffer frame now
records its emitter (`supply_emit_owners`), and an emission on a different supplier is not
collected (`Interpreter::supply_emit_frame_for`).

## 3. Alternatives rejected

- **Replay a backlog to the first tap, keep the send-time bridge.** A tap-local replay leaves two
  delivery paths: the bridge still emits at send time while the value also stays queued (the
  double delivery above), the replay races `receive` and react draining, and close ordering has
  to be patched separately. It treats the symptom of the issue's one repro.
- **Keep the live supplier, make `send` skip the queue when a tap exists.** Still decides at
  send time who gets the value, so a value sent before a tap is lost or must be special-cased,
  and combinators registered at `.map` call time (the live-supplier transform taps) would still
  consume values before anyone tapped the derived supply. rakudo's semantics are on-demand; a
  live representation cannot express them.

## 4. Consequences

- `ChannelState` loses `supplier_ids`/`supply_turn` and gains `taps` (emitters, GC-traced) and a
  round-robin cursor. `supplier_emit_callbacks_for_tap` is gone.
- Tap callbacks still run synchronously on the thread that sends (or closes), as before. A
  callback that dies does not fail the `send`.
- `Channel.Supply.live` is now `False`, matching rakudo.
- `Channel.Supply.tap` with no callback still consumes values, as in rakudo.
