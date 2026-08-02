# A `supply { }` transform between a live source and a react now streams

`react { whenever supply { whenever $live { emit … } } { … } }` — a `supply { }`
transform placed between a live source and a react — delivered nothing. This is
the shape every Cro pipeline stage has (`Cro::Transform.transformer` returns
exactly such a supply), so no Cro pipeline could carry a value.

```raku
my $s = Supplier.new;
my $out = supply { whenever $s.Supply -> $x { emit "got-$x" } };
react {
    whenever $out -> $v { say "GOT $v"; done }
    whenever Promise.in(1) { $s.emit(1) }
    whenever Promise.in(5) { say "TIMEOUT"; done }
}
# was: TIMEOUT      now: GOT got-1
```

The inner supply body *ran*, and its `whenever` *fired* — only the `emit` was
lost. Tapping the same supply with `.tap` worked; only the react `whenever` path
was broken.

## Root cause

The react loop's on-demand branch registers a `StreamConsumer` keyed by the
supply's emitter id, whose `consumer_cb` is the outer `whenever`'s callback. That
is what routes an `emit` inside the supply body downstream. The body is then run
once, and the `whenever` registrations it produced are turned into
`ReactSubscription`s that the event loop polls.

But the `StreamConsumer` was truncated off the stack *before* the event loop
started. So the first, synchronous pass worked and everything afterwards was
dropped on the floor: when the inner subscription fired later, its body's `emit`
found no consumer for the emitter id.

Two consequences had to be handled once the consumer survives:

- `try_stream_emit` has to **swallow** a `done` raised by the consumer callback,
  because the emitting body still needs to unwind; it records
  `StreamConsumer::done` instead. With the consumer now alive during the loop,
  the drive loop checks that flag each poll and ends the react.
- A *chained* transform (`supply { whenever supply { … } { … } }`) still failed:
  `value_to_react_subscription` returns `None` for an on-demand source, so the
  middle stage fell through to `replay_inner_static_subscription`, which replays
  it once. A nested `supply { }` source now gets the full treatment — its own
  emitter id, its own `StreamConsumer` routing to the `whenever` body that
  consumes it, and its inner registrations turned into subscriptions in turn,
  recursing for another stage (`register_nested_on_demand_source`, bounded at 32
  stages by the same runaway-nesting guard `drive_inner_supply_to_consumer` uses).

## Changes

- `src/vm/vm_react_loop.rs`: the react's `StreamConsumer`s live until the event
  loop returns (`stream_base`), and a nested on-demand source is wired up rather
  than replayed.
- `src/vm/vm_react_supply_helpers.rs`: new `register_nested_on_demand_source`,
  the recursive stage-wiring helper.
- `src/vm/vm_react_subscriptions.rs`: the drive loop honours a `done` recorded on
  a `StreamConsumer`.

Pinned by `t/react-whenever-chained-live-supply.t` (4 cases: single stage, `done`
from the outer body, a stage declared in a method, and two chained stages), which
passes identically under `raku`.
