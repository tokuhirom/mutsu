# `done` in a `whenever` body now stops the source delivering later values

`done` inside a `whenever` body ends the enclosing supply, and rakudo tears
the supply's subscriptions down with it — the source stops reaching the body.
mutsu completed the supply (the downstream `done` handler fired) but left the
`whenever`'s tap on the source registered, so the body kept running for every
later emit:

```raku
my $source = Supplier.new;
my @got;
my $s = supply {
    whenever $source -> $v {
        @got.push($v);
        done if $v eq 'stop';
    }
}
$s.tap(-> $ { });
$source.emit('one');
$source.emit('stop');
$source.emit('ignored');
say @got;      # raku: [one stop]      mutsu (before): [one stop ignored]
```

The emitted values went nowhere (the supply was already complete, so nothing
was delivered downstream), which is why this stayed invisible — but any side
effect in the body still ran, once per later emit.

## Root cause

`done` inside a `whenever` body compiles to a call to the enclosing supply
block's emitter Supplier's own `.done` method (not the `OpCode::ReactDone`
control-flow raise used by a bare `done;` inside `supply`/`react` directly).
The emitter's `.done` fired the `__SupplyOnDemandComplete` marker registered
on it, which ran each whenever source's `on_close` callbacks and the
downstream `done` handler — but never closed the taps `native_supply_mut_methods`'
`"tap"` arm had registered on the *trigger* sources (`upstream_taps`, e.g. the
tap on `$source` in the example above). Only an explicit `Supplier.done` or an
explicit `Tap.close`/`.cancel` cascaded through `close_all_supplier_taps` /
`upstream_taps`; the whenever-body `done` path never did.

## Fix

`make_on_demand_complete_marker` now also carries the `upstream_taps` list
built during `.tap()` (the same `[supplier_id, tap_id]` pairs, and nested
`Tap` handles for chained on-demand sources, that `Tap.close`/`.cancel`
already cascade through). `invoke_done_callback`'s `__SupplyOnDemandComplete`
arm closes them via a new shared helper, `Interpreter::close_upstream_taps`
(factored out of `native_tap`'s `"cancel"`/`"close"` cascade in
`native_methods/scheduler.rs`), so a `done` inside a whenever body tears down
the same upstream subscriptions an explicit `Tap.close` would.

This also fixes the case of two `whenever`s sharing one `supply { }` block:
`done` inside one body now closes every sibling whenever's source
subscription too, not just the one that triggered it.

Pinned by `t/supply-done-in-whenever-stops-later-emits.t`. The control-signal
half of this bug (a `done` inside a whenever body no longer surfacing as a
supply failure) was already fixed and is pinned by
`t/supply-done-in-tap-callback-is-not-a-failure.t`; this closes the teardown
half tracked in
`todo/tickets/done-in-a-whenever-body-does-not-stop-later-emits.md`.
