# `done` in a `whenever` body does not stop the source delivering later values

`done` inside a `whenever` body ends the enclosing supply, and rakudo tears the
supply's subscriptions down with it — the source stops reaching the body. mutsu
completes the supply (the downstream `done` handler fires) but leaves the
`whenever`'s tap on the source registered, so the body keeps running for every
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
say @got;      # raku: [one stop]      mutsu: [one stop ignored]
```

The emitted values go nowhere (the supply is already complete, so nothing is
delivered downstream), which is why this has stayed invisible — but any side
effect in the body still runs, once per later emit.

## Where it is

`Supplier.emit` now propagates the `done` control signal out of the tap-callback
dispatch unchanged (`src/runtime/native_supplier_methods.rs`, both the mutable
and immutable `"emit"` arms — see
`news/2026-08/done-in-a-tap-callback-is-a-control-signal.md`). What is missing
is the other half: whoever consumes that signal on behalf of the enclosing
supply block should also close that block's upstream subscriptions, the way
`close_all_supplier_taps` does for an explicit `Supplier.done` and the way the
on-demand `tap` path's `__SupplyOnDemandComplete` marker does for a body-level
`done`. The subscriptions to close are exactly the ones recorded in
`upstream_taps` in `native_supply_mut_methods`' `"tap"` arm.

Pinned-adjacent test: `t/supply-done-in-tap-callback-is-not-a-failure.t` covers
the control-signal half; this ticket is the teardown half.
