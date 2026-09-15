# Supply combinator chains lost every stage but the last

`$supplier.Supply.map(...).map(...)` emitted nothing under mutsu. Neither did
`.map(...).grep(...)`, `.grep(...).produce(...)`, or `.map(...).produce(...)`.
`.produce(...).map(...)` emitted the *unfolded* source values, as if the
`produce` had never been written. rakudo delivers all five correctly, and any
`react whenever` over such a chain simply waited forever on a value that had
been computed upstream and then thrown away.

This is what made `SupplyTimeWindow` 0.0.1's `t/02-use.t` time out in the
ecosystem parity sweep — a member of the timeout cluster,
[#7995](https://github.com/tokuhirom/mutsu/issues/7995). Its whole module is one
chain:

```raku
self.map(-> $i { [{:time(now), :value($i)},] })
    .produce(-> @arr, @last { ... })
    .map(-> $values { @($values)>>.<value> })
```

Nothing reached the test's `react`, so its two `await`s never returned. The
sweep charged it as `SWEEP-TIMEOUT` with 0 of 14 assertions; it now runs 14/14.

## Two independent causes, both about composition

### Every re-emit site ran one action kind out of thirteen

A live combinator builds a *derived supplier* and registers a tap on its source
that forwards into it. An emission at the head of a chain therefore produces
actions on the head's supplier, one of which re-emits into the next supplier —
whose own actions must then be run, and so on to the end of the chain.

Fifteen separate places across five files did that re-emit, each with its own
inline copy of

```rust
supplier_emit(dsid, v.clone());
for a in supplier_emit_callbacks(dsid, &v) {
    if let SupplierEmitAction::Call(tap, emitted, delay) = a { /* ... */ }
}
```

`Call` is the plain "hand the value to a tap callback" action. The other twelve
kinds — `TransformCall`, `ProduceCall`, `BatchEmit`, `FlatEmit`, `ForwardEmit`,
`ZipBuffer`, `Migrate`, `UniqueCheck`, ... — were matched away and dropped on
the floor, silently. So a derived supplier whose own tap was itself a combinator
received nothing at all. One stage worked; two did not.

There is one driver now, `Interpreter::drive_supplier_emit_actions`, and one
re-emit primitive, `Interpreter::handle_supply_forward`, which calls it. Because
the re-emitting action kinds go back through that primitive, a chain of any
length drives itself to the end. The three giant hand-written action `match`es
(the two `Supplier` emit lanes and `IO::Socket::Async`'s) now delegate to it;
the `Supplier` lanes keep only their own `Call` arm, which routes a failing tap
callback to that supplier's quit handlers.

`done` had the same one-hop limit: `get_transform_output_supplier_ids` returned
the *immediate* derived suppliers, so the second stage of a chain was never told
its source had finished and a `done =>` handler behind it never ran. The walk is
transitive now (and covers `produce` and `flat` downstreams, not just
`map`/`grep`). `migrate`'s forward link is deliberately left out — an inner
supply finishing does not finish a migrate output.

### `Supply.produce` was not a pipeline stage at all

`map` and `grep` allocate a derived supplier. `produce` did not: it handed back
a Supply carrying the **source's** supplier id plus a `produce_callable`
attribute, to be noticed at tap time by an `else if` in the tap chokepoint.

That makes it uncomposable in both directions. Downstream, the next combinator
registered itself on the shared id and dropped the attribute — which is exactly
why `.produce(...).map(...)` mapped the raw values. Upstream, the attribute
meant the previous stage's derived supplier had no tap that would ever reach the
produce.

`produce` now owns a supplier like every other stage, fed by a produce tap on
its source (`ProduceState` grew a `produce_downstream` beside the
`reduce_downstream` `Supply.reduce` already used for the same purpose). The
`produce_callable` attribute and its tap-time branch are gone.

## Pinned by

`t/concurrency/supply/supply-combinator-chain.t` — twelve assertions covering
`map`→`map`, `map`→`grep`, `grep`→`map`, `map`→`produce`, `produce`→`map`, the
three-stage `SupplyTimeWindow` shape, `done` propagation through two stages and
through a produce stage, `react whenever` over a chained live Supply, and the
two single-stage cases (`produce`, `reduce`) the change must not disturb.
rakudo passes all twelve unchanged.

## Not fixed here

`head`, `unique`, `lines`, `words` and `elems` still use the
marker-attribute-on-the-shared-supplier-id shape `produce` has just left, so
they remain uncomposable *as a source*: `.head(3).map(...)` drops the `head`
the same way `.produce(...).map(...)` dropped the produce. Filed as
[#8474](https://github.com/tokuhirom/mutsu/issues/8474): the value-propagation
half is already in place, so each one only needs to start owning a supplier —
but `head`'s done semantics, `unique`'s per-tap `seen` keying and `lines`/`words`
partial-input flushes make it its own slice rather than a mechanical port.
