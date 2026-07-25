# `whenever <Promise>` inside a `supply` block runs its body instead of leaking a marker

```raku
my $s = supply { whenever Promise.in(0.05) { emit 'badger' } }
my @r;
$s.tap: { @r.push($_) };
sleep 0.3;
say @r.raku;
# raku:  ["badger"]
# was:   [(Promise.new(…, status => PromiseStatus::Kept), , (), ()),]
```

`whenever <Supply>` inside a `supply` block was fine, and
`react { whenever Promise.in(…) { … } }` was fine — it was specifically a
**Promise source inside a `supply` block**.

## Root cause

`run_whenever_with_value` registers a subscription by pushing a 4-element marker
`[source, body, [LAST…], [QUIT…]]` onto the active `supply_emit_buffer` frame.
Every consumer that later separates markers from genuinely emitted values
recognised one only when its source was a `Supply` instance — the `.tap` path in
`native_supply_mut_methods.rs` and the `await` / `.Promise` path in
`supply_promise.rs`. A Promise-sourced marker matched neither, so it fell
through as an ordinary emitted value and was handed straight to the tap. The
react loop was never affected: it consumes its own frame and already models a
promise source (`ReactSubscription.promise`).

## Fix

Raku's `whenever $promise` is exactly a one-shot supply — emit the result once,
then done — so the two supply-block consumers now each model it that way, using
the shape that already existed for their kind of source:

- The **`.tap` path** rewrites a Promise-sourced marker into a supplier-backed
  `Supply`-sourced one (`normalize_promise_whenever_markers`), which lets the
  whole existing tap / serialize-group / done-group machinery drive it
  unchanged. When the promise is kept, its result is emitted into that stand-in
  supplier and the supplier is immediately marked done; a broken promise
  `quit`s it, so the `whenever`'s QUIT phaser and the tap's `quit` handler both
  see it. `SharedPromise::on_resolve` plus `clone_for_thread()` drives that —
  the same pair `promise_chain_method` uses for `.then`.
- The **`await` / `.Promise` path** builds a one-shot channel subscription
  directly, the same shape `vm_react_loop.rs` builds for a promise source in a
  `react` block.

The interesting constraint is ordering. A supplier keeps no backlog —
`register_supplier_tap` does not replay past emissions — so arming the promise
at rewrite time would let an already-resolved (or quickly-resolved) promise emit
into a tapless supplier and lose the value. The rewrite therefore parks each
`(promise, stand-in supplier)` on `pending_promise_whenever_arms` and the
consumer fires them with `arm_pending_promise_whenevers()` only after its
tap-registration loop has run.

Pin: `t/supply-whenever-promise.t`.

## Impact

`Test::Scheduler` (`TODO_dist` T-037), whose `timeout` combinator is
`supply { whenever $source -> $value { … whenever Promise.in($timeout) { … } } }`.
Together with the two fixes that landed just before it — BUILD-before-defaults
and the `&`-sigil pointy parameter — its `t/virtualized-time.rakutest` goes from
stopping at test 2 to reaching test 28.

Not fixed here, and pre-existing: `await (supply { whenever $live { emit … } })`
with no explicit `done` in the body resolves to `Nil` rather than the last
emitted value. That is not specific to promise sources — a live `Supplier`
source behaves identically — so it is a separate gap.
