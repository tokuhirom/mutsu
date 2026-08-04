# `done` in a tap callback is a control signal, not a supply failure

`Supplier.emit` dispatches to each registered tap callback and treats any error
coming back as a supply failure: it routes the exception to the supplier's
`quit` handlers, or re-raises it as one if there are none. Two control signals
were already carved out of that — `return` (which belongs to the callback's
enclosing routine) and `next` (which skips the rest of the body for this value)
— but `done` and `last` were not.

So a `done` inside a `whenever` body took the failure path. That path reads
`err.exception` and rebuilds a plain `RuntimeError` from it, which **drops the
control flag**: what started as a `Control::ReactDone` signal came out the other
side as an ordinary thrown `X::ControlFlow` with `illegal => "done"`,
`enclosing => "supply or react"`. Nothing downstream recognised it as a
completion any more.

The loudest symptom was on a channel reader thread. A `whenever` on a socket
supply is driven by its own thread (`run_supply_act_loop`); when a body running
there emitted into a `Supplier` whose own tap called `done`, the de-flagged
exception surfaced at the top of that thread as

```
Unhandled exception in code scheduled on thread
done without supply or react
```

and the loop answered it with `std::process::exit(1)`. That killed the whole
process mid-file: the vendored Cro suite's `http-middleware.rakutest` aborted
after subtest 4 in 2 runs out of 3, hiding subtests 5-11 entirely. It now
completes all 11 every time.

Both `"emit"` arms (`native_supplier_mut` and `native_supplier`) now propagate
`done`/`last` unchanged, next to the existing `return` carve-out, and
`run_supply_act_loop` absorbs such a signal as end-of-stream rather than as a
fatal error — the same `is_react_done() || is_last()` treatment every other
supply drive loop already gives it.

One half of the semantics is still missing: completing the supply this way does
not yet tear down the `whenever`'s subscription on its source, so the body keeps
running (to no effect) for later emits. That is tracked in
`todo/tickets/done-in-a-whenever-body-does-not-stop-later-emits.md`.

Pinned by `t/supply-done-in-tap-callback-is-not-a-failure.t`.
