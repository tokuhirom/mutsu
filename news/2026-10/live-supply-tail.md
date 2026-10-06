# `Supply.tail` and `.first(:end)` on a live supply emit

`Supplier.new.Supply.tail(1).tap(...)` never emitted: `tail` sliced the source's
materialized values at call time, and a live supply has emitted nothing yet. It is now a
pipeline stage like `head`: its own derived supplier, fed by a tail tap that holds back
the last N values and releases them, then its own `done`, when the source is done. It
composes with `map`/`grep`/`reduce` in both directions, and `.first(:end, ...)` on a live
supply is `grep(...).tail(1)` like Rakudo.

`tail(1)` (or plain `tail`) of a source that emitted nothing now emits one undefined
value, live or not, as Rakudo does; `tail(N)` for other N still emits nothing.

A `quit` on the source still does not reach the `quit` handler of a tap on a derived
supply, for `tail` as for `map`/`grep`/`head`; that is filed as #12017 (#11839).
