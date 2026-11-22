# `Supply.stable` passes `done` straight through

Rakudo's `Supply.stable` cues the emit of the value it holds on the scheduler
and passes `done` on at once. mutsu flushed the held value on `done` and only
then finished. A finite source materialized to its last value. As a result,
`$s.Supply.stable(10)` followed by `emit 1; done` gave `[1 done]` rather than
`[done]`, and `Supply.from-list(1, 2, 3).stable(0.05).list` gave `(3)` rather
than `()` (#18615).

`done` now reaches the taps at once, and the held value is dropped. Rakudo
still emits it after `done` once its window ends, but a done supply does not
deliver values here. A finite source is debounced per tap through the same
worker stage as a live one, so a `supply` block that pauses between its emits
is debounced in real time.
