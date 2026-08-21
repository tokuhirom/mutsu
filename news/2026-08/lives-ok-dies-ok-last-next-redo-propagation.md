# `lives-ok`/`dies-ok` now propagate a live `last`/`next`/`redo` past the assertion

`lives-ok`/`dies-ok` (`src/runtime/test_functions/eval_exception.rs`) run their block through a
nested-run boundary. A `last`/`next`/`redo` executed inside that block -- e.g. by a closure the
block calls that was captured outside the assertion -- is not necessarily "the block died": when
it targets a real, dynamically-enclosing loop further out (say, a `for` loop that also called
`lives-ok`), the signal has to keep propagating past the assertion's own pass/fail reporting,
exactly like Rakudo. Previously mutsu recorded it as a failed assertion and kept running the
enclosing loop body instead of letting the signal unwind to the loop it actually targeted --
`for 1..2 -> $i { my $cb = -> { last }; lives-ok { $cb() }, "x"; say "after $i" }` ran both loop
iterations and reported two failed tests, where `raku` runs zero and reports zero.

This mirrors an earlier fix for `return` (`is_live_nonlocal_control` already special-cased
`e.is_return()`), but needed a different mechanism: unlike `return`'s compile-time
"lexically in a routine" flag (`OpCode::ReturnFromNonRoutine`'s `lexically_in_routine`),
`last`/`next`/`redo` have no compile-time "is there an enclosing loop" check --
`src/runtime/loop_handler_depth.rs` explains why that check has to be dynamic (a loop-control
signal legitimately crosses routine/`EVAL` call boundaries). Instead, the `Last`/`Next`/`Redo`
opcodes already convert a genuinely homeless signal into a typed `X::ControlFlow` exception
(`RuntimeError::control_flow_illegal`) at the raise site, based on
`loop_handler_depth::loop_handler_in_scope()`. The distinguishing predicate was already sitting
in `RuntimeError::is_illegal_control()`: `control_flow_illegal` sets `exception` (via
`RuntimeError::typed`) in addition to `control`, while the live-signal constructors
(`last_signal`/`next_signal`/`redo_signal`) leave `exception` unset. So the fix was to extend
`is_live_nonlocal_control` to

```rust
e.is_return() || ((e.is_last() || e.is_next() || e.is_redo()) && !e.is_illegal_control())
```

instead of the naive `e.is_last() || e.is_next() || e.is_redo()` that had been tried earlier and
regressed the "genuinely homeless" case (`lives-ok { last }` with no loop anywhere silently
aborted the whole program instead of recording a normal failed assertion) -- `is_illegal_control()`
is exactly what excludes that case, since it is already a typed exception by the time
`is_live_nonlocal_control` sees it.

Added `t/lives-ok-dies-ok-last-next-redo-propagates.t`, mirroring
`t/lives-ok-dies-ok-return-propagates.t`'s style but using `Test::Util`'s `is_run` (since
`last`/`next`/`redo` carry no return value to assert on directly): `last`/`next`/`redo` escaping a
`lives-ok`/`dies-ok` block to a real enclosing `for` loop, and the bare-no-loop case still
correctly reporting a normal failed/passed assertion rather than aborting.
