# A `next` with no loop is a catchable `X::ControlFlow`, not an escaping signal

In rakudo a loop-control statement that finds no construct to act on becomes an
ordinary, catchable exception at the point it is raised:

```raku
try { my $i; { $i++; next; $i--; } };
say $!.^name;      # X::ControlFlow
say $!.message;    # next without loop construct
say $!.illegal;    # next
say $!.enclosing;  # loop construct
```

mutsu raised `RuntimeError::next_signal()` — a control *signal* — and
`try`/`CATCH` deliberately let control signals through, so it escaped every
handler and surfaced only at the top of the program:

```
$ mutsu -e 'say (try { my $i; { $i++; next } }).^name'
Runtime error: X::ControlFlow
```

## Why the discriminator has to be dynamic

Neither `try` nor the compiler can decide this alone:

* **A control signal legitimately crosses routine and `EVAL` boundaries.**
  `sub f { next }; for 1..3 { f() }` iterates three times in rakudo *and* in
  mutsu, and so does `for 1..3 { EVAL 'next' }`. So neither the routine boundary
  nor the `EVAL` boundary may convert the signal.
* **"There is no lexically enclosing loop" is not sufficient**, for the same
  reason: the loop that handles the signal is usually in another routine.
* **Converting at the top of the interpreter loop** fixes the top-level message
  but not catchability — by then the `try` has already passed the signal on, and
  unwinding is one-way.

The only correct question is the one rakudo asks: *is there a construct on the
dynamic chain right now that would handle this?*

## The mechanism

`src/runtime/loop_handler_depth.rs` — a thread-local depth with an RAII guard.
Every construct that handles `next`/`last`/`redo` holds a `LoopHandlerGuard` for
the extent in which it would catch, and the `Last`/`Next`/`Redo` opcodes consult
it: at depth zero they raise `RuntimeError::control_flow_illegal` instead of the
signal.

A thread-local rather than an `Interpreter` field, for two reasons. The guard
needs no borrow of `self` — the handler sites are deep inside loops that already
hold `&mut self` — and `Drop` makes it correct on every early return and `?`,
where a hand-written decrement would have to be repeated at each of dozens of
exits and one missed exit would turn a working `next` into a thrown exception. A
control signal never crosses a thread, so per-thread state is the right scope.

The converted error **keeps** its `control` flag, because a `CONTROL` block still
catches it in rakudo (`try { CONTROL { … }; next }` runs the handler). What stops
`try` from passing it through is the new `is_illegal_control()` — the pair of
"has a `control` flag" *and* "carries a typed exception", the shape only this
constructor produces.

## The sweep

29 handler functions across 20 files, found with `git grep -l
'is_next()\|is_last()\|is_redo()'`: the five loop opcodes,
`map`/`grep`/`deepmap`/`duckmap`/`nodemap`, `reduce`/`produce`, the lazy `for`
and lazy-pull paths, the sequence stepper, the labelled `do` block, and the
supply/react tap loops. Five of the grep hits are *not* handlers and deliberately
have no guard: the error-message enhancer in `calls.rs`, the top-level
signal-to-class conversion in `vm_control_ops.rs`, the
`is_exceptional_block_exit` predicate, and `try`'s own passthrough arms.

`exec_do_block_expr_op` takes its guard **conditionally**, on `has_label` — an
unlabelled `do {}` does not handle the signal, and giving it a guard would make
`try { { next } }` look handled and go on raising an uncatchable signal.

The sweep has to stay complete: a construct that catches the signal without
holding a guard will still catch it *if one is raised*, but the raise site will
have converted it to an exception first and silently broken that loop. The second
half of the pin exists for exactly that — one assertion per construct.

## Result

`roast/S04-statements/do.t`, `roast/S04-statements/redo.t` and
`roast/S04-blocks-and-statements/pointy.t` all pass under the real `Test` module
(they failed on this alone; mutsu's native `throws-like` special-cased control
signals, which is why they passed under the native provider). `make test` and
`make roast` are green.

Pin: `t/loop-control-without-loop.t` — the three illegal forms with their
`illegal`/`enclosing` attributes, `CONTROL` still catching, the routine and
`EVAL` boundaries still transparent, and one assertion per handling construct.
It passes under `raku` as well. (The three `throws-like`s are written out rather
than looped: rakudo's own `throws-like` nests its subtests wrongly when called
from inside a `for` whose EVAL'd argument raises a loop-control exception, so the
loop form fails under `raku` for reasons unrelated to what is tested here.)

## Two divergences found while writing the pin

Both pre-existing, both filed:

* `todo/tickets/labelled-bare-block-is-not-a-loop-construct.md` — `LAB: { last
  LAB }` leaves the block in mutsu and is `labeled last without loop construct`
  in rakudo.
* `todo/tickets/deepmap-on-a-range-does-not-map.md` — `(1..4).deepmap({…})`
  never calls the block and answers a `Range`.
