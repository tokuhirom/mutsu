# `lives-ok`/`dies-ok` still misreport `last`/`next`/`redo` escaping to a live outer loop as "died"

`todo/deep/return-outside-routine-uncatchable-inside-nested-run.md` was fixed by (1) a compiler
fix for `try`/CATCH tail-statement sinking, and (2) teaching `test_fn_lives_ok`/`test_fn_dies_ok`
(`src/runtime/test_functions/eval_exception.rs`) to propagate a live `return` control signal
transparently instead of reporting it as "the block died", when the block calls a closure whose
`return` targets a routine outside the assertion's own nested-run boundary. `last`/`next`/`redo`
were deliberately left out of that fix and still misbehave the same way `return` used to.

## Repro

```
$ raku -e 'use Test; plan 1; for 1..2 -> $i { my $cb = -> { last }; lives-ok { $cb() }, "x"; say "after $i" }; say "done"'
1..1
done
# You planned 1 test, but ran 0

$ mutsu -e 'use Test; plan 1; for 1..2 -> $i { my $cb = -> { last }; lives-ok { $cb() }, "x"; say "after $i" }; say "done"'
1..1
not ok 1 - x
# Failed test 'x'
...
after 1
not ok 2 - x
...
after 2
done
# Planned 1 tests, but ran 2
# You failed 2 tests of 2
```

Same shape for `next` (verified). `mutsu` treats the `last`/`next` as "the block died" (records a
failed assertion, then keeps running the enclosing `for` loop body) instead of letting the signal
unwind past `lives-ok` to the loop it actually targets.

## Why this was NOT bundled into the return fix

`return` has a reliable "is this truly homeless" signal available at the point `lives-ok` checks
the error: `OpCode::ReturnFromNonRoutine`'s `lexically_in_routine` flag is baked in at COMPILE
TIME (does the `return` have *any* lexically-enclosing routine at all?), so a `return` that
reaches `lives-ok`'s error check with `e.is_return() == true` is *guaranteed* to still be a live,
in-flight signal headed for a real enclosing routine -- a genuinely homeless `return` is already
converted to a typed `X::ControlFlow::Return` (`control: None`) before ever reaching there. So
`test_fn_lives_ok`/`test_fn_dies_ok` can safely say "if `e.is_return()`, propagate; it can't be a
truly-dead end".

`last`/`next`/`redo` have **no equivalent compile-time "no enclosing loop" check**. Verified:

```
$ raku -e 'use Test; plan 1; lives-ok { last }, "bare last, no loop anywhere"; say "after"'
1..1
not ok 1 - bare last, no loop anywhere
# last without loop construct
after
```

A bare `last` with genuinely no enclosing loop still reaches `lives-ok`'s check as a raw
`e.is_last() == true` signal (not yet converted to a typed exception) -- the conversion only
happens later, at whatever boundary plays the `last`/`next`/`redo` equivalent of
`vm_run_loop.rs`'s `is_return()` conversion (untraced as part of this ticket). Naively adding
`e.is_last() || e.is_next() || e.is_redo()` to `is_live_nonlocal_control` in
`src/runtime/test_functions/eval_exception.rs` was tried during the return fix's development and
regressed exactly this case: `lives-ok { last }` with no loop stopped reporting "not ok" and
instead aborted the whole program uncaught, because the signal was blindly propagated past
`lives-ok` without ever being converted.

## What a real fix needs

Find (or build) the equivalent of `return`'s compile-time "lexically in a routine" check for
`last`/`next`/`redo` -- i.e. a way to know, cheaply, whether a `last`/`next`/`redo` opcode has
*any* lexically-enclosing loop at compile time, so a genuinely homeless one can be converted to a
typed exception immediately (like `OpCode::ReturnFromNonRoutine` does for `return`) instead of
staying a raw signal until some later boundary. Once that exists, `is_live_nonlocal_control` can
safely include `is_last()`/`is_next()`/`is_redo()` the same way it now includes `is_return()`.

Alternatively: audit whatever *does* eventually convert an escaped raw `is_last()`/`is_next()`
into `X::ControlFlow::Last`/`X::ControlFlow::Next` etc. (analogous to
`vm/vm_run_loop.rs`'s `is_return()` conversion) and see whether it can be made to run reliably
*before* `test_fn_lives_ok`/`test_fn_dies_ok` see the result, rather than only at a true top level.

## Repro pins

None yet -- this file exists so the gap isn't silently reintroduced or forgotten. A future fix
should add `t/lives-ok-dies-ok-last-next-propagates.t` alongside `t/lives-ok-dies-ok-return-propagates.t`
(which pins the `return` case that IS fixed) and the "still dies correctly" bare-`last`-no-loop
case as a regression guard, mirroring `t/try-catch-tail-statement-sink.t`'s approach.
