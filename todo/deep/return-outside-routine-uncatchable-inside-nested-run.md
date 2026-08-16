# A `return` escaping to top level inside a nested run (EVAL, `throws-like`, …) is not catchable

`gather { return 1 }`, evaluated via `EVAL` (as `throws-like 'gather { return
1}', X::ControlFlow::Return` does in `roast/S32-exceptions/misc.t`), raises
"Attempt to return outside of any Routine" but the error is **not** wrapped as
a catchable `X::ControlFlow::Return` — it propagates as a raw `is_return()`
control-flow signal straight past any surrounding `try`/`CATCH`, all the way
to the true top of the program, where it aborts with an uncaught runtime
error instead of being caught.

## Repro

```
$ mutsu -e 'try { EVAL(q[gather { return  1}]); CATCH { default { .message.say; .^name.say } } }; say "reached"'
Attempt to return outside of any Routine
  in block <unit> at -e line 1
# "reached" never printed — the CATCH never ran, the whole program aborted.

$ raku -e 'try { EVAL(q[gather { return  1}]); CATCH { default { .message.say; .^name.say } } }; say "reached"'
Attempt to return outside of any Routine
X::ControlFlow::Return
reached
```

(Separately, note `mutsu -e 'gather { return 1 }'` as a **bare top-level**
statement — no EVAL — exits 0 silently: the lazy `Seq` is never iterated, so
the `return` inside never executes at all. That is arguably correct — sink
context on an un-forced `Seq` is a different question from this ticket, not
investigated here — the EVAL case is the one that matters because
`throws-like`'s harness EVALs its string argument and something in that path
force-iterates the `gather` eagerly.)

## Root cause

`vm/vm_run_loop.rs`'s `run()` converts an escaped `CX::Return` signal into a
catchable `X::ControlFlow::Return` only when:

```rust
if e.is_return() && self.routine_stack().is_empty() && self.nested_run_depth == 0 {
    let inner_err = RuntimeError::controlflow_return(true);
    ...
    return Err(inner_err);
}
```

`self.nested_run_depth` (incremented by `run_with_scratch_registers`, the
helper that runs a nested, ephemeral `CompiledCode` — used by `EVAL`,
`dies-ok { }`/`throws-like { }` blocks, and other nested-execution sites) is
required to be `0`. Inside `EVAL`'s nested run, this is never true, so the raw
`is_return()` error is passed through unconverted at every level, including
past the `try`/`CATCH` (which — per the existing `CATCH` implementation —
only recognizes a `RuntimeError` that already carries a typed `.exception`
Value, not a bare `is_return()` control-flow marker). It only gets converted
at the *true* top level of the whole program (`nested_run_depth == 0` again,
after every nested-run frame has unwound and decremented it back to 0) — by
which point it is outside any `CATCH` and simply aborts.

**The guard is not a bug in isolation — it was added deliberately** (commit
`547422bab`, "fix: five pipeline bugs blocking Cro::HTTP's request/response
parsers", item 3): converting a `CX::Return` to `X::ControlFlow::Return`
whenever `routine_stack().is_empty()` — without the `nested_run_depth` guard —
wrongly fired *inside* nested runs whose escaping `return` was actually meant
for a live outer **VM call frame** that never pushes onto the legacy
tree-walk-era `routine_stack`. So `routine_stack().is_empty()` alone is not a
reliable answer to "is there truly no enclosing routine anywhere on the
dynamic call stack" once VM call frames and the tree-walk `routine_stack` can
disagree.

## Why this needs design, not a quick patch

The real fix has to distinguish, from *inside* a nested run:

1. "This `return` is meant for a live VM call frame that exists somewhere
   outside the nested run" (must NOT be converted — must keep propagating
   until it reaches that frame's own handling), from
2. "This `return` has genuinely run out of every enclosing routine/call frame,
   nested run or not, and needs to become a catchable `X::ControlFlow::Return`
   right here so the nearest `try`/`CATCH` can see it."

`self.routine_stack().is_empty()` is the (currently unreliable, per the
commit above) proxy for (1)/(2) at top level; `nested_run_depth == 0` is a
blunt "only ever answer (2) outside all nesting" workaround that is correct
for the top-level-abort case this repo currently has tests for, but wrong for
the EVAL/throws-like case this ticket found. A correct fix likely needs to
walk the actual VM call-frame stack (not just the legacy `routine_stack`) to
answer "is there a real enclosing routine frame above the current nested-run
boundary", not just "is `nested_run_depth` zero". That is exactly the kind of
dynamic-call-stack-vs-legacy-stack disagreement CLAUDE.md's "Working
agreements" section warns to route through the compiler/VM rather than
special-casing — and the existing guard's own history (a previous version of
this exact check already caused a real regression once) means a change here
needs to be checked against both the Cro streaming-parser fix that motivated
`nested_run_depth` in the first place (`t/tap-callback-nonlocal-return.t`, the
`S17-supply` suite) and this ticket's own repro, not just one of the two.

## Where this was found

`todo/deep/vendor-real-test-module.md`'s ongoing campaign — this is the
last of the 6 individual assertion gaps still open in
`roast/S32-exceptions/misc.t` under `MUTSU_REAL_TEST=1` that traces to
`X::ControlFlow::Return` (`throws-like 'gather { return  1}',
X::ControlFlow::Return`, line 280).

## Suggested next step

Before touching `vm_run_loop.rs`'s conversion check, write a small matrix of
repros covering both directions (return meant for an outer VM frame across a
nested run — must NOT convert early; return with truly nothing above it
inside a nested run — must convert and be catchable) and pin each with a `t/`
test, so a fix can be validated against both without relying on `make roast`
alone to notice a regression in the first direction.
