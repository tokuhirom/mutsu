# A block's match does not reach its defining scope when a routine invokes it

A bare block shares its enclosing routine's `$/` — it is not a routine, so it
has no implicit `my $/` of its own. That stays true when the block is passed to
another routine and invoked there: the block was *written* in the caller's
scope, so the match it performs must be visible in that scope afterwards.

## Minimal repro

```raku
sub call-it(&c) { c() }
"abc" ~~ /(b)(c)/;
call-it({ "yy" ~~ /(y)/ });
say ~$/;      # mutsu: bc     rakudo: y
```

Directly inlined, mutsu gets it right — `{ "yy" ~~ /(y)/ }` at the mainline, or
inside the same routine, does publish to the enclosing scope (pinned by
`t/match-vars-are-routine-scoped.t`, "a bare block writes its enclosing routine
$/"). It is specifically the round trip through a `&`-parameter and a call from
inside another routine that loses it.

## Not caused by the routine-scoping fix

Verified by reverting `runtime::utils::is_routine_scoped_implicit_var` to its
pre-fix body (`name == "!"`) and rebuilding: the assertion fails identically, so
this predates `news/2026-08/match-vars-are-routine-scoped.md`. The routine-side
exclusions are gated on `cf.code.is_routine`, which is false for a block, so
they are not what drops the write.

## Where to look

The closure invocation path (`Interpreter::call_compiled_closure` /
`call_compiled_closure_with_topic` in `src/vm/vm_closure_dispatch.rs`) and how
its env is merged back. The block runs with the invoking routine's env in place,
so its `$/` write most likely lands in that routine's frame and is discarded
with it, instead of reaching the env the closure captured.

## Why it matters beyond `$/`

Any implicit write a block makes to a variable of its *defining* scope has the
same question mark over it when the block is invoked from another routine. `$/`
is simply the case with a crisp rakudo-verified expectation. No roast file in
the current `MUTSU_REAL_TEST=1` residue gates it.
