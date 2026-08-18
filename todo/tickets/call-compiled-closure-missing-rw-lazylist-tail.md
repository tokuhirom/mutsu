# `call_compiled_closure` has no equivalent of the tree-walk branch's `is rw`/LazyList return-value post-processing

## Summary

The tree-walk closure-call branch in `call_sub_value`
(`src/runtime/resolution_call_sub.rs`, tail around line 1058-1103) does two
things to the closure body's return value before handing it back to the
caller:

1. Rebuilds a returned `LazyList`'s env with
   `__mutsu_preserve_lazy_on_array_assign` set.
2. Calls `self.maybe_fetch_rw_proxy(v, data.is_rw && !data.is_raw)`, which
   wraps the value in an rw proxy when the closure/sub is declared `is rw`.

`call_compiled_closure`/`call_compiled_closure_with_topic`
(`src/vm/vm_closure_dispatch.rs`) has no equivalent anywhere — it returns
straight from `finalize_return_with_spec` at the end of
`call_compiled_closure_with_topic` (around line 1448-1459). Confirmed via
`grep -n "fetch_rw\|is_rw\|maybe_fetch_rw_proxy\|LazyList"
src/vm/vm_closure_dispatch.rs`, which returns nothing.

## This is not hypothetical for the *existing* code — it already affects the `compiled_routine` fork

`call_sub_value` already has an earlier fork that goes straight to
`call_compiled_closure` and returns immediately, bypassing this tail entirely:
the `data.compiled_routine.is_some()` branch at
`src/runtime/resolution_call_sub.rs` around line 417-431. Any `is rw`
routine-Sub invoked as a first-class value through that fork today already
loses `maybe_fetch_rw_proxy` handling. This ticket is about the general gap in
`call_compiled_closure` itself, not a new problem introduced by any pending
change.

## Status: gap identified by static audit, not yet reduced to a failing test

This was found by an audit comparing the tree-walk branch against
`call_compiled_closure` line-by-line while investigating whether
`call_sub_value`'s general branch could safely be routed through
`call_compiled_closure` (see
`todo/deep/eval-block-value-recompiles-every-call.md`). No concrete
roast/`t/` repro has been constructed yet that demonstrates an observably
wrong result — the gap is confirmed by code inspection (the tail simply does
not exist on the compiled path), not by a failing assertion. Constructing a
repro (an `is rw` block/routine Sub invoked as a first-class value whose
return should be an rw proxy, or a lazy sequence returned from a block called
via `.()`/a wrap chain) is the next step before fixing this.

## Fix direction

Add the same tail logic (LazyList preserve-on-array-assign,
`maybe_fetch_rw_proxy`) either:

- as a thin wrapper applied at the call sites that invoke `call_compiled_closure`
  in `call_sub_value` and `vm_call_on_value` (`src/vm/vm_dispatch_helpers.rs`), or
- inlined into `call_compiled_closure` itself, so every existing caller
  (including the two `compiled_routine` fork sites and `vm_call_on_value`'s own
  fast path) benefits without each call site having to remember to add it.

The second option is probably right long-term, but check whether
`maybe_fetch_rw_proxy`'s `data.is_rw && !data.is_raw` condition is safe to
apply unconditionally to every `call_compiled_closure` caller (e.g. map/grep
callback invocations, which also route through this function) or whether it
needs to stay opt-in per call site.

## Update (2026-08-18): still not reduced to a failing repro

Tried several shapes that plausibly route through `call_compiled_closure`
without the tree-walk tail, all matched `raku` correctly (so not the gap, or
mutsu takes a different path that happens to still apply the rw/LazyList
handling):

- `my &f = sub () is rw { $x }; &f() = 42;` (anon `is rw` block as a
  first-class value) — correct in both.
- A `sub` returning a `LazyList` (`.map` over a `Range`) via a first-class
  code var, mutated after binding into `@a` — correct in both.
- `sub f() is rw { $x }; my &g = &f; (&g)() = 7;` (named `is rw` sub
  rebound to a code var, invoked via `.()`) — correct in both.

Still an open static-audit finding, not a confirmed bug — whoever picks this
up next should try invoking specifically through the `data.compiled_routine.is_some()`
fork this ticket names (`src/runtime/resolution_call_sub.rs` ~417-431) with an
explicit trace/breakpoint to confirm THAT fork is actually taken for one of
these shapes, rather than guessing black-box from the `raku`-vs-mutsu output
alone — the negative results above don't rule out the gap, they just didn't
happen to hit the code path in question.

## Update (2026-08-18): breakpoint at the fork itself never fires for either `&g()` or `(&g)()`

Set an unconditional `rust-gdb -batch` breakpoint directly at
`src/runtime/resolution_call_sub.rs:439` (the `if data.compiled_routine.is_some()`
line) and ran both:

```raku
my $x = 1;
sub f() is rw { $x }
my &g = &f;
&g() = 42;    # variant A
(&g)() = 42;  # variant B
say $x;
```

The breakpoint was **never hit** for either variant — the program ran to
completion and printed `42` (correct) without ever stopping. So neither
`&g()` nor `(&g)()` even reaches `call_sub_value` at all, let alone this
fork; the call must compile to a direct VM call opcode
(`vm_call_on_value`/a dedicated call-on-code-value path) that has its own
rw-proxy handling, separate from `call_sub_value`'s tree-walk tail this
ticket is about.

This narrows the search: a *confirmed* repro needs a call path that
genuinely funnels through `call_sub_value` (not a direct `&sub()` call) AND
lands in the `compiled_routine.is_some()` fork specifically — grep
`src/runtime/resolution_call_sub.rs:439`'s callers list above (map/grep/sort
callbacks, `.first(&pred)`, module export subs, `EVAL`-installed routines,
`Promise`/`Supply` tap callbacks) for a shape where the callback is 1) a
*named*, already-declared routine (so `compiled_routine` is populated,
unlike an anon block) that is 2) `is rw`, and 3) whose return value is used
in an lvalue position by the caller. `is rw` map/grep callbacks are unusual
in Raku (the callback return isn't normally assigned-to), so a real-world
trigger may not exist — worth reconsidering whether this is a live gap at
all versus dead code on an unreachable combination, before sinking further
gdb time into it.
