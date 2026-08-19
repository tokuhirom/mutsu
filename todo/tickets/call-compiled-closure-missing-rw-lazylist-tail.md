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

## Update (2026-08-19): fork IS reachable (via `Promise.then(&named-sub)`), but no caller ever turns the result into a live rw/lazy repro — recommend closing

Built `target/debug/mutsu` fresh and tried nine more call shapes beyond the
two 2026-08-18 sessions, confirming each with an unconditional
`rust-gdb -batch` breakpoint at `src/runtime/resolution_call_sub.rs:163`
(function entry) and `:439` (the fork itself), not just black-box
raku-vs-mutsu output comparison:

- `&f.wrap(-> { callsame })` then bare `f() = 42` (named `is rw` sub with an
  active wrap chain) — correct in both, breakpoint at :163 **never hit**
  (wrap-chain named calls resolve through `vm_call_sub_value` /
  `vm_call_on_value`, not this `call_sub_value` entry, for this shape).
- `(1,2,3).map(&f)` with `@r[0] = 99` (named non-`is rw` routine callback,
  array-element reassignment) — correct in both (raku itself prints `1`, not
  `99`: `.map`'s result elements are not rw-forwarded to the source sub call
  at all), breakpoint never hit (native `.map` routes through
  `vm_call_map_block`, which calls `call_compiled_closure_with_topic`
  directly).
- `&f.assuming()` bound to `&g`, then `g() = 42` (named `is rw` sub via a
  curried/composed value) — correct in both, breakpoint at :163 never hit
  (the composed-value call goes through `vm_call_on_value`'s own dispatch,
  not `call_sub_value`).
- `%dispatch<a>()` where `%dispatch<a> = &f` (named `is rw` sub invoked via a
  hash-value call) — correct in both, breakpoint never hit (routes through
  the `CallOnValue` VM opcode, whose handler in `vm_call_func_ops.rs`
  applies its own `maybe_fetch_rw_proxy(result, sub_is_rw)` right after
  `vm_call_on_value` returns — reading `is_rw` straight off the *target*
  `Sub` value before dispatch, independent of which internal function
  actually executed the call).
- `(3,1,2).sort(&cmp_named)` (named comparator sub) — breakpoint never hit
  even at function entry (`:163`); native `.sort` doesn't call
  `call_sub_value` for this shape either.
- Two `lazy`-list variants (`lazy (1..*)` — genuinely infinite via
  `sequence_spec` — and `lazy (3,4,5)` — finite, lazy only via the
  `__mutsu_preserve_lazy_on_array_assign` marker) returned from a named sub,
  read through `f()`, `g()` (`my &g = &f`), and `(&g)()` — `.is-lazy` was
  `True` in all three call shapes, matching raku. This confirms the marker
  survives independent of whether `call_sub_value`'s tail re-inserts it: the
  `lazy` prefix sets the marker once on the `LazyList`'s own `env` at
  construction time (`dispatch_core_str.rs`), and that `env` travels with
  the `Value` through every dispatch path (VM-compiled or tree-walk) simply
  by being part of the cloned struct — the tail's unconditional re-insertion
  turned out to be redundant, not load-bearing.

**The one shape that DID hit the fork:** `Promise.new; $p.then(&f)` where
`&f` is a named routine (`sub f($p) is rw { $x }`). Backtrace confirmed the
call originates from `methods_promise.rs:104`
(`promise_chain_method`'s `on_resolve` callback), landing at
`resolution_call_sub.rs:439` with `data.compiled_routine.is_some()` true.
This proves the fork is not unreachable dead code in general.

But extending the repro to actually observe the gap
(`$p2 = $p.then(&f); ...; $p2.result = 42; say $x;`) shows **raku itself**
rejects the assignment (`Cannot modify an immutable Int (1)`) — `.then()`'s
resolved value is never rw-forwarded from the callback to `.result` in
Raku's own semantics, so there is no correct behavior for
`maybe_fetch_rw_proxy` to have produced here even if the tail ran. mutsu
also rejects it today (different message: `X::Assignment::RO: cannot assign
through .result on non-instance`), which is an unrelated, pre-existing
message-text mismatch, not the rw/LazyList gap this ticket is about.
`resolve_promise_callback` stores `cb_result` into the new `Promise`'s
internal result slot — Raku's `Promise` type simply does not expose a
mutable view onto that slot, so no caller of `call_sub_value` in this
family (Promise `.then`/`.on_resolve`, `subtest.rs`'s Supply-tap callbacks,
`methods_promise.rs:394`'s tap) has a way to turn the missing rw-proxy tail
into an observable difference: they all consume the callback's return value
internally (as an opaque `Promise`/tap result), never as something Raku code
assigns through.

**Recommendation: close this ticket.** Across two sessions (2026-08-18,
2026-08-18, 2026-08-19) and roughly 15 distinct call shapes, the "gap" holds
up only as a code-inspection observation, not a live bug:

- Every shape reachable from ordinary Raku call syntax (`&f()`, `(&g)()`,
  `.()`,  hash/array-element calls, `.wrap`, `.assuming`) never even reaches
  `call_sub_value` — it resolves through VM opcodes (`CallOnValue`,
  `CallOnCodeVar`) or `vm_call_on_value`'s own dispatch, both of which apply
  the rw-proxy check themselves, independently of `call_sub_value`'s tail.
- The one confirmed-reachable path (`Promise.then`) has no caller that
  exposes the callback's return value as an rw-assignable target, in either
  raku or mutsu — so `maybe_fetch_rw_proxy`'s absence there is unobservable
  by design, not a bug.
- The `LazyList` half of the tail also turned out to be redundant: the
  `__mutsu_preserve_lazy_on_array_assign` marker lives in the `LazyList`'s
  own `env`, set once where `lazy` is evaluated, and survives every call
  path because it's part of the cloned value, not something that needs
  re-applying per call.

If a future caller of `call_sub_value` is added that DOES expose a named
`is rw` routine's return value as a user-assignable lvalue (or a returned
`LazyList` needs the marker fresh rather than inherited), the fix direction
in this file is still the right one to reach for. Until then, sinking more
gdb time into this specific fork is not worthwhile.
