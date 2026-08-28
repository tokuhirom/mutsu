# A `multi` is a dispatcher even when it has nowhere to defer to

`roast/S06-multi/redispatch.t` test 9 — "It's ok to call `nextsame` in the
last/only candidate" — was one of the whitelisted files that regressed under
`MUTSU_REAL_TEST=1`, the switch from mutsu's native Rust `Test` provider to the
real vendored `modules/Rakudo-Core/lib/Test.rakumod`. It failed with

```
nextsame is not in the dynamic scope of a dispatcher
```

## The real root cause

mutsu's dispatcher stacks were built on the premise that a dispatch frame is
worth pushing only when there is a *next* candidate to defer to. Both places
that push a multi-dispatch frame encoded that premise:

* `Interpreter::push_multi_dispatch_frame` (`src/runtime/accessors_state.rs`)
  bailed out with `if all_candidates.len() <= 1 { return false }`, and then
  pushed nothing more when the winner filter left `remaining` empty.
* `Interpreter::call_function_fallback`
  (`src/runtime/builtins_operators_fallback.rs`) carries an inlined copy of the
  same logic, gated on `!remaining.is_empty()`.

Rakudo draws the line somewhere else. Being a `multi` is what establishes a
dispatcher; having somewhere to defer to is a separate question, answered
afterwards with `Nil`. Measured against rakudo, all four verbs — `nextsame`,
`callsame`, `nextwith`, `callwith` — are legal in the last (or only) candidate
and evaluate to `Nil`; `lastcall` is a live no-op there and `nextcallee` yields
`Nil`. `X::NoDispatcher` is reserved for code that is not in a dispatcher's
dynamic scope at all: a plain (non-`multi`) `sub`, or the mainline.

Because no frame was pushed, `dispatch_next_candidate` fell past the wrap,
method and multi branches all the way to its final `Err(no_dispatcher_error)`.
The multi branch itself already had the right answer — its
`matched_idx == None` arm returns `Nil` — it just never ran.

## Two gaps, not one

The frame-push guard alone explains a `multi` called **by name**. The roast
assertion also needs the second, independent gap: a routine invoked through a
**Callable value** (`lives-ok &e`, `sub s(&c) { c() }`, `my &c = &e; c()`)
resolves through `call_sub_value` -> `call_function` -> `call_function_fallback`,
whose inlined copy of the guard was never updated. That path dropped the
dispatcher for *every* multi, one candidate or many — a two-candidate multi
called through an `&`-parameter lost its frame just the same. Fixing only one of
the two left the assertion red.

The fix makes both sites say the same thing: push the frame whenever the name
has multi candidates at all (with an empty `remaining` when there is nothing to
defer to), and push nothing when it has none, so a plain `sub` still throws.
`push_multi_dispatch_frame` keeps a dedicated single-candidate fast path that
skips the winner resolution and the rw-param capture — with one candidate there
is nothing to filter out and no rw value to chain forward.

## What the assertion's wording described

Nothing that mattered. "It's ok to call `nextsame` in the last/only candidate"
is a true statement about the spec, but "last" was never the trigger: `nextsame`
in the *last of two* candidates already worked, because two candidates meant a
frame got pushed. The two things that actually broke it were "only" (one
candidate, hence no frame) and, invisible in the assertion's text, "called
through a Callable value" — which is simply how the real `Test.rakumod`'s
`lives-ok` invokes the code it is handed (`try { $code(); 1 }`). That is the
recurring shape of this campaign's residue: the failing assertion names a
feature, and the bug is in the plumbing the real module happens to use to reach
it.

## Result

| file | real Test before | real Test after | native before | native after |
| --- | --- | --- | --- | --- |
| `roast/S06-multi/redispatch.t` | 1 failure (#9) | **PASS** | PASS | PASS |
| `roast/S06-multi/subsignature.t` | 1 failure (#66) | **PASS** | PASS | PASS |

`subsignature.t` carries the identical assertion at test 66. Its remaining
`not ok` lines (4 and 43) are `# TODO`-marked expected failures both before and
after, and are unrelated to this change.

Pin: `t/nextsame-in-the-only-candidate.t` (37 assertions, green under real
`raku` as well as mutsu) — the four verbs, every call position through an
`&`-parameter (tail, sink, assigned, `try {}`, `try` expression, bare block,
`do` block, nested anonymous sub), the by-name control, `&`-variable and scalar
invocation, live redispatch in a multi-candidate multi, single-candidate and
inherited methods, and the `X::NoDispatcher` cases that must still throw.

One neighbouring bug found in the same area was deliberately **not** folded in,
because it has a different root cause (argument matching, not the dispatcher
stacks): an anonymous `Any` parameter never matches, so `multi f(Any)` is dead
code. Filed as
`todo/tickets/anonymous-any-parameter-never-matches-in-multi-dispatch.md`.
