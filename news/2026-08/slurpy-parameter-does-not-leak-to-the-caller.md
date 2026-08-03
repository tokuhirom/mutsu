# A callee's slurpy parameter no longer overwrites the caller's

`roast/S24-testing/fails-like.t` aborted under the real `Test` module with

```
No such method 'instead' for invocant of type 'X::Match::Bool'
```

which reads like a missing exception attribute — the family
`news/2026-08/typed-exceptions-carry-their-attributes.md` had just been through.
It is not. `X::Match::Bool` has no `.instead` in rakudo either; the test asks
`throws-like`'s matcher loop for `.message`, and mutsu asked for `.instead`
because by the time the loop ran, `throws-like`'s `*%matcher` held the matcher of
a *different* routine.

## What happened

`Test.rakumod` nests two routines that both end their signature with the same
slurpy named parameter:

```raku
sub throws-like($code, $ex_type, $reason?, *%matcher) { ...
    subtest { ... CATCH { default { ... for %matcher.kv -> $k, $v { $ex."$k"() } ... } } }
}
sub fails-like(\test where Callable:D|Str:D, $ex-type, $reason?, *%matcher) { ... }
```

The test runs `throws-like` with `message => *.contains('instead')` over code
that calls `fails-like(..., :instead)`. `fails-like` throws, `throws-like`'s
`CATCH` runs — and `%matcher` there was `{instead}`, the callee's.

The tree-walk return merge in `call_function_def` propagates the callee's
variables back into the caller's env for every name the caller also has
(`routine_writeback_excluded_names` lists what to leave behind). `@`/`%`
parameters were deliberately *not* excluded, because such a parameter is
normally the caller's own container and its mutations have to be visible after
the call. A slurpy is never that container: `bind_function_args_values` builds a
fresh `Array`/`Hash` out of the leftover arguments, so the merge could only ever
clobber an unrelated caller lexical of the same name. Slurpy (`*@a`, `*%h`) and
non-flattening slurpy (`**@a`) parameters are now excluded; a plain `%h`
parameter still writes back.

## Why it only showed up in a module

A sigilless parameter (`\test`) keeps a module sub off the OTF-compiled call
path, so `fails-like` runs through this tree-walk fallback in the first place.
The same two routines written in a plain script never reach the merge and never
showed the bug — which is why the signature, not the module, is what the
bisection kept pointing at.

Pin: `t/slurpy-param-does-not-leak-to-caller.t` (with `t/lib/SlurpyLeak.rakumod`).
`roast/S24-testing/fails-like.t` now passes under `MUTSU_REAL_TEST=1`.
