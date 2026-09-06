# A `Seq` operand of `eq` skipped its elements' own `Str`

`@objs eq "a b"` was `True` and `@objs.Seq eq "a b"` was `False`, for the same
elements — objects of a class that defines its own `method Str`. So was
`@objs.Seq eq @objs`, and so was every comparison whose two sides differed only
in being an Array on one side and a Seq on the other.

## Cause

`coerce_stringy_operand` (`vm/vm_coerce_concat_ops.rs`) is the string-context
coercion every `~` and `eq`/`ne`/`lt`/… operand goes through. It does two
things that a list of objects needs, in this order:

1. a `Seq` still holding a deferred source is reified, so string context sees
   its elements rather than the opaque `(...)` placeholder;
2. an `Instance` element that defines its own `Str` is replaced by the string
   that method returns, because the pure renderer downstream cannot dispatch a
   user method.

Step 1 `return`ed. A `Seq` therefore never reached step 2, while an Array —
which never takes step 1 — always did. The two sides of the comparison were
rendered by different stringifiers: one by the element's `Str`, the other by
the `ClassName()` fallback.

The fix is to let the reified value fall through to step 2 instead of returning
it.

## Where it showed up

`roast/integration/advent2009-day20.t` under the vendored upstream
`Test.rakumod` (`MUTSU_REAL_TEST=1`), where

```raku
is @b, (@people.sort: { +.karma }), 'Sort explicitly numerically';
```

failed with `expected:` and `got:` diagnostics that printed *identically* —
because `is` in rakudo's Test is `$got eq $expected`, and the diagnostic that
follows is a separate `.raku()` render that does not go through the coercion.
`@b` is an Array, the right-hand side a Seq, and `Person` defines
`method Str { "$.name ($.karma)" }`.

It was equally wrong under mutsu's native provider — `@objs.Seq eq "..."` is a
plain Raku expression — but the native `is` compares by a different route, so
nothing in the suite caught it. The new assertions in
`t/list-str-calls-element-str.t` go through `eq` directly for that reason.

This was one of two real-provider regressions in the 2026-09-06 roast sweep for
`todo/deep/vendor-real-test-module.md`; it also closed
`t/list-str-calls-element-str.t`'s real-provider failure in the `t/` sweep.
