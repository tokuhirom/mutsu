# A package-qualified call no longer silently becomes a builtin

`call_function_fallback` ended with a package-prefix strip: an unresolved
`Foo::bar(…)` retried as `bar(…)`. That retry exists for a real reason — it is
how a call qualified with a package mutsu never registered still finds its own
routine — but it was unconditional, so the qualifier was simply discarded and
the call landed on Raku's same-named builtin:

```
$ mutsu -e 'say Foo::Bar::index("hello", "l")'
2                                    # raku: Could not find symbol '&index' in 'GLOBAL::Foo::Bar'
$ mutsu -e 'use Test; Test::ok(1)'
ok 1 -                               # really emitted a TAP assertion
```

This is the wider half of the shape fixed for `nqp::` in #5450: there the
consequence was a silent wrong answer (`nqp::index` returning Raku's `Nil`
instead of nqp's `-1`), here it is a call that should not have resolved at all.

## The fix

The strip now runs only when mutsu has something **declared** under the short
name — a routine (proto, multi, wrap chain, `&name` in env, or a plain
`resolve_function_with_alias` hit), or a class/role/subset/enum for the
`Foo::Bar("x")` coercion leg. Otherwise the call fails the way raku fails it.

The guard deliberately asks *"is something declared here"* rather than *"is this
a builtin"*. The builtin question has no reliable answer: `index` is dispatched
by a hand-written arm of `call_function` and is not in `BUILTIN_FUNCTION_NAMES`,
so `is_builtin_function` misses it — which is exactly why the earlier `nqp::`
fix could not simply be generalised, and why this ticket stayed open.

The error mirrors rakudo's, including the parts that are not obvious:

| program | message |
| --- | --- |
| `Foo::Bar::index("hello","l")` | `Could not find symbol '&index' in 'GLOBAL::Foo::Bar'` |
| `module M { }; M::nope()` | `Could not find symbol '&nope' in 'M'` |
| `GLOBAL::index("hello","l")` | `Could not find symbol 'index' in 'GLOBAL'` |
| `module Foo {…}; GLOBAL::Foo::f()` | `Could not find symbol 'f' in 'Foo'` |

A package raku knows is named bare; one it has never seen is reported under
`GLOBAL::`. An explicitly written `GLOBAL::` qualifier resolves through the
pseudo-package, and raku then drops the `&` sigil from the symbol — all four
rows above were read off rakudo, and mutsu now reproduces them exactly.

## What is deliberately unchanged

- **The coercion leg.** `class Bar {…}; Foo::Bar("x")` still coerces, where raku
  says "Could not find symbol '&Bar' in 'GLOBAL::Foo'". mutsu registers some
  imported and nested classes under their short name only, so the strip is
  load-bearing for types in a way it is not for routines.
- **A builtin still wins over a user routine reached through the strip.**
  `sub index($a,$b) {99}; M::index("hello","l")` gives 2, not 99: the retry goes
  through `call_function`, whose builtin arm is hit before the fallback's user
  resolution. That is the pre-existing builtin-shadow ordering, not this change;
  raku rejects the whole program anyway (`M` is not a package).

## Verification

`t/qualified-call-does-not-alias-builtin.t` (11 subtests) pins both directions:
the six error shapes above, and the regression guards that the strip still
resolves what it exists for — a qualified `our sub`, one two package levels
deep, a qualified multi, and a qualified call reached through `EVAL`. It
**passes unchanged under rakudo**, so it is a differential test rather than a
record of mutsu's own output.
