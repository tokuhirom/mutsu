# A user `sub Int(...)` no longer shadows the built-in `Int(...)` coercion call

Declaring a sub with the same name as a core type coercer took over the bareword
call form too, so the built-in coercion became unreachable:

```raku
sub Int(Str $s){'what?'};
say [Int, Int('42'), &Int('42')];
# raku:  [(Int) 42 what?]
# mutsu: [(Int) what? what?]
```

In Raku the bareword `Int('42')` is the *type's* coercion regardless of what
routines are in scope; only the explicitly `&`-sigiled `&Int('42')` reaches the
declaration. This holds for user classes as well — `class Foo {}; sub Foo($x)
{...}; Foo(3)` dies with `X::Coerce::Impossible` in rakudo rather than calling
the sub.

## Root cause

The two call forms already compile to *different* opcodes in mutsu — the
bareword to `CallFunc`, the `&`-sigiled form to `CallOnCodeVar` — but the
`CallFunc` funnel resolved user subs before it ever reached `call_function`'s
coercion arms.

## Fix

`exec_call_func_op` now gates a bareword call to a core type's name straight to
the coercer, ahead of every user-sub resolution path. The gate sits after
junction auto-threading (so `Int(1|2)` still threads) and after the arguments
have been materialized, and is safe with respect to the name-keyed light-call
caches above it: those are only ever populated further down, past the gate, so
they can never hold one of these names.

The gated set is exactly the names `call_function` implements a coercion arm
for — `Int Num Str Bool Uni Rat FatRat Complex Real Numeric Array List Hash Set
SetHash Bag BagHash Mix MixHash` — and only when the call has arguments, so a
bare `Int` term is untouched. It is deliberately not extended to arbitrary user
class names: rakudo treats those as coercions too, but mutsu has no coercion
protocol for them yet, so those names keep resolving to whatever routine is
declared.

Pinned by `t/numeric-coercion-gaps.t`.
