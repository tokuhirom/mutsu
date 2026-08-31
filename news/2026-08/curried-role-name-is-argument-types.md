# A curried parametric role is named after its arguments' types

Rakudo names a curried parametric role after the *types* of its arguments;
mutsu stringified the argument values:

```raku
role R[Str:D $n] { }
say R["x"].^name;   # raku: R[Str]    mutsu: R[x]
```

It propagated into anything that embeds the curried name — notably the
`Language/mop.rakudoc` parameterization example, whose `^parameterize` builds a
new name from `roles.map(*.^name)`, so `Foo[R['x']].^name` read `Foo[R[x]]`
where rakudo reads `Foo[R[Str]]`.

## The fix

Five independent renderers each stringified the stored arguments themselves —
`to_string_value` (`value/display.rs`), `raku_value`
(`methods_0arg/raku_repr.rs`), the `.^name` fast path (`dispatch_caret_name`),
the `.WHAT` arm and the `CurriedRoleHOW` name (both `methods_introspect.rs`),
plus the `name` MOP handler (`methods_classhow_dispatch.rs`). They all route
through one new `value::parametric_role_name` helper now, so the spelling
cannot drift between them again.

The rule the helper implements is Rakudo's: an argument contributes its **type
name**. That reduces to the argument's own name when it already IS a type
object, so `R[Int]` keeps reading `R[Int]` — the case that already worked. A
curried role passed as an argument nests (`Outer[Inner['x']]` is
`Outer[Inner[Str]]`).

## What is deliberately left diverging

A **named** argument keeps its current value-based spelling. Rakudo drops named
arguments from the curried name entirely (`A[:a(1)].^name` is just `A`), but
mutsu's composition machinery keys on that string, so collapsing `A[:a(1)]` and
`A[:a(2)]` onto one name is not safe yet — `roast/S14-roles/parameterized-mixin.t`
composes exactly that pair. Left as a separate, narrower divergence.

## Coverage

`t/curried-role-name-is-argument-types.t` (14 assertions): a `Str` and an `Int`
argument, several arguments at once, all five renderings agreeing, a type-object
argument keeping its own name, a nested curried-role argument, the
`^parameterize` hook from `Language/mop.rakudoc`, and the non-regressions that
matter — the curried role still smartmatches its group, a composing class still
does the role, and the argument *value* still reaches the role body. The whole
file passes under `raku` as well as mutsu.
