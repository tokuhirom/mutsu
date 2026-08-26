# A metaobject passed as a HOW method's introspected object now stands for the type its receiver describes

`Language/structures.rakudoc`'s own worked "Introspection" example passes a metaclass
to itself:

```raku
my $metadata = "random object".HOW;
say $metadata.can($metadata, "uc");
```

mutsu answered `()`. The ticket framed this as a `.can` bug, but measuring against
`raku` showed the gap was **every** `ClassHOW` method that takes the introspected
object as an argument, not just `can`:

| call | raku | mutsu (before) |
| --- | --- | --- |
| `$m.can($m, 'uc')` | `(uc uc)` | `()` |
| `$m.name($m)` | `Str` | `Perl6::Metamodel::ClassHOW` |
| `$m.mro($m)` | `(Str Cool Any Mu)` | `(Perl6::Metamodel::ClassHOW Any Mu)` |
| `$m.lookup($m, 'uc')` | `Method` | `Nil` |

## Root cause

Rakudo's `Metamodel::MethodContainer` methods read `self`'s own cached MRO and method
table and **ignore the `$obj` argument entirely** — `Str.HOW.can(Int, "is-prime")`
answers Str's `2`, not Int's `1`. mutsu instead drove dispatch off the passed
argument. That is harmless while the argument is an ordinary value of the described
type (the usual `$obj.^can(...)` desugaring), and wrong the moment the argument is
itself a `Perl6::Metamodel::*HOW` instance: mutsu then introspected the *metaclass
class* rather than the type it describes.

## The fix

`Interpreter::how_dispatch_args` (`src/runtime/methods_native_bypass.rs`) now builds
the argument list for both HOW-dispatch entry points (`methods_call_dispatch.rs` and
the mutating `methods_mut_dispatch.rs`, which previously forwarded `args` verbatim).
It keeps the existing "no object passed at all → supply the receiver's type" rule and
adds: when the introspected-object slot holds a metaobject, replace it with the type
the *receiving* HOW describes.

The narrowing to a metaobject argument is deliberate rather than adopting Rakudo's
full ignore-`$obj` rule. mutsu's `.^mro` head is taken from the invocant's own `.WHAT`
precisely so that `(1 but R).^mro[0]` reports `Int+{R}` rather than the plain `name`
attribute cached on the HOW (see
`news/2026-08/role-instance-how-wrong-metaclass.md`); blanket-replacing the invocant
with the HOW's `name` would undo that. A metaobject in that slot can never be the
role-mixin case, so the two rules do not collide.

## Known remaining difference

`$metadata.can($metadata, "uc")` now answers `(uc)` where Rakudo answers `(uc uc)`.
That residual is not about the invocant at all — plain `Str.^can("uc")` is also `2` in
Rakudo and `1` in mutsu, because Rakudo's `Str` method table holds its own derived
`uc` proto alongside `Cool`'s. It is a builtin method-table modelling difference in
`.^can`'s candidate count, independent of this ticket, so the test asserts
`.elems >= 1` plus `[0].name eq 'uc'`.

Pinned by `t/metamodel-introspection.t`.
