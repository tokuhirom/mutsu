# An imported `constant` type alias is accepted as a type again

A `constant` bound to a bare type name is a **type alias**: it binds a type
object into the lexical scope, and Raku accepts it anywhere a type name goes.
C bindings lean on this heavily — `Gnome::N` writes
`constant \GType is export = uint64` and then spells whole signatures in terms
of it.

mutsu imported such an alias correctly as a *value* (`MyInt.^name` was already
`Int`) but lost its type-ness on the way, so every "is this name a type"
validator rejected it ([#8131](https://github.com/tokuhirom/mutsu/issues/8131)):

```raku
# lib/GT3.rakumod
unit module GT3;
constant MyInt is export = Int;
```

| construct, with `use GT3` in scope | before |
| --- | --- |
| `sub f(MyInt $x)` | `Invalid typename 'MyInt' in parameter declaration.` |
| the same inside a nested block | same |
| `role R { method m(MyInt $x) }` | same |
| `my MyInt $v = 3` | `Package 'MyInt' is insufficiently type-like to qualify a variable.` |
| `class C { method m(MyInt $x) }` | **worked** |

`raku` accepts all five. Declaring the alias in the same compilation unit
worked, because the sub pre-pass accepts the name out of `declared_types` — the
unit's statically gathered declarations — which is exactly what an import does
not populate.

## Three separate causes, one per validator

**The compile-time sub pre-pass never saw the alias.** It runs before the
mainline, so a `use`d module has not been loaded; `collect_use_declared_type_names`
compensates by scanning the module's *source* for declaration keywords. Its list
was `class`/`role`/`grammar`/`enum`/`subset` — `constant` was not on it, and
could not simply be added: alias-ness is decided by the right-hand side, not by
the keyword (`constant TAU = 6.28` names a value and must keep being rejected as
a parameter type), and the name may be spelled sigillessly with traits in
between (`constant \GType is export = uint64`). A companion scan now records
exactly the aliases whose whole initializer is a single identifier the
interpreter already recognises as a type — including one the same module
declares, which the declarator scan has just collected.

**`is_resolvable_type` had no alias-following step at all.** It resolves a
lexical `my class`/`my role` through `resolve_bare_type_name`, but that helper
insists the target be a class or role, so an alias to a builtin (`Int`) or a
native type (`uint64`) did not survive it. It now follows the alias and answers
for its target, which is what makes the role-method validator accept the name
once the module is loaded. The walk is bounded, so a pathological
`constant A = B; constant B = A` pair reports "not an alias" rather than
spinning; `is_type_alias_constant` (which did a single step of the same walk)
now shares it.

**The variable-declaration validator asked the wrong question first.** At
runtime `env` genuinely holds a `Package` under `MyInt`, so `is_declared_package`
matched and produced the "insufficiently type-like" message before anything
considered that this `Package` is an alias with a resolvable target. The
constraint is now resolved to its target once, up front, so everything
downstream sees `Int`: the known-type check runs, and the failure message names
the target the way rakudo does (`expected Int but got Str ("x")`). Any smiley
rides along (`my MyInt:D $v`), and the resolution is gated on the constraint not
already being a known type, so the overwhelmingly common `my Int $x` pays a
static `matches!` rather than an env lookup per typed declaration.

## Deliberately unchanged

`constant TAU = 6.28` used as a parameter type stays rejected. rakudo accepts it
(any term may constrain a parameter, as a value smartmatch) and mutsu does not —
a pre-existing divergence, and the in-unit collector draws the same line
on purpose, so widening it here would have been a second change hiding inside
this one.

Pinned by `t/modules/import-export/imported-constant-type-alias.t` (12
assertions, green under `raku` as written) with a `t/lib` fixture carrying all
three shapes an alias comes in: to a builtin, to a native type spelled
sigillessly, and to a class the same module declares.
