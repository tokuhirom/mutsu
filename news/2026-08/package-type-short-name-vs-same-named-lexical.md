# A same-named lexical no longer shadows its own package-scoped class

`roast/S12-construction/autopairs.t` passed under the native `Test` provider and
failed under `MUTSU_REAL_TEST=1` with

```
# Error: Unknown method value dispatch (fallback disabled): new on Tb
```

The test's *name* is a red herring twice over: the failure has nothing to do
with autopairs (`:$a`) and nothing to do with the space in `:$a )` that
distinguishes the failing subtest from its passing sibling. It also has nothing
to do with `Test`. What the real `Test.rakumod` changes is only *where the
snippet runs*: `eval-lives-ok` routes through `eval_exception`, a sub of a
separate compilation unit, so the `EVAL`'d `class Tb { … }` is compiled while a
module's routine is on the stack and is therefore registered under **that
module's package**. Rakudo agrees with that placement (`Test::Tb`); what mutsu
got wrong was every subsequent reference to the short name.

## Root cause: `env` is the only bridge from a short type name to its package

A type declared inside a package is registered under its qualified name only
(`M::C`). mutsu made the short name `C` work by relying on an `env` alias — but
`env` stores a `$`-sigiled scalar under its **sigil-stripped** key, so `C` and
`$C` are the same env slot. A `my C $C` declaration therefore *overwrote the very
alias* that made its own type name resolvable, and every short-name reference
afterwards degraded to a bare, never-registered `C` type object with no methods.
`my C $C .= new(...)` is the fused form, which calls `.new` on the **bareword**,
so it died on exactly that dead object.

Reduced, with no `Test` anywhere (`EvHelper` is any module with a sub that
`EVAL`s its argument):

```raku
eval-in-module 'class N1 { has $.a }; my N1 $N1 .= new(:a(1))'   # died
eval-in-module 'class N2 { has $.a }; my N2 $z  .= new(:a(2))'   # lived
```

Three distinct sites all resolved the short name through `env`, and all three had
to grow the same registry fallback:

1. **The declaration seed.** `nominal_type_object_name_for_constraint` returned
   the constraint's short spelling once the `env` alias was gone, so even the
   *other-named* control stored a dead `Package("C")` — `my C $z; $z.^name` said
   `C` where rakudo says `M::C`. It now falls back to `package_type_alias` and
   the running package's chain.
2. **The block-entry hoist.** `hoist_typed_var_decls` emits a `SetVarType` for
   every top-level `my TYPE $x` *before* the `class` statements of the same
   block, so it seeded from a constraint that was not resolvable yet and the
   later, real declaration saw a non-`Nil` value and left the dead seed in place.
   `exec_set_var_type` now treats such a value as uninitialised: a `Package`
   naming a type that exists nowhere is never a legitimate value (no assignment
   can produce one), and re-seeding an already-correct value is a no-op because
   the seed is a pure function of the constraint. Deleting the hoist's seeding
   instead was tried and rejected — it is load-bearing for `my Int $Int`, whose
   `env` slot would otherwise keep the `Package(Any)` declaration placeholder.
3. **The bareword read.** `GetBareWord`'s three "this name is a type" branches
   spelled the resolution as `package_type_alias(name)` *or the name itself*,
   which cannot reach a package-scoped registration; and its `Package(Any)`
   placeholder guard — the rule that a not-yet-assigned `my $Buf` must not shadow
   `Buf` — tested only `has_type_direct`, so a lexical `class` inside a routine
   (registered under an ADR-0047-mangled qualified key) still lost to the
   placeholder and `my C $C .= new` built an `Any` instance whose `.raku` read
   `Any.new`. Both now go through one `resolve_bareword_type_name` probe that is
   deliberately blind to `env[name]` — that key is the contested one — and
   consults the registry, import aliases, and the running package's chain.

## Measured against rakudo

| snippet, run from a module sub's `EVAL` | rakudo | mutsu before | mutsu after |
| --- | --- | --- | --- |
| `class C { has $.a }; my C $C .= new(:a(1))` | lives | **dies** | lives |
| `class C { has $.a }; my C $z; $z.^name` | `M::C` | `C` | `M::C` |
| `class C { has $.a }; my C $C; C.^name` | `M::C` | `C` (0 methods) | `M::C` |
| `my Int $Int; $Int.^name` | `Int` | `Int` | `Int` |

and, in a module's own routine (no `EVAL` at all), `class C { … }; my C $C .= new(:a(7))`
went from dying to producing a real `M::C` whose `.^name`, `.WHAT` and `.raku`
all agree with rakudo.

## Result

`roast/S12-construction/autopairs.t` now passes under **both** providers
(4/4 native, 4/4 `MUTSU_REAL_TEST=1`), taking the real-`Test` regression list
down by one. Pinned by `t/package-type-short-name-vs-same-named-lexical.t`
(35 assertions, green under real `raku` too): the same-named typed declaration
at mainline, in a plain sub, in a module sub, and inside an `EVAL` from each of
those, plus the other-name / untyped / nested-block / builtin controls.

A second, unrelated divergence noticed in the same file — a punned-role
instance's `.raku` dropping its attributes (`Tc.new` vs `Tc.new(a => Any)`) —
gates nothing there and is filed as
`todo/tickets/punned-role-raku-drops-undefined-attributes.md`.
