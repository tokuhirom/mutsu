# `but R(42)` composes as `Int+{R}`, not `Int+{R[Int]}`

`R(42)` in `but`/`does` position **initialises** the single public attribute of
an unparameterised role. mutsu recorded the argument as a *type argument*
instead, stamping the argument's type into the composed name:

```raku
role Answerable { has $.answer }
say ('Life' but Answerable(42)).^name;
# raku: Str+{Answerable}   mutsu (before): Str+{Answerable[Int]}
```

The behaviour was right — `.answer` read back `42` — but `.^name` is what `X::`
messages, `.raku`, `.gist` and every introspection print, so the spurious
parameterisation leaked into any program that printed a mixin's type.

## What changed

The two spellings are distinguishable exactly once, at
`extract_role_application` (`src/runtime/types/roles.rs`): `R[Int]` arrives as a
`ParametricRole` view (or, for a built-in parametric role with no `RoleDef`, as
a `Package` whose name is already bracketed), while `R(42)` arrives as a `Pair`
of the role name and its argument list. That fact is now carried through to
`compose_role_on_value` instead of being re-derived from the registry, where it
cannot be recovered: asking whether the role *declares* type parameters gets
`Associative[Int,Int]` wrong (a built-in parametric role has no declaration to
read). With the flag in hand:

- a parameterised role keeps its arguments in `__mutsu_role_typeargs__` and
  therefore in its name (`Int+{P[Int]}` is unchanged);
- an unparameterised role's argument is an attribute initialiser only, and
  records no type arguments at all.

That also fixed a second, quieter face of the same confusion: because the
ADR-0060 composed-`.WHAT` key includes the type arguments,
`(1 but R(2)).WHAT =:= (1 but R(3)).WHAT` was `False` where raku says `True`.

The mirror-image error was fixed in the same place. A genuine parameterisation
was *also* being consumed as an attribute initialiser, so
`role Q[::T] { has $.y }` had `Q[Str]` assign the type object `Str` to `$.y`:

```raku
say (1 but Q[Str]).y;   # raku: (Any)   mutsu (before): (Str)
```

Only the `R(v)` spelling initialises an attribute now.

Losing that accidental path exposed a gap it had been covering: a parameterised
role's attribute default may *reference* the parameter
(`role R[$v] { has $.attr = $v }`), and nothing bound `$v` for the mixin path --
`R[42]` had reached `$.attr` through the initialiser by coincidence, and the
defaulted spelling (`role R[$v = 7]`) had no path at all. The role's parameter
bindings, which the composition already computes for the mixin map, are now also
injected into the env around the attribute-default evaluation, beside the
`captured_env` merge that was already there for closure variables. Both
`(1 but R[42]).attr` (42) and `(1 but R).attr` (7) are right, for value and type
parameters alike.

With that assignment gone the attribute fell back to its declared default, which
exposed a third divergence in the same block: an uninitialised scalar attribute
composed by a mixin was `Nil`, where raku (and mutsu's own class-construction
path) gives the type object. `(1 but role { has $.x }).x` is `Any` again, and
a typed `has Int $.x` still narrows to `Int`; `@` and `%` attributes were
already correct at `[]` and `{}`.

Pinned by `t/role-mixin-init-value-is-not-a-type-parameter.t` — 26 assertions,
all measured against raku v2026.07 first: both `but` and `does`, an `Int` and a
`Str` base type, the attribute actually being initialised, the uninitialised
defaults for `$`/`Int $`/`@`/`%`, a parameterised role keeping its arguments,
a defaulted parameter not being spelled out, `.does` on both spellings, the
parameterised-*and*-attribute-carrying role, an attribute default that reads a
value or type parameter (supplied and defaulted), and the
`X::Role::Initialization` refusal when there is no attribute to initialise.

## Filed, not fixed here

Three neighbouring mixin divergences turned up while measuring and are recorded
rather than folded in — none is caused by this change and each reproduces
without it:

- `todo/tickets/mixin-roles-introspection-omits-the-mixed-in-role.md` —
  `(1 but A).^roles` lists the base type's roles only.
- `todo/tickets/two-identically-mixed-values-are-not-identical.md` —
  `(1 but A) === (1 but A)` is `False`.
- `todo/tickets/multi-role-mixin-name-joins-with-a-comma.md` —
  `((1 but A) but B).^name` is `Int+{A,B}` rather than raku's order-preserving
  `Int+{A}+{B}`.
- `todo/tickets/mixin-picks-the-wrong-role-group-candidate.md` — with two
  same-named role candidates, `1 but Z[Str]` composes the unparameterised one.
  The class-header path (`class C does Z[Str]`) already selects correctly.
