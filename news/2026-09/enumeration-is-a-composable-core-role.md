# `Enumeration` is a composable core role, not just an enum-value constraint

`class Foo does Enumeration { ... }` died as `X::InvalidType: Invalid typename
'Enumeration'`. Every other natively-modelled core role composed — `Dateish`,
`Positional`, `Associative`, `Numeric`, `Real`, `Callable`, `Stringy`,
`Iterable` all accept a `does` — because `Enumeration` existed in mutsu only as
a *type-check constraint* that enum values satisfy
(`runtime/utils/type_constraints.rs`, `types/type_matching.rs`,
`value/types_isa.rs`) and in neither list of composable core role names.

That is not a cosmetic gap. Rakudo's `Enumeration` is a real role with state —
`has $.key`, `has $.value` — and `raku-doc/doc/Type/Enumeration.rakudoc`
documents composing it from an ordinary class as a supported way to build a
constrained key/value pair (`class DNA does Enumeration`, whose `new` blesses
`key`/`value` itself). `Logic::Ternary` 0.0.4 does exactly that, which is how
the gap surfaced: its entry in the `invalid-typename` ecosystem cluster
([#7993](https://github.com/tokuhirom/mutsu/issues/7993)) was never a
typename-resolution problem at all.

## What it took

The role is now supplied as **real Raku source** (`ENUMERATION_ROLE_PRELUDE` in
`src/runtime/run.rs`), parsed once and spliced into any compunit that names it,
the way the `Rational`, `IO::Socket` and `Metamodel::Naming` roles already are.
A composing class therefore gets the role's attributes, its generated
accessors, `$!key`/`$!value` visibility inside its own method bodies, and the
attribute-collision diagnostic, all from the ordinary role-composition path
rather than from a second native implementation of behaviour mutsu already has
for enum values.

The method set is the part of `Enumeration.^methods` that actually *works* on a
composing class in rakudo, measured there rather than transcribed from the
role's source: `kv`, `pair`, `Numeric`, `Int`, `Real`, `gist`, `raku`. The rest
(`enums`, `pred`, `succ`, `pick`, `roll`, `CALL-ME`) reach
`self.^enum_values`, which a `ClassHOW` does not have — they die in rakudo too,
so supplying them would have been a divergence, not a feature. `.Str` is absent
for the same reason: rakudo's composing class falls back to `Mu.Str`, even
though a real enum value stringifies to its key.

Three smaller pieces went with it:

- the prelude is injected for **modules** as well as the main program, because
  the distribution that motivated this composes the role inside the module
  while the program that loads it (`use Logic::Ternary;`) never names
  `Enumeration` at all;
- `Enumeration` joined `BUILTIN_ROLE_NAMES`, which one consumer needs *before*
  any prelude is registered: the compiler qualifies a class header's `does`
  parent with the enclosing package unless the name is a known core type, so
  without the entry `unit module M; class C does Enumeration` compiled a parent
  named `M::Enumeration`;
- the two `constraint == "Enumeration"` fast paths (`type_matches_value` and
  the `~~` handler in `seq_helpers/smart_match.rs`) returned `false` for
  anything that was not an enum value or enum type object, pre-empting the
  ordinary composed-role check. They now assert enum membership and otherwise
  fall through, so `$instance ~~ Enumeration` is `True` for a class that has
  composed the role and still `False` for one that has not.

Pinned by `t/oo/role/class-does-enumeration-role.t`, whose 18 assertions were
all measured against rakudo first — including the documented `DNA` example and
the two enum-value non-regressions.

Closes [#8115](https://github.com/tokuhirom/mutsu/issues/8115).
