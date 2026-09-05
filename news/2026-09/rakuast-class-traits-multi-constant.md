# RakuAST class traits, `multi`, and `constant`

Three more read gaps, all measured against rakudo 2026.07 and byte-for-byte
identical there. Each also lowers back through `EVAL`.

## Class traits

`class C is Int { }`, `class C does R { }` and `class C is rw { }` were one
boundary ("class with inheritance / scope / repr / traits"). raku spells the
three differently, and the differences are not guessable:

- `is Int` → `Trait::Is(type => Type::Simple(...))` — a **named** `type` field.
- `does R` → `Trait::Does(Type::Simple(...))` — **positional**.
- `is rw` → `Trait::Is(name => Name.from-identifier("rw"))` — a trait *name*,
  not a type.

`is repr("P6opaque")` is not a trait at all: it is a `repr` leaf field on the
class, in field order `scope, name, repr, traits, body`.

One mutsu detail the round trip had to respect: a `does` role is recorded in
**both** `ClassDecl.parents` and `ClassDecl.does_parents` — `parents` is the
general composed-type list the dispatcher reads — so the converter skips a
parent that is also a role, or it would render the role twice, and the lowerer
puts it back in both.

Still deferred: `my`/unit scope, `hides`, computed names, and user traits.

## `multi`

`multi sub f(...)` renders `multiness => "multi"`, which precedes `name` in
raku's field order. A `proto` also uses this field but carries a `{*}` body that
mutsu keeps in a separate `Stmt::ProtoDecl`, so it stays a boundary.

## `constant`

`constant X = 5` is a declaration of its own — `RakuAST::VarDeclaration::Constant`
— not a scoped `my`, and its `name` is a plain string rather than a `Name` node.
The package-scoped default spelling emits no `scope`; `my constant Y = 7` emits
`scope => "my"`, which mutsu records as the *absence* of its `is_our` flag.
A sigilled (`constant @a = 1, 2`) or typed constant stays a boundary.

## Coverage

`t/rakuast-class-traits-multi-constant.t` (16 assertions) pins all three trait
spellings, the repr field, that a plain class emits no `traits`, the multiness
field and its absence, all three constant fields, and three `EVAL` round trips
including a class composing a role and calling the composed method. It is a
dual-oracle test: it passes verbatim under both mutsu and rakudo 2026.07.

Each declaration is the last statement of its `EVAL`'d program, so the `EVAL`'d
value is the declared thing and the test inspects it from the outside —
referring to a user class or constant by bare name *inside* the same program is
a separate, still-open read gap (raku renders a type bareword as
`Type::Simple` and a constant bareword as `Term::Name`, both measured).
