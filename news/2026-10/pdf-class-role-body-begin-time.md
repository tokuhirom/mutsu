# Role bodies see their `use` at declaration; role attribute traits compose

Working PDF::Class (an ecosystem roulette draw) turned up a cluster of role
gaps that kept 36 of its provided modules from loading. All of them now load,
as does everything else rakudo can load from the distribution.

- **`use` in a role body is BEGIN-time.** mutsu defers a role body to
  composition, so `role R { use A::B; also does A::B }` reported
  `Unknown role: A::B`, a nested `my role` doing a role imported by the outer
  body failed the same way, and a role that is never composed itself (PDF's
  `PDF::Destination`, whose nested `DestDict` is what consumers compose) never
  imported its trait handler. The body's `use`/`need` statements now run when
  the role is declared, importing into the role's package scope exactly as
  composition does.
- **Role attribute traits run.** `has $.x is entry(...)` in a role never
  called its `trait_mod:<is>`. The trait now runs once, at the role's first
  composition, and each composing class gets its own copy of the
  trait-mutated `Attribute`, whose `compose($class)` hook fires before the
  stub-requirement check — so an alias accessor it installs satisfies a
  role's `method type {...}`, as in rakudo.
- **`also is` in a class expression.** `PDF::COS.loader = class Loader {
  also is PDF::COS::Loader; ... }` left `also is ...` behind as a runtime
  "Two terms in a row"; the expression form now extracts it like the
  statement form.
- **Role-body declarations needed early.** A `my role Common` named by the
  same body's `also does Common` registers before the parent walk; an
  exported `proto sub` is importable as soon as the role's module loads; a
  sub `proto` registers once per role, so composing such a role into two
  classes no longer dies with `X::Redeclaration`; and enum members declared
  in the body are accepted as method parameter value constraints.
