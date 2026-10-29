# Metamodel names, metaroles and constant hashes, as the Kind module needs them

Closes more of the `Kind` distribution's suites (#15349):

- `Metamodel::ClassHOW.^name` / `.raku` (and the other core metamodel classes and
  metaroles) answer `Perl6::Metamodel::ClassHOW`, the name Rakudo gives them;
  `Metamodel::Primitives` keeps its own.
- A type's `.HOW` is a `Metamodel::Naming`, `AttributeContainer`, `Stashing`, ...
  as Rakudo's built-in meta-objects are (table in
  `src/runtime/types/metamodel_how_roles.rs`, read off Rakudo), so
  `nqp::istype_nd($type.HOW, Metamodel::AttributeContainer)` is true.
- A parametric role group's HOW (`Blob.HOW`) can `parameterize`.
- `nqp::can($type, $name)` on a type made by `Metamodel::Primitives.create_type`
  asks its HOW's `find_method`; a HOW without one dies, as on Rakudo.
- `constant %h = hash` / `= %(...)` keeps the mutable `Hash` (only a list is
  coerced to an immutable `Map`), so `%h<k> = v` and `%h<k> := v` work.

Still open on #15349: `.raku` of a metarole (an NQP role) should die, the
`modules`/`packages` kinds (#15520).
