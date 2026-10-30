# RakuAST lookups carry their resolution

The `RakuAST::CompUnit` that `$*CU` answers now resolves its lookup nodes the
way rakudo's CHECK phase does, so `Lookup.is-resolved` and `.resolution` work
in an `add-check-code` closure (`static::CLI`'s analysis reads them):

- a `Var::Lexical` resolves to the `VarDeclaration::Simple` of its `my`/`our`/
  `state` declaration (the very declaration node of the tree) or the
  `ParameterTarget::Var` of its parameter; `compile-time-value` of either is
  the container's type (`Any`, `Int`, `Array`, `Array[Int]`, `Callable`);
- a core operator, subscript or routine call resolves to
  `Declaration::External::Setting` whose `compile-time-value` is the core `Sub`;
  a core type to `Declaration::External::Constant` whose `compile-time-value`
  is the type object;
- dynamic variables, imported or unit-declared routines and unit-declared types
  stay unresolved for now (#15610).

`use static::CLI` now runs its analysis into the core routines' signatures and
stops at `Parameter.signature_constraint`.
