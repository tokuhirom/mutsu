# A `unit` declarator after a package declaration is too late

`role R { }; unit module M; say "ran"` ran in mutsu; rakudo refuses it at
compile time with `X::UnitScope::TooLate` ("Too late for unit-scoped module
definition; Please use the block form."). mutsu only raised that error for a
semicolon-form declarator nested inside a package. The parser now notes when
the compilation unit declares a package-like type -- a `class`, `role`,
`grammar`, `module` or `package` block, at any depth -- and rejects a later
`unit class` / `unit module` / `unit role` / `unit grammar` / `unit sub MAIN`
the same way. An `enum`, a `subset`, a routine or an ordinary statement
before the `unit` declarator stays fine, as on rakudo (#18588).
