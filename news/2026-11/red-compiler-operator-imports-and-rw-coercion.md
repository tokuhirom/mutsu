# Imported operators resolve to their import; coerced `is rw` arguments stay the caller's

Three gaps showed up in Red's CHECK-time compiler (Red t/091-rakuast-compiler).

- **An imported operator resolves to the import.** In the unit's RakuAST, an
  operator a `use` brought in (Red exports `infix:<==>`, `infix:<lt>` and
  `postfix:<++>` candidates) resolved to the setting's routine
  (`Declaration::External::Setting`). rakudo resolves it to the
  `Declaration::Import`. A scope now declares the routines its `use`
  statements imported for the unit's own resolution too, and a `Prefix` or
  `Postfix` consults the unit's declarations as an `Infix` already did.
- **A coercion-typed `is rw` parameter binds the coerced value.** With
  `sub f(Numeric(Str) $b is rw)`, `f($r)` wrote the coerced number back into
  the caller's `$r`, where rakudo binds the coerced value as immutable and
  leaves `$r` alone. Red's `infix:<==>(Red::AST $a, Numeric(Str) $b is rw)` had
  turned a captured `'01'` into `1`, so "Numeric coercion of a string capture"
  matched no rows. An argument that already has the target type still binds
  the caller's container.
- **The winner of a captured-candidate dispatch keeps the call's argument
  sources.** An imported multi handed as a code object to a module that
  cannot see it by name is dispatched over its captured candidates. When
  ranking them ran user code (an operand's `Str` method, for a `Str()`
  candidate), that code consumed the call site's argument sources, and the
  `is rw` winner then died with "expects a writable container" on a variable
  argument.
