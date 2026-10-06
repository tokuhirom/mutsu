# RakuAST: names and literal values (slice S1)

First slice of the plan on #7564. A bareword that none of the converter's
tables knew was refused as `BareWord("…")` in 302 `t/` files, and the other
refusals of this family were `import` / `need` statements, a version, and a
whole complex number written as a literal.

**Names.** Rakudo resolves a bareword at parse time, so its node says what the
name is. The converter's table of the unit's own declarations now also knows:

- a declaration nested in a package or class under every spelling a
  reference can use (`Inner`, `Outer::Inner`, `A::B::C`), for types, enum
  values (`P::Status::S1`) and constants (`M::c`, which is `our`-scoped);
- an `our sub` reached by its qualified name, which rakudo renders as an
  argument-less `Call::Name` (`M::foo`), unlike the bare `Call::Name::
  WithoutParentheses` of an unqualified one;
- a definite type used as a term (`Str:D`, `Int:U`), a `Type::Definedness`;
- a pseudo-package prefix on a name that resolves (`CORE::DateTime`,
  `GLOBAL::A`) and `GLOBAL`.

What a `use` brought in used to be invisible to the converter, because it only
scans the unit's own declarations. The parser keeps the answer in its own scope
tables: the types, enum values and value terms of the unit and of every module
it scanned. The converter now asks `parser::declared_name_kind` for a name it
cannot place, so an imported class is a `Type::Simple` and an imported
constant or enum value a `Term::Name`, as in rakudo. 150 of the 302 files
round-trip with only this.

**Statements and literals.** `need Module;` and `import Module :TAG;` are
`Statement::Need` (`module-names`) and `Statement::Import` (`module-name`, the
tags as `argument`), the same way `use` already carries its tags. `v6.d` and
`v1.2.3+` are `VersionLiteral`; a complex number written whole, `<1+2i>`, is a
`ComplexLiteral` rendered as its angle-bracket literal.

Still refused here, and tracked in the plan: names that only a module's
run-time `EXPORT` hook creates, the `Int:_` smiley, `need Module:ver<1>` (the
parser does not read it), a bare imported routine called without arguments
(rakudo says `Call::Name::WithoutParentheses`, the parser keeps no trace of
the missing parentheses for an import), and the `:(…)` signature literal.
