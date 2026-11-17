# A RakuAST lookup's compile-time value, and `StatementList.insert-statement`

Three `RakuAST::Node` API gaps from the #14916 survey are closed, measured
against rakudo 2026.09 (`RAKUDO_RAKUAST=1`):

- `Type::Simple` / `Type::Setting` `.compile-time-value` and
  `.maybe-compile-time-value` answer the type their resolution names (`Str`, a
  unit's `class Foo`); `Var::Lexical.maybe-compile-time-value` answers the
  container's type object (`Int` for `my Int $x`, `Any` untyped, for a
  parameter and for `$_`) or, for `&say`, the routine. A node built by hand is
  not resolved and dies as rakudo's does (`'Int' has not been resolved.`,
  `This element has not been resolved.`). A code variable naming a core routine
  (`&say`) now resolves to its `Declaration::External::Setting`.
- `StatementList.insert-statement($i, $statement)` inserts in place and
  answers the statements; an index outside `0 .. count` dies with
  `Cannot insert a statement at $i in a list of $n statements`.
- The `VarDeclaration::Simple` a parameter's target declares no longer copies
  the parameter's implicit `Type::Setting` `Any`, and carries the parameter's
  type node as resolved: `visit-children` over `sub f($p)` and
  `sub g(Int $q)` now visits exactly the nodes rakudo visits.

Tests: `t/rakuast/rakuast-lookup-compile-time-value.t`,
`t/rakuast/rakuast-insert-statement.t`.
