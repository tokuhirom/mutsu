# Parametric role literals now parse in expression position

An anonymous parametric role such as `role Name[::T] {}` was not accepted as
an expression term. The parser recognized the `role` keyword and its name,
but required a body immediately afterward. It therefore left the parameter
list behind and eventually reinterpreted a name such as `Zape[...]` as a `Z`
metaoperator.

Expression-position role literals now reuse the declaration parser's optional
role-parameter parser. The resulting `RoleDecl` retains both its plain type
parameter names and their complete parameter definitions, exactly as a
statement-level declaration does.

`t/parametric-role-expression-term.t` covers both a `::T` type capture and a
typed value parameter, and verifies that each expression produces a role type
object.
