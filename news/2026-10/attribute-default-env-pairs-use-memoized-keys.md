# Evaluating an attribute default no longer formats two strings per attribute of the class

An attribute default sees the other attributes as `$!x` / `$.x`, which
`eval_attr_default_expr` provides by installing a `!name` and a `.name` env entry for
every attribute of the class before each default runs and restoring them afterwards.
It built each key with `format!`, interned it and cloned the `String` for the save list:
~1000 string builds and ~1000 interns per `Red::Column.new` (22 attributes, 22 defaults,
so the cost is quadratic in the attribute count). The two key symbols are now built once
per attribute name and the env is touched through its `Symbol` API. `$attr.column`
(one `Red::Column.new`) under Red: 1.18-1.23 ms -> 1.00 ms (plain timers, 300 and 3000
calls). Refs #15498.
