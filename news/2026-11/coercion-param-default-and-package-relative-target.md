# Coercion-typed parameters: coerced defaults, package-relative targets, `X,;` lists

Three gaps surfaced by SQL::Abstract's `select(Column::List(Any) $columns = *, ...)`:

- A coercion-typed parameter now coerces its **default** like a supplied argument
  (`sub f(Foo(Any) $x = *)` runs `Foo.COERCE(*)`; `Int(Str) $n = "42"` binds an `Int`).
- A coercion target written relative to the declaring package (`Column::List(Any)` inside
  `unit class SQ`, declared `SQ::Column::List`) now resolves there even when the routine is called
  from outside that package, and an omitted coercion-typed optional binds the *qualified* type
  object, so a typed attribute accepts it (#16752).
- A statement `$x,;` is a one-element list, as `{ $x, }` already was; this also fixes
  `COND ?? (...) !! $x,;` collapsing to a bare scalar.
