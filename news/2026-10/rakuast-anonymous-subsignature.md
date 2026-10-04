# RakuAST: anonymous destructuring parameters

"non-positional signature sub-signature" was the most common `.AST` refusal
left, with 55 `t/` files.

Measured on rakudo 2026.09, an anonymous destructuring parameter is a
`Parameter` with no target that holds the `sub-signature`:

- `sub f([$a, $b])` marks that signature `is-array => True` and has no type.
- `sub g(($x, $y))` has no `is-array` and carries the implicit
  `Type::Setting(Any)` of a sub signature. A pointy block's has neither type
  nor `is-array`.
- `Pair (:key($k))` carries its written type.
- A capture destructured through a sub-signature (`|c ($x)`, `| ($a, $b)`)
  is the capture parameter, `slurpy => Parameter::Slurpy::Capture`, plus its
  `sub-signature`. The anonymous one has no target.

mutsu rendered the bracket form with a `\@` variable target, and refused the
parenthesised and capture forms. All of them now render as rakudo does, and a
mixed signature's `.AST` text is identical to rakudo's. The lowering names
each one after its form, `@` or `__subsig__`, as the parser does and as the
binder reads.

A `for` loop's destructuring parameter (`for @pairs -> [$k, $v]`) is 30 of the
55 files. It stays refused under its own message, because the parser builds
the same `__for_unpack` parameter for brackets and parentheses and does not
record which was written.

Writing the test also turned up #11891: `sub p(($x))` binds a named argument
as its positional, where rakudo reports too few positionals.
