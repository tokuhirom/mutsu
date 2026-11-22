# A coercion type keeps a Bool or an allomorph that already is its target

A coercion type calls the target's method only on a value that is not already
of the target type. mutsu converted two kinds of values that already were.

- A `Bool` passed to `Int(Bool)`, `Int()` or `Numeric()` became a plain `Int`.
- An allomorph passed to `Str(Int)`, `Str()`, `Int()` or `Num()` became a
  plain `Str`, `Int` or `Num`.

Rakudo binds both unchanged: a `Bool` already is an `Int`, and an `IntStr`
already is a `Str`. mutsu now does the same (#18856). Values of other types,
such as `"42"` passed to `Int()` or `True` passed to `Str()`, are still
converted.
