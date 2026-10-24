# Prefix `?`/`!`/`so`/`not` ask the operand's own `Bool`

With a user `multi prefix:<?>` (or `!`, `so`, `not`) in scope, an operand that no
candidate takes fell back to a core operator that read the value's raw
truthiness, so `?$obj` ignored `$obj`'s `Bool` method (Red declares
`prefix:<?>(Red::AST)`, and `?Model.^all` answered `True` for an empty table).
The fallback now boolifies through the same path as `if`/`so`. A `Bool` declared
by the class or a composed role of the instance under a `but` mixin is honoured
too.
