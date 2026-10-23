# RakuAST interpolated string segments carry their origins

`.AST` now gives the segments of an interpolating string (`"a $x b"`,
`"a{ $x }b"`, `qq{...}`) the ranges Rakudo does:

- the literal text between interpolations, written escapes included;
- a variable where its name is written;
- a `{ }` block, with its `Blockoid`, its `StatementList` and the statements
  in it.

The segments are placed left to right, each where the source text says it
starts. A literal written without escapes must equal its value. An escaped
sigil (`\$x`) is not taken for an interpolation. The walk stops at a segment
whose written form cannot be told for certain: an interpolation followed by a
subscript or method call, or a block holding quotes. The segments after it
stay without an origin. A string spanning several lines now reports the line
each interpolation is written on to a `visit-children` walker.
