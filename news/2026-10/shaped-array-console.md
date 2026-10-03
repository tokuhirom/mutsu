# Shaped arrays: BEGIN prologue, grep, reassignment and Rat subscripts

Array::Shaped::Console renders a 2D shaped array with block characters. It
exercised several shaped-array gaps at once. Its test files now both pass.

- **A shaped declaration lost its shape when the unit also held a `constant`,
  `BEGIN` or `use`.** The BEGIN prologue (ADR-0134) split `my @a[2;2] = ...`
  into an empty static declaration plus a plain `@a = ...` assignment. A shaped
  declaration now stays whole.
- **`.grep` on a multi-dimensional shaped array** now iterates its leaves, as
  `map` and `sort` do. Its promoting path no longer rebuilds the outer level,
  which used to drop the array's own shape.
- **Reassigning a multi-dimensional shaped array** (`@a = (-1,Inf;Inf,-1)`)
  fills it row by row through the same constructor its declaration uses. It
  used to replace it with an unshaped list of lists. A row given as a Range
  (`my @r[1;3] = [1..3,]`) is that row's values.
- **A single non-integer real subscript** (`@range[6.0]`, `(1..5)[1.5]`)
  addresses the element its `Int` names, as Rakudo's `postcircumfix:<[ ]>`
  does. It used to read `Nil` from a Range.
