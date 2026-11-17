# A `for` loop's `@`/`%` parameter type-checks its element

A `for` loop's `@` parameter now refuses an element that is not
`Positional`, and its `%` parameter one that is not `Associative`, as a
routine's parameter does. `for 1, 2 -> @q { }` now dies with rakudo's "Type
check failed in binding to parameter '@q'; expected Positional but got Int
(1)" instead of binding the Int. An `@` parameter still takes what a
routine's binds through `PositionalBindFailover`: a Seq, a lazy list, or a
`.map` result.
