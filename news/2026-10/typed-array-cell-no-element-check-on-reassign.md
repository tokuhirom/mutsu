# A typed array captured by a named sub can be reassigned wholesale

`my Str @buffer` captured by a named sub (the shape of `TAP`'s `parse-stream`) was promoted to a
cell that carried the *element* type `Str` as if it were the array's own constraint, so
`@buffer = ()` from inside the sub died with `Type check failed in assignment to @buffer;
expected Str but got Array ([])`. Cells promoted from `@`/`%` names no longer take that
constraint; the typed container keeps enforcing its element type. Both `TAP` test files now pass.
