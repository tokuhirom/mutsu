# A lazy `@` array carries its container descriptor

`my Int @t = lazy 1..*` stored a lazy list that knew nothing of its element
type or `is default(...)` value, so `Array.default`/`.of` were answered by
looking the variable's name up, and a lazy array passed to a sub lost the
answer (`sub f(@x) { @x.default }; f(@t)` was `(Any)`, Rakudo `(Int)`). The
list now carries the descriptor placed on it by the assignment, and the lazy
step's pulls for `Array.default` and `Array.of` read it; the name-lookup arm
is gone.
