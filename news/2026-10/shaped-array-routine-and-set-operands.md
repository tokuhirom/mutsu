# Routines and set operators see a shaped Array's leaves

Following the change that makes a multi-dimensional shaped Array iterate its
leaves, these forms now read the leaves too:

- the iterating builtin routines (`sum(@c)`, `max(@c)`, `first(&f, @c)`,
  `grep(&f, @c)`, `set(@c)`, ...);
- the set operators (`3 ∈ @c`, `@c ∋ 4`, `@c ⊆ (1..4)`, `@c (|) ...`,
  `[∪]`).

They used to read its first-dimension rows. `elems(@c)` still counts the
first dimension, as in Rakudo (#14308).
