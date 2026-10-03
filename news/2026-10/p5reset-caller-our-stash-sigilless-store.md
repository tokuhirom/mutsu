# `CALLER::OUR::`, writable stash entries and sigilless assignment into an Array

P5reset's `reset` clears the caller's `our` variables by walking
`CALLER::OUR::.kv -> \key, \value` and assigning `Nil` or `Empty` to each value.
Three things stood in the way, and all three are fixed:

- `CALLER::OUR::` (and `CALLER::CALLER::OUR::`) was read as a package literally
  named `CALLER::OUR`, so it was always empty. It now names the package stash of
  the caller frame's package.
- A stash entry for a root `our $x` was a copy of the value. The stash now
  publishes the variable's own shared cell, and `Stash.kv` hands out the entries
  without decontainerizing them, so `for OUR::.kv -> \k, \v { v = Nil }` writes
  the variable, as in rakudo.
- Assigning to a sigilless name bound to an `Array` or `Hash` (`my \v = @a;
  v = Empty`, `-> \k, \v { v = Empty }`, `sub f(\t) { t = 1, 2 }`) died with
  "Cannot modify an immutable Array". A sigilless name holds no container of its
  own, so rakudo's assignment falls back to the object's `STORE`, which replaces
  the elements in place. A new `SigillessAggregateStore` opcode, emitted only for
  a source-level sigilless assignment, does that; the synthetic per-iteration
  binds of loop parameters still re-seat the name. An immutable `List` is still
  refused.

P5reset's test file now passes 7/7 (it was 3/7).
