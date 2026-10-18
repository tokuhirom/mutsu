# `Any`'s `ASSIGN-KEY`, `BIND-KEY` and `DELETE-KEY` are rows

The VM's mutating-method opcode used to answer these methods on an undefined receiver, and
`AT-KEY` on a hash, from its own name check. They now go through `Any`'s rows and the ordinary
`Hash` rows (ADR-11276 §9.172). Two answers now match Rakudo:

- `my $x; $x.ASSIGN-KEY('a', 1)` leaves the new hash in the variable's `Scalar` (`${:a(1)}`).
- `Any.BIND-KEY` binds the value it was given, not the caller's variable.
