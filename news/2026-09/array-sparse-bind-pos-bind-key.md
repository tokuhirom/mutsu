# Array::Sparse no longer crashes: BIND-POS routing, BIND-KEY/ASSIGN-KEY containers, and the Proxy assignment result

Array::Sparse 0.0.13's suite died with `Internal error: called Option::unwrap() on a None value`
at its 21st test. Four interpreter gaps were behind it:

- **The value of an assignment through a Proxy.** `my $b := @a[N]; ($b = 42)` ran the Proxy's
  STORE, but `AssignExprLocal` left nothing on the stack. The next `ContainerizePair` then
  underflowed, which caused the panic. The assignment now yields the assigned value, as the
  other Proxy-store sites already did.
- **Element binds on a custom Positional.** `@a[$i] := v` probed the class's `BIND-POS` with the
  whole one-element subscript list, so a `BIND-POS(Int:D $pos, ...)` signature never matched.
  The bind then fell through to the plain-Array path, which replaced the `is Array::Sparse`
  object with an ordinary Array. When the index was 1234567890, it also materialized an Array
  that long. The probe now uses the unwrapped index. The tied-container dispatch also sees
  through a closure-captured variable's `ContainerRef` cell.
- **`%h.ASSIGN-KEY` wrote into a detached copy.** Through a `:=` alias (`my %s := %!s`) the
  write was lost. On an attribute hash it also retagged the hash as `(my Any % = ...)`. It now
  writes into the shared hash node, as `BIND-KEY` already did.
- **Binding a bare value made nothing read-only.** `%h.BIND-KEY($k, 42)`, or forwarding a
  sigilless parameter that is bound to a literal, now installs a read-only element cell.
  `ASSIGN-KEY` and `%h{$k} = ...` on that key die with "Cannot assign to an immutable value",
  as they do in Rakudo. `ASSIGN-KEY` also honours a key bound through the subscript form
  (`%h<k> := 5`).

The file now passes 22 of 23 tests. The remaining assertion needs `eqv` to compare two objects
by `.WHAT` and `.raku` the way Rakudo does; that is tracked in #9591.
