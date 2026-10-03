# QuantHash containers take assignments in place; `|$bag` slips pairs; `.set` honors `WHICH`

Fixing three QuantHash gaps took CRDT from 2/10 to 7/10 test files passing.

- **Assigning to a `SetHash`/`BagHash`/`MixHash` container** now coerces the
  new value through the container's own type and writes it into the existing
  container. This covers three spellings:
  - `%!v = ...` on an `is BagHash` attribute;
  - `$obj!values = ...` through an rw method that hands the attribute back;
  - a `%` variable bound to one.

  The direct attribute form used to build a plain Hash, which died on an odd
  element count. The rw-method form died with "Cannot modify an immutable
  BagHash". The in-place write is one helper, `Value::store_quanthash_in_place`,
  which the variable store now shares.
- **`|$set`, `|$bag` and `|$mix`** slip the QuantHash's pairs as positional
  items, matching rakudo's `.Slip`. They used to slip the QuantHash itself as
  one item.
- **`SetHash.set` and `.unset`** resolve a user-defined `WHICH` before keying,
  so two objects with the same `WHICH` are one element. Setting an element
  that is already present also stores the new object, as rakudo does.
