# `%!a ∪= ...` keeps a SetHash attribute, and `5 == 3..7` compares element counts

Two bugs found with CRDT, whose ten test files now match rakudo (318/318):

- A meta-assignment such as `%!add ∪= %x` or `%!bag ⊎= %x` on a `has %!add is SetHash` /
  `is BagHash` attribute turned the container into a plain Hash, so the next `%!add.set: ...`
  died with "No such method 'set' for invocant of type 'Hash'". The by-name expression
  assignment now writes through the QuantHash the way the statement form already did.
- `5 == (3..7)`, `5 != (3..7)` and `5 <=> (3..7)` ignored that `Range.Numeric` is `.elems`:
  a lone Range meeting a number now compares by its element count (two ranges already did).
