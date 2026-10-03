# `==` on Hashes/Sets and `ok |$x == ...` call arguments

Found by working the Math::Matrix distribution (`t/022-converter.rakutest` now passes).

- `%a == %b` and `set(..) == set(..)` compared the collections structurally; they now compare
  `.elems` like Rakudo's `Map.Numeric` / `Setty.Numeric`.
- In a paren-less call, `f |$x == (1,2), "d"` slipped the whole comparison. `|` is a tight prefix,
  so only `$x` is slipped now; a lone `|@a` argument still slips.

Residue: `Bag.Numeric` / `Mix.Numeric` (total weight) do not exist yet.
