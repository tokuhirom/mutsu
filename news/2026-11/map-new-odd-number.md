# `Map.new` / `Hash.new` die on a dangling key

`Map.new(1)`, `Hash.new(1, 2, 3)` and `Map.new((1, 2, 3))` quietly paired the last key with `Nil`;
rakudo dies with `X::Hash::Store::OddNumber` (the same exception `.Hash` already raised). The
constructor now raises it; a Pair still counts as a complete entry.
