# Set, Bag and Mix answer Int, Numeric, Real and Num

Rakudo's `QuantHash` declares the four numeric coercions, composed into every quant hash: each is
the total (`set(1, 2).Int` is `2`, `(a => 0.5, b => 1).Mix.Int` is `1`, `.Num` is `1.5e0`). mutsu
had only `Bag.Numeric` and `Mix.Numeric`, so `set(1, 2).Numeric`, `bag(1, 1).Int` and the rest were
`X::Method::NotFound`. They are rows of the built-in method table now, one handler per coercion
over the existing `total`.
