# Hash.new now materializes lazy Associative pairs

`Hash::Agnostic` 0.0.20 now passes its complete test suite under mutsu. Its
`.pairs` method returns the deferred `Seq` produced by `keys.map`, and
`Hash.new` now reifies that lazy result before constructing the hash.

The regression is pinned in
`t/collections/hash/hash-new-associative.t`.
