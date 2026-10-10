# Set/Bag/Mix.join and Nil.join

`Set`, `Bag`, `Mix` (and their `*Hash` variants) now answer `.join` by joining their
pairs (`a<TAB>True`, `a<TAB>2`), as `Any.join` does in Rakudo, instead of dying with
`X::Method::NotFound`. `Nil.join` now warns "Use of Nil in string context" and answers
`""` instead of `Nil`. Fixes #12607.
