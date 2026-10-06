# A named alias with an `@` leaf accepts a Seq

`sub c(:edges(:@x)!)` died with "expected Positional, got Seq" when called with a `Seq`
(`c(edges => (1..3).map(...))`), although plain `:@x` and Rakudo bind it. The wrapper
parameter of an alias carries the caller-facing key, so its sigil never triggered the
`.cache`-style Seq coercion; `bind_named_rename_sub_signature` now applies it to an `@` leaf.
Fixes #11809 (found via `Math::SparseMatrix.new(:edges(:@edge-dataset))`).
