# Assigning an itemized Hash to a `%` variable stores its entries

`%h = $x` (and `%!attr = @array.tail`) with `$x` holding an itemized `Hash` kept the
item marker on the stored hash, so a later `%h, %other` list saw one opaque element and
`.raku` printed `${...}`. The `%` assignment now strips the itemization (the entries are
stored, the source hash is not aliased). Found through the ML::SparseMatrixRecommender
suite: `Math::SparseMatrix.set-column-names` stored its column-name map this way, which made
`column-bind` rename every column (`male.2`) and every recommendation wrong
(`t/04-retrieval-by-query-elements.rakutest` now matches rakudo).
