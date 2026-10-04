# `.flat(N)` descends into Seq elements

The depth-limited `flat` walker treated a `Seq` element of a list as an opaque value, so
`<a b>.map({ @rows.map(...) }).flat(1)` returned the two Seqs instead of their rows.
A `Seq` now counts as one level of nesting, like a `List`
(ML::SparseMatrixRecommender, `t/01-creation.rakutest` long-form dataset).
