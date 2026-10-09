# append/prepend flatten a deferred map Seq on any receiver

`[0,1].append((1,2).map(...))` (a receiver with no variable name) dropped the argument because the deferred `.map`/`.grep` Seq was never reified before the one-argument flattening. The array mutators now reify it first. Found via ML::SparseMatrixRecommender (Math::SparseMatrix `row-bind`/`column-bind`).
