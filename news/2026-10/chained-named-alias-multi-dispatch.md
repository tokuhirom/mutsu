# Chained named aliases take part in multi dispatch

A multi candidate whose named parameter used a chained alias, such as
`:m(:matrix(:$core-matrix))`, was treated as a positional destructure during
candidate matching and rejected, so calls fell through to a less specific
candidate (or to the default `new`). `is_named_rename_sub_signature` now
recognises a nested rename chain. Found via the `ML::SparseMatrixRecommender`
distribution (`Math::SparseMatrix.new`), where every `column-names` came back
wrapped or as `Whatever`. Pinned by `t/routines/multi-chained-named-alias-dispatch.t`.
