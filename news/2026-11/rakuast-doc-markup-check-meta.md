# `Doc::Markup.check-meta` and `splat-letterless`

Two methods of `RakuAST::Doc::Markup` were missing. Both are parse-time helpers that report problems
or rewrite the parent; on a node built by hand they change nothing and print nothing, so only the
answers are observable. Measured on rakudo 2026.09 over A-Z, `a` and the empty letter, with and
without `meta`: `check-meta` answers `Nil` for the letters whose handler ends in a check of its own
(A D E F L M P X) and `Empty` for every other letter; `splat-letterless` answers `Nil` and leaves the
node alone. Part of #14916.
