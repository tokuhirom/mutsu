# `RakuAST::Node.locate-node`

`$ast.locate-node($pos)` answers the deepest descendant whose origin covers a
character position, and `locate-node($pos, $to)` the deepest one covering the
half-open range `$pos .. $to` (an empty or reversed range stands for the
position itself). The search walks `visit-children`, looks through children
that carry no origin of their own, and answers `Nil` when the node it is asked
of does not cover the range. It reads only the ranges the parser recorded
(`rakuast/origin_range.rs`), so how deep it gets follows the origin coverage
of #15780. The `:key` adverb is not implemented (its meaning is not
documented). Part of #14916.
