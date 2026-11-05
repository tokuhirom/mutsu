# `RakuAST::Node.parent`

`map`, `grep` and `first` on a `RakuAST::Node` now keep the lineage of the node they are visiting (rakudo's
`@*LINEAGE`), and `parent($generation = 0)` answers from it: the visited node's ancestors, nearest first,
without the node the walk started from, `Nil` beyond the top, outside those walks and inside `find-nodes`.
A nested walk has a lineage of its own and gives the outer one back. Pinned by
`t/rakuast/rakuast-node-parent.t`, whose expected values are rakudo 2026.09's.
