# RakuAST nodes answer `visit-children` and a real `origin`

`RakuAST::Node.visit-children(&callable)` now calls the callable once with each direct child node, and
a statement's `.origin` is a `RakuAST::Origin` whose `.from`/`.to` and `.source.original-line($pos)`
give the line the statement began on (a node that records no origin answers `Nil`). Found through
`Code::Coverable`, which walks `.AST` this way. Positions are line numbers for now; character offsets
and per-node origins (for example the `else` of an `if`) are tracked separately.
