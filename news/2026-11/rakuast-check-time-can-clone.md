# Every RakuAST node `can` the check-time problem methods, and `clone` copies a node

`sorries`, `worries`, `has-check-time-problems`, `add-sorry` and `add-worry` were registered on
`RakuAST::CompUnit` only, so `'say 1'.AST.can('sorries')` was `False` while rakudo, whose
`CheckTime` role every node composes, says `True` for every node. They are now rows of
`RakuAST::CheckTime` too (one handler tells the compilation unit's queue from a node's own
problems). `RakuAST::Node.clone` copies a node and shares its children, as `Mu.clone` does.
