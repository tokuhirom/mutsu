`done VALUE` inside a `supply` block now round-trips through RakuAST. The
frontend exposes its emitted payload and completion as the two statements
Rakudo keeps in the supply block, then reconstructs the parser's supply body.
