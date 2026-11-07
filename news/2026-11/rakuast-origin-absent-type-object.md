# A hand-built RakuAST node's `.origin` is the `RakuAST::Origin` type object

`RakuAST::IntLiteral.new(5).origin` answered `Nil`; rakudo answers the undefined `RakuAST::Origin`.
Both are skipped by `with`, but `.^name` and typed code now see rakudo's value.
