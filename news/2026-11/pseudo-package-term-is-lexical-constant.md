# `::?CLASS` and its siblings are `Var::Lexical::Constant` in RakuAST

The parser keeps `::?CLASS`, `::?ROLE`, `::?PACKAGE` and `::?MODULE` as `Var("?NAME")`, the same
tree as `$?NAME`, so `.AST` showed a compiler variable and `.DEPARSE` printed `$?CLASS`. The
written `::?NAME` is now kept as a spelling and viewed as `RakuAST::Var::Lexical::Constant.new("::?NAME")`
(a new `RakuAST::Var::Lexical::Constant.new` constructor), and lowering reads it back to the
same pseudo-package lookup.
