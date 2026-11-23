# RakuAST::Term::True and RakuAST::Term::False

`RakuAST::Term::True.new` and `RakuAST::Term::False.new` now build the same
node as `RakuAST::Term::Enum.from-identifier('True')` / `('False')`, as rakudo
does: it deparses to `True`, evaluates to the Bool, and works as a flag value in
`RakuAST::Doc::Block.set-config` (the raku-doc example). Previously the names
failed to resolve (#18982).
