# Scalar placeholder regex colonpairs retain their Block value

Argumented regex subrules now preserve scalar placeholder block-valued
colonpairs such as `:expected{ $^candidate eq $value }` as a direct
`RakuAST::Block`. Placeholder declarations survive the AST round trip and the
existing matcher keeps the block callable with current captured lexicals.

Array/hash slurpy placeholders and explicit block signatures remain deferred
boundaries.
