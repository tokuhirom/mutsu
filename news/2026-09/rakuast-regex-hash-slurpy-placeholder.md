# Hash-slurpy regex placeholders retain their Block value

Argumented regex subrules now preserve hash-slurpy placeholder block values
such as `:expected{ %_ }` as a direct `RakuAST::Block` containing
`VarDeclaration::Placeholder::SlurpyHash`. Constructed RakuAST regexes lower
back through the existing matcher, retaining named arguments and match-time
lexical behavior.

Explicit block signatures and other complex block values remain deferred
boundaries.
