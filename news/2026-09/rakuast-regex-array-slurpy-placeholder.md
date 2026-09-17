# Array-slurpy regex placeholders retain their Block value

Argumented regex subrules now preserve array-slurpy placeholder block values
such as `:expected{ @_ }` as a direct `RakuAST::Block` containing
`VarDeclaration::Placeholder::SlurpyArray`. Constructed RakuAST regexes lower
back through the existing matcher, retaining multiple call arguments and
match-time lexical behavior.

Hash-slurpy placeholders and explicit block signatures remain deferred
boundaries.
