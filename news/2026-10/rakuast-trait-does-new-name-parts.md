# RakuAST: `Trait::Does.new` and `Name.from-identifier(...).parts`

`RakuAST::Trait::Does.new(TYPE)` is now a known positional constructor, and a
`Name` built by `from-identifier` answers `.parts` (one `Name::Part::Simple`)
and `.canonicalize`, matching rakudo.
