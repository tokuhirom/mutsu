# `subset S is foo of T` calls a user `trait_mod:<is>`

A `subset` declaration kept only `is export`; every other `is <trait>` was skipped. It now
records them (`Stmt::SubsetDecl::custom_traits`, carried through the RakuAST round trip as
`Trait::Is` entries) and the compiler hands the registered subset's type object to a user
`trait_mod:<is>`, so `$subset.HOW does Role` persists on the subset's metaobject (#15250,
found via Red's `is sub-model`). An unknown trait is an `X::Inheritance::UnknownParent`, as in
rakudo.
