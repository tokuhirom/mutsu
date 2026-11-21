# RakuAST: origins of a package that sets its representation

A package whose header sets its representation
(`class T is repr('CStruct') { ... }`) absorbs that trait, so no
`Trait::Is` is listed for it; Rakudo still starts the package's `Block` at
it, as at any first trait. mutsu started the block at its `{` and stopped
reading the header at the quoted argument, leaving the traits after it
unplaced (#15780). The `repr(...)` clause is now read past (its quoted name
included), and the block starts at the earlier of it and the first listed
trait.
