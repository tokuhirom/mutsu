# RakuAST: `literalize` of a hash composer

`{a => 1}` (a `Circumfix::HashComposer` of constant pairs) now literalizes to a
`Map`, as rakudo does; it answered `CannotLiteralize`. A `%(...)`
contextualizer and a pair with a non-constant value still do not literalize.
A hash nested in a value stays a `Hash` where rakudo's is a `Map`, and
`{ 1, 2 }` (rakudo: a hash composer) is read as a block here; both are left
open on #14916.
