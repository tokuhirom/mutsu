# A block a slang action compiles belongs to the unit that holds the production

A slang action that compiles a RakuAST block (`RakuAST::Block.to-begin-time` /
`meta-object`, as Qwiratry's `mold ... when { ... }` does) used to give the closure the file of
whichever module's routine asked for it (`Mold::Compiler`). The operators the user's unit
imported (`infix:<∩>`, `infix:<⪪⪪>`) were then invisible inside the block, so a set-composed
`when` query fell back to the core `Set` operator instead of building a query object.

While a slang production is replayed in its unit, the closures and subs a RakuAST tree builds now
carry that unit's file, as rakudo's `$*CU` does. Qwiratry's `t/integration/mold-when-query.rakutest`
now matches `raku` (6 of 6 subtests).
