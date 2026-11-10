# `$0:exists` and the RakuAST ScopePhaser queries

`$0:exists` / `$1:!exists` is now the `:exists` adverb on the positional capture (`$/[0]:exists`), as
`$<foo>:exists` already was; it used to parse as a colonpair argument and made Red's regex test files fail to
compile. `RakuAST::Block` and the routine nodes answer `has-any-phasers`, `has-loop-phasers` and
`has-phaser($name)` (rakudo's `ScopePhaser` role), which Red's RakuAST compiler asks before inlining a block
(Red, ecosystem).
