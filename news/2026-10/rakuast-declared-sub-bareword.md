# RakuAST: a bare mention of a declared sub is an argument-less call

A bareword naming a `sub` the same compilation unit declares (`sub f { }; f`,
`niltest[0]`) now converts to `Call::Name::WithoutParentheses`, the node rakudo
2026.09 renders, instead of stopping the round trip as an unresolved bareword.
Four more `t/` files pass under `MUTSU_RAKUAST=1` and are added to the ratchet.
