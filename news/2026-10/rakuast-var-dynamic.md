# RakuAST: `RakuAST::Var::Dynamic`

A dynamic variable read (`$*x`, `@*a`, `%*h`, `&*c`) now converts to
`RakuAST::Var::Dynamic` of its whole spelling instead of a `Var::Lexical`,
including where it sits in a regex interpolation (`/$*x/`, `/<@*a>/`) or a
colonpair (`:$*x`), and renders on its own lines as rakudo does. The class can
be constructed by hand (`RakuAST::Var::Dynamic.new('$*x')`), answers `.name`
and `.sigil`, smartmatches `RakuAST::Var` and `RakuAST::Term`, and lowers to
the same dynamic lookup the parser builds, so `EVAL` of it reads the variable
through the caller chain. `Var::Lexical` gained the same `.sigil` accessor.
(#11331)
