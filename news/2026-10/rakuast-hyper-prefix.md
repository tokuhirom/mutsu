# RakuAST: hyper prefix operators

`-« @a`, `++<< @a`, `?« @a` and the other hyper prefixes now render in `.AST`
as rakudo 2026.09 builds them: an `ApplyPrefix` whose prefix is a
`RakuAST::MetaPrefix::Hyper` over the plain `Prefix`. The node can be built by
hand (`RakuAST::MetaPrefix::Hyper.new(RakuAST::Prefix.new("-"))`) and
evaluated, and the `MUTSU_RAKUAST=1` round trip lowers it to the same
hyper-prefix call the parser emits; four more `t/` files pass in that mode.
