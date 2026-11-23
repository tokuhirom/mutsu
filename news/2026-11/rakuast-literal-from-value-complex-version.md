# `RakuAST::Literal.from-value` builds Complex and Version literals

`RakuAST::Literal.from-value(42+i)` is now a `RakuAST::ComplexLiteral` and
`RakuAST::Literal.from-value(v6.e)` a `RakuAST::VersionLiteral`, as the new
raku-doc page documents and rakudo does; both were a plain `RakuAST::Literal`.
Their `.DEPARSE` (`<42+1i>`, `v6.e`) and `.EVAL` follow (#18979).
