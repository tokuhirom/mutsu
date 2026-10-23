# RakuAST: `"%h<a>"` is a LiteralHashIndex

An angle subscript inside an interpolating string (`"%h<a>"`, `"$x<k>"`) now keeps the
`<...>` spelling, so `.AST` gives `Postcircumfix::LiteralHashIndex(QuotedString)` as rakudo
does, instead of a `HashIndex` over a `SemiList`. Execution is unchanged. Row 4 of #14820.
