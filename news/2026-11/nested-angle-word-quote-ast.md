# Nested `<...>` word quotes keep their RakuAST shape

A balanced `<...>` written inside a `<...>` word quote (`<a <b c> d>`, or a
subscript such as `%h<value:sym<number>>`) now views in RakuAST as rakudo
builds it: the inner quote is a `RakuAST::QuotedString` segment of its own,
between literal runs that keep the brackets. Before, the whole text was one
`StrLiteral`. `EVAL` of the tree and `DEPARSE` give the same words as before,
and nesting of any depth (up to a bound) round-trips (#18457).
