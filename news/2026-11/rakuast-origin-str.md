# `RakuAST::Origin.Str` is the source text of the range

`$node.origin.Str` (and an origin interpolated into a string) now answers
the source text the origin covers, as rakudo does: `try $y.foo` for the
`try` prefix of `my $x = try $y.foo`, `1 + 2` for a term inside `"x{1 + 2}"`.
It used to give the default object stringification, `RakuAST::Origin<N>`
(#18312).
