# RakuAST round-trips `my $x OP= EXPR`

A declaration initialized by a compound assignment (`my $product *= $_ for
2, 3`, `my $decl_pow **= $decl_pow`) now converts to rakudo's
`ApplyInfix(left => VarDeclaration, infix => MetaInfix::Assign)` and lowers
back to the parser's own expansion, declaring the variable once even under a
postfix `for`. Two files refused under `MUTSU_RAKUAST=1` now pass;
`t/rakuast/rakuast-declaration-compound-assign.t` pins the node shape and the
round trip.
