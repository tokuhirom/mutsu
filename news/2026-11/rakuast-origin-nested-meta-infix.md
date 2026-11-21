# RakuAST: origins of nested meta-operators

An assignment written with a reversed or bracketed operator (`$x R+= 2`,
`$x [+]= 2`, `$x [R-]= 2`) is a meta-operator nested in another, and mutsu
placed neither (#15780). The spelling of an infix operator is now read
recursively (`infix_spelling::written`, moved there from `infix_origins`),
so each operator inside is placed where its spelling puts it: `MetaInfix::Reverse`
over `MetaInfix::Assign` over `Infix`, or `MetaInfix::Assign` over
`BracketedInfix`.
