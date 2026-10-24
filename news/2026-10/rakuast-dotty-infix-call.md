# `TERM .meth` is the dotty infix in RakuAST

A method call after whitespace (`permutations(2) .iterator`, `^4 .combinations`)
is the dotty infix `.` in rakudo 2026.09's tree:
`ApplyDottyInfix(left, DottyInfix::Call, Call::Method)`, and every `.meth`
chained onto it is one too. mutsu rendered a plain postfix call, so a deparse
lost the space and the precedence it implies (#14883). A `.AST` parse now keeps
the spelling (`Spelling::DottyInfixCall`), the conversion renders the dotty
infix, the lowering turns it back into the method call, and
`RakuAST::DottyInfix::Call`, `DottyInfix::CallAssign` and
`ApplyDottyInfix.new` are constructible.
