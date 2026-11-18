# `return Nil` keeps its argument in RakuAST and `.DEPARSE`

The parser read a written `return Nil` and a bare `return` as the same statement, so
`.AST` and `.DEPARSE` printed `return` for both. A written `Nil` argument is now kept
as a spelling (`Spelling::WrittenNil`) and viewed as rakudo's `Type::Simple Nil`
argument of the `Call::Name::WithoutParentheses`; the compiler sees the same tree as before.
