# RakuAST origins of anonymous variables and indented heredocs

`.AST` now matches rakudo 2026.11 on three more points:

- an anonymous variable assigned to in place, as in `(@ = 1, 2)` or
  `($ = 5)`, has its `VarDeclaration::Anonymous` placed over its sigil;
- an indented heredoc whose body starts with an interpolation
  (`    @l[] @l[0]`) now starts with the empty `StrLiteral` that its
  stripped indentation leaves, as rakudo's does, and that literal covers
  the indentation;
- a heredoc beside a string of the same value (`is q:to/E/, "no x\n"`) no
  longer takes that string's range. Its body is converted without the
  shape lookup that matched the string (#15780).
