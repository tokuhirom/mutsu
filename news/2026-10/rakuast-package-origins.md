# RakuAST package declarations carry their origin

`.AST` now gives a `class`, `grammar` or `role` declaration the source
positions Rakudo does: the package itself (its scope declarator included), its
`Name`, and its `Block`, `Blockoid` and `StatementList`, the list running over
every statement of the body, an `also` statement included. As in Rakudo, a
role's `Block` starts at the role's name, the block its type parameters belong
to.

The parser records the name and block positions as it reads the declaration,
in every parse, in a `spans` member of `Stmt::ClassDecl` / `Stmt::RoleDecl`
(ADR-10723 §4), the way the routine declarations already do. Like every
recorded position, it takes no part in the declaration's identity: the stable
declaration fingerprints leave `SrcSpan` out.
