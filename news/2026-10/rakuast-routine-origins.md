# RakuAST routine declarations carry their origin

`.AST` now gives a routine declaration the source positions Rakudo does, all
the way down: the `Sub` / `Method` / `Submethod` itself (its scope and `multi`
declarators included), its `Name`, its `Signature` (from the first parameter
to the end of the last, or at the `)` of an empty one), each `Parameter`
(type, traits and default included) with its target and its type, and the
routine's `Blockoid` and `StatementList`. The unit's own top-level
`StatementList` has its range too.

The parser records the positions as it reads the declaration, in every parse
(ADR-10723 §4): a parameter keeps them in a new `ParamDef::spans` member and
a routine in `spans` on `Stmt::SubDecl` / `Stmt::MethodDecl`, which already
hold what the source wrote. Spans take no part in a node's identity, so the
routine-body fingerprints are unchanged. A `Statement::Expression` without a
statement modifier also hands its range to its expression when the parser
placed nothing finer, which is how the `Sub` gets the range of its statement.

A node's origin is now a member of `RakuAstNode` (`origin: NodeOrigin`), as
the §4 decision describes, rather than a hidden field: no code that walks a
node's fields sees it, and it takes no part in node equality, so a hand-built
node (which has none) still compares equal to a parsed one.
