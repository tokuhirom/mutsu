# A sigilless term is a mark on its declaration

`my \x = 5`, `my \x := $y` and a sigilless pointy parameter used to be a `VarDecl` followed by a
`Stmt::MarkSigillessReadonly` / `Stmt::MarkSigilless` statement, and every consumer re-found the
pair by statement shape. The declaration now carries `VarMarks::term` (`TermMark`), the compiler
settles the term before and after the store itself, and the two statement kinds are gone, with the
shape scans in the export-hook walk, the method-literal invocant fold, the redeclaration pass and
the RakuAST term collector. The parameter prologues (`-> \x`, `-> (\i, \j)`) are direct compiler
calls instead of statements prepended to a cloned body.
