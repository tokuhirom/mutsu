# RakuAST: `(1; 2)` is a SemiList of two statements

A parenthesized list with semicolons now keeps its sections for `.AST`: `Circumfix::Parentheses(SemiList)`
holds one `Statement::Expression` per section, as rakudo's does, where `(1, 2)` still holds one.
The hand-built tree lowers back to the same list. Execution is unchanged. Row 2 of #14820.
