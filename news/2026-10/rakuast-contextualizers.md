# RakuAST: `$(...)`, `@(...)` and `%(...)` render as contextualizers

`.AST` of `$(1, 2)`, `@(1, 2)`, `%(1, 2)`, `$@(1, 2)` and `$%(1, 2)` now yields
rakudo's `Contextualizer::Item` / `List` / `Hash` over a `StatementSequence`
(the `$` of `$@(...)` holds the inner contextualizer directly) instead of an
`ApplyPostfix` with a `Call::Method("item" | "list" | "hash")`. The parser keeps
them as a new `Expr::Contextualizer` node that compiles to the same method call,
so a user-written `.list` still renders as a call, and hand-built
`Contextualizer::*` nodes lower back to it. Item assignment (`$(@a[0]) = 1`)
keeps its transparent-lvalue behavior. The computed-key `{ $k => 1 }` composer
remains a follow-up.
