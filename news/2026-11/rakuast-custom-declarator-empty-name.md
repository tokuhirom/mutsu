An anonymous custom package declarator written with `::` now retains that
empty name in `Str.AST` and `.DEPARSE`. The parser records the written `::`
and the RakuAST view renders its two empty name edges, as Rakudo 2026.11 does.
