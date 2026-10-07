# RakuAST: hand-built `Type::Definedness`, `Type::AnyDefinedness` and `Type::Coercion`

`RakuAST::Type::Definedness.new(base-type => ..., definite => True)`,
`RakuAST::Type::AnyDefinedness.new(base-type => ...)` and
`RakuAST::Type::Coercion.new(base-type => ..., constraint => ...)` now build
their nodes, with rakudo's `.raku` and `DEPARSE` text. Before, only the read
direction (`.AST`) produced these classes and `.new` died with
`Could not find symbol '&Definedness'`. Pinned in
`t/rakuast/rakuast-type-definedness-new.t`, which passes under raku too.
