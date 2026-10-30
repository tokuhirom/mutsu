# `VarDecl` records its initializer as a field, not a `__has_initializer` trait

`Stmt::VarDecl` gains `has_initializer: bool`. The `__has_initializer` pseudo-trait that the parser
pushed onto `custom_traits` (72 readers and writers in 32 files) is gone, so for this mark the
list holds only the traits the program wrote. Every `all(t == "__has_initializer")` "no user
traits" test became `custom_traits.is_empty()`. In the RakuAST converter `var_decl_statement` no
longer lists the marker among its internal traits, and `does_declaration` no longer re-pushes it
to carry an initializer. See ADR-10723 §6.121 (#7564).
