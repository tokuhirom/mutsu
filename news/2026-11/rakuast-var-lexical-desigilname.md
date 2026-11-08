# RakuAST: `Var::Lexical.desigilname`

`RakuAST::Var::Lexical` now answers `desigilname`, the variable's name without its sigil as one `Name`
identifier leaf (`$foo` -> `from-identifier("foo")`; a qualified `$Foo::bar` stays one leaf, as rakudo
2026.09 answers it; `Var::Dynamic` has none). Measured against `raku` over thirteen spellings (#14916).
