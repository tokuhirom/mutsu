# `.AST`: `@0` / `%0` are positional captures with their sigil

A numbered capture read through a sigil (`@0`, `%1`) is `Var::PositionalCapture` carrying
the written sigil (`.sigil` answers `@` / `%`), as rakudo 2026.11 has it, not a
`Contextualizer::List` / `Hash` over `$0`; `EVAL` of the tree runs the same `.list` /
`.hash` the parser built (#18252).
