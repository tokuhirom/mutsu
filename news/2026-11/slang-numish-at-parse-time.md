# A slang's `numish` override runs where the parser reads the literal

The core-action override `numish` (ADR-15762) used to run in a pass over the finished tree, where a
numeric literal had already lost the text it was written as, so `overload::constant`'s handlers saw
`1000` for `1e3` and `42` for `0x2A`. The parser now runs it where it reads the literal, with the
source text (#17114), and a slang's hooks end with the block that holds its `use` (the lexical scope
restores them), so the earlier tree-walk for `numish` is gone.
