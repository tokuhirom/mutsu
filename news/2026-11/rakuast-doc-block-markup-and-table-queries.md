# RakuAST: `Doc::Block.allowed-markup` and `.visual-table`

`allowed-markup` answers the `RakuAST::Doc::Block::NOK` type object for `code`, `implicit-code`
and `defn` blocks and `RakuAST::Doc::Block::OK` for every other type; both are `Map` subclasses
whose `<X>` is `False` / `True`. `visual-table` is `True` for a `table` block holding a
`LegacyRow`, `False` for another table and the junction `any(False, False)` for any other block
type. Each was measured against rakudo 2026.09 (ADR-14916, #14916).
