# The object hash `:{ ... }` is a typed node the parser builds (ADR-15334)

The parser built `:{ 1 => 2 }` as a call to the reserved `__object_hash` routine, and
`rakuast::convert` recognised that name to render `Circumfix::HashComposer(:object-hash)` again.

The parser now builds `ParsedExpr::ObjectHash(entries)`; its view is `hash_literal::object_hash_node` and
its one lowering is the existing `hash_literal::lower_composer`, shared with `EVAL` of a hand-built tree.

Deleted: the `"__object_hash"` literal in `src/parser` (`internal-name-literals` 44 -> 43), the by-name
arm in `convert` (`convert-lines` 6900 -> 6886). Parity test `t/rakuast/rakuast-stage2-object-hash-paths.t`.
