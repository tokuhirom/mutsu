# The parser's post-parse passes run one top-level statement at a time

ADR-15760 slice 2d, step 1. After a unit was parsed, the parser walked the whole tree several times: the
whatever-curry mark (with the CORE type fold), the `use trace` hook numbering, the out-of-scope `whenever`
check, the outer-redeclaration scan and the sink-context warning analysis. Each now keeps its state across
the unit's top-level statements (`parser::unit_passes`), so it can run as a statement is committed; the
unit's diagnostics are settled at its end in the order the parser always reported them (`whenever`, then
`EXPORTHOW`, then the scope diagnostic, then the soft sorries), and the sink warnings are published in the
order the whole-unit walks gave them.

The passes are still driven after the parse, by a loop over the statements, so nothing changes in what a
program does. The next steps of slice 2d move the finalization to the same per-statement shape and then let
the parser drive both as it commits each statement.
