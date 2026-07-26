# RakuAST StatementList construction

Phase 4 construction now supports an empty `RakuAST::StatementList.new` and the
`add-statement` mutator. Appends preserve shared node identity, so aliases observe the same children,
and a fully constructed statement list can be passed to `EVAL` through the existing RakuAST lowering
and compiler pipeline.

The mutator is part of the native model API and therefore appears in `.^methods(:local)` and
`.^method_table`; `.^attributes(:local)` continues to report only the `statements` model field.
