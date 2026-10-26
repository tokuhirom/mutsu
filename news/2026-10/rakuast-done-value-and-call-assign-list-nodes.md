# `done VALUE` and a `.=` declaration list are typed parser nodes

The statement-list converter no longer recognizes two parser desugarings by the shape of their
statements. `done VALUE` (`ParsedStmt::DoneValue`) and `my Int $c .= new(), 5`
(`ParsedStmt::CallAssignList`) are nodes the parser keeps and `lower` expands, like the loose-tail
declaration before them. `done_value_expansion` and `convert_call_assign_list`'s block-shape
matcher are deleted (ADR-10723 §6.117).

`.AST` of `done VALUE` now matches rakudo: one `done` call carrying the payload, instead of an
`emit VALUE` and a bare `done`. It also works under a statement modifier (`done 2 if $x`), which
the old shape could not represent, and `EVAL` of a hand-built `done(VALUE)` call works.
