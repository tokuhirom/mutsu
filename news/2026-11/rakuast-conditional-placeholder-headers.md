# RakuAST conditional chains retain their placeholder signatures

An `EVAL($ast)` of a block containing `elsif` or `orwith` now includes the
later clauses' condition placeholders in the block's inferred signature.
The lowered conditional is a chain of `If` statements, so the shared
control-header visitor now walks each clause condition while leaving branch
bodies and ordinary nested `else` blocks in their own scopes. This makes
`t/routines/with-placeholder-condition-topic.t` pass through the RakuAST
evaluation driver (#16356).
