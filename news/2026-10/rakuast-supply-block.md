# RakuAST: `supply { … }` through a source-form record

With `whenever` across the boundary, the next refusal in 94 `t/` files was a
`supply { … }` block. The parser expands it into
`Supply.on-demand(-> $emitter { … })`, rewriting the body's `emit` / `done`
onto the emitter, so `.AST` met an internal `__mutsu_supply_emitter_N`
lambda instead of the block the source wrote. Rakudo 2026.09 renders the
block as a `StatementPrefix::Supply` around its `Block`.

This uses ADR-10723's source-form pattern, already used for signature
declarations and method-assign declarations. The expansion now opens the
emitter lambda's body with a `SourceForm::SupplyBlock` record of the written
body; the compiler skips it.
- `convert` renders `StatementPrefix::Supply` from that record, never from
  the rewritten body.
- `lower` hands the lowered block back to the same expansion
  (`parser::supply_block`), so the round trip runs exactly the code the
  parser would have produced.

The statement forms, `supply whenever …` and `supply STATEMENT`, still
decline; rakudo renders them with a statement as the blorst.

The round-trip ratchet grew by 81 files, to 3360.
