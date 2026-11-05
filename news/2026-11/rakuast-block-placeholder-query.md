# RakuAST: `Block.has-placeholder-parameters`

`RakuAST::Block` answers `has-placeholder-parameters` as rakudo 2026.09 does: true when the block's
own body declares a placeholder (`$^a`, `:$^n`, `@_`, `%_`), false when only a nested block, pointy
block or routine does (`src/rakuast/block_queries.rs`). Matches `raku` over nine shapes. Not done:
`bare-block` (rakudo marks a statement-level block; the node carries no such state), `replace-body`.
Test: `t/rakuast/rakuast-block-placeholder-query.t`.
