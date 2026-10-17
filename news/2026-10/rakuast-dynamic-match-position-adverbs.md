RakuAST now preserves dynamic `m:p(EXPR)` / `m:pos(EXPR)` and `m:c(EXPR)` / `m:continue(EXPR)` values as `QuotedRegex` adverbs and lowers them through the existing per-match evaluation path.
