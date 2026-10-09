# RakuAST retains conflicting subscript adverbs

When two value adverbs conflict, the ordinary parser builds an `X::Adverb` call. RakuAST parsing now also records the original index and colonpairs, so `.AST` exposes the postcircumfix shape Rakudo uses. Lowering recognizes conflicting built-in value adverbs and rebuilds the shared error call. Its index descriptor is calculated by the same helper as the ordinary parser, including element, slice, whatever, and zen forms.

The frontend ratchet adds the conflict, zen-slice, and hash-adverb suites plus a focused Rakudo-compatible read/EVAL test. This is an S10 slice of #7564.
