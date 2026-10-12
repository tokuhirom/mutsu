The `:{ ... }` object-hash spelling now round-trips as a
`RakuAST::Circumfix::HashComposer` with `object-hash => True`. Lowering
reconstructs the existing `__object_hash` call, preserving Mu-keyed object
keys and the current compiler/VM execution path. Empty, single-pair,
multi-pair, and hand-built object composers are covered by
`t/rakuast/rakuast-object-hash.t`. The RakuAST frontend ratchet passes 6960 of
7274 files.
