# A routine's compiled chunk is shared, not copied

`CompiledFunction` owned its `CompiledCode`, so every clone of a compiled routine deep-copied the
whole chunk: its ops, constants and nested tables. That happened on every routine registration
(`adapt_compiled_to_def` clones the plan's body to stamp the declaration's signature onto it),
on `Arc::make_mut` of a table entry another table shares, and on the method and phaser paths
that wrapped a copy of the chunk in a fresh `Arc`. The chunk is now an `Arc<CompiledCode>`, so
those clones are a reference-count bump; the few writers (source-file stamping and the lexical
frame passes) go through `Arc::make_mut`.

The precompilation codec also stopped interning a symbol per occurrence inside the AST fragments
and parameter lists of a compiled section: those serde-encoded symbols now use the entry's symbol
table index, as natively encoded symbols already did, and the decoded table is shared instead of
copied per nested chunk.

Load cost of `use Test; ok 1;` minus an empty script (profiling build, warm precompilation cache,
callgrind instructions), against the same `main`: 62.43M → 60.37M total, about 2.1M less per
test file (#11756).
