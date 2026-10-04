# Compiled bytecode can be encoded, and the whole suite runs on decoded code

The precompilation cache stores only a module's AST. Every load compiles the
whole module again, and for `use Test` that is about 30M instructions per test
file ([#11756](https://github.com/tokuhirom/mutsu/issues/11756)).
[ADR-11756](../../docs/adr/11756-compiled-bytecode-precompilation.md) decides
to cache the compiled bytecode instead. This is its second step: an encoding
for `CompiledCode` and `CompiledFns`. Nothing is written to disk yet.

The encoding (`src/precomp_codec/`) is `[symbol table][payload]` in bincode 2:

- Plain data derives its codec. That covers the opcodes, the declaration
  plans, the TRIR ops and the small spec structs. AST fragments reuse their
  existing serde implementations.
- A `Symbol` is an index into the entry's string table. Decoding interns each
  distinct name once and hands the symbols to the decoder as its context.
- A constant goes through `PortableValue`, which refuses any value that
  records an object id. Such a chunk is simply not cached.
- The containers that hold run-time caches next to compiled data
  (`CompiledCode`, `CompiledFunction`, `CompiledFns`, `TrChunk`) have
  hand-written codecs:
  - the caches are skipped and come back empty, as after a fresh compile;
  - TRIR chunk ids are minted afresh;
  - every hash set and map is written in sorted order, so the same chunk
    always encodes to the same bytes.

  These codecs destructure their struct without `..`, so adding a field is a
  compile error until the field is given a codec line.

`MUTSU_PRECOMP_ROUNDTRIP=1` encodes every compile's result, decodes it,
requires the re-encoding to be byte-identical, and runs the decoded copy. With
it set, all 6430 files in `t/` pass. Getting there flushed out three places
where iteration order of a hash map leaked into the encoding:
`LexScopeChain`'s per-scope maps, its local map, and a routine's
`declared_locals`.

Measured under that mode on `use Test; ok 1;`, decoding everything costs
6.7M instructions against the 30.0M the compiler spent producing it. Most
of the decode is the serde-encoded AST fragments, which still intern every
`Symbol` occurrence. They are the first candidate for a native codec.
