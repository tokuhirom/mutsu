# An empty map no longer builds a deferred pipeline

`Array.map` is lazy: it ordinarily returns a `Seq` which retains the source,
the mapper and (for a mutable Array) its writeback source until the result is
consumed. An empty input has no callback invocation and no writeback to defer,
but mutsu still built that entire `MapGrep` pipeline.

The empty-input paths now return an already-reified empty `Seq` after the
ordinary dispatcher has established the map semantics. This preserves the
result type and the fact that an empty mapper is never invoked, while removing
five allocations per construction from the empty `@!resources.map(*.flat)`
TWEAK in `bench-ctor`.

On the `bench-ctor` allocation-scoped A/B, `bless:tweak-phase` falls from
325,245 to 300,185 allocations across 5,000 constructions, and whole-process
allocations fall from 1,531,024 to 1,505,984. The latter includes unrelated
process setup, so it is evidence of less work rather than a cross-runtime speed
claim.

Part of [#8995](https://github.com/tokuhirom/mutsu/issues/8995).
