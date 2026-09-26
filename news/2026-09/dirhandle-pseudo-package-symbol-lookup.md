# Static pseudo-package symbol lookups now parse correctly

mutsu now treats bare static keys such as `MY::<DirHandle>` as pseudo-package
stash lookups instead of folding them into qualified type names. This fixes the
DirHandle ecosystem distribution's lexical import smoke test while preserving
ordinary `Foo::<Bar>` type-name parsing.
