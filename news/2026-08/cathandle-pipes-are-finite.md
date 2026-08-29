# `IO::CatHandle` pipelines now reify finite sources

A lazy `map` or `grep` pipeline over `IO::CatHandle.handles` is now recognised
as finite. Previously it was treated as genuinely lazy, causing strict
consumers such as `.raku` and structural equality to observe an opaque `(...)`
placeholder rather than the handles' results.

The finite-source classifier now includes `CatHandle` pulls. The behavior is
pinned by `t/io-cathandle-lazy.t` and restores
`roast/S32-io/io-cathandle.t` under the vendored upstream `Test` module.
