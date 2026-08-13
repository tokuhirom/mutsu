# ADR-0019 E11 slice 3: collection unary builtins route through the resolver

ADR-0019 Phase E box E11 retires direct callers of the
`native_method_{0,1,2}arg` arity cascades outside the resolver's two canonical
invocation points. The free-function `keys()`/`values()`/`kv()`/`pairs()`
builtins in `runtime/builtins_collection.rs` were still one of the deferred
sites: `builtin_unary_collection_method` took `&self` and called
`native_method_0arg()` directly, bypassing `call_method_with_values()`.

This slice routes the four builtins through `call_method_with_values()`,
guarded by the E2 catalog's `e2_native_method_exists()` existence check (added
in slice 2) to preserve the exact prior fallback: an unrecognized
`(target, method)` pair — e.g. a bare `keys()` call with no target — still
yields an empty list instead of a dispatch error.

Verified against real `raku` across `Hash`/`Array`/`Int`/`Any` receivers.
`make test` (3131 files) and the relevant roast files
(`S32-hash`/`S32-array` `keys`/`values`/`kv`/`pairs`, `S02-types`
`set`/`bag`/`mix` family) all green.
