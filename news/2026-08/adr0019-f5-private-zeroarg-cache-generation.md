# ADR-0019 F5: private_zeroarg_method_cache gains a read-site generation refresh

ADR-0019 Phase F box F5 ("remove superseded method caches and manual invalidation") listed
`private_zeroarg_method_cache` as one of the caches still depending entirely on hand-written eager
clear calls rather than the shared generation scheme, the same shape `func_multi_resolve_cache`/
`func_multi_type_cacheable` had before #6425 added `refresh_func_multi_caches_for_generation`.

Checked whether the same gap was real here. `resolve_private_method_for_vm` — one of five call sites
that end up reading `private_zeroarg_method_cache` via `resolve_private_method_any_owner` — already
called `refresh_method_caches_for_generation()` before the read. The other four
(`methods_call_dispatch.rs`, `methods_signature_shaped.rs`, `methods_instance_ops.rs` ×2) call
`resolve_private_method_any_owner` directly and skipped that refresh entirely, relying only on the
nine `clear_private_zeroarg_method_cache()` calls scattered across class/role/augment registration —
a real generation-blind gap for those four call sites, not just a cosmetic one.

Fixed by moving the refresh into `resolve_private_method_any_owner` itself
(`src/runtime/resolution_private_method.rs`), the function that actually owns the cache read, so
every caller gets it regardless of which entry point they came through. `Registry::method_generation`
is already bumped by every one of the nine class/role/augment registration paths that used to call
`clear_private_zeroarg_method_cache()` by hand (the same generation Phase B/E1a's write side already
maintains), so no new generation-bumping code was needed — those nine calls are now redundant for
correctness and kept only to drop the cache's capacity promptly, the same tradeoff `func_multi_*`
made in #6425.

`make test` (3157 files) green; targeted private-method tests
(`t/private-method-call-in-closure.t`, `t/private-method-compiled-dispatch.t`,
`t/private-method-unqualified.t`, `t/role-public-private-same-name-method.t`,
`t/mixin-private-method-self-dispatch.t`) pass.

What's still open in F5: the second generation scheme `fn_resolve_cache_gen`
(`accessors_misc.rs`) has not been unified with `Registry::method_generation`/`fn_resolve_gen`, and
`method_resolve_cache`/`fast_method_cache`/`native_ctor_plan_cache` remain eager-cleared at
`invalidate_method_dispatch_caches`'s 7 call sites.
