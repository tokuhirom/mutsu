use super::*;

impl Interpreter {
    /// Eagerly invalidate every name-keyed method/function resolution cache.
    ///
    /// ADR-0019 Phase F box F5: this is the block that used to be duplicated
    /// verbatim at every registry mutation site that can shadow or replace an
    /// already-cached resolution (module load/import/no/need, a `my sub`
    /// leaving block scope, a `my class`/role/enum redeclaration, a fresh sub
    /// installation).
    ///
    /// `func_multi_resolve_cache`/`func_multi_type_cacheable` now also
    /// self-invalidate lazily via `refresh_func_multi_caches_for_generation`,
    /// keyed on `fn_resolve_gen` (which this function bumps unconditionally) --
    /// closing a real staleness gap the eager clear alone did not: `fn_resolve_gen`
    /// is bumped at ~15 other sub/multi-registration sites that never called this
    /// function, so those two caches could go stale from a fresh multi-sub
    /// candidate added at one of them. Their `.clear()` calls below are therefore
    /// now redundant at every one of *this* function's own call sites and are kept
    /// only to drop the maps' allocated capacity immediately.
    ///
    /// The remaining caches (`method_resolve_cache`, `fast_method_cache`, ...) are
    /// also generation-guarded at their own read site
    /// (`refresh_method_caches_for_generation`, keyed on `Registry::method_generation`),
    /// but that generation is not known to be bumped at every one of this
    /// function's 7 call sites (module load/import/no/need, block-scope exit, sub
    /// registration are all sub/function-registry events, not method-registry
    /// ones) -- unlike the `func_multi_*` pair above, removing their eager clear
    /// here has not been audited as safe, so they stay.
    pub(crate) fn invalidate_method_dispatch_caches(&mut self) {
        self.fn_resolve_gen += 1;
        self.method_resolve_cache.clear();
        self.last_method_resolve = None;
        self.fast_method_cache.clear();
        self.native_ctor_plan_cache.clear();
        self.multi_resolve_cache.clear();
        self.multi_type_cacheable.clear();
        self.resolved_seq_cache.clear();
        self.func_multi_resolve_cache.clear();
        self.func_multi_type_cacheable.clear();
        self.dispatch_multi_candidate.clear();
    }
}
