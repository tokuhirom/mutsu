use super::*;

impl Interpreter {
    /// Eagerly invalidate every name-keyed method/function resolution cache.
    ///
    /// ADR-0019 Phase F box F5: this is the block that used to be duplicated
    /// verbatim at every registry mutation site that can shadow or replace an
    /// already-cached resolution (module load/import/no/need, a `my sub`
    /// leaving block scope, a `my class`/role/enum redeclaration, a fresh sub
    /// installation). `resolve_method_cached` also invalidates its own subset
    /// lazily via `refresh_method_caches_for_generation`, keyed on
    /// `Registry::method_generation` -- but `func_multi_resolve_cache` /
    /// `func_multi_type_cacheable` (plain multi *sub* dispatch, read by
    /// `resolve_function_multi_cached`) have no generation guard at their read
    /// site and depend entirely on being cleared here. Do not drop this eager
    /// call without first adding an equivalent generation check to that read
    /// path -- see `todo/deep/adr0019-*.md` for the fuller F5 design.
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
