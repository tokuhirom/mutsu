//! The rank-0 (bare-name) ownership rule of `reconcile_attrs`, shared by its
//! reading pre-filter and its per-attribute scan.

use super::*;

impl Interpreter {
    /// Whether `reconcile_attrs` may read the bare name `bare` as a sigilless
    /// attribute's alias: a slot this frame owns that it did not declare as
    /// a parameter or a `my` (see the rank-0 rule in `reconcile_attrs`).
    // Cost: O(l + p), l = the frame's local slots, p = its parameters.
    pub(super) fn reconcile_bare_reading_owned(
        code: &CompiledCode,
        param_defs: &[crate::ast::ParamDef],
        bare: &str,
    ) -> bool {
        code.locals.iter().any(|n| n == bare)
            && !crate::ast::param_defs_declare_lexical(param_defs, bare)
            && !code
                .my_declared_sym
                .contains(&crate::symbol::Symbol::intern(bare))
            // A `my` written inside an expression
            // (`f($!x, my $x := ...)`) is recorded apart.
            && !code
                .expr_declared_syms
                .contains(&crate::symbol::Symbol::intern(bare))
    }
}
