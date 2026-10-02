//! `OpCode::CheckReadOnly`: the check every whole-variable assignment runs
//! before its store. One implementation, shared by the interpreter's dispatch
//! arm and the JIT's dedicated shim (#10955).

use super::*;

impl Interpreter {
    /// Refuse an assignment to the readonly variable `code.constants[name_idx]`,
    /// or let it through. A sigilless/`constant` term bound to an object with a
    /// user `STORE` is let through with `pending_sigilless_store` set, so the
    /// store that follows calls that `STORE`.
    ///
    /// This runs on every whole-variable assignment, per iteration in a tight
    /// loop, so the common case is kept to the gated probes: the name string is
    /// only fetched by the paths that use it.
    // Cost: O(1) (gated hashed probes; the error path is cold).
    pub(super) fn exec_check_read_only_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // A `:=`-bound container (`my %a := %b`) is marked readonly as a
        // bind signal, but a whole reassignment (`%a = (...)`) is allowed
        // — it writes through to the bound source. The `__mutsu_bound::`
        // marker distinguishes it from a genuinely immutable `constant`.
        // Both marker probes are gated on their process-global
        // "ever created" flags: the common program never creates either
        // marker — skipping the two `format!` allocations plus env lookups
        // entirely.
        if crate::env::bound_marker_possible() {
            let name = Self::const_str(code, name_idx);
            let bound_key = crate::meta_ns::MetaNs::Bound.key_for_str(name);
            if matches!(
                self.env().get_sym(bound_key).map(Value::view),
                Some(ValueView::Bool(true))
            ) {
                return Ok(());
            }
        }
        // Probe through the pre-interned constant Symbol:
        // `check_readonly_for_modify(name)` would re-intern the name on each
        // execution just to miss the set. The error construction (readonly
        // hit) is the cold path.
        if self.is_readonly_sym(code.const_sym(name_idx)) {
            let name = Self::const_str(code, name_idx);
            // A term that IS its value (`constant term:<$x> =
            // Obj.new`, `constant x = ...`) bound to an object with a
            // user `STORE` is assignable, as a sigilless `my \x` is
            // below (#9566).
            if self.readonly_kind(name) == Some(crate::ast::ReadonlyKind::ImmutableValue)
                && self.sigilless_value_has_store(code, name)
            {
                self.pending_sigilless_store = Some(name.to_string());
                return Ok(());
            }
            self.check_readonly_for_modify(name)?;
        }
        // Also check env-based readonly status set by cross-scope
        // `:=` binding (e.g. binding to a readonly sub parameter
        // in a closure).  The readonly_vars set is scope-local
        // and gets restored on frame pop, but the env key persists.
        if crate::env::sigilless_readonly_keys_possible() {
            let name = Self::const_str(code, name_idx);
            let readonly_key = crate::runtime::sigilless_readonly_key(name);
            if matches!(
                self.env().get_sym(readonly_key).map(Value::view),
                Some(ValueView::Bool(true))
            ) {
                // An object with a user `STORE` is its own container
                // (rakudo's p6store falls back to `.STORE`): let the
                // assignment through and have the store that follows
                // call it (#9551, FixedInt).
                if self.sigilless_value_has_store(code, name) {
                    self.pending_sigilless_store = Some(name.to_string());
                    return Ok(());
                }
                // A sigilless term (`my \\c = 5`) IS the value, so
                // rakudo names the value in the error: "Cannot modify
                // an immutable Int (5)".
                return Err(self.immutable_value_error(name));
            }
        }
        Ok(())
    }
}
