//! `OpCode::CheckReadOnly`: the check every whole-variable assignment runs
//! before its store. One implementation, shared by the interpreter's dispatch
//! arm and the JIT's dedicated shim (#10955).

use super::*;

/// How many cells of a binding-cell chain a readonly probe follows. A binding
/// cell holds a container (ADR-0097 §14), so a real chain is 1-2 cells long.
const MAX_BINDING_CHAIN: usize = 8;

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
        // A free variable (not a local of this code) resolves to a binding in
        // another frame. When that binding is a readonly binding cell, the
        // cell's kind is the answer: the registry below is keyed by name and
        // may hold a same-named mark of whichever frame called this one
        // (ADR-11142, #11142). Gated on any such cell existing at all.
        if crate::value::readonly_binding_cells_possible()
            && let Some((kind, bound)) = self.free_var_readonly_binding(code, name_idx)
        {
            return Err(Self::readonly_binding_error(kind, &bound));
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

    /// The error an assignment through a binding readonly for the reason
    /// `kind` raises, worded from the value `bound` the binding holds (not
    /// from a by-name lookup, which may see another frame's same-named
    /// variable).
    fn readonly_binding_error(kind: crate::ast::ReadonlyKind, bound: &Value) -> RuntimeError {
        use crate::ast::ReadonlyKind;
        match kind {
            ReadonlyKind::Alias => RuntimeError::readonly_variable(),
            ReadonlyKind::Immutable | ReadonlyKind::ImmutableDeep => {
                RuntimeError::immutable_value()
            }
            ReadonlyKind::ImmutableValue => RuntimeError::assignment_ro_typename(
                crate::runtime::utils::value_type_name(bound),
                &bound.to_string_value(),
            ),
            ReadonlyKind::TypeObject => {
                let type_name = match bound.view() {
                    ValueView::Package(sym) => sym.to_string(),
                    _ => "Any".to_string(),
                };
                RuntimeError::assign_requires_concrete_object(&type_name)
            }
        }
    }

    /// The readonly kind of the binding the free variable
    /// `code.constants[name_idx]` resolves to, with the value it is bound to,
    /// when that binding is a readonly binding cell. `None` for one of
    /// `code`'s own locals (the registry still answers for those) and for any
    /// binding without a recorded kind.
    // Cost: O(d), d = env tiers walked to resolve the name (the by-name read's
    // cost); the cell chain is bounded by `MAX_BINDING_CHAIN`.
    fn free_var_readonly_binding(
        &self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Option<(crate::ast::ReadonlyKind, Value)> {
        let sym = code.const_sym(name_idx);
        if !code.local_slots_of(sym).is_empty() {
            return None;
        }
        // The order a by-name read resolves a scalar in (a unit lexical the
        // running routine captured, then the env chain), but on the raw
        // binding: the read itself derefs the unit lexical's cell.
        let name = Self::const_str(code, name_idx);
        let binding = match self.unit_lexical_slot(name) {
            Some(v) => v.clone(),
            None => self.env().get_sym(sym)?.clone(),
        };
        // A binding cell (ADR-0097 §14) holds the variable's container: the
        // kind is on whichever cell of the chain was seated for the bind.
        // Bounded: no Raku container contains itself, but a cell cycle left by
        // a bug elsewhere must not hang a store (see `value_is_defined`).
        let mut cur = binding;
        for _ in 0..MAX_BINDING_CHAIN {
            let ValueView::ContainerRef(cell) = cur.view() else {
                return None;
            };
            let inner = cell
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone();
            if let Some(kind) = cell.readonly_kind() {
                return Some((kind, inner));
            }
            cur = inner;
        }
        None
    }
}
