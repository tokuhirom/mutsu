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
        // A binding its declaring frame decided is writable answers too: the
        // registry is then not asked at all (#11165).
        let mut binding_decided_writable = false;
        if crate::value::readonly_binding_cells_possible() {
            match self.free_var_readonly_binding(code, name_idx) {
                Some(FreeVarBinding::Readonly(kind, bound)) => {
                    // A sigilless name bound to a mutable Array/Hash assigns
                    // into it (see `vm_sigilless_aggregate_store`).
                    if kind == crate::ast::ReadonlyKind::ImmutableValue
                        && Self::is_sigilless_assignable_aggregate(&bound.deref_container())
                    {
                        self.pending_sigilless_store =
                            Some(Self::const_str(code, name_idx).to_string());
                        return Ok(());
                    }
                    return Err(Self::readonly_binding_error(kind, &bound));
                }
                Some(FreeVarBinding::Writable) => binding_decided_writable = true,
                None => {
                    // One of this code's own slots, holding a binding cell a
                    // rebind decided (#9277).
                    match self.own_slot_rebind_decision(code, code.const_sym(name_idx)) {
                        Some(FreeVarBinding::Readonly(kind, bound)) => {
                            return Err(Self::readonly_binding_error(kind, &bound));
                        }
                        Some(FreeVarBinding::Writable) => binding_decided_writable = true,
                        None => {}
                    }
                }
            }
        }
        // Probe through the pre-interned constant Symbol:
        // `check_readonly_for_modify(name)` would re-intern the name on each
        // execution just to miss the set. The error construction (readonly
        // hit) is the cold path.
        if !binding_decided_writable && self.is_readonly_sym(code.const_sym(name_idx)) {
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

    /// Refuse an in-place `++`/`--`/`OP=` on the named variable `name`
    /// (`code`'s constant-pool name, interned as `name_sym`) when its binding
    /// is readonly. A free variable whose binding was decided by its declaring
    /// frame answers from that decision, as in `CheckReadOnly`; anything else
    /// asks the registry and the sigilless marker (#11165).
    // Cost: as `free_var_readonly_binding` when a decided binding cell exists;
    // O(1) otherwise.
    pub(super) fn check_named_incdec_readonly(
        &self,
        code: &CompiledCode,
        name: &str,
        name_sym: crate::symbol::Symbol,
        op: &str,
    ) -> Result<(), RuntimeError> {
        let decision = if crate::value::readonly_binding_cells_possible() {
            self.free_var_binding_decision(code, name, name_sym)
                .or_else(|| self.own_slot_rebind_decision(code, name_sym))
        } else {
            None
        };
        match decision {
            Some(FreeVarBinding::Readonly(..)) => {
                Err(crate::runtime::incdec_rw_sub::incdec_requires_mutable_error(op, name))
            }
            // Still ask the sigilless marker: it is a separate mechanism the
            // decision does not cover.
            Some(FreeVarBinding::Writable) => {
                if crate::env::sigilless_readonly_keys_possible()
                    && matches!(
                        self.env()
                            .get_sym(crate::runtime::utils::sigilless_readonly_key(name))
                            .map(Value::view),
                        Some(ValueView::Bool(true))
                    )
                {
                    return Err(
                        crate::runtime::incdec_rw_sub::incdec_requires_mutable_error(op, name),
                    );
                }
                Ok(())
            }
            None => self.check_readonly_for_incdec_for(name, Some(name_sym), op),
        }
    }

    /// Refuse an in-place `OP=` on the named variable when its binding is
    /// readonly. Same decision as [`Self::check_named_incdec_readonly`], but
    /// a compound assignment is an assignment, so it raises what plain
    /// assignment raises, not the `postfix:<++>` dispatch failure.
    // Cost: as `check_named_incdec_readonly`.
    pub(super) fn check_named_compound_readonly(
        &self,
        code: &CompiledCode,
        name: &str,
        name_sym: crate::symbol::Symbol,
    ) -> Result<(), RuntimeError> {
        if self
            .check_named_incdec_readonly(code, name, name_sym, "postfix:<++>")
            .is_ok()
        {
            return Ok(());
        }
        let sigilless = crate::env::sigilless_readonly_keys_possible()
            && matches!(
                self.env()
                    .get_sym(crate::runtime::utils::sigilless_readonly_key(name))
                    .map(Value::view),
                Some(ValueView::Bool(true))
            );
        Err(if sigilless {
            self.immutable_value_error(name)
        } else {
            RuntimeError::readonly_variable()
        })
    }

    /// The error an assignment through a binding readonly for the reason
    /// `kind` raises, worded from the value `bound` the binding holds (not
    /// from a by-name lookup, which may see another frame's same-named
    /// variable).
    pub(super) fn readonly_binding_error(
        kind: crate::ast::ReadonlyKind,
        bound: &Value,
    ) -> RuntimeError {
        use crate::ast::ReadonlyKind;
        match kind {
            ReadonlyKind::Alias => RuntimeError::readonly_variable(),
            ReadonlyKind::Immutable | ReadonlyKind::ImmutableDeep => {
                RuntimeError::immutable_value()
            }
            ReadonlyKind::ImmutableValue => RuntimeError::assignment_ro_value(bound.clone()),
            ReadonlyKind::TypeObject => {
                let type_name = match bound.view() {
                    ValueView::Package(sym) => sym.to_string(),
                    _ => "Any".to_string(),
                };
                RuntimeError::assign_requires_concrete_object(&type_name)
            }
        }
    }

    /// What the binding the free variable `code.constants[name_idx]` resolves
    /// to says about its own writability. `None` for one of `code`'s own
    /// locals (the registry still answers for those) and for a binding no
    /// frame decided for.
    ///
    /// A readonly kind anywhere on the binding-cell chain wins over a writable
    /// decision: a decided `my $x` container rebound by `$x := 42` sits behind
    /// the binding cell that now leads to the readonly cell instead.
    // Cost: O(d), d = env tiers walked to resolve the name (the by-name read's
    // cost); the cell chain is bounded by `MAX_BINDING_CHAIN`.
    fn free_var_readonly_binding(
        &self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Option<FreeVarBinding> {
        self.free_var_binding_decision(
            code,
            Self::const_str(code, name_idx),
            code.const_sym(name_idx),
        )
    }

    /// [`Self::free_var_readonly_binding`] for a caller that holds the
    /// (possibly rewritten) name itself: a by-name `SetGlobal` store.
    // Cost: as `free_var_readonly_binding`.
    pub(super) fn free_var_binding_decision(
        &self,
        code: &CompiledCode,
        name: &str,
        sym: crate::symbol::Symbol,
    ) -> Option<FreeVarBinding> {
        if !code.local_slots_of(sym).is_empty() {
            return None;
        }
        // The order a by-name read resolves a scalar in (a unit lexical the
        // running routine captured, then the env chain), but on the raw
        // binding: the read itself derefs the unit lexical's cell.
        let binding = match self.unit_lexical_slot(name, Some(sym)) {
            Some(v) => v.clone(),
            None => self.env().get_sym(sym)?.clone(),
        };
        Self::binding_chain_decision(binding)
    }

    /// What the binding in one of `code`'s own local slots says about its
    /// writability, when that binding is a rebound binding cell: a `:=`
    /// rebind made by another frame (a routine rebinding the captured
    /// variable) decided it on the cell, while this frame's registry mark is
    /// still the one from before the rebind (#9277). `None` for a name with
    /// several slots, a slot holding no rebound cell, or no decision.
    // Cost: O(1); the cell chain is bounded by `MAX_BINDING_CHAIN`.
    pub(super) fn own_slot_rebind_decision(
        &self,
        code: &CompiledCode,
        sym: crate::symbol::Symbol,
    ) -> Option<FreeVarBinding> {
        let [slot] = code.local_slots_of(sym) else {
            return None;
        };
        let binding = self.locals.get(*slot as usize)?;
        Self::binding_cell_of(binding)?;
        Self::binding_chain_decision(binding.clone())
    }

    /// The decision recorded along the binding-cell chain starting at
    /// `binding`: a readonly kind anywhere on it wins, else a writable
    /// decision, else `None`.
    // Cost: O(c), c = cells on the chain, bounded by `MAX_BINDING_CHAIN`.
    fn binding_chain_decision(binding: Value) -> Option<FreeVarBinding> {
        // A binding cell (ADR-0097 §14) holds the variable's container: the
        // kind is on whichever cell of the chain was seated for the bind.
        // Bounded: no Raku container contains itself, but a cell cycle left by
        // a bug elsewhere must not hang a store (see `value_is_defined`).
        let mut cur = binding;
        let mut writable = false;
        for _ in 0..MAX_BINDING_CHAIN {
            let ValueView::ContainerRef(cell) = cur.view() else {
                break;
            };
            let inner = cell
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner)
                .clone();
            match cell.binding_decision() {
                Some(Some(kind)) => return Some(FreeVarBinding::Readonly(kind, inner)),
                Some(None) => writable = true,
                None => {}
            }
            cur = inner;
        }
        writable.then_some(FreeVarBinding::Writable)
    }
}

/// See [`Interpreter::free_var_readonly_binding`].
pub(super) enum FreeVarBinding {
    /// Refused for this reason; the value is what the binding holds, for the
    /// error's wording.
    Readonly(crate::ast::ReadonlyKind, Value),
    /// Its declaring frame decided it is writable.
    Writable,
}
