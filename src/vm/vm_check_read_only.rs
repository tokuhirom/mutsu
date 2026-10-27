//! `OpCode::CheckReadOnly`: the check every whole-variable assignment runs
//! before its store. One implementation, shared by the interpreter's dispatch
//! arm and the JIT's dedicated shim (#10955).

use super::*;

/// How many cells of a binding-cell chain a readonly probe follows. A binding
/// cell holds a container (ADR-0097 §14), so a real chain is 1-2 cells long.
const MAX_BINDING_CHAIN: usize = 8;

impl Interpreter {
    /// The `X::Assignment::RO` a whole assignment to the `%` variable `name`
    /// raises when it holds an immutable associative (a `Map`, or a `Set`,
    /// `Bag` or `Mix`). Assignment coerces to a `Hash`, so such a value is one
    /// a `:=` bind or an `is Map` declaration put there, and rakudo refuses to
    /// reassign it. The value decides, not a by-name readonly mark: a bind to
    /// a mutable `Hash` stays assignable, writing through to its source.
    // Cost: O(1), plus the value's text on the error path.
    pub(super) fn immutable_associative_assignment(
        name: &str,
        current: &Value,
    ) -> Option<RuntimeError> {
        if !name.starts_with('%') {
            return None;
        }
        let current = current.deref_container();
        let type_name = match current.view() {
            ValueView::Mix(_, false) => "Mix",
            ValueView::Set(_, false) => "Set",
            ValueView::Bag(_, false) => "Bag",
            _ if current.is_immutable_map() => "Map",
            _ => return None,
        };
        Some(RuntimeError::assignment_ro_typename(
            type_name,
            &current.to_string_value(),
        ))
    }

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
        // A free variable (not a local of this code) resolves to a binding in
        // another frame. When that binding is a readonly binding cell, the
        // cell's kind is the answer: the registry below is keyed by name and
        // may hold a same-named mark of whichever frame called this one
        // (ADR-11142, #11142). Gated on any such cell existing at all.
        // A binding its declaring frame decided is writable answers too: the
        // registry is then not asked at all (#11165).
        let mut binding_decided_writable = false;
        if crate::value::readonly_binding_cells_possible() {
            // One probe of the chunk's slot index settles both questions: a name
            // that owns a slot is never a free variable, and a name that does
            // not has no slot to hold a rebound cell.
            let sym = code.const_sym(name_idx);
            let decision = match code.local_slots_of(sym) {
                [] => self.free_var_nonlocal_decision(Self::const_str(code, name_idx), sym),
                [slot] => self.slot_rebind_decision(*slot),
                _ => None,
            };
            match decision {
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
                None => {}
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

    /// The `X::Multi::NoMatch` for `op` stepping the readonly variable `name`,
    /// naming its current value's type.
    // Cost: O(1) env lookup plus the message build.
    fn incdec_readonly_error(&self, op: &str, name: &str) -> RuntimeError {
        let current = self.env().get(name).map(|v| v.clone().deref_container());
        crate::runtime::incdec_rw_sub::incdec_requires_mutable_error(op, current.as_ref())
    }

    /// Refuse an in-place `++`/`--`/`OP=` on the named variable `name`
    /// (`code`'s constant-pool name, interned as `name_sym`) when its binding
    /// is readonly. A free variable whose binding was decided by its declaring
    /// frame answers from that decision, as in `CheckReadOnly`; anything else
    /// asks the registry and the sigilless marker (#11165).
    // Cost: as `free_var_binding_decision` when a decided binding cell exists;
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
            Some(FreeVarBinding::Readonly(..)) => Err(self.incdec_readonly_error(op, name)),
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
                    return Err(self.incdec_readonly_error(op, name));
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

    /// What the binding of the free variable `name` resolves to says about its
    /// own writability, for a caller that holds the (possibly rewritten) name
    /// itself: a by-name `SetGlobal` store. `None` for one of `code`'s own
    /// locals and for a binding no frame decided for.
    // Cost: O(d), d = env tiers walked to resolve the name; the cell chain is
    // bounded by `MAX_BINDING_CHAIN`.
    pub(super) fn free_var_binding_decision(
        &self,
        code: &CompiledCode,
        name: &str,
        sym: crate::symbol::Symbol,
    ) -> Option<FreeVarBinding> {
        if !code.local_slots_of(sym).is_empty() {
            return None;
        }
        self.free_var_nonlocal_decision(name, sym)
    }

    /// [`Self::free_var_binding_decision`] for a name that is not a local of
    /// the running chunk.
    // Cost: as `free_var_binding_decision`.
    fn free_var_nonlocal_decision(
        &self,
        name: &str,
        sym: crate::symbol::Symbol,
    ) -> Option<FreeVarBinding> {
        // The order a by-name read resolves a scalar in (a unit lexical the
        // running routine captured, then the env chain), but on the raw
        // binding: the read itself derefs the unit lexical's cell.
        let binding = match self.unit_lexical_slot(name, Some(sym)) {
            Some(v) => v.clone(),
            None => match self.our_package_scalar_cell(name) {
                Some((_, cell)) => Value::container_ref(cell),
                None => self.env().get_sym(sym)?.clone(),
            },
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
        self.slot_rebind_decision(*slot)
    }

    /// [`Self::own_slot_rebind_decision`] for the slot already found.
    // Cost: as `own_slot_rebind_decision`.
    fn slot_rebind_decision(&self, slot: u32) -> Option<FreeVarBinding> {
        let binding = self.locals.get(slot as usize)?;
        // The tag test settles the overwhelmingly common plain value without
        // the cell walk.
        if !binding.is_container_ref() {
            return None;
        }
        Self::binding_cell_of(binding)?;
        Self::binding_chain_decision(binding.clone())
    }

    /// The binding's own kind for consumers that do not need an error value.
    /// `Some(None)` means the binding was explicitly decided writable.
    // Cost: O(c), c = cells on the chain, bounded by `MAX_BINDING_CHAIN`.
    pub(crate) fn binding_chain_readonly_kind(
        binding: &Value,
    ) -> Option<Option<crate::ast::ReadonlyKind>> {
        match Self::binding_chain_decision(binding.clone())? {
            FreeVarBinding::Readonly(kind, _) => Some(Some(kind)),
            FreeVarBinding::Writable => Some(None),
        }
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

/// See [`Interpreter::free_var_binding_decision`].
pub(super) enum FreeVarBinding {
    /// Refused for this reason; the value is what the binding holds, for the
    /// error's wording.
    Readonly(crate::ast::ReadonlyKind, Value),
    /// Its declaring frame decided it is writable.
    Writable,
}
