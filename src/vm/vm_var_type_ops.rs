//! `SetVarType` / `SetVarTypeScoped` — declaration-time type-constraint
//! registration for `my TYPE $x` (and the typed `@`/`%` container forms).
use super::*;

impl Interpreter {
    /// Execute a `SetVarType` / `SetVarTypeScoped` op. `scoped` selects the
    /// env-only registration used for a scalar `my`/`state` lexically inside a
    /// routine (see `OpCode::SetVarTypeScoped`); the registration store is the
    /// ONLY difference between the two ops — the Nil→type-object seeding and
    /// container tagging below are shared.
    pub(super) fn exec_set_var_type(
        &mut self,
        code: &CompiledCode,
        ip: &mut usize,
        name_idx: u32,
        tc_idx: u32,
        scoped: bool,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let raw_constraint = Self::const_str(code, tc_idx).to_string();
        // Empty constraint = CLEAR: an untyped expression-position
        // declaration dropping a stale same-named constraint (the
        // compiler never emits an empty string for a real type).
        if raw_constraint.is_empty() {
            self.vm_set_var_type_constraint(&name, None);
            *ip += 1;
            return Ok(());
        }
        // Resolve type capture variables (e.g., `T` → `Int` when `::T`
        // was captured earlier in the signature).
        let constraint = loan_env!(self, resolved_type_capture_name(&raw_constraint));
        // Clear stale atomic CAS state when an @-variable is
        // (re-)declared with a type constraint like atomicint.
        if name.starts_with('@') && constraint == "atomicint" {
            self.clear_atomic_array_state(&name);
        }
        if scoped {
            self.loan_env_for(|i| i.set_var_type_constraint_routine_scoped(&name, &constraint));
        } else {
            self.vm_set_var_type_constraint_decl(&name, Some(constraint.clone()));
        }
        // For scalar variables, if the current value is Nil, set it to the type object.
        // Exception: if the constraint is "Nil", keep the value as Nil
        // (the Nil type object is Nil itself, not the Package "Nil").
        if !name.starts_with('@') && !name.starts_with('%') && constraint != "Nil" {
            let is_nil = matches!(
                self.env().get(&name).map(Value::view),
                Some(ValueView::Nil) | None
            );
            if is_nil {
                let init_val = self.typed_scalar_nil_seed_value(&name, &constraint);
                self.set_env_with_main_alias(&name, init_val.clone());
                self.update_local_if_exists(code, &name, &init_val);
            }
        } else if let Some(value) = self.get_env_with_main_alias(&name) {
            let info = crate::runtime::ContainerTypeInfo {
                value_type: loan_env!(self, var_type_constraint(&name)).unwrap_or(constraint),
                key_type: if name.starts_with('%') {
                    loan_env!(self, var_hash_key_constraint(&name))
                } else {
                    None
                },
                declared_type: None,
            };
            // Hashes embed metadata in `HashData`; write the tagged value
            // back (no-op Arc for array/instance side-table containers).
            // Tagging an object hash also re-keys it by `.WHICH`
            // (see `tag_container_metadata`).
            let tagged = self.tag_container_metadata(value, info);
            self.set_env_with_main_alias(&name, tagged.clone());
            self.update_local_if_exists(code, &name, &tagged);
        }
        *ip += 1;
        Ok(())
    }

    /// The value a Nil-valued typed scalar holds under constraint `constraint`:
    /// the nominal type object, except native types (zero/empty defaults) and
    /// parameterized roles (the ParametricRole type object, so `.WHAT`/`.raku`
    /// keep the type arguments). Used both by the declaration-time seeding
    /// above and by the SetLocal store path when a Nil is ASSIGNED to a typed
    /// scalar — the read paths deliberately do not consult env-scoped
    /// constraints for Nil→type-object conversion (a `= Nil` parameter default
    /// must stay Nil), so the stored value itself must carry the type object.
    pub(crate) fn typed_scalar_nil_seed_value(&mut self, name: &str, constraint: &str) -> Value {
        if crate::runtime::native_types::is_native_int_type(constraint) {
            Value::int(0)
        } else if matches!(constraint, "num" | "num32" | "num64") {
            Value::num(0.0)
        } else if constraint == "str" {
            Value::str(String::new())
        } else {
            // A parameterized role constraint (`my Cup of EggNog $mug` /
            // `my Cup[EggNog] $mug`) resolves to the ParametricRole type
            // object. The stored constraint metadata normalizes to the base
            // name, so probe the raw constraint here.
            let parametric = constraint
                .contains('[')
                .then(|| loan_env!(self, type_arg_value_from_name(constraint)));
            match parametric {
                Some(v) if matches!(v.view(), ValueView::ParametricRole { .. }) => v,
                _ => {
                    // The seeded package must carry the NOMINAL type name —
                    // smileys stripped (`my Int:_ $a` seeds `Int`, not
                    // `Int:_`) and coercion parens unwrapped — same as the
                    // read-path Nil→type-object conversion it replaces.
                    let base = loan_env!(self, var_type_constraint(name))
                        .unwrap_or_else(|| constraint.to_string());
                    let nominal = loan_env!(self, nominal_type_object_name_for_constraint(&base));
                    Value::package(Symbol::intern(&nominal))
                }
            }
        }
    }
}
