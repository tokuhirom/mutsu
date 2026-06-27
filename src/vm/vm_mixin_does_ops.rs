//! Mixin (`but`/`does`), `isa`, and pair-construction ops.
use super::*;

impl Interpreter {
    pub(super) fn exec_but_mixin_op(&mut self, code: &CompiledCode) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // `$obj but R` runs role R's `submethod TWEAK`/`BUILD` via the interpreter,
        // which can mutate a captured-outer caller lexical by name (`my $invoked;
        // role R { submethod TWEAK { $invoked = True } }`). Snapshot the
        // overwritable slots for the precise diff that reconciles them.
        let pre_env = self.snapshot_carrier_overwritable_env(code);
        // `but` composing a role or another type into a type-object invocant is
        // illegal (no instance); mixing a concrete value (`Method but True`) is
        // allowed, so this only guards the role / type-object-RHS branches.
        let left_type_object = self.does_invocant_type_object(&left);
        let role_composed = match &right {
            Value::Pair(name, boxed)
                if self.has_role(name) && matches!(boxed.as_ref(), Value::Array(..)) =>
            {
                Some(loan_env!(
                    self,
                    eval_does_values(left.clone(), right.clone())
                ))
            }
            Value::Package(name) if self.has_role(&name.resolve()) => Some(loan_env!(
                self,
                eval_does_values(left.clone(), right.clone())
            )),
            Value::Str(name) if self.has_role(name) => Some(loan_env!(
                self,
                eval_does_values(left.clone(), right.clone())
            )),
            _ => None,
        };
        if let Some(composed) = role_composed {
            if let Some(tn) = &left_type_object {
                return Err(Self::does_type_object_error("but", tn));
            }
            let composed = composed?;
            // Reconcile the caller's slots from env so a captured-outer write made
            // by the role's `submethod TWEAK`/`BUILD` is visible.
            self.carrier_writeback_changed_aggregates(code, &pre_env);
            self.stack.push(composed);
            return Ok(());
        }
        // A type object / class (not a role) on the RHS: into a type-object
        // invocant it is X::Does::TypeObject; otherwise not composable.
        if matches!(&right, Value::Package(_)) {
            if let Some(tn) = &left_type_object {
                return Err(Self::does_type_object_error("but", tn));
            }
            return Err(Self::mixin_not_composable_error(&left, &right));
        }
        let result = Self::apply_but_mixin(left, right)?;
        self.stack.push(result);
        Ok(())
    }

    /// Apply a `but` mixin for a single element from a tuple expansion,
    /// with duplicate type conflict checking.
    pub(super) fn exec_but_mixin_tuple_elem_op(&mut self) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let mixin_type = Self::mixin_type_for_value(&right);
        // Check for duplicate type conflict
        if let Value::Mixin(_, ref existing) = left
            && existing.contains_key(&mixin_type)
        {
            return Err(RuntimeError::new(format!(
                "Method '{}' must be resolved by class {}+{{}} because it exists in multiple roles",
                mixin_type,
                crate::runtime::utils::value_type_name(&left)
            )));
        }
        let result = Self::apply_single_mixin(left, mixin_type, right);
        self.stack.push(result);
        Ok(())
    }

    /// Determine the mixin type name for a value (used by `but` operator).
    fn mixin_type_for_value(val: &Value) -> String {
        match val {
            Value::Bool(_) => "Bool".to_string(),
            Value::Int(_) | Value::BigInt(_) => "Int".to_string(),
            Value::Num(_) => "Num".to_string(),
            Value::Str(_) => "Str".to_string(),
            Value::Rat(..) | Value::FatRat(..) | Value::BigRat(..) => "Rat".to_string(),
            Value::Complex(..) => "Complex".to_string(),
            Value::Package(name) => name.resolve(),
            Value::Enum { enum_type, .. } => enum_type.resolve(),
            Value::Instance { class_name, .. } => class_name.resolve(),
            _ => "Any".to_string(),
        }
    }

    /// Apply a `but`-style mixin: wrap the left value with the right value
    /// keyed by its type name.
    fn apply_but_mixin(left: Value, right: Value) -> Result<Value, RuntimeError> {
        // Handle Array RHS: `True but [1, 2]` generates a single "Array" mixin
        if let Value::Array(_, kind) = &right {
            if kind.is_real_array() {
                return Ok(Self::apply_single_mixin(left, "Array".to_string(), right));
            }
            // List values from method calls like .list -> "List" mixin
            return Ok(Self::apply_single_mixin(left, "List".to_string(), right));
        }
        if matches!(&right, Value::Seq(_)) {
            return Ok(Self::apply_single_mixin(left, "List".to_string(), right));
        }

        let mixin_type = Self::mixin_type_for_value(&right);
        Ok(Self::apply_single_mixin(left, mixin_type, right))
    }

    /// Apply a single mixin with the given type name.
    fn apply_single_mixin(left: Value, mixin_type: String, right: Value) -> Value {
        match left {
            Value::Mixin(inner, existing_mixins) => {
                let mut mixins = (*existing_mixins).clone();
                mixins.insert(mixin_type, right);
                Value::mixin((*inner).clone(), mixins)
            }
            other => {
                let mut mixins = std::collections::HashMap::new();
                mixins.insert(mixin_type, right);
                Value::mixin(other, mixins)
            }
        }
    }

    pub(super) fn exec_isa_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let type_name = right.to_string_value();
        let result = left.isa_check(&type_name);
        self.stack.push(Value::Bool(result));
    }

    /// If `left` is a *built-in* type object, return its type name. Mixing a
    /// role into such an object via `does`/`but` is illegal (X::Does::TypeObject)
    /// because there is no instance to compose into. A *user-defined* class type
    /// object (including an anonymous `class {}`) may have a role mixed in —
    /// that creates a new anonymous subtype — so it is excluded here. Undefined
    /// scalars are stored as `Nil` and act as the `Any` type object.
    fn does_invocant_type_object(&self, left: &Value) -> Option<String> {
        match left {
            Value::Nil => Some("Any".to_string()),
            Value::Package(name) => {
                let n = name.resolve();
                if self.has_class(&n) {
                    None
                } else {
                    Some(n.to_string())
                }
            }
            _ => None,
        }
    }

    /// `does`/`but` used with a type object as the invocant is illegal —
    /// there is no instance to mix into (`Bool does role {...}`,
    /// `(my $x) does Int`).
    fn does_type_object_error(op: &str, type_name: &str) -> RuntimeError {
        let msg = format!(
            "Cannot use '{}' operator on a type object {}.",
            op, type_name
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        attrs.insert("typename".to_string(), Value::str(type_name.to_string()));
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Does::TypeObject"),
            attrs,
        )));
        err
    }

    /// Mixing a non-composable type (a class or type object, not a role or a
    /// concrete value) into an object is illegal (`5 does Int`, `obj does NC`).
    /// `target` is the object being mixed into, `rolish` the offending type.
    fn mixin_not_composable_error(target: &Value, rolish: &Value) -> RuntimeError {
        let mixin_type = rolish.to_string_value();
        let into_type = crate::runtime::utils::value_type_name(target);
        let msg = format!(
            "Cannot mix in non-composable type {} into object of type {}",
            mixin_type, into_type
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg.clone()));
        attrs.insert("target".to_string(), target.clone());
        attrs.insert("rolish".to_string(), rolish.clone());
        let mut err = RuntimeError::new(msg);
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Mixin::NotComposable"),
            attrs,
        )));
        err
    }

    /// Interpreter-native `does` check. Inlines the pure `does_check` path and
    /// only falls back to the interpreter for actual role composition.
    fn vm_does_values(&mut self, left: Value, right: Value) -> Result<Value, RuntimeError> {
        // `does`/`but` on a type-object invocant (undefined scalars are stored
        // as Nil and act as the `Any` type object) is illegal *when composing a
        // role or another type* — there is no instance to compose into. Mixing a
        // *concrete value* (e.g. `Method but True`) is still allowed, so this
        // check guards only the role / type-object-RHS branches below.
        let left_type_object = self.does_invocant_type_object(&left);
        // Handle array of roles: `$obj does (RoleA, RoleB)`
        if let Value::Array(ref items, ..) = right {
            let all_roles = items.iter().all(|item| self.is_role_application(item));
            if all_roles && !items.is_empty() {
                if let Some(tn) = &left_type_object {
                    return Err(Self::does_type_object_error("does", tn));
                }
                return loan_env!(self, eval_does_values_list(left, items.as_ref()));
            }
        }
        // Check if the RHS is a role that needs to be composed onto the value.
        // If so, delegate to the interpreter which manages role state.
        if self.is_role_application(&right) {
            if let Some(tn) = &left_type_object {
                return Err(Self::does_type_object_error("does", tn));
            }
            return loan_env!(self, eval_does_values(left, right));
        }
        // When the RHS is an enum value, `does` acts as a mixin (like `but`).
        if matches!(&right, Value::Enum { .. }) {
            return Self::apply_but_mixin(left, right);
        }
        // When the RHS is a concrete value (Str, Int, Bool, etc.), `does` acts
        // as a mutating mixin — it overrides the corresponding type method.
        // For example, `$o does "modded"` makes `$o.Str` return "modded".
        if matches!(
            &right,
            Value::Str(_)
                | Value::Int(_)
                | Value::BigInt(_)
                | Value::Num(_)
                | Value::Bool(_)
                | Value::Rat(..)
        ) {
            return Self::apply_but_mixin(left, right);
        }
        // When the RHS is an Instance, mix it in so that calling .$ClassName
        // on the result returns that instance.
        if matches!(&right, Value::Instance { .. }) {
            return Self::apply_but_mixin(left, right);
        }
        // A type object / class (not a role) on the RHS: `(my $foo) does Int`
        // mixes into a type-object invocant (X::Does::TypeObject); otherwise the
        // type itself is not composable (`5 does Int`, `obj does SomeClass`).
        if matches!(&right, Value::Package(_)) {
            if let Some(tn) = &left_type_object {
                return Err(Self::does_type_object_error("does", tn));
            }
            return Err(Self::mixin_not_composable_error(&left, &right));
        }
        // Pure check: does the value conform to the named role/type?
        let role_name = right.to_string_value();
        Ok(Value::Bool(left.does_check(&role_name)))
    }

    pub(super) fn exec_does_op(&mut self, code: &CompiledCode) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Sync Interpreter locals to interpreter env so BUILD submethods can access
        // and modify closure variables from the enclosing scope.
        self.sync_env_from_locals(code);
        // Snapshot for the precise BUILD/TWEAK captured-outer writeback (see
        // exec_does_var_op).
        let pre_env = self.snapshot_carrier_overwritable_env(code);
        let result = self.vm_does_values(left, right)?;
        // Sync back: BUILD submethods may have modified closure variables, so the
        // captured-outer writes reach the caller's slots.
        self.carrier_writeback_changed_aggregates(code, &pre_env);
        // Capture Mixin value for trait_mod writeback (same as DoesVar path)
        if matches!(&result, Value::Mixin(..)) && self.trait_mod_writeback_key.is_some() {
            self.trait_mod_writeback_value = Some(result.clone());
        }
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_does_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // Sync Interpreter locals to interpreter env so BUILD submethods can access
        // and modify closure variables from the enclosing scope.
        self.sync_env_from_locals(code);
        // A `submethod BUILD`/`TWEAK` run by the mixin can mutate a captured-outer
        // caller lexical (`my $n=0; role R { submethod TWEAK { $n++ } }; $x does R`).
        // Snapshot the overwritable slots so the precise diff reconciles them.
        let pre_env = self.snapshot_carrier_overwritable_env(code);
        let updated = self.vm_does_values(left, right)?;
        // Sync back: BUILD submethods may have modified closure variables.
        self.carrier_writeback_changed_aggregates(code, &pre_env);
        let name = Self::const_str(code, name_idx).to_string();
        self.env_mut().insert(name.clone(), updated.clone());
        self.update_local_if_exists(code, &name, &updated);
        // Capture Mixin value for trait_mod writeback: when `$r does Role`
        // runs inside a trait_mod:<is>, the Mixin needs to propagate back
        // to the outer scope's `&name` variable.
        if matches!(&updated, Value::Mixin(..)) && self.trait_mod_writeback_key.is_some() {
            self.trait_mod_writeback_value = Some(updated.clone());
        }
        self.stack.push(updated);
        Ok(())
    }

    pub(super) fn exec_make_pair_op(&mut self, code: &crate::opcode::CompiledCode) {
        let mut right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        // `key => $var`: the RHS scalar variable was tagged with `WrapVarRef`, so
        // capture its container. The Pair's value becomes a `ContainerRef` shared
        // with `$var`, giving Raku's write-through semantics: `$pair.value = X`
        // updates `$var`, and `$pair.value<k> = v` writes through to `$var`'s
        // backing Array/Hash. (See S02:1704 / roast S02-types/pair.t.)
        if let Value::Capture { positional, named } = &right
            && positional.is_empty()
            && let Some(Value::Str(source_name)) = named.get("__mutsu_varref_name")
            && let Some(inner) = named.get("__mutsu_varref_value")
        {
            let source_name = source_name.to_string();
            let inner = inner.clone();
            right = self.capture_var_cell(code, &source_name, inner);
        }
        // Preserve the original key type for `.key` to return the correct type.
        // Use ValuePair for non-string keys, Pair for string keys.
        match &left {
            Value::Str(_) => {
                let key = left.to_string_value();
                self.stack.push(Value::Pair(key, Box::new(right)));
            }
            _ => {
                self.stack
                    .push(Value::ValuePair(Box::new(left), Box::new(right)));
            }
        }
    }
}
