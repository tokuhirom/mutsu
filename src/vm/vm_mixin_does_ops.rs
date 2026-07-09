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
        let role_composed = match right.view() {
            ValueView::Pair(name, boxed)
                if self.has_role(name) && matches!(boxed.view(), ValueView::Array(..)) =>
            {
                Some(loan_env!(
                    self,
                    eval_does_values(left.clone(), right.clone())
                ))
            }
            ValueView::Package(name) if self.has_role(&name.resolve()) => Some(loan_env!(
                self,
                eval_does_values(left.clone(), right.clone())
            )),
            ValueView::Str(name) if self.has_role(name) => Some(loan_env!(
                self,
                eval_does_values(left.clone(), right.clone())
            )),
            _ => None,
        };
        if let Some(composed) = role_composed {
            if let Some(tn) = &left_type_object {
                return Err(self.but_on_type_object_error(tn));
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
        if matches!(right.view(), ValueView::Package(_)) {
            if let Some(tn) = &left_type_object {
                return Err(self.but_on_type_object_error(tn));
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
        if let ValueView::Mixin(_, existing) = left.view()
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
        match val.view() {
            ValueView::Bool(_) => "Bool".to_string(),
            ValueView::Int(_) | ValueView::BigInt(_) => "Int".to_string(),
            ValueView::Num(_) => "Num".to_string(),
            ValueView::Str(_) => "Str".to_string(),
            ValueView::Rat(..) | ValueView::FatRat(..) | ValueView::BigRat(..) => "Rat".to_string(),
            ValueView::Complex(..) => "Complex".to_string(),
            ValueView::Package(name) => name.resolve(),
            ValueView::Enum { enum_type, .. } => enum_type.resolve(),
            ValueView::Instance { class_name, .. } => class_name.resolve(),
            _ => "Any".to_string(),
        }
    }

    /// Apply a `but`-style mixin: wrap the left value with the right value
    /// keyed by its type name.
    fn apply_but_mixin(left: Value, right: Value) -> Result<Value, RuntimeError> {
        // Handle Array RHS: `True but [1, 2]` generates a single "Array" mixin
        if let ValueView::Array(_, kind) = right.view() {
            if kind.is_real_array() {
                return Ok(Self::apply_single_mixin(left, "Array".to_string(), right));
            }
            // List values from method calls like .list -> "List" mixin
            return Ok(Self::apply_single_mixin(left, "List".to_string(), right));
        }
        if matches!(right.view(), ValueView::Seq(_)) {
            return Ok(Self::apply_single_mixin(left, "List".to_string(), right));
        }

        let mixin_type = Self::mixin_type_for_value(&right);
        Ok(Self::apply_single_mixin(left, mixin_type, right))
    }

    /// Apply a single mixin with the given type name.
    fn apply_single_mixin(left: Value, mixin_type: String, right: Value) -> Value {
        match left.view() {
            ValueView::Mixin(inner, existing_mixins) => {
                let mut mixins = (**existing_mixins).clone();
                mixins.insert(mixin_type, right);
                Value::mixin(inner.as_ref().clone(), mixins)
            }
            _ => {
                let mut mixins = std::collections::HashMap::new();
                mixins.insert(mixin_type, right);
                Value::mixin(left, mixins)
            }
        }
    }

    pub(super) fn exec_isa_op(&mut self) {
        let right = self.stack.pop().unwrap();
        let left = self.stack.pop().unwrap();
        let type_name = right.to_string_value();
        let result = left.isa_check(&type_name);
        self.stack.push(Value::truth(result));
    }

    /// If `left` is a *built-in* type object, return its type name. Mixing a
    /// role into such an object via `does`/`but` is illegal (X::Does::TypeObject)
    /// because there is no instance to compose into. A *user-defined* class type
    /// object (including an anonymous `class {}`) may have a role mixed in —
    /// that creates a new anonymous subtype — so it is excluded here. Undefined
    /// scalars are stored as `Nil` and act as the `Any` type object.
    fn does_invocant_type_object(&self, left: &Value) -> Option<String> {
        match left.view() {
            ValueView::Nil => Some("Any".to_string()),
            ValueView::Package(name) => {
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

    /// Is `name` a role type — a user-declared role or one of the built-in
    /// roles? A role type object carries a `ParametricRoleGroupHOW`, which is the
    /// distinction that matters for `but` on a type object (see
    /// `but_on_type_object_error`).
    fn is_role_type_name(&self, name: &str) -> bool {
        const BUILTIN_ROLES: &[&str] = &[
            "Positional",
            "Associative",
            "Callable",
            "Iterable",
            "Numeric",
            "Real",
            "Stringy",
            "Mixy",
            "Setty",
            "Baggy",
            "Blob",
            "Buf",
        ];
        self.is_role(name) || BUILTIN_ROLES.contains(&name)
    }

    /// Error for `but` applied to a *type object* invocant. A role type object
    /// (e.g. `Callable but role {...}`) carries a `ParametricRoleGroupHOW`, which
    /// has no `mixin` metamethod, so raku reports X::Method::NotFound. A non-role
    /// type object cannot be mixed into either (no instance) — that path stays
    /// X::Does::TypeObject.
    fn but_on_type_object_error(&self, type_name: &str) -> RuntimeError {
        if self.is_role_type_name(type_name) {
            return RuntimeError::method_not_found(
                "mixin",
                "Perl6::Metamodel::ParametricRoleGroupHOW",
            );
        }
        Self::does_type_object_error("but", type_name)
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
    /// If `value` is a per-attribute container descriptor produced by
    /// `Attribute.container` (tagged with `__mutsu_attr_container_owner`), return
    /// its `(owner, attr_name)` so a `does` applied to it can be recorded.
    fn attr_container_target(value: &Value) -> Option<(String, String)> {
        let ValueView::Instance { attributes, .. } = value.view() else {
            return None;
        };
        let map = attributes.as_map();
        let owner = map.get("__mutsu_attr_container_owner")?.to_string_value();
        let attr = map.get("__mutsu_attr_container_name")?.to_string_value();
        if owner.is_empty() || attr.is_empty() {
            return None;
        }
        Some((owner, attr))
    }

    /// Record the role-mixin overrides produced by `$attr.container.VAR does
    /// Role(...)` so instance construction mixes the role into the attribute's
    /// value (see `apply_attribute_does_role_mixins`).
    fn record_attr_container_mixin(&mut self, owner: &str, attr: &str, result: &Value) {
        let ValueView::Mixin(_, overrides) = result.view() else {
            return;
        };
        // Drop the sentinel descriptor's own tag keys; keep only the role/
        // attribute mixin keys that apply to a real attribute value.
        let clean: std::collections::HashMap<String, Value> = overrides
            .iter()
            .filter(|(k, _)| !k.starts_with("__mutsu_attr_container_"))
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();
        self.registry_mut()
            .class_attribute_container_mixins
            .entry((owner.to_string(), attr.to_string()))
            .or_default()
            .push(clean);
    }

    fn vm_does_values(&mut self, left: Value, right: Value) -> Result<Value, RuntimeError> {
        // `does`/`but` on a type-object invocant (undefined scalars are stored
        // as Nil and act as the `Any` type object) is illegal *when composing a
        // role or another type* — there is no instance to compose into. Mixing a
        // *concrete value* (e.g. `Method but True`) is still allowed, so this
        // check guards only the role / type-object-RHS branches below.
        let left_type_object = self.does_invocant_type_object(&left);
        // Handle array of roles: `$obj does (RoleA, RoleB)`
        if let ValueView::Array(items, ..) = right.view() {
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
        if matches!(right.view(), ValueView::Enum { .. }) {
            return Self::apply_but_mixin(left, right);
        }
        // When the RHS is a concrete value (Str, Int, Bool, etc.), `does` acts
        // as a mutating mixin — it overrides the corresponding type method.
        // For example, `$o does "modded"` makes `$o.Str` return "modded".
        if matches!(
            right.view(),
            ValueView::Str(_)
                | ValueView::Int(_)
                | ValueView::BigInt(_)
                | ValueView::Num(_)
                | ValueView::Bool(_)
                | ValueView::Rat(..)
        ) {
            return Self::apply_but_mixin(left, right);
        }
        // When the RHS is an Instance, mix it in so that calling .$ClassName
        // on the result returns that instance.
        if matches!(right.view(), ValueView::Instance { .. }) {
            return Self::apply_but_mixin(left, right);
        }
        // A type object / class (not a role) on the RHS: `(my $foo) does Int`
        // mixes into a type-object invocant (X::Does::TypeObject); otherwise the
        // type itself is not composable (`5 does Int`, `obj does SomeClass`).
        if matches!(right.view(), ValueView::Package(_)) {
            if let Some(tn) = &left_type_object {
                return Err(Self::does_type_object_error("does", tn));
            }
            return Err(Self::mixin_not_composable_error(&left, &right));
        }
        // Pure check: does the value conform to the named role/type?
        let role_name = right.to_string_value();
        Ok(Value::truth(left.does_check(&role_name)))
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
        // `does` on a first-class container (`$obj.attr.VAR does Role`): compose
        // onto the inner value and store the mixin back *through* the cell, so
        // every alias of the container (the attribute slot, a `:=`-bound var)
        // observes the mixin. The expression yields the container itself, so a
        // chained `.name` read/write keeps the container identity.
        if let ValueView::ContainerRef(cell) = left.view() {
            let inner = cell.lock().unwrap().clone();
            let result = self.vm_does_values(inner, right)?;
            *cell.lock().unwrap() = result;
            self.stack.push(left);
            return Ok(());
        }
        // `$a.container.VAR does Role(...)` inside a custom `trait_mod:<is>`:
        // record the role mixin against the owning attribute so construction
        // applies it to each instance's attribute value.
        let attr_container = Self::attr_container_target(&left);
        let result = self.vm_does_values(left, right)?;
        if let Some((owner, attr_name)) = attr_container {
            self.record_attr_container_mixin(&owner, &attr_name, &result);
        }
        // Sync back: BUILD submethods may have modified closure variables, so the
        // captured-outer writes reach the caller's slots.
        self.carrier_writeback_changed_aggregates(code, &pre_env);
        // Capture Mixin value for trait_mod writeback (same as DoesVar path)
        if matches!(result.view(), ValueView::Mixin(..)) && self.trait_mod_writeback_key.is_some() {
            self.trait_mod_writeback_value = Some(result.clone());
        }
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_does_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: Option<u32>,
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
        // §1.5: mirror into the compiler-baked local slot when known, else by name.
        self.write_local_slot_or_name(code, slot, &name, updated.clone());
        // Capture Mixin value for trait_mod writeback: when `$r does Role`
        // runs inside a trait_mod:<is>, the Mixin needs to propagate back
        // to the outer scope's `&name` variable.
        if matches!(updated.view(), ValueView::Mixin(..)) && self.trait_mod_writeback_key.is_some()
        {
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
        if let ValueView::Capture { positional, named } = right.view()
            && positional.is_empty()
            && let Some(ValueView::Str(source_name)) =
                named.get("__mutsu_varref_name").map(Value::view)
            && let Some(inner) = named.get("__mutsu_varref_value")
        {
            let source_name = source_name.to_string();
            let inner = inner.clone();
            right = self.capture_var_cell(code, &source_name, inner);
        }
        // Preserve the original key type for `.key` to return the correct type.
        // Use ValuePair for non-string keys, Pair for string keys.
        match left.view() {
            ValueView::Str(_) => {
                let key = left.to_string_value();
                self.stack.push(Value::pair(key, right));
            }
            _ => {
                self.stack.push(Value::value_pair(left, right));
            }
        }
    }
}
