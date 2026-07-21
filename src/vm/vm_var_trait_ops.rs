//! Variable trait application (`my $x is Foo` / container parameterization).
use super::*;

impl Interpreter {
    pub(super) fn exec_apply_var_trait_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        trait_name_idx: u32,
        has_arg: bool,
        slot: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        let trait_name = Self::const_str(code, trait_name_idx).to_string();
        // Under shadow slots a by-name `position` search resolves to the OUTER
        // same-named slot, so the trait would tag/replace the wrong container
        // (e.g. `my @a is default(42)` shadowing an outer `@a` tagged the outer
        // array and clobbered env with it). Prefer the compile-time-baked slot
        // of the declared variable; OFF keeps the by-name path byte-identical.
        let eff_slot = if crate::compiler::shadow_slots_active() {
            slot
        } else {
            None
        };

        // Handle `is default(...)` as a built-in variable trait.
        if trait_name == "default" {
            let default_value = if has_arg {
                self.stack.pop().unwrap_or(Value::NIL)
            } else {
                Value::TRUE
            };
            let name = name.to_string();
            self.set_var_default(&name, default_value.clone());
            // For array/hash variables, also register the container default
            // so that element access on missing indices returns the default.
            // Fall back to the env value when the variable is not a local slot:
            // an anonymous `(my % is default(...))` in expression position is
            // stored via SetGlobal, not a local, so the embedded default would
            // otherwise be skipped and lost when the value flows out.
            if (name.starts_with('@') || name.starts_with('%'))
                && let Some(container) = self
                    .read_local_slot_or_name(code, eff_slot, &name)
                    .or_else(|| self.get_env_with_main_alias(&name))
            {
                let container = self.tag_container_default(container, default_value.clone());
                self.write_local_slot_or_name(code, eff_slot, &name, container.clone());
                self.set_env_with_main_alias(&name, container.clone());
                // Replace existing Nil and uninitialized (Package("Any"))
                // elements with the default value (Raku container semantics:
                // Nil/uninitialized slots in a defaulted container become
                // the default).
                if name.starts_with('@')
                    && let ValueView::Array(items, kind) = container.view()
                {
                    let is_hole = |v: &Value| {
                        v.is_nil() || matches!(v.view(), ValueView::Package(n) if n == "Any")
                    };
                    let has_holes = items.iter().any(is_hole);
                    if has_holes {
                        let replaced: Vec<Value> = items
                            .iter()
                            .map(|v| {
                                if is_hole(v) {
                                    default_value.clone()
                                } else {
                                    v.clone()
                                }
                            })
                            .collect();
                        let new_arr = Value::array_with_kind(
                            crate::gc::Gc::new(crate::value::ArrayData::new(replaced)),
                            kind,
                        );
                        let new_arr = self.tag_container_default(new_arr, default_value.clone());
                        self.write_local_slot_or_name(code, eff_slot, &name, new_arr.clone());
                        self.set_env_with_main_alias(&name, new_arr);
                    }
                }
            }
            // If the variable is currently Nil (uninitialized scalar), set it to the default.
            if !name.starts_with('@') && !name.starts_with('%') {
                let current = self.read_local_slot_or_name(code, eff_slot, &name);
                if matches!(
                    current.as_ref().map(Value::view),
                    Some(ValueView::Nil) | None
                ) {
                    self.write_local_slot_or_name(code, eff_slot, &name, default_value.clone());
                    self.set_env_with_main_alias(&name, default_value);
                }
            }
            return Ok(());
        }

        // Handle `is Buf/Blob/buf8/...` trait for array variables:
        // converts the array variable into a native typed buffer.
        if name.starts_with('@') {
            if trait_name == "List" {
                if has_arg {
                    self.stack.pop(); // discard unsupported trait argument
                }
                self.mark_readonly(name);
                return Ok(());
            }
            let is_buf_trait = matches!(
                trait_name.as_str(),
                "Buf"
                    | "Blob"
                    | "buf"
                    | "blob"
                    | "buf8"
                    | "buf16"
                    | "buf32"
                    | "buf64"
                    | "blob8"
                    | "blob16"
                    | "blob32"
                    | "blob64"
                    | "utf8"
            );
            if is_buf_trait {
                if has_arg {
                    self.stack.pop(); // discard unused arg
                }
                let buf_type = match trait_name.as_str() {
                    "buf" | "blob" => {
                        let resolved = self
                            .call_function("EVAL", vec![Value::str(trait_name.clone())])
                            .ok()
                            .or_else(|| self.get_env_with_main_alias(&trait_name))
                            .or_else(|| self.locals_get_by_name(code, &trait_name));
                        match resolved.as_ref().map(Value::view) {
                            Some(ValueView::Package(sym)) => Value::package(sym),
                            Some(ValueView::Scalar(inner)) => match inner.view() {
                                ValueView::Package(sym) => Value::package(sym),
                                _ => Value::package(crate::symbol::Symbol::intern(
                                    if trait_name == "buf" { "Buf" } else { "Blob" },
                                )),
                            },
                            _ => Value::package(crate::symbol::Symbol::intern(
                                if trait_name == "buf" { "Buf" } else { "Blob" },
                            )),
                        }
                    }
                    _ => Value::package(crate::symbol::Symbol::intern(&trait_name)),
                };
                // Get current value, convert to buf.
                // The value might be an Array (from the initializer) or already
                // a Buf/Blob Instance (if SetLocal coerced through an old Buf
                // container in the same slot, e.g. in a loop redeclaration).
                let current = self
                    .read_local_slot_or_name(code, eff_slot, name)
                    .unwrap_or(Value::NIL);
                let items = match current.view() {
                    ValueView::Array(items, ..) => items
                        .iter()
                        .map(|v| Value::int(crate::runtime::to_int(v)))
                        .collect(),
                    ValueView::Instance { attributes, .. } => {
                        // Extract items from an existing Buf/Blob instance
                        if let Some(ValueView::Array(items, ..)) =
                            attributes.as_map().get("bytes").map(Value::view)
                        {
                            items
                                .iter()
                                .map(|v| Value::int(crate::runtime::to_int(v)))
                                .collect()
                        } else {
                            Vec::new()
                        }
                    }
                    _ => Vec::new(),
                };
                let buf = self.try_compiled_method_or_interpret(buf_type, "new", items)?;
                let name_str = name.to_string();
                self.write_local_slot_or_name(code, eff_slot, &name_str, buf.clone());
                self.set_env_with_main_alias(&name_str, buf);
                return Ok(());
            }
        }

        // Handle `is Map` on hash variables: register the hash as a Map type
        // and mark it read-only (Maps are immutable in Raku).
        if name.starts_with('%') && trait_name == "Map" {
            if has_arg {
                self.stack.pop(); // discard unused trait argument
            }
            let name_str = name.to_string();
            // Register container type metadata with declared_type "Map"
            if let Some(container) = self.read_local_slot_or_name(code, eff_slot, &name_str) {
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                };
                // Hashes embed metadata in `HashData`; store the tagged value
                // back into both the local slot and env.
                let tagged = self.tag_container_metadata(container, info);
                self.write_local_slot_or_name(code, eff_slot, &name_str, tagged.clone());
                self.set_env_with_main_alias(&name_str, tagged);
            }
            // Mark the variable read-only to prevent mutation
            self.mark_readonly(&name_str);
            return Ok(());
        }

        // Handle `is BagHash`, `is SetHash`, `is MixHash`, `is Bag`, `is Set`, `is Mix`
        // (and parameterized versions like `is Set[Int]`, `is Bag[Str]`)
        // on hash variables: replace the variable with an instance of the appropriate type.
        if name.starts_with('%') {
            let base_trait = trait_name.split('[').next().unwrap_or(&trait_name);
            let is_hash_like_trait = matches!(
                base_trait,
                "BagHash" | "SetHash" | "MixHash" | "Bag" | "Set" | "Mix"
            );
            if is_hash_like_trait {
                if has_arg {
                    self.stack.pop(); // discard unused trait argument
                }
                let name_str = name.to_string();
                // Check if the variable already has initial values from the declaration.
                // If so, construct the QuantHash from those values instead of creating
                // an empty one. This handles `my %h is Bag = <a b b c>`.
                let current_val = self.read_local_slot_or_name(code, eff_slot, &name_str);
                let has_init_values = match current_val.as_ref().map(Value::view) {
                    Some(ValueView::Hash(h)) => !h.is_empty(),
                    Some(ValueView::Array(a, _)) => !a.is_empty(),
                    // A Seq/Slip initializer (e.g. `is SetHash = %h.map: {...}`)
                    // must be coerced, not treated as "no initializer".
                    Some(ValueView::Seq(s) | ValueView::Slip(s)) => !s.is_empty(),
                    Some(ValueView::LazyList(_)) => true,
                    // Already converted to a QuantHash by type constraint coercion
                    Some(ValueView::Set(_, _) | ValueView::Bag(_, _) | ValueView::Mix(_, _)) => {
                        true
                    }
                    Some(ValueView::Nil) | Some(ValueView::Package(_)) | None => false,
                    // A scalar initializer — `my %s is SetHash = <a>` (a single
                    // angle-quoted word is a Str, not a List), a Pair, an Int
                    // weight, ... — is an init value to coerce; treating it as
                    // "no initializer" would overwrite the slot with an EMPTY
                    // QuantHash and silently drop the initialization.
                    Some(_) => true,
                };
                let mut instance = if has_init_values {
                    let init_val = current_val.unwrap();
                    // If already the target QuantHash type, use it directly
                    let already_target = matches!(
                        (init_val.view(), base_trait),
                        (ValueView::Mix(_, _), "MixHash" | "Mix")
                            | (ValueView::Bag(_, _), "BagHash" | "Bag")
                            | (ValueView::Set(_, _), "SetHash" | "Set")
                    );
                    if already_target {
                        // Ensure mutability flag matches the trait
                        match init_val.view() {
                            ValueView::Mix(data, _) => {
                                let mutable = base_trait == "MixHash";
                                Value::mix_parts(data.clone(), mutable)
                            }
                            ValueView::Bag(data, _) => {
                                let mutable = base_trait == "BagHash";
                                Value::bag_parts(data.clone(), mutable)
                            }
                            ValueView::Set(data, _) => {
                                let mutable = base_trait == "SetHash";
                                Value::set_parts(data.clone(), mutable)
                            }
                            _ => init_val,
                        }
                    } else {
                        // Convert initial values to the target QuantHash type
                        self.try_compiled_method_or_interpret(init_val, base_trait, vec![])?
                    }
                } else {
                    let type_obj = Value::package(crate::symbol::Symbol::intern(base_trait));
                    self.try_compiled_method_or_interpret(type_obj, "new", vec![])?
                };
                // Type check for parameterized QuantHash (e.g. Mix[Int], Set[Str])
                // and store typed original_keys so .keys returns typed values
                if let Some(bracket_pos) = trait_name.find('[') {
                    let constraint = &trait_name[bracket_pos + 1..trait_name.len() - 1];
                    if constraint.starts_with(char::is_uppercase)
                        && constraint != "Any"
                        && constraint != "Mu"
                    {
                        let keys: Vec<String> = match instance.view() {
                            ValueView::Mix(m, _) => m.weights.keys().cloned().collect(),
                            ValueView::Bag(b, _) => b.counts.keys().cloned().collect(),
                            ValueView::Set(s, _) => s.elements.iter().cloned().collect(),
                            _ => vec![],
                        };
                        // Try to coerce keys to the constraint type for type checking
                        let mut typed_keys = std::collections::HashMap::new();
                        for key in &keys {
                            let coerced = self.try_coerce_str_to_type(key, constraint);
                            if let Some(ref typed_val) = coerced {
                                if !self.type_matches_value(constraint, typed_val) {
                                    let got_type = crate::value::what_type_name(typed_val);
                                    return Err(RuntimeError::typecheck_binding_parameter(
                                        key, constraint, &got_type, None,
                                    ));
                                }
                                typed_keys.insert(key.clone(), typed_val.clone());
                            } else {
                                // Can't coerce to type - check original string
                                let key_val = Value::str(key.clone());
                                if !self.type_matches_value(constraint, &key_val) {
                                    let got_type = crate::value::what_type_name(&key_val);
                                    return Err(RuntimeError::typecheck_binding_parameter(
                                        key, constraint, &got_type, None,
                                    ));
                                }
                            }
                        }
                        // Store typed keys in the QuantHash so .keys returns typed values
                        if !typed_keys.is_empty() {
                            instance = match instance.view() {
                                ValueView::Mix(data, mutable) => {
                                    let new_data = crate::value::MixData::with_original_keys(
                                        data.weights.clone(),
                                        typed_keys,
                                    );
                                    Value::mix_parts(crate::gc::Gc::new(new_data), mutable)
                                }
                                ValueView::Bag(data, mutable) => {
                                    let new_data = crate::value::BagData::with_original_keys(
                                        data.counts.clone(),
                                        typed_keys,
                                    );
                                    Value::bag_parts(crate::gc::Gc::new(new_data), mutable)
                                }
                                ValueView::Set(data, mutable) => {
                                    let new_data = crate::value::SetData::with_original_keys(
                                        data.elements.clone(),
                                        typed_keys,
                                    );
                                    Value::set_parts(crate::gc::Gc::new(new_data), mutable)
                                }
                                _ => instance,
                            };
                        }
                    }
                }
                // Embed container type metadata so assignment operations
                // know to coerce back to BagHash/SetHash/etc.
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some(trait_name.clone()),
                };
                let instance = self.tag_container_metadata(instance, info);
                self.write_local_slot_or_name(code, eff_slot, &name_str, instance.clone());
                self.set_env_with_main_alias(&name_str, instance.clone());
                // Set type constraint so future assignments are coerced correctly
                self.vm_set_var_type_constraint(&name_str, Some(trait_name.clone()));
                return Ok(());
            }
        }

        // For array/hash variables with a class trait that inherits from Array[X] or Hash[V,K],
        // propagate the element type constraint so .of works correctly.
        if name.starts_with('@') || name.starts_with('%') {
            let element_type = self.find_parameterized_container_parent(&trait_name);
            if let Some(et) = element_type {
                if has_arg {
                    self.stack.pop(); // discard unused trait argument
                }
                let name_str = name.to_string();
                self.vm_set_var_type_constraint(&name_str, Some(et));
                return Ok(());
            }
        }

        // `my %h is CustomClass` where CustomClass composes `Associative` (e.g.
        // `does Hash::Agnostic`): back the variable with a blessed instance, so
        // subscripting (AT-KEY/ASSIGN-KEY/DELETE-KEY), iteration and coercion
        // methods (.Str/.raku/.List/.Slip/.gist/...) all dispatch to the class's
        // (possibly role-provided) methods — a "tied hash". The instance keeps
        // its identity across `%h = ...` (STORE) reassignments.
        if name.starts_with('%')
            && self.registry().classes.contains_key(&trait_name)
            && self.class_does_role(&trait_name, "Associative")
        {
            if has_arg {
                self.stack.pop();
            }
            let name_str = name.to_string();
            // Gather any initializer values (`my %h is Foo = @pairs` assigns the
            // initializer before this trait op runs) as Pairs / a flat kv list.
            let init_source = self
                .read_local_slot_or_name(code, slot, &name_str)
                .or_else(|| self.get_env_with_main_alias(&name_str));
            let init_values: Vec<Value> = match init_source.as_ref().map(Value::view) {
                Some(ValueView::Hash(h)) if !h.is_empty() => h
                    .iter()
                    .map(|(k, v)| Value::pair(k.clone(), v.clone()))
                    .collect(),
                Some(ValueView::Array(a, _)) if !a.is_empty() => a.iter().cloned().collect(),
                Some(ValueView::Seq(s) | ValueView::Slip(s)) if !s.is_empty() => {
                    s.iter().cloned().collect()
                }
                _ => Vec::new(),
            };
            let type_obj = Value::package(crate::symbol::Symbol::intern(&trait_name));
            let instance = self.try_compiled_method_or_interpret(type_obj, "new", vec![])?;
            // Bind the instance to the variable first, then STORE the initializer
            // through the bound variable so the mutating dispatch resolves `self`
            // to the same instance the variable now holds.
            self.write_local_slot_or_name(code, eff_slot, &name_str, instance.clone());
            self.set_env_with_main_alias(&name_str, instance.clone());
            if !init_values.is_empty() {
                // Pass the initializer as a single positional list (not as
                // separate Pair args, which STORE's signature would bind as
                // *named* arguments); STORE's slurpy `*@values` flattens it.
                let list_arg = Value::array(init_values);
                let stored = self.try_compiled_method_or_interpret(
                    instance.clone(),
                    "STORE",
                    vec![list_arg],
                )?;
                let bound = if matches!(stored.view(), ValueView::Instance { .. }) {
                    stored
                } else {
                    instance
                };
                self.write_local_slot_or_name(code, eff_slot, &name_str, bound.clone());
                self.set_env_with_main_alias(&name_str, bound);
            }
            return Ok(());
        }

        if !(self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>")) {
            // For uppercase type-like traits (e.g. `is Map`, `is Set`), silently
            // accept them even if trait_mod:<is> is not defined. These are type
            // container traits that may not have runtime trait_mod:<is> handlers.
            if trait_name
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase())
            {
                if has_arg {
                    self.stack.pop();
                }
                return Ok(());
            }
            return Err(RuntimeError::new(format!(
                "X::Comp::Trait::Unknown: Unknown variable trait 'is {}'",
                trait_name
            )));
        }
        let trait_value = if has_arg {
            self.stack.pop().unwrap_or(Value::NIL)
        } else {
            Value::TRUE
        };
        let target = self.env().get(name).cloned().unwrap_or(Value::NIL);
        // CARRIER: `.VAR` pseudo-method + `trait_mod:<is>` metaprogramming hook
        // (reflective container object + user trait handler). See ledger §C.
        let var_obj = loan_env!(
            self,
            call_method_mut_with_values(name, target, "VAR", vec![])
        )?;
        let named_arg = Value::pair(trait_name, trait_value);
        self.vm_call_function("trait_mod:<is>", vec![var_obj, named_arg])?;
        Ok(())
    }
}
