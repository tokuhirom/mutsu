//! Variable trait application (`my $x is Foo` / container parameterization).
use super::*;

impl Interpreter {
    pub(super) fn exec_apply_var_trait_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        trait_name_idx: u32,
        has_arg: bool,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        let trait_name = Self::const_str(code, trait_name_idx).to_string();

        // Handle `is default(...)` as a built-in variable trait.
        if trait_name == "default" {
            let default_value = if has_arg {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Bool(true)
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
                    .locals_get_by_name(code, &name)
                    .or_else(|| self.get_env_with_main_alias(&name))
            {
                let container = self.tag_container_default(container, default_value.clone());
                self.locals_set_by_name(code, &name, container.clone());
                self.set_env_with_main_alias(&name, container.clone());
                // Replace existing Nil and uninitialized (Package("Any"))
                // elements with the default value (Raku container semantics:
                // Nil/uninitialized slots in a defaulted container become
                // the default).
                if name.starts_with('@')
                    && let Value::Array(ref items, kind) = container
                {
                    let is_hole = |v: &Value| {
                        matches!(v, Value::Nil) || matches!(v, Value::Package(n) if n == "Any")
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
                        let new_arr =
                            Value::Array(Arc::new(crate::value::ArrayData::new(replaced)), kind);
                        let new_arr = self.tag_container_default(new_arr, default_value.clone());
                        self.locals_set_by_name(code, &name, new_arr.clone());
                        self.set_env_with_main_alias(&name, new_arr);
                    }
                }
            }
            // If the variable is currently Nil (uninitialized scalar), set it to the default.
            if !name.starts_with('@') && !name.starts_with('%') {
                let current = self.locals_get_by_name(code, &name);
                if matches!(current, Some(Value::Nil) | None) {
                    self.locals_set_by_name(code, &name, default_value.clone());
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
                        match resolved {
                            Some(Value::Package(sym)) => Value::Package(sym),
                            Some(Value::Scalar(inner)) => match *inner {
                                Value::Package(sym) => Value::Package(sym),
                                _ => Value::Package(crate::symbol::Symbol::intern(
                                    if trait_name == "buf" { "Buf" } else { "Blob" },
                                )),
                            },
                            _ => Value::Package(crate::symbol::Symbol::intern(
                                if trait_name == "buf" { "Buf" } else { "Blob" },
                            )),
                        }
                    }
                    _ => Value::Package(crate::symbol::Symbol::intern(&trait_name)),
                };
                // Get current value, convert to buf.
                // The value might be an Array (from the initializer) or already
                // a Buf/Blob Instance (if SetLocal coerced through an old Buf
                // container in the same slot, e.g. in a loop redeclaration).
                let current = self.locals_get_by_name(code, name).unwrap_or(Value::Nil);
                let items = match &current {
                    Value::Array(items, ..) => items
                        .iter()
                        .map(|v| Value::Int(crate::runtime::to_int(v)))
                        .collect(),
                    Value::Instance { attributes, .. } => {
                        // Extract items from an existing Buf/Blob instance
                        if let Some(Value::Array(items, ..)) = attributes.as_map().get("bytes") {
                            items
                                .iter()
                                .map(|v| Value::Int(crate::runtime::to_int(v)))
                                .collect()
                        } else {
                            Vec::new()
                        }
                    }
                    _ => Vec::new(),
                };
                let buf = self.try_compiled_method_or_interpret(buf_type, "new", items)?;
                let name_str = name.to_string();
                self.locals_set_by_name(code, &name_str, buf.clone());
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
            if let Some(container) = self.locals_get_by_name(code, &name_str) {
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                };
                // Hashes embed metadata in `HashData`; store the tagged value
                // back into both the local slot and env.
                let tagged = self.tag_container_metadata(container, info);
                self.locals_set_by_name(code, &name_str, tagged.clone());
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
                let current_val = self.locals_get_by_name(code, &name_str);
                let has_init_values = match &current_val {
                    Some(Value::Hash(h)) => !h.is_empty(),
                    Some(Value::Array(a, _)) => !a.is_empty(),
                    // A Seq/Slip initializer (e.g. `is SetHash = %h.map: {...}`)
                    // must be coerced, not treated as "no initializer".
                    Some(Value::Seq(s) | Value::Slip(s)) => !s.is_empty(),
                    Some(Value::LazyList(_)) => true,
                    // Already converted to a QuantHash by type constraint coercion
                    Some(Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _)) => true,
                    _ => false,
                };
                let mut instance = if has_init_values {
                    let init_val = current_val.unwrap();
                    // If already the target QuantHash type, use it directly
                    let already_target = matches!(
                        (&init_val, base_trait),
                        (Value::Mix(_, _), "MixHash" | "Mix")
                            | (Value::Bag(_, _), "BagHash" | "Bag")
                            | (Value::Set(_, _), "SetHash" | "Set")
                    );
                    if already_target {
                        // Ensure mutability flag matches the trait
                        match init_val {
                            Value::Mix(data, _) => {
                                let mutable = base_trait == "MixHash";
                                Value::Mix(data, mutable)
                            }
                            Value::Bag(data, _) => {
                                let mutable = base_trait == "BagHash";
                                Value::Bag(data, mutable)
                            }
                            Value::Set(data, _) => {
                                let mutable = base_trait == "SetHash";
                                Value::Set(data, mutable)
                            }
                            other => other,
                        }
                    } else {
                        // Convert initial values to the target QuantHash type
                        self.try_compiled_method_or_interpret(init_val, base_trait, vec![])?
                    }
                } else {
                    let type_obj = Value::Package(crate::symbol::Symbol::intern(base_trait));
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
                        let keys: Vec<String> = match &instance {
                            Value::Mix(m, _) => m.weights.keys().cloned().collect(),
                            Value::Bag(b, _) => b.counts.keys().cloned().collect(),
                            Value::Set(s, _) => s.elements.iter().cloned().collect(),
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
                            instance = match instance {
                                Value::Mix(data, mutable) => {
                                    let new_data = crate::value::MixData::with_original_keys(
                                        data.weights.clone(),
                                        typed_keys,
                                    );
                                    Value::Mix(std::sync::Arc::new(new_data), mutable)
                                }
                                Value::Bag(data, mutable) => {
                                    let new_data = crate::value::BagData::with_original_keys(
                                        data.counts.clone(),
                                        typed_keys,
                                    );
                                    Value::Bag(std::sync::Arc::new(new_data), mutable)
                                }
                                Value::Set(data, mutable) => {
                                    let new_data = crate::value::SetData::with_original_keys(
                                        data.elements.clone(),
                                        typed_keys,
                                    );
                                    Value::Set(std::sync::Arc::new(new_data), mutable)
                                }
                                other => other,
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
                self.locals_set_by_name(code, &name_str, instance.clone());
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
            self.stack.pop().unwrap_or(Value::Nil)
        } else {
            Value::Bool(true)
        };
        let target = self.env().get(name).cloned().unwrap_or(Value::Nil);
        // CARRIER: `.VAR` pseudo-method + `trait_mod:<is>` metaprogramming hook
        // (reflective container object + user trait handler). See ledger §C.
        let var_obj = loan_env!(
            self,
            call_method_mut_with_values(name, target, "VAR", vec![])
        )?;
        let named_arg = Value::Pair(trait_name, Box::new(trait_value));
        self.vm_call_function("trait_mod:<is>", vec![var_obj, named_arg])?;
        Ok(())
    }
}
