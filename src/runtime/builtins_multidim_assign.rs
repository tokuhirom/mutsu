//! Multi-dimensional index-assignment and assign-through-accessor lvalue ops.
use super::*;

impl Interpreter {
    /// Handle `$obj.method<key> = value` — index assignment through a method accessor.
    /// Gets the current container (hash/array) via the accessor, modifies it, writes back.
    pub(super) fn builtin_index_assign_method_lvalue(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 5 {
            return Err(RuntimeError::new(
                "__mutsu_index_assign_method_lvalue expects target, method, index, value, var_name",
            ));
        }
        let target = args[0].clone();
        let method = args[1].to_string_value();
        let index = args[2].clone();
        let value = args[3].clone();
        let var_name = args[4].to_string_value();

        // Get the current container via the accessor
        let current = self.call_method_with_values(target.clone(), &method, Vec::new())?;
        // Slice 2a: the accessor may return a shared `ContainerRef` cell (e.g. a
        // Pair value aliasing a `=`-array-shared scalar `my $a = @src`). Deref it
        // for the element modify; the shared-Arc propagation below
        // (`overwrite_array_bindings_by_identity`, now cell-aware) reaches every
        // alias through the cell's inner Arc.
        let current = match &current {
            Value::ContainerRef(cell) => cell.lock().unwrap().clone(),
            _ => current,
        };

        // Save Arc pointers before modifying (for shared container propagation)
        let old_array_arc = match &current {
            Value::Array(arc, ..) => Some(arc.clone()),
            _ => None,
        };
        let old_hash_arc = match &current {
            Value::Hash(arc) => Some(arc.clone()),
            _ => None,
        };

        // Check if index is multi-dimensional (array of indices like [2, 1] from [2;1])
        let dims: Vec<usize> = if let Value::Array(ref items, ..) = index {
            items
                .iter()
                .map(|v| crate::runtime::to_int(v) as usize)
                .collect()
        } else {
            Vec::new()
        };

        // When assigning Nil to a container element with `is default(...)`,
        // restore the default value instead of Nil.
        let effective_value = if matches!(value, Value::Nil) {
            if let Some(def) = self.container_default(&current).cloned() {
                def
            } else {
                // Check class_attribute_default for instance attributes
                let class_default = if let Value::Instance { class_name, .. } = &target {
                    self.class_attribute_default(&class_name.resolve(), &method)
                } else {
                    None
                };
                class_default.unwrap_or_else(|| value.clone())
            }
        } else {
            value.clone()
        };

        // Type check for typed hash/array attribute subscript assignment
        // (e.g., $o.h<a> = 'b' where h is Int, or $o.a[2] = $*IN where a is Int)
        if let Value::Instance { class_name, .. } = &target {
            let tc = self.get_attr_type_constraint(&class_name.resolve(), &method);
            let is_hash_attr = matches!(&current, Value::Hash(_));
            let is_array_attr = matches!(&current, Value::Array(..));
            // An object hash (`%.h{Str:D}`) checks elements against the value type.
            let elem_tc = tc.as_deref().map(|t| {
                crate::runtime::types::split_object_hash_constraint(t)
                    .0
                    .to_string()
            });
            if (is_hash_attr || is_array_attr)
                && let Some(ref type_constraint) = elem_tc
                && !matches!(type_constraint.as_str(), "Mu" | "Any")
                && !matches!(effective_value, Value::Nil)
                && !self.type_matches_value(type_constraint, &effective_value)
            {
                let sigil = if is_hash_attr { "%" } else { "@" };
                return Err(crate::runtime::RuntimeError::new(format!(
                    "Type check failed for an element of {}{}; expected {} but got {}",
                    sigil,
                    method,
                    type_constraint,
                    crate::runtime::utils::value_type_name(&effective_value),
                )));
            }
            // Detect autovivification into typed hash attribute:
            // $o.h<key1><key2> = val  would autovivify h<key1> as a Hash,
            // but if h is typed (e.g. Int), a Hash is not a valid value.
            if is_hash_attr
                && let Some(ref type_constraint) = tc
                && !matches!(type_constraint.as_str(), "Mu" | "Any" | "Hash")
            {
                // The assignment target is a subscript on the hash.
                // If the value we're assigning is itself a subscript/nested assignment,
                // the effective_value would be valid, but the autovivification of the
                // intermediate key would create a Hash value, which fails the type check.
                // We detect this by checking if effective_value itself is a Hash
                // (which would happen in nested assignment like h<key><sub> = val).
                if matches!(&effective_value, Value::Hash(_)) {
                    return Err(crate::runtime::RuntimeError::new(format!(
                        "Type check failed in assignment to %{}; expected {} but got Hash",
                        method, type_constraint,
                    )));
                }
            }
        }
        // Also check via container type metadata (for non-attribute typed hashes/arrays)
        {
            let is_hash_attr = matches!(&current, Value::Hash(_));
            let is_array_attr = matches!(&current, Value::Array(..));
            if (is_hash_attr || is_array_attr)
                && let Some(info) = self.container_type_metadata(&current)
            {
                let constraint = &info.value_type.clone();
                if constraint != "Mu"
                    && constraint != "Any"
                    && !self.type_matches_value(constraint, &effective_value)
                {
                    let sigil = if is_hash_attr { "%" } else { "@" };
                    return Err(crate::runtime::RuntimeError::new(format!(
                        "Type check failed for an element of {}; expected {} but got {}",
                        sigil,
                        constraint,
                        crate::runtime::utils::value_type_name(&effective_value),
                    )));
                }
            }
        }

        // Modify the container
        let updated = if dims.len() >= 2 {
            // Multi-dimensional index assignment (e.g., $c.a[2;1] = value)
            Self::multidim_assign_nested(current, &dims, effective_value.clone())?
        } else {
            let key = index.to_string_value();
            match current {
                Value::Hash(ref h) => {
                    // Check for autovivification via nested subscript assignment:
                    // If the hash attribute has a type constraint and the key doesn't exist,
                    // Raku would normally autovivify a hash value, but for typed Int/etc,
                    // this should fail because {} is not an Int.
                    // (This is handled by type check above for the actual value being assigned.)
                    let mut new_hash = (**h).clone();
                    new_hash.insert(key, effective_value.clone());
                    Value::hash(new_hash)
                }
                Value::Array(ref items, kind) => {
                    let idx = crate::runtime::to_int(&index) as usize;
                    let mut new_items = (**items).clone();
                    if idx >= new_items.len() {
                        if crate::runtime::utils::is_shaped_array(&current) {
                            return Err(RuntimeError::new("Index out of bounds"));
                        }
                        new_items.resize(
                            idx + 1,
                            Value::Package(crate::symbol::Symbol::intern("Any")),
                        );
                    }
                    new_items[idx] = effective_value.clone();
                    Value::Array(std::sync::Arc::new(new_items), kind)
                }
                _ => return Ok(effective_value),
            }
        };

        // Propagate container changes to all instances sharing the same
        // Arc (handles clone semantics where multiple instances share the
        // same array/hash container).
        if let Some(old_arc) = &old_array_arc {
            self.propagate_shared_array_in_instances(old_arc, &updated);
            // Also propagate to plain variables sharing the same Arc, so a
            // mutation through a Pair value (`$pair.value[0] = x`) writes back
            // to the source variable the Pair aliases (`my $a = [...]; $p = ($a
            // => $a); $p.value[0] = x` updates `$a`). See roast S02-types/pair.t.
            self.overwrite_array_bindings_by_identity(old_arc, updated.clone());
        }
        if let Some(old_arc) = &old_hash_arc {
            self.propagate_shared_hash_in_instances(old_arc, &updated);
            self.overwrite_hash_bindings_by_identity(old_arc, updated.clone());
        }

        // Write back via the setter
        self.assign_method_lvalue_with_values(
            if var_name.is_empty() {
                None
            } else {
                Some(var_name.as_str())
            },
            target,
            &method,
            Vec::new(),
            updated,
        )?;
        Ok(effective_value)
    }

    /// Handle `$obj.method<key>:delete` — element delete through a method accessor.
    /// Gets the current container (hash/array) via the accessor, removes the
    /// element, writes the modified container back through the setter, and returns
    /// the removed value (or the container's `is default(...)` for an absent key).
    pub(super) fn builtin_index_delete_method_lvalue(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 4 {
            return Err(RuntimeError::new(
                "__mutsu_index_delete_method_lvalue expects target, method, index, var_name",
            ));
        }
        let target = args[0].clone();
        let method = args[1].to_string_value();
        let index = args[2].clone();
        let var_name = args[3].to_string_value();

        let current = self.call_method_with_values(target.clone(), &method, Vec::new())?;
        let current = match &current {
            Value::ContainerRef(cell) => cell.lock().unwrap().clone(),
            _ => current,
        };
        // The default returned for an absent key: the container's own
        // `is default(...)`, else the attribute's declared default, else Nil.
        let absent_default = self.container_default(&current).cloned().or_else(|| {
            if let Value::Instance { class_name, .. } = &target {
                self.class_attribute_default(&class_name.resolve(), &method)
            } else {
                None
            }
        });

        // Save the pre-delete Arc identity so the modified container can be
        // propagated to every instance/binding sharing it (mirrors the assign path).
        let old_array_arc = match &current {
            Value::Array(arc, ..) => Some(arc.clone()),
            _ => None,
        };
        let old_hash_arc = match &current {
            Value::Hash(arc) => Some(arc.clone()),
            _ => None,
        };

        let (removed, updated) = match &current {
            Value::Hash(h) => {
                let key = index.to_string_value();
                let mut new_hash = (**h).clone();
                let removed = new_hash
                    .remove(&key)
                    .unwrap_or_else(|| absent_default.clone().unwrap_or(Value::Nil));
                (removed, Value::Hash(std::sync::Arc::new(new_hash)))
            }
            Value::Array(items, kind) => {
                let idx = crate::runtime::to_int(&index);
                let mut new_items = (**items).clone();
                let removed = if idx >= 0 && (idx as usize) < new_items.len() {
                    let i = idx as usize;
                    let r = new_items[i].clone();
                    // Trailing element shrinks; an interior delete leaves a hole.
                    if i + 1 == new_items.len() {
                        new_items.truncate(i);
                    } else {
                        new_items[i] = Value::Nil;
                    }
                    r
                } else {
                    absent_default.clone().unwrap_or(Value::Nil)
                };
                (removed, Value::Array(std::sync::Arc::new(new_items), *kind))
            }
            _ => return Ok(absent_default.unwrap_or(Value::Nil)),
        };

        if let Some(old_arc) = &old_array_arc {
            self.propagate_shared_array_in_instances(old_arc, &updated);
            self.overwrite_array_bindings_by_identity(old_arc, updated.clone());
        }
        if let Some(old_arc) = &old_hash_arc {
            self.propagate_shared_hash_in_instances(old_arc, &updated);
            self.overwrite_hash_bindings_by_identity(old_arc, updated.clone());
        }

        self.assign_method_lvalue_with_values(
            if var_name.is_empty() {
                None
            } else {
                Some(var_name.as_str())
            },
            target,
            &method,
            Vec::new(),
            updated,
        )?;
        Ok(removed)
    }

    /// Handle nested subscript assignment on a typed attribute accessor.
    /// Called for patterns like `$o.a[42]<foo> = 3` or `$o.h<key1><key2> = val`.
    /// Detects when the intermediate container element is Nil and the attribute
    /// has a type constraint that would prevent autovivification of a Hash/Array.
    pub(super) fn builtin_index_assign_method_lvalue_nested(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 5 {
            return Err(RuntimeError::new(
                "__mutsu_index_assign_method_lvalue_nested expects target, method, inner_index, outer_index, value[, var_name]",
            ));
        }
        let target = args[0].clone();
        let method = args[1].to_string_value();
        let inner_index = args[2].clone();
        let _outer_index = args[3].clone();
        let value = args[4].clone();

        // Get the attribute container
        let container = self.call_method_with_values(target.clone(), &method, Vec::new())?;

        // For typed containers, check if the inner element is Nil (would need autovivification)
        if let Value::Instance { class_name, .. } = &target
            && let Some(type_constraint) =
                self.get_attr_type_constraint(&class_name.resolve(), &method)
            && !matches!(type_constraint.as_str(), "Mu" | "Any" | "Hash" | "Map")
        {
            // Check if the inner_index slot is Nil/undef (would need autovivification)
            let inner_val = match &container {
                Value::Array(items, ..) => {
                    let idx = crate::runtime::to_int(&inner_index) as usize;
                    items.get(idx).cloned().unwrap_or(Value::Nil)
                }
                Value::Hash(map) => {
                    let key = inner_index.to_string_value();
                    map.get(&key).cloned().unwrap_or(Value::Nil)
                }
                _ => Value::Nil,
            };
            let is_nil = matches!(&inner_val, Value::Nil | Value::Package(_));
            if is_nil {
                // Autovivification would create a Hash at this slot,
                // but the container is typed, so it's not allowed.
                let sigil = if matches!(&container, Value::Hash(_)) {
                    "%"
                } else {
                    "@"
                };
                return Err(RuntimeError::new(format!(
                    "Type check failed for an element of {}{} (no autovivification in typed container); expected {} but got Hash",
                    sigil, method, type_constraint,
                )));
            }
        }

        // If we get here (element is not Nil or no type constraint),
        // fall through to do the actual nested assignment on the current element.
        // For now, just return the value (the assignment silently does nothing
        // if the intermediate value is not a container).
        // TODO: implement proper nested assignment for non-Nil elements.
        let _ = value;
        Ok(Value::Nil)
    }

    /// Assign a value into a nested multi-dimensional array structure.
    /// `dims` contains the indices for each dimension, e.g. [2, 1] for @a[2;1].
    /// Checks bounds against the shaped array dimensions.
    pub(super) fn multidim_assign_nested(
        container: Value,
        dims: &[usize],
        value: Value,
    ) -> Result<Value, RuntimeError> {
        if dims.is_empty() {
            return Ok(value);
        }
        // Check bounds against shape if this is a shaped array
        let shape = crate::runtime::utils::shaped_array_shape(&container);
        if let Some(ref shape) = shape {
            for (i, &idx) in dims.iter().enumerate() {
                if i < shape.len() && idx >= shape[i] {
                    return Err(RuntimeError::new("Index out of bounds"));
                }
            }
        }
        match container {
            Value::Array(ref items, kind) => {
                let idx = dims[0];
                let mut new_items = (**items).clone();
                if idx >= new_items.len() {
                    new_items.resize(
                        idx + 1,
                        Value::Package(crate::symbol::Symbol::intern("Any")),
                    );
                }
                if dims.len() == 1 {
                    new_items[idx] = value;
                } else {
                    let inner = new_items[idx].clone();
                    new_items[idx] = Self::multidim_assign_nested(inner, &dims[1..], value)?;
                }
                let result = Value::Array(std::sync::Arc::new(new_items), kind);
                // Preserve the shape registration on the new Arc so subsequent
                // bounds checks (via shaped_array_shape) still work.
                if let Some(ref shape) = shape {
                    crate::runtime::utils::mark_shaped_array(&result, Some(shape));
                }
                Ok(result)
            }
            _ => {
                // If it's not an array, wrap the assignment in a fresh array
                if dims.len() == 1 {
                    let idx = dims[0];
                    let mut new_items =
                        vec![Value::Package(crate::symbol::Symbol::intern("Any")); idx + 1];
                    new_items[idx] = value;
                    Ok(Value::real_array(new_items))
                } else {
                    Err(RuntimeError::new(
                        "Multi-dimensional index on non-array container",
                    ))
                }
            }
        }
    }

    pub(super) fn builtin_assign_method_lvalue(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 4 {
            return Err(RuntimeError::new(
                "__mutsu_assign_method_lvalue expects target, method name, method args, and value",
            ));
        }
        let target = args[0].clone();
        let method = args[1].to_string_value();
        let method_args = match &args[2] {
            Value::Array(items, ..) => items.to_vec(),
            Value::Nil => Vec::new(),
            other => vec![other.clone()],
        };
        let value = args[3].clone();
        let target_var = args.get(4).and_then(|v| {
            let name = v.to_string_value();
            if name.is_empty() { None } else { Some(name) }
        });
        self.assign_method_lvalue_with_values(
            target_var.as_deref(),
            target,
            &method,
            method_args,
            value,
        )
    }
}
