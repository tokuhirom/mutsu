//! Multi-dimensional index-assignment and assign-through-accessor lvalue ops.
use super::*;
use crate::value::ValueView;

impl Interpreter {
    fn detached_lvalue_value(value: &Value) -> Value {
        match value.view() {
            ValueView::ContainerRef(cell) => {
                Self::detached_lvalue_value(&cell.lock().unwrap().clone())
            }
            ValueView::Hash(hash) => Value::hash(
                hash.iter()
                    .map(|(key, value)| (key.clone(), Self::detached_lvalue_value(value)))
                    .collect::<std::collections::HashMap<_, _>>(),
            ),
            ValueView::Array(array, kind) => Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(
                    array
                        .iter()
                        .map(Self::detached_lvalue_value)
                        .collect::<Vec<_>>(),
                )),
                kind,
            ),
            _ => value.clone(),
        }
    }

    /// Handle `$obj.method<key> = value` — index assignment through a method accessor.
    /// Gets the current container (hash/array) via the accessor, modifies it, writes back.
    pub(super) fn builtin_index_assign_method_lvalue(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 5 {
            return Err(RuntimeError::new(
                "__mutsu_index_assign_method_lvalue expects target, method, optional method args, index, value, var_name",
            ));
        }
        let target = args[0].clone();
        let method = args[1].to_string_value();
        let has_method_args = args.len() >= 6;
        let method_args = if has_method_args {
            Self::sub_call_args_from_value(args.get(2))
        } else {
            Vec::new()
        };
        let offset = usize::from(has_method_args);
        let index = args[2 + offset].clone();
        let value = args[3 + offset].clone();
        let var_name = args[4 + offset].to_string_value();

        // Path accessors conventionally receive `(root, @steps)`. Resolve that
        // shape from the supplied root so the selected container stays anchored
        // there even when the accessor's `return-rw` temporary is unwound.
        let current = if matches!(target.view(), ValueView::Package(_))
            && let Some(root) = method_args.first()
            && let Some(steps) = method_args.get(1)
            && let ValueView::Array(steps, _) = steps.view()
        {
            let mut selected = root.clone();
            for step in steps.iter() {
                selected = match selected.view() {
                    ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
                    ValueView::Hash(hash) => hash
                        .get(&step.to_string_value())
                        .cloned()
                        .unwrap_or(Value::NIL),
                    ValueView::Array(array, _) => step
                        .to_string_value()
                        .parse::<usize>()
                        .ok()
                        .and_then(|index| array.get(index).cloned())
                        .unwrap_or(Value::NIL),
                    _ => Value::NIL,
                };
            }
            selected
        } else {
            self.call_method_with_values(target.clone(), &method, method_args.clone())?
        };
        // Slice 2a: the accessor may return a shared `ContainerRef` cell (e.g. a
        // Pair value aliasing a `=`-array-shared scalar `my $a = @src`). Deref it
        // for the element modify; the shared-Arc propagation below
        // (`overwrite_array_bindings_by_identity`, now cell-aware) reaches every
        // alias through the cell's inner Arc.
        let current = match current.view() {
            ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
            _ => current,
        };

        // Package-level `is rw` accessors with arguments (for example
        // `Crane::At.at($root, @path)`) return the selected container itself.
        // Mutate that container directly: invoking the accessor again as a
        // setter would resolve a new temporary instead of preserving the
        // returned container's identity.
        if matches!(target.view(), ValueView::Package(_)) {
            match current.view() {
                ValueView::Hash(hash) => {
                    let key = index.to_string_value();
                    let hash = unsafe { crate::value::gc_contents_mut(&hash) };
                    Value::hash_insert_through(&mut hash.map, key, value.clone());
                    if let Some(root) = method_args.first() {
                        self.env
                            .insert(var_name.clone(), Self::detached_lvalue_value(root));
                    }
                    return Ok(value);
                }
                ValueView::Array(array, _) => {
                    if let Ok(index) = index.to_string_value().parse::<usize>() {
                        let items = unsafe { crate::value::gc_contents_mut(&array) }.items_mut();
                        Self::autoviv_resize(
                            items,
                            index + 1,
                            Value::package(crate::symbol::Symbol::intern("Any")),
                        )?;
                        Value::assign_element_slot(&mut items[index], value.clone());
                        if let Some(root) = method_args.first() {
                            self.env
                                .insert(var_name.clone(), Self::detached_lvalue_value(root));
                        }
                        return Ok(value);
                    }
                }
                _ => {}
            }
        }

        // The accessor returned an Associative/Positional OBJECT (URI's
        // `$u.query<foo> = v` — `.query` yields a URI::Query instance):
        // dispatch the raku subscript protocol on it instead of treating it
        // as a plain container. Instance-internal mutation travels through
        // the shared attribute cell, so no write-back is needed.
        if let ValueView::Instance { class_name, .. } = current.view() {
            let (primary, secondary) = if matches!(index.view(), ValueView::Int(_)) {
                ("ASSIGN-POS", "ASSIGN-KEY")
            } else {
                ("ASSIGN-KEY", "ASSIGN-POS")
            };
            let cn = class_name.resolve();
            let m = if self.has_user_method(&cn, primary) {
                Some(primary)
            } else if self.has_user_method(&cn, secondary) {
                Some(secondary)
            } else {
                None
            };
            if let Some(m) = m {
                let idx_arg = match index.view() {
                    ValueView::Array(items, _) if items.len() == 1 => items[0].clone(),
                    ValueView::Seq(items) if items.len() == 1 => items[0].clone(),
                    ValueView::Slip(items) if items.len() == 1 => items[0].clone(),
                    _ => index.clone(),
                };
                // A Pair VALUE must arrive as a positional argument, not be
                // eaten as a named arg.
                let val_arg = match value.view() {
                    ValueView::Pair(k, v) => Value::value_pair(Value::str(k.clone()), v.clone()),
                    _ => value.clone(),
                };
                self.call_method_with_values(current.clone(), m, vec![idx_arg, val_arg])?;
                return Ok(value);
            }
        }

        // The accessor returned a punned ROLE object (`has %.Converter is
        // DBDish::TypeConverter`, then `$obj.Converter{Int} = $sub`). Its
        // subscript is served by the container attribute the role delegates
        // AT-KEY/ASSIGN-KEY to, and the mutation travels through the wrapped
        // instance's shared attribute cell, so nothing has to be written back
        // here. Without this the object was replaced by a plain Hash.
        if matches!(current.view(), ValueView::Mixin(..)) {
            let idx_arg = match index.view() {
                ValueView::Array(items, _) if items.len() == 1 => items[0].clone(),
                ValueView::Seq(items) if items.len() == 1 => items[0].clone(),
                ValueView::Slip(items) if items.len() == 1 => items[0].clone(),
                _ => index.clone(),
            };
            if self
                .assign_role_mixin_element(&current, &idx_arg, &value, &None)?
                .is_some()
            {
                return Ok(value);
            }
        }

        // Save Arc pointers before modifying (for shared container propagation)
        let old_array_arc = match current.view() {
            ValueView::Array(arc, ..) => Some(arc.clone()),
            _ => None,
        };
        let old_hash_arc = match current.view() {
            ValueView::Hash(arc) => Some(arc.clone()),
            _ => None,
        };

        // Check if index is multi-dimensional (array of indices like [2, 1] from [2;1])
        let dims: Vec<usize> = if let ValueView::Array(items, ..) = index.view() {
            items
                .iter()
                .map(|v| crate::runtime::to_int(v) as usize)
                .collect()
        } else {
            Vec::new()
        };

        // When assigning Nil to a container element with `is default(...)`,
        // restore the default value instead of Nil.
        let effective_value = if value.is_nil() {
            if let Some(def) = self.container_default(&current) {
                def
            } else {
                // Check class_attribute_default for instance attributes
                let class_default = if let ValueView::Instance { class_name, .. } = target.view() {
                    self.class_attribute_default_with_role_fallback(&class_name.resolve(), &method)
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
        if let ValueView::Instance { class_name, .. } = target.view() {
            let tc = self.get_attr_type_constraint(&class_name.resolve(), &method);
            let is_hash_attr = matches!(current.view(), ValueView::Hash(_));
            let is_array_attr = matches!(current.view(), ValueView::Array(..));
            // An object hash (`%.h{Str:D}`) checks elements against the value type.
            let elem_tc = tc.as_deref().map(|t| {
                crate::runtime::types::split_object_hash_constraint(t)
                    .0
                    .to_string()
            });
            if (is_hash_attr || is_array_attr)
                && let Some(ref type_constraint) = elem_tc
                && !matches!(type_constraint.as_str(), "Mu" | "Any")
                && !effective_value.is_nil()
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
                if matches!(effective_value.view(), ValueView::Hash(_)) {
                    return Err(crate::runtime::RuntimeError::new(format!(
                        "Type check failed in assignment to %{}; expected {} but got Hash",
                        method, type_constraint,
                    )));
                }
            }
        }
        // Also check via container type metadata (for non-attribute typed hashes/arrays)
        {
            let is_hash_attr = matches!(current.view(), ValueView::Hash(_));
            let is_array_attr = matches!(current.view(), ValueView::Array(..));
            if (is_hash_attr || is_array_attr)
                && let Some(info) = self.container_type_metadata(&current)
            {
                let constraint = &info.value_type.clone();
                // An empty value_type means "no element constraint": a Map
                // carries embedded metadata (declared_type) with no value
                // type, and `hashdata_type_info` renders that as "" — checking
                // against it would reject EVERY assignment (`has %.h =
                // Map.new(...)`; `$obj.h{k} = v` — DBDish::Pg's
                // dynamic-types).
                if !constraint.is_empty()
                    && constraint != "Mu"
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

        // ADR-0036: `$p.value<k> = v` / `$p.value[i] = v` on a Pair. A Pair
        // BINDS its value (rakudo's BUILD does `$!value := value`), so the pair
        // and the variable it was built from are the *same* container and the
        // element write belongs in it, in place. The clone-and-rebind below
        // would fork them: `overwrite_*_bindings_by_identity` moves the
        // variable onto the fresh `Gc` while the pair keeps the old one, so
        // only the first write is ever visible through both
        // (`t/pair-value-writethrough-coherence.t`).
        if matches!(method.as_str(), "value" | "key")
            && matches!(
                target.view(),
                ValueView::Pair(..) | ValueView::ValuePair(..)
            )
            && dims.len() < 2
        {
            match current.view() {
                ValueView::Hash(h) if h.key_type.is_none() => {
                    let key = index.to_string_value();
                    if let Some(entry) = current.hash_autovivify(&key) {
                        entry.hash_entry_write(effective_value.clone());
                        return Ok(effective_value);
                    }
                }
                ValueView::Array(items, _) => {
                    let idx = crate::runtime::to_int(&index) as usize;
                    if idx >= items.len() && !crate::runtime::utils::is_shaped_array(&current) {
                        current.array_grow_to(idx);
                    }
                    if current.array_set_in_place(idx, effective_value.clone()) {
                        return Ok(effective_value);
                    }
                }
                _ => {}
            }
        }

        // Modify the container
        let updated = if dims.len() >= 2 {
            // Multi-dimensional index assignment (e.g., $c.a[2;1] = value)
            Self::multidim_assign_nested(current, &dims, effective_value.clone())?
        } else {
            let is_object_hash =
                matches!(current.view(), ValueView::Hash(h) if h.key_type.is_some());
            let key = if !is_object_hash && matches!(index.view(), ValueView::Package(_)) {
                // A bare type object keyed into a plain (Str-keyed) hash
                // attribute coerces to "" with Rakudo's "uninitialized value
                // of type X in string context" warning, matching the lookup
                // path (DBDish::Pg keys converters by type object).
                self.coerce_type_object_hash_key(&index)?
            } else {
                index.to_string_value()
            };
            match current.view() {
                ValueView::Hash(h) => {
                    // Check for autovivification via nested subscript assignment:
                    // If the hash attribute has a type constraint and the key doesn't exist,
                    // Raku would normally autovivify a hash value, but for typed Int/etc,
                    // this should fail because {} is not an Int.
                    // (This is handled by type check above for the actual value being assigned.)
                    let mut new_hash = (**h).clone();
                    if new_hash.key_type.is_some() {
                        // An object hash (`has %.a{Str:D}`) stores `.WHICH`
                        // keys and records the key object.
                        let which = crate::runtime::utils::value_which_key(&index);
                        new_hash
                            .original_keys
                            .get_or_insert_with(std::collections::HashMap::new)
                            .insert(which.clone(), index.clone());
                        new_hash.map.insert(which, effective_value.clone());
                    } else {
                        new_hash.insert(key, effective_value.clone());
                    }
                    Value::hash(new_hash)
                }
                ValueView::Array(items, kind) => {
                    let idx = crate::runtime::to_int(&index) as usize;
                    let mut new_items = (**items).clone();
                    if idx >= new_items.len() {
                        if crate::runtime::utils::is_shaped_array(&current) {
                            return Err(RuntimeError::new("Index out of bounds"));
                        }
                        new_items.resize(
                            idx + 1,
                            Value::package(crate::symbol::Symbol::intern("Any")),
                        );
                    }
                    new_items[idx] = effective_value.clone();
                    Value::array_with_kind(crate::gc::Gc::new(new_items), kind)
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
            method_args,
            updated,
            true,
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
        let current = match current.view() {
            ValueView::ContainerRef(cell) => cell.lock().unwrap().clone(),
            _ => current,
        };
        // The default returned for an absent key: the container's own
        // `is default(...)`, else the attribute's declared default, else Nil.
        let absent_default = self.container_default(&current).or_else(|| {
            if let ValueView::Instance { class_name, .. } = target.view() {
                self.class_attribute_default_with_role_fallback(&class_name.resolve(), &method)
            } else {
                None
            }
        });

        // Save the pre-delete Arc identity so the modified container can be
        // propagated to every instance/binding sharing it (mirrors the assign path).
        let old_array_arc = match current.view() {
            ValueView::Array(arc, ..) => Some(arc.clone()),
            _ => None,
        };
        let old_hash_arc = match current.view() {
            ValueView::Hash(arc) => Some(arc.clone()),
            _ => None,
        };

        let (removed, updated) = match current.view() {
            ValueView::Hash(h) => {
                // An object hash (`has %.a{Str:D}`) stores `.WHICH` keys.
                let key = if h.key_type.is_some() {
                    crate::runtime::utils::value_which_key(&index)
                } else if matches!(index.view(), ValueView::Package(_)) {
                    self.coerce_type_object_hash_key(&index)?
                } else {
                    index.to_string_value()
                };
                let mut new_hash = (**h).clone();
                let removed = new_hash
                    .remove(&key)
                    .unwrap_or_else(|| absent_default.clone().unwrap_or(Value::NIL));
                (removed, Value::hash_with_data(crate::gc::Gc::new(new_hash)))
            }
            ValueView::Array(items, kind) => {
                let idx = crate::runtime::to_int(&index);
                let mut new_items = (**items).clone();
                let removed = if idx >= 0 && (idx as usize) < new_items.len() {
                    let i = idx as usize;
                    let r = new_items[i].clone();
                    // Trailing element shrinks; an interior delete leaves a hole.
                    if i + 1 == new_items.len() {
                        new_items.truncate(i);
                    } else {
                        new_items[i] = Value::NIL;
                    }
                    r
                } else {
                    absent_default.clone().unwrap_or(Value::NIL)
                };
                (
                    removed,
                    Value::array_with_kind(crate::gc::Gc::new(new_items), kind),
                )
            }
            _ => return Ok(absent_default.unwrap_or(Value::NIL)),
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
            true,
        )?;
        Ok(removed)
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
        match container.view() {
            ValueView::Array(items, kind) => {
                let idx = dims[0];
                let mut new_items = (**items).clone();
                if idx >= new_items.len() {
                    new_items.resize(
                        idx + 1,
                        Value::package(crate::symbol::Symbol::intern("Any")),
                    );
                }
                if dims.len() == 1 {
                    new_items[idx] = value;
                } else {
                    let inner = new_items[idx].clone();
                    new_items[idx] = Self::multidim_assign_nested(inner, &dims[1..], value)?;
                }
                let result = Value::array_with_kind(crate::gc::Gc::new(new_items), kind);
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
                        vec![Value::package(crate::symbol::Symbol::intern("Any")); idx + 1];
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
        let method_args = match args[2].view() {
            ValueView::Array(items, ..) => items.to_vec(),
            ValueView::Nil => Vec::new(),
            _ => vec![args[2].clone()],
        };
        // ADR-0040's store boundary, Proxy half: the whole call is exempt from
        // the caller's argument auto-FETCH (`skip_proxy_fetch`) because the
        // TARGET must keep its container, but the assigned VALUE is an ordinary
        // rvalue — `$obj.attr = $p` stores what `$p` FETCHes.
        let value = self.fetch_proxy_for_store(args[3].clone())?;
        let target_var = args.get(4).and_then(|v| {
            let name = v.to_string_value();
            if name.is_empty() { None } else { Some(name) }
        });
        let preserve_hash_entries =
            matches!(args.get(5).map(Value::view), Some(ValueView::Bool(true)));
        self.assign_method_lvalue_with_values(
            target_var.as_deref(),
            target,
            &method,
            method_args,
            value,
            preserve_hash_entries,
        )
    }
}
