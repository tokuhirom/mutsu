use super::*;

impl Interpreter {
    /// Resolve WhateverCode indices for array deletion.
    /// Converts `*-N` style closures to concrete integer indices.
    fn resolve_delete_index_for_array(&mut self, idx: Value, container: &Value) -> Value {
        let arr_len = match container.view() {
            ValueView::Array(items, ..) => items.len(),
            _ => return idx,
        };
        match idx.view() {
            ValueView::Sub(data) => {
                let len = arr_len as i64;
                let mut sub_env = data.env.clone();
                for p in &data.params {
                    sub_env.insert(p.to_string(), Value::int(len));
                }
                let saved_env = std::mem::take(self.env_mut());
                *self.env_mut() = sub_env;
                let resolved = loan_env!(self, eval_block_value(&data.body)).unwrap_or(Value::NIL);
                *self.env_mut() = saved_env;
                resolved
            }
            ValueView::Array(items, ..) => {
                // Array of indices: resolve each element
                let resolved: Vec<Value> = items
                    .iter()
                    .map(|v| self.resolve_delete_index_for_array(v.clone(), container))
                    .collect();
                Value::array(resolved)
            }
            _ => idx,
        }
    }

    /// Trim trailing "holes" from a named array variable after deletion.
    /// A hole is either `Nil` (deleted slot) or an uninitialized
    /// `Package("Any")` slot (auto-vivified gap).  Explicitly
    /// assigned slots are tracked via `__mutsu_initialized_index::` metadata
    /// and are NOT trimmed.
    fn trim_trailing_array_holes(&mut self, var_name: &str) {
        // Get the type constraint for typed arrays (e.g. "Int" for `my Int @a`)
        let type_constraint = loan_env!(self, var_type_constraint(var_name)).unwrap_or_default();
        let env = self.env_mut();
        let Some(container) = env.get_mut(var_name) else {
            return;
        };
        container.with_array_mut(|items, _| {
            // Container identity (§3): trim through the shared backing node.
            let arr = crate::value::gc_data_mut(items);
            // The explicitly-assigned indices travel with the array (embedded set).
            let initialized = arr.initialized.clone().unwrap_or_default();
            while let Some(last) = arr.last() {
                let idx = arr.len() - 1;
                let is_hole = match last.view() {
                    ValueView::Nil => true,
                    ValueView::Package(name) if name == "Any" => !initialized.contains(&idx),
                    // For typed arrays (e.g. `my Int @a`), the type object is also a hole
                    ValueView::Package(name)
                        if !type_constraint.is_empty() && name == type_constraint.as_str() =>
                    {
                        !initialized.contains(&idx)
                    }
                    _ => false,
                };
                if is_hole {
                    arr.pop();
                    if let Some(s) = arr.initialized.as_mut() {
                        s.remove(&idx);
                    }
                } else {
                    break;
                }
            }
        });
    }

    /// Fast path for simple `%h{$key}:delete` — skip metadata lookups.
    #[inline]
    fn try_fast_hash_delete(
        &mut self,
        code: &CompiledCode,
        var_name: &str,
        slot: Option<u32>,
        idx: &Value,
    ) -> Option<Result<Value, RuntimeError>> {
        if !var_name.starts_with('%') || var_name == "%*ENV" {
            return None;
        }
        if !self.local_bind_pairs.is_empty() {
            return None;
        }
        // Reject complex index types
        if matches!(
            idx.view(),
            ValueView::Array(..) | ValueView::Whatever | ValueView::GenericRange { .. }
        ) {
            return None;
        }
        if self.var_type_constraint_fast(var_name).is_some()
            || self.readonly_vars().contains(var_name)
        {
            return None;
        }
        let env = self.env();
        match env.get(var_name).map(Value::view) {
            Some(ValueView::Hash(hash_arc)) => {
                let strong_count = crate::gc::Gc::strong_count_of(hash_arc);
                if strong_count > 2 {
                    return None;
                }
                let local_slot = if strong_count == 2 {
                    match self.resolve_local_slot(code, slot, var_name) {
                        Some(slot) => Some(slot),
                        None => return None,
                    }
                } else {
                    None
                };
                if hash_arc.has_type_meta() {
                    return None;
                }
                // A `:delete` of an absent key yields the hash's `is default(...)`
                // value (Raku semantics), not Nil. Capture it before the env
                // borrow is released for the mutating remove below.
                let container_default = hash_arc.default.as_deref().cloned();
                let key = idx.to_string_value();
                if let Some(slot) = local_slot {
                    self.locals[slot] = Value::NIL;
                }
                let removed = self
                    .env_mut()
                    .get_mut(var_name)
                    .and_then(|v| {
                        // Container identity (§3): remove through the shared
                        // backing node.
                        v.with_hash_mut(|hash| crate::value::gc_data_mut(hash).remove(&key))
                    })
                    .flatten();
                let removed = removed.or(container_default).unwrap_or(Value::NIL);
                if let Some(slot) = local_slot
                    && let Some(env_val) = self.env().get(var_name).cloned()
                {
                    self.locals[slot] = env_val;
                }
                Some(Ok(removed))
            }
            _ => None,
        }
    }

    pub(super) fn exec_delete_index_named_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: Option<u32>,
    ) -> Result<(), RuntimeError> {
        // A whole-container `:=` bound variable (`my %g := %h`) holds a shared
        // `ContainerRef` cell. The delete logic operates on plain Hash/Array
        // values read from env, so temporarily unwrap the cell's inner container
        // into env for the duration of the op, then write the mutated result
        // back through the cell (so every alias observes the delete) and restore
        // the cell in env and the local slot.
        let var_name = Self::const_str(code, name_idx).to_string();
        // A lazy `@`-array reifies its prefix before an element delete
        // (`@a[i]:delete`) — delete needs a materialized backing. (L2)
        self.reify_lazy_array_slot(&var_name)?;
        let bound_cell = match self.env().get(&var_name).map(Value::view) {
            Some(ValueView::ContainerRef(cell)) => Some(cell.clone()),
            _ => None,
        };
        if let Some(ref cell) = bound_cell {
            let inner = cell.lock().unwrap().clone();
            self.env_mut().insert(var_name.clone(), inner);
        }
        let result = self.exec_delete_index_named_op_inner(code, name_idx, slot);
        if let Some(cell) = bound_cell {
            if let Some(mutated) = self.env().get(&var_name).cloned() {
                *cell.lock().unwrap() = mutated;
            }
            let cell_val = Value::container_ref(cell);
            self.env_mut().insert(var_name.clone(), cell_val.clone());
            self.write_local_slot_or_name(code, slot, &var_name, cell_val);
        }
        result
    }

    fn exec_delete_index_named_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        slot: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let idx = self.stack.pop().unwrap_or(Value::NIL);
        // An *itemized* list subscript (`@a[$(7,8,9)]:delete`) is a SINGLE index
        // (its `.Int`, the element count), not a slice.
        let idx = match idx.view() {
            ValueView::Array(items, crate::value::ArrayKind::ItemList) => {
                Value::int(items.len() as i64)
            }
            ValueView::Scalar(inner)
                if inner.is_range() || matches!(inner.view(), ValueView::Array(..)) =>
            {
                Value::int(crate::runtime::utils::value_to_list(inner).len() as i64)
            }
            _ => idx,
        };
        // Fast path for simple hash delete
        if let Some(result) = self.try_fast_hash_delete(code, &var_name, slot, &idx) {
            self.stack.push(result?);
            return Ok(());
        }
        let declared_type_del = self
            .env()
            .get(&var_name)
            .cloned()
            .and_then(|v| self.container_type_metadata(&v))
            .and_then(|info| info.declared_type);
        let _target_is_mixhash = declared_type_del.as_deref().is_some_and(|t| t == "MixHash");
        let _target_is_baghash = declared_type_del.as_deref().is_some_and(|t| t == "BagHash");
        let _target_is_sethash = declared_type_del.as_deref().is_some_and(|t| t == "SetHash");
        // Deleting from a native typed array (e.g. `array[int]`) is illegal:
        // native arrays have no "holes" to create. Throw X::Delete.
        if let Some(dt) = declared_type_del.as_deref()
            && let Some(elem) = dt.strip_prefix("array[").and_then(|s| s.strip_suffix(']'))
            && crate::runtime::native_types::is_native_array_element_type(elem)
        {
            return Err(RuntimeError::typed(
                "X::Delete",
                [(
                    "message".to_string(),
                    Value::str(format!(
                        "Cannot delete from a native {} array",
                        crate::runtime::native_types::native_family_name(elem)
                    )),
                )]
                .into_iter()
                .collect(),
            ));
        }
        // Note: Bag/Set immutability checks for :delete are intentionally
        // omitted here because Bag/BagHash and Set/SetHash share the same
        // Value variants and the declared_type metadata is not always
        // available (e.g., in set operator internals). The Mix check below
        // is kept because Mix operations are less commonly used internally.
        // Sync OS environment and $*HOME when deleting from %*ENV
        if var_name == "%*ENV" {
            // Remove from OS environment
            #[cfg(not(target_family = "wasm"))]
            match idx.view() {
                ValueView::Array(keys, ..) => {
                    for k in keys.iter() {
                        let key_str = k.to_string_value();
                        // SAFETY: std::env::remove_var is unsafe because mutating
                        // the process environment races with concurrent env
                        // access on another thread. mutsu deletes %*ENV keys from
                        // the executing thread; a spawned worker reading env
                        // concurrently would be a latent race (tracked with the
                        // cross-thread container work, see aliased_mut.rs).
                        unsafe {
                            std::env::remove_var(&key_str);
                        }
                    }
                }
                _ => {
                    let key_str = idx.to_string_value();
                    // SAFETY: std::env::remove_var is unsafe because mutating the
                    // process environment races with concurrent env access on
                    // another thread. mutsu deletes %*ENV keys from the executing
                    // thread; a spawned worker reading env concurrently would be a
                    // latent race (tracked with the cross-thread container work,
                    // see aliased_mut.rs).
                    unsafe {
                        std::env::remove_var(&key_str);
                    }
                }
            }
            let deletes_home = match idx.view() {
                ValueView::Array(keys, ..) => keys.iter().any(|k| k.to_string_value() == "HOME"),
                _ => idx.to_string_value() == "HOME",
            };
            if deletes_home {
                self.env_mut().insert("$*HOME".to_string(), Value::NIL);
                self.env_mut().insert("*HOME".to_string(), Value::NIL);
            }
        }
        // Save type metadata before delete (Arc::make_mut may change pointer)
        let saved_meta = self
            .env()
            .get(&var_name)
            .cloned()
            .and_then(|v| self.container_type_metadata(&v));
        // Save container default (pointer-keyed) before delete so we can
        // re-apply it after `Arc::make_mut` changes the pointer. Only
        // trust this when a name-based `var_default` is also registered
        // for this variable: Arc pointers can be reused across
        // allocations, so a stale pointer-keyed entry from a freed
        // same-named container must not leak into the new container.
        // Prefer the value-carried default (HashData/ArrayData) so `:delete` of
        // an absent key yields the default even when the container arrived via a
        // parameter (whose name is not in the name-keyed `var_defaults` table).
        let saved_default = self
            .env()
            .get(&var_name)
            .and_then(|v| self.container_default(v).cloned())
            .or_else(|| self.var_default(&var_name).cloned());
        // Resolve WhateverCode indices (e.g. *-1) for array targets
        let idx = if let Some(container) = self.env().get(&var_name).cloned() {
            self.resolve_delete_index_for_array(idx, &container)
        } else {
            idx
        };
        // A nested single-dim slice delete (`@a[(3, (30, (5,)))]:delete`) returns
        // the deleted values in the index tree's shape and removes every leaf slot.
        if let Some(inner) = Self::nested_index_elements(&idx)
            && inner
                .iter()
                .any(|e| Self::nested_index_elements(e).is_some())
            && matches!(
                self.env().get(&var_name).map(Value::view),
                Some(ValueView::Array(..))
            )
        {
            return self.exec_nested_slice_delete(code, &var_name, slot, &inner);
        }
        // For typed arrays (e.g. `my Int @a`), deleted elements become
        // the type object (e.g. `Int`) instead of `Any`.
        let hole_type =
            loan_env!(self, var_type_constraint(&var_name)).unwrap_or_else(|| "Any".to_string());
        // For object hashes, convert index to WHICH-based key format
        let is_obj_hash_del = loan_env!(self, var_hash_key_constraint(&var_name)).is_some();
        let idx = if is_obj_hash_del
            && !matches!(
                idx.view(),
                ValueView::Whatever | ValueView::Nil | ValueView::Array(..)
            ) {
            // Convert index value to a Str containing the WHICH key
            let which = crate::runtime::utils::value_which_key(&idx);
            // Check if the hash uses WHICH keys or encoded keys
            if let Some(ValueView::Hash(map)) = self.env().get(&var_name).map(Value::view) {
                if map.contains_key(&which) {
                    Value::str(which)
                } else {
                    let encoded = Value::hash_key_encode(&idx);
                    Value::str(encoded)
                }
            } else {
                Value::str(which)
            }
        } else if is_obj_hash_del && let ValueView::Array(keys, ..) = idx.view() {
            // Convert array of keys to WHICH format
            let converted: Vec<Value> = keys
                .iter()
                .map(|k| {
                    let which = crate::runtime::utils::value_which_key(k);
                    if let Some(ValueView::Hash(map)) = self.env().get(&var_name).map(Value::view)
                        && map.contains_key(&which)
                    {
                        Value::str(which)
                    } else {
                        let encoded = Value::hash_key_encode(k);
                        Value::str(encoded)
                    }
                })
                .collect();
            Value::array(converted)
        } else {
            idx
        };
        // Save idx for unmark step (idx is consumed by delete_from_container)
        let idx_for_unmark = idx.clone();
        let result = if let Some(container) = self.env_mut().get_mut(&var_name) {
            // Check immutability for Set/Bag/Mix (immutable variants)
            match container.view() {
                ValueView::Mix(_, is_mutable) if !is_mutable => {
                    return Err(RuntimeError::immutable("Mix", "delete"));
                }
                ValueView::Set(_, is_mutable) if !is_mutable => {
                    return Err(RuntimeError::immutable("Set", "delete"));
                }
                ValueView::Bag(_, is_mutable) if !is_mutable => {
                    return Err(RuntimeError::immutable("Bag", "delete"));
                }
                _ => {}
            }
            Self::delete_from_container(container, idx, &hole_type)?
        } else {
            Self::delete_from_missing_container(idx)
        };
        // Remove deleted indices from the initialized-index tracking set
        // so that trim_trailing_array_holes recognizes them as holes.
        self.unmark_initialized_indices(&var_name, &idx_for_unmark);
        // Mark deleted positions so :exists can report them as missing even
        // though the slot still holds a type-object hole value.
        self.mark_deleted_indices(&var_name, &idx_for_unmark);
        // Remove deleted indices from the bound-index tracking set to sever bindings.
        self.unmark_bound_indices(&var_name, &idx_for_unmark);
        // Trim trailing holes from arrays after deletion.
        // A "hole" is either Nil (deleted) or an uninitialized Package("Any") slot.
        self.trim_trailing_array_holes(&var_name);
        // If the deleted value is a hole (Nil or type object like Package("Any")),
        // substitute the container's default value if one was set via `is default(...)`.
        // For a SLICE delete (`%h<a b c>:delete`) the result is a list whose
        // absent-key entries are holes — replace each of them with the default,
        // so `my %h is default(42); %h<a b c>:delete` yields `(1, 2, 42)`.
        let is_hole = |v: &Value| matches!(v.view(), ValueView::Nil | ValueView::Package(_));
        let result = if let Some(def) = &saved_default {
            match result.view() {
                _ if is_hole(&result) => def.clone(),
                ValueView::Array(items, kind) => {
                    let replaced: Vec<Value> = items
                        .iter()
                        .map(|v| if is_hole(v) { def.clone() } else { v.clone() })
                        .collect();
                    Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(replaced)),
                        kind,
                    )
                }
                ValueView::Seq(items) | ValueView::Slip(items) => {
                    let replaced: Vec<Value> = items
                        .iter()
                        .map(|v| if is_hole(v) { def.clone() } else { v.clone() })
                        .collect();
                    Value::seq(replaced)
                }
                _ => result,
            }
        } else {
            result
        };
        // Re-register type metadata if it was lost due to Arc::make_mut. Hashes
        // embed metadata in `HashData`, so the re-tagged value is written back
        // (no-op Arc for array/instance side-table containers).
        if let Some(info) = saved_meta
            && let Some(container) = self.env().get(&var_name).cloned()
            && self.container_type_metadata(&container).is_none()
        {
            let container = container.clone();
            let tagged = self.tag_container_metadata(container, info);
            self.env_mut().insert(var_name.to_string(), tagged.clone());
            self.write_local_slot_or_name(code, slot, &var_name, tagged);
        }
        // Re-register container default if it was lost due to Arc::make_mut.
        // Use the pointer-keyed container_default saved before delete so we
        // don't inherit a leaked default from a same-named variable in an
        // outer scope.
        if let Some(def) = saved_default
            && let Some(container) = self.env().get(&var_name).cloned()
            && self.container_default(&container).is_none()
        {
            let tagged = self.tag_container_default(container, def);
            self.env_mut().insert(var_name.clone(), tagged);
        }
        // Sync env value to locals so reads through locals see the
        // updated container after delete (Arc::make_mut may have changed
        // the container pointer).
        if let Some(container) = self.env().get(&var_name).cloned() {
            self.write_local_slot_or_name(code, slot, &var_name, container);
        }
        self.stack.push(result);
        Ok(())
    }

    /// Plain `:delete` on a nested single-dim slice (`@a[(3, (30, (5,)))]:delete`):
    /// return the deleted values in the index tree's shape (missing slots read as
    /// the array's hole type) and remove every addressed leaf slot.
    fn exec_nested_slice_delete(
        &mut self,
        code: &CompiledCode,
        var_name: &str,
        slot: Option<u32>,
        inner: &[Value],
    ) -> Result<(), RuntimeError> {
        let hole_type =
            loan_env!(self, var_type_constraint(var_name)).unwrap_or_else(|| "Any".to_string());
        // Snapshot the pre-delete contents for the returned (value) tree.
        let items_snap: Vec<Value> = match self.env().get(var_name).map(Value::view) {
            Some(ValueView::Array(items, ..)) => items.to_vec(),
            _ => Vec::new(),
        };
        let missing = Value::package(crate::symbol::Symbol::intern(&hole_type));
        // Plain `:delete` returns the values, keeping missing slots (as the hole).
        let result = Value::array(Self::format_positional_slice_level(
            &items_snap,
            None,
            &missing,
            inner,
            "v",
            true,
        ));
        // Remove every leaf index through the shared flat-delete machinery.
        let mut leaves = Vec::new();
        Self::collect_slice_leaf_indices(inner, &mut leaves);
        let flat_idx = Value::array(leaves.iter().map(|i| Value::int(*i)).collect::<Vec<_>>());
        if let Some(container) = self.env_mut().get_mut(var_name) {
            let _ = Self::delete_from_container(container, flat_idx.clone(), &hole_type)?;
        }
        self.unmark_initialized_indices(var_name, &flat_idx);
        self.mark_deleted_indices(var_name, &flat_idx);
        self.unmark_bound_indices(var_name, &flat_idx);
        self.trim_trailing_array_holes(var_name);
        if let Some(container) = self.env().get(var_name).cloned() {
            self.write_local_slot_or_name(code, slot, var_name, container);
        }
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_delete_index_expr_op(&mut self) -> Result<(), RuntimeError> {
        let idx = self.stack.pop().unwrap_or(Value::NIL);
        let mut target = self.stack.pop().unwrap_or(Value::NIL);
        // Note: We cannot distinguish Bag from BagHash or Set from SetHash
        // in the expression form (no variable metadata), so immutability
        // checks for Bag/Set are only in the named op path.
        let result = Self::delete_from_container(&mut target, idx, "Any")?;
        self.stack.push(result);
        Ok(())
    }

    /// Delete element(s) from an array container.
    /// When idx is a single value, delete that one element.
    /// When idx is an Array, delete each element (slice delete).
    /// When idx is a Range, expand to indices and delete each.
    /// Delete element(s) from an array container.
    /// When idx is a single value, delete that one element.
    /// When idx is an Array, delete each element (slice delete).
    /// When idx is a Range, expand to indices and delete each.
    fn delete_from_array(
        container: &mut Value,
        idx: Value,
        hole_type: &str,
    ) -> Result<Value, RuntimeError> {
        match idx.view() {
            ValueView::Whatever => {
                // `@a[*]:delete` — delete all elements, returning the
                // previous contents of the array.
                let len = match container.view() {
                    ValueView::Array(items, ..) => items.len(),
                    _ => 0,
                };
                let indices: Vec<Value> = (0..len as i64).map(Value::int).collect();
                let mut results = Vec::with_capacity(indices.len());
                for i in indices {
                    let r =
                        Self::delete_array_multidim(container, std::slice::from_ref(&i), hole_type)
                            .unwrap_or(Value::NIL);
                    results.push(r);
                }
                Ok(Value::array(results))
            }
            ValueView::Array(indices, ..) => {
                let indices_vec: Vec<Value> = indices.to_vec();
                let mut results = Vec::with_capacity(indices_vec.len());
                for i in indices_vec {
                    let r =
                        Self::delete_array_multidim(container, std::slice::from_ref(&i), hole_type)
                            .unwrap_or(Value::NIL);
                    results.push(r);
                }
                Ok(Value::array(results))
            }
            ValueView::Range(..)
            | ValueView::RangeExcl(..)
            | ValueView::RangeExclStart(..)
            | ValueView::RangeExclBoth(..)
            | ValueView::GenericRange { .. } => {
                let expanded = crate::runtime::utils::value_to_list(&idx);
                let mut results = Vec::with_capacity(expanded.len());
                for i in expanded {
                    let r =
                        Self::delete_array_multidim(container, std::slice::from_ref(&i), hole_type)
                            .unwrap_or(Value::NIL);
                    results.push(r);
                }
                Ok(Value::array(results))
            }
            _ => {
                let r =
                    Self::delete_array_multidim(container, std::slice::from_ref(&idx), hole_type)?;
                Ok(r)
            }
        }
    }

    fn delete_from_missing_container(idx: Value) -> Value {
        match idx.view() {
            ValueView::Array(keys, ..) => Value::array(vec![Value::NIL; keys.len()]),
            _ => Value::NIL,
        }
    }

    fn delete_from_container(
        container: &mut Value,
        idx: Value,
        hole_type: &str,
    ) -> Result<Value, RuntimeError> {
        // Container identity (§3): delete through the shared backing node so
        // every by-value holder of the same container observes the removal.
        if let Some(removed) = container.with_hash_mut(|hash| match idx.view() {
            ValueView::Whatever => {
                let h = crate::value::gc_data_mut(hash);
                let removed: Vec<Value> = h.values().cloned().collect();
                h.clear();
                Value::array(removed)
            }
            ValueView::Num(f) if f.is_infinite() && f.is_sign_positive() => {
                let h = crate::value::gc_data_mut(hash);
                let removed: Vec<Value> = h.values().cloned().collect();
                h.clear();
                Value::array(removed)
            }
            ValueView::Array(keys, ..) => {
                let h = crate::value::gc_data_mut(hash);
                let removed = keys
                    .iter()
                    .map(|key| h.remove(&key.to_string_value()).unwrap_or(Value::NIL))
                    .collect();
                Value::array(removed)
            }
            _ => crate::value::gc_data_mut(hash)
                .remove(&idx.to_string_value())
                .unwrap_or(Value::NIL),
        }) {
            return Ok(removed);
        }
        if let ValueView::Package(type_name) = container.view()
            && (type_name == "Hash" || type_name == "Hash:U")
        {
            return Ok(match idx.view() {
                ValueView::Array(keys, ..) => Value::array(vec![Value::NIL; keys.len()]),
                _ => Value::NIL,
            });
        }
        if matches!(container.view(), ValueView::Array(..)) {
            return Self::delete_from_array(container, idx, hole_type);
        }
        if let Some(removed) = container.with_set_mut(|set, _| match idx.view() {
            ValueView::Array(keys, ..) => {
                let s = crate::value::gc_data_mut(set);
                let removed = keys
                    .iter()
                    .map(|key| Value::truth(s.remove(&key.to_string_value())))
                    .collect();
                Value::array(removed)
            }
            _ => Value::truth(crate::value::gc_data_mut(set).remove(&idx.to_string_value())),
        }) {
            return Ok(removed);
        }
        if let Some(removed) = container.with_bag_mut(|bag, _| match idx.view() {
            ValueView::Array(keys, ..) => {
                let b = crate::value::gc_data_mut(bag);
                let removed = keys
                    .iter()
                    .map(|key| {
                        Value::from_bigint(b.remove(&key.to_string_value()).unwrap_or_default())
                    })
                    .collect();
                Value::array(removed)
            }
            _ => Value::from_bigint(
                crate::value::gc_data_mut(bag)
                    .remove(&idx.to_string_value())
                    .unwrap_or_default(),
            ),
        }) {
            return Ok(removed);
        }
        if let Some(removed) = container.with_mix_mut(|mix, _| match idx.view() {
            ValueView::Array(keys, ..) => {
                let m = crate::value::gc_data_mut(mix);
                let removed = keys
                    .iter()
                    .map(|key| Value::num(m.remove(&key.to_string_value()).unwrap_or(0.0)))
                    .collect();
                Value::array(removed)
            }
            _ => Value::num(
                crate::value::gc_data_mut(mix)
                    .remove(&idx.to_string_value())
                    .unwrap_or(0.0),
            ),
        }) {
            return Ok(removed);
        }
        Ok(match idx.view() {
            ValueView::Array(keys, ..) => Value::array(vec![Value::NIL; keys.len()]),
            _ => Value::NIL,
        })
    }

    pub(super) fn mix_weight_as_value(weight: f64) -> Value {
        crate::value::mix_weight_to_value(weight)
    }
}
