use super::*;
use crate::symbol::Symbol;

impl VM {
    /// Write a mutating hyper's result back to its *named* `@`/`%` target
    /// variable precisely. If the variable is bound (holds a shared
    /// `ContainerRef` cell, e.g. `$b := @a`), write through the cell so all
    /// aliases observe the change; otherwise replace the binding (COW-detach) so
    /// a copy (`my @x = @a`) keeps its own backing and is not corrupted. This is
    /// the lvalue-precise alternative to the Arc-identity binding scan, which
    /// cannot tell a bound alias from a COW copy.
    fn write_back_hyper_target_var(&mut self, code: &CompiledCode, var: &str, new_val: Value) {
        if let Some(Value::ContainerRef(cell)) = self.interpreter.env().get(var) {
            *cell.lock().unwrap() = new_val;
            return;
        }
        if let Some(slot) = self.find_local_slot(code, var)
            && let Value::ContainerRef(cell) = &self.locals[slot]
        {
            *cell.lock().unwrap() = new_val;
            return;
        }
        self.set_env_with_main_alias(var, new_val.clone());
        self.locals_set_by_name(code, var, new_val);
        self.env_dirty = true;
    }

    pub(super) fn exec_hyper_method_call_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        target_name_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let method_raw = Self::const_str(code, name_idx);
        let target_var: Option<String> =
            target_name_idx.map(|idx| Self::const_str(code, idx).to_string());
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx));
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new("VM stack underflow in HyperMethodCall"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, false);
        }
        let target = self
            .stack
            .pop()
            .ok_or_else(|| RuntimeError::new("VM stack underflow in HyperMethodCall target"))?;
        // Mark Seq as consumed (single-use semantics).
        if let Value::Seq(ref arc) = target {
            crate::value::seq_consume(arc)?;
        }
        // Hyper method/postfix on a Hash applies to each *value*, preserving the
        // keys: `%h>>.uc` and `%h>>!` yield a Hash, not a list of pairs.
        let hash_keys: Option<Vec<String>> = if let Value::Hash(map) = &target {
            Some(map.keys().cloned().collect())
        } else {
            None
        };
        // For Buf/Blob instances, expand to individual byte values for hyper dispatch
        let mut items = if let Some(keys) = &hash_keys {
            if let Value::Hash(map) = &target {
                keys.iter()
                    .map(|k| map.get(k).cloned().unwrap_or(Value::Nil))
                    .collect()
            } else {
                Vec::new()
            }
        } else if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        {
            if let Some(Value::Array(bytes, ..)) = attributes.as_map().get("bytes") {
                bytes.to_vec()
            } else {
                Vec::new()
            }
        } else {
            crate::runtime::value_to_list(&target)
        };
        let mut results = Vec::with_capacity(items.len());
        // A "nodal" hyper method (one natively defined on the list type, e.g.
        // .reverse/.sort/.elems) operates on each node rather than recursing to
        // leaves, and its hyper result is a List rather than an Array.
        let mut method_is_nodal = false;
        // A per-element native method may raise a *resumable* warn (e.g.
        // `.indent(-N)` when asked to outdent more than exists). We cannot
        // suspend the Rust loop to let an enclosing `CONTROL { .resume }` resume
        // mid-iteration, so for the plain Array/List result path we collect the
        // first such warn, use each element's carried resume value, and re-raise
        // once after the loop with the *whole* result as the resume value. That
        // lets the outer CONTROL (or the default warn handler) resume the entire
        // hyper expression instead of aborting it. Container targets
        // (Hash/Set/Bag/Mix) take their own early-return paths, so we leave their
        // warns propagating as before.
        let collect_warns = hash_keys.is_none()
            && !matches!(&target, Value::Mix(..) | Value::Bag(..) | Value::Set(..));
        let mut pending_warn: Option<RuntimeError> = None;
        for (idx, item) in items.iter_mut().enumerate() {
            let method = Self::rewrite_method_name(method_raw, modifier);
            // Special case: CALL-ME on callable items (from >>.(args) syntax).
            // Instead of method dispatch, invoke the item directly as a callable.
            if method == "CALL-ME"
                && matches!(
                    item,
                    Value::Sub(..) | Value::WeakSub(..) | Value::Routine { .. } | Value::Mixin(..)
                )
            {
                let val = self.vm_call_on_value(item.clone(), args.clone(), None)?;
                results.push(val);
                continue;
            }
            // Special case: user-defined postfix:<...> operators applied via hyper (>>op / >>op).
            // These are function calls, not method calls.
            // Exclude built-in postfix operators (++, --) which are handled by method dispatch.
            if method.starts_with("postfix:<")
                && !matches!(method.as_str(), "postfix:<++>" | "postfix:<-->")
            {
                let mut call_args = vec![item.clone()];
                call_args.extend(args.clone());
                let empty_fns = HashMap::new();
                let val = self.call_function_compiled_first(&method, call_args, &empty_fns)?;
                results.push(val);
                continue;
            }
            let mut skip_native = method == "VAR"
                || (quoted
                    && matches!(
                        method.as_str(),
                        "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
                    ));
            if !skip_native
                && !matches!(
                    method.as_str(),
                    "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
                )
            {
                let class_name = match item {
                    Value::Instance { class_name, .. } => Some(class_name.resolve()),
                    Value::Package(name) => Some(name.resolve()),
                    _ => None,
                };
                if let Some(cn) = class_name
                    && self.interpreter.has_user_method(&cn, &method)
                {
                    skip_native = true;
                }
            }
            let item_args = args.clone();
            match modifier {
                Some("?") => {
                    let val = if !skip_native {
                        if let Some(native_result) =
                            self.try_native_method(item, Symbol::intern(&method), &item_args)
                        {
                            native_result.unwrap_or(Value::Package(Symbol::intern("Any")))
                        } else {
                            match self
                                .call_method_mut_with_temp_target(item, &method, item_args, idx)
                            {
                                Ok((v, updated)) => {
                                    *item = updated;
                                    v
                                }
                                Err(_) => Value::Package(Symbol::intern("Any")),
                            }
                        }
                    } else {
                        match self.call_method_mut_with_temp_target(item, &method, item_args, idx) {
                            Ok((v, updated)) => {
                                *item = updated;
                                v
                            }
                            Err(_) => Value::Package(Symbol::intern("Any")),
                        }
                    };
                    results.push(val);
                }
                Some("+") => {
                    let vals = if !skip_native {
                        if let Some(native_result) =
                            self.try_native_method(item, Symbol::intern(&method), &item_args)
                        {
                            vec![native_result?]
                        } else {
                            let (v, updated) = self
                                .call_method_all_with_temp_target(item, &method, item_args, idx)?;
                            *item = updated;
                            v
                        }
                    } else {
                        let (v, updated) =
                            self.call_method_all_with_temp_target(item, &method, item_args, idx)?;
                        *item = updated;
                        v
                    };
                    results.push(Value::real_array(vals));
                }
                Some("*") => {
                    if !skip_native
                        && let Some(native_result) =
                            self.try_native_method(item, Symbol::intern(&method), &item_args)
                    {
                        match native_result {
                            Ok(v) => results.push(Value::real_array(vec![v])),
                            Err(_) => results.push(Value::real_array(vec![])),
                        }
                    } else {
                        match self.call_method_all_with_temp_target(item, &method, item_args, idx) {
                            Ok((vals, updated)) => {
                                *item = updated;
                                results.push(Value::real_array(vals));
                            }
                            Err(_) => results.push(Value::real_array(vec![])),
                        }
                    }
                }
                _ => {
                    // Hyper method dispatch on nested list/array/seq items.
                    // Raku's >> descends into Iterable structures, but stops
                    // if the method is natively defined on the list type
                    // (e.g., .join, .elems, .sort, .reverse, .unique, .squish).
                    let is_iterable_item =
                        matches!(item, Value::Array(..) | Value::Seq(..) | Value::Slip(..));
                    let is_list_native_method = matches!(
                        method.as_str(),
                        "join"
                            | "elems"
                            | "end"
                            | "sort"
                            | "reverse"
                            | "unique"
                            | "squish"
                            | "pick"
                            | "roll"
                            | "head"
                            | "tail"
                            | "first"
                            | "min"
                            | "max"
                            | "minmax"
                            | "sum"
                            | "flat"
                            | "eager"
                            | "lazy"
                            | "sink"
                            | "cache"
                            | "List"
                            | "Array"
                            | "Seq"
                            | "Slip"
                            | "Supply"
                            | "Set"
                            | "SetHash"
                            | "Bag"
                            | "BagHash"
                            | "Mix"
                            | "MixHash"
                            | "Str"
                            | "gist"
                            | "raku"
                            | "perl"
                            | "WHAT"
                            | "WHO"
                            | "HOW"
                            | "so"
                            | "Bool"
                            | "Numeric"
                            | "Int"
                            | "Rat"
                            | "Real"
                            | "hash"
                            | "Hash"
                            | "kv"
                            | "keys"
                            | "values"
                            | "pairs"
                            | "antipairs"
                            | "classify"
                            | "categorize"
                            | "map"
                            | "grep"
                            | "reduce"
                            | "produce"
                            | "combinations"
                            | "permutations"
                            | "rotate"
                            | "batch"
                            | "rotor"
                            | "repeated"
                            | "snip"
                            | "defined"
                            | "DEFINITE"
                            | "item"
                            | "list"
                            | "AT-POS"
                            | "AT-KEY"
                            | "EXISTS-POS"
                            | "EXISTS-KEY"
                            | "DELETE-POS"
                            | "DELETE-KEY"
                            | "ASSIGN-POS"
                            | "ASSIGN-KEY"
                            | "BIND-POS"
                            | "BIND-KEY"
                            | "push"
                            | "pop"
                            | "shift"
                            | "unshift"
                            | "append"
                            | "prepend"
                            | "splice"
                            | "all"
                            | "any"
                            | "one"
                            | "none"
                            | "duckmap"
                            | "deepmap"
                            | "nodemap"
                            | "flatmap"
                            | "pairup"
                    );
                    if is_list_native_method {
                        method_is_nodal = true;
                    }
                    if is_iterable_item && !is_list_native_method {
                        // Hyper methods descend recursively through nested
                        // Iterables (and Hash values) down to the leaves, and
                        // any in-place leaf mutation (e.g. `@a>>++` on nested
                        // arrays) is written back through every level.
                        let (sub_result, sub_mutated) = self.hyper_method_apply_recursive(
                            item,
                            &method,
                            &item_args,
                            skip_native,
                        )?;
                        *item = sub_mutated;
                        results.push(sub_result);
                    } else {
                        let val = if !skip_native {
                            if let Some(native_result) =
                                self.try_native_method(item, Symbol::intern(&method), &item_args)
                            {
                                match native_result {
                                    Ok(v) => v,
                                    // Resumable warn from a per-element native
                                    // method: use its carried resume value and
                                    // remember the warn to re-raise after the loop.
                                    Err(e) if collect_warns && e.is_warn => {
                                        let rv = e.return_value.clone().unwrap_or(Value::Nil);
                                        if pending_warn.is_none() {
                                            pending_warn = Some(e);
                                        }
                                        rv
                                    }
                                    Err(e) => return Err(e),
                                }
                            } else {
                                let (v, updated) = self.call_method_mut_with_temp_target(
                                    item, &method, item_args, idx,
                                )?;
                                *item = updated;
                                v
                            }
                        } else {
                            let (v, updated) = self
                                .call_method_mut_with_temp_target(item, &method, item_args, idx)?;
                            *item = updated;
                            v
                        };
                        results.push(val);
                    }
                }
            }
        }
        if let Value::Array(existing, kind) = &target {
            if let Some(var) = &target_var {
                // Precise writeback to the *named* `@`-variable's binding only.
                // A bound variable holds a shared `ContainerRef` cell (e.g.
                // `$b := @a`): write the new array through the cell so aliases
                // observe it. A plain array binding is replaced (COW-detach), so
                // a copy `my @x = @a` keeps its own Arc and is NOT corrupted —
                // unlike the Arc-identity scan, which over-reaches COW copies.
                let new_arr = Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(items.clone())),
                    *kind,
                );
                self.write_back_hyper_target_var(code, var, new_arr);
            } else {
                // Non-variable target (`@b[0]>>++`, `(1,2,3)>>.uc`): write back
                // in place through the target's `Arc<ArrayData>` so a hyper
                // mutation on a *nested* element reaches it (the read shares the
                // inner Arc with the slot holding it), plus the by-identity scan
                // for any top-level binding that COW-detached.
                // SAFETY: mutsu is single-threaded; no immutable borrow into
                // this ArrayData is alive across the write.
                {
                    let ptr = std::sync::Arc::as_ptr(existing) as *mut crate::value::ArrayData;
                    unsafe {
                        (*ptr).items = items.clone();
                    }
                }
                self.interpreter.overwrite_array_items_by_identity_for_vm(
                    existing,
                    items.clone(),
                    *kind,
                );
            }
            if let Some(gv) = existing.grep_source.as_deref() {
                let mut source_items = gv.source.to_vec();
                for (filtered_idx, source_idx) in gv.indices.iter().enumerate() {
                    if filtered_idx < items.len() && *source_idx < source_items.len() {
                        source_items[*source_idx] = items[filtered_idx].clone();
                    }
                }
                self.interpreter.overwrite_array_items_by_identity_for_vm(
                    &gv.source,
                    source_items,
                    gv.source_kind,
                );
            }
            self.env_dirty = true;
        }
        // Hash target: write any in-place per-value mutation back to the
        // original variable (e.g. `%h>>++` increments the stored values). The
        // returned hash (built from `results` below) carries the method return
        // values, which for postfix `++`/`--` differ from the new stored values.
        if let Value::Hash(existing) = &target
            && let Some(keys) = &hash_keys
        {
            let mut map = std::collections::HashMap::with_capacity(keys.len());
            for (key, item) in keys.iter().zip(items.iter()) {
                map.insert(key.clone(), item.clone());
            }
            let new_hash = Value::Hash(Value::hash_arc(map));
            if let Some(var) = &target_var {
                // Precise writeback to the named `%`-variable (see the Array
                // arm); avoids corrupting a COW copy `my %g = %h`.
                self.write_back_hyper_target_var(code, var, new_hash);
            } else {
                self.interpreter
                    .overwrite_hash_bindings_by_identity(existing, new_hash);
            }
            self.env_dirty = true;
        }
        // Preserve the container type of the target for QuantHash types.
        // The items list was produced by value_to_list, which yields Pairs
        // in HashMap iteration order. Reconstruct the same type using the
        // keys from those pairs and the transformed results.
        match &target {
            Value::Mix(_, is_mutable) => {
                let mut weights = std::collections::HashMap::new();
                for (i, item) in items.iter().enumerate() {
                    if let Some(result) = results.get(i) {
                        let key = match item {
                            Value::Pair(k, _) => k.clone(),
                            Value::ValuePair(k, _) => k.to_string_value(),
                            other => other.to_string_value(),
                        };
                        weights.insert(
                            key,
                            crate::runtime::utils::to_float_value(result).unwrap_or(0.0),
                        );
                    }
                }
                let result = if *is_mutable {
                    Value::mix_hash(weights)
                } else {
                    Value::mix(weights)
                };
                self.stack.push(result);
                return Ok(());
            }
            Value::Bag(_, is_mutable) => {
                let mut counts = std::collections::HashMap::new();
                for (i, item) in items.iter().enumerate() {
                    if let Some(result) = results.get(i) {
                        let key = match item {
                            Value::Pair(k, _) => k.clone(),
                            Value::ValuePair(k, _) => k.to_string_value(),
                            other => other.to_string_value(),
                        };
                        counts.insert(key, crate::runtime::utils::to_int(result));
                    }
                }
                let result = if *is_mutable {
                    Value::bag_hash(counts)
                } else {
                    Value::bag(counts)
                };
                self.stack.push(result);
                return Ok(());
            }
            Value::Set(_, is_mutable) => {
                let mut elems = std::collections::HashSet::new();
                for (i, item) in items.iter().enumerate() {
                    if let Some(result) = results.get(i) {
                        let key = match item {
                            Value::Pair(k, _) => k.clone(),
                            Value::ValuePair(k, _) => k.to_string_value(),
                            other => other.to_string_value(),
                        };
                        if result.truthy() {
                            elems.insert(key);
                        }
                    }
                }
                let result = if *is_mutable {
                    Value::set_hash(elems)
                } else {
                    Value::set(elems)
                };
                self.stack.push(result);
                return Ok(());
            }
            _ => {}
        }
        // Hash target: rebuild a Hash pairing the original keys with the
        // per-value results.
        if let Some(keys) = hash_keys {
            let mut map = std::collections::HashMap::with_capacity(keys.len());
            for (key, value) in keys.into_iter().zip(results) {
                map.insert(key, value);
            }
            self.stack.push(Value::Hash(Value::hash_arc(map)));
            return Ok(());
        }
        // Preserve the container type of the target: Array->Array, List->List.
        // Nodal methods (applied at the node level) always yield a List, even
        // when hypered over an Array (e.g. `[[2,3],[4,5]]>>.reverse` is a List).
        let result_kind = match &target {
            Value::Array(_, kind) if kind.is_real_array() && !method_is_nodal => ArrayKind::Array,
            _ => ArrayKind::List,
        };
        let result = Value::Array(
            std::sync::Arc::new(crate::value::ArrayData::new(results)),
            result_kind,
        );
        // A per-element native method raised a resumable warn: re-raise it once,
        // carrying the whole hyper result as the resume value, so an enclosing
        // `CONTROL { .resume }` (or the default warn handler) resumes the entire
        // expression. The HyperMethodCall opcode records the resume point.
        if let Some(mut warn) = pending_warn {
            warn.return_value = Some(result);
            return Err(warn);
        }
        self.stack.push(result);
        Ok(())
    }

    /// Recursively apply a (non-nodal) hyper method to a value, descending into
    /// nested Iterables and Hash values down to the leaves. Returns
    /// `(result, mutated)`: `result` mirrors the structure with the method
    /// return values, while `mutated` mirrors the structure with any in-place
    /// leaf mutations (e.g. `>>++`) applied at every level so the caller can
    /// write the changes back to the original container.
    fn hyper_method_apply_recursive(
        &mut self,
        item: &Value,
        method: &str,
        args: &[Value],
        skip_native: bool,
    ) -> Result<(Value, Value), RuntimeError> {
        match item {
            Value::Array(elems, kind) => {
                let mut results = Vec::with_capacity(elems.len());
                let mut mutated = Vec::with_capacity(elems.len());
                for sub in elems.iter() {
                    let (r, m) =
                        self.hyper_method_apply_recursive(sub, method, args, skip_native)?;
                    results.push(r);
                    mutated.push(m);
                }
                Ok((
                    Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(results)),
                        *kind,
                    ),
                    Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(mutated)),
                        *kind,
                    ),
                ))
            }
            Value::Seq(elems) | Value::Slip(elems) => {
                let mut results = Vec::with_capacity(elems.len());
                let mut mutated = Vec::with_capacity(elems.len());
                for sub in elems.iter() {
                    let (r, m) =
                        self.hyper_method_apply_recursive(sub, method, args, skip_native)?;
                    results.push(r);
                    mutated.push(m);
                }
                Ok((
                    Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(results)),
                        ArrayKind::List,
                    ),
                    Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(mutated)),
                        ArrayKind::List,
                    ),
                ))
            }
            Value::Hash(map) => {
                let keys: Vec<String> = map.keys().cloned().collect();
                let mut res_map = std::collections::HashMap::with_capacity(keys.len());
                let mut mut_map = std::collections::HashMap::with_capacity(keys.len());
                for k in keys {
                    let v = map.get(&k).cloned().unwrap_or(Value::Nil);
                    let (r, m) =
                        self.hyper_method_apply_recursive(&v, method, args, skip_native)?;
                    res_map.insert(k.clone(), r);
                    mut_map.insert(k, m);
                }
                Ok((
                    Value::Hash(Value::hash_arc(res_map)),
                    Value::Hash(Value::hash_arc(mut_map)),
                ))
            }
            _ => {
                // Leaf: apply the method, mirroring the non-recursive leaf path.
                if !skip_native
                    && let Some(native_result) =
                        self.try_native_method(item, Symbol::intern(method), args)
                {
                    let v = native_result?;
                    // Native methods do not mutate the receiver: the mutated
                    // value is the original leaf unchanged.
                    return Ok((v, item.clone()));
                }
                let (v, updated) =
                    self.call_method_mut_with_temp_target(item, method, args.to_vec(), 0)?;
                Ok((v, updated))
            }
        }
    }

    pub(super) fn exec_hyper_method_call_dynamic_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        modifier_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx));
        let arity = arity as usize;
        if self.stack.len() < arity + 2 {
            return Err(RuntimeError::new(
                "VM stack underflow in HyperMethodCallDynamic",
            ));
        }
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        let name_val = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("VM stack underflow in HyperMethodCallDynamic name")
        })?;
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("VM stack underflow in HyperMethodCallDynamic target")
        })?;
        let mut items = crate::runtime::value_to_list(&target);
        let mut results = Vec::with_capacity(items.len());
        let method = (!matches!(
            &name_val,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
        ))
        .then(|| {
            let method_raw = name_val.to_string_value();
            Self::rewrite_method_name(&method_raw, modifier)
        });
        for (idx, item) in items.iter_mut().enumerate() {
            let item_args = args.clone();
            if matches!(
                &name_val,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            ) {
                let mut call_args = Vec::with_capacity(item_args.len() + 1);
                call_args.push(item.clone());
                call_args.extend(item_args);
                match modifier {
                    Some("?") => {
                        results.push(
                            self.vm_call_on_value(name_val.clone(), call_args, None)
                                .unwrap_or(Value::Package(Symbol::intern("Any"))),
                        );
                    }
                    Some("+") => {
                        let val = self.vm_call_on_value(name_val.clone(), call_args, None)?;
                        results.push(Value::array(vec![val]));
                    }
                    Some("*") => match self.vm_call_on_value(name_val.clone(), call_args, None) {
                        Ok(v) => results.push(Value::array(vec![v])),
                        Err(_) => results.push(Value::array(vec![])),
                    },
                    _ => {
                        results.push(self.vm_call_on_value(name_val.clone(), call_args, None)?);
                    }
                }
                continue;
            }
            let method = method
                .as_ref()
                .expect("method string exists for non-callables");
            match modifier {
                Some("?") => {
                    let val = if let Some(native_result) =
                        self.try_native_method(item, Symbol::intern(method), &item_args)
                    {
                        native_result.unwrap_or(Value::Package(Symbol::intern("Any")))
                    } else {
                        match self.call_method_mut_with_temp_target(item, method, item_args, idx) {
                            Ok((v, updated)) => {
                                *item = updated;
                                v
                            }
                            Err(_) => Value::Package(Symbol::intern("Any")),
                        }
                    };
                    results.push(val);
                }
                Some("+") => {
                    let vals = if let Some(native_result) =
                        self.try_native_method(item, Symbol::intern(method), &item_args)
                    {
                        vec![native_result?]
                    } else {
                        let (v, updated) =
                            self.call_method_all_with_temp_target(item, method, item_args, idx)?;
                        *item = updated;
                        v
                    };
                    results.push(Value::real_array(vals));
                }
                Some("*") => {
                    if let Some(native_result) =
                        self.try_native_method(item, Symbol::intern(method), &item_args)
                    {
                        match native_result {
                            Ok(v) => results.push(Value::real_array(vec![v])),
                            Err(_) => results.push(Value::real_array(vec![])),
                        }
                    } else {
                        match self.call_method_all_with_temp_target(item, method, item_args, idx) {
                            Ok((vals, updated)) => {
                                *item = updated;
                                results.push(Value::real_array(vals));
                            }
                            Err(_) => results.push(Value::real_array(vec![])),
                        }
                    }
                }
                _ => {
                    let val = if let Some(native_result) =
                        self.try_native_method(item, Symbol::intern(method), &item_args)
                    {
                        native_result?
                    } else {
                        let (v, updated) =
                            self.call_method_mut_with_temp_target(item, method, item_args, idx)?;
                        *item = updated;
                        v
                    };
                    results.push(val);
                }
            }
        }
        if let Value::Array(existing, kind) = &target {
            // In-place write back through the target's `Arc<ArrayData>` so a
            // nested-element hyper mutation reaches the element (see the twin
            // site in `exec_hyper_method_call_op`). SAFETY: single-threaded.
            {
                let ptr = std::sync::Arc::as_ptr(existing) as *mut crate::value::ArrayData;
                unsafe {
                    (*ptr).items = items.clone();
                }
            }
            self.interpreter.overwrite_array_items_by_identity_for_vm(
                existing,
                items.clone(),
                *kind,
            );
            self.env_dirty = true;
        }
        // Preserve the container type of the target for QuantHash types
        match &target {
            Value::Mix(_, is_mutable) => {
                let mut weights = std::collections::HashMap::new();
                for (i, item) in items.iter().enumerate() {
                    if let Some(result) = results.get(i) {
                        let key = match item {
                            Value::Pair(k, _) => k.clone(),
                            Value::ValuePair(k, _) => k.to_string_value(),
                            other => other.to_string_value(),
                        };
                        weights.insert(
                            key,
                            crate::runtime::utils::to_float_value(result).unwrap_or(0.0),
                        );
                    }
                }
                let result = if *is_mutable {
                    Value::mix_hash(weights)
                } else {
                    Value::mix(weights)
                };
                self.stack.push(result);
                return Ok(());
            }
            Value::Bag(_, is_mutable) => {
                let mut counts = std::collections::HashMap::new();
                for (i, item) in items.iter().enumerate() {
                    if let Some(result) = results.get(i) {
                        let key = match item {
                            Value::Pair(k, _) => k.clone(),
                            Value::ValuePair(k, _) => k.to_string_value(),
                            other => other.to_string_value(),
                        };
                        counts.insert(key, crate::runtime::utils::to_int(result));
                    }
                }
                let result = if *is_mutable {
                    Value::bag_hash(counts)
                } else {
                    Value::bag(counts)
                };
                self.stack.push(result);
                return Ok(());
            }
            Value::Set(_, is_mutable) => {
                let mut elems = std::collections::HashSet::new();
                for (i, item) in items.iter().enumerate() {
                    if let Some(result) = results.get(i) {
                        let key = match item {
                            Value::Pair(k, _) => k.clone(),
                            Value::ValuePair(k, _) => k.to_string_value(),
                            other => other.to_string_value(),
                        };
                        if result.truthy() {
                            elems.insert(key);
                        }
                    }
                }
                let result = if *is_mutable {
                    Value::set_hash(elems)
                } else {
                    Value::set(elems)
                };
                self.stack.push(result);
                return Ok(());
            }
            _ => {}
        }
        // Preserve the container type of the target: Array->Array, List->List
        let result_kind = match &target {
            Value::Array(_, kind) if kind.is_real_array() => ArrayKind::Array,
            _ => ArrayKind::List,
        };
        self.stack.push(Value::Array(
            std::sync::Arc::new(crate::value::ArrayData::new(results)),
            result_kind,
        ));
        Ok(())
    }
}
