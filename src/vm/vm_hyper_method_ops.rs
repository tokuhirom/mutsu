use super::*;
use crate::symbol::Symbol;

impl VM {
    pub(super) fn exec_hyper_method_call_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
    ) -> Result<(), RuntimeError> {
        self.ensure_env_synced(code);
        let method_raw = Self::const_str(code, name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
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
        // For Buf/Blob instances, expand to individual byte values for hyper dispatch
        let mut items = if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && crate::runtime::utils::is_buf_or_blob_class(&class_name.resolve())
        {
            if let Some(Value::Array(bytes, ..)) = attributes.get("bytes") {
                bytes.to_vec()
            } else {
                Vec::new()
            }
        } else {
            crate::runtime::value_to_list(&target)
        };
        let mut results = Vec::with_capacity(items.len());
        for (idx, item) in items.iter_mut().enumerate() {
            let method = Self::rewrite_method_name(&method_raw, modifier.as_deref());
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
            match modifier.as_deref() {
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
                    );
                    if is_iterable_item && !is_list_native_method {
                        let sub_items = crate::runtime::value_to_list(item);
                        let mut sub_results = Vec::with_capacity(sub_items.len());
                        for sub_item in sub_items {
                            let sub_val = if !skip_native {
                                if let Some(native_result) = self.try_native_method(
                                    &sub_item,
                                    Symbol::intern(&method),
                                    &item_args,
                                ) {
                                    native_result?
                                } else {
                                    let (v, _updated) = self.call_method_mut_with_temp_target(
                                        &sub_item,
                                        &method,
                                        item_args.clone(),
                                        idx,
                                    )?;
                                    v
                                }
                            } else {
                                let (v, _updated) = self.call_method_mut_with_temp_target(
                                    &sub_item,
                                    &method,
                                    item_args.clone(),
                                    idx,
                                )?;
                                v
                            };
                            sub_results.push(sub_val);
                        }
                        let sub_kind = match item {
                            Value::Array(_, kind) => *kind,
                            _ => ArrayKind::List,
                        };
                        results.push(Value::Array(std::sync::Arc::new(sub_results), sub_kind));
                    } else {
                        let val = if !skip_native {
                            if let Some(native_result) =
                                self.try_native_method(item, Symbol::intern(&method), &item_args)
                            {
                                native_result?
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
            self.interpreter.overwrite_array_items_by_identity_for_vm(
                existing,
                items.clone(),
                *kind,
            );
            if let Some((source, indices, source_kind)) =
                crate::runtime::utils::get_grep_view_binding(existing)
            {
                let mut source_items = source.to_vec();
                for (filtered_idx, source_idx) in indices.iter().enumerate() {
                    if filtered_idx < items.len() && *source_idx < source_items.len() {
                        source_items[*source_idx] = items[filtered_idx].clone();
                    }
                }
                self.interpreter.overwrite_array_items_by_identity_for_vm(
                    &source,
                    source_items,
                    source_kind,
                );
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
        // Preserve the container type of the target: Array->Array, List->List
        let result_kind = match &target {
            Value::Array(_, kind) if kind.is_real_array() => ArrayKind::Array,
            _ => ArrayKind::List,
        };
        self.stack
            .push(Value::Array(std::sync::Arc::new(results), result_kind));
        Ok(())
    }

    pub(super) fn exec_hyper_method_call_dynamic_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        modifier_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        self.ensure_env_synced(code);
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
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
            Self::rewrite_method_name(&method_raw, modifier.as_deref())
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
                match modifier.as_deref() {
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
            match modifier.as_deref() {
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
        // Preserve the container type of the target
        let result_kind = match &target {
            Value::Array(_, kind) if kind.is_real_array() => ArrayKind::Array,
            _ => ArrayKind::List,
        };
        self.stack
            .push(Value::Array(std::sync::Arc::new(results), result_kind));
        Ok(())
    }
}
