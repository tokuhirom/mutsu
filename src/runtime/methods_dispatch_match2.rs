use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Dispatch collection and iteration methods by name.
    /// Returns Some(result) if the method was handled, None to fall through.
    #[allow(clippy::too_many_lines)]
    pub(super) fn dispatch_method_by_name_2(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        match method {
            "Seq" if args.is_empty() => Some(self.dispatch_seq_coercion(target)),
            "list" | "Array" if args.is_empty() => {
                if let Value::Instance {
                    class_name,
                    attributes,
                    ..
                } = &target
                    && class_name == "Supply"
                {
                    return Some(self.supply_list_values(attributes, true).map(Value::array));
                }
                None
            }
            "List" if args.is_empty() => Some(self.dispatch_list_coercion(target)),
            "Set" | "SetHash" if args.is_empty() => {
                let result = match self.dispatch_to_set(target) {
                    Ok(r) => r,
                    Err(e) => return Some(Err(e)),
                };
                // If SetHash, ensure the mutable flag is set
                if method == "SetHash"
                    && let Value::Set(items, _) = result
                {
                    return Some(Ok(Value::Set(items, true)));
                }
                Some(Ok(result))
            }
            "Bag" | "BagHash" if args.is_empty() => {
                let result = match self.dispatch_to_bag(target) {
                    Ok(r) => r,
                    Err(e) => return Some(Err(e)),
                };
                // If BagHash, ensure the mutable flag is set
                if method == "BagHash"
                    && let Value::Bag(items, _) = result
                {
                    return Some(Ok(Value::Bag(items, true)));
                }
                Some(Ok(result))
            }
            "Mix" | "MixHash" if args.is_empty() => {
                let result = match self.dispatch_to_mix(target) {
                    Ok(r) => r,
                    Err(e) => return Some(Err(e)),
                };
                if method == "MixHash" {
                    // Ensure mutable flag is set for MixHash
                    let result = if let Value::Mix(items, _) = result {
                        Value::Mix(items, true)
                    } else {
                        result
                    };
                    self.register_container_type_metadata(
                        &result,
                        ContainerTypeInfo {
                            value_type: "Real".to_string(),
                            key_type: None,
                            declared_type: Some("MixHash".to_string()),
                        },
                    );
                    return Some(Ok(result));
                }
                Some(Ok(result))
            }
            "Setty" | "Baggy" | "Mixy" if args.is_empty() => {
                self.dispatch_setty_baggy_mixy(&target, method)
            }
            "Map" | "Hash" if args.is_empty() => {
                if matches!(&target, Value::Package(_)) {
                    return Some(Ok(Value::Package(Symbol::intern(method))));
                }
                if method == "Map" {
                    let result = match self.dispatch_to_map(target) {
                        Ok(r) => r,
                        Err(e) => return Some(Err(e)),
                    };
                    if matches!(&result, Value::Hash(_)) {
                        let info = ContainerTypeInfo {
                            value_type: String::new(),
                            key_type: None,
                            declared_type: Some("Map".to_string()),
                        };
                        self.register_container_type_metadata(&result, info);
                    }
                    return Some(Ok(result));
                }
                Some(self.dispatch_to_hash(target))
            }
            "hash" if args.is_empty() && !matches!(&target, Value::Instance { .. }) => {
                Some(self.dispatch_to_hash(target))
            }
            "any" | "all" | "one" | "none" if args.is_empty() => {
                let kind = match method {
                    "any" => JunctionKind::Any,
                    "all" => JunctionKind::All,
                    "one" => JunctionKind::One,
                    _ => JunctionKind::None,
                };
                let values = Self::value_to_list(&target);
                Some(Ok(Value::junction(kind, values)))
            }
            "iterator" if args.is_empty() => Some(self.dispatch_iterator_method(target)),
            "produce" => self.dispatch_produce_method(target, args),
            "reduce" => Some(self.dispatch_reduce_method(target, args)),
            "elems" => self.dispatch_elems_method(target, args),
            "map" => Some(self.dispatch_map_method(target, args)),
            "duckmap" => {
                let block = args.first().cloned().unwrap_or(Value::Nil);
                Some(self.duckmap_iterate(&block, &target))
            }
            "deepmap" => {
                let block = args.first().cloned().unwrap_or(Value::Nil);
                Some(self.deepmap_iterate(&block, &target))
            }
            "nodemap" => {
                let block = args.first().cloned().unwrap_or(Value::Nil);
                Some(self.nodemap_iterate(&block, &target))
            }
            "max" | "min" => Some(self.dispatch_min_max_method(target, method, args)),
            "minpairs" | "maxpairs" if args.is_empty() => {
                Some(self.dispatch_minmaxpairs(target, method))
            }
            "pop" => Some(self.dispatch_pop_method(target, args)),
            "sort" => Some(self.dispatch_sort_method(target, args)),
            "unique" => {
                if !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply")
                {
                    Some(self.dispatch_unique(target, &args))
                } else {
                    None
                }
            }
            "repeated" => {
                if !matches!(&target, Value::Instance { class_name, .. } if class_name == "Supply")
                {
                    Some(self.dispatch_repeated(target, &args))
                } else {
                    None
                }
            }
            "squish" => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    Some(self.dispatch_supply_transform(target, "squish", &args))
                } else {
                    Some(self.dispatch_squish(target, &args))
                }
            }
            "minmax" => self.dispatch_minmax_method(target, method, args),
            "snip" => self.dispatch_snip_method(target, method, args),
            "head" | "flat" | "batch" | "comb" | "words" | "wait" | "zip" | "zip-latest" => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    Some(self.dispatch_supply_transform(target, method, &args))
                } else {
                    None
                }
            }
            "set" | "primary" | "secondary" | "tertiary" | "quaternary" | "gist" if matches!(&target, Value::Instance { class_name, .. } if class_name == "Collation") => {
                Some(self.dispatch_collation_method(target, method, &args))
            }
            "collate" if args.is_empty() => Some(self.dispatch_collate(target)),
            "take" if args.is_empty() => {
                match self.take_value(target.clone()) {
                    Ok(_) => {}
                    Err(e) => return Some(Err(e)),
                }
                Some(Ok(target))
            }
            "rotor" => {
                if let Value::Instance { class_name, .. } = &target
                    && class_name == "Supply"
                {
                    Some(self.dispatch_supply_transform(target, "rotor", &args))
                } else {
                    Some(self.dispatch_rotor(target, &args))
                }
            }
            _ => None,
        }
    }

    /// Dispatch the "iterator" method.
    fn dispatch_iterator_method(&mut self, target: Value) -> Result<Value, RuntimeError> {
        if matches!(&target, Value::Instance { class_name, .. } if class_name == "Iterator") {
            return Ok(target);
        }
        if let Value::Seq(items) = &target {
            let seq_id = std::sync::Arc::as_ptr(items) as usize;
            if let Some(meta) = self.squish_iterator_meta.remove(&seq_id) {
                for key in meta.revert_remove {
                    self.env.remove(&key);
                }
                for (key, value) in meta.revert_values {
                    self.env.insert(key, value);
                }
                let mut attrs = HashMap::new();
                attrs.insert("squish_source".to_string(), Value::array(meta.source_items));
                attrs.insert("squish_as".to_string(), meta.as_func.unwrap_or(Value::Nil));
                attrs.insert(
                    "squish_with".to_string(),
                    meta.with_func.unwrap_or(Value::Nil),
                );
                attrs.insert("squish_scan_index".to_string(), Value::Int(0));
                attrs.insert("squish_prev_key".to_string(), Value::Nil);
                attrs.insert("squish_initialized".to_string(), Value::Bool(false));
                return Ok(Value::make_instance(Symbol::intern("Iterator"), attrs));
            }
        }
        let lazy = crate::builtins::methods_0arg::is_value_lazy(&target);
        let items = if crate::runtime::utils::is_shaped_array(&target) {
            crate::runtime::utils::shaped_array_leaves(&target)
        } else {
            crate::runtime::utils::value_to_list(&target)
        };
        let mut attrs = HashMap::new();
        attrs.insert("items".to_string(), Value::array(items));
        attrs.insert("index".to_string(), Value::Int(0));
        if lazy {
            attrs.insert("is_lazy".to_string(), Value::Bool(true));
        }
        Ok(Value::make_instance(Symbol::intern("Iterator"), attrs))
    }

    /// Dispatch the "produce" method.
    fn dispatch_produce_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let Value::Instance { class_name, .. } = &target
            && class_name == "Supply"
        {
            return Some(self.dispatch_supply_transform(target, "produce", &args));
        }
        let callable = match args.first().cloned() {
            Some(c) => c,
            None => return Some(Err(RuntimeError::new("produce expects a callable"))),
        };
        if !matches!(
            target,
            Value::Array(_, _)
                | Value::Seq(_)
                | Value::Slip(_)
                | Value::LazyList(_)
                | Value::Range(_, _)
                | Value::RangeExcl(_, _)
                | Value::RangeExclStart(_, _)
                | Value::RangeExclBoth(_, _)
                | Value::GenericRange { .. }
                | Value::Hash(_)
        ) {
            return Some(Ok(target));
        }
        Some(self.call_function("produce", vec![callable, target]))
    }

    /// Dispatch the "reduce" method.
    fn dispatch_reduce_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let callable = args
            .first()
            .cloned()
            .ok_or_else(|| RuntimeError::new("reduce expects a callable"))?;
        if let Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } = target
            && class_name == "Supply"
        {
            let attrs_clone = (**attributes).clone();
            return self.dispatch_supply_reduce(target, &attrs_clone, callable);
        }
        let items = Self::value_to_list(&target);
        self.reduce_items(callable, items)
    }

    /// Dispatch the "elems" method.
    fn dispatch_elems_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } = target
            && class_name == "Supply"
        {
            return Some(self.dispatch_supply_elems(attributes, &args));
        }
        Some(self.call_function("elems", vec![target]))
    }

    /// Dispatch the "map" method.
    fn dispatch_map_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance {
            ref class_name,
            ref attributes,
            ..
        } = target
            && class_name == "Supply"
        {
            return self.dispatch_supply_map(attributes, &args);
        }
        // Validate that the map argument is callable (X::Cannot::Map)
        if let Some(func) = args.first() {
            let is_callable = matches!(
                func,
                Value::Sub(_)
                    | Value::Routine { .. }
                    | Value::WeakSub(_)
                    | Value::Regex(_)
                    | Value::RegexWithAdverbs { .. }
            ) || (matches!(func, Value::Instance { class_name, .. } if class_name.resolve() == "WhateverCode" || class_name.resolve() == "HyperWhateverCode"))
                || matches!(func, Value::Whatever);
            if !is_callable {
                let mut attrs = HashMap::new();
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Cannot map a {} to a {}, it's not callable.",
                        super::value_type_name(func),
                        super::value_type_name(&target),
                    )),
                );
                let ex =
                    Value::make_instance(crate::symbol::Symbol::intern("X::Cannot::Map"), attrs);
                let mut err = RuntimeError::new(format!(
                    "X::Cannot::Map: Cannot map a {} to a {}",
                    super::value_type_name(func),
                    super::value_type_name(&target),
                ));
                err.exception = Some(Box::new(ex));
                return Err(err);
            }
        }
        let items = if crate::runtime::utils::is_shaped_array(&target) {
            crate::runtime::utils::shaped_array_leaves(&target)
        } else {
            Self::value_to_list(&target)
        };
        let result = self.eval_map_over_items(args.first().cloned(), items)?;
        // .map() returns a Seq per Raku spec
        Ok(match result {
            Value::Array(items, _) => Value::Seq(items),
            other => other,
        })
    }

    /// Dispatch "min" and "max" methods.
    fn dispatch_min_max_method(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance { class_name, .. } = &target
            && class_name == "Supply"
        {
            return self.dispatch_supply_running_extrema(target, method, &args);
        }
        let mut call_args = vec![target.clone()];
        if let Some(first) = args.first() {
            if matches!(
                first,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            ) {
                call_args.push(Value::Pair("by".to_string(), Box::new(first.clone())));
            } else {
                call_args.extend(args.clone());
            }
        }
        if method == "max" {
            self.builtin_max(&call_args)
        } else {
            self.builtin_min(&call_args)
        }
    }

    /// Dispatch the "pop" method.
    fn dispatch_pop_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if !args.is_empty() {
            return Err(RuntimeError::new(format!(
                "Too many positionals passed; expected 1 argument but got {}",
                args.len() + 1
            )));
        }
        match target {
            Value::Array(_, kind) if kind.is_lazy() => Err(RuntimeError::cannot_lazy("pop")),
            Value::Array(mut items, ..) => {
                let items_mut = Arc::make_mut(&mut items);
                Ok(if items_mut.is_empty() {
                    make_empty_array_failure("pop")
                } else {
                    items_mut.pop().unwrap_or(Value::Nil)
                })
            }
            _ => Ok(make_empty_array_failure("pop")),
        }
    }

    /// Dispatch the "sort" method.
    fn dispatch_sort_method(
        &mut self,
        target: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance { class_name, .. } = &target
            && class_name == "Supply"
        {
            return self.dispatch_supply_transform(target, "sort", &args);
        }
        if let Value::Package(name) = &target
            && name == "Supply"
        {
            return Ok(Value::Seq(std::sync::Arc::new(vec![target])));
        }
        self.dispatch_sort(target, &args)
    }

    /// Dispatch the "minmax" method.
    fn dispatch_minmax_method(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let Value::Instance { class_name, .. } = &target
            && class_name == "Supply"
        {
            if !args.is_empty() {
                return Some(self.dispatch_supply_transform(target, method, &args));
            }
            // Fall through for 0-arg Supply.minmax
            return None;
        }
        let mut call_args = vec![target.clone()];
        if let Some(first) = args.first() {
            if matches!(
                first,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            ) {
                call_args.push(Value::Pair("by".to_string(), Box::new(first.clone())));
            } else {
                call_args.extend(args.clone());
            }
        }
        Some(self.builtin_minmax(&call_args))
    }

    /// Dispatch the "snip" method.
    fn dispatch_snip_method(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Option<Result<Value, RuntimeError>> {
        if let Value::Instance { class_name, .. } = &target
            && class_name == "Supply"
        {
            return Some(self.dispatch_supply_transform(target, method, &args));
        }
        if !args.is_empty() {
            let matcher = args[0].clone();
            let items = crate::runtime::utils::value_to_list(&target);
            return Some(self.eval_snip(matcher, items));
        }
        None
    }
}
