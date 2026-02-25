use super::*;

impl Interpreter {
    fn overwrite_array_bindings_by_identity(
        &mut self,
        needle: &std::sync::Arc<Vec<Value>>,
        replacement: Value,
    ) {
        let keys: Vec<String> = self
            .env
            .iter()
            .filter_map(|(name, value)| match value {
                Value::Array(existing, ..) if std::sync::Arc::ptr_eq(existing, needle) => {
                    Some(name.clone())
                }
                _ => None,
            })
            .collect();
        for key in keys {
            self.env.insert(key, replacement.clone());
        }
    }

    fn overwrite_hash_bindings_by_identity(
        &mut self,
        needle: &std::sync::Arc<std::collections::HashMap<String, Value>>,
        replacement: Value,
    ) {
        let keys: Vec<String> = self
            .env
            .iter()
            .filter_map(|(name, value)| match value {
                Value::Hash(existing) if std::sync::Arc::ptr_eq(existing, needle) => {
                    Some(name.clone())
                }
                _ => None,
            })
            .collect();
        for key in keys {
            self.env.insert(key, replacement.clone());
        }
    }

    pub(super) fn overwrite_instance_bindings_by_identity(
        &mut self,
        class_name: &str,
        id: u64,
        updated: std::collections::HashMap<String, Value>,
    ) {
        for bound in self.env.values_mut() {
            let should_replace = match bound {
                Value::Instance {
                    class_name: existing_class,
                    id: existing_id,
                    ..
                } => existing_class == class_name && *existing_id == id,
                _ => false,
            };
            if should_replace {
                *bound = Value::Instance {
                    class_name: class_name.to_string(),
                    attributes: std::sync::Arc::new(updated.clone()),
                    id,
                };
            }
        }
    }

    fn rw_method_attribute_target(body: &[Stmt]) -> Option<String> {
        let first = body.first()?;
        let extract_attr = |expr: &Expr| -> Option<String> {
            match expr {
                Expr::Var(name) if name.starts_with('!') && name.len() > 1 => {
                    Some(name[1..].to_string())
                }
                Expr::Call { name, args } if name == "return-rw" && args.len() == 1 => {
                    if let Expr::Var(attr) = &args[0]
                        && attr.starts_with('!')
                        && attr.len() > 1
                    {
                        return Some(attr[1..].to_string());
                    }
                    None
                }
                _ => None,
            }
        };
        match first {
            Stmt::Expr(expr) | Stmt::Return(expr) => extract_attr(expr),
            _ => None,
        }
    }

    pub(crate) fn assign_method_lvalue_with_values(
        &mut self,
        target_var: Option<&str>,
        target: Value,
        method: &str,
        method_args: Vec<Value>,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        if method == "value"
            && let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
            && class_name == "Pair"
            && let Some(Value::Str(key)) = attributes.get("key")
            && let Some(Value::Hash(source_hash)) = attributes.get("__mutsu_hash_ref")
        {
            let mut updated = (**source_hash).clone();
            updated.insert(key.clone(), value.clone());
            let replacement = Value::hash(updated);
            self.overwrite_hash_bindings_by_identity(source_hash, replacement);
            return Ok(value);
        }
        if method == "value"
            && let Value::Pair(key, current_value) = &target
        {
            let mut selected_hash: Option<
                std::sync::Arc<std::collections::HashMap<String, Value>>,
            > = None;

            if let Some(var_name) = target_var
                && let Some(Value::Hash(candidate)) = self.env.get(var_name)
                && candidate.contains_key(key)
            {
                selected_hash = Some(candidate.clone());
            }

            if selected_hash.is_none() {
                let mut candidates = self.env.values().filter_map(|bound| match bound {
                    Value::Hash(map)
                        if map
                            .get(key)
                            .is_some_and(|existing| existing == current_value.as_ref()) =>
                    {
                        Some(map.clone())
                    }
                    _ => None,
                });
                if let Some(first) = candidates.next()
                    && candidates.all(|other| std::sync::Arc::ptr_eq(&first, &other))
                {
                    selected_hash = Some(first);
                }
            }

            if let Some(source_hash) = selected_hash {
                let mut updated = (*source_hash).clone();
                updated.insert(key.clone(), value.clone());
                let replacement = Value::hash(updated);
                self.overwrite_hash_bindings_by_identity(&source_hash, replacement);
                return Ok(value);
            }
        }

        // Preserve existing accessor/setter assignment behavior for concrete variables.
        if let Some(var_name) = target_var {
            match self.call_method_mut_with_values(
                var_name,
                target.clone(),
                method,
                vec![value.clone()],
            ) {
                Ok(result) => return Ok(result),
                Err(err) => {
                    if !err
                        .message
                        .starts_with("No matching candidates for method:")
                    {
                        return Err(err);
                    }
                }
            }
        }

        let Value::Instance {
            class_name,
            attributes,
            id: target_id,
        } = target
        else {
            return Err(RuntimeError::new(format!(
                "X::Assignment::RO: cannot assign through .{} on non-instance",
                method
            )));
        };

        let method_def = self
            .resolve_method(&class_name, method, &method_args)
            .ok_or_else(|| {
                RuntimeError::new(format!("No matching candidates for method: {method}"))
            })?;
        if !method_def.is_rw {
            return Err(RuntimeError::new(format!(
                "X::Assignment::RO: method '{}' is not rw",
                method
            )));
        }
        let attr_name = Self::rw_method_attribute_target(&method_def.body).ok_or_else(|| {
            RuntimeError::new(format!(
                "X::Assignment::RO: rw method '{}' does not expose an assignable attribute",
                method
            ))
        })?;

        let mut updated = (*attributes).clone();
        updated.insert(attr_name, value.clone());
        if let Some(var_name) = target_var {
            self.overwrite_instance_bindings_by_identity(&class_name, target_id, updated.clone());
            self.env.insert(
                var_name.to_string(),
                Value::Instance {
                    class_name,
                    attributes: std::sync::Arc::new(updated),
                    id: target_id,
                },
            );
        }
        Ok(value)
    }

    pub(crate) fn call_method_mut_with_values(
        &mut self,
        target_var: &str,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if method == "VAR" && args.is_empty() {
            let readonly_key = format!("__mutsu_sigilless_readonly::{}", target_var);
            let alias_key = format!("__mutsu_sigilless_alias::{}", target_var);
            let has_sigilless_meta =
                self.env.contains_key(&readonly_key) || self.env.contains_key(&alias_key);
            if has_sigilless_meta {
                let readonly = matches!(self.env.get(&readonly_key), Some(Value::Bool(true)));
                let itemized_array = matches!(target, Value::Array(_, true));
                if readonly && !itemized_array {
                    return Ok(target);
                }
            }
            let class_name = if target_var.starts_with('@') {
                "Array"
            } else if target_var.starts_with('%') {
                "Hash"
            } else if target_var.starts_with('&') {
                "Sub"
            } else {
                "Scalar"
            };
            let display_name = if target_var.starts_with('$')
                || target_var.starts_with('@')
                || target_var.starts_with('%')
                || target_var.starts_with('&')
            {
                target_var.to_string()
            } else {
                format!("${}", target_var)
            };
            let mut attributes = HashMap::new();
            attributes.insert("name".to_string(), Value::Str(display_name));
            attributes.insert(
                "dynamic".to_string(),
                Value::Bool(self.is_var_dynamic(target_var)),
            );
            return Ok(Value::make_instance(class_name.to_string(), attributes));
        }
        if target_var.starts_with('@') {
            let key = target_var.to_string();
            match method {
                "push" => {
                    // Check shared_vars first (for cross-thread array sharing)
                    let mut items =
                        if let Some(Value::Array(existing, ..)) = self.get_shared_var(&key) {
                            existing.to_vec()
                        } else {
                            match self.env.get(&key) {
                                Some(Value::Array(existing, ..)) => existing.to_vec(),
                                _ => match target {
                                    Value::Array(v, ..) => v.to_vec(),
                                    _ => Vec::new(),
                                },
                            }
                        };
                    items.extend(args);
                    let result = Value::array(items.clone());
                    self.set_shared_var(&key, Value::array(items));
                    return Ok(result);
                }
                "append" => {
                    let mut items = match self.env.get(&key) {
                        Some(Value::Array(existing, ..)) => existing.to_vec(),
                        _ => match target {
                            Value::Array(v, ..) => v.to_vec(),
                            _ => Vec::new(),
                        },
                    };
                    for arg in args {
                        match arg {
                            Value::Array(vals, ..) => items.extend(vals.iter().cloned()),
                            other => items.push(other),
                        }
                    }
                    self.env.insert(key, Value::array(items));
                    return Ok(Value::Nil);
                }
                "unshift" | "prepend" => {
                    let items = match self.env.get(&key) {
                        Some(Value::Array(existing, ..)) => existing.to_vec(),
                        _ => match target {
                            Value::Array(v, ..) => v.to_vec(),
                            _ => Vec::new(),
                        },
                    };
                    let mut pref = args;
                    pref.extend(items);
                    self.env.insert(key, Value::array(pref));
                    return Ok(Value::Nil);
                }
                "pop" => {
                    let mut items = match self.env.get(&key) {
                        Some(Value::Array(existing, ..)) => existing.to_vec(),
                        _ => match target {
                            Value::Array(v, ..) => v.to_vec(),
                            _ => Vec::new(),
                        },
                    };
                    let out = items.pop().unwrap_or(Value::Nil);
                    self.env.insert(key, Value::array(items));
                    return Ok(out);
                }
                "shift" => {
                    let mut items = match self.env.get(&key) {
                        Some(Value::Array(existing, ..)) => existing.to_vec(),
                        _ => match target {
                            Value::Array(v, ..) => v.to_vec(),
                            _ => Vec::new(),
                        },
                    };
                    let out = if items.is_empty() {
                        Value::Nil
                    } else {
                        items.remove(0)
                    };
                    self.env.insert(key, Value::array(items));
                    return Ok(out);
                }
                "splice" => {
                    let mut items = match self.env.get(&key) {
                        Some(Value::Array(existing, ..)) => existing.to_vec(),
                        _ => match target {
                            Value::Array(v, ..) => v.to_vec(),
                            _ => Vec::new(),
                        },
                    };
                    let start = args
                        .first()
                        .and_then(|v| match v {
                            Value::Int(i) => Some((*i).max(0) as usize),
                            _ => None,
                        })
                        .unwrap_or(0)
                        .min(items.len());
                    let count = args
                        .get(1)
                        .and_then(|v| match v {
                            Value::Int(i) => Some((*i).max(0) as usize),
                            _ => None,
                        })
                        .unwrap_or(items.len().saturating_sub(start));
                    let end = (start + count).min(items.len());
                    let removed: Vec<Value> = items.drain(start..end).collect();
                    if let Some(new_val) = args.get(2) {
                        match new_val {
                            Value::Array(new_items, ..) => {
                                for (i, item) in new_items.iter().enumerate() {
                                    items.insert(start + i, item.clone());
                                }
                            }
                            other => items.insert(start, other.clone()),
                        }
                    }
                    self.env.insert(key, Value::array(items));
                    return Ok(Value::array(removed));
                }
                "squish" => {
                    let items = match self.env.get(&key) {
                        Some(Value::Array(existing, ..)) => existing.to_vec(),
                        _ => match target {
                            Value::Array(v, ..) => v.to_vec(),
                            _ => Vec::new(),
                        },
                    };
                    let mut squished = Vec::new();
                    let mut last: Option<String> = None;
                    for item in items {
                        let s = item.to_string_value();
                        if last.as_ref() != Some(&s) {
                            last = Some(s);
                            squished.push(item);
                        }
                    }
                    self.env.insert(key, Value::array(squished.clone()));
                    return Ok(Value::array(squished));
                }
                _ => {}
            }
        }

        // Handle push/append on hash variables
        if target_var.starts_with('%') {
            let key = target_var.to_string();
            match method {
                "push" | "append" => {
                    let is_push = method == "push";
                    let mut hash: std::collections::HashMap<String, Value> =
                        match self.env.get(&key) {
                            Some(Value::Hash(h, ..)) => (**h).clone(),
                            _ => match &target {
                                Value::Hash(h, ..) => (**h).clone(),
                                _ => std::collections::HashMap::new(),
                            },
                        };

                    // Collect key-value pairs from arguments
                    let pairs = Self::hash_push_collect_pairs(args);

                    // Push/append each pair into the hash
                    for (k, v) in pairs {
                        Self::hash_push_insert(&mut hash, k, v, is_push);
                    }

                    let result = Value::hash(hash.clone());
                    self.env.insert(key, Value::hash(hash));
                    return Ok(result);
                }
                _ => {}
            }
        }

        // Handle push/append/pop/shift/unshift on sigilless array bindings
        if !target_var.starts_with('@') && matches!(&target, Value::Array(..)) {
            let key = target_var.to_string();
            match method {
                "push" | "append" => {
                    let mut items = match &target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    if method == "append" {
                        for arg in &args {
                            match arg {
                                Value::Array(inner, ..) => items.extend(inner.iter().cloned()),
                                other => items.push(other.clone()),
                            }
                        }
                    } else {
                        items.extend(args);
                    }
                    let result = Value::array(items.clone());
                    self.env.insert(key, Value::array(items));
                    return Ok(result);
                }
                "pop" => {
                    let mut items = match &target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    let out = items.pop().unwrap_or(Value::Nil);
                    self.env.insert(key, Value::array(items));
                    return Ok(out);
                }
                "unshift" | "prepend" => {
                    let mut items = match &target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    for (i, arg) in args.iter().enumerate() {
                        items.insert(i, arg.clone());
                    }
                    let result = Value::array(items.clone());
                    self.env.insert(key, Value::array(items));
                    return Ok(result);
                }
                "shift" => {
                    let mut items = match &target {
                        Value::Array(v, ..) => v.to_vec(),
                        _ => Vec::new(),
                    };
                    let out = if items.is_empty() {
                        Value::Nil
                    } else {
                        items.remove(0)
                    };
                    self.env.insert(key, Value::array(items));
                    return Ok(out);
                }
                _ => {}
            }
        }

        // SharedPromise/SharedChannel are internally mutable — delegate to immutable dispatch
        if matches!(target, Value::Promise(_) | Value::Channel(_)) {
            return self.call_method_with_values(target, method, args);
        }

        if let Value::Instance {
            class_name,
            attributes,
            id: target_id,
        } = target.clone()
        {
            if class_name == "Iterator" {
                let mut updated = (*attributes).clone();
                let items = match updated.get("items") {
                    Some(Value::Array(values, ..)) => values.to_vec(),
                    _ => Vec::new(),
                };
                let mut index = match updated.get("index") {
                    Some(Value::Int(i)) if *i >= 0 => *i as usize,
                    _ => 0,
                };
                let len = items.len();

                let mut append_to_first_array_arg = |vals: &[Value]| {
                    if vals.is_empty() {
                        return;
                    }
                    if let Some(Value::Array(existing, is_array)) = args.first() {
                        let mut next = existing.to_vec();
                        next.extend(vals.iter().cloned());
                        let updated_array = Value::Array(std::sync::Arc::new(next), *is_array);
                        self.overwrite_array_bindings_by_identity(existing, updated_array);
                    }
                };

                let ret = match method {
                    "pull-one" => {
                        if index < len {
                            let out = items[index].clone();
                            index += 1;
                            out
                        } else {
                            Value::Str("IterationEnd".to_string())
                        }
                    }
                    "push-exactly" | "push-at-least" => {
                        let want = args.get(1).map(super::to_int).unwrap_or(1).max(0) as usize;
                        let available = len.saturating_sub(index);
                        let take = available.min(want);
                        if take > 0 {
                            append_to_first_array_arg(&items[index..index + take]);
                            index += take;
                        }
                        if index >= len {
                            Value::Str("IterationEnd".to_string())
                        } else {
                            Value::Nil
                        }
                    }
                    "push-all" | "push-until-lazy" => {
                        if index < len {
                            append_to_first_array_arg(&items[index..]);
                            index = len;
                        }
                        Value::Str("IterationEnd".to_string())
                    }
                    "sink-all" => {
                        index = len;
                        Value::Str("IterationEnd".to_string())
                    }
                    "skip-one" => {
                        if index < len {
                            index += 1;
                            Value::Bool(true)
                        } else {
                            Value::Bool(false)
                        }
                    }
                    "skip-at-least" => {
                        let want = args.first().map(super::to_int).unwrap_or(0).max(0) as usize;
                        let available = len.saturating_sub(index);
                        if available >= want {
                            index += want;
                            Value::Bool(true)
                        } else {
                            index = len;
                            Value::Bool(false)
                        }
                    }
                    "skip-at-least-pull-one" => {
                        let want = args.first().map(super::to_int).unwrap_or(0).max(0) as usize;
                        let available = len.saturating_sub(index);
                        if available >= want {
                            index += want;
                            if index < len {
                                let out = items[index].clone();
                                index += 1;
                                out
                            } else {
                                Value::Str("IterationEnd".to_string())
                            }
                        } else {
                            index = len;
                            Value::Str("IterationEnd".to_string())
                        }
                    }
                    "can" => {
                        let method_name = args
                            .first()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let supported = matches!(
                            method_name.as_str(),
                            "pull-one"
                                | "push-exactly"
                                | "push-at-least"
                                | "push-all"
                                | "push-until-lazy"
                                | "sink-all"
                                | "skip-one"
                                | "skip-at-least"
                                | "skip-at-least-pull-one"
                        );
                        if supported {
                            return Ok(Value::array(vec![Value::Str(method_name)]));
                        } else {
                            return Ok(Value::array(Vec::new()));
                        }
                    }
                    _ => self.call_method_with_values(target, method, args)?,
                };

                updated.insert("index".to_string(), Value::Int(index as i64));
                self.overwrite_instance_bindings_by_identity(
                    &class_name,
                    target_id,
                    updated.clone(),
                );
                self.env.insert(
                    target_var.to_string(),
                    Value::Instance {
                        class_name: class_name.clone(),
                        attributes: std::sync::Arc::new(updated),
                        id: target_id,
                    },
                );
                return Ok(ret);
            }

            if args.len() == 1 {
                let class_attrs = self.collect_class_attributes(&class_name);
                let is_public_accessor = if class_attrs.is_empty() {
                    attributes.contains_key(method)
                } else {
                    class_attrs
                        .iter()
                        .any(|(attr_name, is_public, _)| *is_public && attr_name == method)
                };
                if is_public_accessor {
                    let mut updated = (*attributes).clone();
                    let assigned = args[0].clone();
                    updated.insert(method.to_string(), assigned.clone());
                    self.overwrite_instance_bindings_by_identity(
                        &class_name,
                        target_id,
                        updated.clone(),
                    );
                    self.env.insert(
                        target_var.to_string(),
                        Value::Instance {
                            class_name: class_name.clone(),
                            attributes: std::sync::Arc::new(updated),
                            id: target_id,
                        },
                    );
                    return Ok(assigned);
                }
            }

            if self.is_native_method(&class_name, method) {
                // Try mutable dispatch first; if no mutable handler, fall back to immutable
                match self.call_native_instance_method_mut(
                    &class_name,
                    (*attributes).clone(),
                    method,
                    args.clone(),
                ) {
                    Ok((result, updated)) => {
                        self.overwrite_instance_bindings_by_identity(
                            &class_name,
                            target_id,
                            updated.clone(),
                        );
                        self.env.insert(
                            target_var.to_string(),
                            Value::Instance {
                                class_name: class_name.clone(),
                                attributes: std::sync::Arc::new(updated),
                                id: target_id,
                            },
                        );
                        return Ok(result);
                    }
                    Err(_) => {
                        return self.call_native_instance_method(
                            &class_name,
                            &attributes,
                            method,
                            args,
                        );
                    }
                }
            }
            if self.has_user_method(&class_name, method) {
                let (result, updated) = self.run_instance_method(
                    &class_name,
                    (*attributes).clone(),
                    method,
                    args,
                    Some(target.clone()),
                )?;
                self.overwrite_instance_bindings_by_identity(
                    &class_name,
                    target_id,
                    updated.clone(),
                );
                self.env.insert(
                    target_var.to_string(),
                    Value::Instance {
                        class_name: class_name.clone(),
                        attributes: std::sync::Arc::new(updated),
                        id: target_id,
                    },
                );
                return Ok(result);
            }
        }
        self.call_method_with_values(target, method, args)
    }

    /// Collect key-value pairs from Hash.push/append arguments.
    /// Arguments can be Pair values or alternating key, value flat lists.
    fn hash_push_collect_pairs(args: Vec<Value>) -> Vec<(String, Value)> {
        let mut pairs = Vec::new();
        let mut iter = args.into_iter().peekable();
        while let Some(arg) = iter.next() {
            match &arg {
                Value::Pair(k, v) => {
                    pairs.push((k.clone(), (**v).clone()));
                }
                Value::ValuePair(k, v) => {
                    pairs.push((k.to_string_value(), (**v).clone()));
                }
                Value::Array(items, ..) => {
                    // Recursively collect pairs from array elements
                    let inner_pairs = Self::hash_push_collect_pairs(items.to_vec());
                    pairs.extend(inner_pairs);
                }
                Value::Hash(h, ..) => {
                    for (k, v) in h.iter() {
                        pairs.push((k.clone(), v.clone()));
                    }
                }
                _ => {
                    // Alternating key, value
                    let key = arg.to_string_value();
                    let val = iter.next().unwrap_or(Value::Nil);
                    pairs.push((key, val));
                }
            }
        }
        pairs
    }

    /// Insert a key-value pair into a hash with push/append semantics.
    /// push: if key exists, stack the new value (existing becomes [existing, new])
    /// append: if key exists, flatten arrays when appending
    fn hash_push_insert(
        hash: &mut std::collections::HashMap<String, Value>,
        key: String,
        value: Value,
        is_push: bool,
    ) {
        if let Some(existing) = hash.get(&key) {
            let new_val = match existing {
                Value::Array(arr, ..) => {
                    let mut items = arr.to_vec();
                    if is_push {
                        // push: add value as-is (could be nested array)
                        items.push(value);
                    } else {
                        // append: flatten arrays
                        match value {
                            Value::Array(new_items, ..) => {
                                items.extend(new_items.iter().cloned());
                            }
                            other => items.push(other),
                        }
                    }
                    Value::array(items)
                }
                _ => {
                    // First duplicate: create array [existing, new]
                    if is_push {
                        Value::array(vec![existing.clone(), value])
                    } else {
                        // append: flatten arrays
                        let mut items = vec![existing.clone()];
                        match value {
                            Value::Array(new_items, ..) => {
                                items.extend(new_items.iter().cloned());
                            }
                            other => items.push(other),
                        }
                        Value::array(items)
                    }
                }
            };
            hash.insert(key, new_val);
        } else {
            hash.insert(key, value);
        }
    }
}
