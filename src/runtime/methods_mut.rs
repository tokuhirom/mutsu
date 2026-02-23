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
        let Value::Instance {
            class_name,
            attributes,
            ..
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
            self.env.insert(
                var_name.to_string(),
                Value::make_instance(class_name, updated),
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
            ..
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
                    _ => self.call_method_with_values(target, method, args)?,
                };

                updated.insert("index".to_string(), Value::Int(index as i64));
                self.env.insert(
                    target_var.to_string(),
                    Value::make_instance(class_name, updated),
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
                    self.env.insert(
                        target_var.to_string(),
                        Value::make_instance(class_name, updated),
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
                        self.env.insert(
                            target_var.to_string(),
                            Value::make_instance(class_name, updated),
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
                let (result, updated) =
                    self.run_instance_method(&class_name, (*attributes).clone(), method, args)?;
                self.env.insert(
                    target_var.to_string(),
                    Value::make_instance(class_name, updated),
                );
                return Ok(result);
            }
        }
        self.call_method_with_values(target, method, args)
    }
}
