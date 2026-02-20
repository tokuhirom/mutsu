use super::*;

impl Interpreter {
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
                    let mut items = match self.env.get(&key) {
                        Some(Value::Array(existing)) => existing.clone(),
                        _ => match target {
                            Value::Array(v) => v,
                            _ => Vec::new(),
                        },
                    };
                    items.extend(args);
                    let result = Value::Array(items.clone());
                    self.env.insert(key, Value::Array(items));
                    return Ok(result);
                }
                "append" => {
                    let mut items = match self.env.get(&key) {
                        Some(Value::Array(existing)) => existing.clone(),
                        _ => match target {
                            Value::Array(v) => v,
                            _ => Vec::new(),
                        },
                    };
                    for arg in args {
                        match arg {
                            Value::Array(vals) => items.extend(vals),
                            other => items.push(other),
                        }
                    }
                    self.env.insert(key, Value::Array(items));
                    return Ok(Value::Nil);
                }
                "unshift" | "prepend" => {
                    let items = match self.env.get(&key) {
                        Some(Value::Array(existing)) => existing.clone(),
                        _ => match target {
                            Value::Array(v) => v,
                            _ => Vec::new(),
                        },
                    };
                    let mut pref = args;
                    pref.extend(items);
                    self.env.insert(key, Value::Array(pref));
                    return Ok(Value::Nil);
                }
                "pop" => {
                    let mut items = match self.env.get(&key) {
                        Some(Value::Array(existing)) => existing.clone(),
                        _ => match target {
                            Value::Array(v) => v,
                            _ => Vec::new(),
                        },
                    };
                    let out = items.pop().unwrap_or(Value::Nil);
                    self.env.insert(key, Value::Array(items));
                    return Ok(out);
                }
                "shift" => {
                    let mut items = match self.env.get(&key) {
                        Some(Value::Array(existing)) => existing.clone(),
                        _ => match target {
                            Value::Array(v) => v,
                            _ => Vec::new(),
                        },
                    };
                    let out = if items.is_empty() {
                        Value::Nil
                    } else {
                        items.remove(0)
                    };
                    self.env.insert(key, Value::Array(items));
                    return Ok(out);
                }
                "splice" => {
                    let mut items = match self.env.get(&key) {
                        Some(Value::Array(existing)) => existing.clone(),
                        _ => match target {
                            Value::Array(v) => v,
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
                            Value::Array(new_items) => {
                                for (i, item) in new_items.iter().enumerate() {
                                    items.insert(start + i, item.clone());
                                }
                            }
                            other => items.insert(start, other.clone()),
                        }
                    }
                    self.env.insert(key, Value::Array(items));
                    return Ok(Value::Array(removed));
                }
                "squish" => {
                    let items = match self.env.get(&key) {
                        Some(Value::Array(existing)) => existing.clone(),
                        _ => match target {
                            Value::Array(v) => v,
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
                    self.env.insert(key, Value::Array(squished.clone()));
                    return Ok(Value::Array(squished));
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
            if self.is_native_method(&class_name, method) {
                // Try mutable dispatch first; if no mutable handler, fall back to immutable
                match self.call_native_instance_method_mut(
                    &class_name,
                    attributes.clone(),
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
                    self.run_instance_method(&class_name, attributes, method, args)?;
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
