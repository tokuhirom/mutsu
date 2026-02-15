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
                    self.env.insert(key, Value::Array(items));
                    return Ok(Value::Nil);
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

        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = target.clone()
        {
            if class_name == "Promise" && method == "keep" {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                let mut attrs = attributes.clone();
                attrs.insert("result".to_string(), value);
                attrs.insert("status".to_string(), Value::Str("Kept".to_string()));
                self.env.insert(
                    target_var.to_string(),
                    Value::make_instance(class_name, attrs),
                );
                return Ok(Value::Nil);
            }
            if class_name == "Channel" {
                if method == "send" {
                    let value = args.first().cloned().unwrap_or(Value::Nil);
                    let mut attrs = attributes.clone();
                    match attrs.get_mut("queue") {
                        Some(Value::Array(items)) => items.push(value),
                        _ => {
                            attrs.insert("queue".to_string(), Value::Array(vec![value]));
                        }
                    }
                    self.env.insert(
                        target_var.to_string(),
                        Value::make_instance(class_name, attrs),
                    );
                    return Ok(Value::Nil);
                }
                if method == "receive" && args.is_empty() {
                    let mut attrs = attributes.clone();
                    let mut value = Value::Nil;
                    if let Some(Value::Array(items)) = attrs.get_mut("queue")
                        && !items.is_empty()
                    {
                        value = items.remove(0);
                    }
                    self.env.insert(
                        target_var.to_string(),
                        Value::make_instance(class_name, attrs),
                    );
                    return Ok(value);
                }
                if method == "close" && args.is_empty() {
                    let mut attrs = attributes.clone();
                    attrs.insert("closed".to_string(), Value::Bool(true));
                    self.env.insert(
                        target_var.to_string(),
                        Value::make_instance(class_name, attrs),
                    );
                    return Ok(Value::Nil);
                }
            }
            if class_name == "Supply" {
                if method == "emit" {
                    let value = args.first().cloned().unwrap_or(Value::Nil);
                    let mut attrs = attributes.clone();
                    if let Some(Value::Array(items)) = attrs.get_mut("values") {
                        items.push(value.clone());
                    } else {
                        attrs.insert("values".to_string(), Value::Array(vec![value.clone()]));
                    }
                    if let Some(Value::Array(taps)) = attrs.get_mut("taps") {
                        for tap in taps.clone() {
                            let _ = self.call_sub_value(tap, vec![value.clone()], true);
                        }
                    }
                    self.env.insert(
                        target_var.to_string(),
                        Value::make_instance(class_name, attrs),
                    );
                    return Ok(Value::Nil);
                }
                if method == "tap" {
                    let tap = args.first().cloned().unwrap_or(Value::Nil);
                    let mut attrs = attributes.clone();
                    if let Some(Value::Array(items)) = attrs.get_mut("taps") {
                        items.push(tap.clone());
                    } else {
                        attrs.insert("taps".to_string(), Value::Array(vec![tap.clone()]));
                    }
                    if let Some(Value::Array(values)) = attrs.get("values") {
                        for v in values {
                            let _ = self.call_sub_value(tap.clone(), vec![v.clone()], true);
                        }
                    }
                    self.env.insert(
                        target_var.to_string(),
                        Value::make_instance(class_name, attrs),
                    );
                    return Ok(Value::Nil);
                }
            }
            if class_name == "Proc::Async" && method == "start" && args.is_empty() {
                let mut attrs = attributes.clone();
                attrs.insert("started".to_string(), Value::Bool(true));
                self.env.insert(
                    target_var.to_string(),
                    Value::make_instance(class_name, attrs),
                );
                return Ok(self.make_promise_instance("Kept", Value::Int(0)));
            }
            if self.class_has_method(&class_name, method) {
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
