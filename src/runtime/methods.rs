use super::*;

impl Interpreter {
    pub(crate) fn call_method_with_values(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let native_result = match args.as_slice() {
            [] => crate::builtins::native_method_0arg(&target, method),
            [a] => crate::builtins::native_method_1arg(&target, method, a),
            [a, b] => crate::builtins::native_method_2arg(&target, method, a, b),
            _ => None,
        };
        if let Some(result) = native_result {
            return result;
        }
        if method == "say" && args.is_empty() {
            self.output.push_str(&target.to_string_value());
            self.output.push('\n');
            return Ok(Value::Nil);
        }
        if method == "VAR" && args.is_empty() {
            // Non-container .VAR is identity. Container variables are handled in
            // call_method_mut_with_values via target variable metadata.
            return Ok(target);
        }
        if (method == "in" || method == "anyof" || method == "allof")
            && matches!(&target, Value::Package(name) if name == "Promise")
        {
            return Ok(self.make_promise_instance("Kept", Value::Nil));
        }
        if method == "WHAT" && args.is_empty() {
            let type_name = match &target {
                Value::Int(_) => "Int",
                Value::BigInt(_) => "Int",
                Value::Num(_) => "Num",
                Value::Str(_) => "Str",
                Value::Bool(_) => "Bool",
                Value::Range(_, _) => "Range",
                Value::RangeExcl(_, _)
                | Value::RangeExclStart(_, _)
                | Value::RangeExclBoth(_, _) => "Range",
                Value::Array(_) => "Array",
                Value::LazyList(_) => "Array",
                Value::Hash(_) => "Hash",
                Value::Rat(_, _) => "Rat",
                Value::FatRat(_, _) => "FatRat",
                Value::Complex(_, _) => "Complex",
                Value::Set(_) => "Set",
                Value::Bag(_) => "Bag",
                Value::Mix(_) => "Mix",
                Value::Pair(_, _) => "Pair",
                Value::Enum { enum_type, .. } => enum_type.as_str(),
                Value::Nil => "Any",
                Value::Package(name) => name.as_str(),
                Value::Routine { .. } => "Routine",
                Value::Sub { .. } => "Sub",
                Value::CompUnitDepSpec { .. } => "CompUnit::DependencySpecification",
                Value::Instance { class_name, .. } => class_name.as_str(),
                Value::Junction { .. } => "Junction",
                Value::Regex(_) => "Regex",
                Value::Version { .. } => "Version",
                Value::Slip(_) => "Slip",
            };
            return Ok(Value::Package(type_name.to_string()));
        }
        if method == "enums"
            && let Value::Str(type_name) = &target
            && let Some(variants) = self.enum_types.get(type_name)
        {
            let mut map = HashMap::new();
            for (k, v) in variants {
                map.insert(k.clone(), Value::Int(*v));
            }
            return Ok(Value::Hash(map));
        }
        if method == "match" {
            if let Some(pattern) = args.first() {
                let text = target.to_string_value();
                return match pattern {
                    Value::Regex(pat) | Value::Str(pat) => {
                        if let Some(captures) = self.regex_match_with_captures(pat, &text) {
                            for (k, v) in captures {
                                self.env.insert(format!("<{}>", k), Value::Str(v));
                            }
                            Ok(Value::Bool(true))
                        } else {
                            Ok(Value::Bool(false))
                        }
                    }
                    _ => Ok(Value::Nil),
                };
            }
            return Ok(Value::Nil);
        }
        if (method == "Set" || method == "SetHash") && args.is_empty() {
            let mut elems = HashSet::new();
            match target {
                Value::Set(_) => return Ok(target),
                Value::Array(items) => {
                    for item in items {
                        elems.insert(item.to_string_value());
                    }
                }
                Value::Hash(items) => {
                    for (k, v) in items {
                        if v.truthy() {
                            elems.insert(k);
                        }
                    }
                }
                Value::Bag(b) => {
                    for k in b.keys() {
                        elems.insert(k.clone());
                    }
                }
                Value::Mix(m) => {
                    for k in m.keys() {
                        elems.insert(k.clone());
                    }
                }
                other => {
                    elems.insert(other.to_string_value());
                }
            }
            return Ok(Value::Set(elems));
        }
        if (method == "Bag" || method == "BagHash") && args.is_empty() {
            let mut counts: HashMap<String, i64> = HashMap::new();
            match target {
                Value::Bag(_) => return Ok(target),
                Value::Array(items) => {
                    for item in items {
                        *counts.entry(item.to_string_value()).or_insert(0) += 1;
                    }
                }
                Value::Set(s) => {
                    for k in s {
                        counts.insert(k, 1);
                    }
                }
                Value::Mix(m) => {
                    for (k, v) in m {
                        counts.insert(k, v as i64);
                    }
                }
                other => {
                    counts.insert(other.to_string_value(), 1);
                }
            }
            return Ok(Value::Bag(counts));
        }
        if (method == "Mix" || method == "MixHash") && args.is_empty() {
            let mut weights: HashMap<String, f64> = HashMap::new();
            match target {
                Value::Mix(_) => return Ok(target),
                Value::Array(items) => {
                    for item in items {
                        *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                    }
                }
                Value::Set(s) => {
                    for k in s {
                        weights.insert(k, 1.0);
                    }
                }
                Value::Bag(b) => {
                    for (k, v) in b {
                        weights.insert(k, v as f64);
                    }
                }
                other => {
                    weights.insert(other.to_string_value(), 1.0);
                }
            }
            return Ok(Value::Mix(weights));
        }
        if method == "map" {
            let items = Self::value_to_list(&target);
            return self.eval_map_over_items(args.first().cloned(), items);
        }
        if method == "sort" {
            return match target {
                Value::Array(mut items) => {
                    if let Some(Value::Sub {
                        params, body, env, ..
                    }) = args.first().cloned()
                    {
                        let placeholders = collect_placeholders(&body);
                        let is_key_extractor =
                            placeholders.len() < 2 && params.len() <= 1 || placeholders.len() == 1;
                        if is_key_extractor {
                            items.sort_by(|a, b| {
                                let saved = self.env.clone();
                                for (k, v) in &env {
                                    self.env.insert(k.clone(), v.clone());
                                }
                                if let Some(p) = params.first() {
                                    self.env.insert(p.clone(), a.clone());
                                }
                                self.env.insert("_".to_string(), a.clone());
                                let key_a = self.eval_block_value(&body).unwrap_or(Value::Nil);
                                self.env = saved.clone();
                                for (k, v) in &env {
                                    self.env.insert(k.clone(), v.clone());
                                }
                                if let Some(p) = params.first() {
                                    self.env.insert(p.clone(), b.clone());
                                }
                                self.env.insert("_".to_string(), b.clone());
                                let key_b = self.eval_block_value(&body).unwrap_or(Value::Nil);
                                self.env = saved;
                                key_a.to_string_value().cmp(&key_b.to_string_value())
                            });
                        } else {
                            items.sort_by(|a, b| {
                                let saved = self.env.clone();
                                for (k, v) in &env {
                                    self.env.insert(k.clone(), v.clone());
                                }
                                if let Some(p) = params.first() {
                                    self.env.insert(p.clone(), a.clone());
                                }
                                if placeholders.len() >= 2 {
                                    self.env.insert(placeholders[0].clone(), a.clone());
                                    self.env.insert(placeholders[1].clone(), b.clone());
                                }
                                self.env.insert("_".to_string(), a.clone());
                                let result = self.eval_block_value(&body).unwrap_or(Value::Int(0));
                                self.env = saved;
                                match result {
                                    Value::Int(n) => n.cmp(&0),
                                    Value::Enum {
                                        enum_type, value, ..
                                    } if enum_type == "Order" => value.cmp(&0),
                                    _ => std::cmp::Ordering::Equal,
                                }
                            });
                        }
                    } else {
                        items.sort_by_key(|a| a.to_string_value());
                    }
                    Ok(Value::Array(items))
                }
                other => Ok(other),
            };
        }
        if method == "new"
            && let Value::Package(class_name) = &target
        {
            match class_name.as_str() {
                "Hash" | "Map" => {
                    let mut flat = Vec::new();
                    for arg in &args {
                        flat.extend(Self::value_to_list(arg));
                    }
                    let mut map = HashMap::new();
                    let mut iter = flat.into_iter();
                    while let Some(item) = iter.next() {
                        match item {
                            Value::Pair(k, v) => {
                                map.insert(k, *v);
                            }
                            other => {
                                let key = other.to_string_value();
                                let value = iter.next().unwrap_or(Value::Nil);
                                map.insert(key, value);
                            }
                        }
                    }
                    return Ok(Value::Hash(map));
                }
                "Uni" => {
                    // Uni.new(codepoint, ...) → array of Int codepoints
                    let codepoints: Vec<Value> = args
                        .iter()
                        .map(|a| match a {
                            Value::Int(i) => Value::Int(*i),
                            Value::Num(f) => Value::Int(*f as i64),
                            other => {
                                Value::Int(other.to_string_value().parse::<i64>().unwrap_or(0))
                            }
                        })
                        .collect();
                    return Ok(Value::Array(codepoints));
                }
                "Version" => {
                    let arg = args.first().cloned().unwrap_or(Value::Nil);
                    return Ok(Self::version_from_value(arg));
                }
                "Promise" => return Ok(self.make_promise_instance("Planned", Value::Nil)),
                "Channel" => {
                    let mut attrs = HashMap::new();
                    attrs.insert("queue".to_string(), Value::Array(Vec::new()));
                    attrs.insert("closed".to_string(), Value::Bool(false));
                    return Ok(Value::make_instance(class_name.clone(), attrs));
                }
                "Supply" => return Ok(self.make_supply_instance()),
                "Proc::Async" => {
                    let mut attrs = HashMap::new();
                    attrs.insert("cmd".to_string(), Value::Array(args.clone()));
                    attrs.insert("started".to_string(), Value::Bool(false));
                    attrs.insert("stdout".to_string(), self.make_supply_instance());
                    attrs.insert("stderr".to_string(), self.make_supply_instance());
                    return Ok(Value::make_instance(class_name.clone(), attrs));
                }
                "IO::Path" => {
                    let path = args
                        .first()
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Ok(self.make_io_path_instance(&path));
                }
                "Rat" => {
                    let a = match args.first() {
                        Some(Value::Int(i)) => *i,
                        _ => 0,
                    };
                    let b = match args.get(1) {
                        Some(Value::Int(i)) => *i,
                        _ => 1,
                    };
                    return Ok(make_rat(a, b));
                }
                "FatRat" => {
                    let a = match args.first() {
                        Some(Value::Int(i)) => *i,
                        _ => 0,
                    };
                    let b = match args.get(1) {
                        Some(Value::Int(i)) => *i,
                        _ => 1,
                    };
                    return Ok(Value::FatRat(a, b));
                }
                "Set" => {
                    let mut elems = HashSet::new();
                    for arg in &args {
                        for item in Self::value_to_list(arg) {
                            elems.insert(item.to_string_value());
                        }
                    }
                    return Ok(Value::Set(elems));
                }
                "Bag" => {
                    let mut counts: HashMap<String, i64> = HashMap::new();
                    for arg in &args {
                        for item in Self::value_to_list(arg) {
                            *counts.entry(item.to_string_value()).or_insert(0) += 1;
                        }
                    }
                    return Ok(Value::Bag(counts));
                }
                "Mix" => {
                    let mut weights: HashMap<String, f64> = HashMap::new();
                    for arg in &args {
                        for item in Self::value_to_list(arg) {
                            *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                        }
                    }
                    return Ok(Value::Mix(weights));
                }
                "Complex" => {
                    let re = match args.first() {
                        Some(Value::Int(i)) => *i as f64,
                        Some(Value::Num(f)) => *f,
                        _ => 0.0,
                    };
                    let im = match args.get(1) {
                        Some(Value::Int(i)) => *i as f64,
                        Some(Value::Num(f)) => *f,
                        _ => 0.0,
                    };
                    return Ok(Value::Complex(re, im));
                }
                _ => {}
            }
            if self.classes.contains_key(class_name) {
                let mut attrs = HashMap::new();
                for (attr_name, _is_public, default) in self.collect_class_attributes(class_name) {
                    let val = if let Some(expr) = default {
                        self.eval_block_value(&[Stmt::Expr(expr)])?
                    } else {
                        Value::Nil
                    };
                    attrs.insert(attr_name, val);
                }
                for val in &args {
                    if let Value::Pair(k, v) = val {
                        attrs.insert(k.clone(), *v.clone());
                    }
                }
                if self.class_has_method(class_name, "BUILD") {
                    let (_v, updated) =
                        self.run_instance_method(class_name, attrs, "BUILD", Vec::new())?;
                    attrs = updated;
                }
                if self.class_has_method(class_name, "TWEAK") {
                    let (_v, updated) =
                        self.run_instance_method(class_name, attrs, "TWEAK", Vec::new())?;
                    attrs = updated;
                }
                return Ok(Value::make_instance(class_name.clone(), attrs));
            }
        }
        if method == "new" {
            return match target {
                Value::Package(name) => match name.as_str() {
                    "Set" | "SetHash" => Ok(Value::Set(HashSet::new())),
                    "Bag" | "BagHash" => Ok(Value::Bag(HashMap::new())),
                    "Mix" | "MixHash" => Ok(Value::Mix(HashMap::new())),
                    _ => Err(RuntimeError::new(format!(
                        "Unknown method value dispatch (fallback disabled): {}",
                        method
                    ))),
                },
                Value::Str(_) => Ok(Value::Str(String::new())),
                Value::Int(_) => Ok(Value::Int(0)),
                Value::Num(_) => Ok(Value::Num(0.0)),
                Value::Bool(_) => Ok(Value::Bool(false)),
                _ => Err(RuntimeError::new(format!(
                    "Unknown method value dispatch (fallback disabled): {}",
                    method
                ))),
            };
        }
        if method == "grep" {
            return match target {
                Value::Array(items) => self.eval_grep_over_items(args.first().cloned(), items),
                other => Ok(other),
            };
        }
        if let Value::Enum {
            enum_type,
            key,
            value,
            index,
        } = &target
        {
            match method {
                "key" => return Ok(Value::Str(key.clone())),
                "value" | "Int" | "Numeric" => return Ok(Value::Int(*value)),
                "WHAT" => return Ok(Value::Package(enum_type.clone())),
                "raku" | "perl" => return Ok(Value::Str(format!("{}::{}", enum_type, key))),
                "gist" | "Str" => return Ok(Value::Str(key.clone())),
                "kv" => {
                    return Ok(Value::Array(vec![
                        Value::Str(key.clone()),
                        Value::Int(*value),
                    ]));
                }
                "pair" => return Ok(Value::Pair(key.clone(), Box::new(Value::Int(*value)))),
                "pred" => {
                    if *index == 0 {
                        return Ok(Value::Nil);
                    }
                    if let Some(variants) = self.enum_types.get(enum_type)
                        && let Some((prev_key, prev_val)) = variants.get(index - 1)
                    {
                        return Ok(Value::Enum {
                            enum_type: enum_type.clone(),
                            key: prev_key.clone(),
                            value: *prev_val,
                            index: index - 1,
                        });
                    }
                    return Ok(Value::Nil);
                }
                "succ" => {
                    if let Some(variants) = self.enum_types.get(enum_type)
                        && let Some((next_key, next_val)) = variants.get(index + 1)
                    {
                        return Ok(Value::Enum {
                            enum_type: enum_type.clone(),
                            key: next_key.clone(),
                            value: *next_val,
                            index: index + 1,
                        });
                    }
                    return Ok(Value::Nil);
                }
                _ => {}
            }
        }
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
        {
            if self.is_native_method(class_name, method) {
                return self.call_native_instance_method(class_name, attributes, method, args);
            }
            if method == "isa" {
                let target_name = match args.first().cloned().unwrap_or(Value::Nil) {
                    Value::Package(name) => name,
                    Value::Str(name) => name,
                    Value::Instance { class_name, .. } => class_name,
                    other => other.to_string_value(),
                };
                return Ok(Value::Bool(
                    self.class_mro(class_name).contains(&target_name),
                ));
            }
            if (method == "raku" || method == "perl") && args.is_empty() {
                return Ok(Value::Str(format!("{}.new()", class_name)));
            }
            if method == "name" && args.is_empty() {
                return Ok(attributes.get("name").cloned().unwrap_or(Value::Nil));
            }
            if method == "clone" {
                let mut attrs = attributes.clone();
                for arg in &args {
                    if let Value::Pair(key, boxed) = arg {
                        attrs.insert(key.clone(), *boxed.clone());
                    }
                }
                return Ok(Value::make_instance(class_name.clone(), attrs));
            }
            if args.is_empty() {
                for (attr_name, is_public, _) in self.collect_class_attributes(class_name) {
                    if is_public && attr_name == method {
                        return Ok(attributes.get(method).cloned().unwrap_or(Value::Nil));
                    }
                }
            }
            if self.has_user_method(class_name, method) {
                let (result, _updated) =
                    self.run_instance_method(class_name, attributes.clone(), method, args)?;
                return Ok(result);
            }
        }
        if method == "gist" && args.is_empty() {
            return match target {
                Value::Package(name) => Ok(Value::Str(format!("({})", name))),
                other => Ok(Value::Str(other.to_string_value())),
            };
        }
        if (method == "raku" || method == "perl") && args.is_empty() {
            return match target {
                Value::Package(name) => Ok(Value::Str(format!("({})", name))),
                other => Ok(Value::Str(other.to_string_value())),
            };
        }
        if method == "name" && args.is_empty() {
            return match target {
                Value::Routine { name, .. } => Ok(Value::Str(name)),
                Value::Package(name) => Ok(Value::Str(name)),
                Value::Str(name) => Ok(Value::Str(name)),
                Value::Sub { name, .. } => Ok(Value::Str(name)),
                _ => Ok(Value::Nil),
            };
        }
        if method == "Str" && args.is_empty() {
            return Ok(Value::Str(target.to_string_value()));
        }
        Err(RuntimeError::new(format!(
            "Unknown method value dispatch (fallback disabled): {}",
            method
        )))
    }
}
