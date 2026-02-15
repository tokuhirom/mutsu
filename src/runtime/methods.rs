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
        if method == "key"
            && args.is_empty()
            && let Value::Pair(k, _) = &target
        {
            return Ok(Value::Str(k.clone()));
        }
        if method == "Slip" && args.is_empty() {
            return match target {
                Value::Array(items) => Ok(Value::Slip(items)),
                Value::Slip(_) => Ok(target),
                other => Ok(Value::Slip(vec![other])),
            };
        }
        if method == "sink" && args.is_empty() {
            return Ok(Value::Nil);
        }
        if (method == "list" || method == "Array") && args.is_empty() {
            return match target {
                Value::Range(a, b) => Ok(Value::Array((a..=b).map(Value::Int).collect())),
                Value::RangeExcl(a, b) => Ok(Value::Array((a..b).map(Value::Int).collect())),
                Value::RangeExclStart(a, b) => {
                    Ok(Value::Array((a + 1..=b).map(Value::Int).collect()))
                }
                Value::RangeExclBoth(a, b) => {
                    Ok(Value::Array((a + 1..b).map(Value::Int).collect()))
                }
                Value::Array(items) => Ok(Value::Array(items)),
                other => Ok(Value::Array(vec![other])),
            };
        }
        if method == "Range" && args.is_empty() {
            return match target {
                Value::Array(items) => Ok(Value::RangeExcl(0, items.len() as i64)),
                Value::Str(s) => Ok(Value::RangeExcl(0, s.chars().count() as i64)),
                other @ (Value::Range(_, _)
                | Value::RangeExcl(_, _)
                | Value::RangeExclStart(_, _)
                | Value::RangeExclBoth(_, _)) => Ok(other),
                _ => Ok(Value::Nil),
            };
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
        if method == "Set" && args.is_empty() {
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
        if method == "Bag" && args.is_empty() {
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
        if method == "Mix" && args.is_empty() {
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
        if method == "abs"
            && args.is_empty()
            && let Value::Complex(r, i) = target
        {
            return Ok(Value::Num((r * r + i * i).sqrt()));
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
        if method == "squish" {
            return match target {
                Value::Array(items) => {
                    let mut result = Vec::new();
                    let mut last: Option<String> = None;
                    for item in items {
                        let s = item.to_string_value();
                        if last.as_ref() != Some(&s) {
                            last = Some(s);
                            result.push(item);
                        }
                    }
                    Ok(Value::Array(result))
                }
                other => Ok(other),
            };
        }
        if method == "value"
            && args.is_empty()
            && let Value::Pair(_, v) = target
        {
            return Ok(*v);
        }
        if method == "re" && args.is_empty() {
            return match target {
                Value::Complex(r, _) => Ok(Value::Num(r)),
                Value::Int(i) => Ok(Value::Num(i as f64)),
                Value::Num(f) => Ok(Value::Num(f)),
                _ => Ok(Value::Num(0.0)),
            };
        }
        if method == "nude" && args.is_empty() {
            return match target {
                Value::Rat(n, d) => Ok(Value::Array(vec![Value::Int(n), Value::Int(d)])),
                Value::FatRat(n, d) => Ok(Value::Array(vec![Value::Int(n), Value::Int(d)])),
                Value::Int(i) => Ok(Value::Array(vec![Value::Int(i), Value::Int(1)])),
                _ => Ok(Value::Array(vec![Value::Int(0), Value::Int(1)])),
            };
        }
        if method == "is-prime" && args.is_empty() {
            return match target {
                Value::Int(n) => {
                    let n = n.abs();
                    let prime = if n < 2 {
                        false
                    } else if n < 4 {
                        true
                    } else if n % 2 == 0 || n % 3 == 0 {
                        false
                    } else {
                        let mut i = 5i64;
                        let mut result = true;
                        while i * i <= n {
                            if n % i == 0 || n % (i + 2) == 0 {
                                result = false;
                                break;
                            }
                            i += 6;
                        }
                        result
                    };
                    Ok(Value::Bool(prime))
                }
                _ => Ok(Value::Bool(false)),
            };
        }
        if method == "elems" && args.is_empty() {
            return match target {
                Value::Array(items) => Ok(Value::Int(items.len() as i64)),
                Value::Hash(items) => Ok(Value::Int(items.len() as i64)),
                Value::Set(s) => Ok(Value::Int(s.len() as i64)),
                Value::Bag(b) => Ok(Value::Int(b.len() as i64)),
                Value::Mix(m) => Ok(Value::Int(m.len() as i64)),
                Value::Junction { values, .. } => Ok(Value::Int(values.len() as i64)),
                _ => Ok(Value::Int(1)),
            };
        }
        if method == "total" && args.is_empty() {
            match target {
                Value::Set(s) => return Ok(Value::Int(s.len() as i64)),
                Value::Bag(b) => return Ok(Value::Int(b.values().sum::<i64>())),
                Value::Mix(m) => return Ok(Value::Num(m.values().sum::<f64>())),
                _ => {}
            }
        }
        if method == "values" && args.is_empty() {
            return match target {
                Value::Hash(items) => Ok(Value::Array(items.values().cloned().collect())),
                Value::Set(s) => Ok(Value::Array(s.iter().map(|_| Value::Bool(true)).collect())),
                Value::Bag(b) => Ok(Value::Array(b.values().map(|v| Value::Int(*v)).collect())),
                Value::Mix(m) => Ok(Value::Array(m.values().map(|v| Value::Num(*v)).collect())),
                _ => Ok(Value::Array(Vec::new())),
            };
        }
        if method == "keys" && args.is_empty() {
            return match target {
                Value::Hash(items) => Ok(Value::Array(
                    items.keys().map(|k| Value::Str(k.clone())).collect(),
                )),
                Value::Set(s) => Ok(Value::Array(
                    s.iter().map(|k| Value::Str(k.clone())).collect(),
                )),
                Value::Bag(b) => Ok(Value::Array(
                    b.keys().map(|k| Value::Str(k.clone())).collect(),
                )),
                Value::Mix(m) => Ok(Value::Array(
                    m.keys().map(|k| Value::Str(k.clone())).collect(),
                )),
                _ => Ok(Value::Array(Vec::new())),
            };
        }
        if method == "kv" && args.is_empty() {
            return match target {
                Value::Hash(items) => {
                    let mut kv = Vec::new();
                    for (k, v) in &items {
                        kv.push(Value::Str(k.clone()));
                        kv.push(v.clone());
                    }
                    Ok(Value::Array(kv))
                }
                Value::Set(s) => {
                    let mut kv = Vec::new();
                    for k in &s {
                        kv.push(Value::Str(k.clone()));
                        kv.push(Value::Bool(true));
                    }
                    Ok(Value::Array(kv))
                }
                Value::Bag(b) => {
                    let mut kv = Vec::new();
                    for (k, v) in &b {
                        kv.push(Value::Str(k.clone()));
                        kv.push(Value::Int(*v));
                    }
                    Ok(Value::Array(kv))
                }
                Value::Mix(m) => {
                    let mut kv = Vec::new();
                    for (k, v) in &m {
                        kv.push(Value::Str(k.clone()));
                        kv.push(Value::Num(*v));
                    }
                    Ok(Value::Array(kv))
                }
                Value::Enum { key, value, .. } => {
                    Ok(Value::Array(vec![Value::Str(key), Value::Int(value)]))
                }
                _ => Ok(Value::Array(Vec::new())),
            };
        }
        if method == "pairs" && args.is_empty() {
            return match target {
                Value::Hash(items) => Ok(Value::Array(
                    items
                        .iter()
                        .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                        .collect(),
                )),
                Value::Set(s) => Ok(Value::Array(
                    s.iter()
                        .map(|k| Value::Pair(k.clone(), Box::new(Value::Bool(true))))
                        .collect(),
                )),
                Value::Bag(b) => Ok(Value::Array(
                    b.iter()
                        .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Int(*v))))
                        .collect(),
                )),
                Value::Mix(m) => Ok(Value::Array(
                    m.iter()
                        .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Num(*v))))
                        .collect(),
                )),
                _ => Ok(Value::Array(Vec::new())),
            };
        }
        if method == "numerator" && args.is_empty() {
            return match target {
                Value::Rat(n, _) => Ok(Value::Int(n)),
                Value::FatRat(n, _) => Ok(Value::Int(n)),
                Value::Int(i) => Ok(Value::Int(i)),
                _ => Ok(Value::Int(0)),
            };
        }
        if method == "denominator" && args.is_empty() {
            return match target {
                Value::Rat(_, d) => Ok(Value::Int(d)),
                Value::FatRat(_, d) => Ok(Value::Int(d)),
                Value::Int(_) => Ok(Value::Int(1)),
                _ => Ok(Value::Int(1)),
            };
        }
        if method == "isNaN" && args.is_empty() {
            return match target {
                Value::Rat(0, 0) => Ok(Value::Bool(true)),
                Value::Num(f) => Ok(Value::Bool(f.is_nan())),
                _ => Ok(Value::Bool(false)),
            };
        }
        if method == "minmax" && args.is_empty() {
            return match target {
                Value::Array(items) if !items.is_empty() => {
                    let mut min = &items[0];
                    let mut max = &items[0];
                    for item in &items[1..] {
                        if super::compare_values(item, min) < 0 {
                            min = item;
                        }
                        if super::compare_values(item, max) > 0 {
                            max = item;
                        }
                    }
                    Ok(Value::Range(super::to_int(min), super::to_int(max)))
                }
                _ => Ok(Value::Nil),
            };
        }
        if method == "grep" {
            return match target {
                Value::Array(items) => self.eval_grep_over_items(args.first().cloned(), items),
                other => Ok(other),
            };
        }
        if method == "VAR" && args.is_empty() {
            return Ok(target);
        }
        if method == "im" && args.is_empty() {
            return match target {
                Value::Complex(_, i) => Ok(Value::Num(i)),
                _ => Ok(Value::Num(0.0)),
            };
        }
        if method == "conj" && args.is_empty() {
            return match target {
                Value::Complex(r, i) => Ok(Value::Complex(r, -i)),
                Value::Int(i) => Ok(Value::Complex(i as f64, 0.0)),
                Value::Num(f) => Ok(Value::Complex(f, 0.0)),
                _ => Ok(Value::Complex(0.0, 0.0)),
            };
        }
        if method == "reals" && args.is_empty() {
            return match target {
                Value::Complex(r, i) => Ok(Value::Array(vec![Value::Num(r), Value::Num(i)])),
                other => Ok(Value::Array(vec![other, Value::Num(0.0)])),
            };
        }
        if method == "Complex" && args.is_empty() {
            return match target {
                Value::Complex(_, _) => Ok(target),
                Value::Int(i) => Ok(Value::Complex(i as f64, 0.0)),
                Value::Num(f) => Ok(Value::Complex(f, 0.0)),
                _ => Ok(Value::Complex(0.0, 0.0)),
            };
        }
        if method == "Int"
            && args.is_empty()
            && let Value::Complex(r, _) = target
        {
            return Ok(Value::Int(r as i64));
        }
        if (method == "Num" || method == "Numeric")
            && args.is_empty()
            && let Value::Complex(r, _) = target
        {
            return Ok(Value::Num(r));
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
            if class_name == "IO::Path" {
                let p = attributes
                    .get("path")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let path_buf = self.resolve_path(&p);
                let cwd_path = self.get_cwd_path();
                let original = Path::new(&p);
                match method {
                    "Str" | "gist" => return Ok(Value::Str(p.clone())),
                    "IO" => return Ok(target.clone()),
                    "basename" => {
                        let bname = original
                            .file_name()
                            .map(|s| s.to_string_lossy().to_string())
                            .unwrap_or_default();
                        return Ok(Value::Str(bname));
                    }
                    "parent" => {
                        let mut levels = 1i64;
                        if let Some(Value::Int(i)) = args.first() {
                            levels = (*i).max(1);
                        }
                        let mut path = p.clone();
                        for _ in 0..levels {
                            if let Some(par) = Path::new(&path).parent() {
                                let s = par.to_string_lossy().to_string();
                                if s.is_empty() {
                                    path = ".".to_string();
                                    break;
                                }
                                path = s;
                            } else {
                                path = ".".to_string();
                                break;
                            }
                        }
                        return Ok(self.make_io_path_instance(&path));
                    }
                    "child" | "add" => {
                        let child_name = args
                            .first()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let joined = Self::stringify_path(&original.join(&child_name));
                        return Ok(self.make_io_path_instance(&joined));
                    }
                    "extension" => {
                        let ext = original
                            .extension()
                            .map(|s| s.to_string_lossy().to_string())
                            .unwrap_or_default();
                        return Ok(Value::Str(ext));
                    }
                    "absolute" => {
                        let absolute = Self::stringify_path(&path_buf);
                        return Ok(self.make_io_path_instance(&absolute));
                    }
                    "relative" => {
                        let rel = path_buf
                            .strip_prefix(&cwd_path)
                            .map(Self::stringify_path)
                            .unwrap_or_else(|_| Self::stringify_path(&path_buf));
                        return Ok(Value::Str(rel));
                    }
                    "resolve" => {
                        let canonical = fs::canonicalize(&path_buf).map_err(|err| {
                            RuntimeError::new(format!("Failed to resolve '{}': {}", p, err))
                        })?;
                        let resolved = Self::stringify_path(&canonical);
                        return Ok(self.make_io_path_instance(&resolved));
                    }
                    "volume" => {
                        let volume = path_buf
                            .components()
                            .next()
                            .map(|comp| comp.as_os_str().to_string_lossy().to_string())
                            .unwrap_or_default();
                        return Ok(Value::Str(volume));
                    }
                    "is-absolute" => return Ok(Value::Bool(original.is_absolute())),
                    "is-relative" => return Ok(Value::Bool(!original.is_absolute())),
                    "e" => return Ok(Value::Bool(path_buf.exists())),
                    "f" => return Ok(Value::Bool(path_buf.is_file())),
                    "d" => return Ok(Value::Bool(path_buf.is_dir())),
                    "l" => {
                        let linked = fs::symlink_metadata(&path_buf)
                            .map(|meta| meta.file_type().is_symlink())
                            .unwrap_or(false);
                        return Ok(Value::Bool(linked));
                    }
                    "r" => return Ok(Value::Bool(fs::metadata(&path_buf).is_ok())),
                    "w" => {
                        let writable = fs::metadata(&path_buf)
                            .map(|meta| !meta.permissions().readonly())
                            .unwrap_or(false);
                        return Ok(Value::Bool(writable));
                    }
                    "x" => {
                        let executable = fs::metadata(&path_buf)
                            .map(|meta| Self::metadata_is_executable(&meta))
                            .unwrap_or(false);
                        return Ok(Value::Bool(executable));
                    }
                    "s" => {
                        let size =
                            fs::metadata(&path_buf)
                                .map(|meta| meta.len())
                                .map_err(|err| {
                                    RuntimeError::new(format!("Failed to stat '{}': {}", p, err))
                                })?;
                        return Ok(Value::Int(size as i64));
                    }
                    "z" => {
                        let zero = fs::metadata(&path_buf)
                            .map(|meta| meta.len() == 0)
                            .unwrap_or(false);
                        return Ok(Value::Bool(zero));
                    }
                    "modified" => {
                        let ts = fs::metadata(&path_buf)
                            .and_then(|meta| meta.modified())
                            .map(Self::system_time_to_int)
                            .map_err(|err| {
                                RuntimeError::new(format!(
                                    "Failed to get modified time '{}': {}",
                                    p, err
                                ))
                            })?;
                        return Ok(Value::Int(ts));
                    }
                    "accessed" => {
                        let ts = fs::metadata(&path_buf)
                            .and_then(|meta| meta.accessed())
                            .map(Self::system_time_to_int)
                            .map_err(|err| {
                                RuntimeError::new(format!(
                                    "Failed to get accessed time '{}': {}",
                                    p, err
                                ))
                            })?;
                        return Ok(Value::Int(ts));
                    }
                    "changed" => {
                        let ts = fs::metadata(&path_buf)
                            .and_then(|meta| meta.modified())
                            .map(Self::system_time_to_int)
                            .map_err(|err| {
                                RuntimeError::new(format!(
                                    "Failed to get changed time '{}': {}",
                                    p, err
                                ))
                            })?;
                        return Ok(Value::Int(ts));
                    }
                    "lines" => {
                        let content = fs::read_to_string(&path_buf).map_err(|err| {
                            RuntimeError::new(format!("Failed to read '{}': {}", p, err))
                        })?;
                        let parts = content
                            .lines()
                            .map(|line| Value::Str(line.to_string()))
                            .collect();
                        return Ok(Value::Array(parts));
                    }
                    "words" => {
                        let content = fs::read_to_string(&path_buf).map_err(|err| {
                            RuntimeError::new(format!("Failed to read '{}': {}", p, err))
                        })?;
                        let parts = content
                            .split_whitespace()
                            .map(|token| Value::Str(token.to_string()))
                            .collect();
                        return Ok(Value::Array(parts));
                    }
                    "slurp" => {
                        let content = fs::read_to_string(&path_buf).map_err(|err| {
                            RuntimeError::new(format!("Failed to slurp '{}': {}", p, err))
                        })?;
                        return Ok(Value::Str(content));
                    }
                    "open" => {
                        let (read, write, append) = Self::parse_io_flags_values(&args);
                        return self.open_file_handle(&path_buf, read, write, append);
                    }
                    "copy" => {
                        let dest = args
                            .first()
                            .map(|v| v.to_string_value())
                            .ok_or_else(|| RuntimeError::new("copy requires destination"))?;
                        let dest_buf = self.resolve_path(&dest);
                        fs::copy(&path_buf, &dest_buf).map_err(|err| {
                            RuntimeError::new(format!("Failed to copy '{}': {}", p, err))
                        })?;
                        return Ok(Value::Bool(true));
                    }
                    "rename" | "move" => {
                        let dest = args
                            .first()
                            .map(|v| v.to_string_value())
                            .ok_or_else(|| RuntimeError::new("rename/move requires destination"))?;
                        let dest_buf = self.resolve_path(&dest);
                        fs::rename(&path_buf, &dest_buf).map_err(|err| {
                            RuntimeError::new(format!("Failed to rename '{}': {}", p, err))
                        })?;
                        return Ok(Value::Bool(true));
                    }
                    "chmod" => {
                        let mode_value = args
                            .first()
                            .cloned()
                            .ok_or_else(|| RuntimeError::new("chmod requires mode"))?;
                        let mode_int = match mode_value {
                            Value::Int(i) => i as u32,
                            Value::Str(s) => u32::from_str_radix(&s, 8).unwrap_or(0),
                            other => {
                                return Err(RuntimeError::new(format!(
                                    "Invalid mode: {}",
                                    other.to_string_value()
                                )));
                            }
                        };
                        #[cfg(unix)]
                        {
                            let perms = PermissionsExt::from_mode(mode_int);
                            fs::set_permissions(&path_buf, perms).map_err(|err| {
                                RuntimeError::new(format!("Failed to chmod '{}': {}", p, err))
                            })?;
                        }
                        #[cfg(not(unix))]
                        {
                            return Err(RuntimeError::new("chmod not supported on this platform"));
                        }
                        return Ok(Value::Bool(true));
                    }
                    "mkdir" => {
                        fs::create_dir_all(&path_buf).map_err(|err| {
                            RuntimeError::new(format!("Failed to mkdir '{}': {}", p, err))
                        })?;
                        return Ok(Value::Bool(true));
                    }
                    "rmdir" => {
                        fs::remove_dir(&path_buf).map_err(|err| {
                            RuntimeError::new(format!("Failed to rmdir '{}': {}", p, err))
                        })?;
                        return Ok(Value::Bool(true));
                    }
                    "dir" => {
                        let mut entries = Vec::new();
                        for entry in fs::read_dir(&path_buf).map_err(|err| {
                            RuntimeError::new(format!("Failed to read dir '{}': {}", p, err))
                        })? {
                            let entry = entry.map_err(|err| {
                                RuntimeError::new(format!(
                                    "Failed to read dir entry '{}': {}",
                                    p, err
                                ))
                            })?;
                            entries.push(Value::Str(entry.path().to_string_lossy().to_string()));
                        }
                        return Ok(Value::Array(entries));
                    }
                    "spurt" => {
                        let content = args
                            .first()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        fs::write(&path_buf, &content).map_err(|err| {
                            RuntimeError::new(format!("Failed to spurt '{}': {}", p, err))
                        })?;
                        return Ok(Value::Bool(true));
                    }
                    "unlink" => {
                        fs::remove_file(&path_buf).map_err(|err| {
                            RuntimeError::new(format!("Failed to unlink '{}': {}", p, err))
                        })?;
                        return Ok(Value::Bool(true));
                    }
                    _ => {}
                }
            }
            if class_name == "IO::Handle" {
                match method {
                    "close" => return Ok(Value::Bool(self.close_handle_value(&target)?)),
                    "get" => {
                        let line = self.read_line_from_handle_value(&target)?;
                        if line.is_empty() {
                            return Ok(Value::Nil);
                        }
                        return Ok(Value::Str(line));
                    }
                    "getc" => {
                        let bytes = self.read_bytes_from_handle_value(&target, 1)?;
                        return Ok(Value::Str(String::from_utf8_lossy(&bytes).to_string()));
                    }
                    "lines" => {
                        let mut lines = Vec::new();
                        loop {
                            let line = self.read_line_from_handle_value(&target)?;
                            if line.is_empty() {
                                break;
                            }
                            lines.push(Value::Str(line));
                        }
                        return Ok(Value::Array(lines));
                    }
                    "words" => {
                        let mut words = Vec::new();
                        loop {
                            let line = self.read_line_from_handle_value(&target)?;
                            if line.is_empty() {
                                break;
                            }
                            for token in line.split_whitespace() {
                                words.push(Value::Str(token.to_string()));
                            }
                        }
                        return Ok(Value::Array(words));
                    }
                    "read" => {
                        let count = args
                            .first()
                            .and_then(|v| match v {
                                Value::Int(i) if *i > 0 => Some(*i as usize),
                                _ => None,
                            })
                            .unwrap_or(0);
                        if count > 0 {
                            let bytes = self.read_bytes_from_handle_value(&target, count)?;
                            return Ok(Value::Str(String::from_utf8_lossy(&bytes).to_string()));
                        }
                        let path = {
                            let state = self.handle_state_mut(&target)?;
                            state.path.clone()
                        };
                        if let Some(path) = path {
                            let content = fs::read_to_string(&path).map_err(|err| {
                                RuntimeError::new(format!("Failed to read '{}': {}", path, err))
                            })?;
                            return Ok(Value::Str(content));
                        }
                        return Ok(Value::Str(String::new()));
                    }
                    "write" | "print" => {
                        let content = args
                            .first()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        self.write_to_handle_value(&target, &content, false)?;
                        return Ok(Value::Bool(true));
                    }
                    "say" => {
                        let content = args
                            .first()
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        self.write_to_handle_value(&target, &content, true)?;
                        return Ok(Value::Bool(true));
                    }
                    "flush" => {
                        if let Ok(state) = self.handle_state_mut(&target)
                            && let Some(file) = state.file.as_mut()
                        {
                            file.flush().map_err(|err| {
                                RuntimeError::new(format!("Failed to flush handle: {}", err))
                            })?;
                        }
                        return Ok(Value::Bool(true));
                    }
                    "seek" => {
                        let pos = args
                            .first()
                            .and_then(|v| match v {
                                Value::Int(i) => Some(*i),
                                _ => None,
                            })
                            .unwrap_or(0);
                        let offset = self.seek_handle_value(&target, pos)?;
                        return Ok(Value::Int(offset));
                    }
                    "tell" => {
                        let position = self.tell_handle_value(&target)?;
                        return Ok(Value::Int(position));
                    }
                    "eof" => {
                        let at_end = self.handle_eof_value(&target)?;
                        return Ok(Value::Bool(at_end));
                    }
                    "encoding" => {
                        if let Some(arg) = args.first() {
                            let encoding = arg.to_string_value();
                            let prev = self.set_handle_encoding(&target, Some(encoding.clone()))?;
                            return Ok(Value::Str(prev));
                        }
                        let current = self.set_handle_encoding(&target, None)?;
                        return Ok(Value::Str(current));
                    }
                    "opened" => {
                        let state = self.handle_state_mut(&target)?;
                        return Ok(Value::Bool(!state.closed));
                    }
                    "slurp" => {
                        let path = {
                            let state = self.handle_state_mut(&target)?;
                            state.path.clone()
                        };
                        if let Some(path) = path {
                            let content = fs::read_to_string(&path).map_err(|err| {
                                RuntimeError::new(format!("Failed to slurp '{}': {}", path, err))
                            })?;
                            return Ok(Value::Str(content));
                        }
                        return Ok(Value::Str(String::new()));
                    }
                    _ => {}
                }
            }
            if class_name == "Distro" {
                match method {
                    "name" | "auth" | "desc" | "release" | "path-sep" | "is-win" | "version"
                    | "signature" => {
                        return Ok(attributes.get(method).cloned().unwrap_or(Value::Nil));
                    }
                    "gist" => {
                        let n = attributes
                            .get("name")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let v = attributes
                            .get("version")
                            .map(|v| {
                                if let Value::Version { parts, .. } = v {
                                    Value::version_parts_to_string(parts)
                                } else {
                                    v.to_string_value()
                                }
                            })
                            .unwrap_or_default();
                        return Ok(Value::Str(format!("{} ({})", n, v)));
                    }
                    "Str" => {
                        let n = attributes
                            .get("name")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        return Ok(Value::Str(n));
                    }
                    "raku" | "perl" => {
                        let release = attributes
                            .get("release")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let path_sep = attributes
                            .get("path-sep")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let n = attributes
                            .get("name")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let auth = attributes
                            .get("auth")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let ver = attributes
                            .get("version")
                            .map(|v| {
                                if let Value::Version { parts, .. } = v {
                                    format!("v{}", Value::version_parts_to_string(parts))
                                } else {
                                    v.to_string_value()
                                }
                            })
                            .unwrap_or_default();
                        let desc = attributes
                            .get("desc")
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        return Ok(Value::Str(format!(
                            "Distro.new(release => \"{}\", path-sep => \"{}\", name => \"{}\", auth => \"{}\", version => {}, signature => Blob, desc => \"{}\")",
                            release, path_sep, n, auth, ver, desc
                        )));
                    }
                    _ => {}
                }
            }
            if class_name == "Perl"
                && let Some(val) = attributes.get(method)
            {
                return Ok(val.clone());
            }
            if class_name == "Promise" {
                if method == "result" && args.is_empty() {
                    return Ok(attributes.get("result").cloned().unwrap_or(Value::Nil));
                }
                if method == "status" && args.is_empty() {
                    return Ok(attributes
                        .get("status")
                        .cloned()
                        .unwrap_or(Value::Str("Planned".to_string())));
                }
                if method == "then" {
                    let block = args.first().cloned().unwrap_or(Value::Nil);
                    let status = attributes
                        .get("status")
                        .cloned()
                        .unwrap_or(Value::Str("Planned".to_string()));
                    if matches!(status, Value::Str(ref s) if s == "Kept") {
                        let value = attributes.get("result").cloned().unwrap_or(Value::Nil);
                        let result = self.call_sub_value(block, vec![value], false)?;
                        return Ok(self.make_promise_instance("Kept", result));
                    }
                    return Ok(self.make_promise_instance("Planned", Value::Nil));
                }
            }
            if class_name == "Channel" && method == "closed" && args.is_empty() {
                return Ok(attributes
                    .get("closed")
                    .cloned()
                    .unwrap_or(Value::Bool(false)));
            }
            if class_name == "Proc::Async" {
                if method == "command" && args.is_empty() {
                    return Ok(attributes
                        .get("cmd")
                        .cloned()
                        .unwrap_or(Value::Array(Vec::new())));
                }
                if method == "started" && args.is_empty() {
                    return Ok(attributes
                        .get("started")
                        .cloned()
                        .unwrap_or(Value::Bool(false)));
                }
                if method == "stdout" && args.is_empty() {
                    return Ok(attributes.get("stdout").cloned().unwrap_or(Value::Nil));
                }
                if method == "stderr" && args.is_empty() {
                    return Ok(attributes.get("stderr").cloned().unwrap_or(Value::Nil));
                }
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
            if self.class_has_method(class_name, method) {
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
