use super::*;

impl Interpreter {
    pub(super) fn dispatch_to_set(&self, target: Value) -> Result<Value, RuntimeError> {
        let mut elems = HashSet::new();
        match target {
            Value::Set(_, _) => return Ok(target),
            Value::Array(items, ..) => {
                for item in items.iter() {
                    match item {
                        Value::Pair(k, v) => {
                            if v.truthy() {
                                elems.insert(k.clone());
                            }
                        }
                        Value::ValuePair(k, v) => {
                            if v.truthy() {
                                elems.insert(k.to_string_value());
                            }
                        }
                        _ => {
                            elems.insert(item.to_string_value());
                        }
                    }
                }
            }
            Value::Hash(items) => {
                for (k, v) in items.iter() {
                    if v.truthy() {
                        elems.insert(k.clone());
                    }
                }
            }
            Value::Bag(b, _) => {
                for k in b.keys() {
                    elems.insert(k.clone());
                }
            }
            Value::Mix(m, _) => {
                for k in m.keys() {
                    elems.insert(k.clone());
                }
            }
            Value::Pair(k, v) => {
                if v.truthy() {
                    elems.insert(k);
                }
            }
            Value::ValuePair(k, v) => {
                if v.truthy() {
                    elems.insert(k.to_string_value());
                }
            }
            other if other.is_range() => {
                for item in Self::value_to_list(&other) {
                    elems.insert(item.to_string_value());
                }
            }
            other => {
                elems.insert(other.to_string_value());
            }
        }
        Ok(Value::set(elems))
    }

    pub(super) fn dispatch_to_bag(&self, target: Value) -> Result<Value, RuntimeError> {
        let mut counts: HashMap<String, i64> = HashMap::new();
        let mut original_keys: HashMap<String, Value> = HashMap::new();
        let pair_weight = |v: &Value| -> i64 {
            match v {
                Value::Int(i) => *i,
                Value::Num(n) => *n as i64,
                Value::Rat(n, d) if *d != 0 => n / d,
                Value::FatRat(n, d) if *d != 0 => n / d,
                Value::Bool(b) => i64::from(*b),
                _ => i64::from(v.truthy()),
            }
        };
        let add_pair_key = |map: &mut HashMap<String, i64>, key: String, weight: i64| {
            if weight > 0 {
                // Pair value is the weight/count for the Bag entry
                *map.entry(key).or_insert(0) += weight;
            }
        };
        let mut has_non_str_keys = false;
        match target {
            Value::Bag(_, _) => return Ok(target),
            Value::Array(items, ..) => {
                for item in items.iter() {
                    match item {
                        Value::Pair(k, v) => {
                            add_pair_key(&mut counts, k.clone(), pair_weight(v));
                        }
                        Value::ValuePair(k, v) => {
                            let str_key = k.to_string_value();
                            if !matches!(k.as_ref(), Value::Str(_)) {
                                has_non_str_keys = true;
                                original_keys
                                    .entry(str_key.clone())
                                    .or_insert_with(|| k.as_ref().clone());
                            }
                            add_pair_key(&mut counts, str_key, pair_weight(v));
                        }
                        _ => {
                            let str_key = item.to_string_value();
                            if !matches!(item, Value::Str(_)) {
                                has_non_str_keys = true;
                                original_keys
                                    .entry(str_key.clone())
                                    .or_insert_with(|| item.clone());
                            }
                            *counts.entry(str_key).or_insert(0) += 1;
                        }
                    }
                }
            }
            Value::Set(s, _) => {
                for k in s.iter() {
                    counts.insert(k.clone(), 1);
                }
            }
            Value::Mix(m, _) => {
                for (k, v) in m.iter() {
                    counts.insert(k.clone(), *v as i64);
                }
            }
            Value::Hash(h) => {
                for (k, v) in h.iter() {
                    add_pair_key(&mut counts, k.clone(), pair_weight(v));
                }
            }
            Value::Pair(k, v) => {
                add_pair_key(&mut counts, k, pair_weight(&v));
            }
            Value::ValuePair(k, v) => {
                add_pair_key(&mut counts, k.to_string_value(), pair_weight(&v));
            }
            other if other.is_range() => {
                for item in Self::value_to_list(&other) {
                    let str_key = item.to_string_value();
                    if !matches!(item, Value::Str(_)) {
                        has_non_str_keys = true;
                        original_keys
                            .entry(str_key.clone())
                            .or_insert_with(|| item.clone());
                    }
                    *counts.entry(str_key).or_insert(0) += 1;
                }
            }
            other => {
                let str_key = other.to_string_value();
                if !matches!(other, Value::Str(_)) {
                    has_non_str_keys = true;
                    original_keys
                        .entry(str_key.clone())
                        .or_insert(other.clone());
                }
                counts.insert(str_key, 1);
            }
        }
        if has_non_str_keys {
            Ok(Value::bag_typed(counts, original_keys))
        } else {
            Ok(Value::bag(counts))
        }
    }

    pub(super) fn mix_pair_weight(v: &Value) -> Result<f64, RuntimeError> {
        match v {
            Value::Int(i) => Ok(*i as f64),
            Value::Num(n) => {
                if n.is_infinite() {
                    let mut err = RuntimeError::new(format!(
                        "Value out of range. Is: {}, should be in -Inf^..^Inf",
                        if *n > 0.0 { "Inf" } else { "-Inf" }
                    ));
                    err.exception = Some(Box::new(Value::make_instance(
                        crate::symbol::Symbol::intern("X::OutOfRange"),
                        [
                            ("what".to_string(), Value::str_from("Value")),
                            ("got".to_string(), Value::Num(*n)),
                            ("range".to_string(), Value::str_from("-Inf^..^Inf")),
                        ]
                        .into_iter()
                        .collect(),
                    )));
                    Err(err)
                } else if n.is_nan() {
                    let mut err =
                        RuntimeError::new("Value out of range. Is: NaN, should be in -Inf^..^Inf");
                    err.exception = Some(Box::new(Value::make_instance(
                        crate::symbol::Symbol::intern("X::OutOfRange"),
                        [
                            ("what".to_string(), Value::str_from("Value")),
                            ("got".to_string(), Value::Num(*n)),
                            ("range".to_string(), Value::str_from("-Inf^..^Inf")),
                        ]
                        .into_iter()
                        .collect(),
                    )));
                    Err(err)
                } else {
                    Ok(*n)
                }
            }
            Value::Rat(n, d) if *d != 0 => Ok(*n as f64 / *d as f64),
            Value::Bool(b) => Ok(if *b { 1.0 } else { 0.0 }),
            Value::Complex(_, _) => {
                let mut err = RuntimeError::new(
                    "Cannot convert Complex to Real; use .re or .im to extract components",
                );
                err.exception = Some(Box::new(Value::make_instance(
                    crate::symbol::Symbol::intern("X::Numeric::Real"),
                    [
                        ("source".to_string(), v.clone()),
                        (
                            "reason".to_string(),
                            Value::str_from("Complex to Real conversion"),
                        ),
                        ("target".to_string(), Value::str_from("Real")),
                    ]
                    .into_iter()
                    .collect(),
                )));
                Err(err)
            }
            Value::Str(s) => {
                // Try to parse the string as a number
                if let Ok(n) = s.parse::<f64>() {
                    if n.is_infinite() || n.is_nan() {
                        let mut err = RuntimeError::new(format!(
                            "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}' (Str)",
                            s
                        ));
                        err.exception = Some(Box::new(Value::make_instance(
                            crate::symbol::Symbol::intern("X::Str::Numeric"),
                            [
                                ("source".to_string(), Value::str(s.to_string())),
                                (
                                    "reason".to_string(),
                                    Value::str_from(
                                        "base-10 number must begin with valid digits or '.'",
                                    ),
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        )));
                        return Err(err);
                    }
                    Ok(n)
                } else {
                    let mut err = RuntimeError::new(format!(
                        "Cannot convert string to number: base-10 number must begin with valid digits or '.' in '{}' (Str)",
                        s
                    ));
                    err.exception = Some(Box::new(Value::make_instance(
                        crate::symbol::Symbol::intern("X::Str::Numeric"),
                        [
                            ("source".to_string(), Value::str(s.to_string())),
                            (
                                "reason".to_string(),
                                Value::str_from(
                                    "base-10 number must begin with valid digits or '.'",
                                ),
                            ),
                        ]
                        .into_iter()
                        .collect(),
                    )));
                    Err(err)
                }
            }
            _ => {
                if v.truthy() {
                    Ok(1.0)
                } else {
                    Ok(0.0)
                }
            }
        }
    }

    fn mix_add_item(weights: &mut HashMap<String, f64>, item: &Value) -> Result<(), RuntimeError> {
        match item {
            Value::Pair(k, v) => {
                let w = Self::mix_pair_weight(v)?;
                *weights.entry(k.clone()).or_insert(0.0) += w;
            }
            Value::ValuePair(k, v) => {
                let w = Self::mix_pair_weight(v)?;
                *weights.entry(k.to_string_value()).or_insert(0.0) += w;
            }
            Value::Hash(h) => {
                for (k, v) in h.iter() {
                    let w = Self::mix_pair_weight(v)?;
                    if w != 0.0 {
                        *weights.entry(k.clone()).or_insert(0.0) += w;
                    }
                }
            }
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                for sub_item in items.iter() {
                    Self::mix_add_item(weights, sub_item)?;
                }
            }
            Value::Set(s, _) => {
                for k in s.iter() {
                    *weights.entry(k.clone()).or_insert(0.0) += 1.0;
                }
            }
            Value::Bag(b, _) => {
                for (k, v) in b.iter() {
                    *weights.entry(k.clone()).or_insert(0.0) += *v as f64;
                }
            }
            Value::Mix(m, _) => {
                for (k, v) in m.iter() {
                    *weights.entry(k.clone()).or_insert(0.0) += v;
                }
            }
            _ => {
                *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
            }
        }
        Ok(())
    }

    /// Check if a value is a lazy iterable (infinite range, lazy list, etc.)
    pub(super) fn is_lazy_for_set_ops(v: &Value) -> bool {
        match v {
            Value::LazyList(_) => true,
            Value::Array(_, kind) if kind.is_lazy() => true,
            Value::Range(_, end)
            | Value::RangeExcl(_, end)
            | Value::RangeExclStart(_, end)
            | Value::RangeExclBoth(_, end) => *end == i64::MAX,
            Value::GenericRange { end, .. } => {
                let end_f = end.to_f64();
                end_f.is_infinite() && end_f.is_sign_positive()
            }
            _ => false,
        }
    }

    pub(super) fn dispatch_to_mix(&self, target: Value) -> Result<Value, RuntimeError> {
        // Check for lazy iterables
        if Self::is_lazy_for_set_ops(&target) {
            let mut err = RuntimeError::new("Cannot .Mix a lazy list");
            err.exception = Some(Box::new(Value::make_instance(
                crate::symbol::Symbol::intern("X::Cannot::Lazy"),
                [("what".to_string(), Value::str_from("Mix"))]
                    .into_iter()
                    .collect(),
            )));
            return Err(err);
        }
        let mut weights: HashMap<String, f64> = HashMap::new();
        match target {
            Value::Mix(_, _) => return Ok(target),
            Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => {
                for item in items.iter() {
                    Self::mix_add_item(&mut weights, item)?;
                }
            }
            ref other @ (Value::Set(_, _)
            | Value::Bag(_, _)
            | Value::Pair(..)
            | Value::ValuePair(..)
            | Value::Hash(_)) => {
                Self::mix_add_item(&mut weights, other)?;
            }
            other if other.is_range() => {
                for item in Self::value_to_list(&other) {
                    *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                }
            }
            other => {
                weights.insert(other.to_string_value(), 1.0);
            }
        }
        Ok(Value::mix(weights))
    }

    pub(super) fn dispatch_to_map(&self, target: Value) -> Result<Value, RuntimeError> {
        // Map is stored as Hash internally in mutsu.
        // When converting from Hash to Map, decontainerize all values
        // (Map values are not wrapped in Scalar containers).
        match target {
            Value::Hash(ref map) => {
                // Check if already a Map (same identity)
                if self
                    .container_type_metadata(&target)
                    .and_then(|info| info.declared_type)
                    .is_some_and(|dt| dt == "Map")
                {
                    return Ok(target);
                }
                // Decontainerize values for the new Map
                let deconted: HashMap<String, Value> = map
                    .iter()
                    .map(|(k, v)| {
                        let deconted = match v {
                            Value::Scalar(inner) => (**inner).clone(),
                            other => other.clone(),
                        };
                        (k.clone(), deconted)
                    })
                    .collect();
                Ok(Value::hash(deconted))
            }
            _ => self.dispatch_to_hash_impl(target, true),
        }
    }

    pub(super) fn dispatch_to_hash(&self, target: Value) -> Result<Value, RuntimeError> {
        self.dispatch_to_hash_impl(target, true)
    }

    fn dispatch_to_hash_impl(&self, target: Value, check_odd: bool) -> Result<Value, RuntimeError> {
        match target {
            Value::Hash(_) => Ok(target),
            Value::Array(items, ..) => Self::items_to_hash(items.as_ref(), check_odd),
            Value::Seq(items) | Value::Slip(items) => {
                Self::items_to_hash(items.as_ref(), check_odd)
            }
            Value::Set(s, _) => {
                let mut map = HashMap::new();
                for k in s.iter() {
                    map.insert(k.clone(), Value::Bool(true));
                }
                Ok(Value::hash(map))
            }
            Value::Bag(b, _) => {
                let mut map = HashMap::new();
                for (k, v) in b.iter() {
                    map.insert(k.clone(), Value::Int(*v));
                }
                Ok(Value::hash(map))
            }
            Value::Mix(m, _) => {
                let mut map = HashMap::new();
                for (k, v) in m.iter() {
                    map.insert(k.clone(), Value::Num(*v));
                }
                Ok(Value::hash(map))
            }
            Value::Instance {
                ref class_name,
                ref attributes,
                ..
            } if class_name == "Match" => {
                // %($/) returns the named captures hash
                if let Some(named) = attributes.get("named") {
                    Ok(named.clone())
                } else {
                    Ok(Value::hash(HashMap::new()))
                }
            }
            other => {
                // Single non-Pair scalar: this is 1 element (odd), so throw
                if check_odd {
                    if let Value::Pair(k, v) = other {
                        let mut map = HashMap::new();
                        map.insert(k.to_string(), *v);
                        return Ok(Value::hash(map));
                    }
                    return Err(Self::make_odd_number_error(&[other]));
                }
                let mut map = HashMap::new();
                map.insert(other.to_string_value(), Value::Bool(true));
                Ok(Value::hash(map))
            }
        }
    }

    /// Convert a slice of items to a Hash, optionally checking for odd element count.
    fn items_to_hash(items: &[Value], check_odd: bool) -> Result<Value, RuntimeError> {
        if check_odd {
            // Count non-Pair elements to detect odd count
            let non_pair_count = items
                .iter()
                .filter(|v| !matches!(v, Value::Pair(..) | Value::ValuePair(..)))
                .count();
            if non_pair_count % 2 != 0 {
                return Err(Self::make_odd_number_error(items));
            }
        }
        let mut map = HashMap::new();
        let mut iter = items.iter();
        while let Some(item) = iter.next() {
            match item {
                Value::Pair(k, v) => {
                    map.insert(k.clone(), *v.clone());
                }
                Value::ValuePair(k, v) => {
                    map.insert(k.to_string_value(), *v.clone());
                }
                other => {
                    let key = other.to_string_value();
                    let value = iter.next().cloned().unwrap_or(Value::Nil);
                    map.insert(key, value);
                }
            }
        }
        Ok(Value::hash(map))
    }

    /// Create an X::Hash::Store::OddNumber error.
    fn make_odd_number_error(items: &[Value]) -> RuntimeError {
        let count = items.len();
        let last = items
            .last()
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let message = if count == 1 {
            format!(
                "Odd number of elements found where hash initializer expected:\n\
                 Only saw: {}",
                last
            )
        } else {
            format!(
                "Odd number of elements found where hash initializer expected:\n\
                 Found {} (implicit) elements:\nLast element seen: {}",
                count, last
            )
        };
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(
            crate::symbol::Symbol::intern("X::Hash::Store::OddNumber"),
            attrs,
        );
        RuntimeError {
            exception: Some(Box::new(ex)),
            ..RuntimeError::new(message)
        }
    }
}
