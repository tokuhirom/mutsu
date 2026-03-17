use super::*;

impl Interpreter {
    pub(super) fn dispatch_to_set(&self, target: Value) -> Result<Value, RuntimeError> {
        let mut elems = HashSet::new();
        match target {
            Value::Set(_) => return Ok(target),
            Value::Array(items, ..) => {
                for item in items.iter() {
                    match item {
                        Value::Pair(k, v) => {
                            if v.truthy() {
                                elems.insert(k.clone());
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
            Value::Pair(k, v) => {
                if v.truthy() {
                    elems.insert(k);
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
        match target {
            Value::Bag(_) => return Ok(target),
            Value::Array(items, ..) => {
                for item in items.iter() {
                    match item {
                        Value::Pair(k, v) => {
                            add_pair_key(&mut counts, k.clone(), pair_weight(v));
                        }
                        Value::ValuePair(k, v) => {
                            add_pair_key(&mut counts, k.to_string_value(), pair_weight(v));
                        }
                        _ => {
                            *counts.entry(item.to_string_value()).or_insert(0) += 1;
                        }
                    }
                }
            }
            Value::Set(s) => {
                for k in s.iter() {
                    counts.insert(k.clone(), 1);
                }
            }
            Value::Mix(m) => {
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
                    *counts.entry(item.to_string_value()).or_insert(0) += 1;
                }
            }
            other => {
                counts.insert(other.to_string_value(), 1);
            }
        }
        Ok(Value::bag(counts))
    }

    pub(super) fn dispatch_to_mix(&self, target: Value) -> Result<Value, RuntimeError> {
        let mut weights: HashMap<String, f64> = HashMap::new();
        match target {
            Value::Mix(_) => return Ok(target),
            Value::Array(items, ..) => {
                for item in items.iter() {
                    match item {
                        Value::Pair(k, v) => {
                            let w = match v.as_ref() {
                                Value::Int(i) => *i as f64,
                                Value::Num(n) => *n,
                                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                                _ => 1.0,
                            };
                            *weights.entry(k.clone()).or_insert(0.0) += w;
                        }
                        Value::ValuePair(k, v) => {
                            let w = match v.as_ref() {
                                Value::Int(i) => *i as f64,
                                Value::Num(n) => *n,
                                Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                                _ => 1.0,
                            };
                            *weights.entry(k.to_string_value()).or_insert(0.0) += w;
                        }
                        _ => {
                            *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                        }
                    }
                }
            }
            Value::Set(s) => {
                for k in s.iter() {
                    weights.insert(k.clone(), 1.0);
                }
            }
            Value::Bag(b) => {
                for (k, v) in b.iter() {
                    weights.insert(k.clone(), *v as f64);
                }
            }
            Value::Pair(k, v) => {
                let w = match v.as_ref() {
                    Value::Int(i) => *i as f64,
                    Value::Num(n) => *n,
                    Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                    _ => 1.0,
                };
                *weights.entry(k.clone()).or_insert(0.0) += w;
            }
            Value::ValuePair(k, v) => {
                let w = match v.as_ref() {
                    Value::Int(i) => *i as f64,
                    Value::Num(n) => *n,
                    Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                    _ => 1.0,
                };
                *weights.entry(k.to_string_value()).or_insert(0.0) += w;
            }
            Value::Hash(h) => {
                for (k, v) in h.iter() {
                    let w = match v {
                        Value::Int(i) => *i as f64,
                        Value::Num(n) => *n,
                        Value::Rat(n, d) if *d != 0 => *n as f64 / *d as f64,
                        Value::Bool(b) => {
                            if *b {
                                1.0
                            } else {
                                0.0
                            }
                        }
                        _ => {
                            if v.truthy() {
                                1.0
                            } else {
                                0.0
                            }
                        }
                    };
                    if w != 0.0 {
                        weights.insert(k.clone(), w);
                    }
                }
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
        // Map is stored as Hash internally in mutsu; .Map on a Hash returns it as-is
        self.dispatch_to_hash_impl(target, true)
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
            Value::Set(s) => {
                let mut map = HashMap::new();
                for k in s.iter() {
                    map.insert(k.clone(), Value::Bool(true));
                }
                Ok(Value::hash(map))
            }
            Value::Bag(b) => {
                let mut map = HashMap::new();
                for (k, v) in b.iter() {
                    map.insert(k.clone(), Value::Int(*v));
                }
                Ok(Value::hash(map))
            }
            Value::Mix(m) => {
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
