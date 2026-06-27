use super::*;

pub(crate) fn coerce_to_set(val: &Value) -> HashSet<String> {
    fn insert_set_elem(elems: &mut HashSet<String>, value: &Value) {
        let pair_selected = |weight: &Value| weight.truthy() || matches!(weight, Value::Nil);
        match value {
            Value::Set(items, _) => {
                elems.extend(items.iter().cloned());
            }
            Value::Bag(items, _) => {
                for (k, v) in items.iter() {
                    if v.is_positive() {
                        elems.insert(k.clone());
                    }
                }
            }
            Value::Mix(items, _) => {
                for (k, v) in items.iter() {
                    if *v != 0.0 {
                        elems.insert(k.clone());
                    }
                }
            }
            Value::Hash(items) => {
                for (k, v) in items.iter() {
                    if v.truthy() || matches!(v, Value::Nil) {
                        elems.insert(k.clone());
                    }
                }
            }
            _ if value.as_list_items().is_some() => {
                for item in value.as_list_items().unwrap().iter() {
                    insert_set_elem(elems, item);
                }
            }
            range if range.is_range() => {
                for item in value_to_list(range) {
                    insert_set_elem(elems, &item);
                }
            }
            Value::Pair(key, weight) => {
                if pair_selected(weight) {
                    elems.insert(key.clone());
                }
            }
            Value::ValuePair(key, weight) => {
                if pair_selected(weight) {
                    elems.insert(key.to_string_value());
                }
            }
            other => {
                let sv = other.to_string_value();
                if !sv.is_empty() {
                    elems.insert(sv);
                }
            }
        }
    }

    let val = val.descalarize();
    match val {
        Value::Set(s, _) => s.elements.clone(),
        Value::Bag(b, _) => {
            let resolved = resolve_bag_tab_keys(b);
            resolved.keys().cloned().collect()
        }
        Value::Mix(m, _) => m.keys().cloned().collect(),
        Value::Hash(items) => items
            .iter()
            .filter_map(|(k, v)| {
                if v.truthy() || matches!(v, Value::Nil) {
                    Some(k.clone())
                } else {
                    None
                }
            })
            .collect(),
        _ if val.as_list_items().is_some() => {
            let mut elems = HashSet::new();
            for item in val.as_list_items().unwrap().iter() {
                insert_set_elem(&mut elems, item);
            }
            elems
        }
        Value::Pair(_, _) | Value::ValuePair(_, _) => {
            let mut elems = HashSet::new();
            insert_set_elem(&mut elems, val);
            elems
        }
        range if range.is_range() => {
            let mut elems = HashSet::new();
            for item in value_to_list(range) {
                insert_set_elem(&mut elems, &item);
            }
            elems
        }
        _ => {
            let mut s = HashSet::new();
            let sv = val.to_string_value();
            if !sv.is_empty() {
                s.insert(sv);
            }
            s
        }
    }
}

/// Coerce a value to a QuantHash (Set/Bag/Mix) for use as a single operand to set operators.
/// - Set/Bag/Mix pass through as-is
/// - Hash: include keys with truthy values as Set elements
/// - List/Array: convert items to Set (excluding Pairs with falsy value)
/// - Pair with falsy value → empty Set
/// - Other scalars → Set with one element
pub(crate) fn coerce_value_to_quanthash(val: &Value) -> Value {
    let val = val.descalarize();
    match val {
        Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _) => val.clone(),
        Value::Hash(h) => {
            let mut set = HashSet::new();
            for (k, v) in h.iter() {
                if v.truthy() {
                    set.insert(k.clone());
                }
            }
            Value::set(set)
        }
        _ if val.as_list_items().is_some() => {
            let mut set = HashSet::new();
            for item in val.as_list_items().unwrap().iter() {
                match item {
                    Value::Pair(k, v) => {
                        if v.truthy() {
                            set.insert(k.clone());
                        }
                    }
                    Value::Hash(h) => {
                        for (k, v) in h.iter() {
                            if v.truthy() {
                                set.insert(k.clone());
                            }
                        }
                    }
                    other => {
                        set.insert(other.to_string_value());
                    }
                }
            }
            Value::set(set)
        }
        Value::Pair(k, v) => {
            let mut set = HashSet::new();
            if v.truthy() {
                set.insert(k.clone());
            }
            Value::set(set)
        }
        _ => {
            let mut set = HashSet::new();
            let sv = val.to_string_value();
            if !sv.is_empty() {
                set.insert(sv);
            }
            Value::set(set)
        }
    }
}

/// Determine the promotion level for set operations: 0=Set, 1=Bag, 2=Mix
pub(crate) fn set_type_level(v: &Value) -> u8 {
    match v {
        Value::Mix(_, _) => 2,
        Value::Bag(_, _) => 1,
        _ => 0,
    }
}

/// Convert a value to a Mix-level HashMap (key → f64 count)
pub(crate) fn to_mix_map(v: &Value) -> HashMap<String, f64> {
    match v {
        Value::Mix(m, _) => m.weights.clone(),
        Value::Bag(b, _) => {
            let resolved = resolve_bag_tab_keys(b);
            resolved.into_iter().map(|(k, v)| (k, v as f64)).collect()
        }
        Value::Set(s, _) => s.iter().map(|k| (k.clone(), 1.0)).collect(),
        Value::Hash(h) => {
            let mut result = HashMap::new();
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
                    result.insert(k.clone(), w);
                }
            }
            result
        }
        _ => {
            // Count occurrences for list-like values (e.g. (a, a, b) → {a: 2.0, b: 1.0})
            let items = value_to_list(v);
            let mut result = HashMap::new();
            for item in &items {
                *result.entry(item.to_string_value()).or_insert(0.0f64) += 1.0;
            }
            result
        }
    }
}

/// Resolve Bag entries that use the internal "key\tweight" tab format
/// into plain key→weight entries.
pub(crate) fn resolve_bag_tab_keys(bag: &HashMap<String, BigInt>) -> HashMap<String, i64> {
    let mut result = HashMap::new();
    for (k, c) in bag.iter() {
        let c = bigint_to_i64_sat(c);
        if let Some((base, raw_weight)) = k.split_once('\t') {
            let weight = match raw_weight {
                "True" => 1i64,
                "False" => 0,
                _ => raw_weight.parse::<i64>().unwrap_or(1),
            };
            *result.entry(base.to_string()).or_insert(0) += weight * c;
        } else {
            *result.entry(k.clone()).or_insert(0) += c;
        }
    }
    // Remove zero/negative entries for Bag semantics
    result.retain(|_, v| *v > 0);
    result
}

/// Convert a value to a Bag-level HashMap (key → i64 count)
pub(crate) fn to_bag_map(v: &Value) -> HashMap<String, i64> {
    match v {
        Value::Bag(b, _) => resolve_bag_tab_keys(b),
        Value::Set(s, _) => s.iter().map(|k| (k.clone(), 1i64)).collect(),
        Value::Hash(h) => {
            let mut result = HashMap::new();
            for (k, v) in h.iter() {
                let count = match v {
                    Value::Int(i) => *i,
                    Value::Num(n) => *n as i64,
                    Value::Bool(b) => i64::from(*b),
                    _ => i64::from(v.truthy()),
                };
                if count > 0 {
                    result.insert(k.clone(), count);
                }
            }
            result
        }
        _ => {
            // Count occurrences for list-like values (e.g. (a, a, b) → {a: 2, b: 1})
            let items = value_to_list(v);
            let mut result = HashMap::new();
            for item in &items {
                *result.entry(item.to_string_value()).or_insert(0i64) += 1;
            }
            result
        }
    }
}
