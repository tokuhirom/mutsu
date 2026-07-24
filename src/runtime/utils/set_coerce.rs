use super::*;

/// The Set-element store key for a hash entry, recording the key object in
/// `originals` when it is not a plain Str: an object hash stores `.WHICH`
/// keys already (decode the object and re-key — identical for well-formed
/// stores); a plain hash key is a Str element (`"Str|<k>"`).
pub(crate) fn hash_elem_key(
    h: &crate::value::HashData,
    k: &str,
    originals: &mut HashMap<String, Value>,
) -> String {
    if h.has_typed_keys() {
        let (key, obj) = quanthash_elem_entry(&h.typed_key(k));
        record_quanthash_original(originals, &key, &obj);
        key
    } else {
        str_elem_key(k)
    }
}

pub(crate) fn coerce_to_set(
    val: &Value,
    originals: &mut HashMap<String, Value>,
) -> HashSet<String> {
    fn insert_set_elem(
        elems: &mut HashSet<String>,
        originals: &mut HashMap<String, Value>,
        value: &Value,
    ) {
        let pair_selected = |weight: &Value| weight.truthy() || weight.is_nil();
        match value.view() {
            ValueView::Set(items, _) => {
                extend_quanthash_originals(originals, &items.original_keys);
                elems.extend(items.iter().cloned());
            }
            ValueView::Bag(items, _) => {
                extend_quanthash_originals(originals, &items.original_keys);
                for (k, v) in items.iter() {
                    if v.is_positive() {
                        elems.insert(k.clone());
                    }
                }
            }
            ValueView::Mix(items, _) => {
                extend_quanthash_originals(originals, &items.original_keys);
                for (k, v) in items.iter() {
                    if *v != 0.0 {
                        elems.insert(k.clone());
                    }
                }
            }
            ValueView::Hash(items) => {
                for (k, v) in items.iter() {
                    if v.truthy() || v.is_nil() {
                        let key = hash_elem_key(&items, k, originals);
                        elems.insert(key);
                    }
                }
            }
            _ if value.as_list_items().is_some() => {
                for item in value.as_list_items().unwrap().iter() {
                    insert_set_elem(elems, originals, item);
                }
            }
            _ if value.is_range() => {
                for item in value_to_list(value) {
                    insert_set_elem(elems, originals, &item);
                }
            }
            ValueView::Pair(key, weight) => {
                if pair_selected(weight) {
                    elems.insert(str_elem_key(key));
                }
            }
            ValueView::ValuePair(key, weight) => {
                if pair_selected(weight) {
                    quanthash_insert_set(elems, originals, key);
                }
            }
            _ => {
                // Preserve the historical "empty stringification contributes
                // no element" rule (an undefined scalar operand unions from
                // the empty set).
                let (key, elem) = quanthash_elem_entry(value);
                if !elem.to_string_value().is_empty() {
                    record_quanthash_original(originals, &key, &elem);
                    elems.insert(key);
                }
            }
        }
    }

    let val = val.descalarize();
    match val.view() {
        ValueView::Set(s, _) => {
            extend_quanthash_originals(originals, &s.original_keys);
            s.elements.clone()
        }
        ValueView::Bag(b, _) => {
            extend_quanthash_originals(originals, &b.original_keys);
            let resolved = resolve_bag_tab_keys(&b);
            resolved.keys().cloned().collect()
        }
        ValueView::Mix(m, _) => {
            extend_quanthash_originals(originals, &m.original_keys);
            m.keys().cloned().collect()
        }
        ValueView::Hash(items) => {
            let mut elems = HashSet::new();
            for (k, v) in items.iter() {
                if v.truthy() || v.is_nil() {
                    let key = hash_elem_key(&items, k, originals);
                    elems.insert(key);
                }
            }
            elems
        }
        _ if val.as_list_items().is_some() => {
            let mut elems = HashSet::new();
            for item in val.as_list_items().unwrap().iter() {
                insert_set_elem(&mut elems, originals, item);
            }
            elems
        }
        ValueView::Pair(_, _) | ValueView::ValuePair(_, _) => {
            let mut elems = HashSet::new();
            insert_set_elem(&mut elems, originals, val);
            elems
        }
        _ if val.is_range() => {
            let mut elems = HashSet::new();
            for item in value_to_list(val) {
                insert_set_elem(&mut elems, originals, &item);
            }
            elems
        }
        _ => {
            let mut s = HashSet::new();
            let (key, elem) = quanthash_elem_entry(val);
            if !elem.to_string_value().is_empty() {
                record_quanthash_original(originals, &key, &elem);
                s.insert(key);
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
    match val.view() {
        ValueView::Set(_, _) | ValueView::Bag(_, _) | ValueView::Mix(_, _) => val.clone(),
        ValueView::Hash(h) => {
            let mut set = HashSet::new();
            let mut originals = HashMap::new();
            for (k, v) in h.iter() {
                if v.truthy() {
                    let key = hash_elem_key(&h, k, &mut originals);
                    set.insert(key);
                }
            }
            Value::set_typed(set, originals)
        }
        _ if val.as_list_items().is_some() => {
            let mut set = HashSet::new();
            let mut originals = HashMap::new();
            for item in val.as_list_items().unwrap().iter() {
                match item.view() {
                    ValueView::Pair(k, v) => {
                        if v.truthy() {
                            set.insert(str_elem_key(k));
                        }
                    }
                    ValueView::Hash(h) => {
                        for (k, v) in h.iter() {
                            if v.truthy() {
                                let key = hash_elem_key(&h, k, &mut originals);
                                set.insert(key);
                            }
                        }
                    }
                    _ => {
                        quanthash_insert_set(&mut set, &mut originals, item);
                    }
                }
            }
            Value::set_typed(set, originals)
        }
        ValueView::Pair(k, v) => {
            let mut set = HashSet::new();
            if v.truthy() {
                set.insert(str_elem_key(k));
            }
            Value::set(set)
        }
        // A Range enumerates its elements (`@n (<=) (1..49)`), mirroring
        // `coerce_to_set` above — without this arm it fell to the scalar
        // catch-all and became a one-element Set of the string "1..49".
        _ if val.is_range() => {
            let mut set = HashSet::new();
            let mut originals = HashMap::new();
            for item in value_to_list(val) {
                quanthash_insert_set(&mut set, &mut originals, &item);
            }
            Value::set_typed(set, originals)
        }
        _ => {
            let mut set = HashSet::new();
            let mut originals = HashMap::new();
            let (key, elem) = quanthash_elem_entry(val);
            if !elem.to_string_value().is_empty() {
                record_quanthash_original(&mut originals, &key, &elem);
                set.insert(key);
            }
            Value::set_typed(set, originals)
        }
    }
}

/// Determine the promotion level for set operations: 0=Set, 1=Bag, 2=Mix
pub(crate) fn set_type_level(v: &Value) -> u8 {
    match v.view() {
        ValueView::Mix(_, _) => 2,
        ValueView::Bag(_, _) => 1,
        _ => 0,
    }
}

/// Convert a value to a Mix-level HashMap (key → f64 count)
pub(crate) fn to_mix_map(
    v: &Value,
    originals: &mut HashMap<String, Value>,
) -> HashMap<String, f64> {
    match v.view() {
        ValueView::Mix(m, _) => {
            extend_quanthash_originals(originals, &m.original_keys);
            m.weights.clone()
        }
        ValueView::Bag(b, _) => {
            extend_quanthash_originals(originals, &b.original_keys);
            let resolved = resolve_bag_tab_keys(&b);
            resolved.into_iter().map(|(k, v)| (k, v as f64)).collect()
        }
        ValueView::Set(s, _) => {
            extend_quanthash_originals(originals, &s.original_keys);
            s.iter().map(|k| (k.clone(), 1.0)).collect()
        }
        ValueView::Hash(h) => {
            let mut result = HashMap::new();
            for (k, v) in h.iter() {
                let w = match v.view() {
                    ValueView::Int(i) => i as f64,
                    ValueView::Num(n) => n,
                    ValueView::Rat(n, d) if d != 0 => n as f64 / d as f64,
                    ValueView::Bool(b) => {
                        if b {
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
                    let key = hash_elem_key(&h, k, originals);
                    result.insert(key, w);
                }
            }
            result
        }
        _ => {
            // Count occurrences for list-like values (e.g. (a, a, b) → {a: 2.0, b: 1.0})
            let items = value_to_list(v);
            let mut result = HashMap::new();
            for item in &items {
                let (key, elem) = quanthash_elem_entry(item);
                record_quanthash_original(originals, &key, &elem);
                *result.entry(key).or_insert(0.0f64) += 1.0;
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
pub(crate) fn to_bag_map(
    v: &Value,
    originals: &mut HashMap<String, Value>,
) -> HashMap<String, i64> {
    match v.view() {
        ValueView::Bag(b, _) => {
            extend_quanthash_originals(originals, &b.original_keys);
            resolve_bag_tab_keys(&b)
        }
        ValueView::Set(s, _) => {
            extend_quanthash_originals(originals, &s.original_keys);
            s.iter().map(|k| (k.clone(), 1i64)).collect()
        }
        ValueView::Hash(h) => {
            let mut result = HashMap::new();
            for (k, v) in h.iter() {
                let count = match v.view() {
                    ValueView::Int(i) => i,
                    ValueView::Num(n) => n as i64,
                    ValueView::Bool(b) => i64::from(b),
                    _ => i64::from(v.truthy()),
                };
                if count > 0 {
                    let key = hash_elem_key(&h, k, originals);
                    result.insert(key, count);
                }
            }
            result
        }
        _ => {
            // Count occurrences for list-like values (e.g. (a, a, b) → {a: 2, b: 1})
            let items = value_to_list(v);
            let mut result = HashMap::new();
            for item in &items {
                let (key, elem) = quanthash_elem_entry(item);
                record_quanthash_original(originals, &key, &elem);
                *result.entry(key).or_insert(0i64) += 1;
            }
            result
        }
    }
}
