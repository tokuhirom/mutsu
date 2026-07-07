use super::*;

impl Interpreter {
    /// Collect key-value pairs from Hash.push/append arguments.
    /// Arguments can be Pair values or alternating key, value flat lists.
    pub(crate) fn hash_push_collect_pairs(args: Vec<Value>) -> Vec<(String, Value)> {
        let mut pairs = Vec::new();
        let mut iter = args.into_iter().peekable();
        while let Some(arg) = iter.next() {
            match arg.view() {
                ValueView::Pair(k, v) => {
                    pairs.push((k.clone(), v.clone()));
                }
                ValueView::ValuePair(k, v) => {
                    pairs.push((k.to_string_value(), v.clone()));
                }
                ValueView::Array(items, ..) => {
                    // Recursively collect pairs from array elements
                    let inner_pairs = Self::hash_push_collect_pairs(items.to_vec());
                    pairs.extend(inner_pairs);
                }
                ValueView::Seq(items) | ValueView::Slip(items) => {
                    // A Seq/Slip of pairs (e.g. from `%h.push: %x.invert`) is
                    // flattened like an array, not stringified as one key.
                    let inner_pairs = Self::hash_push_collect_pairs(items.to_vec());
                    pairs.extend(inner_pairs);
                }
                ValueView::Hash(h) => {
                    for (k, v) in h.iter() {
                        pairs.push((k.clone(), v.clone()));
                    }
                }
                _ => {
                    // Alternating key, value
                    let key = arg.to_string_value();
                    let val = iter.next().unwrap_or(Value::NIL);
                    pairs.push((key, val));
                }
            }
        }
        pairs
    }

    /// Like [`hash_push_collect_pairs`] but preserves the original key *value*
    /// (not its stringification), so typed object hashes can type-check the key
    /// and store it under its `.WHICH` key. The first element of each tuple is
    /// the key value, the second the value.
    pub(crate) fn hash_push_collect_pairs_kv(args: Vec<Value>) -> Vec<(Value, Value)> {
        let mut pairs = Vec::new();
        let mut iter = args.into_iter().peekable();
        while let Some(arg) = iter.next() {
            match arg.view() {
                ValueView::Pair(k, v) => {
                    pairs.push((Value::str(k.clone()), v.clone()));
                }
                ValueView::ValuePair(k, v) => {
                    pairs.push((k.clone(), v.clone()));
                }
                ValueView::Array(items, ..) => {
                    pairs.extend(Self::hash_push_collect_pairs_kv(items.to_vec()));
                }
                ValueView::Seq(items) | ValueView::Slip(items) => {
                    pairs.extend(Self::hash_push_collect_pairs_kv(items.to_vec()));
                }
                ValueView::Hash(h) => {
                    for (k, v) in h.map.iter() {
                        let key = h
                            .original_keys
                            .as_ref()
                            .and_then(|o| o.get(k).cloned())
                            .unwrap_or_else(|| Value::str(k.clone()));
                        pairs.push((key, v.clone()));
                    }
                }
                _ => {
                    let val = iter.next().unwrap_or(Value::NIL);
                    pairs.push((arg.clone(), val));
                }
            }
        }
        pairs
    }

    /// Insert a key-value pair into a hash with push/append semantics.
    /// push: if key exists, stack the new value (existing becomes [existing, new])
    /// append: if key exists, flatten arrays when appending
    pub(crate) fn hash_push_insert(
        hash: &mut std::collections::HashMap<String, Value>,
        key: String,
        value: Value,
        is_push: bool,
    ) {
        if let Some(existing) = hash.get(&key) {
            let new_val = match existing.view() {
                ValueView::Array(arr, ..) => {
                    let mut items = arr.to_vec();
                    if is_push {
                        // push: add value as-is (could be nested array)
                        items.push(value);
                    } else {
                        // append: flatten arrays
                        match value.view() {
                            ValueView::Array(new_items, ..) => {
                                items.extend(new_items.iter().cloned());
                            }
                            _ => items.push(value.clone()),
                        }
                    }
                    Value::real_array(items)
                }
                _ => {
                    // First duplicate: create array [existing, new]
                    if is_push {
                        Value::real_array(vec![existing.clone(), value])
                    } else {
                        // append: flatten arrays
                        let mut items = vec![existing.clone()];
                        match value.view() {
                            ValueView::Array(new_items, ..) => {
                                items.extend(new_items.iter().cloned());
                            }
                            _ => items.push(value.clone()),
                        }
                        Value::real_array(items)
                    }
                }
            };
            hash.insert(key, new_val);
        } else {
            hash.insert(key, value);
        }
    }
}
