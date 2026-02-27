use crate::runtime;
use crate::value::{RuntimeError, Value};
use std::sync::Arc;

fn positional_pairs(values: &[Value]) -> Vec<Value> {
    values
        .iter()
        .enumerate()
        .map(|(idx, value)| {
            Value::ValuePair(Box::new(Value::Int(idx as i64)), Box::new(value.clone()))
        })
        .collect()
}

fn positional_keys(values: &[Value]) -> Vec<Value> {
    values
        .iter()
        .enumerate()
        .map(|(idx, _)| Value::Int(idx as i64))
        .collect()
}

fn positional_kv(values: &[Value]) -> Vec<Value> {
    let mut kv = Vec::with_capacity(values.len() * 2);
    for (idx, value) in values.iter().enumerate() {
        kv.push(Value::Int(idx as i64));
        kv.push(value.clone());
    }
    kv
}

fn positional_antipairs(values: &[Value]) -> Vec<Value> {
    values
        .iter()
        .enumerate()
        .map(|(idx, value)| {
            Value::ValuePair(Box::new(value.clone()), Box::new(Value::Int(idx as i64)))
        })
        .collect()
}

fn push_permutations(
    items: &[Value],
    used: &mut [bool],
    current: &mut Vec<Value>,
    out: &mut Vec<Value>,
) {
    if current.len() == items.len() {
        out.push(Value::array(current.clone()));
        return;
    }
    for idx in 0..items.len() {
        if used[idx] {
            continue;
        }
        used[idx] = true;
        current.push(items[idx].clone());
        push_permutations(items, used, current, out);
        current.pop();
        used[idx] = false;
    }
}

fn all_permutations(items: &[Value]) -> Vec<Value> {
    if items.is_empty() {
        return vec![Value::array(Vec::new())];
    }
    let mut out = Vec::new();
    let mut used = vec![false; items.len()];
    let mut current = Vec::with_capacity(items.len());
    push_permutations(items, &mut used, &mut current, &mut out);
    out
}

/// Collection-related 0-arg methods: keys, values, kv, pairs, total, minmax, squish
pub(super) fn dispatch(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    match method {
        "hash" => {
            let items = crate::runtime::utils::value_to_list(target);
            Some(crate::runtime::utils::build_hash_from_items(items))
        }
        "keys" => {
            if crate::runtime::utils::is_shaped_array(target) {
                let indexed = crate::runtime::utils::shaped_array_indexed_leaves(target);
                let keys: Vec<Value> = indexed
                    .into_iter()
                    .map(|(idx, _)| Value::array(idx.into_iter().map(Value::Int).collect()))
                    .collect();
                return Some(Ok(Value::array(keys)));
            }
            match target {
                Value::Hash(map) => {
                    let keys: Vec<Value> = map.keys().map(|k| Value::Str(k.clone())).collect();
                    Some(Ok(Value::array(keys)))
                }
                Value::Pair(key, _) => {
                    Some(Ok(Value::Seq(Arc::new(vec![Value::Str(key.clone())]))))
                }
                Value::ValuePair(key, _) => Some(Ok(Value::Seq(Arc::new(vec![*key.clone()])))),
                Value::Nil => Some(Ok(Value::array(Vec::new()))),
                Value::Set(s) => Some(Ok(Value::array(
                    s.iter().map(|k| Value::Str(k.clone())).collect(),
                ))),
                Value::Bag(b) => Some(Ok(Value::array(
                    b.keys().map(|k| Value::Str(k.clone())).collect(),
                ))),
                Value::Mix(m) => Some(Ok(Value::array(
                    m.keys().map(|k| Value::Str(k.clone())).collect(),
                ))),
                Value::Package(_) => Some(Ok(Value::array(Vec::new()))),
                v if v.is_range() => Some(Ok(Value::array(positional_keys(
                    &crate::runtime::utils::value_to_list(v),
                )))),
                other => Some(Ok(Value::array(positional_keys(
                    &crate::runtime::utils::value_to_list(other),
                )))),
            }
        }
        "values" => {
            if crate::runtime::utils::is_shaped_array(target) {
                let leaves = crate::runtime::utils::shaped_array_leaves(target);
                return Some(Ok(Value::array(leaves)));
            }
            match target {
                Value::Hash(map) => {
                    let values: Vec<Value> = map.values().cloned().collect();
                    Some(Ok(Value::array(values)))
                }
                Value::Pair(_, value) | Value::ValuePair(_, value) => {
                    Some(Ok(Value::Seq(Arc::new(vec![*value.clone()]))))
                }
                Value::Nil => Some(Ok(Value::array(Vec::new()))),
                Value::Set(s) => Some(Ok(Value::array(
                    s.iter().map(|_| Value::Bool(true)).collect(),
                ))),
                Value::Bag(b) => Some(Ok(Value::array(
                    b.values().map(|v| Value::Int(*v)).collect(),
                ))),
                Value::Mix(m) => Some(Ok(Value::array(
                    m.values().map(|v| Value::Num(*v)).collect(),
                ))),
                Value::Package(_) => Some(Ok(Value::array(Vec::new()))),
                v if v.is_range() => {
                    Some(Ok(Value::array(crate::runtime::utils::value_to_list(v))))
                }
                other => Some(Ok(Value::array(crate::runtime::utils::value_to_list(
                    other,
                )))),
            }
        }
        "kv" => {
            if crate::runtime::utils::is_shaped_array(target) {
                let indexed = crate::runtime::utils::shaped_array_indexed_leaves(target);
                let mut kv = Vec::with_capacity(indexed.len() * 2);
                for (idx, val) in indexed {
                    kv.push(Value::array(idx.into_iter().map(Value::Int).collect()));
                    kv.push(val);
                }
                return Some(Ok(Value::array(kv)));
            }
            match target {
                Value::Hash(items) => {
                    let mut kv = Vec::new();
                    for (k, v) in items.iter() {
                        kv.push(Value::Str(k.clone()));
                        kv.push(v.clone());
                    }
                    Some(Ok(Value::array(kv)))
                }
                Value::Pair(key, value) => Some(Ok(Value::Seq(Arc::new(vec![
                    Value::Str(key.clone()),
                    *value.clone(),
                ])))),
                Value::ValuePair(key, value) => {
                    Some(Ok(Value::Seq(Arc::new(vec![*key.clone(), *value.clone()]))))
                }
                Value::Nil => Some(Ok(Value::array(Vec::new()))),
                Value::Set(s) => {
                    let mut kv = Vec::new();
                    for k in s.iter() {
                        kv.push(Value::Str(k.clone()));
                        kv.push(Value::Bool(true));
                    }
                    Some(Ok(Value::array(kv)))
                }
                Value::Bag(b) => {
                    let mut kv = Vec::new();
                    for (k, v) in b.iter() {
                        kv.push(Value::Str(k.clone()));
                        kv.push(Value::Int(*v));
                    }
                    Some(Ok(Value::array(kv)))
                }
                Value::Mix(m) => {
                    let mut kv = Vec::new();
                    for (k, v) in m.iter() {
                        kv.push(Value::Str(k.clone()));
                        kv.push(Value::Num(*v));
                    }
                    Some(Ok(Value::array(kv)))
                }
                Value::Enum { key, value, .. } => Some(Ok(Value::Seq(Arc::new(vec![
                    Value::Str(key.clone()),
                    Value::Int(*value),
                ])))),
                Value::Package(_) => Some(Ok(Value::array(Vec::new()))),
                v if v.is_range() => Some(Ok(Value::array(positional_kv(
                    &crate::runtime::utils::value_to_list(v),
                )))),
                other => Some(Ok(Value::array(positional_kv(
                    &crate::runtime::utils::value_to_list(other),
                )))),
            }
        }
        "pairs" => {
            if crate::runtime::utils::is_shaped_array(target) {
                let indexed = crate::runtime::utils::shaped_array_indexed_leaves(target);
                let pairs: Vec<Value> = indexed
                    .into_iter()
                    .map(|(idx, val)| {
                        let key = Value::array(idx.into_iter().map(Value::Int).collect());
                        Value::ValuePair(Box::new(key), Box::new(val))
                    })
                    .collect();
                return Some(Ok(Value::array(pairs)));
            }
            match target {
                Value::Hash(items) => Some(Ok(Value::array(
                    items
                        .iter()
                        .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                        .collect(),
                ))),
                Value::Set(s) => Some(Ok(Value::array(
                    s.iter()
                        .map(|k| Value::Pair(k.clone(), Box::new(Value::Bool(true))))
                        .collect(),
                ))),
                Value::Bag(b) => Some(Ok(Value::array(
                    b.iter()
                        .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Int(*v))))
                        .collect(),
                ))),
                Value::Mix(m) => Some(Ok(Value::array(
                    m.iter()
                        .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Num(*v))))
                        .collect(),
                ))),
                Value::Package(_) => Some(Ok(Value::array(Vec::new()))),
                v if v.is_range() => Some(Ok(Value::array(positional_pairs(
                    &crate::runtime::utils::value_to_list(v),
                )))),
                other => Some(Ok(Value::array(positional_pairs(
                    &crate::runtime::utils::value_to_list(other),
                )))),
            }
        }
        "pairup" => match target {
            Value::Package(name) if name == "Any" => Some(Ok(Value::Seq(Vec::new().into()))),
            _ => {
                let items = crate::runtime::utils::value_to_list(target);
                if !items.len().is_multiple_of(2) {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("got".to_string(), Value::Int(items.len() as i64));
                    attrs.insert(
                        "message".to_string(),
                        Value::Str(format!(
                            "Cannot pair up odd number of elements ({})",
                            items.len()
                        )),
                    );
                    let ex = Value::make_instance("X::Pairup::OddNumber".to_string(), attrs);
                    let mut err = RuntimeError::new(format!(
                        "X::Pairup::OddNumber: Cannot pair up odd number of elements ({})",
                        items.len()
                    ));
                    err.exception = Some(Box::new(ex));
                    return Some(Err(err));
                }
                let pairs: Vec<Value> = items
                    .chunks_exact(2)
                    .map(|chunk| {
                        Value::ValuePair(Box::new(chunk[0].clone()), Box::new(chunk[1].clone()))
                    })
                    .collect();
                Some(Ok(Value::Seq(pairs.into())))
            }
        },
        "antipairs" => match target {
            Value::Hash(items) => Some(Ok(Value::array(
                items
                    .iter()
                    .map(|(k, v)| match v {
                        Value::Str(s) => Value::Pair(s.clone(), Box::new(Value::Str(k.clone()))),
                        _ => Value::ValuePair(Box::new(v.clone()), Box::new(Value::Str(k.clone()))),
                    })
                    .collect(),
            ))),
            Value::Package(_) => Some(Ok(Value::array(Vec::new()))),
            v if v.is_range() => {
                let values = crate::runtime::utils::value_to_list(v);
                Some(Ok(Value::array(positional_antipairs(&values))))
            }
            other => {
                let values = crate::runtime::utils::value_to_list(other);
                Some(Ok(Value::array(positional_antipairs(&values))))
            }
        },
        "invert" => match target {
            Value::Hash(items) => {
                let mut result = Vec::new();
                let make_inverted_pair = |val: &Value, key: &str| -> Value {
                    match val {
                        Value::Str(s) => {
                            Value::Pair(s.clone(), Box::new(Value::Str(key.to_string())))
                        }
                        _ => Value::ValuePair(
                            Box::new(val.clone()),
                            Box::new(Value::Str(key.to_string())),
                        ),
                    }
                };
                for (k, v) in items.iter() {
                    match v {
                        Value::Array(arr, ..) => {
                            for item in arr.iter() {
                                result.push(make_inverted_pair(item, k));
                            }
                        }
                        _ => {
                            result.push(make_inverted_pair(v, k));
                        }
                    }
                }
                Some(Ok(Value::array(result)))
            }
            _ => None,
        },
        "total" => match target {
            Value::Set(s) => Some(Ok(Value::Int(s.len() as i64))),
            Value::Bag(b) => Some(Ok(Value::Int(b.values().sum::<i64>()))),
            Value::Mix(m) => Some(Ok(Value::Num(m.values().sum::<f64>()))),
            _ => None,
        },
        "minmax" => match target {
            Value::Array(items, ..) if !items.is_empty() => {
                let mut min = &items[0];
                let mut max = &items[0];
                for item in &items[1..] {
                    if runtime::compare_values(item, min) < 0 {
                        min = item;
                    }
                    if runtime::compare_values(item, max) > 0 {
                        max = item;
                    }
                }
                Some(Ok(Value::GenericRange {
                    start: Box::new(min.clone()),
                    end: Box::new(max.clone()),
                    excl_start: false,
                    excl_end: false,
                }))
            }
            _ => None,
        },
        "sum" => match target {
            Value::Array(items, ..) => {
                let has_float = items
                    .iter()
                    .any(|v| matches!(v, Value::Num(_) | Value::Rat(_, _)));
                if has_float {
                    let total: f64 = items
                        .iter()
                        .map(|v| runtime::to_float_value(v).unwrap_or(0.0))
                        .sum();
                    Some(Ok(Value::Num(total)))
                } else {
                    let total: i64 = items.iter().map(runtime::to_int).sum();
                    Some(Ok(Value::Int(total)))
                }
            }
            _ => None,
        },
        "squish" => match target {
            Value::Array(items, ..) => {
                let mut result = Vec::new();
                let mut last: Option<String> = None;
                for item in items.iter() {
                    let s = item.to_string_value();
                    if last.as_ref() != Some(&s) {
                        last = Some(s);
                        result.push(item.clone());
                    }
                }
                Some(Ok(Value::array(result)))
            }
            _ => None,
        },
        "permutations" => {
            let items = match target {
                Value::Array(items, ..) | Value::Seq(items) | Value::Slip(items) => items.to_vec(),
                _ => runtime::value_to_list(target),
            };
            Some(Ok(Value::Seq(all_permutations(&items).into())))
        }
        _ => None,
    }
}
