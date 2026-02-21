use crate::runtime;
use crate::value::{RuntimeError, Value};

/// Collection-related 0-arg methods: keys, values, kv, pairs, total, minmax, squish
pub(super) fn dispatch(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    match method {
        "keys" => match target {
            Value::Hash(map) => {
                let keys: Vec<Value> = map.keys().map(|k| Value::Str(k.clone())).collect();
                Some(Ok(Value::array(keys)))
            }
            Value::Set(s) => Some(Ok(Value::array(
                s.iter().map(|k| Value::Str(k.clone())).collect(),
            ))),
            Value::Bag(b) => Some(Ok(Value::array(
                b.keys().map(|k| Value::Str(k.clone())).collect(),
            ))),
            Value::Mix(m) => Some(Ok(Value::array(
                m.keys().map(|k| Value::Str(k.clone())).collect(),
            ))),
            _ => None,
        },
        "values" => match target {
            Value::Hash(map) => {
                let values: Vec<Value> = map.values().cloned().collect();
                Some(Ok(Value::array(values)))
            }
            Value::Set(s) => Some(Ok(Value::array(
                s.iter().map(|_| Value::Bool(true)).collect(),
            ))),
            Value::Bag(b) => Some(Ok(Value::array(
                b.values().map(|v| Value::Int(*v)).collect(),
            ))),
            Value::Mix(m) => Some(Ok(Value::array(
                m.values().map(|v| Value::Num(*v)).collect(),
            ))),
            _ => None,
        },
        "kv" => match target {
            Value::Hash(items) => {
                let mut kv = Vec::new();
                for (k, v) in items.iter() {
                    kv.push(Value::Str(k.clone()));
                    kv.push(v.clone());
                }
                Some(Ok(Value::array(kv)))
            }
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
            Value::Enum { key, value, .. } => Some(Ok(Value::array(vec![
                Value::Str(key.clone()),
                Value::Int(*value),
            ]))),
            _ => None,
        },
        "pairs" => match target {
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
            _ => None,
        },
        "antipairs" => match target {
            Value::Hash(items) => Some(Ok(Value::array(
                items
                    .iter()
                    .map(|(k, v)| Value::Pair(v.to_string_value(), Box::new(Value::Str(k.clone()))))
                    .collect(),
            ))),
            _ => None,
        },
        "invert" => match target {
            Value::Hash(items) => {
                let mut result = Vec::new();
                for (k, v) in items.iter() {
                    match v {
                        Value::Array(arr) => {
                            for item in arr.iter() {
                                result.push(Value::Pair(
                                    item.to_string_value(),
                                    Box::new(Value::Str(k.clone())),
                                ));
                            }
                        }
                        _ => {
                            result.push(Value::Pair(
                                v.to_string_value(),
                                Box::new(Value::Str(k.clone())),
                            ));
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
            Value::Array(items) if !items.is_empty() => {
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
            Value::Array(items) => {
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
            Value::Array(items) => {
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
        _ => None,
    }
}
