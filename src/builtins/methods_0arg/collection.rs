use crate::runtime;
use crate::value::{RuntimeError, Value};

/// Collection-related 0-arg methods: keys, values, kv, pairs, total, minmax, squish
pub(super) fn dispatch(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    match method {
        "keys" => match target {
            Value::Hash(map) => {
                let keys: Vec<Value> = map.keys().map(|k| Value::Str(k.clone())).collect();
                Some(Ok(Value::Array(keys)))
            }
            Value::Set(s) => Some(Ok(Value::Array(
                s.iter().map(|k| Value::Str(k.clone())).collect(),
            ))),
            Value::Bag(b) => Some(Ok(Value::Array(
                b.keys().map(|k| Value::Str(k.clone())).collect(),
            ))),
            Value::Mix(m) => Some(Ok(Value::Array(
                m.keys().map(|k| Value::Str(k.clone())).collect(),
            ))),
            _ => None,
        },
        "values" => match target {
            Value::Hash(map) => {
                let values: Vec<Value> = map.values().cloned().collect();
                Some(Ok(Value::Array(values)))
            }
            Value::Set(s) => Some(Ok(Value::Array(
                s.iter().map(|_| Value::Bool(true)).collect(),
            ))),
            Value::Bag(b) => Some(Ok(Value::Array(
                b.values().map(|v| Value::Int(*v)).collect(),
            ))),
            Value::Mix(m) => Some(Ok(Value::Array(
                m.values().map(|v| Value::Num(*v)).collect(),
            ))),
            _ => None,
        },
        "kv" => match target {
            Value::Hash(items) => {
                let mut kv = Vec::new();
                for (k, v) in items {
                    kv.push(Value::Str(k.clone()));
                    kv.push(v.clone());
                }
                Some(Ok(Value::Array(kv)))
            }
            Value::Set(s) => {
                let mut kv = Vec::new();
                for k in s {
                    kv.push(Value::Str(k.clone()));
                    kv.push(Value::Bool(true));
                }
                Some(Ok(Value::Array(kv)))
            }
            Value::Bag(b) => {
                let mut kv = Vec::new();
                for (k, v) in b {
                    kv.push(Value::Str(k.clone()));
                    kv.push(Value::Int(*v));
                }
                Some(Ok(Value::Array(kv)))
            }
            Value::Mix(m) => {
                let mut kv = Vec::new();
                for (k, v) in m {
                    kv.push(Value::Str(k.clone()));
                    kv.push(Value::Num(*v));
                }
                Some(Ok(Value::Array(kv)))
            }
            Value::Enum { key, value, .. } => Some(Ok(Value::Array(vec![
                Value::Str(key.clone()),
                Value::Int(*value),
            ]))),
            _ => None,
        },
        "pairs" => match target {
            Value::Hash(items) => Some(Ok(Value::Array(
                items
                    .iter()
                    .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                    .collect(),
            ))),
            Value::Set(s) => Some(Ok(Value::Array(
                s.iter()
                    .map(|k| Value::Pair(k.clone(), Box::new(Value::Bool(true))))
                    .collect(),
            ))),
            Value::Bag(b) => Some(Ok(Value::Array(
                b.iter()
                    .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Int(*v))))
                    .collect(),
            ))),
            Value::Mix(m) => Some(Ok(Value::Array(
                m.iter()
                    .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Num(*v))))
                    .collect(),
            ))),
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
                Some(Ok(Value::Range(runtime::to_int(min), runtime::to_int(max))))
            }
            _ => None,
        },
        "squish" => match target {
            Value::Array(items) => {
                let mut result = Vec::new();
                let mut last: Option<String> = None;
                for item in items {
                    let s = item.to_string_value();
                    if last.as_ref() != Some(&s) {
                        last = Some(s);
                        result.push(item.clone());
                    }
                }
                Some(Ok(Value::Array(result)))
            }
            _ => None,
        },
        _ => None,
    }
}
