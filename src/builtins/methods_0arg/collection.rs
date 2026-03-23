use crate::runtime;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value};
use num_bigint::BigInt as NumBigInt;
use std::sync::Arc;

/// If the value represents an integer (even as Num, Rat, Str, or BigInt), return as BigInt.
fn value_as_bigint(v: &Value) -> Option<NumBigInt> {
    match v {
        Value::Int(i) => Some(NumBigInt::from(*i)),
        Value::BigInt(n) => Some((**n).clone()),
        Value::Num(f) => {
            if f.is_finite() && *f == f.trunc() && f.abs() < i64::MAX as f64 {
                Some(NumBigInt::from(*f as i64))
            } else {
                None
            }
        }
        Value::Rat(n, d) => {
            if *d != 0 && *n % *d == 0 {
                Some(NumBigInt::from(*n / *d))
            } else {
                None
            }
        }
        Value::Str(s) => {
            let trimmed = s.trim();
            trimmed.parse::<i64>().ok().map(NumBigInt::from)
        }
        Value::Bool(b) => Some(NumBigInt::from(if *b { 1 } else { 0 })),
        _ => None,
    }
}

fn gcd_u64(mut a: u64, mut b: u64) -> u64 {
    while b != 0 {
        let t = b;
        b = a % b;
        a = t;
    }
    a
}

fn f64_to_rat(f: f64) -> (i64, i64) {
    if f.is_nan() {
        return (0, 0);
    }
    if f.is_infinite() {
        return if f > 0.0 { (1, 0) } else { (-1, 0) };
    }
    let negative = f < 0.0;
    let f = f.abs();
    let mut den: i64 = 1;
    let mut num = f;
    for _ in 0..18 {
        if (num - num.round()).abs() < 1e-10 {
            break;
        }
        num *= 10.0;
        den *= 10;
    }
    let n = num.round() as i64;
    let g = gcd_u64(n.unsigned_abs(), den.unsigned_abs());
    let n = n / g as i64;
    let d = den / g as i64;
    if negative { (-n, d) } else { (n, d) }
}

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

fn make_inverted_pair(key: Value, value: Value) -> Value {
    match key {
        Value::Str(s) => Value::Pair((*s).clone(), Box::new(value)),
        other => Value::ValuePair(Box::new(other), Box::new(value)),
    }
}

fn should_expand_invert_value(value: &Value) -> bool {
    matches!(
        value,
        Value::Array(_, _)
            | Value::Seq(_)
            | Value::Slip(_)
            | Value::Hash(_)
            | Value::Set(_)
            | Value::Bag(_)
            | Value::Mix(_)
    ) || value.is_range()
}

fn extend_inverted_pairs(out: &mut Vec<Value>, key: Value, value: &Value) {
    let values = match value {
        Value::Str(_) => vec![value.clone()],
        other if should_expand_invert_value(other) => crate::runtime::utils::value_to_list(other),
        other => vec![other.clone()],
    };
    for item in values {
        out.push(make_inverted_pair(item, key.clone()));
    }
}

fn invert_value(target: &Value) -> Option<Value> {
    let mut result = Vec::new();
    match target {
        Value::Hash(items) => {
            for (k, v) in items.iter() {
                extend_inverted_pairs(&mut result, Value::str(k.clone()), v);
            }
        }
        Value::Bag(items) => {
            for (k, count) in items.iter() {
                result.push(make_inverted_pair(
                    Value::Int(*count),
                    Value::str(k.clone()),
                ));
            }
        }
        Value::Set(items) => {
            for k in items.iter() {
                result.push(make_inverted_pair(Value::Bool(true), Value::str(k.clone())));
            }
        }
        Value::Mix(items) => {
            for (k, weight) in items.iter() {
                let (n, d) = f64_to_rat(*weight);
                let weight_val = if d == 1 {
                    Value::Int(n)
                } else {
                    Value::Rat(n, d)
                };
                result.push(make_inverted_pair(weight_val, Value::str(k.clone())));
            }
        }
        Value::Pair(key, value) => {
            extend_inverted_pairs(&mut result, Value::str(key.clone()), value);
        }
        Value::ValuePair(key, value) => {
            extend_inverted_pairs(&mut result, *key.clone(), value);
        }
        Value::Array(_, _) | Value::Seq(_) | Value::Slip(_) => {
            for item in crate::runtime::utils::value_to_list(target) {
                match item {
                    Value::Pair(key, value) => {
                        extend_inverted_pairs(&mut result, Value::str(key), &value);
                    }
                    Value::ValuePair(key, value) => {
                        extend_inverted_pairs(&mut result, *key, &value);
                    }
                    _ => return None,
                }
            }
        }
        _ => return None,
    }
    Some(Value::Seq(result.into()))
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

/// Generate all combinations of `k` items from `items`.
pub(crate) fn combinations_k(items: &[Value], k: usize) -> Vec<Value> {
    let n = items.len();
    if k == 0 {
        return vec![Value::array(Vec::new())];
    }
    if k > n {
        return Vec::new();
    }
    let mut result = Vec::new();
    let mut indices: Vec<usize> = (0..k).collect();
    loop {
        let combo: Vec<Value> = indices.iter().map(|&i| items[i].clone()).collect();
        result.push(Value::array(combo));
        // Find rightmost index that can be incremented
        let mut i = k;
        while i > 0 {
            i -= 1;
            if indices[i] != i + n - k {
                break;
            }
            if i == 0 && indices[0] == n - k {
                return result;
            }
        }
        indices[i] += 1;
        for j in (i + 1)..k {
            indices[j] = indices[j - 1] + 1;
        }
    }
}

/// Generate powerset (all combinations for k=0..n).
pub(crate) fn combinations_all(items: &[Value]) -> Vec<Value> {
    let mut result = Vec::new();
    for k in 0..=items.len() {
        result.extend(combinations_k(items, k));
    }
    result
}

/// Generate combinations for a range of k values, clamped to [0, n].
pub(crate) fn combinations_range(items: &[Value], min_k: i64, max_k: i64) -> Vec<Value> {
    let n = items.len() as i64;
    let lo = min_k.max(0);
    let hi = max_k.min(n);
    if lo > hi {
        return Vec::new();
    }
    let mut result = Vec::new();
    for k in (lo as usize)..=(hi as usize) {
        result.extend(combinations_k(items, k));
    }
    result
}

/// Collection-related 0-arg methods: keys, values, kv, pairs, total, minmax, squish
pub(super) fn dispatch(target: &Value, method: &str) -> Option<Result<Value, RuntimeError>> {
    match method {
        "hash" => match target {
            Value::Set(s) => {
                let mut map = std::collections::HashMap::new();
                for k in s.iter() {
                    map.insert(k.clone(), Value::Bool(true));
                }
                Some(Ok(Value::hash(map)))
            }
            Value::Bag(b) => {
                let mut map = std::collections::HashMap::new();
                for (k, v) in b.iter() {
                    map.insert(k.clone(), Value::Int(*v));
                }
                Some(Ok(Value::hash(map)))
            }
            Value::Mix(m) => {
                let mut map = std::collections::HashMap::new();
                for (k, v) in m.iter() {
                    map.insert(k.clone(), Value::Num(*v));
                }
                Some(Ok(Value::hash(map)))
            }
            Value::Instance { .. } => {
                // Instance types should fall through to accessor dispatch,
                // not be coerced via .hash builtin
                None
            }
            _ => {
                let items = crate::runtime::utils::value_to_list(target);
                Some(crate::runtime::utils::build_hash_from_items(items))
            }
        },
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
                    let keys: Vec<Value> = map.keys().map(|k| Value::str(k.clone())).collect();
                    Some(Ok(Value::array(keys)))
                }
                Value::Pair(key, _) => {
                    Some(Ok(Value::Seq(Arc::new(vec![Value::str(key.clone())]))))
                }
                Value::ValuePair(key, _) => Some(Ok(Value::Seq(Arc::new(vec![*key.clone()])))),
                Value::Nil => Some(Ok(Value::array(Vec::new()))),
                Value::Set(s) => Some(Ok(Value::array(
                    s.iter().map(|k| Value::str(k.clone())).collect(),
                ))),
                Value::Bag(b) => Some(Ok(Value::array(
                    b.keys().map(|k| Value::str(k.clone())).collect(),
                ))),
                Value::Mix(m) => Some(Ok(Value::array(
                    m.keys().map(|k| Value::str(k.clone())).collect(),
                ))),
                Value::Package(_) => None, // let runtime handle (may be enum type)
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
                Value::Package(_) => None, // let runtime handle (may be enum type)
                v if v.is_range() => {
                    Some(Ok(Value::array(crate::runtime::utils::value_to_list(v))))
                }
                Value::LazyList(_) => None, // fall through to runtime to force
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
                        kv.push(Value::str(k.clone()));
                        kv.push(v.clone());
                    }
                    Some(Ok(Value::array(kv)))
                }
                Value::Pair(key, value) => Some(Ok(Value::Seq(Arc::new(vec![
                    Value::str(key.clone()),
                    *value.clone(),
                ])))),
                Value::ValuePair(key, value) => {
                    Some(Ok(Value::Seq(Arc::new(vec![*key.clone(), *value.clone()]))))
                }
                Value::Nil => Some(Ok(Value::array(Vec::new()))),
                Value::Set(s) => {
                    let mut kv = Vec::new();
                    for k in s.iter() {
                        kv.push(Value::str(k.clone()));
                        kv.push(Value::Bool(true));
                    }
                    Some(Ok(Value::array(kv)))
                }
                Value::Bag(b) => {
                    let mut kv = Vec::new();
                    for (k, v) in b.iter() {
                        kv.push(Value::str(k.clone()));
                        kv.push(Value::Int(*v));
                    }
                    Some(Ok(Value::array(kv)))
                }
                Value::Mix(m) => {
                    let mut kv = Vec::new();
                    for (k, v) in m.iter() {
                        kv.push(Value::str(k.clone()));
                        kv.push(Value::Num(*v));
                    }
                    Some(Ok(Value::array(kv)))
                }
                Value::Enum { key, value, .. } => Some(Ok(Value::Seq(Arc::new(vec![
                    Value::str(key.resolve()),
                    value.to_value(),
                ])))),
                Value::Package(_) => None, // let runtime handle (may be enum type)
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
                Value::Pair(_, _) | Value::ValuePair(_, _) => {
                    Some(Ok(Value::array(vec![target.clone()])))
                }
                Value::Package(_) => None, // let runtime handle (may be enum type)
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
                        Value::str(format!(
                            "Cannot pair up odd number of elements ({})",
                            items.len()
                        )),
                    );
                    let ex = Value::make_instance(Symbol::intern("X::Pairup::OddNumber"), attrs);
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
        "antipairs" => {
            if crate::runtime::utils::is_shaped_array(target) {
                let indexed = crate::runtime::utils::shaped_array_indexed_leaves(target);
                let pairs: Vec<Value> = indexed
                    .into_iter()
                    .map(|(idx, val)| {
                        let key = Value::array(idx.into_iter().map(Value::Int).collect());
                        make_inverted_pair(val, key)
                    })
                    .collect();
                return Some(Ok(Value::array(pairs)));
            }
            match target {
                Value::Hash(items) => Some(Ok(Value::array(
                    items
                        .iter()
                        .map(|(k, v)| match v {
                            Value::Str(s) => {
                                Value::Pair(s.to_string(), Box::new(Value::str(k.clone())))
                            }
                            _ => Value::ValuePair(
                                Box::new(v.clone()),
                                Box::new(Value::str(k.clone())),
                            ),
                        })
                        .collect(),
                ))),
                Value::Bag(items) => Some(Ok(Value::array(
                    items
                        .iter()
                        .map(|(k, v)| {
                            Value::ValuePair(
                                Box::new(Value::Int(*v)),
                                Box::new(Value::str(k.clone())),
                            )
                        })
                        .collect(),
                ))),
                Value::Set(items) => Some(Ok(Value::array(
                    items
                        .iter()
                        .map(|k| {
                            Value::ValuePair(
                                Box::new(Value::Bool(true)),
                                Box::new(Value::str(k.clone())),
                            )
                        })
                        .collect(),
                ))),
                Value::Mix(items) => Some(Ok(Value::array(
                    items
                        .iter()
                        .map(|(k, v)| {
                            let weight =
                                crate::value::make_rat((*v * 1_000_000.0) as i64, 1_000_000);
                            // Simplify to Int if the weight is a whole number
                            let weight_val = if v.fract() == 0.0 {
                                Value::Int(*v as i64)
                            } else {
                                weight
                            };
                            Value::ValuePair(Box::new(weight_val), Box::new(Value::str(k.clone())))
                        })
                        .collect(),
                ))),
                Value::Package(_) => None, // let runtime handle (may be enum type)
                v if v.is_range() => {
                    let values = crate::runtime::utils::value_to_list(v);
                    Some(Ok(Value::array(positional_antipairs(&values))))
                }
                other => {
                    let values = crate::runtime::utils::value_to_list(other);
                    Some(Ok(Value::array(positional_antipairs(&values))))
                }
            }
        }
        "kxxv" => match target {
            Value::Bag(items) => {
                let mut result = Vec::new();
                for (k, count) in items.iter() {
                    for _ in 0..*count {
                        result.push(Value::str(k.clone()));
                    }
                }
                Some(Ok(Value::array(result)))
            }
            Value::Mix(items) => {
                let mut result = Vec::new();
                for (k, weight) in items.iter() {
                    let count = weight.floor() as i64;
                    for _ in 0..count.max(0) {
                        result.push(Value::str(k.clone()));
                    }
                }
                Some(Ok(Value::array(result)))
            }
            Value::Set(items) => {
                // Set is like Bag with all counts = 1
                Some(Ok(Value::array(
                    items.iter().map(|k| Value::str(k.clone())).collect(),
                )))
            }
            Value::Hash(items) => {
                let mut result = Vec::new();
                for (k, v) in items.iter() {
                    let count = match v {
                        Value::Int(i) => *i,
                        _ => v.to_f64() as i64,
                    };
                    for _ in 0..count.max(0) {
                        result.push(Value::str(k.clone()));
                    }
                }
                Some(Ok(Value::array(result)))
            }
            _ => None,
        },
        "invert" => invert_value(target).map(Ok),
        "total" => match target {
            Value::Set(s) => Some(Ok(Value::Int(s.len() as i64))),
            Value::Bag(b) => Some(Ok(Value::Int(b.values().sum::<i64>()))),
            Value::Mix(m) => {
                let (n, d) = f64_to_rat(m.values().sum::<f64>());
                Some(Ok(crate::value::make_rat(n, d)))
            }
            _ => None,
        },
        "minmax" => match target {
            Value::Array(items, ..) if !items.is_empty() => {
                // Collect all candidates, extracting Range endpoints
                let mut candidates = Vec::new();
                for item in items.iter() {
                    crate::runtime::builtins_collection::collect_minmax_candidates_pub(
                        item,
                        &mut candidates,
                    );
                }
                if candidates.is_empty() {
                    Some(Ok(Value::GenericRange {
                        start: Arc::new(Value::Num(f64::INFINITY)),
                        end: Arc::new(Value::Num(f64::NEG_INFINITY)),
                        excl_start: false,
                        excl_end: false,
                    }))
                } else {
                    let mut min = &candidates[0];
                    let mut max = &candidates[0];
                    for item in &candidates[1..] {
                        if runtime::compare_values(item, min) < 0 {
                            min = item;
                        }
                        if runtime::compare_values(item, max) > 0 {
                            max = item;
                        }
                    }
                    Some(Ok(
                        crate::runtime::builtins_collection::make_inclusive_range_pub(
                            min.clone(),
                            max.clone(),
                        ),
                    ))
                }
            }
            Value::Array(..) => {
                // Empty array: return Inf..-Inf
                Some(Ok(Value::GenericRange {
                    start: Arc::new(Value::Num(f64::INFINITY)),
                    end: Arc::new(Value::Num(f64::NEG_INFINITY)),
                    excl_start: false,
                    excl_end: false,
                }))
            }
            _ => None,
        },
        "sum" => match target {
            Value::Array(items, ..) => {
                // Check for non-numeric strings first
                for item in items.iter() {
                    if let Value::Str(s) = item {
                        let trimmed = s.trim();
                        if trimmed.parse::<f64>().is_err() {
                            let reason =
                                "base-10 number must begin with valid digits or '.'".to_string();
                            let msg =
                                format!("Cannot convert string '{}' to number: {}", s, reason);
                            let mut attrs = std::collections::HashMap::new();
                            attrs.insert("source".to_string(), Value::str(s.to_string()));
                            attrs.insert("reason".to_string(), Value::str(reason));
                            attrs.insert("pos".to_string(), Value::Int(0));
                            attrs.insert(
                                "target-name".to_string(),
                                Value::str("Numeric".to_string()),
                            );
                            attrs.insert("message".to_string(), Value::str(msg.clone()));
                            let ex = Value::make_instance(
                                crate::symbol::Symbol::intern("X::Str::Numeric"),
                                attrs,
                            );
                            let mut err = RuntimeError::new(&msg);
                            err.exception = Some(Box::new(ex));
                            return Some(Err(err));
                        }
                    }
                }
                let has_float = items
                    .iter()
                    .any(|v| matches!(v, Value::Num(_) | Value::Rat(_, _) | Value::Str(_)));
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
            // Integer ranges: use Gauss formula for O(1) sum
            Value::Range(a, b) => {
                if a > b {
                    Some(Ok(Value::Int(0)))
                } else {
                    let n = *b - *a + 1;
                    // n * (a + b) / 2, but careful about overflow
                    let sum = if (*a + *b) % 2 == 0 {
                        ((*a + *b) / 2) * n
                    } else {
                        (*a + *b) * (n / 2)
                    };
                    Some(Ok(Value::Int(sum)))
                }
            }
            Value::RangeExcl(a, b) => {
                // a ..^ b means a to b-1 inclusive
                if a >= b {
                    Some(Ok(Value::Int(0)))
                } else {
                    let end = *b - 1;
                    let n = end - *a + 1;
                    let sum = if (*a + end) % 2 == 0 {
                        ((*a + end) / 2) * n
                    } else {
                        (*a + end) * (n / 2)
                    };
                    Some(Ok(Value::Int(sum)))
                }
            }
            Value::RangeExclStart(a, b) => {
                // a ^.. b means a+1 to b inclusive
                let start = *a + 1;
                if start > *b {
                    Some(Ok(Value::Int(0)))
                } else {
                    let n = *b - start + 1;
                    let sum = if (start + *b) % 2 == 0 {
                        ((start + *b) / 2) * n
                    } else {
                        (start + *b) * (n / 2)
                    };
                    Some(Ok(Value::Int(sum)))
                }
            }
            Value::RangeExclBoth(a, b) => {
                // a ^..^ b means a+1 to b-1 inclusive
                let start = *a + 1;
                let end = *b - 1;
                if start > end {
                    Some(Ok(Value::Int(0)))
                } else {
                    let n = end - start + 1;
                    let sum = if (start + end) % 2 == 0 {
                        ((start + end) / 2) * n
                    } else {
                        (start + end) * (n / 2)
                    };
                    Some(Ok(Value::Int(sum)))
                }
            }
            Value::GenericRange {
                start,
                end,
                excl_start,
                excl_end,
            } => {
                // Check if endpoints are integer-valued (using BigInt for arbitrary precision)
                let start_bi = value_as_bigint(start);
                let end_bi = value_as_bigint(end);

                if let (Some(a), Some(b)) = (start_bi, end_bi) {
                    // Integer-valued range: use Gauss formula with BigInt
                    let one = NumBigInt::from(1);
                    let two = NumBigInt::from(2);
                    let zero = NumBigInt::from(0);
                    let effective_start = if *excl_start { &a + &one } else { a };
                    let effective_end = if *excl_end { &b - &one } else { b };
                    if effective_start > effective_end {
                        Some(Ok(Value::Int(0)))
                    } else {
                        let n = &effective_end - &effective_start + &one;
                        let s_plus = &effective_start + &effective_end;
                        let sum = if &s_plus % &two == zero {
                            (&s_plus / &two) * &n
                        } else {
                            &s_plus * (&n / &two)
                        };
                        // Try to fit in i64, otherwise return BigInt
                        if let Ok(val) = i64::try_from(&sum) {
                            Some(Ok(Value::Int(val)))
                        } else {
                            Some(Ok(Value::BigInt(Arc::new(sum))))
                        }
                    }
                } else {
                    // Non-integer range: convert to list and sum
                    let items = runtime::value_to_list(target);
                    let has_rat = items.iter().any(|v| matches!(v, Value::Rat(_, _)));
                    if has_rat {
                        let mut num: i64 = 0;
                        let mut den: i64 = 1;
                        for item in &items {
                            let (in_num, in_den) = match item {
                                Value::Rat(n, d) => (*n, *d),
                                Value::Int(n) => (*n, 1),
                                _ => (runtime::to_int(item), 1),
                            };
                            num = num * in_den + in_num * den;
                            den *= in_den;
                            let g = gcd_u64(num.unsigned_abs(), den.unsigned_abs()) as i64;
                            if g > 1 {
                                num /= g;
                                den /= g;
                            }
                        }
                        if den == 1 {
                            Some(Ok(Value::Int(num)))
                        } else {
                            Some(Ok(Value::Rat(num, den)))
                        }
                    } else {
                        let total: i64 = items.iter().map(runtime::to_int).sum();
                        Some(Ok(Value::Int(total)))
                    }
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
                Some(Ok(Value::Seq(std::sync::Arc::new(result))))
            }
            _ => None,
        },
        "permutations" => {
            let items = if crate::runtime::utils::is_shaped_array(target) {
                crate::runtime::utils::shaped_array_leaves(target)
            } else {
                target
                    .as_list_items()
                    .map(|items| items.to_vec())
                    .unwrap_or_else(|| runtime::value_to_list(target))
            };
            Some(Ok(Value::Seq(all_permutations(&items).into())))
        }
        "combinations" => {
            let items = if crate::runtime::utils::is_shaped_array(target) {
                crate::runtime::utils::shaped_array_leaves(target)
            } else {
                target
                    .as_list_items()
                    .map(|items| items.to_vec())
                    .unwrap_or_else(|| runtime::value_to_list(target))
            };
            Some(Ok(Value::Seq(combinations_all(&items).into())))
        }
        "cache" => {
            let items = target
                .as_list_items()
                .map(|items| items.to_vec())
                .unwrap_or_else(|| runtime::value_to_list(target));
            Some(Ok(Value::array(items)))
        }
        _ => None,
    }
}
