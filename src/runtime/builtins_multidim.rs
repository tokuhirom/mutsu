use super::*;
use crate::value::ArrayKind;

/// Navigate a multi-dimensional array to get a value.
pub(super) fn multidim_index(target: &Value, indices: &[Value]) -> Value {
    if indices.is_empty() {
        return target.clone();
    }
    let head = &indices[0];
    // Whatever (*) means "all elements of this dimension"
    if matches!(head, Value::Whatever) {
        let Value::Array(items, ..) = target else {
            return Value::Nil;
        };
        let mut out = Vec::with_capacity(items.len());
        for item in items.iter() {
            out.push(multidim_index(item, &indices[1..]));
        }
        return Value::array(out);
    }
    // List/Array as index means "multiple indices in this dimension"
    if let Value::Array(idx_items, ..) = head {
        let mut out = Vec::with_capacity(idx_items.len());
        for idx in idx_items.iter() {
            let mut sub_indices = vec![idx.clone()];
            sub_indices.extend_from_slice(&indices[1..]);
            out.push(multidim_index(target, &sub_indices));
        }
        return Value::array(out);
    }
    // Hash multi-dim indexing: %h{key1;key2;...}
    if let Value::Hash(map, ..) = target {
        let key = head.to_string_value();
        return match map.get(&key) {
            Some(val) => multidim_index(val, &indices[1..]),
            None => Value::Nil,
        };
    }
    let Value::Array(items, ..) = target else {
        return Value::Nil;
    };
    let i = match head {
        Value::Int(n) => {
            let n = *n;
            if n < 0 {
                let len = items.len() as i64;
                if -n > len {
                    return Value::Nil;
                }
                (len + n) as usize
            } else {
                n as usize
            }
        }
        Value::Str(s) => s.parse::<usize>().unwrap_or(0),
        Value::Num(f) => *f as usize,
        Value::Rat(n, d) => (*n as f64 / *d as f64) as usize,
        Value::FatRat(n, d) => (*n as f64 / *d as f64) as usize,
        Value::BigRat(_, _) => to_float_value(head).unwrap_or(0.0) as usize,
        _ => return Value::Nil,
    };
    if i >= items.len() {
        return Value::Nil;
    }
    multidim_index(&items[i], &indices[1..])
}

/// Delete element from a multi-dimensional array, returning the deleted value.
pub(super) fn multidim_delete(target: &mut Value, indices: &[Value]) -> Value {
    let default = || Value::Package(crate::symbol::Symbol::intern("Any"));
    if indices.is_empty() {
        let old = target.clone();
        *target = default();
        return old;
    }
    let head = &indices[0];
    // Whatever (*) means "all elements of this dimension"
    if matches!(head, Value::Whatever) {
        let Value::Array(items, ..) = target else {
            return default();
        };
        let items = std::sync::Arc::make_mut(items);
        let mut out = Vec::with_capacity(items.len());
        for item in items.iter_mut() {
            out.push(multidim_delete(item, &indices[1..]));
        }
        // Truncate trailing Any values
        while items
            .last()
            .is_some_and(|v| matches!(v, Value::Package(s) if s == "Any"))
        {
            items.pop();
        }
        return Value::array(out);
    }
    // List/Array as index means "multiple indices in this dimension"
    if let Value::Array(idx_items, ..) = head {
        let idx_list: Vec<Value> = idx_items.as_ref().clone().items;
        let mut out = Vec::with_capacity(idx_list.len());
        for idx in &idx_list {
            let mut sub_indices = vec![idx.clone()];
            sub_indices.extend_from_slice(&indices[1..]);
            out.push(multidim_delete(target, &sub_indices));
        }
        return Value::array(out);
    }
    // Hash multi-dim indexing: %h{key1;key2;...}
    if let Value::Hash(map) = target {
        let key = head.to_string_value();
        if indices.len() == 1 {
            let map_mut = std::sync::Arc::make_mut(map);
            return map_mut.remove(&key).unwrap_or_else(default);
        }
        let map_mut = std::sync::Arc::make_mut(map);
        return match map_mut.get_mut(&key) {
            Some(inner) => multidim_delete(inner, &indices[1..]),
            None => default(),
        };
    }
    let Value::Array(items, ..) = target else {
        return default();
    };
    let i = match head {
        Value::Int(n) => {
            let n = *n;
            if n < 0 {
                let len = items.len() as i64;
                if -n > len {
                    return default();
                }
                (len + n) as usize
            } else {
                n as usize
            }
        }
        Value::Str(s) => s.parse::<usize>().unwrap_or(0),
        Value::Num(f) => *f as usize,
        _ => return default(),
    };
    let items = std::sync::Arc::make_mut(items);
    if i >= items.len() {
        return default();
    }
    if indices.len() == 1 {
        let old = items[i].clone();
        items[i] = default();
        // Truncate trailing Any values
        while items
            .last()
            .is_some_and(|v| matches!(v, Value::Package(s) if s == "Any"))
        {
            items.pop();
        }
        old
    } else {
        multidim_delete(&mut items[i], &indices[1..])
    }
}

/// Convert an Array value to a List (changes ArrayKind).
pub(super) fn array_to_list(value: Value) -> Value {
    match value {
        Value::Array(items, _) => Value::Array(items, ArrayKind::List),
        other => other,
    }
}

/// Build a key tuple from multi-dimensional indices.
pub(super) fn make_key_tuple(indices: &[Value]) -> Value {
    if indices.len() == 1 {
        return indices[0].clone();
    }
    Value::Array(
        std::sync::Arc::new(crate::value::ArrayData::new(indices.to_vec())),
        ArrayKind::List,
    )
}

/// Collect (path, value) leaves from a multi-dimensional array,
/// expanding Whatever and Array indices along the way.
pub(super) fn multidim_collect_leaves(
    target: &Value,
    indices: &[Value],
    prefix: &[i64],
    out: &mut Vec<(Vec<i64>, Value)>,
) {
    if indices.is_empty() {
        out.push((prefix.to_vec(), target.clone()));
        return;
    }
    let head = &indices[0];
    let rest = &indices[1..];

    if matches!(head, Value::Whatever) {
        if let Value::Array(items, ..) = target {
            for (i, item) in items.iter().enumerate() {
                let mut p = prefix.to_vec();
                p.push(i as i64);
                multidim_collect_leaves(item, rest, &p, out);
            }
        }
        return;
    }
    if let Value::Array(idx_items, ..) = head {
        for idx in idx_items.iter() {
            let i = match idx {
                Value::Int(n) => *n,
                _ => idx.to_string_value().parse::<i64>().unwrap_or(0),
            };
            let mut p = prefix.to_vec();
            p.push(i);
            let child = multidim_index(target, std::slice::from_ref(idx));
            multidim_collect_leaves(&child, rest, &p, out);
        }
        return;
    }
    let i = match head {
        Value::Int(n) => *n,
        _ => head.to_string_value().parse::<i64>().unwrap_or(0),
    };
    let mut p = prefix.to_vec();
    p.push(i);
    let child = multidim_index(target, std::slice::from_ref(head));
    multidim_collect_leaves(&child, rest, &p, out);
}

/// Check if any index in the list is a Whatever or an Array (multi-index).
pub(super) fn has_multi_indices(indices: &[Value]) -> bool {
    indices
        .iter()
        .any(|v| matches!(v, Value::Whatever) || matches!(v, Value::Array(..)))
}
