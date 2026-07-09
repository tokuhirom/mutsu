use super::*;
use crate::value::ArrayKind;

/// Navigate a multi-dimensional array to get a value.
pub(super) fn multidim_index(target: &Value, indices: &[Value]) -> Value {
    // A file-scoped `@a`/`%h` shared across frames (or a nested cell-promoted
    // element) is a `ContainerRef` cell; an itemized container is a `Scalar`.
    // Read through both so the Array/Hash navigation below sees the container.
    if matches!(
        target.view(),
        ValueView::ContainerRef(_) | ValueView::Scalar(_)
    ) {
        return target.with_deref(|inner| {
            let inner = inner.descalarize();
            multidim_index(inner, indices)
        });
    }
    if indices.is_empty() {
        return target.clone();
    }
    let head = &indices[0];
    // Whatever (*) means "all elements of this dimension"
    if matches!(head.view(), ValueView::Whatever) {
        match target.view() {
            ValueView::Array(items, ..) => {
                let mut out = Vec::with_capacity(items.len());
                for item in items.iter() {
                    out.push(multidim_index(item, &indices[1..]));
                }
                return Value::array(out);
            }
            // Hash level: `*` selects every value at this level.
            ValueView::Hash(map) => {
                let mut out = Vec::with_capacity(map.len());
                for v in map.values() {
                    out.push(multidim_index(v, &indices[1..]));
                }
                return Value::array(out);
            }
            _ => return Value::NIL,
        }
    }
    // List/Array as index means "multiple indices in this dimension"
    if let ValueView::Array(idx_items, ..) = head.view() {
        let mut out = Vec::with_capacity(idx_items.len());
        for idx in idx_items.iter() {
            let mut sub_indices = vec![idx.clone()];
            sub_indices.extend_from_slice(&indices[1..]);
            out.push(multidim_index(target, &sub_indices));
        }
        return Value::array(out);
    }
    // Hash multi-dim indexing: %h{key1;key2;...}
    if let ValueView::Hash(map) = target.view() {
        let key = head.to_string_value();
        return match map.get(&key) {
            Some(val) => multidim_index(val, &indices[1..]),
            None => Value::NIL,
        };
    }
    let ValueView::Array(items, ..) = target.view() else {
        return Value::NIL;
    };
    let i = match head.view() {
        ValueView::Int(n) => {
            if n < 0 {
                let len = items.len() as i64;
                if -n > len {
                    return Value::NIL;
                }
                (len + n) as usize
            } else {
                n as usize
            }
        }
        ValueView::Str(s) => s.parse::<usize>().unwrap_or(0),
        ValueView::Num(f) => f as usize,
        ValueView::Rat(n, d) => (n as f64 / d as f64) as usize,
        ValueView::FatRat(n, d) => (n as f64 / d as f64) as usize,
        ValueView::BigRat(_, _) => to_float_value(head).unwrap_or(0.0) as usize,
        _ => return Value::NIL,
    };
    if i >= items.len() {
        return Value::NIL;
    }
    multidim_index(&items[i], &indices[1..])
}

/// Delete element from a multi-dimensional array, returning the deleted value.
pub(super) fn multidim_delete(target: &mut Value, indices: &[Value]) -> Value {
    let default = || Value::package(crate::symbol::Symbol::intern("Any"));
    // A file-scoped `@a`/`%h` shared across frames (or a nested cell-promoted
    // element) is a `ContainerRef` cell: mutate the inner container in place
    // through the lock so every alias (env entry + caller local slot, which
    // share the same `Arc`) observes the delete.
    if let ValueView::ContainerRef(cell) = target.view() {
        let mut inner = cell.lock().unwrap();
        return multidim_delete(&mut inner, indices);
    }
    if indices.is_empty() {
        let old = target.clone();
        *target = default();
        return old;
    }
    let head = &indices[0];
    // Whatever (*) means "all elements of this dimension"
    if matches!(head.view(), ValueView::Whatever) {
        // Hash level: `*` deletes through every value at this level (a
        // terminal `*` removes all entries).
        if matches!(target.view(), ValueView::Hash(..)) {
            return target
                .with_hash_mut(|map| {
                    let map_mut = crate::gc::Gc::make_mut(map);
                    if indices.len() == 1 {
                        let out: Vec<Value> = map_mut.drain().map(|(_, v)| v).collect();
                        Value::array(out)
                    } else {
                        let mut out = Vec::with_capacity(map_mut.len());
                        for v in map_mut.values_mut() {
                            out.push(multidim_delete(v, &indices[1..]));
                        }
                        Value::array(out)
                    }
                })
                .unwrap_or_else(default);
        }
        return target
            .with_array_mut(|items, _kind| {
                let items = crate::gc::Gc::make_mut(items);
                let mut out = Vec::with_capacity(items.len());
                for item in items.iter_mut() {
                    out.push(multidim_delete(item, &indices[1..]));
                }
                // Truncate trailing Any values
                while items
                    .last()
                    .is_some_and(|v| matches!(v.view(), ValueView::Package(s) if s == "Any"))
                {
                    items.pop();
                }
                Value::array(out)
            })
            .unwrap_or_else(default);
    }
    // List/Array as index means "multiple indices in this dimension"
    if let ValueView::Array(idx_items, ..) = head.view() {
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
    if matches!(target.view(), ValueView::Hash(..)) {
        let key = head.to_string_value();
        return target
            .with_hash_mut(|map| {
                if indices.len() == 1 {
                    let map_mut = crate::gc::Gc::make_mut(map);
                    return map_mut.remove(&key).unwrap_or_else(default);
                }
                let map_mut = crate::gc::Gc::make_mut(map);
                match map_mut.get_mut(&key) {
                    Some(inner) => multidim_delete(inner, &indices[1..]),
                    None => default(),
                }
            })
            .unwrap();
    }
    target
        .with_array_mut(|items_gc, _kind| {
            let i = match head.view() {
                ValueView::Int(n) => {
                    if n < 0 {
                        let len = items_gc.len() as i64;
                        if -n > len {
                            return default();
                        }
                        (len + n) as usize
                    } else {
                        n as usize
                    }
                }
                ValueView::Str(s) => s.parse::<usize>().unwrap_or(0),
                ValueView::Num(f) => f as usize,
                _ => return default(),
            };
            let items = crate::gc::Gc::make_mut(items_gc);
            if i >= items.len() {
                return default();
            }
            if indices.len() == 1 {
                let old = items[i].clone();
                items[i] = default();
                // Truncate trailing Any values
                while items
                    .last()
                    .is_some_and(|v| matches!(v.view(), ValueView::Package(s) if s == "Any"))
                {
                    items.pop();
                }
                old
            } else {
                multidim_delete(&mut items[i], &indices[1..])
            }
        })
        .unwrap_or_else(default)
}

/// Convert an Array value to a List (changes ArrayKind).
pub(super) fn array_to_list(value: Value) -> Value {
    if matches!(value.view(), ValueView::Array(..)) {
        let (items, _) = value.into_array().unwrap();
        Value::array_with_kind(items, ArrayKind::List)
    } else {
        value
    }
}

/// Build a key tuple from multi-dimensional indices.
pub(super) fn make_key_tuple(indices: &[Value]) -> Value {
    if indices.len() == 1 {
        return indices[0].clone();
    }
    Value::array_with_kind(
        crate::gc::Gc::new(crate::value::ArrayData::new(indices.to_vec())),
        ArrayKind::List,
    )
}

/// Collect (path, value) leaves from a multi-dimensional array or hash,
/// expanding Whatever and Array indices along the way. Path elements are the
/// concrete index/key values (`Int` for array levels, `Str` for hash keys), so
/// the `:k`/`:kv`/`:p` adverbs can rebuild the key tuple losslessly.
pub(super) fn multidim_collect_leaves(
    target: &Value,
    indices: &[Value],
    prefix: &[Value],
    out: &mut Vec<(Vec<Value>, Value)>,
) {
    if let ValueView::ContainerRef(_) | ValueView::Scalar(_) = target.view() {
        return target.with_deref(|inner| {
            let inner = inner.descalarize();
            multidim_collect_leaves(inner, indices, prefix, out)
        });
    }
    if indices.is_empty() {
        out.push((prefix.to_vec(), target.clone()));
        return;
    }
    let head = &indices[0];
    let rest = &indices[1..];

    if matches!(head.view(), ValueView::Whatever) {
        match target.view() {
            ValueView::Array(items, ..) => {
                for (i, item) in items.iter().enumerate() {
                    let mut p = prefix.to_vec();
                    p.push(Value::int(i as i64));
                    multidim_collect_leaves(item, rest, &p, out);
                }
            }
            // Hash level: `*` walks every entry, recording the (typed) key.
            ValueView::Hash(map) => {
                for (k, v) in map.iter() {
                    let mut p = prefix.to_vec();
                    p.push(map.typed_key(k));
                    multidim_collect_leaves(v, rest, &p, out);
                }
            }
            _ => {}
        }
        return;
    }
    if let ValueView::Array(idx_items, ..) = head.view() {
        for idx in idx_items.iter() {
            let mut p = prefix.to_vec();
            p.push(idx.clone());
            let child = multidim_index(target, std::slice::from_ref(idx));
            multidim_collect_leaves(&child, rest, &p, out);
        }
        return;
    }
    let mut p = prefix.to_vec();
    p.push(head.clone());
    let child = multidim_index(target, std::slice::from_ref(head));
    multidim_collect_leaves(&child, rest, &p, out);
}

/// Build the key tuple for one collected leaf path: a single-dimension path is
/// the bare key; a multi-dimension path is a List of the keys.
pub(super) fn leaf_key_tuple(path: Vec<Value>) -> Value {
    if path.len() == 1 {
        path.into_iter().next().unwrap()
    } else {
        Value::array_with_kind(
            crate::gc::Gc::new(crate::value::ArrayData::new(path)),
            ArrayKind::List,
        )
    }
}

/// Check if any index in the list is a Whatever or an Array (multi-index).
pub(super) fn has_multi_indices(indices: &[Value]) -> bool {
    indices.iter().any(|v| {
        matches!(v.view(), ValueView::Whatever) || matches!(v.view(), ValueView::Array(..))
    })
}
