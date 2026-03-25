use super::*;
use crate::symbol::Symbol;
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
        let idx_list: Vec<Value> = idx_items.as_ref().clone();
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
    Value::Array(std::sync::Arc::new(indices.to_vec()), ArrayKind::List)
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

impl Interpreter {
    /// Handle `$obj.method<key> = value` — index assignment through a method accessor.
    /// Gets the current container (hash/array) via the accessor, modifies it, writes back.
    pub(super) fn builtin_index_assign_method_lvalue(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 5 {
            return Err(RuntimeError::new(
                "__mutsu_index_assign_method_lvalue expects target, method, index, value, var_name",
            ));
        }
        let target = args[0].clone();
        let method = args[1].to_string_value();
        let index = args[2].clone();
        let value = args[3].clone();
        let var_name = args[4].to_string_value();

        // Get the current container via the accessor
        let current = self.call_method_with_values(target.clone(), &method, Vec::new())?;

        // Check if index is multi-dimensional (array of indices like [2, 1] from [2;1])
        let dims: Vec<usize> = if let Value::Array(ref items, ..) = index {
            items
                .iter()
                .map(|v| crate::runtime::to_int(v) as usize)
                .collect()
        } else {
            Vec::new()
        };

        // Modify the container
        let updated = if dims.len() >= 2 {
            // Multi-dimensional index assignment (e.g., $c.a[2;1] = value)
            Self::multidim_assign_nested(current, &dims, value.clone())?
        } else {
            let key = index.to_string_value();
            match current {
                Value::Hash(ref h) => {
                    let mut new_hash = (**h).clone();
                    new_hash.insert(key, value.clone());
                    Value::hash(new_hash)
                }
                Value::Array(ref items, kind) => {
                    let idx = crate::runtime::to_int(&index) as usize;
                    let mut new_items = (**items).clone();
                    if idx >= new_items.len() {
                        if crate::runtime::utils::is_shaped_array(&current) {
                            return Err(RuntimeError::new("Index out of bounds"));
                        }
                        new_items.resize(
                            idx + 1,
                            Value::Package(crate::symbol::Symbol::intern("Any")),
                        );
                    }
                    new_items[idx] = value.clone();
                    Value::Array(std::sync::Arc::new(new_items), kind)
                }
                _ => return Ok(value),
            }
        };

        // Write back via the setter
        self.assign_method_lvalue_with_values(
            if var_name.is_empty() {
                None
            } else {
                Some(var_name.as_str())
            },
            target,
            &method,
            Vec::new(),
            updated,
        )?;
        Ok(value)
    }

    /// Assign a value into a nested multi-dimensional array structure.
    /// `dims` contains the indices for each dimension, e.g. [2, 1] for @a[2;1].
    /// Checks bounds against the shaped array dimensions.
    pub(super) fn multidim_assign_nested(
        container: Value,
        dims: &[usize],
        value: Value,
    ) -> Result<Value, RuntimeError> {
        if dims.is_empty() {
            return Ok(value);
        }
        // Check bounds against shape if this is a shaped array
        let shape = crate::runtime::utils::shaped_array_shape(&container);
        if let Some(ref shape) = shape {
            for (i, &idx) in dims.iter().enumerate() {
                if i < shape.len() && idx >= shape[i] {
                    return Err(RuntimeError::new("Index out of bounds"));
                }
            }
        }
        match container {
            Value::Array(ref items, kind) => {
                let idx = dims[0];
                let mut new_items = (**items).clone();
                if idx >= new_items.len() {
                    new_items.resize(
                        idx + 1,
                        Value::Package(crate::symbol::Symbol::intern("Any")),
                    );
                }
                if dims.len() == 1 {
                    new_items[idx] = value;
                } else {
                    let inner = new_items[idx].clone();
                    new_items[idx] = Self::multidim_assign_nested(inner, &dims[1..], value)?;
                }
                let result = Value::Array(std::sync::Arc::new(new_items), kind);
                // Preserve the shape registration on the new Arc so subsequent
                // bounds checks (via shaped_array_shape) still work.
                if let Some(ref shape) = shape {
                    crate::runtime::utils::mark_shaped_array(&result, Some(shape));
                }
                Ok(result)
            }
            _ => {
                // If it's not an array, wrap the assignment in a fresh array
                if dims.len() == 1 {
                    let idx = dims[0];
                    let mut new_items =
                        vec![Value::Package(crate::symbol::Symbol::intern("Any")); idx + 1];
                    new_items[idx] = value;
                    Ok(Value::real_array(new_items))
                } else {
                    Err(RuntimeError::new(
                        "Multi-dimensional index on non-array container",
                    ))
                }
            }
        }
    }

    pub(super) fn builtin_assign_method_lvalue(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 4 {
            return Err(RuntimeError::new(
                "__mutsu_assign_method_lvalue expects target, method name, method args, and value",
            ));
        }
        let target = args[0].clone();
        let method = args[1].to_string_value();
        let method_args = match &args[2] {
            Value::Array(items, ..) => items.to_vec(),
            Value::Nil => Vec::new(),
            other => vec![other.clone()],
        };
        let value = args[3].clone();
        let target_var = args.get(4).and_then(|v| {
            let name = v.to_string_value();
            if name.is_empty() { None } else { Some(name) }
        });
        self.assign_method_lvalue_with_values(
            target_var.as_deref(),
            target,
            &method,
            method_args,
            value,
        )
    }

    pub(super) fn builtin_subscript_adverb(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() < 3 {
            return Err(RuntimeError::new(
                "__mutsu_subscript_adverb expects target, index, and mode",
            ));
        }
        let target = args[0].clone();
        let index = args[1].clone();
        let mode = args[2].to_string_value();
        let var_name = args
            .get(3)
            .map(Value::to_string_value)
            .filter(|s| !s.is_empty());
        let mut delete_after = false;
        let mut adverb_cond: Option<bool> = None;
        let mut skip_next = false;
        for (i, extra) in args.iter().skip(4).enumerate() {
            if skip_next {
                skip_next = false;
                continue;
            }
            match extra {
                Value::Pair(key, value) if key == "delete" => {
                    delete_after = value.truthy();
                }
                Value::ValuePair(key, value) if key.to_string_value() == "delete" => {
                    delete_after = value.truthy();
                }
                Value::Str(s) if s.as_ref() == "__adverb_cond__" => {
                    // The next argument is the dynamic condition expression value.
                    if let Some(cond_val) = args.get(4 + i + 1) {
                        adverb_cond = Some(cond_val.truthy());
                    }
                    skip_next = true;
                }
                _ => {}
            }
        }

        let (kind, keep_missing) = match mode.as_str() {
            "kv" => ("kv", false),
            "not-kv" | "kv0" => ("kv", true),
            "p" => ("p", false),
            "not-p" | "p0" => ("p", true),
            "k" => ("k", false),
            "not-k" | "k0" => ("k", true),
            "v" => ("v", false),
            "not-v" | "v0" => ("v", true),
            _ => return Ok(Value::Nil),
        };
        // When a dynamic condition was provided (e.g. `:k($ok)`),
        // override keep_missing based on the condition's truthiness.
        // If the condition is false, it's like `:!k` (keep_missing = true).
        // If the condition is true, it's like `:k` (keep_missing = false).
        let keep_missing = if let Some(cond) = adverb_cond {
            !cond
        } else {
            keep_missing
        };

        let mut indices = match index {
            Value::Array(items, ..) => items.to_vec(),
            other => vec![other],
        };
        if matches!(indices.first(), Some(Value::Whatever))
            || matches!(indices.first(), Some(Value::Num(f)) if f.is_infinite() && *f > 0.0)
        {
            indices = match &target {
                Value::Array(items, ..) => (0..items.len())
                    .map(|i| Value::Int(i as i64))
                    .collect::<Vec<_>>(),
                Value::Hash(map) => map
                    .keys()
                    .map(|k| Value::str(k.clone()))
                    .collect::<Vec<_>>(),
                _ => Vec::new(),
            };
        }
        let is_multi = indices.len() != 1;

        let mut rows: Vec<(Value, Value, bool)> = Vec::with_capacity(indices.len());
        match &target {
            Value::Array(items, ..) => {
                let bound_map = var_name
                    .as_ref()
                    .and_then(|name| self.env.get(&format!("__mutsu_initialized_index::{name}")))
                    .and_then(|v| match v {
                        Value::Hash(map) => Some(map),
                        _ => None,
                    });
                // Get container default value (from `is default(...)` trait)
                let container_default = var_name
                    .as_ref()
                    .and_then(|name| self.var_default(name).cloned());
                let type_constraint_default = var_name
                    .as_ref()
                    .and_then(|name| self.var_type_constraint(name))
                    .map(|t| Value::Package(Symbol::intern(&t)));
                let missing_value = container_default
                    .or(type_constraint_default)
                    .unwrap_or_else(|| Value::Package(Symbol::intern("Any")));
                for idx in &indices {
                    let i = match idx {
                        Value::Int(i) => *i,
                        Value::Num(f) => *f as i64,
                        _ => idx.to_string_value().parse::<i64>().unwrap_or(-1),
                    };
                    let key = Value::Int(i);
                    if i < 0 || i as usize >= items.len() {
                        rows.push((key, missing_value.clone(), false));
                        continue;
                    }
                    let ui = i as usize;
                    let exists = if let Some(map) = bound_map {
                        if map.contains_key(&i.to_string()) {
                            true
                        } else {
                            !matches!(&items[ui], Value::Package(name) if name == "Any")
                        }
                    } else {
                        true
                    };
                    rows.push((key, items[ui].clone(), exists));
                }
            }
            Value::Hash(map) => {
                let default_type = var_name
                    .as_ref()
                    .and_then(|name| self.var_type_constraint(name))
                    .unwrap_or_else(|| "Any".to_string());
                for idx in &indices {
                    let key_str = idx.to_string_value();
                    let key =
                        super::builtins_collection::builtin_val(&[Value::str(key_str.clone())]);
                    let exists = map.contains_key(&key_str);
                    let value = map
                        .get(&key_str)
                        .cloned()
                        .unwrap_or_else(|| Value::Package(Symbol::intern(&default_type)));
                    rows.push((key, value, exists));
                }
            }
            _ => return Ok(Value::Nil),
        }

        if delete_after && let Some(var_name) = var_name.as_ref() {
            // Get hole type before mutable borrow
            let hole_type = self
                .var_type_constraint(var_name)
                .unwrap_or_else(|| "Any".to_string());
            if let Some(container) = self.env.get_mut(var_name) {
                match container {
                    Value::Hash(map) => {
                        let h = std::sync::Arc::make_mut(map);
                        for idx in &indices {
                            h.remove(&idx.to_string_value());
                        }
                    }
                    Value::Array(items, ..) => {
                        let hole_value = Value::Package(crate::symbol::Symbol::intern(&hole_type));
                        let arr = std::sync::Arc::make_mut(items);
                        for idx in &indices {
                            let i = match idx {
                                Value::Int(i) => *i,
                                Value::Num(f) => *f as i64,
                                _ => idx.to_string_value().parse::<i64>().unwrap_or(-1),
                            };
                            if i >= 0 && (i as usize) < arr.len() {
                                arr[i as usize] = hole_value.clone();
                            }
                        }
                        // Trim trailing holes
                        while let Some(last) = arr.last() {
                            let is_hole = match last {
                                Value::Nil => true,
                                Value::Package(name) => name == "Any" || name == hole_type.as_str(),
                                _ => false,
                            };
                            if is_hole {
                                arr.pop();
                            } else {
                                break;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        if !is_multi {
            let Some((key, value, exists)) = rows.into_iter().next() else {
                return Ok(Value::Nil);
            };
            if !keep_missing && !exists {
                return Ok(Value::array(Vec::new()));
            }
            return Ok(match kind {
                "kv" => Value::array(vec![key, value]),
                "p" => Value::ValuePair(Box::new(key), Box::new(value)),
                "k" => key,
                "v" => value,
                _ => Value::Nil,
            });
        }

        let mut out = Vec::new();
        for (key, value, exists) in rows {
            if !keep_missing && !exists {
                continue;
            }
            match kind {
                "kv" => {
                    out.push(key);
                    out.push(value);
                }
                "p" => out.push(Value::ValuePair(Box::new(key), Box::new(value))),
                "k" => out.push(key),
                "v" => out.push(value),
                _ => {}
            }
        }
        Ok(Value::array(out))
    }
}
