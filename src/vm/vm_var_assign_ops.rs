use super::*;
use crate::symbol::Symbol;
use std::collections::HashMap;
use std::sync::Arc;

impl Interpreter {
    /// Return the default fill value for a native type constraint.
    /// For `int`/`uint` variants returns `Value::Int(0)`,
    /// for `num` variants returns `Value::Num(0.0)`,
    /// for `str` returns `Value::str("")`,
    /// otherwise returns `Value::Package("Any")`.
    pub(crate) fn native_fill_for_constraint(constraint: Option<&str>) -> Value {
        match constraint {
            Some(
                "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16"
                | "uint32" | "uint64" | "byte" | "atomicint",
            ) => Value::Int(0),
            Some("num" | "num32" | "num64") => Value::Num(0.0),
            Some("str") => Value::str(String::new()),
            // A boxed typed array (e.g. `my Int @a`) fills empty slots with the
            // element type's type object, so holes gist as `(Int)` and roundtrip
            // through `.raku`. Strip any smiley/coercion suffix first.
            Some(c) => {
                let base = c
                    .split_once('(')
                    .map_or(c, |(b, _)| b)
                    .trim_end_matches(":D")
                    .trim_end_matches(":U");
                if base.is_empty() || base == "Any" || base == "Mu" || base.contains('[') {
                    Value::Package(Symbol::intern("Any"))
                } else {
                    Value::Package(Symbol::intern(base))
                }
            }
            None => Value::Package(Symbol::intern("Any")),
        }
    }

    /// Auto-vivify `items` to at least `new_len` elements, filling new slots
    /// with `fill`. Uses fallible reservation (`try_reserve`) so an absurd
    /// index (e.g. `@a[9999999999999] = 1`) yields a catchable `X::` instead of
    /// an uncatchable `handle_alloc_error` abort that `try {}` cannot recover
    /// from. (raku aborts with a MoarVM panic on the same input.)
    ///
    /// `pub(crate)` so other fallible allocation sites (e.g. shaped-array
    /// construction in `make_shaped_array`) can reuse the same guard.
    pub(crate) fn autoviv_resize(
        items: &mut Vec<Value>,
        new_len: usize,
        fill: Value,
    ) -> Result<(), RuntimeError> {
        if new_len > items.len() {
            items.try_reserve(new_len - items.len()).map_err(|_| {
                RuntimeError::new(format!(
                    "Cannot autovivify array to {new_len} elements: memory allocation failed"
                ))
            })?;
            items.resize(new_len, fill);
        }
        Ok(())
    }

    pub(super) fn delegated_mixin_attr_key(
        &self,
        mixins: &std::collections::HashMap<String, Value>,
        method_name: &str,
    ) -> Option<String> {
        self.delegated_role_attr_key_from_mixins(mixins, method_name)
    }

    pub(crate) fn assign_mixin_container_slot(
        attr_value: &mut Value,
        idx: &Value,
        val: &Value,
        range_slice: &Option<(Vec<usize>, Vec<Value>)>,
    ) -> Result<bool, RuntimeError> {
        match attr_value {
            Value::Array(items, kind) if !matches!(idx, Value::Str(_)) => {
                let mut updated = (**items).clone();
                if let Some((slice_indices, vals)) = range_slice {
                    if let Some(max_idx) = slice_indices.last().copied()
                        && max_idx >= updated.len()
                    {
                        Self::autoviv_resize(
                            &mut updated,
                            max_idx + 1,
                            Value::Package(Symbol::intern("Any")),
                        )?;
                    }
                    for (offset, i) in slice_indices.iter().enumerate() {
                        updated[*i] = vals.get(offset).cloned().unwrap_or(Value::Nil);
                    }
                } else if let Some(i) = Self::index_to_usize(idx) {
                    Self::autoviv_resize(
                        &mut updated,
                        i + 1,
                        Value::Package(Symbol::intern("Any")),
                    )?;
                    updated[i] = val.clone();
                } else {
                    return Ok(false);
                }
                *attr_value = Value::Array(Arc::new(updated), *kind);
                Ok(true)
            }
            Value::Hash(hash) if matches!(idx, Value::Str(_)) => {
                let mut updated = (**hash).clone();
                updated.insert(idx.to_string_value(), val.clone());
                *attr_value = Value::Hash(Value::hash_arc(updated));
                Ok(true)
            }
            Value::Nil if !matches!(idx, Value::Str(_)) => {
                let mut updated = Vec::new();
                if let Some((slice_indices, vals)) = range_slice {
                    if let Some(max_idx) = slice_indices.last().copied() {
                        Self::autoviv_resize(
                            &mut updated,
                            max_idx + 1,
                            Value::Package(Symbol::intern("Any")),
                        )?;
                    }
                    for (offset, i) in slice_indices.iter().enumerate() {
                        updated[*i] = vals.get(offset).cloned().unwrap_or(Value::Nil);
                    }
                } else if let Some(i) = Self::index_to_usize(idx) {
                    Self::autoviv_resize(
                        &mut updated,
                        i + 1,
                        Value::Package(Symbol::intern("Any")),
                    )?;
                    updated[i] = val.clone();
                } else {
                    return Ok(false);
                }
                *attr_value = Value::real_array(updated);
                Ok(true)
            }
            Value::Nil if matches!(idx, Value::Str(_)) => {
                let mut updated = std::collections::HashMap::new();
                updated.insert(idx.to_string_value(), val.clone());
                *attr_value = Value::Hash(Value::hash_arc(updated));
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    /// Unwrap a `__mutsu_bind_index_value` payload to `(value, first_source)`.
    /// `value` is the RHS value being bound; `first_source` is the name of the
    /// single source variable when the bind is the common `LHS := $scalar`
    /// shape. Returns `(val, None)` for a plain (non-bind) value, so callers can
    /// treat the non-bind path unchanged.
    pub(crate) fn unwrap_bind_index_value(val: Value) -> (Value, Option<String>) {
        if let Value::Pair(name, payload) = &val
            && name == "__mutsu_bind_index_value"
        {
            if let Value::Array(items, ..) = payload.as_ref() {
                let value = items.first().cloned().unwrap_or(Value::Nil);
                let source = match items.get(1) {
                    Some(Value::Array(srcs, ..)) => match srcs.first() {
                        Some(Value::Str(s)) if !s.is_empty() => Some((**s).clone()),
                        _ => None,
                    },
                    _ => None,
                };
                return (value, source);
            }
            return ((**payload).clone(), None);
        }
        (val, None)
    }

    pub(crate) fn varref_target(value: &Value) -> Option<(String, Option<usize>)> {
        if let Value::Capture { positional, named } = value
            && positional.is_empty()
            && let Some(Value::Str(name)) = named.get("__mutsu_varref_name")
        {
            let source_index = match named.get("__mutsu_varref_index") {
                Some(Value::Int(i)) if *i >= 0 => Some(*i as usize),
                _ => None,
            };
            return Some((name.to_string(), source_index));
        }
        None
    }

    pub(crate) fn make_varref_value(
        name: String,
        value: Value,
        source_index: Option<usize>,
    ) -> Value {
        let mut named = std::collections::HashMap::new();
        named.insert("__mutsu_varref_name".to_string(), Value::str(name));
        named.insert("__mutsu_varref_value".to_string(), value);
        if let Some(i) = source_index {
            named.insert("__mutsu_varref_index".to_string(), Value::Int(i as i64));
        }
        Value::capture(Vec::new(), named)
    }

    pub(crate) fn assign_varref_target(
        &mut self,
        source_name: &str,
        source_index: Option<usize>,
        value: Value,
    ) -> Result<(), RuntimeError> {
        // Handle indexed source encoding: "varname\x00idx\x00index_str"
        if let Some(sep_pos) = source_name.find("\x00idx\x00") {
            let var_name = &source_name[..sep_pos];
            let idx_str = &source_name[sep_pos + 5..];
            if let Ok(i) = idx_str.parse::<usize>() {
                let Some(container) = self.env_mut().get_mut(var_name) else {
                    return Err(RuntimeError::assignment_ro(None));
                };
                if let Value::Array(items, ..) = container {
                    let arr = Arc::make_mut(items);
                    Self::autoviv_resize(arr, i + 1, Value::Package(Symbol::intern("Any")))?;
                    arr[i] = value;
                    return Ok(());
                }
            }
            if let Some(Value::Hash(hash)) = self.env_mut().get_mut(source_name) {
                let h = Arc::make_mut(hash);
                h.insert(idx_str.to_string(), value);
                return Ok(());
            }
            return Err(RuntimeError::assignment_ro(None));
        }
        if let Some(i) = source_index {
            let Some(container) = self.env_mut().get_mut(source_name) else {
                return Err(RuntimeError::assignment_ro(None));
            };
            let Value::Array(items, ..) = container else {
                return Err(RuntimeError::assignment_ro(None));
            };
            let arr = Arc::make_mut(items);
            Self::autoviv_resize(arr, i + 1, Value::Package(Symbol::intern("Any")))?;
            arr[i] = value;
            return Ok(());
        }
        self.env_mut().insert(source_name.to_string(), value);
        Ok(())
    }

    pub(crate) fn resolve_whatever_index_for_target(
        &mut self,
        idx: Value,
        target: Option<&Value>,
    ) -> Value {
        let len = match target {
            Some(Value::Array(items, ..)) => items.len() as i64,
            // A `:=`-bound array is held in a `ContainerRef` cell; descend it so
            // a from-end index (`@a[*-1]`) resolves the real length instead of 0
            // (which would yield a negative effective index and X::OutOfRange).
            Some(Value::ContainerRef(cell)) => match &*cell.lock().unwrap() {
                Value::Array(items, ..) => items.len() as i64,
                _ => 0,
            },
            _ => 0,
        };
        // Bare Whatever (*) in array subscript means all indices: 0, 1, ..., len-1
        if matches!(idx, Value::Whatever) {
            let indices: Vec<Value> = (0..len).map(Value::Int).collect();
            return Value::Array(
                Arc::new(crate::value::ArrayData::new(indices)),
                crate::value::ArrayKind::List,
            );
        }
        if let Value::Sub(ref data) = idx {
            let mut sub_env = data.env.clone();
            // Pass length for ALL WhateverCode parameters (e.g. *-4 .. *-2 has 2 params)
            for p in &data.params {
                sub_env.insert(p.to_string(), Value::Int(len));
            }
            let saved_env = std::mem::take(self.env_mut());
            *self.env_mut() = sub_env;
            let result = loan_env!(self, eval_block_value(&data.body)).unwrap_or(Value::Nil);
            *self.env_mut() = saved_env;
            return result;
        }
        // Resolve Array of WhateverCode indices: @a[*-3, *-2, *-1]
        if let Value::Array(ref items, kind) = idx {
            let mut needs_resolve = false;
            for item in items.iter() {
                if matches!(item, Value::Sub(_)) {
                    needs_resolve = true;
                    break;
                }
            }
            if needs_resolve {
                let mut resolved = Vec::with_capacity(items.len());
                for item in items.iter() {
                    if let Value::Sub(data) = item {
                        let mut sub_env = data.env.clone();
                        for p in &data.params {
                            sub_env.insert(p.to_string(), Value::Int(len));
                        }
                        let saved_env = std::mem::take(self.env_mut());
                        *self.env_mut() = sub_env;
                        let result =
                            loan_env!(self, eval_block_value(&data.body)).unwrap_or(Value::Nil);
                        *self.env_mut() = saved_env;
                        resolved.push(result);
                    } else {
                        resolved.push(item.clone());
                    }
                }
                return Value::Array(Arc::new(crate::value::ArrayData::new(resolved)), kind);
            }
        }
        idx
    }

    pub(super) fn quant_hash_trait_from_constraint(constraint: &str) -> Option<&'static str> {
        let base = constraint
            .split_once('[')
            .map(|(head, _)| head)
            .unwrap_or(constraint);
        match base {
            "Mix" => Some("Mix"),
            "MixHash" => Some("MixHash"),
            "Bag" => Some("Bag"),
            "BagHash" => Some("BagHash"),
            _ => None,
        }
    }

    /// After assigning to a hash variable, check if any values in the new hash
    /// reference the old hash Arc (captured on the RHS before assignment).
    /// If so, replace them with the new hash's Arc to create a true circular
    /// reference, matching Raku's container semantics for `%h = :b(%h)`.
    pub(super) fn fixup_circular_hash_refs(new_val: &mut Value, old_ptr: &Option<usize>) {
        let Some(old_ptr) = old_ptr else { return };
        if let Value::Hash(new_arc) = new_val {
            // Check if any values in the hash reference the old Arc.
            let has_old_ref = new_arc.values().any(|v| {
                if let Value::Hash(inner_arc) = v {
                    Arc::as_ptr(inner_arc) as usize == *old_ptr
                } else {
                    false
                }
            });
            if !has_old_ref {
                return;
            }
            // Build a new map where old-hash references are replaced with
            // a placeholder, then wrap it in an Arc and fix up the placeholder.
            let mut new_map = HashMap::new();
            let mut circular_keys = Vec::new();
            for (k, v) in new_arc.iter() {
                if let Value::Hash(inner_arc) = v
                    && Arc::as_ptr(inner_arc) as usize == *old_ptr
                {
                    circular_keys.push(k.clone());
                    // Placeholder - will be replaced below
                    new_map.insert(k.clone(), Value::Nil);
                    continue;
                }
                new_map.insert(k.clone(), v.clone());
            }
            // Create the new Arc with the map
            let result_arc = Value::hash_arc(new_map);
            // Now fix up the circular references: set the placeholder values
            // to point to the result_arc itself.
            // SAFETY: result_arc was just created here; building a
            // self-referential cycle requires the in-place write (a freshly
            // created Arc cannot be made cyclic via `get_mut`). See
            // `arc_contents_mut`; no borrow into the map is live across the write.
            let data = unsafe { crate::value::arc_contents_mut(&result_arc) };
            for key in &circular_keys {
                data.map
                    .insert(key.clone(), Value::Hash(result_arc.clone()));
            }
            *new_arc = result_arc;
        }
    }

    /// Check if a value contains a reference to the given array Arc pointer,
    /// either directly or nested inside hash values.
    /// `seen_hashes` tracks hash Arc pointers already visited to avoid infinite
    /// recursion on self-referencing hashes.
    pub(crate) fn value_contains_array_ref(
        v: &Value,
        old_ptr: usize,
        seen_hashes: &mut Vec<usize>,
    ) -> bool {
        match v {
            Value::Array(inner_arc, _) => Arc::as_ptr(inner_arc) as usize == old_ptr,
            Value::Hash(map) => {
                let hash_ptr = Arc::as_ptr(map) as usize;
                if seen_hashes.contains(&hash_ptr) {
                    return false;
                }
                seen_hashes.push(hash_ptr);
                let result = map
                    .values()
                    .any(|hv| Self::value_contains_array_ref(hv, old_ptr, seen_hashes));
                seen_hashes.pop();
                result
            }
            _ => false,
        }
    }

    /// Replace old array Arc references with the new array Arc, recursively
    /// traversing into hash values. Preserves hash self-references when
    /// cloning hash maps.
    /// `seen_hashes` tracks hash Arc pointers already visited to avoid infinite
    /// recursion on self-referencing hashes.
    pub(crate) fn replace_array_refs_in_value(
        v: &mut Value,
        old_ptr: usize,
        new_array: &Arc<crate::value::ArrayData>,
        kind: ArrayKind,
        seen_hashes: &mut Vec<usize>,
    ) {
        match v {
            Value::Array(inner_arc, _) if Arc::as_ptr(inner_arc) as usize == old_ptr => {
                *v = Value::Array(new_array.clone(), kind);
            }
            Value::Hash(map) => {
                let hash_ptr = Arc::as_ptr(map) as usize;
                if seen_hashes.contains(&hash_ptr) {
                    return;
                }
                seen_hashes.push(hash_ptr);
                // Check if any values in this hash contain the old array ref
                let needs_fixup = map.values().any(|hv| {
                    Self::value_contains_array_ref(hv, old_ptr, &mut seen_hashes.clone())
                });
                if needs_fixup {
                    // Clone the map, but track self-referencing hash keys so we
                    // can preserve the circular hash structure in the new Arc.
                    let mut new_map = HashMap::new();
                    let mut self_ref_keys = Vec::new();
                    for (k, hv) in map.iter() {
                        if let Value::Hash(inner_arc) = hv
                            && Arc::as_ptr(inner_arc) as usize == hash_ptr
                        {
                            self_ref_keys.push(k.clone());
                            new_map.insert(k.clone(), Value::Nil);
                        } else {
                            new_map.insert(k.clone(), hv.clone());
                        }
                    }
                    let new_hash_arc = Value::hash_arc(new_map);
                    let new_hash_ptr = Arc::as_ptr(&new_hash_arc) as usize;
                    // Mark the new hash as seen so recursive calls don't
                    // re-enter it via self-referencing values.
                    seen_hashes.push(new_hash_ptr);
                    // SAFETY: new_hash_arc was just created here; the
                    // self-reference insert and the in-place recursive fixup must
                    // alias it. See `arc_contents_mut`.
                    let data = unsafe { crate::value::arc_contents_mut(&new_hash_arc) };
                    for key in &self_ref_keys {
                        data.map
                            .insert(key.clone(), Value::Hash(new_hash_arc.clone()));
                    }
                    for hv in data.map.values_mut() {
                        Self::replace_array_refs_in_value(
                            hv,
                            old_ptr,
                            new_array,
                            kind,
                            seen_hashes,
                        );
                    }
                    seen_hashes.pop();
                    *map = new_hash_arc;
                }
                seen_hashes.pop();
            }
            _ => {}
        }
    }

    /// Fix up circular references in array assignment.
    /// When `@a = 42, @a`, the RHS contains a reference to the old array.
    /// Replace that reference with the new array to create a circular structure.
    /// Also handles cross-type cycles like `@b = %h, @b` where `%h` contains `@b`.
    pub(super) fn fixup_circular_array_refs(new_val: &mut Value, old_ptr: &Option<usize>) {
        let Some(old_ptr) = old_ptr else { return };
        if let Value::Array(new_arc, kind) = new_val {
            let mut seen_hashes = Vec::new();
            let has_old_ref = new_arc
                .iter()
                .any(|v| Self::value_contains_array_ref(v, *old_ptr, &mut seen_hashes));
            if !has_old_ref {
                return;
            }
            // Build a new items list, replacing old-array references with Nil placeholders
            let mut new_items: Vec<Value> = Vec::with_capacity(new_arc.len());
            let mut circular_indices = Vec::new();
            let mut hash_fixup_indices = Vec::new();
            for (i, v) in new_arc.iter().enumerate() {
                if let Value::Array(inner_arc, _) = v
                    && Arc::as_ptr(inner_arc) as usize == *old_ptr
                {
                    circular_indices.push(i);
                    new_items.push(Value::Nil); // placeholder
                } else if matches!(v, Value::Hash(_)) {
                    let mut seen = Vec::new();
                    if Self::value_contains_array_ref(v, *old_ptr, &mut seen) {
                        hash_fixup_indices.push(i);
                    }
                    new_items.push(v.clone());
                } else {
                    new_items.push(v.clone());
                }
            }
            let result_arc = crate::value::Value::array_arc(new_items);
            // SAFETY: result_arc was just created here; building a
            // self-referential cycle and the in-place recursive fixup must alias
            // it. See `arc_contents_mut`.
            let data = unsafe { crate::value::arc_contents_mut(&result_arc) };
            for idx in &circular_indices {
                data.items[*idx] = Value::Array(result_arc.clone(), *kind);
            }
            for idx in &hash_fixup_indices {
                let mut seen = Vec::new();
                Self::replace_array_refs_in_value(
                    &mut data.items[*idx],
                    *old_ptr,
                    &result_arc,
                    *kind,
                    &mut seen,
                );
            }
            *new_arc = result_arc;
        }
    }
}
