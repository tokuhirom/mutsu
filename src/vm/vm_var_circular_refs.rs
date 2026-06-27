use super::*;

impl Interpreter {
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
    fn value_contains_array_ref(v: &Value, old_ptr: usize, seen_hashes: &mut Vec<usize>) -> bool {
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
    fn replace_array_refs_in_value(
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
