use super::*;
use crate::symbol::Symbol;
use std::collections::HashMap;

impl Interpreter {
    /// Return the default fill value for a native type constraint.
    /// For `int`/`uint` variants returns `Value::int(0)`,
    /// for `num` variants returns `Value::num(0.0)`,
    /// for `str` returns `Value::str("")`,
    /// otherwise returns `Value::package("Any")`.
    pub(crate) fn native_fill_for_constraint(constraint: Option<&str>) -> Value {
        match constraint {
            Some(
                "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16"
                | "uint32" | "uint64" | "byte" | "atomicint",
            ) => Value::int(0),
            Some("num" | "num32" | "num64") => Value::num(0.0),
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
                    Value::package(Symbol::intern("Any"))
                } else {
                    Value::package(Symbol::intern(base))
                }
            }
            None => Value::package(Symbol::intern("Any")),
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
        match attr_value.view() {
            ValueView::Array(items, kind) if !matches!(idx.view(), ValueView::Str(_)) => {
                let mut updated = (**items).clone();
                if let Some((slice_indices, vals)) = range_slice {
                    if let Some(max_idx) = slice_indices.last().copied()
                        && max_idx >= updated.len()
                    {
                        Self::autoviv_resize(
                            &mut updated,
                            max_idx + 1,
                            Value::package(Symbol::intern("Any")),
                        )?;
                    }
                    for (offset, i) in slice_indices.iter().enumerate() {
                        updated[*i] = vals.get(offset).cloned().unwrap_or(Value::NIL);
                    }
                } else if let Some(i) = Self::index_to_usize(idx) {
                    Self::autoviv_resize(
                        &mut updated,
                        i + 1,
                        Value::package(Symbol::intern("Any")),
                    )?;
                    updated[i] = val.clone();
                } else {
                    return Ok(false);
                }
                *attr_value = Value::array_with_kind(crate::gc::Gc::new(updated), kind);
                Ok(true)
            }
            ValueView::Hash(hash) if matches!(idx.view(), ValueView::Str(_)) => {
                let mut updated = (**hash).clone();
                updated.insert(idx.to_string_value(), val.clone());
                *attr_value = Value::hash_with_data(Value::hash_arc(updated));
                Ok(true)
            }
            ValueView::Nil if !matches!(idx.view(), ValueView::Str(_)) => {
                let mut updated = Vec::new();
                if let Some((slice_indices, vals)) = range_slice {
                    if let Some(max_idx) = slice_indices.last().copied() {
                        Self::autoviv_resize(
                            &mut updated,
                            max_idx + 1,
                            Value::package(Symbol::intern("Any")),
                        )?;
                    }
                    for (offset, i) in slice_indices.iter().enumerate() {
                        updated[*i] = vals.get(offset).cloned().unwrap_or(Value::NIL);
                    }
                } else if let Some(i) = Self::index_to_usize(idx) {
                    Self::autoviv_resize(
                        &mut updated,
                        i + 1,
                        Value::package(Symbol::intern("Any")),
                    )?;
                    updated[i] = val.clone();
                } else {
                    return Ok(false);
                }
                *attr_value = Value::real_array(updated);
                Ok(true)
            }
            ValueView::Nil if matches!(idx.view(), ValueView::Str(_)) => {
                let mut updated = std::collections::HashMap::new();
                updated.insert(idx.to_string_value(), val.clone());
                *attr_value = Value::hash_with_data(Value::hash_arc(updated));
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
        if let ValueView::Pair(name, payload) = val.view()
            && name == "__mutsu_bind_index_value"
        {
            if let ValueView::Array(items, ..) = payload.view() {
                let value = items.first().cloned().unwrap_or(Value::NIL);
                let source = match items.get(1).map(Value::view) {
                    Some(ValueView::Array(srcs, ..)) => match srcs.first().map(Value::view) {
                        Some(ValueView::Str(s)) if !s.is_empty() => Some((**s).clone()),
                        _ => None,
                    },
                    _ => None,
                };
                return (value, source);
            }
            return (payload.clone(), None);
        }
        (val, None)
    }

    pub(crate) fn varref_target(value: &Value) -> Option<(String, Option<usize>)> {
        let (name, _, index) = value.as_varref()?;
        Some((name.resolve(), index.map(|i| i as usize)))
    }

    pub(crate) fn make_varref_value(
        name: String,
        value: Value,
        source_index: Option<usize>,
    ) -> Value {
        Value::varref(Symbol::intern(&name), value, source_index.map(|i| i as u32))
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
            // `value` is consumed by exactly one of the branches below; the
            // `Option` shuffle lets the array closure take it while keeping it
            // available for the hash fallthrough when the target is not an array.
            let mut value = Some(value);
            if let Ok(i) = idx_str.parse::<usize>() {
                let Some(container) = self.env_mut().get_mut(var_name) else {
                    return Err(RuntimeError::assignment_ro(None));
                };
                if let Some(res) = container.with_array_mut(|items, _| {
                    let arr = crate::value::gc_data_mut(items);
                    Self::autoviv_resize(arr, i + 1, Value::package(Symbol::intern("Any")))?;
                    arr[i] = value.take().unwrap_or(Value::NIL);
                    Ok(())
                }) {
                    res?;
                    return Ok(());
                }
            }
            let value = value.take().unwrap_or(Value::NIL);
            if self
                .env_mut()
                .get_mut(source_name)
                .and_then(|v| {
                    v.with_hash_mut(|hash| {
                        let h = crate::value::gc_data_mut(hash);
                        h.insert(idx_str.to_string(), value);
                    })
                })
                .is_some()
            {
                return Ok(());
            }
            return Err(RuntimeError::assignment_ro(None));
        }
        if let Some(i) = source_index {
            let Some(container) = self.env_mut().get_mut(source_name) else {
                return Err(RuntimeError::assignment_ro(None));
            };
            let Some(res) = container.with_array_mut(|items, _| {
                let arr = crate::value::gc_data_mut(items);
                Self::autoviv_resize(arr, i + 1, Value::package(Symbol::intern("Any")))?;
                arr[i] = value;
                Ok(())
            }) else {
                return Err(RuntimeError::assignment_ro(None));
            };
            res?;
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
        let len = match target.map(Value::view) {
            Some(ValueView::Array(items, ..)) => items.len() as i64,
            // A `:=`-bound array is held in a `ContainerRef` cell; descend it so
            // a from-end index (`@a[*-1]`) resolves the real length instead of 0
            // (which would yield a negative effective index and X::OutOfRange).
            Some(ValueView::ContainerRef(cell)) => match cell.lock().unwrap().view() {
                ValueView::Array(items, ..) => items.len() as i64,
                _ => 0,
            },
            _ => 0,
        };
        // Bare Whatever (*) in array subscript means all indices: 0, 1, ..., len-1
        if matches!(idx.view(), ValueView::Whatever) {
            let indices: Vec<Value> = (0..len).map(Value::int).collect();
            return Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(indices)),
                crate::value::ArrayKind::List,
            );
        }
        if let ValueView::Sub(data) = idx.view() {
            let mut sub_env = data.env.clone();
            // Pass length for ALL WhateverCode parameters (e.g. *-4 .. *-2 has 2 params)
            for p in &data.params {
                sub_env.insert(p.to_string(), Value::int(len));
            }
            let saved_env = std::mem::take(self.env_mut());
            *self.env_mut() = sub_env;
            let result = loan_env!(self, eval_block_value(&data.body)).unwrap_or(Value::NIL);
            *self.env_mut() = saved_env;
            return result;
        }
        // Resolve Array of WhateverCode indices: @a[*-3, *-2, *-1]
        if let ValueView::Array(items, kind) = idx.view() {
            let mut needs_resolve = false;
            for item in items.iter() {
                if matches!(item.view(), ValueView::Sub(_)) {
                    needs_resolve = true;
                    break;
                }
            }
            if needs_resolve {
                let mut resolved = Vec::with_capacity(items.len());
                for item in items.iter() {
                    if let ValueView::Sub(data) = item.view() {
                        let mut sub_env = data.env.clone();
                        for p in &data.params {
                            sub_env.insert(p.to_string(), Value::int(len));
                        }
                        let saved_env = std::mem::take(self.env_mut());
                        *self.env_mut() = sub_env;
                        let result =
                            loan_env!(self, eval_block_value(&data.body)).unwrap_or(Value::NIL);
                        *self.env_mut() = saved_env;
                        resolved.push(result);
                    } else {
                        resolved.push(item.clone());
                    }
                }
                return Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(resolved)),
                    kind,
                );
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
        new_val.with_hash_mut(|new_arc| {
            // Check if any values in the hash reference the old Arc.
            let has_old_ref = new_arc.values().any(|v| {
                if let ValueView::Hash(inner_arc) = v.view() {
                    crate::gc::Gc::as_ptr(&inner_arc) as usize == *old_ptr
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
                if let ValueView::Hash(inner_arc) = v.view()
                    && crate::gc::Gc::as_ptr(&inner_arc) as usize == *old_ptr
                {
                    circular_keys.push(k.clone());
                    // Placeholder - will be replaced below
                    new_map.insert(k.clone(), Value::NIL);
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
            // Inventory bucket (a) — provably-unique (docs/gc-contents-mut-inventory.md).
            debug_assert_eq!(
                result_arc.strong_count(),
                1,
                "fixup_circular_hash: aliased &mut requires a uniquely-owned freshly-created node"
            );
            result_arc.verify_unique_for_aliased_mut("fixup_circular_hash");
            let data = unsafe { crate::value::gc_contents_mut(&result_arc) };
            for key in &circular_keys {
                data.map
                    .insert(key.clone(), Value::hash_with_data(result_arc.clone()));
            }
            *new_arc = result_arc;
        });
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
        match v.view() {
            ValueView::Array(inner_arc, _) => crate::gc::Gc::as_ptr(&inner_arc) as usize == old_ptr,
            ValueView::Hash(map) => {
                let hash_ptr = crate::gc::Gc::as_ptr(&map) as usize;
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
        new_array: &crate::gc::Gc<crate::value::ArrayData>,
        kind: ArrayKind,
        seen_hashes: &mut Vec<usize>,
    ) {
        if matches!(
            v.view(),
            ValueView::Array(inner_arc, _) if crate::gc::Gc::as_ptr(&inner_arc) as usize == old_ptr
        ) {
            *v = Value::array_with_kind(new_array.clone(), kind);
            return;
        }
        v.with_hash_mut(|map| {
            let hash_ptr = crate::gc::Gc::as_ptr(map) as usize;
            if seen_hashes.contains(&hash_ptr) {
                return;
            }
            seen_hashes.push(hash_ptr);
            // Check if any values in this hash contain the old array ref
            let needs_fixup = map
                .values()
                .any(|hv| Self::value_contains_array_ref(hv, old_ptr, &mut seen_hashes.clone()));
            if needs_fixup {
                // Clone the map, but track self-referencing hash keys so we
                // can preserve the circular hash structure in the new Arc.
                let mut new_map = HashMap::new();
                let mut self_ref_keys = Vec::new();
                for (k, hv) in map.iter() {
                    if let ValueView::Hash(inner_arc) = hv.view()
                        && crate::gc::Gc::as_ptr(&inner_arc) as usize == hash_ptr
                    {
                        self_ref_keys.push(k.clone());
                        new_map.insert(k.clone(), Value::NIL);
                    } else {
                        new_map.insert(k.clone(), hv.clone());
                    }
                }
                let new_hash_arc = Value::hash_arc(new_map);
                let new_hash_ptr = crate::gc::Gc::as_ptr(&new_hash_arc) as usize;
                // Mark the new hash as seen so recursive calls don't
                // re-enter it via self-referencing values.
                seen_hashes.push(new_hash_ptr);
                // SAFETY: new_hash_arc was just created here; the
                // self-reference insert and the in-place recursive fixup must
                // alias it. See `arc_contents_mut`.
                // Inventory bucket (a) — provably-unique (docs/gc-contents-mut-inventory.md).
                debug_assert_eq!(
                    new_hash_arc.strong_count(),
                    1,
                    "replace_array_refs_in_value: aliased &mut requires a uniquely-owned freshly-created node"
                );
                new_hash_arc.verify_unique_for_aliased_mut("replace_array_refs_in_value");
                let data = unsafe { crate::value::gc_contents_mut(&new_hash_arc) };
                for key in &self_ref_keys {
                    data.map
                        .insert(key.clone(), Value::hash_with_data(new_hash_arc.clone()));
                }
                for hv in data.map.values_mut() {
                    Self::replace_array_refs_in_value(hv, old_ptr, new_array, kind, seen_hashes);
                }
                seen_hashes.pop();
                *map = new_hash_arc;
            }
            seen_hashes.pop();
        });
    }

    /// Fix up circular references in array assignment.
    /// When `@a = 42, @a`, the RHS contains a reference to the old array.
    /// Replace that reference with the new array to create a circular structure.
    /// Also handles cross-type cycles like `@b = %h, @b` where `%h` contains `@b`.
    pub(super) fn fixup_circular_array_refs(new_val: &mut Value, old_ptr: &Option<usize>) {
        let Some(old_ptr) = old_ptr else { return };
        new_val.with_array_mut(|new_arc, kind| {
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
                if let ValueView::Array(inner_arc, _) = v.view()
                    && crate::gc::Gc::as_ptr(&inner_arc) as usize == *old_ptr
                {
                    circular_indices.push(i);
                    new_items.push(Value::NIL); // placeholder
                } else if matches!(v.view(), ValueView::Hash(_)) {
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
            // Inventory bucket (a) — provably-unique (docs/gc-contents-mut-inventory.md).
            debug_assert_eq!(
                result_arc.strong_count(),
                1,
                "fixup_circular_array_refs: aliased &mut requires a uniquely-owned freshly-created node"
            );
            result_arc.verify_unique_for_aliased_mut("fixup_circular_array_refs");
            let data = unsafe { crate::value::gc_contents_mut(&result_arc) };
            for idx in &circular_indices {
                data.items[*idx] = Value::array_with_kind(result_arc.clone(), *kind);
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
        });
    }

    /// Container identity (§3, splice.t): copy `new_gc`'s array contents into the
    /// original backing `old_gc` in place — preserving the pointer identity every
    /// by-value holder of `old_gc` shares (an `@a` captured into a list `(0, @a)` /
    /// a `\param`) — and redirect any self-reference that (after circular-ref
    /// fixup) pointed at the about-to-be-dropped `new_gc` back to `old_gc`. Returns
    /// the `Array` value that should be stored in the slot (backed by `old_gc`).
    pub(super) fn array_inplace_reassign(
        old_gc: &crate::gc::Gc<crate::value::ArrayData>,
        new_gc: &crate::gc::Gc<crate::value::ArrayData>,
        kind: ArrayKind,
    ) -> Value {
        let new_ptr = crate::gc::Gc::as_ptr(new_gc) as usize;
        let mut new_data = (**new_gc).clone();
        let mut seen = Vec::new();
        for item in new_data.items.iter_mut() {
            Self::replace_array_refs_in_value(item, new_ptr, old_gc, kind, &mut seen);
        }
        // SAFETY: single audited aliased in-place write; `new_data` is a fresh
        // clone and no borrow into `old_gc`'s contents is live across the write.
        unsafe {
            *crate::value::gc_contents_mut(old_gc) = new_data;
        }
        Value::array_with_kind(old_gc.clone(), kind)
    }

    /// Ensure a `@`/`%`-var value-assignment owns a DISTINCT container, matching
    /// Raku `=` copy semantics (`my @b = @a` yields `@b !=:= @a`). If the
    /// array/hash's backing `Gc` is *shared* with another holder, return a fresh-
    /// `Gc` shallow copy (elements/values stay shared references — only the outer
    /// container is duplicated). A singly-owned `Gc` is already exclusive and
    /// returned unchanged; non-container values pass through. Without this, mutsu's
    /// historical copy-on-write masking (a shared `Gc` that only *appears*
    /// independent because the next mutation reallocated) breaks once whole-
    /// container reassignment writes in place (§3): the in-place write would reach
    /// the aliased source. Thin wrapper over [`Value::detach_shared_container`]
    /// (also used by `is copy` parameter binding).
    pub(super) fn detach_shared_container(val: Value) -> Value {
        val.detach_shared_container()
    }

    /// Store a whole-container reassignment through a `ContainerRef` cell while
    /// PRESERVING the inner container's identity: when the cell currently holds
    /// an Array/Hash and the new value is the same container kind, copy the new
    /// contents into the EXISTING backing `Gc` (via `*_inplace_reassign`) instead
    /// of swapping the cell to a fresh pointer. A boxed captured `@a`/`%h`
    /// (escape analysis promotes captured+mutated outer containers to cells)
    /// whole-reassigned from a nested frame otherwise orphans every by-value
    /// holder of the old backing `Gc` (e.g. `%h` captured into a list). Non
    /// matching shapes fall back to a plain store.
    pub(super) fn cell_store_preserving_container_identity(
        arc: &crate::gc::Gc<std::sync::Mutex<Value>>,
        val: &Value,
    ) {
        let mut inner = arc.lock().unwrap();
        let replacement = match (inner.view(), val.view()) {
            (ValueView::Hash(old_gc), ValueView::Hash(new_gc))
                if !crate::gc::Gc::ptr_eq(&old_gc, &new_gc) =>
            {
                Some(Self::hash_inplace_reassign(&old_gc, &new_gc))
            }
            (ValueView::Array(old_gc, _), ValueView::Array(new_gc, kind))
                if !crate::gc::Gc::ptr_eq(&old_gc, &new_gc) =>
            {
                Some(Self::array_inplace_reassign(&old_gc, &new_gc, kind))
            }
            _ => None,
        };
        match replacement {
            Some(v) => *inner = v,
            None => {
                drop(inner);
                // A cell holding a `HashEntryRef` deferred token (a boxed
                // `\target` bound to a missing hash key) materializes on first
                // write: `store_through_cell` installs the cell at the token's
                // path before storing, so the hash and the binding stay aliased.
                Value::store_through_cell(arc, val);
            }
        }
    }

    /// The `Hash` analogue of [`array_inplace_reassign`]. Redirects any
    /// self-referencing hash value that pointed at `new_gc` back to `old_gc`.
    pub(super) fn hash_inplace_reassign(
        old_gc: &crate::gc::Gc<crate::value::HashData>,
        new_gc: &crate::gc::Gc<crate::value::HashData>,
    ) -> Value {
        let new_ptr = crate::gc::Gc::as_ptr(new_gc) as usize;
        let mut new_data = (**new_gc).clone();
        for v in new_data.map.values_mut() {
            if let ValueView::Hash(inner) = v.view()
                && crate::gc::Gc::as_ptr(&inner) as usize == new_ptr
            {
                *v = Value::hash_with_data(old_gc.clone());
            }
        }
        // SAFETY: as above — single audited aliased in-place write.
        unsafe {
            *crate::value::gc_contents_mut(old_gc) = new_data;
        }
        Value::hash_with_data(old_gc.clone())
    }
}
