use super::*;
use crate::symbol::Symbol;
use std::collections::HashMap;
use std::sync::Arc;
use unicode_normalization::UnicodeNormalization;

/// A `:=` bind-source cell pre-read before the container borrow: the shared
/// `ContainerRef` cell plus the source variable name to install it into
/// afterwards (`None` when the source is already bound to that cell).
type BindSourceCell = (Option<String>, Arc<std::sync::Mutex<Value>>);

impl Interpreter {
    /// Return the default fill value for a native type constraint.
    /// For `int`/`uint` variants returns `Value::Int(0)`,
    /// for `num` variants returns `Value::Num(0.0)`,
    /// for `str` returns `Value::str("")`,
    /// otherwise returns `Value::Package("Any")`.
    fn native_fill_for_constraint(constraint: Option<&str>) -> Value {
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

    pub(super) fn delegated_mixin_attr_key(
        &self,
        mixins: &std::collections::HashMap<String, Value>,
        method_name: &str,
    ) -> Option<String> {
        self.delegated_role_attr_key_from_mixins(mixins, method_name)
    }

    fn assign_mixin_container_slot(
        attr_value: &mut Value,
        idx: &Value,
        val: &Value,
        range_slice: &Option<(Vec<usize>, Vec<Value>)>,
    ) -> bool {
        match attr_value {
            Value::Array(items, kind) if !matches!(idx, Value::Str(_)) => {
                let mut updated = (**items).clone();
                if let Some((slice_indices, vals)) = range_slice {
                    if let Some(max_idx) = slice_indices.last().copied()
                        && max_idx >= updated.len()
                    {
                        updated.resize(max_idx + 1, Value::Package(Symbol::intern("Any")));
                    }
                    for (offset, i) in slice_indices.iter().enumerate() {
                        updated[*i] = vals.get(offset).cloned().unwrap_or(Value::Nil);
                    }
                } else if let Some(i) = Self::index_to_usize(idx) {
                    if i >= updated.len() {
                        updated.resize(i + 1, Value::Package(Symbol::intern("Any")));
                    }
                    updated[i] = val.clone();
                } else {
                    return false;
                }
                *attr_value = Value::Array(Arc::new(updated), *kind);
                true
            }
            Value::Hash(hash) if matches!(idx, Value::Str(_)) => {
                let mut updated = (**hash).clone();
                updated.insert(idx.to_string_value(), val.clone());
                *attr_value = Value::Hash(Value::hash_arc(updated));
                true
            }
            Value::Nil if !matches!(idx, Value::Str(_)) => {
                let mut updated = Vec::new();
                if let Some((slice_indices, vals)) = range_slice {
                    if let Some(max_idx) = slice_indices.last().copied() {
                        updated.resize(max_idx + 1, Value::Package(Symbol::intern("Any")));
                    }
                    for (offset, i) in slice_indices.iter().enumerate() {
                        updated[*i] = vals.get(offset).cloned().unwrap_or(Value::Nil);
                    }
                } else if let Some(i) = Self::index_to_usize(idx) {
                    updated.resize(i + 1, Value::Package(Symbol::intern("Any")));
                    updated[i] = val.clone();
                } else {
                    return false;
                }
                *attr_value = Value::real_array(updated);
                true
            }
            Value::Nil if matches!(idx, Value::Str(_)) => {
                let mut updated = std::collections::HashMap::new();
                updated.insert(idx.to_string_value(), val.clone());
                *attr_value = Value::Hash(Value::hash_arc(updated));
                true
            }
            _ => false,
        }
    }

    /// Unwrap a `__mutsu_bind_index_value` payload to `(value, first_source)`.
    /// `value` is the RHS value being bound; `first_source` is the name of the
    /// single source variable when the bind is the common `LHS := $scalar`
    /// shape. Returns `(val, None)` for a plain (non-bind) value, so callers can
    /// treat the non-bind path unchanged.
    fn unwrap_bind_index_value(val: Value) -> (Value, Option<String>) {
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

    fn varref_target(value: &Value) -> Option<(String, Option<usize>)> {
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

    fn make_varref_value(name: String, value: Value, source_index: Option<usize>) -> Value {
        let mut named = std::collections::HashMap::new();
        named.insert("__mutsu_varref_name".to_string(), Value::str(name));
        named.insert("__mutsu_varref_value".to_string(), value);
        if let Some(i) = source_index {
            named.insert("__mutsu_varref_index".to_string(), Value::Int(i as i64));
        }
        Value::capture(Vec::new(), named)
    }

    fn assign_varref_target(
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
                    if i >= arr.len() {
                        arr.resize(i + 1, Value::Package(Symbol::intern("Any")));
                    }
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
            if i >= arr.len() {
                arr.resize(i + 1, Value::Package(Symbol::intern("Any")));
            }
            arr[i] = value;
            return Ok(());
        }
        self.env_mut().insert(source_name.to_string(), value);
        Ok(())
    }

    fn resolve_whatever_index_for_target(&mut self, idx: Value, target: Option<&Value>) -> Value {
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

    /// Type-check a `:=` bind to a typed-hash variable.
    ///
    /// Binding (unlike assignment) does not coerce — it aliases the RHS
    /// container directly. Raku therefore requires the RHS to *be* an
    /// `Associative[T]` matching the variable's declared value type `T`:
    ///
    /// ```text
    /// my Int %a; my Int  %b := %a;            # ok   (Int does Int)
    /// my Int %a; my Cool %b := %a;            # ok   (Int does Cool)
    /// my Int %h := :42foo.Set.Hash;           # dies (Hash[Any,Any])
    /// ```
    ///
    /// mutsu tracks a hash's element type via `container_type_metadata`; a
    /// plain/coerced hash has no metadata, so its value type defaults to `Mu`,
    /// which only conforms to an `Any`/`Mu`-typed target.
    fn check_hash_bind_value_type(
        &mut self,
        name: &str,
        value: &Value,
    ) -> Result<(), RuntimeError> {
        if !name.starts_with('%') {
            return Ok(());
        }
        let Some(constraint) = loan_env!(self, var_type_constraint(name)) else {
            return Ok(());
        };
        // Only enforce for a plain value-type constraint (`my Int %h`). Skip
        // container-trait declarations (`is Set`/`is Bag`/`is Map`/...), key-typed
        // forms (`%h{Int}`), and the trivial Any/Mu types.
        if constraint.contains('{')
            || matches!(constraint.as_str(), "" | "Any" | "Mu")
            || Self::quant_hash_trait_from_constraint(&constraint).is_some()
            || matches!(
                constraint.split('[').next().unwrap_or(&constraint),
                "Set"
                    | "SetHash"
                    | "Bag"
                    | "BagHash"
                    | "Mix"
                    | "MixHash"
                    | "Map"
                    | "Hash"
                    | "Associative"
                    | "QuantHash"
            )
        {
            return Ok(());
        }
        // The bound container's element type (defaults to Mu when untyped).
        let rhs_value_type = self
            .container_type_metadata(value)
            .map(|info| info.value_type)
            .filter(|vt| !vt.is_empty())
            .unwrap_or_else(|| "Mu".to_string());
        if crate::runtime::Interpreter::type_matches(&constraint, &rhs_value_type) {
            return Ok(());
        }
        let got = crate::runtime::utils::value_type_name(value);
        let message = format!(
            "Type check failed in binding; expected Associative[{}] but got {}",
            constraint, got
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("got".to_string(), value.clone());
        attrs.insert(
            "expected".to_string(),
            Value::Package(crate::symbol::Symbol::intern(&format!(
                "Associative[{}]",
                constraint
            ))),
        );
        attrs.insert("symbol".to_string(), Value::str(name.to_string()));
        attrs.insert("message".to_string(), Value::str(message.clone()));
        let ex = Value::make_instance(
            crate::symbol::Symbol::intern("X::TypeCheck::Binding"),
            attrs,
        );
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(ex));
        Err(err)
    }

    pub(super) fn coerce_hash_var_value(
        &mut self,
        name: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        if let Some(constraint) = self.var_type_constraint_fast(name).cloned()
            && let Some(trait_name) = Self::quant_hash_trait_from_constraint(&constraint)
        {
            // Only coerce if the variable IS a QuantHash container (declared via `is`),
            // not when the constraint is a value type (declared via `of`).
            // Check: if the current value is already a QuantHash, it's an `is` trait.
            let current = self.env().get(name).cloned();
            let is_quanthash_container = matches!(
                current,
                Some(Value::Bag(_, _) | Value::Mix(_, _) | Value::Set(_, _))
            );
            if is_quanthash_container {
                return self.try_compiled_method_or_interpret(value, trait_name, vec![]);
            }
        }
        if self.check_readonly_for_modify(name).is_err()
            && matches!(
                value,
                Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _)
            )
        {
            return Ok(value);
        }
        // A multi-param for-loop `%`-binding keeps its QuantHash identity (Raku
        // binds params; it does not assign-coerce them). A Set/Bag/Mix value
        // passes through unchanged; any other value (e.g. a Seq of pairs from a
        // `%a = %reset.pairs` reset) is coerced to the binding's *current*
        // QuantHash type rather than collapsing to a plain Hash.
        if self.quanthash_bind_params.iter().any(|n| n == name) {
            if matches!(
                value,
                Value::Set(_, _) | Value::Bag(_, _) | Value::Mix(_, _)
            ) {
                return Ok(value);
            }
            let trait_name = match self.env().get(name) {
                Some(Value::Set(_, _)) => Some("SetHash"),
                Some(Value::Bag(_, _)) => Some("BagHash"),
                Some(Value::Mix(_, _)) => Some("MixHash"),
                _ => None,
            };
            if let Some(tn) = trait_name {
                return self.try_compiled_method_or_interpret(value, tn, vec![]);
            }
        }
        if let Some(constraint) = loan_env!(self, var_type_constraint(name))
            && constraint.starts_with("SetHash")
        {
            let result = runtime::utils::coerce_value_to_quanthash(&value);
            // SetHash should be mutable
            if let Value::Set(items, _) = result {
                return Ok(Value::Set(items, true));
            }
            return Ok(result);
        }
        // For Array/Seq/Slip values, use `build_hash_from_items` which
        // raises "Odd number of elements" when appropriate. Hash values from
        // scalar containers (`$h`) are NOT pre-flattened, so they appear as
        // opaque items (triggering the odd-number check when expected).
        let hash_val = match value {
            Value::Array(ref items, _) => {
                // `build_hash_from_items` flattens a bare `%h` element into its
                // pairs (`%m = %h, a => 42` and the single-element `%m = (%h,)`),
                // while a hash sourced from a `$` scalar carries
                // `HashData.itemized` and stays opaque (`%m = ($hashitem,)` →
                // "Odd number") — matching Raku.
                runtime::utils::build_hash_from_items(items.iter().cloned().collect())?
            }
            Value::Seq(ref items) | Value::Slip(ref items) => {
                runtime::utils::build_hash_from_items(items.iter().cloned().collect())?
            }
            // A single bare scalar assigned to a hash is a one-element (odd)
            // initializer: `my %h = 1` is X::Hash::Store::OddNumber. Hashes,
            // pairs, sets, instances, Nil, etc. keep their existing coercion.
            ref scalar
                if matches!(
                    scalar.clone().into_descalarized(),
                    Value::Int(_)
                        | Value::BigInt(_)
                        | Value::Num(_)
                        | Value::Str(_)
                        | Value::Bool(_)
                        | Value::Rat(..)
                        | Value::FatRat(..)
                        | Value::BigRat(..)
                ) =>
            {
                runtime::utils::build_hash_from_items(vec![value])?
            }
            _ => runtime::coerce_to_hash(value),
        };
        // Resolve hash sentinel entries (self-refs) and decont `:=`-bound
        // `ContainerRef` cells when assigning to a new hash variable:
        // assignment creates new containers, so the copy snapshots values
        // instead of sharing cells.
        if let Value::Hash(ref items) = hash_val
            && (Self::hash_has_sentinels(items) || items.values().any(Value::is_container_ref))
        {
            return Ok(self.resolve_hash_for_iteration(items));
        }
        Ok(hash_val)
    }

    /// Coerce a value for `constant %x = ...` assignment.
    /// Associative values (Hash, Pair, Bag, Set, Mix) are preserved.
    /// Non-Associative values (Lists, Arrays) are coerced to Map.
    /// Instance objects that don't do Associative get `.Map` called.
    pub(crate) fn coerce_constant_hash_value(
        &mut self,
        name: &str,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        match &value {
            // Associative types are preserved as-is
            Value::Hash(_) | Value::Pair(..) | Value::Set(..) | Value::Bag(..) | Value::Mix(..) => {
                Ok(value)
            }
            // Instance objects: check if they do Associative
            Value::Instance { class_name, .. } => {
                let cn = class_name.resolve();
                let does_associative = matches!(
                    cn.as_str(),
                    "Hash"
                        | "Map"
                        | "Pair"
                        | "Set"
                        | "Bag"
                        | "Mix"
                        | "SetHash"
                        | "BagHash"
                        | "MixHash"
                ) || self
                    .class_composed_roles(&cn)
                    .is_some_and(|roles| roles.iter().any(|r| r == "Associative"));
                if does_associative {
                    Ok(value)
                } else {
                    // Call .Map on non-Associative to coerce.
                    // Skip native methods so user-defined .Map is called.
                    let mapped = self.call_method_all_with_fallback(&value, "Map", &[], true)?;
                    let mapped_val = mapped.into_iter().next().unwrap_or(Value::Nil);
                    // Check that .Map returned an Associative
                    let is_assoc = matches!(
                        &mapped_val,
                        Value::Hash(_)
                            | Value::Pair(..)
                            | Value::Set(..)
                            | Value::Bag(..)
                            | Value::Mix(..)
                    );
                    if !is_assoc {
                        let got_type = crate::runtime::utils::value_type_name(&mapped_val);
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("got".to_string(), mapped_val);
                        attrs.insert(
                            "expected".to_string(),
                            Value::Package(crate::symbol::Symbol::intern("Associative")),
                        );
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!(
                                "Type check failed in assignment to {}; expected Associative but got {}",
                                name, got_type
                            )),
                        );
                        let ex = Value::make_instance(
                            crate::symbol::Symbol::intern("X::TypeCheck"),
                            attrs,
                        );
                        let mut err = RuntimeError::new(format!(
                            "Type check failed in assignment to {}; expected Associative but got {}",
                            name, got_type
                        ));
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                    // Register as Map
                    let info = crate::runtime::ContainerTypeInfo {
                        value_type: String::new(),
                        key_type: None,
                        declared_type: Some("Map".to_string()),
                    };
                    Ok(self.tag_container_metadata(mapped_val, info))
                }
            }
            // Non-Associative values: coerce to Map
            Value::Array(items, _) => {
                let hash = runtime::utils::build_hash_from_items(items.iter().cloned().collect())?;
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                };
                Ok(self.tag_container_metadata(hash, info))
            }
            Value::Seq(items) | Value::Slip(items) => {
                let hash = runtime::utils::build_hash_from_items(items.iter().cloned().collect())?;
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                };
                Ok(self.tag_container_metadata(hash, info))
            }
            _ => {
                // For other types (Int, Str, etc.), coerce to Map via
                // build_hash_from_items which raises X::Hash::Store::OddNumber
                // for odd element counts (e.g., `constant %h = 42`).
                let hash = runtime::utils::build_hash_from_items(vec![value])?;
                let info = crate::runtime::ContainerTypeInfo {
                    value_type: String::new(),
                    key_type: None,
                    declared_type: Some("Map".to_string()),
                };
                Ok(self.tag_container_metadata(hash, info))
            }
        }
    }

    pub(super) fn mix_assignment_weight(value: &Value) -> Result<f64, RuntimeError> {
        match value {
            Value::Int(i) => Ok(*i as f64),
            Value::Num(n) => Ok(*n),
            Value::Rat(n, d) if *d != 0 => Ok(*n as f64 / *d as f64),
            Value::Bool(flag) => Ok(if *flag { 1.0 } else { 0.0 }),
            Value::Str(s) => {
                // Try to parse as numeric; throw X::Str::Numeric on failure
                if let Ok(n) = s.parse::<f64>() {
                    Ok(n)
                } else {
                    Err(RuntimeError::str_numeric(
                        s,
                        "base-10 number must begin with valid digits or '.'",
                    ))
                }
            }
            _ => {
                if value.truthy() {
                    Ok(1.0)
                } else {
                    Ok(0.0)
                }
            }
        }
    }

    /// Coerce a value assigned to a Bag/BagHash weight to an `i64` count, with
    /// the same numeric validation as Mix: a non-numeric `Str` raises
    /// X::Str::Numeric rather than being silently treated as truthy (1).
    pub(super) fn bag_assignment_count(value: &Value) -> Result<num_bigint::BigInt, RuntimeError> {
        use num_bigint::BigInt;
        match value {
            Value::Int(i) => Ok(BigInt::from(*i)),
            // Weights can exceed i64::MAX (e.g. `%h<k> = 10**19`), so a BigInt
            // weight is preserved verbatim rather than truncated to a native int.
            Value::BigInt(n) => Ok((**n).clone()),
            Value::Num(n) => Ok(BigInt::from(*n as i64)),
            Value::Rat(n, d) if *d != 0 => Ok(BigInt::from(*n / *d)),
            Value::Bool(flag) => Ok(BigInt::from(i64::from(*flag))),
            Value::Str(s) => {
                if let Ok(n) = s.parse::<BigInt>() {
                    Ok(n)
                } else if let Ok(n) = s.parse::<f64>() {
                    Ok(BigInt::from(n as i64))
                } else {
                    Err(RuntimeError::str_numeric(
                        s,
                        "base-10 number must begin with valid digits or '.'",
                    ))
                }
            }
            _ => {
                if value.truthy() {
                    Ok(BigInt::from(1))
                } else {
                    Ok(BigInt::from(0))
                }
            }
        }
    }

    const LAZY_ASSIGN_PRESERVE_MARKER: &str = "__mutsu_preserve_lazy_on_array_assign";
    const MAX_ASSIGN_SLICE_EXPAND: i64 = 100_000;

    fn assignment_rhs_values(&mut self, val: &Value) -> Result<Vec<Value>, RuntimeError> {
        Ok(match val {
            Value::Array(v, ..) => v.as_ref().clone().items,
            Value::Seq(v) | Value::Slip(v) => v.iter().cloned().collect(),
            Value::Range(a, b) => {
                let end = (*b).min(a.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < *a {
                    Vec::new()
                } else {
                    (*a..=end).map(Value::Int).collect()
                }
            }
            Value::RangeExcl(a, b) => {
                let end = (*b).min(a.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end <= *a {
                    Vec::new()
                } else {
                    (*a..end).map(Value::Int).collect()
                }
            }
            Value::RangeExclStart(a, b) => {
                let start = a.saturating_add(1);
                let end = (*b).min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < start {
                    Vec::new()
                } else {
                    (start..=end).map(Value::Int).collect()
                }
            }
            Value::RangeExclBoth(a, b) => {
                let start = a.saturating_add(1);
                let end = (*b).min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end <= start {
                    Vec::new()
                } else {
                    (start..end).map(Value::Int).collect()
                }
            }
            Value::LazyList(list) => self.force_lazy_list_vm(list)?,
            _ => vec![val.clone()],
        })
    }

    /// Track whether a scalar variable is bound (`:=`) to a Positional value.
    /// A bound scalar is NOT a Scalar container, so `@a = $bound` must flatten
    /// rather than itemize. The `ItemizeVar` opcode reads this marker. Plain
    /// assignment to a scalar clears any stale marker (guarded by
    /// `bound_decont_active` so the common no-bind case stays cheap).
    pub(super) fn update_bound_decont_marker(&mut self, name: &str, is_bind: bool, val: &Value) {
        // Scalar local names carry no sigil (e.g. "y"); `@`/`%`/`&` vars do.
        if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
            return;
        }
        if is_bind {
            // A scalar bound to a container (`$x := @a` / `:= %h` / `:= (1,2,3)`)
            // is not a Scalar container of its own: it aliases the container, so
            // `@a = $bound` flattens (ItemizeVar) and `$bound.VAR.^name` reflects
            // the container type (List/Array/Hash/Set/Bag/Mix) rather than Scalar.
            let is_container = match &val {
                Value::Array(_, k) => !k.is_itemized(),
                Value::Seq(_)
                | Value::Slip(_)
                | Value::LazyList(_)
                | Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::Hash(_)
                | Value::Set(..)
                | Value::Bag(..)
                | Value::Mix(..) => true,
                // A `:=` bind to a whole-container `@`/`%` variable holds a
                // shared cell whose inner value is the container.
                Value::ContainerRef(cell) => matches!(
                    &*cell.lock().unwrap(),
                    Value::Array(..)
                        | Value::Hash(_)
                        | Value::Set(..)
                        | Value::Bag(..)
                        | Value::Mix(..)
                ),
                _ => false,
            };
            if is_container {
                let key = format!("__mutsu_bound_decont::{}", name);
                self.env_mut().insert(key, Value::Bool(true));
                self.bound_decont_active = true;
                return;
            }
        }
        if self.bound_decont_active {
            let key = format!("__mutsu_bound_decont::{}", name);
            self.env_mut().remove(&key);
        }
    }

    /// Slice 2a (`docs/scalar-array-sharing.md`): is `name` a `$` scalar that
    /// currently shares an array/hash by reference (`$n = @z`)? Such a scalar's
    /// whole reassignment REPLACES its slot instead of writing through the cell.
    pub(super) fn is_array_share_scalar(&self, name: &str) -> bool {
        self.env()
            .get(&format!("__mutsu_array_share::{}", name))
            .is_some()
    }

    fn clear_array_share_marker(&mut self, name: &str) {
        self.env_mut()
            .remove(&format!("__mutsu_array_share::{}", name));
    }

    /// Slice 2a: `$n = @z` / `$n = %h`. Promote the source container variable to
    /// a shared `ContainerRef` cell and store that same cell in the scalar
    /// target, so structural mutations (`.push`) through either name are seen by
    /// both (raku reference semantics) — a snapshotting copy would COW-detach on
    /// the first `.push`. Marks the scalar `__mutsu_array_share::` so a later
    /// whole reassignment (`$n = 5`) replaces the slot instead of mutating the
    /// shared cell. Does NOT set the bound-decont marker: the scalar stays
    /// itemized (`@a = $n` itemizes, unlike a `:=` bind which flattens).
    fn array_share_assign(
        &mut self,
        code: &CompiledCode,
        idx: usize,
        val: Value,
        source_name: String,
    ) -> Result<(), RuntimeError> {
        let resolved_source = self.resolve_sigilless_alias_source_name(&source_name);
        let name = code.locals[idx].clone();
        // Build (or reuse) the shared cell: reuse an existing cell carried by the
        // value or already held by the source variable, else wrap the snapshot.
        let cell = match &val {
            Value::ContainerRef(arc) => arc.clone(),
            _ => match self.env().get(&resolved_source) {
                Some(Value::ContainerRef(arc)) => arc.clone(),
                _ => std::sync::Arc::new(std::sync::Mutex::new(val.clone())),
            },
        };
        let container = Value::ContainerRef(cell);
        // Promote the SOURCE container variable to the same cell so its own
        // `.push` / whole-reassign (`@z = (...)`) mutate through and stay visible
        // via the scalar.
        if let Some(source_idx) = code.locals.iter().rposition(|n| n == &resolved_source) {
            self.locals[source_idx] = container.clone();
            self.flush_local_to_env(code, source_idx);
        }
        self.set_env_with_main_alias(&resolved_source, container.clone());
        // Propagate the shared cell into saved call frames so the sharing
        // survives method returns (env restore).
        for frame in self.call_frames.iter_mut().rev() {
            if frame.saved_env.contains_key(&resolved_source) {
                frame
                    .saved_env
                    .insert(resolved_source.clone(), container.clone());
            }
            for (i, local_name) in code.locals.iter().enumerate() {
                if local_name == &resolved_source && i < frame.saved_locals.len() {
                    frame.saved_locals[i] = container.clone();
                }
            }
        }
        // Store the shared cell in the scalar target (itemized scalar).
        self.locals[idx] = container.clone();
        // Clear any stale bound-decont marker inherited from an earlier bind of
        // the same name (this `=` share is itemized, not a `:=` decont alias).
        self.update_bound_decont_marker(&name, false, &val);
        // Mark the scalar so a later whole reassignment replaces the slot.
        self.env_mut()
            .insert(format!("__mutsu_array_share::{}", name), Value::Bool(true));
        self.array_share_active = true;
        self.set_env_with_main_alias(&name, container.clone());
        self.flush_local_to_env(code, idx);
        Ok(())
    }

    pub(super) fn normalize_scalar_assignment_value(val: Value) -> Value {
        let is_nilish = |v: &Value| match v {
            Value::Nil => true,
            Value::Package(sym) => sym.resolve() == "Any",
            _ => false,
        };
        match val {
            Value::Array(items, kind) if items.len() == 1 => {
                if items.first().is_some_and(is_nilish) {
                    Value::Nil
                } else {
                    Value::Array(items, kind)
                }
            }
            Value::Seq(items) if items.len() == 1 && items.first().is_some_and(is_nilish) => {
                Value::Nil
            }
            Value::Slip(items) if items.len() == 1 && items.first().is_some_and(is_nilish) => {
                Value::Nil
            }
            other => other,
        }
    }

    fn extract_varref_binding(raw_val: Value) -> (Value, Option<String>) {
        if let Value::Capture { positional, named } = &raw_val
            && positional.is_empty()
            && let Some(Value::Str(name)) = named.get("__mutsu_varref_name")
            && let Some(inner) = named.get("__mutsu_varref_value")
        {
            return (inner.clone(), Some(name.to_string()));
        }
        (raw_val, None)
    }

    pub(crate) fn resolve_sigilless_alias_source_name(&self, source_name: &str) -> String {
        let mut resolved = source_name.to_string();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(resolved.clone()) {
            let key = format!("__mutsu_sigilless_alias::{}", resolved);
            let Some(Value::Str(next)) = self.env().get(&key) else {
                break;
            };
            resolved = next.to_string();
        }
        resolved
    }

    /// Try to reconstruct a typed value from a stringified hash key.
    /// Hash keys are always stored as strings, but for key-constrained hashes
    /// (e.g. `%h{Int}`), we need to check the original type. If the string
    /// looks like a valid value of the target type, return that typed value.
    fn try_reconstruct_typed_key(key: &str, target_type: &str) -> Value {
        match target_type {
            "Int" => {
                if let Ok(n) = key.parse::<i64>() {
                    return Value::Int(n);
                }
            }
            "Num" => {
                if let Ok(n) = key.parse::<f64>() {
                    return Value::Num(n);
                }
            }
            "Numeric" | "Real" | "Cool" | "Any" | "Mu" => {
                // These accept strings, so just return the string
                return Value::str(key.to_string());
            }
            _ => {}
        }
        Value::str(key.to_string())
    }

    pub(super) fn coerce_typed_container_assignment(
        &mut self,
        var_name: &str,
        value: Value,
        explicit_initializer: bool,
    ) -> Result<Value, RuntimeError> {
        let coercion_target = |constraint: &str| -> Option<String> {
            if let Some(open) = constraint.find('(')
                && constraint.ends_with(')')
            {
                let target = &constraint[..open];
                if !target.is_empty() {
                    return Some(target.to_string());
                }
            }
            None
        };
        if var_name.starts_with('@')
            && let Some(constraint) = loan_env!(self, var_type_constraint(var_name))
            && let Value::Array(items, kind) = value
        {
            // Native typed arrays cannot store lazy sequences
            if kind.is_lazy()
                && crate::runtime::native_types::is_native_array_element_type(&constraint)
            {
                let declared = format!("array[{}]", constraint);
                return Err(RuntimeError::typed(
                    "X::Cannot::Lazy",
                    [
                        (
                            "message".to_string(),
                            Value::str(format!(
                                "Cannot initialize an array of {} with a lazy list",
                                constraint
                            )),
                        ),
                        ("action".to_string(), Value::str_from("initialize")),
                        ("what".to_string(), Value::str(declared)),
                    ]
                    .into_iter()
                    .collect(),
                ));
            }
            let coerced_items = self.coerce_typed_array_elements(
                var_name,
                &constraint,
                &items,
                kind,
                &coercion_target,
                explicit_initializer,
            )?;
            return Ok(Value::Array(
                Arc::new(crate::value::ArrayData::new(coerced_items)),
                kind,
            ));
        }

        if var_name.starts_with('%')
            && let Value::Hash(map) = value
        {
            // Preserve original keys from the source hash before coercion
            // (coercion creates a new Arc, losing the registration).
            let saved_original_keys =
                runtime::utils::hash_original_keys_snapshot(&Value::Hash(map.clone()));
            let value_constraint = loan_env!(self, var_type_constraint(var_name));
            let key_constraint = loan_env!(self, var_hash_key_constraint(var_name));
            let mut coerced_map = std::collections::HashMap::with_capacity(map.len());
            for (key, val) in map.iter() {
                let coerced_key = if let Some(constraint) = &key_constraint {
                    // Hash keys are always stored as strings internally, but for
                    // key-constrained hashes (e.g. %h{Int}), we need to check
                    // that the key can be interpreted as the constraint type.
                    // Since hash construction stringifies pair keys (e.g. 1 => 2
                    // becomes "1" => 2), we try to reconstruct the typed value
                    // from the string key before checking the constraint.
                    let target_type =
                        coercion_target(constraint).unwrap_or_else(|| constraint.clone());
                    let key_as_typed_value = Self::try_reconstruct_typed_key(key, &target_type);
                    if !loan_env!(self, type_matches_value(&target_type, &key_as_typed_value)) {
                        return Err(runtime::utils::type_check_element_typed_error(
                            var_name,
                            constraint,
                            &Value::str(key.clone()),
                        ));
                    }
                    key.clone()
                } else {
                    key.clone()
                };
                let coerced_val = if let Some(constraint) = &value_constraint {
                    if matches!(val, Value::Nil) {
                        if let Some(default) = self.var_default(var_name) {
                            default.clone()
                        } else if explicit_initializer && self.is_definite_constraint(constraint) {
                            return Err(runtime::utils::type_check_element_typed_error(
                                var_name, constraint, val,
                            ));
                        } else {
                            val.clone()
                        }
                    } else {
                        let target_type =
                            coercion_target(constraint).unwrap_or_else(|| constraint.clone());
                        let coerced = if self.type_matches_value(&target_type, val) {
                            val.clone()
                        } else {
                            loan_env!(
                                self,
                                try_coerce_value_for_constraint(constraint, val.clone())
                            )?
                        };
                        if !self.type_matches_value(&target_type, &coerced) {
                            return Err(runtime::utils::type_check_element_typed_error(
                                var_name, constraint, val,
                            ));
                        }
                        coerced
                    }
                } else {
                    val.clone()
                };
                coerced_map.insert(coerced_key, coerced_val);
            }
            let mut result = Value::hash(coerced_map);
            // Re-embed original keys on the new (coerced) hash.
            if let Some(orig) = saved_original_keys {
                result = runtime::utils::set_hash_original_keys(result, orig);
            }
            return Ok(result);
        }

        Ok(value)
    }

    /// Coerce/check elements of a typed array, recursing into shaped sub-arrays.
    fn coerce_typed_array_elements(
        &mut self,
        var_name: &str,
        constraint: &str,
        items: &[Value],
        kind: crate::value::ArrayKind,
        coercion_target: &dyn Fn(&str) -> Option<String>,
        explicit_initializer: bool,
    ) -> Result<Vec<Value>, RuntimeError> {
        let native_constraint =
            crate::runtime::native_types::is_native_array_element_type(constraint);
        let mut coerced_items = Vec::with_capacity(items.len());
        for item in items.iter() {
            // A lazy/infinite Range (e.g. `-Inf..0e0`, `0e0..Inf`) cannot initialize
            // a native array, regardless of which end is unbounded.
            if native_constraint && crate::builtins::methods_0arg::is_value_lazy(item) {
                return Err(RuntimeError::typed(
                    "X::Cannot::Lazy",
                    [
                        (
                            "message".to_string(),
                            Value::str(format!(
                                "Cannot initialize an array of {} with a lazy list",
                                constraint
                            )),
                        ),
                        ("action".to_string(), Value::str_from("initialize")),
                        (
                            "what".to_string(),
                            Value::str(format!("array[{constraint}]")),
                        ),
                    ]
                    .into_iter()
                    .collect(),
                ));
            }
            // A type-object hole (e.g. an `Any` gap from `@a[1] = x`) assigned to a
            // native array becomes that array's default (`0`/`0e0`/`""`).
            if native_constraint && matches!(item, Value::Package(_)) {
                coerced_items.push(Self::native_fill_for_constraint(Some(constraint)));
                continue;
            }
            if matches!(item, Value::Nil) {
                if let Some(default) = self.var_default(var_name) {
                    coerced_items.push(default.clone());
                } else if explicit_initializer && self.is_definite_constraint(constraint) {
                    return Err(runtime::utils::type_check_element_typed_error(
                        var_name, constraint, item,
                    ));
                } else {
                    coerced_items.push(item.clone());
                }
                continue;
            }
            // For shaped arrays, sub-arrays are structural — recurse into them
            if kind == crate::value::ArrayKind::Shaped
                && let Value::Array(sub_items, sub_kind) = item
            {
                let sub_coerced = self.coerce_typed_array_elements(
                    var_name,
                    constraint,
                    sub_items,
                    *sub_kind,
                    coercion_target,
                    explicit_initializer,
                )?;
                coerced_items.push(Value::Array(
                    Arc::new(crate::value::ArrayData::new(sub_coerced)),
                    *sub_kind,
                ));
                continue;
            }
            let target_type = coercion_target(constraint).unwrap_or_else(|| constraint.to_string());
            // Whether the constraint is an explicit coercion type like `Array()`.
            // A plain type constraint (e.g. `Array`) must type-check elements
            // strictly without coercing scalars into containers.
            let is_coercion = coercion_target(constraint).is_some();
            let coerced = if self.type_matches_value(&target_type, item) {
                item.clone()
            } else if target_type == "Array" {
                match item {
                    Value::Array(items, kind) if !kind.is_real_array() => {
                        Value::Array(items.clone(), crate::value::ArrayKind::Array)
                    }
                    Value::Scalar(inner) => match inner.as_ref() {
                        Value::Array(items, kind) if !kind.is_real_array() => {
                            Value::Array(items.clone(), crate::value::ArrayKind::Array)
                        }
                        Value::Array(..) => inner.as_ref().clone(),
                        _ if is_coercion => loan_env!(
                            self,
                            try_coerce_value_for_constraint("Array()", item.clone())
                        )?,
                        _ => item.clone(),
                    },
                    _ if is_coercion => loan_env!(
                        self,
                        try_coerce_value_for_constraint("Array()", item.clone())
                    )?,
                    _ => item.clone(),
                }
            } else if matches!(target_type.as_str(), "Array" | "List" | "Hash") && is_coercion {
                self.try_coerce_value_for_constraint(&format!("{target_type}()"), item.clone())?
            } else {
                loan_env!(
                    self,
                    try_coerce_value_for_constraint(constraint, item.clone())
                )?
            };
            if !self.type_matches_value(&target_type, &coerced) {
                return Err(runtime::utils::type_check_element_typed_error(
                    var_name, constraint, item,
                ));
            }
            // Wrap/check native integer overflow for native typed arrays
            let coerced = Self::wrap_native_int_by_constraint(&target_type, coerced)?;
            coerced_items.push(coerced);
        }
        Ok(coerced_items)
    }

    /// Resolve a GenericRange with WhateverCode endpoints into a concrete range
    /// by evaluating the lambda endpoints against the given array length.
    fn resolve_generic_range_for_assign(&mut self, idx: &Value, array_len: usize) -> Option<Value> {
        if let Value::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } = idx
        {
            let len = array_len as i64;
            let resolve_endpoint = |vm: &mut Self, val: &Value| -> i64 {
                match val {
                    Value::Int(i) => *i,
                    Value::Sub(data) => {
                        let mut sub_env = data.env.clone();
                        for p in &data.params {
                            sub_env.insert(p.to_string(), Value::Int(len));
                        }
                        let saved_env = std::mem::take(vm.env_mut());
                        *vm.env_mut() = sub_env;
                        let result = vm.eval_block_value(&data.body).unwrap_or(Value::Nil);
                        *vm.env_mut() = saved_env;
                        match result {
                            Value::Int(i) => i,
                            _ => 0,
                        }
                    }
                    Value::Num(f) => *f as i64,
                    _ => 0,
                }
            };
            let s = resolve_endpoint(self, start);
            let e = resolve_endpoint(self, end);
            let resolved = if *excl_start && *excl_end {
                Value::RangeExclBoth(s, e)
            } else if *excl_start {
                Value::RangeExclStart(s, e)
            } else if *excl_end {
                Value::RangeExcl(s, e)
            } else {
                Value::Range(s, e)
            };
            Some(resolved)
        } else {
            None
        }
    }

    fn slice_indices_from_index(idx: &Value) -> Option<Vec<usize>> {
        match idx {
            Value::Range(a, b) => {
                let start = (*a).max(0);
                let end = (*b).min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < start {
                    return Some(Vec::new());
                }
                Some(
                    (start..=end)
                        .filter_map(|i| usize::try_from(i).ok())
                        .collect(),
                )
            }
            Value::RangeExcl(a, b) => {
                let start = (*a).max(0);
                let end_excl = (*b)
                    .max(0)
                    .min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if start >= end_excl {
                    return Some(Vec::new());
                }
                Some(
                    (start..end_excl)
                        .filter_map(|i| usize::try_from(i).ok())
                        .collect(),
                )
            }
            Value::RangeExclStart(a, b) => {
                let start = a.saturating_add(1).max(0);
                let end = (*b).min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < start {
                    return Some(Vec::new());
                }
                Some(
                    (start..=end)
                        .filter_map(|i| usize::try_from(i).ok())
                        .collect(),
                )
            }
            Value::RangeExclBoth(a, b) => {
                let start = a.saturating_add(1).max(0);
                let end_excl = (*b)
                    .max(0)
                    .min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if start >= end_excl {
                    return Some(Vec::new());
                }
                Some(
                    (start..end_excl)
                        .filter_map(|i| usize::try_from(i).ok())
                        .collect(),
                )
            }
            _ => None,
        }
    }

    /// Create an X::OutOfRange RuntimeError for negative index assignment
    fn make_out_of_range_error(effective_index: i64) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str_from(&format!(
                "Effective index out of range. Is: {}, should be in 0..^Inf",
                effective_index
            )),
        );
        attrs.insert("got".to_string(), Value::Int(effective_index));
        attrs.insert("range".to_string(), Value::str_from("0..^Inf"));
        RuntimeError::typed("X::OutOfRange", attrs)
    }

    pub(super) fn exec_string_concat_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = self.stack.drain(start..).collect();
        // Slice F: a user `.Str`/`.Stringy` method run during interpolation can
        // mutate a captured-outer caller lexical (`my $c; method Str {$c++; ...}`);
        // this op has no surrounding CallMethod op to drain the writeback, so
        // capture the caller frame's code and reconcile after the loop (see
        // coerce_numeric_bridge_value / exec_say_op).
        let caller_code = self.current_code;
        let mut result = String::new();
        for v in values {
            // Interpolating an unhandled Failure into a string throws its underlying
            // exception (Raku: a Failure is an "unthrown exception" that explodes on
            // use as a value). Mirrors the prefix:<~> stringify path; without this,
            // `"$f"` would silently render "Failure()" and hide real errors.
            if let Some(err) = self.failure_to_runtime_error_if_unhandled(&v) {
                return Err(err);
            }
            // Buf/Blob instances with "bytes" attribute: call .Str which throws X::Buf::AsStr
            // Blob type objects (no "bytes" attr, e.g. $*DISTRO.signature) stringify to ""
            if let Value::Instance { attributes, .. } = &v
                && Self::is_buf_value(&v)
                && attributes.contains_key("bytes")
            {
                let str_result = self.try_compiled_method_or_interpret(v, "Str", Vec::new())?;
                result.push_str(&str_result.to_string_value());
                continue;
            }
            // For non-Buf instances, try .Stringy() for string context (Raku spec:
            // string interpolation calls .Str which delegates to .Stringy).
            if let Value::Instance { .. } = &v {
                if let Ok(str_result) =
                    self.try_compiled_method_or_interpret(v.clone(), "Stringy", Vec::new())
                {
                    result.push_str(&str_result.to_string_value());
                    continue;
                }
                // Fall back to .Str() if .Stringy() is not defined
                if let Ok(str_result) =
                    self.try_compiled_method_or_interpret(v.clone(), "Str", Vec::new())
                {
                    result.push_str(&str_result.to_string_value());
                    continue;
                }
                // Fall through to default stringification
                result.push_str(&crate::runtime::utils::coerce_to_str(&v));
                continue;
            }
            result.push_str(&crate::runtime::utils::coerce_to_str(&v));
        }
        self.reconcile_caller_after_lazy_force(caller_code);
        if result.is_ascii() {
            self.stack.push(Value::str(result));
        } else {
            let normalized: String = result.nfc().collect();
            self.stack.push(Value::str(normalized));
        }
        Ok(())
    }

    /// Increment a value, calling .succ() on Instance values with custom methods.
    pub(super) fn increment_value_smart(&mut self, val: &Value) -> Result<Value, RuntimeError> {
        // Route user-defined `.succ` through the Interpreter's unified compiled-first
        // dispatch (same entry point `.Str` interpolation already uses) instead
        // of a raw interpreter tree-walk — one method-dispatch path, not two.
        if let Value::Instance { .. } = val
            && let Ok(result) =
                self.try_compiled_method_or_interpret(val.clone(), "succ", Vec::new())
        {
            return Ok(result);
        }
        Ok(Self::increment_value(val))
    }

    /// Enforce a scalar variable's declared type/subset constraint on the result
    /// of an in-place `++`/`--`. Raku re-checks the constraint after the
    /// mutation, so `my Even $x = 2; $x++` must throw X::TypeCheck::Assignment
    /// (3 is not Even) and leave the variable untouched. Native int/num/str
    /// variables wrap instead of erroring, so they are skipped, as are
    /// container (`@`/`%`/`&`) variables whose constraint applies to elements.
    pub(super) fn check_incdec_type_constraint(
        &mut self,
        name: &str,
        new_val: &Value,
    ) -> Result<(), RuntimeError> {
        if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
            return Ok(());
        }
        if let Some(constraint) = loan_env!(self, var_type_constraint(name)) {
            if crate::runtime::native_types::is_native_int_type(&constraint)
                || matches!(constraint.as_str(), "num" | "num32" | "num64" | "str")
            {
                return Ok(());
            }
            if !matches!(new_val, Value::Nil) && !self.type_matches_value(&constraint, new_val) {
                let display = if name.starts_with('$') {
                    name.to_string()
                } else {
                    format!("${}", name)
                };
                return Err(runtime::utils::type_check_assignment_typed_error(
                    &display,
                    &constraint,
                    new_val,
                ));
            }
        }
        Ok(())
    }

    /// Atomically read-modify-write a shared `ContainerRef` scalar cell for an
    /// in-place `++`/`--`, holding the cell lock across the whole RMW so that
    /// concurrent `start { $shared++ }` blocks (Track C: shared lexical cells)
    /// don't lose updates the way a separate lock-read / lock-write would.
    /// `increment` selects `++` vs `--`; `post` selects which value to push
    /// (post-inc/dec pushes the old value, pre-inc/dec the new one).
    /// Returns `true` if it handled the op atomically. Instance cells run a
    /// user-defined `.succ`/`.pred`, which can't be dispatched while the cell
    /// lock is held (reentrancy), so for those it returns `false` and the
    /// caller falls back to the (non-atomic) smart path.
    pub(super) fn atomic_container_incdec(
        &mut self,
        arc: &std::sync::Arc<std::sync::Mutex<Value>>,
        name: &str,
        increment: bool,
        post: bool,
    ) -> bool {
        let mut guard = arc.lock().unwrap();
        if matches!(&*guard, Value::Instance { .. }) {
            return false;
        }
        let old = self.normalize_incdec_source_with_type(name, guard.clone());
        let new_val = if increment {
            Self::increment_value(&old)
        } else {
            Self::decrement_value(&old)
        };
        *guard = new_val.clone();
        drop(guard);
        self.stack.push(if post { old } else { new_val });
        true
    }

    /// Decrement a value, calling .pred() on Instance values with custom methods.
    pub(super) fn decrement_value_smart(&mut self, val: &Value) -> Result<Value, RuntimeError> {
        // Route user-defined `.pred` through the Interpreter's unified compiled-first
        // dispatch (see increment_value_smart) instead of a raw interpreter
        // tree-walk — one method-dispatch path, not two.
        if let Value::Instance { .. } = val
            && let Ok(result) =
                self.try_compiled_method_or_interpret(val.clone(), "pred", Vec::new())
        {
            return Ok(result);
        }
        Ok(Self::decrement_value(val))
    }

    /// Propagate a scalar write along the `__mutsu_sigilless_alias::` chain from
    /// `name`: each alias target receives `val` in env and its local slot, and an
    /// attribute-twigil alias (`!x`) is additionally mirrored into self's shared
    /// cell so a sigilless attribute write (`has $x; $x = v`) reaches the cell
    /// (Phase 3 Stage 2c (ii)). Shared by the inc/dec ops; the cycle guard copes
    /// with the bidirectional `x ↔ !x` alias table.
    fn propagate_sigilless_alias_chain(&mut self, code: &CompiledCode, name: &str, val: &Value) {
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        let mut alias_name = self.env().get(&alias_key).and_then(|v| {
            if let Value::Str(n) = v {
                Some(n.to_string())
            } else {
                None
            }
        });
        let mut seen_aliases = std::collections::HashSet::new();
        while let Some(current_alias) = alias_name {
            if !seen_aliases.insert(current_alias.clone()) {
                break;
            }
            self.set_env_with_main_alias(&current_alias, val.clone());
            self.update_local_if_exists(code, &current_alias, val);
            self.write_self_attr_cell(&current_alias, val.clone());
            // Slice F: an inc/dec through a sigilless param (`\target`) aliases a
            // caller variable; record it so the call-site drain writes the env
            // value through to the caller's local slot (without relying on the
            // reverse `sync_locals_from_env` pull).
            self.pending_rw_writeback_sources
                .push(current_alias.clone());
            let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
            alias_name = self.env().get(&next_key).and_then(|v| {
                if let Value::Str(n) = v {
                    Some(n.to_string())
                } else {
                    None
                }
            });
        }
    }

    /// Store the result of a read-modify-write on a NAMED (env) scalar, mirroring
    /// the non-cell write-back tail of `exec_post_increment_op_inner`. Applies
    /// native-int wrapping and the type constraint, writes the value into env
    /// (with main-alias), the anonymous-state shadow, this code's local slot, any
    /// `:=`-bound sibling slots, the sigilless-alias chain, and — when the target
    /// is the topic `$_` — back to the topic source variable. Does NOT push to the
    /// stack; the caller decides what to leave there. Returns the (possibly
    /// native-int-wrapped) stored value. Shared by `++`/`--` and the fused
    /// compound-assignment op so their propagation is identical by construction.
    pub(super) fn store_named_scalar_rmw_result(
        &mut self,
        code: &CompiledCode,
        name: &str,
        new_val: Value,
    ) -> Result<Value, RuntimeError> {
        let new_val = self.maybe_wrap_native_int(name, new_val);
        self.check_incdec_type_constraint(name, &new_val)?;
        self.set_env_with_main_alias(name, new_val.clone());
        self.sync_anon_state_value(name, &new_val);
        self.update_local_if_exists(code, name, &new_val);
        // Slice F: an inc/dec (`$*foo++`) of a caller-declared dynamic variable
        // writes only `env` by name; record it so the call-site drain writes it
        // through to the caller frame's slot (mirrors the SetGlobal path).
        if name.starts_with('*') {
            self.pending_rw_writeback_sources.push(name.to_string());
        }
        // Propagate via local_bind_pairs (for `:=` bindings within this scope
        // or cross-scope bindings resolved by resolve_pending_alias_binds).
        if let Some(source_idx) = code.locals.iter().position(|n| n == name) {
            let mut env_updates = Vec::new();
            for &(src, tgt) in &self.local_bind_pairs {
                if src == source_idx {
                    self.locals[tgt] = new_val.clone();
                    env_updates.push((code.locals[tgt].clone(), new_val.clone()));
                }
            }
            // Also update env so ensure_locals_synced doesn't overwrite
            // with stale data.
            for (target_name, val) in env_updates {
                self.set_env_with_main_alias(&target_name, val);
            }
        }
        // Propagate the new value along the sigilless alias chain and into self's
        // shared cell for an attribute-twigil alias.
        self.propagate_sigilless_alias_chain(code, name, &new_val);
        // Write back to source variable when the target is `$_` bound to a container.
        if name == "_"
            && let Some(ref source_var) = self.topic_source_var
            && !source_var.starts_with('@')
            && !source_var.starts_with('%')
        {
            let sv = source_var.clone();
            self.set_env_with_main_alias(&sv, new_val.clone());
            self.update_local_if_exists(code, &sv, &new_val);
        }
        Ok(new_val)
    }

    /// Apply a fused compound-assignment base operator to `left OP right` by
    /// reusing the exact `exec_*_op` the plain `Binary` path runs (so operator
    /// semantics, coercions, and user-`infix` overloads are identical). The
    /// operands are pushed left-then-right (matching the stack order each
    /// `exec_*_op` pops) and the result is popped back off.
    fn apply_compound_base_op(
        &mut self,
        op: crate::opcode::CompoundBaseOp,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        use crate::opcode::CompoundBaseOp as B;
        self.stack.push(left);
        self.stack.push(right);
        match op {
            B::Add => self.exec_add_op()?,
            B::Sub => self.exec_sub_op()?,
            B::Mul => self.exec_mul_op()?,
            B::Div => self.exec_div_op()?,
            B::Mod => self.exec_mod_op()?,
            B::Pow => self.exec_pow_op()?,
            B::Concat => self.exec_concat_op(),
            B::BitAnd => self.exec_bit_and_op(),
            B::BitOr => self.exec_bit_or_op(),
            B::BitXor => self.exec_bit_xor_op(),
            B::BitShiftLeft => self.exec_bit_shift_left_op(),
            B::BitShiftRight => self.exec_bit_shift_right_op(),
            B::IntDiv => self.exec_int_div_op()?,
            B::IntMod => self.exec_int_mod_op()?,
            B::Gcd => self.exec_gcd_op(),
            B::Lcm => self.exec_lcm_op(),
            B::InfixMin => self.exec_infix_min_op(),
            B::InfixMax => self.exec_infix_max_op(),
            B::StringRepeat => self.exec_string_repeat_op()?,
        }
        Ok(self.stack.pop().unwrap())
    }

    /// Execute a fused compound assignment to a NAMED (env) scalar: `$x OP= rhs`.
    /// The rhs is already on the stack. Structurally mirrors
    /// `exec_post_increment_op_inner`: a `ContainerRef` cell gets an atomic
    /// locked read-modify-write (Track C cross-thread atomicity), a `Proxy` is
    /// fetched/op'd/stored, and a plain env scalar is written through the shared
    /// `store_named_scalar_rmw_result` tail so propagation is identical to `++`.
    /// Leaves the new value on the stack.
    pub(super) fn exec_atomic_compound_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        op: crate::opcode::CompoundBaseOp,
    ) -> Result<(), RuntimeError> {
        let rhs = self.stack.pop().unwrap();
        self.resolve_pending_alias_binds(code);
        let name = Self::const_str(code, name_idx);
        self.check_readonly_for_increment(name)?;
        // Default to Nil (NOT Int(0) like `++`) so `my $w; $w ~= "z"` yields "z",
        // not "0z"; the binary op descalarizes/numifies/stringifies Nil itself.
        let raw_val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Nil);
        // ContainerRef cell: atomic RMW under the cell lock so concurrent
        // `start { $shared += n }` blocks don't lose updates (Track C).
        if let Value::ContainerRef(ref arc) = raw_val {
            let arc = arc.clone();
            // Hold the cell lock across the whole read-modify-write so concurrent
            // threads can't interleave and lose updates. `rhs` was already popped
            // (a concrete value), and the base op operates only on `old`/`rhs`, so
            // it never re-enters this cell — no deadlock.
            let mut guard = arc.lock().unwrap();
            let old = guard.clone();
            let new_val = self.apply_compound_base_op(op, old, rhs)?;
            *guard = new_val.clone();
            drop(guard);
            self.stack.push(new_val);
            return Ok(());
        }
        // Proxy: FETCH -> op -> STORE.
        if let Value::Proxy { storer, .. } = &raw_val
            && !matches!(storer.as_ref(), Value::Nil)
        {
            let fetched = loan_env!(self, auto_fetch_proxy(&raw_val))?;
            let new_val = self.apply_compound_base_op(op, fetched, rhs)?;
            loan_env!(self, assign_proxy_lvalue(raw_val, new_val.clone()))?;
            self.stack.push(new_val);
            return Ok(());
        }
        // Plain env scalar: compute old OP rhs, store via the shared `++` tail so
        // METHOD captured-outer propagation is identical to `++` by construction.
        let new_val = self.apply_compound_base_op(op, raw_val, rhs)?;
        let stored = self.store_named_scalar_rmw_result(code, name, new_val)?;
        self.stack.push(stored);
        Ok(())
    }

    pub(super) fn exec_post_increment_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // Phase 3 Stage 2: scalar attribute increments read-modify-write the cell.
        let attr_name = Self::const_str(code, name_idx).to_string();
        self.sync_attr_local_from_cell_by_name(code, &attr_name);
        let r = self.exec_post_increment_op_inner(code, name_idx);
        if r.is_ok() {
            self.mirror_attr_local_to_cell_by_name(code, &attr_name);
        }
        r
    }

    fn exec_post_increment_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // Lazily convert pending alias bind names into local_bind_pairs.
        self.resolve_pending_alias_binds(code);
        let name = Self::const_str(code, name_idx);
        // Handle $CALLER::varname++ — increment through caller scope
        if let Some((bare_name, depth)) = crate::compiler::Compiler::parse_caller_prefix(name) {
            let raw_val = loan_env!(self, get_caller_var(&bare_name, depth))?;
            let val = Self::normalize_incdec_source(raw_val);
            let new_val = self.increment_value_smart(&val)?;
            loan_env!(self, set_caller_var(&bare_name, depth, new_val))?;
            self.stack.push(val);
            return Ok(());
        }
        self.check_readonly_for_increment(name)?;
        if name.starts_with('!')
            && let Some(slot) = self.find_local_slot(code, name)
            && !matches!(self.locals[slot], Value::Proxy { .. })
        {
            // ContainerRef: increment through the shared arc (e.g. `$!attr := outer_var`).
            if let Value::ContainerRef(ref arc) = self.locals[slot].clone() {
                if self.atomic_container_incdec(arc, name, true, true) {
                    return Ok(());
                }
                let inner = arc.lock().unwrap().clone();
                let val = self.normalize_incdec_source_with_type(name, inner);
                let new_val = self.increment_value_smart(&val)?;
                arc.lock().unwrap().clone_from(&new_val);
                self.stack.push(val);
                return Ok(());
            }
            let raw_val = self.locals[slot].clone();
            let val = self.normalize_incdec_source_with_type(name, raw_val);
            let new_val = self.increment_value_smart(&val)?;
            self.locals[slot] = new_val.clone();
            self.flush_local_to_env(code, slot);
            // Propagate the new value along the sigilless alias chain and into
            // self's shared cell for an attribute-twigil alias.
            self.propagate_sigilless_alias_chain(code, name, &new_val);
            self.stack.push(val);
            return Ok(());
        }
        let raw_val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        // ContainerRef: deref for increment, write back through the shared container.
        // Use an atomic read-modify-write under the cell lock so concurrent
        // `start { $shared++ }` blocks don't lose updates (Track C).
        if let Value::ContainerRef(ref arc) = raw_val {
            if self.atomic_container_incdec(arc, name, true, true) {
                return Ok(());
            }
            let inner = arc.lock().unwrap().clone();
            let val = self.normalize_incdec_source_with_type(name, inner);
            let new_val = self.increment_value_smart(&val)?;
            arc.lock().unwrap().clone_from(&new_val);
            self.stack.push(val);
            return Ok(());
        }
        // If the value is a Proxy, FETCH → increment → STORE
        if let Value::Proxy { storer, .. } = &raw_val
            && !matches!(storer.as_ref(), Value::Nil)
        {
            let fetched = loan_env!(self, auto_fetch_proxy(&raw_val))?;
            let val = Self::normalize_incdec_source(fetched);
            let new_val = self.increment_value_smart(&val)?;
            loan_env!(self, assign_proxy_lvalue(raw_val, new_val))?;
            self.stack.push(val);
            return Ok(());
        }
        let val = self.normalize_incdec_source_with_type(name, raw_val);
        let new_val = self.increment_value_smart(&val)?;
        self.store_named_scalar_rmw_result(code, name, new_val)?;
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_post_decrement_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // Phase 3 Stage 2: scalar attribute decrements read-modify-write the cell.
        let attr_name = Self::const_str(code, name_idx).to_string();
        self.sync_attr_local_from_cell_by_name(code, &attr_name);
        let r = self.exec_post_decrement_op_inner(code, name_idx);
        if r.is_ok() {
            self.mirror_attr_local_to_cell_by_name(code, &attr_name);
        }
        r
    }

    fn exec_post_decrement_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        self.check_readonly_for_increment(name)?;
        if name.starts_with('!')
            && let Some(slot) = self.find_local_slot(code, name)
            && !matches!(self.locals[slot], Value::Proxy { .. })
        {
            // ContainerRef: decrement through the shared arc (e.g. `$!attr := outer_var`).
            if let Value::ContainerRef(ref arc) = self.locals[slot].clone() {
                if self.atomic_container_incdec(arc, name, false, true) {
                    return Ok(());
                }
                let inner = arc.lock().unwrap().clone();
                let val = self.normalize_incdec_source_with_type(name, inner);
                let new_val = self.decrement_value_smart(&val)?;
                arc.lock().unwrap().clone_from(&new_val);
                self.stack.push(val);
                return Ok(());
            }
            let raw_val = self.locals[slot].clone();
            let val = self.normalize_incdec_source_with_type(name, raw_val);
            let new_val = self.decrement_value_smart(&val)?;
            self.locals[slot] = new_val.clone();
            self.flush_local_to_env(code, slot);
            // Propagate the new value along the sigilless alias chain and into
            // self's shared cell for an attribute-twigil alias.
            self.propagate_sigilless_alias_chain(code, name, &new_val);
            self.stack.push(val);
            return Ok(());
        }
        let raw_val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        // ContainerRef: deref for decrement, write back through the shared container.
        // Atomic RMW under the cell lock for concurrent `start` blocks (Track C).
        if let Value::ContainerRef(ref arc) = raw_val {
            if self.atomic_container_incdec(arc, name, false, true) {
                return Ok(());
            }
            let inner = arc.lock().unwrap().clone();
            let val = self.normalize_incdec_source_with_type(name, inner);
            let new_val = self.decrement_value_smart(&val)?;
            arc.lock().unwrap().clone_from(&new_val);
            self.stack.push(val);
            return Ok(());
        }
        let val = self.normalize_incdec_source_with_type(name, raw_val);
        let new_val = self.decrement_value_smart(&val)?;
        self.store_named_scalar_rmw_result(code, name, new_val)?;
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_post_increment_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_inc_dec_index_op(code, name_idx, true, false)
    }

    pub(super) fn exec_post_decrement_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_inc_dec_index_op(code, name_idx, false, false)
    }

    pub(super) fn exec_pre_increment_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_inc_dec_index_op(code, name_idx, true, true)
    }

    pub(super) fn exec_pre_decrement_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.exec_inc_dec_index_op(code, name_idx, false, true)
    }

    fn exec_inc_dec_index_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        increment: bool,
        return_new: bool,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        // Element type constraint of the variable, used to fill array holes with
        // the proper type object (`(Int)`) instead of Nil when autovivifying.
        let declared_constraint_incdec = loan_env!(self, var_type_constraint(&name));
        let declared_type_incdec = self
            .env()
            .get(&name)
            .cloned()
            .and_then(|v| self.container_type_metadata(&v))
            .and_then(|info| info.declared_type);
        let _target_is_mixhash_incdec = declared_type_incdec
            .as_deref()
            .is_some_and(|t| t == "MixHash");
        let _target_is_baghash_incdec = declared_type_incdec
            .as_deref()
            .is_some_and(|t| t == "BagHash");
        let _target_is_sethash_incdec = declared_type_incdec
            .as_deref()
            .is_some_and(|t| t == "SetHash");
        let idx_val = self.stack.pop().unwrap_or(Value::Nil);
        let key = idx_val.to_string_value();
        let container = self.get_env_with_main_alias(&name);
        // `$c[0]++` / `$c<a>++` on a Capture: when the element is a shared
        // `ContainerRef` cell (built from `\($a)` / `\(:$a)`), increment *through*
        // the cell so the original variable observes the change. The generic
        // Hash/Array path below does not understand Captures.
        if let Some(Value::Capture { positional, named }) = container.as_ref() {
            let elem = if let Ok(i) = key.parse::<usize>() {
                positional.get(i).cloned()
            } else {
                named.get(&key).cloned()
            };
            if let Some(Value::ContainerRef(arc)) = elem {
                let inner = arc.lock().unwrap().clone();
                let effective = Self::normalize_incdec_source(inner);
                let new_val = if increment {
                    self.increment_value_smart(&effective)?
                } else {
                    self.decrement_value_smart(&effective)?
                };
                arc.lock().unwrap().clone_from(&new_val);
                self.stack
                    .push(if return_new { new_val } else { effective });
                return Ok(());
            }
        }
        // `$n[0]++` / `$h<k>++` where `$n`/`$h` is itself a shared `ContainerRef`
        // cell (a scalar-bound container param, `my $s = @a`, etc.): increment the
        // element *through* the cell so the caller's container observes it. The
        // generic Array/Hash read/writeback paths below only match plain
        // containers, so a `ContainerRef` would otherwise read `Nil` and discard
        // the write (unlike `$n[0] = …` / `$n[0] += …`, which descend the cell).
        // `$n[0]++` / `$h<k>++` where the variable resolves to a raw `ContainerRef`
        // cell (a positional scalar-bound container param, `my $s := @a`, etc.):
        // increment the element *through* the cell so the caller observes it. The
        // generic Array/Hash read/writeback paths below match plain containers
        // only, so a raw cell would read `Nil` and discard the write. (Named
        // params resolve to a deref'd-but-Arc-shared plain container instead, which
        // the strong_count>1 in-place writeback below handles.)
        if let Some(Value::ContainerRef(arc)) = container.as_ref() {
            let inner = arc.lock().unwrap().clone();
            let current = match &inner {
                Value::Hash(h) => h.get(&key).cloned().unwrap_or(Value::Nil),
                Value::Array(arr, ..) => key
                    .parse::<usize>()
                    .ok()
                    .and_then(|i| arr.get(i).cloned())
                    .unwrap_or(Value::Nil),
                _ => Value::Nil,
            };
            let effective = Self::normalize_incdec_source(match current {
                Value::Nil => Value::Int(0),
                other => other,
            });
            let new_val = if increment {
                self.increment_value_smart(&effective)?
            } else {
                self.decrement_value_smart(&effective)?
            };
            let mut updated = inner;
            match &mut updated {
                Value::Hash(h) => {
                    Value::hash_insert_through(
                        &mut Arc::make_mut(h).map,
                        key.clone(),
                        new_val.clone(),
                    );
                }
                Value::Array(arr, ..) => {
                    if let Ok(i) = key.parse::<usize>() {
                        let a = Arc::make_mut(arr);
                        while a.len() <= i {
                            a.push(Value::Nil);
                        }
                        a[i] = new_val.clone();
                    }
                }
                _ => {}
            }
            *arc.lock().unwrap() = updated;
            self.stack
                .push(if return_new { new_val } else { effective });
            return Ok(());
        }
        let current = if let Some(container_value) = container.as_ref() {
            match container_value {
                Value::Hash(h) => h.get(&key).cloned().unwrap_or(Value::Nil),
                Value::Array(arr, ..) => {
                    if let Ok(i) = key.parse::<usize>() {
                        arr.get(i).cloned().unwrap_or(Value::Nil)
                    } else {
                        Value::Nil
                    }
                }
                Value::Mix(mix, _) => mix
                    .get(&key)
                    .map_or(Value::Int(0), |w| Self::mix_weight_as_value(*w)),
                Value::Set(set, _) => {
                    if set.contains(&key) {
                        Value::Bool(true)
                    } else {
                        Value::Bool(false)
                    }
                }
                Value::Bag(bag, _) => {
                    Value::from_bigint(bag.get(&key).cloned().unwrap_or_default())
                }
                _ => Value::Nil,
            }
        } else {
            Value::Nil
        };
        let effective = match &current {
            Value::Nil => {
                // Check if the container has an `is default(...)` value;
                // e.g. `my @a is default(42); @a[0]++` should increment 42.
                if let Some(def) = self.var_default(&name) {
                    if matches!(def, Value::Nil) {
                        Value::Int(0)
                    } else {
                        def.clone()
                    }
                } else {
                    Value::Int(0)
                }
            }
            other => other.clone(),
        };
        let effective = Self::normalize_incdec_source(effective);
        let new_val = if increment {
            self.increment_value_smart(&effective)?
        } else {
            self.decrement_value_smart(&effective)?
        };
        // A Bag/BagHash holds non-negative integer counts: decrementing a weight
        // below 0 clamps the *returned* (and stored) value to 0 (the element is
        // then removed), so `--$bh<k>` on an absent key returns 0, not -1. Mix
        // weights are unbounded and keep their negative value.
        let target_is_bag = matches!(container.as_ref(), Some(Value::Bag(..)))
            || matches!(
                container.as_ref(),
                Some(Value::Package(sym)) if sym.resolve() == "BagHash"
            )
            || declared_type_incdec.as_deref() == Some("BagHash")
            || declared_constraint_incdec.as_deref() == Some("BagHash");
        let new_val = if target_is_bag {
            match &new_val {
                Value::Int(n) if *n < 0 => Value::Int(0),
                _ => new_val,
            }
        } else {
            new_val
        };
        // Type-check the incremented value against the element constraint of a
        // typed array/hash, e.g. `subset Y of Int where 1..10; my Y @x; @x[0]=10;
        // @x[0]++` must throw when the new value (11) falls outside the subset.
        // Native arrays wrap instead of erroring, so skip them. Skip container-type
        // constraints (e.g. `%h is SetHash`), where the constraint names the whole
        // container rather than its element/value type.
        if (name.starts_with('@') || name.starts_with('%'))
            && let Some(constraint) = declared_constraint_incdec.as_deref()
            && !crate::runtime::native_types::is_native_array_element_type(constraint)
            && !matches!(constraint, "num" | "num32" | "num64" | "str")
            && !matches!(
                constraint,
                "Hash"
                    | "Array"
                    | "Map"
                    | "List"
                    | "Bag"
                    | "Set"
                    | "Mix"
                    | "BagHash"
                    | "SetHash"
                    | "MixHash"
                    | "Seq"
            )
            && !self.is_container_subclass(constraint)
            && !matches!(&new_val, Value::Nil)
            && !self.type_matches_value(constraint, &new_val)
        {
            return Err(runtime::utils::type_check_element_typed_error(
                &name, constraint, &new_val,
            ));
        }
        // Modify the container in-place in the env to preserve Arc sharing
        // (e.g. when two variables reference the same array via Arc).
        // First try to modify via env_mut().get_mut() to avoid clone.
        let modified_in_place = if let Some(container_value) = self.env_mut().get_mut(&name) {
            match container_value {
                Value::Hash(h) => {
                    // Mirror the array arm below: when the hash Arc is shared
                    // (strong_count > 1) via a scalar-bound `ContainerRef` cell
                    // (`sub f($h){ $h<k>++ }` / `my $s = %h; $s<k>++`), mutate it
                    // in place so the caller observes the change. `Arc::make_mut`
                    // would COW-detach and silently drop the write (the array
                    // path already special-cased this; the hash path did not).
                    let use_inplace = Arc::strong_count(h) > 1 && !name.starts_with('%');
                    if use_inplace {
                        // SAFETY: aliased in-place mutation of a shared hash
                        // (strong_count > 1, the shared-cell case); mirrors the
                        // array arm's `arc_contents_mut` usage.
                        let h = unsafe { crate::value::arc_contents_mut(h) };
                        Value::hash_insert_through(&mut h.map, key.clone(), new_val.clone());
                    } else {
                        Value::hash_insert_through(
                            &mut Arc::make_mut(h).map,
                            key.clone(),
                            new_val.clone(),
                        );
                    }
                    true
                }
                Value::Array(arr, ..) => {
                    if let Ok(i) = idx_val.to_string_value().parse::<usize>() {
                        // Use in-place mutation when the array is shared
                        // (strong_count > 1) to preserve identity semantics,
                        // matching the behavior of index assignment.
                        let use_inplace = Arc::strong_count(arr) > 1 && !name.starts_with('@');
                        let a: &mut crate::value::ArrayData = if use_inplace {
                            // SAFETY: aliased in-place mutation of a shared array
                            // (strong_count > 1, the case that needs the shared
                            // write); see `arc_contents_mut`.
                            unsafe { crate::value::arc_contents_mut(arr) }
                        } else {
                            Arc::make_mut(arr)
                        };
                        // Fill holes with the element type's type object for a
                        // typed array (e.g. `my Int @a; @a[4]++` leaves `(Int)`
                        // placeholders), or 0/0.0/"" for native arrays.
                        let fill =
                            Self::native_fill_for_constraint(declared_constraint_incdec.as_deref());
                        while a.len() <= i {
                            a.push(fill.clone());
                        }
                        a[i] = new_val.clone();
                        true
                    } else {
                        false
                    }
                }
                Value::Mix(mix, is_mutable) => {
                    if !*is_mutable {
                        return Err(RuntimeError::assignment_ro(Some("Mix")));
                    }
                    let weight = Self::mix_assignment_weight(&new_val)?;
                    let m = Arc::make_mut(mix);
                    if new_val.truthy() {
                        m.insert(key.clone(), weight);
                    } else {
                        m.remove(&key);
                    }
                    true
                }
                Value::Set(set, is_mutable) => {
                    if !*is_mutable {
                        return Err(RuntimeError::assignment_ro(Some("Set")));
                    }
                    let s = Arc::make_mut(set);
                    if new_val.truthy() {
                        s.insert(key.clone());
                    } else {
                        s.remove(&key);
                    }
                    true
                }
                Value::Bag(bag, is_mutable) => {
                    if !*is_mutable {
                        return Err(RuntimeError::assignment_ro(Some("Bag")));
                    }
                    let b = Arc::make_mut(bag);
                    let n = match &new_val {
                        Value::Int(i) => num_bigint::BigInt::from(*i),
                        Value::BigInt(big) => (**big).clone(),
                        _ => num_bigint::BigInt::from(0),
                    };
                    if num_traits::Signed::is_positive(&n) {
                        b.insert(key.clone(), n);
                    } else {
                        b.remove(&key);
                    }
                    true
                }
                // Autovivify typed variables: `my MixHash $mh; $mh<key>++`
                Value::Package(sym) => {
                    let type_name = sym.resolve();
                    match type_name.as_str() {
                        "MixHash" => {
                            let mut weights = HashMap::new();
                            let weight = Self::mix_assignment_weight(&new_val)?;
                            if new_val.truthy() {
                                weights.insert(key.clone(), weight);
                            }
                            *container_value = Value::mix_hash(weights);
                            true
                        }
                        "BagHash" => {
                            let mut counts = HashMap::new();
                            if let Value::Int(n) = &new_val
                                && *n > 0
                            {
                                counts.insert(key.clone(), *n);
                            }
                            *container_value = Value::bag_hash(counts);
                            true
                        }
                        "SetHash" => {
                            let mut items = std::collections::HashSet::new();
                            if new_val.truthy() {
                                items.insert(key.clone());
                            }
                            *container_value =
                                Value::Set(Arc::new(crate::value::SetData::new(items)), true);
                            true
                        }
                        _ => false,
                    }
                }
                _ => false,
            }
        } else {
            false
        };
        if modified_in_place {
            // Update local slot to match the modified env value
            if let Some(val) = self.env().get(&name).cloned() {
                self.update_local_if_exists(code, &name, &val);
            }
        } else {
            // Autovivify typed containers for inc/dec on undefined variables
            let constraint = loan_env!(self, var_type_constraint(&name));
            let effective_type = declared_type_incdec.as_deref().or(constraint.as_deref());
            if let Some(type_name) = effective_type
                && matches!(type_name, "MixHash" | "BagHash" | "SetHash")
            {
                let new_container = match type_name {
                    "MixHash" => {
                        let mut weights = HashMap::new();
                        let weight = Self::mix_assignment_weight(&new_val)?;
                        if new_val.truthy() {
                            weights.insert(key.clone(), weight);
                        }
                        Value::mix_hash(weights)
                    }
                    "BagHash" => {
                        let mut counts = HashMap::new();
                        if let Value::Int(n) = &new_val
                            && *n > 0
                        {
                            counts.insert(key.clone(), *n);
                        }
                        Value::bag_hash(counts)
                    }
                    "SetHash" => {
                        let mut items = std::collections::HashSet::new();
                        if new_val.truthy() {
                            items.insert(key.clone());
                        }
                        Value::Set(Arc::new(crate::value::SetData::new(items)), true)
                    }
                    _ => unreachable!(),
                };
                self.env_mut().insert(name.clone(), new_container);
                if let Some(val) = self.env().get(&name).cloned() {
                    self.update_local_if_exists(code, &name, &val);
                }
            }
        }
        if return_new {
            self.stack.push(new_val);
        } else {
            self.stack.push(effective);
        }
        Ok(())
    }

    /// Track C: route a simple `%h{$k} = $v` through the shared cell when a
    /// thread is active, so concurrent `start` blocks accumulate into one hash
    /// instead of each mutating a private snapshot (last-writer-wins).
    /// Applies the same simplicity guards as `try_fast_hash_element_assign`
    /// (rejecting type constraints, defaults, bound indices, complex indices).
    /// Returns `Some(Ok)` when it wrote through the shared hash, else `None`.
    fn try_shared_hash_element_assign(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Option<Result<(), RuntimeError>> {
        // Cheap early-out: only meaningful while a thread shares this env.
        if !self.shared_vars_active {
            return None;
        }
        if !self.local_bind_pairs.is_empty() {
            return None;
        }
        let var_name = Self::const_str(code, name_idx);
        if !var_name.starts_with('%') {
            return None;
        }
        let stack_len = self.stack.len();
        if stack_len < 2 {
            return None;
        }
        let idx_ref = &self.stack[stack_len - 1];
        let val_ref = &self.stack[stack_len - 2];
        // Reject complex index types (slices/ranges/junctions need the full path).
        if matches!(
            idx_ref,
            Value::Array(..)
                | Value::Junction { .. }
                | Value::GenericRange { .. }
                | Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::Nil
                | Value::Seq(..)
                | Value::Slip(..)
        ) {
            return None;
        }
        // Reject bind-mode markers and Nil values (need default/type handling).
        if matches!(val_ref, Value::Pair(name, _) if name == "__mutsu_bind_index_value")
            || matches!(val_ref, Value::Nil)
        {
            return None;
        }
        // Reject when type/key constraints, defaults, readonly, or bound indices
        // exist — those need the full assignment path's healing.
        if self.var_type_constraint_fast(var_name).is_some()
            || self.var_default(var_name).is_some()
            || self.var_hash_key_constraint_fast(var_name)
            || self.readonly_vars().contains(var_name)
        {
            return None;
        }
        {
            let bound_key = format!("__mutsu_bound_index::{}", var_name);
            if self.env().contains_key(&bound_key) {
                return None;
            }
        }
        let var_name = var_name.to_string();
        let key = idx_ref.to_string_value();
        // Commit: pop idx then val and write through the shared cell.
        let idx = self.stack.pop().unwrap();
        let val = self.stack.pop().unwrap();
        match loan_env!(
            self,
            assign_hash_elem_to_shared_var(&var_name, key, val.clone())
        ) {
            Some(_) => {
                self.stack.push(val);
                Some(Ok(()))
            }
            None => {
                // Not a shared hash after all (e.g. not yet seeded): restore the
                // [val, idx] stack order and fall through to the normal path.
                self.stack.push(val);
                self.stack.push(idx);
                None
            }
        }
    }

    /// Track C: route a simple `@a[$i] = $v` through the shared cell when a
    /// thread is active, so concurrent `start` blocks accumulate into one array
    /// instead of each mutating a private snapshot (last-writer-wins). Handles
    /// only the simple case: a plain non-negative integer index, a plain value,
    /// and no type constraints / defaults / shaped dims / bound indices. Returns
    /// `Some(Ok)` when it wrote through the shared array, else `None`.
    fn try_shared_array_element_assign(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Option<Result<(), RuntimeError>> {
        if !self.shared_vars_active {
            return None;
        }
        if !self.local_bind_pairs.is_empty() {
            return None;
        }
        let var_name = Self::const_str(code, name_idx);
        if !var_name.starts_with('@') {
            return None;
        }
        let stack_len = self.stack.len();
        if stack_len < 2 {
            return None;
        }
        // Only a plain non-negative Int index; anything else (slice, Whatever,
        // negative, lazy) needs the full index-assign path.
        let idx = match &self.stack[stack_len - 1] {
            Value::Int(n) if *n >= 0 => *n as usize,
            _ => return None,
        };
        let val_ref = &self.stack[stack_len - 2];
        if matches!(val_ref, Value::Pair(name, _) if name == "__mutsu_bind_index_value")
            || matches!(val_ref, Value::Nil)
        {
            return None;
        }
        // Reject typed / defaulted / shaped / readonly / bound arrays — those need
        // the full path's native-fill, hole, and shape handling.
        if self.var_type_constraint_fast(var_name).is_some()
            || self.var_default(var_name).is_some()
            || self.readonly_vars().contains(var_name)
        {
            return None;
        }
        {
            let shaped_key = format!("__mutsu_shaped_array_dims::{}", var_name);
            let bound_key = format!("__mutsu_bound_index::{}", var_name);
            if self.env().contains_key(&shaped_key) || self.env().contains_key(&bound_key) {
                return None;
            }
        }
        let var_name = var_name.to_string();
        // Commit: pop idx then val and write through the shared cell.
        let idx_val = self.stack.pop().unwrap();
        let val = self.stack.pop().unwrap();
        match loan_env!(
            self,
            assign_array_elem_to_shared_var(&var_name, idx, val.clone())
        ) {
            Some(_) => {
                self.stack.push(val);
                Some(Ok(()))
            }
            None => {
                // Not a shared array (e.g. not yet seeded): restore [val, idx]
                // stack order and fall through to the normal path.
                self.stack.push(val);
                self.stack.push(idx_val);
                None
            }
        }
    }

    /// Fast path for simple hash element assignment: `%h{$key} = $val`.
    ///
    /// Returns `Some(Ok(()))` if the fast path handled the assignment,
    /// `None` if the caller should fall through to the full slow path.
    /// The fast path never returns `Some(Err(...))` — any edge case that
    /// might error falls through to the slow path instead.
    ///
    /// Preconditions checked (all must hold for the fast path to fire):
    /// - Variable name starts with `%` (hash sigil)
    /// - Stack top two values are a simple index (not Array/Junction/GenericRange/Nil)
    ///   and a simple value (not a bind-mode marker)
    /// - The variable exists in the env as `Value::Hash`
    /// - No type constraints, no key constraints, no var defaults
    /// - Variable is not readonly (not bound via `:=`)
    /// - No container type metadata on the hash
    fn try_fast_hash_element_assign(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        _is_positional: bool,
    ) -> Option<Result<(), RuntimeError>> {
        // Reject if there are any local bind pairs (`:=` bindings in scope)
        if !self.local_bind_pairs.is_empty() {
            return None;
        }
        let var_name = Self::const_str(code, name_idx);
        // Only handle %-sigiled hash variables
        if !var_name.starts_with('%') {
            return None;
        }
        // Peek at stack to check for bind-mode marker and complex indices
        // without popping (we'll pop only if we commit to the fast path)
        let stack_len = self.stack.len();
        if stack_len < 2 {
            return None;
        }
        // idx is on top of stack, val is below it
        let idx_ref = &self.stack[stack_len - 1];
        let val_ref = &self.stack[stack_len - 2];
        // Reject complex index types that need special handling
        if matches!(
            idx_ref,
            Value::Array(..)
                | Value::Junction { .. }
                | Value::GenericRange { .. }
                | Value::Range(..)
                | Value::RangeExcl(..)
                | Value::RangeExclStart(..)
                | Value::RangeExclBoth(..)
                | Value::Nil
                | Value::Seq(..)
                | Value::Slip(..)
        ) {
            return None;
        }
        // Reject bind-mode marker values
        if matches!(val_ref, Value::Pair(name, _) if name == "__mutsu_bind_index_value") {
            return None;
        }
        // Reject Nil values (need default/type-object handling)
        if matches!(val_ref, Value::Nil) {
            return None;
        }
        // Check that no type constraints, key constraints, or defaults exist
        // Use fast lookups that avoid format! allocations
        if self.var_type_constraint_fast(var_name).is_some()
            || self.var_default(var_name).is_some()
            || self.var_hash_key_constraint_fast(var_name)
            || self.readonly_vars().contains(var_name)
        {
            return None;
        }
        // Reject if any bound indices exist for this variable
        // (e.g. `%h<a> := $foo` makes element writes propagate to $foo)
        {
            let bound_key = format!("__mutsu_bound_index::{}", var_name);
            if self.env().contains_key(&bound_key) {
                return None;
            }
        }
        // Check that the variable exists in env as a plain Hash
        // and that it has no container type metadata
        let env = self.env();
        match env.get(var_name) {
            Some(Value::Hash(hash_arc)) => {
                let strong_count = Arc::strong_count(hash_arc);
                // Reject if the hash Arc has more than 2 refs (e.g. HashEntryRef binding)
                // strong_count == 1: only env holds it (no local slot)
                // strong_count == 2: env + locals hold it (common case in for loops)
                // strong_count > 2: external binding exists, fall through to slow path
                if strong_count > 2 {
                    return None;
                }
                let local_slot = if strong_count == 2 {
                    // The extra ref should be from locals — verify
                    match self.find_local_slot(code, var_name) {
                        Some(slot) => Some(slot),
                        None => return None,
                    }
                } else {
                    None
                };
                // Reject if there's container type metadata
                if hash_arc.has_type_meta() {
                    return None;
                }
                // Peek at the key to check if the existing element is a bound ref
                let peek_key = self.stack[stack_len - 1].to_string_value();
                if let Some(existing) = hash_arc.get(&peek_key) {
                    let is_bound = match existing {
                        Value::HashEntryRef { .. } | Value::Scalar(..) => true,
                        // Slice 2b: a `=`-shared (or `:=`-bound) element holds a
                        // `ContainerRef` cell; reassignment needs the slow path's
                        // replace-vs-write-through guard, not a blind insert.
                        Value::ContainerRef(_) => true,
                        Value::Pair(name, _) if name.starts_with("__mutsu_bound") => true,
                        _ => false,
                    };
                    if is_bound {
                        return None;
                    }
                }
                // All checks passed — commit to fast path
                let idx = self.stack.pop().unwrap();
                let val = self.stack.pop().unwrap();
                let key = idx.to_string_value();
                // When locals and env share the same Arc (strong_count == 2),
                // drop the local ref first so Arc::make_mut can mutate in-place
                // instead of cloning the entire HashMap (O(n) → O(1) per insert).
                if let Some(slot) = local_slot {
                    self.locals[slot] = Value::Nil;
                }
                if let Some(Value::Hash(hash)) = self.env_mut().get_mut(var_name) {
                    Value::hash_insert_through(
                        &mut Arc::make_mut(hash).map,
                        key.clone(),
                        val.clone(),
                    );
                }
                // Restore the local slot to point to the (now mutated) env Arc
                if let Some(slot) = local_slot
                    && let Some(env_val) = self.env().get(var_name).cloned()
                {
                    self.locals[slot] = env_val;
                }
                // strong_count==1 divergence repair: a re-entrant call evaluated
                // as the RHS (e.g. a `proto {*}` redispatch) can swap `self.env`
                // out from under the block's local slot via
                // `restore_env_preserving_existing`, leaving the slot pointing at
                // a stale, detached Arc while env holds the live one (strong_count
                // drops to 1). The assign above mutated only env, so a local slot
                // that still exists is — by definition of strong_count==1 — a
                // diverged copy. Mirror the live env value back to it to keep the
                // dual store coherent, so a later `state`-var persist (which reads
                // env first, then `sync_env_from_locals` flushes the slot) does not
                // clobber the value with the stale slot. No-op for a genuine
                // env-only hash (e.g. `%*ENV`) that has no local slot, and the
                // default build's blanket reconcile makes it redundant (byte-
                // identical) — it only matters on the single-store path.
                if local_slot.is_none()
                    && let Some(slot) = self.find_local_slot(code, var_name)
                    && let Some(env_val) = self.env().get(var_name).cloned()
                {
                    self.locals[slot] = env_val;
                }
                // Sync OS environment when %*ENV is modified
                #[cfg(not(target_family = "wasm"))]
                if var_name == "%*ENV" {
                    // SAFETY: mutsu is single-threaded
                    unsafe {
                        std::env::set_var(&key, val.to_string_value());
                    }
                    // Sync $*HOME when %*ENV<HOME> changes
                    if key == "HOME" {
                        let home_str = val.to_string_value();
                        let home_val = self.make_io_path_instance(&home_str);
                        self.env_mut()
                            .insert("$*HOME".to_string(), home_val.clone());
                        self.env_mut().insert("*HOME".to_string(), home_val);
                    }
                }
                self.stack.push(val);
                Some(Ok(()))
            }
            None => {
                // Hash doesn't exist yet — auto-vivify and insert
                let idx = self.stack.pop().unwrap();
                let val = self.stack.pop().unwrap();
                let key = idx.to_string_value();
                let mut map = std::collections::HashMap::new();
                map.insert(key.clone(), val.clone());
                self.env_mut()
                    .insert(var_name.to_string(), Value::hash(map));
                // Sync OS environment when %*ENV is modified
                #[cfg(not(target_family = "wasm"))]
                if var_name == "%*ENV" {
                    // SAFETY: mutsu is single-threaded
                    unsafe {
                        std::env::set_var(&key, val.to_string_value());
                    }
                    if key == "HOME" {
                        let home_str = val.to_string_value();
                        let home_val = self.make_io_path_instance(&home_str);
                        self.env_mut()
                            .insert("$*HOME".to_string(), home_val.clone());
                        self.env_mut().insert("*HOME".to_string(), home_val);
                    }
                }
                self.stack.push(val);
                Some(Ok(()))
            }
            _ => None, // Not a Hash — fall through to slow path
        }
    }

    pub(super) fn exec_index_assign_expr_named_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
    ) -> Result<(), RuntimeError> {
        // A lazy `@`-array must reify its prefix before an element assignment
        // (`@a[i] = v`) — the assign machinery needs a materialized backing. (L2)
        self.reify_lazy_array_slot(Self::const_str(code, name_idx))?;
        // Slice 2b: `@aoa[i] = @row` / `%h<k> = @row` was compiled as a `:=` bind
        // (so the bind machinery installs a shared `ContainerRef` cell and
        // promotes the source) plus a `MarkElementShare` flag. Capture which
        // element to mark as a `=` value share — a simple Int/Str subscript — so
        // a later non-share reassignment REPLACES the slot instead of writing
        // through the shared cell. Complex subscripts keep pure `:=` semantics.
        let elem_share_mark: Option<(String, String)> = if self.element_share_pending {
            self.element_share_pending = false;
            let var_name = Self::const_str(code, name_idx).to_string();
            self.stack.last().and_then(|idx| match idx {
                Value::Int(n) if *n >= 0 => Some((var_name, idx.to_string_value())),
                Value::Str(_) => Some((var_name, idx.to_string_value())),
                _ => None,
            })
        } else {
            None
        };
        // --- Track C: shared hash/array element assignment across threads ---
        // `%h{$k} = $v` / `@a[$i] = $v` inside a `start` block must write through
        // the shared cell so concurrent threads all land (snapshot semantics
        // otherwise lose updates).
        if let Some(result) = self.try_shared_hash_element_assign(code, name_idx) {
            return result;
        }
        if let Some(result) = self.try_shared_array_element_assign(code, name_idx) {
            return result;
        }
        // --- Fast path for simple hash element assignment ---
        // Handles the common case: %h{$key} = $val with no type constraints,
        // no binding, no special containers. Skips ~16 HashMap lookups.
        if let Some(result) = self.try_fast_hash_element_assign(code, name_idx, is_positional) {
            return result;
        }
        // Save type metadata and container default by pointer BEFORE the
        // inner op runs. Auto-vivification and Arc::make_mut may
        // reconstruct the array Arc, changing the pointer used as the
        // metadata key. Reapply them on the final container so typed-array
        // hole semantics and `is default(...)` are preserved.
        let save_var_name = Self::const_str(code, name_idx).to_string();
        // Hash type metadata (including the object-hash key constraint) is now
        // embedded in `HashData` and travels with the hash across copy-on-write,
        // so the old name-based reconcile healing is no longer needed.
        let saved_type_meta_outer = self
            .env()
            .get(&save_var_name)
            .cloned()
            .and_then(|v| self.container_type_metadata(&v));
        // Guard against stale pointer-keyed defaults (Arc reuse across
        // allocations): only trust the saved default when a name-based
        // var_default is also registered.
        let saved_default_outer = if self.var_default(&save_var_name).is_some() {
            self.env()
                .get(&save_var_name)
                .and_then(|v| self.container_default(v).cloned())
        } else {
            None
        };
        let result = self.exec_index_assign_expr_named_op_inner(code, name_idx, is_positional);
        // Restore metadata on the post-assignment container when the
        // identity-keyed map lost it OR holds a stale entry. Copy-on-write
        // changes the hash's Arc pointer (the metadata key), and freed pointers
        // get reused by later allocations carrying *different* stale metadata,
        // so a mere `.is_none()` check leaves a reused pointer's wrong entry in
        // place — re-register whenever the current entry differs from the value
        // saved before the assignment. Object-hash element reads (`%h{$int}`)
        // detect their key constraint only through this pointer-keyed metadata
        // (the read op has no variable name to fall back on), so a stale/lost
        // entry silently degrades them to string-keyed lookups returning Nil.
        if let Some(info) = saved_type_meta_outer
            && let Some(container) = self.env().get(&save_var_name).cloned()
            && self.container_type_metadata(&container).as_ref() != Some(&info)
        {
            // Hashes embed metadata in `HashData`, so the re-tagged value must
            // be written back into both env and the fast-path local slot
            // (`tag_container_metadata` returns the same Arc for non-hash
            // containers, whose Arc-pointer side table is updated in place).
            let tagged = self.tag_container_metadata(container, info);
            self.env_mut().insert(save_var_name.clone(), tagged.clone());
            self.locals_set_by_name(code, &save_var_name, tagged);
        }
        if let Some(def) = saved_default_outer
            && let Some(container) = self.env().get(&save_var_name).cloned()
            && self.container_default(&container).is_none()
        {
            let tagged = self.tag_container_default(container, def);
            self.env_mut().insert(save_var_name.clone(), tagged.clone());
            self.locals_set_by_name(code, &save_var_name, tagged);
        }
        // Object-hash original keys are embedded in `HashData` and travel with
        // the hash across copy-on-write, so no pointer migration is needed.
        // Slice 2b: now that the shared cell is installed in the element, record
        // it as a `=` value share so a later non-share reassignment replaces it.
        if result.is_ok()
            && let Some((var_name, encoded)) = elem_share_mark
        {
            self.mark_element_share(&var_name, encoded);
        }
        // Object index-assign (`$obj[i] = v` / `$obj{k} = v` dispatching
        // ASSIGN-POS/ASSIGN-KEY to an Instance or Mixin that does Positional/
        // Associative) writes the mutated object back into `env[var]` but the
        // inner op does not refresh the caller's local slot. The default build's
        // blanket env reconcile carries this; make it a precise slot write-through
        // so the `MUTSU_NO_BLANKET_RECONCILE` single-store path (and the eventual
        // `env_dirty` removal) keeps the slot coherent. Plain Array/Hash element
        // assigns already update the slot via the fast paths and never reach here
        // as an Instance/Mixin, so this only fires for object subscript targets.
        if result.is_ok()
            && matches!(
                self.env().get(&save_var_name),
                Some(Value::Instance { .. }) | Some(Value::Mixin(..))
            )
            && let Some(v) = self.env().get(&save_var_name).cloned()
        {
            self.locals_set_by_name(code, &save_var_name, v);
        }
        result
    }

    fn exec_index_assign_expr_named_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        is_positional: bool,
    ) -> Result<(), RuntimeError> {
        let original_var_name = Self::const_str(code, name_idx).to_string();
        // Resolve sigilless alias: if `h` is a sigilless alias for `%a`,
        // operate on `%a` directly so in-place mutations are visible.
        let sigilless_alias_target = {
            let alias_key = format!("__mutsu_sigilless_alias::{}", original_var_name);
            self.env().get(&alias_key).and_then(|v| {
                if let Value::Str(target) = v {
                    Some(target.to_string())
                } else {
                    None
                }
            })
        };
        let var_name = sigilless_alias_target
            .as_deref()
            .unwrap_or(&original_var_name)
            .to_string();
        // Pre-compute fill value for native typed arrays (e.g. int->0, num->0e0, str->"")
        // Must be done before mutable borrows to avoid borrow conflicts.
        let native_fill = {
            let tc = loan_env!(self, var_type_constraint(&var_name));
            Self::native_fill_for_constraint(tc.as_deref())
        };
        // Capture the old Arc pointer before any mutation so the post-mutation
        // sync code can distinguish stale COW copies from unrelated containers.
        let old_container_arc_ptr: Option<usize> =
            if let Some(container) = self.env().get(&var_name) {
                match container {
                    Value::Array(arc, _) => Some(Arc::as_ptr(arc) as usize),
                    Value::Hash(arc) => Some(Arc::as_ptr(arc) as usize),
                    _ => None,
                }
            } else {
                None
            };
        let declared_type = self
            .env()
            .get(&var_name)
            .cloned()
            .and_then(|v| self.container_type_metadata(&v))
            .and_then(|info| info.declared_type);
        let _target_is_mixhash = declared_type.as_deref().is_some_and(|t| t == "MixHash");
        let _target_is_baghash = declared_type.as_deref().is_some_and(|t| t == "BagHash");
        let _target_is_sethash = declared_type.as_deref().is_some_and(|t| t == "SetHash");
        let declared_shape_key = format!("__mutsu_shaped_array_dims::{var_name}");
        let has_declared_shape = self.env().contains_key(&declared_shape_key);
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let index_target = self.env().get(&var_name).cloned();
        let idx = self.resolve_whatever_index_for_target(idx, index_target.as_ref());
        // Normalize Seq/Slip/Range index to Array for uniform handling in assignment.
        // For hash variables, expand Range indices into individual keys so that
        // `%h{^5} = (0 xx 5)` performs a hash slice assignment (5 separate keys)
        // instead of treating the stringified range as a single key.
        // Native typed arrays (e.g. `array[num]`) need numeric Range slice
        // indices expanded to an explicit index list so the slice-assign path
        // distributes each value to a single element (rather than checking the
        // whole RHS list against the scalar element type).
        let vtc_native = loan_env!(self, var_type_constraint(&var_name))
            .as_deref()
            .is_some_and(crate::runtime::native_types::is_native_array_element_type);
        let ct_native = index_target
            .as_ref()
            .cloned()
            .and_then(|v| self.container_type_metadata(&v))
            .is_some_and(|info| {
                crate::runtime::native_types::is_native_array_element_type(&info.value_type)
            });
        let array_var_is_native = !var_name.starts_with('%')
            && matches!(index_target, Some(Value::Array(..)))
            && (vtc_native || ct_native);
        let expand_range = var_name.starts_with('%') || array_var_is_native;
        let idx = match idx {
            Value::Seq(items) => Value::Array(
                crate::value::Value::array_arc(items.to_vec()),
                crate::value::ArrayKind::List,
            ),
            Value::Slip(items) => Value::Array(
                crate::value::Value::array_arc(items.to_vec()),
                crate::value::ArrayKind::List,
            ),
            Value::Range(a, b) if expand_range => {
                let items: Vec<Value> = (a..=b).map(Value::Int).collect();
                Value::Array(
                    Arc::new(crate::value::ArrayData::new(items)),
                    crate::value::ArrayKind::List,
                )
            }
            Value::RangeExcl(a, b) if expand_range => {
                let items: Vec<Value> = (a..b).map(Value::Int).collect();
                Value::Array(
                    Arc::new(crate::value::ArrayData::new(items)),
                    crate::value::ArrayKind::List,
                )
            }
            Value::RangeExclStart(a, b) if expand_range => {
                let items: Vec<Value> = ((a + 1)..=b).map(Value::Int).collect();
                Value::Array(
                    Arc::new(crate::value::ArrayData::new(items)),
                    crate::value::ArrayKind::List,
                )
            }
            Value::RangeExclBoth(a, b) if expand_range => {
                let items: Vec<Value> = ((a + 1)..b).map(Value::Int).collect();
                Value::Array(
                    Arc::new(crate::value::ArrayData::new(items)),
                    crate::value::ArrayKind::List,
                )
            }
            other => other,
        };
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let (val, bind_mode, bind_sources) = match raw_val {
            Value::Pair(name, payload) if name == "__mutsu_bind_index_value" => match *payload {
                Value::Array(items, ..) if items.len() >= 2 => {
                    let value = items.first().cloned().unwrap_or(Value::Nil);
                    let sources = match items.get(1) {
                        Some(Value::Array(srcs, ..)) => srcs
                            .iter()
                            .map(|src| match src {
                                Value::Str(s) if !s.is_empty() => Some((**s).clone()),
                                _ => None,
                            })
                            .collect(),
                        _ => Vec::new(),
                    };
                    (value, true, sources)
                }
                other => (other, true, Vec::new()),
            },
            other => (other, false, Vec::new()),
        };
        // For typed container elements, explicit `is default(...)` wins over
        // the nominal type-object fallback when Nil is assigned.
        let val = if matches!(val, Value::Nil) {
            if let Some(default) = self.var_default(&var_name) {
                default.clone()
            } else if let Some(constraint) = loan_env!(self, var_type_constraint(&var_name)) {
                let nominal = loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                Value::Package(Symbol::intern(&nominal))
            } else {
                val
            }
        } else {
            val
        };
        // Map containers are immutable - prevent assignment and binding to keys
        if declared_type.as_deref().is_some_and(|t| t == "Map") {
            if bind_mode {
                return Err(RuntimeError::typed(
                    "X::Bind",
                    [("target".to_string(), Value::str(var_name.clone()))]
                        .into_iter()
                        .collect(),
                ));
            } else {
                return Err(RuntimeError::assignment_ro_typename("Map", "Map"));
            }
        }
        // Mix/Set/Bag: prevent auto-vivification of undefined typed variables.
        // When `my Mix $m; $m<key> = val`, the variable is undefined (Nil or
        // type object) but has an immutable type constraint.
        {
            let current_val = self.env().get(&var_name).cloned();
            let is_undefined = current_val.is_none()
                || matches!(&current_val, Some(Value::Nil))
                || matches!(&current_val, Some(Value::Package(_)));
            if is_undefined {
                let constraint_owned = loan_env!(self, var_type_constraint(&var_name));
                let type_name_check = declared_type.as_deref().or(constraint_owned.as_deref());
                if type_name_check.is_some_and(|t| matches!(t, "Mix" | "Set" | "Bag")) {
                    let type_name = type_name_check.unwrap_or("Mix");
                    return Err(RuntimeError::new(format!(
                        "Cannot auto-vivify an immutable {}",
                        type_name
                    )));
                }
            }
        }
        // Immutable List/Range containers - prevent assignment and binding
        if let Some(target_val) = self.env().get(&var_name) {
            let is_immutable = matches!(
                target_val,
                Value::Array(_, crate::value::ArrayKind::List)
                    | Value::Array(_, crate::value::ArrayKind::ItemList)
                    | Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. }
            );
            if is_immutable {
                if bind_mode {
                    return Err(RuntimeError::typed(
                        "X::Bind",
                        [("target".to_string(), Value::str(var_name.clone()))]
                            .into_iter()
                            .collect(),
                    ));
                }
                // For List containers: allow assignment through a Scalar element.
                // `my $l := List.new: 1, 2, my $ = 3` stores a Value::Scalar at
                // position 2; `$l[2] = 42` should update that Scalar in-place.
                let list_scalar_hit = if let Value::Array(items, kind) = target_val
                    && (kind == &crate::value::ArrayKind::List
                        || kind == &crate::value::ArrayKind::ItemList)
                    && let Some(i) = Self::index_to_usize(&idx)
                    && matches!(items.get(i), Some(Value::Scalar(_)))
                {
                    Some((items.clone(), i))
                } else {
                    None
                };
                if let Some((items, i)) = list_scalar_hit {
                    // Update the Scalar element in-place.
                    // SAFETY: aliased in-place mutation of a shared list backing;
                    // see `arc_contents_mut`. No borrow into the items is live
                    // across the write. (The old code cast the `ArrayData` pointer
                    // straight to `*mut Vec<Value>`, assuming `items` sits at
                    // offset 0; this types it properly as `&mut ArrayData`.)
                    let data = unsafe { crate::value::arc_contents_mut(&items) };
                    data.items[i] = Value::Scalar(Box::new(val.clone()));
                    self.stack.push(val);
                    return Ok(());
                }
                let type_name = match target_val {
                    Value::Array(..) => "List",
                    _ => "Range",
                };
                let display = target_val.to_string_value();
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Cannot modify an immutable {} ({})",
                        type_name, display
                    )),
                );
                attrs.insert("value".to_string(), target_val.clone());
                return Err(RuntimeError::typed("X::Assignment::RO", attrs));
            }
        }
        let encoded_idx = Self::encode_bound_index(&idx);
        // Slice 2b: is the existing element a `=` value share (`@aoa[i] = @row`)?
        // A non-bind reassignment to such an element REPLACES the shared cell
        // (raku value semantics) instead of writing through it. Precomputed here
        // because the element-write chokepoints below hold a `&mut` borrow of the
        // container and cannot call `self`; the marker is cleared after the store.
        let elem_is_value_share =
            !bind_mode && self.array_share_active && self.is_element_share(&var_name, &encoded_idx);
        // Native typed arrays store unboxed scalars and cannot bind containers to
        // their elements: `my num @a; @a[0] := $x` is illegal.
        if bind_mode
            && var_name.starts_with('@')
            && let Some(constraint) = loan_env!(self, var_type_constraint(&var_name))
            && crate::runtime::native_types::is_native_array_element_type(&constraint)
        {
            return Err(RuntimeError::new(format!(
                "Cannot bind to a native {} array",
                crate::runtime::native_types::native_family_name(&constraint)
            )));
        }
        let is_bound_index = if bind_mode {
            self.is_bound_index(&var_name, &encoded_idx)
        } else {
            false
        };
        if !bind_mode && self.is_bound_index(&var_name, &encoded_idx) {
            // `:=`-bound elements are shared `ContainerRef` cells now (no
            // sentinel back-references), so any remaining bound-index metadata
            // for an existing non-cell element is stale (e.g. the binding was
            // broken by splice or an array reset) — clean it up; the write
            // proceeds either way (cell writes go through the cell arm).
            if let Some(Value::Array(items, ..)) = self.env().get(&var_name)
                && let Some(i) = Self::index_to_usize(&idx)
                && matches!(items.get(i), Some(v) if !v.is_container_ref())
            {
                self.remove_bound_index(&var_name, &encoded_idx);
            }
        }
        if !self.env().contains_key(&var_name)
            && let Some(slot) = self.find_local_slot(code, &var_name)
        {
            self.set_env_with_main_alias(&var_name, self.locals[slot].clone());
        }
        // Junction autothreading: when writing back with a junction index,
        // expand the junction and assign each element separately.
        if let Value::Junction {
            values: junc_keys, ..
        } = &idx
        {
            let junc_vals = if let Value::Junction { values: jv, .. } = &val {
                jv.clone()
            } else {
                // Same value for all junction elements
                Arc::new(vec![val.clone(); junc_keys.len()])
            };
            for (i, key) in junc_keys.iter().enumerate() {
                let v = junc_vals.get(i).cloned().unwrap_or(Value::Nil);
                let k = key.to_string_value();
                if var_name.starts_with('%') {
                    // Descend through any `:=`-bound `ContainerRef` cell so the
                    // junction write reaches the shared container (otherwise it
                    // would detach the bind by overwriting the cell with a fresh
                    // hash — see `my %h := %g; %h{'x'|'y'} = v`).
                    if !matches!(self.env_root_descended_mut(&var_name), Some(Value::Hash(_))) {
                        self.env_mut().insert(
                            var_name.clone(),
                            Value::hash(std::collections::HashMap::new()),
                        );
                    }
                    if let Some(Value::Hash(hash)) = self.env_root_descended_mut(&var_name) {
                        let h = Arc::make_mut(hash);
                        h.insert(k, v);
                    }
                } else if let Some(idx_usize) = Self::index_to_usize(key) {
                    // For array variables with junction index, use numeric indices
                    // (descend through any `:=`-bound cell, same as the hash arm).
                    if let Some(Value::Array(items, ..)) = self.env_root_descended_mut(&var_name) {
                        let arr = Arc::make_mut(items);
                        if idx_usize >= arr.len() {
                            let fill = native_fill.clone();
                            arr.resize(idx_usize + 1, fill);
                        }
                        arr[idx_usize] = v;
                    }
                }
            }
            self.stack.push(val);
            return Ok(());
        }
        match &idx {
            Value::Array(keys, ..) => {
                let mut vals = self.assignment_rhs_values(&val)?;
                // Per-element type check for slice assignment to a typed array,
                // e.g. `my Array @x; @x[0,2] = 2, 3` must reject each Int element.
                if var_name.starts_with('@')
                    && let Some(constraint) = loan_env!(self, var_type_constraint(&var_name))
                {
                    for v in &vals {
                        if !matches!(v, Value::Nil) && !self.type_matches_value(&constraint, v) {
                            return Err(runtime::utils::type_check_element_typed_error(
                                &var_name,
                                &constraint,
                                v,
                            ));
                        }
                    }
                }
                // Descend through a whole-container `:=` bound cell (`my @x :=
                // @a`) so an array slice assignment mutates the shared inner
                // Array (every alias observes it) instead of falling through to
                // the hash-slice path below.
                if let Some(container) = self.env_root_descended_mut(&var_name)
                    && matches!(container, Value::Array(..))
                {
                    let is_shaped =
                        has_declared_shape || crate::runtime::utils::is_shaped_array(container);
                    let mut initialized_marks: Vec<String> = Vec::new();
                    if is_shaped {
                        if bind_mode && is_bound_index {
                            return Err(RuntimeError::assignment_ro(None));
                        }
                        let depth = Self::array_depth(container);
                        if depth <= 1 && keys.len() > 1 {
                            // 1D shaped array with multiple indices: slice assignment
                            for (i, key) in keys.iter().enumerate() {
                                let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                                Self::assign_array_multidim(
                                    container,
                                    std::slice::from_ref(key),
                                    v,
                                )?;
                                initialized_marks.push(Self::encode_bound_index(key));
                            }
                        } else {
                            // Multidimensional indexing: @arr[0;0] = 'x'
                            Self::assign_array_multidim(container, keys.as_ref(), val.clone())?;
                            initialized_marks.push(encoded_idx.clone());
                        }
                    } else if keys.is_empty() {
                        // Empty slice assignment (e.g. @n[*] on empty array): no-op
                    } else {
                        // Flat slice assignment: @a[2,3,4,6] = <foo bar foo bar>
                        // Auto-extend the array to accommodate all indices
                        let max_idx = keys
                            .iter()
                            .filter_map(Self::index_to_usize)
                            .max()
                            .unwrap_or(0);
                        if let Value::Array(items, ..) = container {
                            let arr = Arc::make_mut(items);
                            if max_idx >= arr.len() {
                                let fill = native_fill.clone();
                                arr.resize(max_idx + 1, fill);
                            }
                        }
                        // Assign each value to the corresponding index
                        for (i, key) in keys.iter().enumerate() {
                            let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                            Self::assign_array_multidim(container, std::slice::from_ref(key), v)?;
                            initialized_marks.push(Self::encode_bound_index(key));
                        }
                    }
                    for encoded in initialized_marks {
                        self.mark_initialized_index(&var_name, encoded);
                    }
                    if bind_mode {
                        self.mark_bound_index(&var_name, encoded_idx);
                    }
                    self.stack.push(val);
                    return Ok(());
                }
                if vals.is_empty() {
                    vals.push(Value::Nil);
                }
                // Check value type constraint for hash slice assignment
                if let Some(constraint) = loan_env!(self, var_type_constraint(&var_name))
                    && !self.is_container_subclass(&constraint)
                {
                    for v in &vals {
                        if !matches!(v, Value::Nil) && !self.type_matches_value(&constraint, v) {
                            return Err(runtime::utils::type_check_element_typed_error(
                                &var_name,
                                &constraint,
                                v,
                            ));
                        }
                    }
                }
                // Check key type constraint for hash slice assignment
                if let Some(key_constraint) = loan_env!(self, var_hash_key_constraint(&var_name)) {
                    for key in keys.iter() {
                        if !self.type_matches_value(&key_constraint, key) {
                            return Err(runtime::utils::type_check_element_typed_error(
                                &var_name,
                                &key_constraint,
                                key,
                            ));
                        }
                    }
                }
                if !matches!(self.env().get(&var_name), Some(Value::Hash(_))) {
                    self.env_mut().insert(
                        var_name.clone(),
                        Value::hash(std::collections::HashMap::new()),
                    );
                }
                let slice_is_object_hash =
                    loan_env!(self, var_hash_key_constraint(&var_name)).is_some();
                // Phase 2 Stage 2 (hash slice bind): pre-read each bind source
                // before the mutable container borrow, reusing an existing
                // `ContainerRef` cell binding or creating a fresh cell to
                // install back into the source var (the #2914 array pattern).
                let mut slice_bind_cells: Vec<Option<BindSourceCell>> = Vec::new();
                if bind_mode {
                    for (i, _) in keys.iter().enumerate() {
                        let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                        let entry = if let Value::ContainerRef(cell) = &v {
                            // Element source: already promoted to a shared cell.
                            Some((None, cell.clone()))
                        } else if let Some(Some(source_name)) = bind_sources.get(i)
                            && !source_name.contains('\0')
                        {
                            match self.env().get(source_name) {
                                Some(Value::ContainerRef(cell)) => Some((None, cell.clone())),
                                _ => Some((
                                    Some(source_name.clone()),
                                    Arc::new(std::sync::Mutex::new(v)),
                                )),
                            }
                        } else {
                            None
                        };
                        slice_bind_cells.push(entry);
                    }
                }
                let mut pending_source_cells: Vec<(String, Arc<std::sync::Mutex<Value>>)> =
                    Vec::new();
                if let Some(Value::Hash(hash)) = self.env_mut().get_mut(&var_name) {
                    let h = Arc::make_mut(hash);
                    for (i, key) in keys.iter().enumerate() {
                        let k = if slice_is_object_hash {
                            runtime::utils::value_which_key(key)
                        } else {
                            key.to_string_value()
                        };
                        let v = if bind_mode {
                            vals.get(i).cloned().unwrap_or(Value::Nil)
                        } else {
                            vals[i % vals.len()].clone()
                        };
                        if bind_mode
                            && let Some(Some((source_install, cell))) = slice_bind_cells.get(i)
                        {
                            h.insert(k, Value::ContainerRef(cell.clone()));
                            if let Some(source_name) = source_install {
                                pending_source_cells.push((source_name.clone(), cell.clone()));
                            }
                        } else {
                            h.insert(k, v);
                        }
                    }
                    // Store original keys for object hashes (embedded in
                    // HashData, COW-stable) after slice insert.
                    if slice_is_object_hash {
                        let orig = h
                            .original_keys
                            .get_or_insert_with(std::collections::HashMap::new);
                        for key in keys.iter() {
                            let wk = runtime::utils::value_which_key(key);
                            orig.insert(wk, key.clone());
                        }
                    }
                }
                for (source_name, cell) in pending_source_cells {
                    // Bind the source variable to the same cell installed at
                    // the slice entry, so both sides alias one container.
                    let cell_val = Value::ContainerRef(cell);
                    self.set_env_with_main_alias(&source_name, cell_val.clone());
                    self.update_local_if_exists(code, &source_name, &cell_val);
                }
            }
            _ => {
                // For object hashes, use .WHICH as the internal key.
                let key_constraint = if var_name.starts_with('%') {
                    loan_env!(self, var_hash_key_constraint(&var_name))
                } else {
                    None
                };
                let is_object_hash = key_constraint.is_some();
                // A coercion key type (`my %h{Int(Str)}`) coerces the key to the
                // target type before it is stored / `.WHICH`-keyed. A failed
                // coercion (e.g. a non-numeric string into Int) throws, matching
                // raku (`X::Str::Numeric`).
                let idx = if let Some(kc) = &key_constraint
                    && let Some(open) = kc.find('(')
                    && kc.ends_with(')')
                    && open > 0
                    && !self.type_matches_value(&kc[..open], &idx)
                {
                    // A numeric coercion target rejects a non-numeric string the
                    // raku way (`X::Str::Numeric`) rather than silently coercing
                    // it to 0, since the lenient `coerce_value` fast path does not
                    // throw.
                    if matches!(
                        &kc[..open],
                        "Int" | "UInt" | "Num" | "Rat" | "FatRat" | "Real" | "Numeric" | "Complex"
                    ) {
                        runtime::utils::check_str_numeric(&idx)?;
                    }
                    loan_env!(self, try_coerce_value_for_constraint(kc, idx.clone()))?
                } else {
                    idx
                };
                // Determine the keying mode for an object hash. A freshly created
                // or already-`.WHICH`-keyed hash (one carrying `original_keys`) is
                // keyed by `.WHICH`. A *plain-built* object hash — e.g. one bulk
                // assigned from a pair list (`my %h{Mu} = :42a, ...`), whose keys
                // were stringified during hash construction and which carries no
                // `original_keys` — must keep using plain string keys here so that
                // a follow-up element assignment (`%h<c> = ...`) overwrites the
                // existing entry instead of inserting a duplicate under a `.WHICH`
                // key. (Fully unifying object-hash keying across construction,
                // flatten, gist and comparison is a larger change.)
                let use_which = is_object_hash
                    && match &index_target {
                        Some(Value::Hash(h)) => h.original_keys.is_some() || h.map.is_empty(),
                        _ => true,
                    };
                let key = if use_which {
                    runtime::utils::value_which_key(&idx)
                } else {
                    idx.to_string_value()
                };
                let array_elem_constraint = loan_env!(self, var_type_constraint(&var_name));
                // A `%h is BagHash`/`MixHash`/`SetHash` (or `Bag`/`Mix`/`Set`)
                // variable IS that QuantHash: the constraint names the *whole*
                // container, not the element type, and `%h<k> = weight` sets a
                // weight (validated/handled by the QuantHash weight-assign path
                // below), so it must not be element-type-checked against the
                // container type.
                let target_is_quanthash = matches!(
                    &index_target,
                    Some(Value::Mix(..) | Value::Bag(..) | Value::Set(..))
                );
                // A parameterized QuantHash (`BagHash[Int]`, `MixHash[Str]`, ...)
                // constrains its *keys*: `%bh<foo> = 42` on a `BagHash[Int]` must
                // throw because the key "foo" is not an Int. The bracketed key type
                // is carried in the container metadata's `value_type` (e.g.
                // "BagHash[Int]") for Bag/Mix/Set, falling back to `declared_type`.
                if target_is_quanthash
                    && let Some(meta) = index_target
                        .as_ref()
                        .and_then(|c| self.container_type_metadata(c))
                    && let Some(declared) = meta
                        .declared_type
                        .as_deref()
                        .filter(|d| d.contains('['))
                        .or(Some(meta.value_type.as_str()))
                    && let Some(open) = declared.find('[')
                    && declared.ends_with(']')
                {
                    let key_type = &declared[open + 1..declared.len() - 1];
                    // A numeric-looking string key (`%bh<7>`) is an allomorph
                    // (IntStr in Raku) that satisfies a numeric key type, so only a
                    // string that does NOT coerce to the numeric type (`%bh<foo>`)
                    // is rejected.
                    let allomorph_ok = matches!(&idx, Value::Str(s)
                        if crate::runtime::str_numeric::parse_raku_str_to_numeric(s).is_some())
                        && matches!(
                            key_type,
                            "Int" | "UInt" | "Num" | "Rat" | "Real" | "Numeric" | "Cool"
                        );
                    if !matches!(key_type, "" | "Any" | "Mu" | "Str")
                        && !allomorph_ok
                        && !self.type_matches_value(key_type, &idx)
                    {
                        return Err(runtime::utils::type_check_binding_typed_error(
                            key_type, &idx,
                        ));
                    }
                }
                if let Some(constraint) = array_elem_constraint
                    && !target_is_quanthash
                    && !matches!(val, Value::Nil)
                    && !self.type_matches_value(&constraint, &val)
                    // For `$`-sigil variables holding a container (e.g. a `Hash $h`
                    // parameter), a container type constraint describes the whole
                    // container, not its elements, so element assignment like
                    // `$h<k> = v` must not be checked against it. For `@`/`%`
                    // variables the constraint IS the element/value type and must
                    // be enforced (e.g. `my Array @x; @x[0] = 1` is a type error).
                    && (var_name.starts_with('@')
                        || var_name.starts_with('%')
                        || !(matches!(
                            constraint.as_str(),
                            "Hash" | "Array" | "Map" | "List" | "Bag" | "Set" | "Mix"
                                | "BagHash" | "SetHash" | "MixHash" | "Seq"
                        ) || self.is_container_subclass(&constraint)))
                {
                    return Err(runtime::utils::type_check_element_typed_error(
                        &var_name,
                        &constraint,
                        &val,
                    ));
                }
                // Check key type constraint for single-key hash element assignment
                if var_name.starts_with('%')
                    && let Some(key_constraint) =
                        loan_env!(self, var_hash_key_constraint(&var_name))
                    && !self.type_matches_value(&key_constraint, &idx)
                {
                    return Err(runtime::utils::type_check_element_typed_error(
                        &var_name,
                        &key_constraint,
                        &idx,
                    ));
                }
                // Native integer arrays store the wrapped value (`-1` -> `255` in a
                // uint8 array); the assignment expression still yields the original.
                let native_store_val = self.wrap_native_int_for_var(&var_name, val.clone());
                // Resolve GenericRange with WhateverCode endpoints (e.g. @a[*-4 .. *-1] = ...)
                let resolved_idx;
                let idx_for_slice = if let Value::GenericRange { .. } = &idx {
                    let array_len = if let Some(Value::Array(items, ..)) = self.env().get(&var_name)
                    {
                        items.len()
                    } else {
                        0
                    };
                    resolved_idx = self.resolve_generic_range_for_assign(&idx, array_len);
                    resolved_idx.as_ref().unwrap_or(&idx)
                } else {
                    &idx
                };
                let range_slice =
                    if let Some(indices) = Self::slice_indices_from_index(idx_for_slice) {
                        Some((indices, self.assignment_rhs_values(&val)?))
                    } else {
                        None
                    };
                // Per-element type check for slice assignment to a typed array,
                // e.g. `my Array @x; @x[0,2] = 2, 3` must reject each Int element.
                if let Some((_, ref rhs_values)) = range_slice
                    && (var_name.starts_with('@') || var_name.starts_with('%'))
                    && let Some(constraint) = loan_env!(self, var_type_constraint(&var_name))
                {
                    for v in rhs_values {
                        if !matches!(v, Value::Nil) && !self.type_matches_value(&constraint, v) {
                            return Err(runtime::utils::type_check_element_typed_error(
                                &var_name,
                                &constraint,
                                v,
                            ));
                        }
                    }
                }
                if let Some(current) = self.env().get(&var_name).cloned()
                    && let Value::Mixin(inner, mixins) = current
                {
                    let mut updated_mixins = (*mixins).clone();
                    let mut assigned_object_slot = false;
                    let delegated_attr_key = if matches!(&idx, Value::Str(_)) {
                        self.delegated_mixin_attr_key(&updated_mixins, "ASSIGN-KEY")
                    } else {
                        self.delegated_mixin_attr_key(&updated_mixins, "ASSIGN-POS")
                    };
                    if let Some(attr_key) = delegated_attr_key
                        && let Some(attr_value) = updated_mixins.get_mut(&attr_key)
                    {
                        assigned_object_slot =
                            Self::assign_mixin_container_slot(attr_value, &idx, &val, &range_slice);
                    }
                    if !assigned_object_slot {
                        for (key, attr_value) in updated_mixins.iter_mut() {
                            if !key.starts_with("__mutsu_attr__") {
                                continue;
                            }
                            if Self::assign_mixin_container_slot(
                                attr_value,
                                &idx,
                                &val,
                                &range_slice,
                            ) {
                                assigned_object_slot = true;
                                break;
                            }
                        }
                    }
                    if assigned_object_slot {
                        self.env_mut().insert(
                            var_name.clone(),
                            Value::Mixin(inner, Arc::new(updated_mixins)),
                        );
                        self.stack.push(val);
                        return Ok(());
                    }
                }
                let mut range_initialized_marks: Vec<String> = Vec::new();
                let mut pending_varref_update: Option<(String, Option<usize>, Value)> = None;
                // Phase 2 Stage 2: a single-level element bind (`@a[i] := ...`,
                // `%h<k> := ...`) stores a shared `ContainerRef` cell. The cell
                // is written back to the source var after the write completes.
                let mut pending_source_cell: Option<(String, Arc<std::sync::Mutex<Value>>)> = None;
                // Whether the bind stored a shared cell at the element (skips
                // the bound-index side table — the cell IS the alias).
                let mut stored_bind_cell = false;
                // Pre-read the bind source before the mutable container borrow.
                // An element source (`:= @b[j]`) arrives already promoted to a
                // shared cell by IndexAutovivifyLazyTerminal; a source variable
                // that is already cell-bound (e.g. `@arr[0] := $x; %h<k> := $x`)
                // must REUSE its existing cell so all aliases stay shared; a
                // plain scalar source gets a fresh cell installed back into the
                // source var after the write. `None` source-install means the
                // cell is already in place.
                let bind_cell: Option<BindSourceCell> = if bind_mode {
                    if let Value::ContainerRef(cell) = &val {
                        Some((None, cell.clone()))
                    } else if let Some(Some(source_name)) = bind_sources.first()
                        && !source_name.contains('\0')
                    {
                        match self.env().get(source_name) {
                            Some(Value::ContainerRef(cell)) => Some((None, cell.clone())),
                            _ => Some((
                                Some(source_name.clone()),
                                Arc::new(std::sync::Mutex::new(val.clone())),
                            )),
                        }
                    } else {
                        None
                    }
                } else {
                    None
                };
                // Pre-compute whether this %-sigiled variable was bound via `:=`.
                // Bound hash variables are marked readonly, so we use that as a
                // signal to allow in-place mutation (preserving shared identity).
                // A `:=` bind also records a dedicated `__mutsu_bound::%name`
                // marker (see `Stmt::MarkBoundContainer`), which distinguishes a
                // writable bound hash from a `constant %M` — both are readonly,
                // but only the former may be mutated in place.
                let is_readonly_hash_var =
                    var_name.starts_with('%') && self.readonly_vars().contains(&var_name);
                let is_bound_hash_var = is_readonly_hash_var
                    && self
                        .env()
                        .contains_key(&format!("__mutsu_bound::{}", var_name));
                // A readonly `%`-var that was NOT `:=`-bound is a `constant %M`
                // (an immutable Map): element assignment must die with
                // X::Assignment::RO, mirroring raku and mutsu's own `constant @A`
                // behavior. Only fire for plain `Value::Hash`; immutable
                // Set/Bag/Mix containers keep their dedicated RO paths below.
                if is_readonly_hash_var
                    && !is_bound_hash_var
                    && matches!(self.env().get(&var_name), Some(Value::Hash(_)))
                {
                    let elem = match self.env().get(&var_name) {
                        Some(Value::Hash(hd)) => hd.map.get(&key).cloned(),
                        _ => None,
                    };
                    return Err(match elem {
                        Some(v) => {
                            let tn = crate::runtime::utils::value_type_name(&v);
                            RuntimeError::assignment_ro_typename(tn, &v.to_string_value())
                        }
                        None => RuntimeError::assignment_ro(None),
                    });
                }
                // Type check for parameterized SetHash[T]/BagHash[T]/MixHash[T]
                // element binding. Only applies when the declared type is explicitly
                // parameterized (e.g. SetHash[Str]), not when the constraint is just
                // `is SetHash`. The subscript key is the element, so it must satisfy
                // the parameterized element (keyof) type.
                let set_val_clone = self
                    .env()
                    .get(&var_name)
                    .filter(|v| matches!(v, Value::Set(..) | Value::Bag(..) | Value::Mix(..)))
                    .cloned();
                // The subscript key is checked against the element (keyof) type.
                // For Set/Bag that equals `value_type`; for Mix the keyof lives in
                // `key_type` (while `value_type` is the weight type, Real).
                let elem_type = set_val_clone.as_ref().and_then(|sv| {
                    self.container_type_metadata(sv).and_then(|info| {
                        if !info
                            .declared_type
                            .as_deref()
                            .is_some_and(|t| t.contains('['))
                        {
                            return None;
                        }
                        info.key_type
                            .filter(|t| !t.is_empty())
                            .or(Some(info.value_type))
                            .filter(|t| !t.is_empty() && t != "Any" && t != "Mu")
                    })
                });
                if let Some(elem_type) = elem_type
                    && !self.type_matches_value(&elem_type, &idx)
                {
                    let got_type = crate::value::what_type_name(&idx);
                    let got_repr = idx.to_string_value();
                    let msg = format!(
                        "Type check failed in binding; expected {} but got {} (\"{}\")",
                        elem_type, got_type, got_repr,
                    );
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(msg.clone()));
                    attrs.insert("operation".to_string(), Value::str_from("bind"));
                    attrs.insert("got".to_string(), idx.clone());
                    attrs.insert(
                        "expected".to_string(),
                        Value::Package(crate::symbol::Symbol::intern(&elem_type)),
                    );
                    let ex = Value::make_instance(
                        crate::symbol::Symbol::intern("X::TypeCheck::Binding"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(msg);
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                // When the container is an Instance of a Hash subclass (e.g.
                // `class MyHash is Hash {}`), convert it to a plain Hash
                // before element assignment so hash operations work correctly.
                // But if it inherits from an immutable type (Set, Bag, Mix),
                // throw X::Assignment::RO instead.
                if let Some(Value::Instance {
                    class_name,
                    attributes,
                    ..
                }) = self.env().get(&var_name)
                    && self.is_container_subclass(&class_name.resolve())
                {
                    let cn = class_name.resolve();
                    if self.class_inherits_from_immutable_setty(&cn) {
                        let display = format!("{}()", cn);
                        return Err(RuntimeError::assignment_ro_typename(&cn, &display));
                    }
                    let hash_map: HashMap<String, Value> = HashMap::clone(&attributes.as_map());
                    let hash_val = Value::hash(hash_map);
                    self.env_mut().insert(var_name.clone(), hash_val);
                }
                // Autovivify Nil-valued typed containers (MixHash, BagHash, SetHash)
                // before the main container dispatch so they are handled correctly.
                {
                    let constraint = loan_env!(self, var_type_constraint(&var_name));
                    let effective_type = declared_type.as_deref().or(constraint.as_deref());
                    if let Some(type_name) = effective_type
                        && matches!(type_name, "MixHash" | "BagHash" | "SetHash")
                        && matches!(
                            self.env().get(&var_name),
                            Some(Value::Nil) | Some(Value::Package(_)) | None
                        )
                    {
                        let new_container = match type_name {
                            "MixHash" => {
                                let mut weights = HashMap::new();
                                let weight = Self::mix_assignment_weight(&val)?;
                                if weight != 0.0 {
                                    weights.insert(key.clone(), weight);
                                }
                                Value::mix_hash(weights)
                            }
                            "BagHash" => {
                                let mut counts = HashMap::new();
                                if let Value::Int(n) = &val
                                    && *n > 0
                                {
                                    counts.insert(key.clone(), *n);
                                }
                                Value::bag_hash(counts)
                            }
                            "SetHash" => {
                                let mut items = std::collections::HashSet::new();
                                if val.truthy() {
                                    items.insert(key.clone());
                                }
                                Value::Set(Arc::new(crate::value::SetData::new(items)), true)
                            }
                            _ => unreachable!(),
                        };
                        self.env_mut().insert(var_name.clone(), new_container);
                        self.stack.push(val);
                        // Update local slot
                        if let Some(new_val) = self.env().get(&var_name).cloned() {
                            self.update_local_if_exists(code, &var_name, &new_val);
                        }
                        return Ok(());
                    }
                }
                if let Some(container) = self.env_root_descended_mut(&var_name) {
                    match *container {
                        Value::Hash(ref mut hash) => {
                            let is_self_hash_ref = matches!(
                                &val,
                                Value::Hash(source_hash) if Arc::ptr_eq(hash, source_hash)
                            );
                            // Use in-place mutation instead of Arc::make_mut when the
                            // hash is shared (strong_count > 1) AND the variable is a
                            // scalar ($) container.  This preserves Raku's object
                            // identity semantics: when a hash is stored in an array
                            // slot via `$arr[i] = $hash`, mutations through the original
                            // variable must be visible through the array.
                            // SAFETY: mutsu is single-threaded, so exclusive access is
                            // guaranteed even though the Arc is shared.
                            // For %-sigiled hash variables (e.g. `%h is copy`),
                            // normally use COW.  For scalar ($) variables holding
                            // hashes, use in-place mutation to preserve identity.
                            // Exception: when a %-sigiled variable is bound via
                            // `:=` (marked readonly), use in-place mutation so
                            // modifications propagate to the bound source.
                            // %-sigiled vars have names like "%h", scalar vars
                            // have names without a sigil prefix (e.g. "bar").
                            let use_inplace = Arc::strong_count(hash) > 1
                                && (!var_name.starts_with('%') || is_bound_hash_var);
                            // Mutate the whole `HashData` (map + embedded
                            // object-hash original keys) so the original-key map
                            // travels with the hash through copy-on-write.
                            let hd: &mut crate::value::HashData = if use_inplace {
                                // SAFETY: aliased in-place mutation of a shared
                                // hash; see `arc_contents_mut`.
                                unsafe { crate::value::arc_contents_mut(hash) }
                            } else {
                                Arc::make_mut(hash)
                            };
                            if bind_mode && let Some((source_install, cell)) = &bind_cell {
                                // Phase 2 Stage 2: a `:=`-bound entry holds a
                                // shared `ContainerRef` cell (no more
                                // BOUND_HASH_REF_SENTINEL back-references).
                                // Reads decont at `resolve_hash_entry`; writes
                                // go through `hash_insert_through`.
                                hd.map
                                    .insert(key.clone(), Value::ContainerRef(cell.clone()));
                                if let Some(source_name) = source_install {
                                    pending_source_cell = Some((source_name.clone(), cell.clone()));
                                }
                            } else if is_self_hash_ref {
                                hd.map.insert(key.clone(), Self::self_hash_ref_marker());
                            } else if elem_is_value_share {
                                // Slice 2b: replace the `=`-shared cell rather than
                                // write through it, so the source stays unaffected.
                                hd.map.insert(key.clone(), val.clone());
                            } else {
                                Value::hash_insert_through(&mut hd.map, key.clone(), val.clone());
                            }
                            // For object hashes, store the original key object in
                            // the embedded `original_keys` map (COW-stable). Skip
                            // for a plain-keyed object hash (see `use_which`) so it
                            // is not flipped into `.WHICH` keying mid-stream.
                            if use_which {
                                hd.original_keys
                                    .get_or_insert_with(std::collections::HashMap::new)
                                    .insert(key.clone(), idx.clone());
                            }
                        }
                        Value::Array(..) => {
                            if has_declared_shape
                                || crate::runtime::utils::is_shaped_array(container)
                            {
                                if bind_mode && is_bound_index {
                                    return Err(RuntimeError::assignment_ro(None));
                                }
                                Self::assign_array_multidim(
                                    container,
                                    std::slice::from_ref(&idx),
                                    val.clone(),
                                )?;
                            } else if let Some((slice_indices, vals)) = &range_slice {
                                if let Value::Array(items, ..) = container {
                                    let arr = Arc::make_mut(items);
                                    if let Some(max_idx) = slice_indices.last().copied()
                                        && max_idx >= arr.len()
                                    {
                                        arr.resize(
                                            max_idx + 1,
                                            Value::Package(Symbol::intern("Any")),
                                        );
                                    }
                                }
                                for (offset, i) in slice_indices.iter().enumerate() {
                                    let key = Value::Int(*i as i64);
                                    let v = vals.get(offset).cloned().unwrap_or(Value::Nil);
                                    Self::assign_array_multidim(
                                        container,
                                        std::slice::from_ref(&key),
                                        v,
                                    )?;
                                    range_initialized_marks.push(Self::encode_bound_index(&key));
                                }
                            } else if let Some(i) = Self::index_to_usize(&idx) {
                                if let Value::Array(items, ..) = container {
                                    let is_self_array_ref = matches!(
                                        &val,
                                        Value::Array(source_items, ..) if Arc::ptr_eq(items, source_items)
                                    );
                                    // Use in-place mutation when the array is shared
                                    // (strong_count > 1) to preserve identity semantics
                                    // and support shared `ContainerRef` cell binding.
                                    let use_inplace =
                                        Arc::strong_count(items) > 1 && !var_name.starts_with('@');
                                    let arr: &mut crate::value::ArrayData = if use_inplace {
                                        // SAFETY: aliased in-place mutation of a
                                        // shared array; see `arc_contents_mut`.
                                        unsafe { crate::value::arc_contents_mut(items) }
                                    } else {
                                        Arc::make_mut(items)
                                    };
                                    if i >= arr.len() {
                                        let fill = native_fill.clone();
                                        arr.resize(i + 1, fill);
                                    }
                                    if bind_mode && let Some((source_install, cell)) = &bind_cell {
                                        // Phase 2 Stage 2: a `:=`-bound element
                                        // holds a shared `ContainerRef` cell (no
                                        // more BOUND_ARRAY_REF_SENTINEL
                                        // back-references). Reads decont at
                                        // `resolve_array_entry`; writes go
                                        // through the cell arm below.
                                        arr[i] = Value::ContainerRef(cell.clone());
                                        stored_bind_cell = true;
                                        if let Some(source_name) = source_install {
                                            pending_source_cell =
                                                Some((source_name.clone(), cell.clone()));
                                        }
                                    } else if let Some((source_name, source_index)) =
                                        Self::varref_target(&arr[i])
                                    {
                                        pending_varref_update =
                                            Some((source_name.clone(), source_index, val.clone()));
                                        arr[i] = Self::make_varref_value(
                                            source_name,
                                            val.clone(),
                                            source_index,
                                        );
                                    } else if let Value::ContainerRef(cell) = &arr[i] {
                                        if elem_is_value_share {
                                            // Slice 2b: a `=`-shared element
                                            // reassigned with a non-share value
                                            // REPLACES the slot (raku value
                                            // semantics) — drop the shared cell so
                                            // the source stays unaffected.
                                            arr[i] = native_store_val.clone();
                                        } else {
                                            // Phase 2: the element is a `:=`-bound
                                            // shared cell — write through it so the
                                            // alias observes the new value.
                                            *cell.lock().unwrap() = native_store_val.clone();
                                        }
                                    } else {
                                        arr[i] = if is_self_array_ref {
                                            Self::self_array_ref_marker()
                                        } else {
                                            // A native integer array stores the
                                            // wrapped value (e.g. -1 -> 255 in a
                                            // uint8 array), while the assignment
                                            // expression still yields the original.
                                            native_store_val.clone()
                                        };
                                    }
                                }
                            } else if let Value::Int(i) = &idx
                                && *i < 0
                            {
                                // Negative index from WhateverCode resolution
                                // (e.g. @arr[*-1] = 42 on empty array)
                                return Err(Self::make_out_of_range_error(*i));
                            } else {
                                return Err(RuntimeError::new("Index out of bounds"));
                            }
                            self.mark_initialized_index(&var_name, encoded_idx.clone());
                            // A cell-bound element does not need the bound-index
                            // side table — the `ContainerRef` cell is the alias
                            // and write-through happens via the cell.
                            if bind_mode && !stored_bind_cell {
                                self.mark_bound_index(&var_name, encoded_idx.clone());
                            }
                        }
                        Value::Set(ref mut set, is_mutable) => {
                            if !is_mutable {
                                return Err(RuntimeError::assignment_ro(Some("Set")));
                            }
                            let s = Arc::make_mut(set);
                            if val.truthy() {
                                s.insert(key.clone());
                            } else {
                                s.remove(&key);
                            }
                        }
                        Value::Bag(ref mut bag, is_mutable) => {
                            if !is_mutable {
                                return Err(RuntimeError::assignment_ro(Some("Bag")));
                            }
                            let b = Arc::make_mut(bag);
                            let count = Self::bag_assignment_count(&val)?;
                            if count == num_bigint::BigInt::from(0) {
                                b.remove(&key);
                            } else {
                                b.insert(key.clone(), count);
                            }
                        }
                        Value::Mix(ref mut mix, is_mutable) => {
                            if !is_mutable {
                                return Err(RuntimeError::assignment_ro(Some("Mix")));
                            }
                            let m = Arc::make_mut(mix);
                            let weight = Self::mix_assignment_weight(&val)?;
                            if weight == 0.0 {
                                m.remove(&key);
                            } else {
                                m.insert(key.clone(), weight);
                            }
                        }
                        // Autovivify typed containers: MixHash, BagHash, SetHash
                        Value::Package(sym)
                            if matches!(
                                sym.resolve().as_str(),
                                "MixHash" | "BagHash" | "SetHash"
                            ) =>
                        {
                            let type_name = sym.resolve();
                            match type_name.as_str() {
                                "MixHash" => {
                                    let mut weights = HashMap::new();
                                    let weight = Self::mix_assignment_weight(&val)?;
                                    if weight != 0.0 {
                                        weights.insert(key.clone(), weight);
                                    }
                                    *container = Value::mix_hash(weights);
                                }
                                "BagHash" => {
                                    let mut counts = HashMap::new();
                                    let count = Self::bag_assignment_count(&val)?;
                                    if num_traits::Signed::is_positive(&count) {
                                        counts.insert(key.clone(), count);
                                    }
                                    *container = Value::bag_hash_big(counts);
                                }
                                "SetHash" => {
                                    let mut items = std::collections::HashSet::new();
                                    if val.truthy() {
                                        items.insert(key.clone());
                                    }
                                    *container = Value::Set(
                                        Arc::new(crate::value::SetData::new(items)),
                                        true,
                                    );
                                }
                                _ => unreachable!(),
                            }
                        }
                        _ => {
                            // Autovivify Nil/uninitialized container: pick
                            // Array if subscript was positional, else Hash.
                            if (var_name.starts_with('@')
                                || (is_positional && !var_name.starts_with('%')))
                                && let Some(i) = Self::index_to_usize(&idx)
                            {
                                let mut arr = vec![Value::Package(Symbol::intern("Any")); i + 1];
                                arr[i] = val.clone();
                                *container = Value::real_array(arr);
                            } else {
                                let mut hash = std::collections::HashMap::new();
                                hash.insert(key.clone(), val.clone());
                                let mut hash_val = Value::hash(hash);
                                if use_which {
                                    let mut orig = HashMap::new();
                                    orig.insert(key.clone(), idx.clone());
                                    hash_val =
                                        runtime::utils::set_hash_original_keys(hash_val, orig);
                                }
                                *container = hash_val;
                            }
                        }
                    }
                } else {
                    // Autovivify the missing variable
                    if (var_name.starts_with('@') || (is_positional && !var_name.starts_with('%')))
                        && let Some(i) = Self::index_to_usize(&idx)
                    {
                        let mut arr = vec![Value::Package(Symbol::intern("Any")); i + 1];
                        arr[i] = val.clone();
                        self.env_mut()
                            .insert(var_name.clone(), Value::real_array(arr));
                    } else {
                        let mut hash = std::collections::HashMap::new();
                        hash.insert(key.clone(), val.clone());
                        let mut hash_val = Value::hash(hash);
                        if use_which {
                            let mut orig = HashMap::new();
                            orig.insert(key.clone(), idx.clone());
                            hash_val = runtime::utils::set_hash_original_keys(hash_val, orig);
                        }
                        self.env_mut().insert(var_name.clone(), hash_val);
                    }
                }
                if let Some((source_name, source_index, source_value)) = pending_varref_update {
                    self.assign_varref_target(&source_name, source_index, source_value)?;
                }
                if let Some((source_name, cell)) = pending_source_cell {
                    // Bind the source variable to the same cell installed at the
                    // element, so both sides alias one container.
                    let cell_val = Value::ContainerRef(cell);
                    self.set_env_with_main_alias(&source_name, cell_val.clone());
                    self.update_local_if_exists(code, &source_name, &cell_val);
                }
                for encoded in range_initialized_marks {
                    self.mark_initialized_index(&var_name, encoded);
                }
                // Slice 2b: a `=`-shared element just reassigned with a non-share
                // value has been replaced by a plain value — drop the share
                // marker so a later `:=` bind of the same element is not mistaken
                // for a value share.
                if elem_is_value_share {
                    self.clear_element_share(&var_name, &encoded_idx);
                }
                // Sync OS environment when %*ENV is modified
                #[cfg(not(target_family = "wasm"))]
                if var_name == "%*ENV" {
                    // SAFETY: mutsu is single-threaded
                    unsafe {
                        std::env::set_var(&key, val.to_string_value());
                    }
                }
                // Sync $*HOME when %*ENV<HOME> changes
                if var_name == "%*ENV" && key == "HOME" {
                    let home_str = val.to_string_value();
                    let home_val = self.make_io_path_instance(&home_str);
                    self.env_mut()
                        .insert("$*HOME".to_string(), home_val.clone());
                    self.env_mut().insert("*HOME".to_string(), home_val);
                }
            }
        }
        if let Some(updated) = self.get_env_with_main_alias(&var_name) {
            self.update_local_if_exists(code, &var_name, &updated);
            // Write the whole topic back to its source variable only when the
            // source is a scalar (e.g. `for $x { $_[1] = ... }`). For array/hash
            // sources (`for @a { $_[1] = ... }`), the for-loop's own per-element
            // writeback (write_back_for_topic_item) syncs the mutated element
            // back at the correct index; replacing the whole container with the
            // single topic value here would corrupt the source.
            if var_name == "_"
                && let Some(ref source_var) = self.topic_source_var
                && !source_var.starts_with('@')
                && !source_var.starts_with('%')
            {
                // For @/% sources, element writeback to a per-index slot is
                // handled by write_back_for_topic_item at the end of the loop
                // body. Overwriting the whole container with $_ here would
                // clobber the aggregate (e.g. `$_[1] = 9 for @a`).
                let source_name = source_var.clone();
                self.set_env_with_main_alias(&source_name, updated.clone());
                self.update_local_if_exists(code, &source_name, &updated);
            }
            // Re-attach the `is default(...)` element default after mutation
            // when a rebuild dropped it (embedded in ArrayData, so plain
            // Arc::make_mut mutations carry it on their own).
            if let Some(def) = self.var_default(&var_name).cloned()
                && self.container_default(&updated).is_none()
            {
                let tagged = self.tag_container_default(updated.clone(), def);
                self.set_env_with_main_alias(&var_name, tagged.clone());
                self.update_local_if_exists(code, &var_name, &tagged);
            }
        }
        // When operating through a sigilless alias (e.g., `h` → `%a`),
        // sync the modified container back to the alias variable so reads
        // of the sigilless variable see the updated value.
        if let Some(ref alias_target) = sigilless_alias_target
            && let Some(updated_container) = self.env().get(alias_target).cloned()
        {
            self.env_mut()
                .insert(original_var_name.clone(), updated_container.clone());
            self.update_local_if_exists(code, &original_var_name, &updated_container);
        }
        // After element assignment, Arc::make_mut may have created a new Arc
        // (COW). Sync HashEntryRef locals whose root `hash` Arc pointed to the
        // OLD container so they reference the new one. Only update refs that
        // pointed to the same container (identified by old_container_arc_ptr),
        // not refs to unrelated nested containers.
        if let Some(old_ptr) = old_container_arc_ptr {
            let current = self
                .get_env_with_main_alias(&var_name)
                .or_else(|| self.env().get(&original_var_name).cloned());
            if let Some(ref container) = current {
                let new_arc_ptr = match container {
                    Value::Array(arc, _) => Some(Arc::as_ptr(arc) as usize),
                    Value::Hash(arc) => Some(Arc::as_ptr(arc) as usize),
                    _ => None,
                };
                if let Some(new_ptr) = new_arc_ptr {
                    // Only sync if the Arc pointer actually changed (COW happened)
                    if new_ptr != old_ptr {
                        for local in self.locals.iter_mut() {
                            // Only update refs that pointed to the OLD container.
                            if let Value::HashEntryRef { hash, .. } = local
                                && Arc::as_ptr(hash) as usize == old_ptr
                                && let Value::Hash(new_arc) = container
                            {
                                *hash = new_arc.clone();
                            }
                        }
                    }
                }
            }
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_index_assign_pseudo_stash_named_op(
        &mut self,
        code: &CompiledCode,
        stash_name_idx: u32,
        key_name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let stash_name = Self::const_str(code, stash_name_idx);
        // `PROCESS::<$name> = value` sets the process-level dynamic variable, so
        // a later `$*name` (stored in the env as `*name`) resolves to it. This is
        // how `Rakudo::Internals.REGISTER-DYNAMIC` installs defaults (e.g.
        // DBIish's `$*DBI-DEFS`).
        if stash_name == "PROCESS::" {
            let raw_key = Self::const_str(code, key_name_idx).to_string();
            // Map the sigiled stash key to the env dynamic-var key:
            //   $name → *name, @name → @*name, %name → %*name, name → *name
            let env_key = match raw_key.chars().next() {
                Some('$') => format!("*{}", &raw_key[1..]),
                Some('@') => format!("@*{}", &raw_key[1..]),
                Some('%') => format!("%*{}", &raw_key[1..]),
                _ => format!("*{raw_key}"),
            };
            let val = self.stack.pop().unwrap_or(Value::Nil);
            let val = if env_key.starts_with('@') {
                runtime::coerce_to_array(val)
            } else if env_key.starts_with('%') {
                self.coerce_hash_var_value(&env_key, val)?
            } else {
                Self::normalize_scalar_assignment_value(val)
            };
            self.env_mut().insert(env_key, val.clone());
            // An assignment is an expression: leave the assigned value on the
            // stack so `Rakudo::Internals.REGISTER-DYNAMIC`'s block (whose body is
            // `PROCESS::<$x> = ...`) returns it.
            self.stack.push(val);
            return Ok(());
        }
        if stash_name != "MY::" {
            return Err(RuntimeError::new(format!(
                "Unsupported pseudo-stash assignment target {stash_name}"
            )));
        }

        let raw_key = Self::const_str(code, key_name_idx);
        let resolved_name = if let Some(name) = raw_key.strip_prefix('$') {
            name.to_string()
        } else {
            raw_key.to_string()
        };

        let val = self.stack.pop().unwrap_or(Value::Nil);
        if let Some(slot) = self.find_local_slot(code, &resolved_name) {
            self.stack.push(val);
            self.exec_assign_expr_local_op(code, slot as u32)
        } else {
            let mut val = if resolved_name.starts_with('@') {
                runtime::coerce_to_array(val)
            } else if resolved_name.starts_with('%') {
                self.coerce_hash_var_value(&resolved_name, val)?
            } else {
                Self::normalize_scalar_assignment_value(val)
            };

            self.check_readonly_for_modify(&resolved_name)?;
            if let Some(default) = self.var_default(&resolved_name)
                && matches!(val, Value::Nil)
            {
                val = default.clone();
            }
            if resolved_name.starts_with('@') || resolved_name.starts_with('%') {
                val = self.coerce_typed_container_assignment(&resolved_name, val, false)?;
            }
            if let Some(constraint) = loan_env!(self, var_type_constraint(&resolved_name))
                && !resolved_name.starts_with('@')
                && !resolved_name.starts_with('%')
            {
                if matches!(val, Value::Nil) {
                    if constraint != "Mu" {
                        let nominal =
                            loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                        val = Value::Package(Symbol::intern(&nominal));
                    }
                } else if !self.type_matches_value(&constraint, &val) {
                    return Err(runtime::utils::type_check_assignment_typed_error(
                        &resolved_name,
                        &constraint,
                        &val,
                    ));
                }
                if !matches!(val, Value::Nil | Value::Package(_)) {
                    val = loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?;
                }
            }

            self.set_env_with_main_alias(&resolved_name, val.clone());
            self.stack.push(val);
            Ok(())
        }
    }

    pub(super) fn exec_index_assign_expr_nested_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        outer_positional: bool,
        inner_positional: bool,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let native_fill = {
            let tc = loan_env!(self, var_type_constraint(&var_name));
            Self::native_fill_for_constraint(tc.as_deref())
        };
        let inner_idx = self.stack.pop().unwrap_or(Value::Nil);
        let outer_idx = self.stack.pop().unwrap_or(Value::Nil);
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        // Detect bind marker (__mutsu_bind_index_value) and extract the actual value
        let val = match raw_val {
            Value::Pair(ref name, ref payload) if name == "__mutsu_bind_index_value" => {
                match payload.as_ref() {
                    Value::Array(items, ..) if !items.is_empty() => {
                        items.first().cloned().unwrap_or(Value::Nil)
                    }
                    other => other.clone(),
                }
            }
            other => other,
        };

        // Junction / slice outer subscript (`%h<x>{any('p','q')} = v`,
        // `%h<x>{@k} = (...)`): autothread per outer key. The rest of this op
        // handles a single scalar outer key, so re-dispatch for each expanded
        // key (previously the junction/array was stringified into one garbage
        // entry, so a later read of any real key returned Any).
        if matches!(outer_idx, Value::Junction { .. } | Value::Array(..)) {
            let outer_keys: Vec<Value> = match &outer_idx {
                Value::Junction { values, .. } => values.as_ref().clone(),
                Value::Array(a, ..) => a.items.to_vec(),
                _ => unreachable!(),
            };
            // A scalar RHS goes to every junction key; a slice/junction RHS
            // pairs element-wise.
            let vals: Vec<Value> = match (&outer_idx, &val) {
                (Value::Junction { .. }, Value::Junction { values: jv, .. }) => jv.as_ref().clone(),
                (Value::Junction { .. }, _) => vec![val.clone(); outer_keys.len()],
                _ => self.assignment_rhs_values(&val)?,
            };
            for (i, ok) in outer_keys.into_iter().enumerate() {
                let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                // Re-dispatch with a single scalar outer key. Stack order
                // (bottom→top): value, outer_idx, inner_idx.
                self.stack.push(v);
                self.stack.push(ok);
                self.stack.push(inner_idx.clone());
                self.exec_index_assign_expr_nested_op(
                    code,
                    name_idx,
                    outer_positional,
                    inner_positional,
                )?;
                self.stack.pop();
            }
            self.stack.push(val);
            return Ok(());
        }

        let inner_key = inner_idx.to_string_value();
        let outer_key = outer_idx.to_string_value();

        // If the outer hash has a value type constraint, nested autovivification
        // would need to create a Hash value which won't match the constraint
        // (e.g. `my Int %h; %h<z><t> = 3` should die because %h<z> can't be a Hash).
        if var_name.starts_with('%')
            && let Some(constraint) = loan_env!(self, var_type_constraint(&var_name))
        {
            let inner_hash = Value::hash(std::collections::HashMap::new());
            if !loan_env!(self, type_matches_value(&constraint, &inner_hash)) {
                return Err(RuntimeError::new(format!(
                    "Type check failed in assignment to {var_name}; expected {constraint} but got Hash (autovivification)"
                )));
            }
        }

        if !self.env().contains_key(&var_name) {
            // Autovivify the variable as Array if the inner subscript was
            // positional (`[...]`), otherwise as Hash. For sigiled vars
            // (`@x`, `%h`) the sigil already constrains the kind, so this
            // mainly matters for scalar `$x` autoviv.
            let init = if var_name.starts_with('@') {
                Value::real_array(Vec::new())
            } else if var_name.starts_with('%') {
                Value::hash(std::collections::HashMap::new())
            } else if inner_positional {
                Value::real_array(Vec::new())
            } else {
                Value::hash(std::collections::HashMap::new())
            };
            self.env_mut().insert(var_name.clone(), init);
        }

        // Handle Array-as-outer-container (e.g. `@array[42][23] = 17`,
        // `@array[42]<key> = 17`). `inner_key` is the first/closer subscript
        // value (here "42"). `outer_key` is the outermost subscript value
        // (here "23" or "key"). We autovivify the missing entry at
        // `inner_i` based on `outer_positional`.
        // Drop the locals copy first so the Arc refcount is 1 (avoids
        // unnecessary cloning in Arc::make_mut).
        if let Some(slot) = self.find_local_slot(code, &var_name)
            && matches!(self.locals[slot], Value::Array(..))
        {
            self.locals[slot] = Value::Nil;
        }
        if let Some(Value::Array(outer_arr, _kind)) = self.env_root_descended_mut(&var_name)
            && let Ok(inner_i) = inner_key.parse::<usize>()
        {
            let arr = Arc::make_mut(outer_arr);
            if inner_i >= arr.len() {
                let fill = native_fill.clone();
                arr.resize(inner_i + 1, fill);
            }
            // Autovivify the slot if it's not already a container. A
            // `:=`-bound element is a shared `ContainerRef` cell holding a
            // container — descend through it (below) instead of clobbering it.
            let needs_viv = !matches!(
                &arr[inner_i],
                Value::Array(..) | Value::Hash(..) | Value::ContainerRef(_)
            );
            if needs_viv {
                arr[inner_i] = if outer_positional {
                    Value::real_array(Vec::new())
                } else {
                    Value::hash(std::collections::HashMap::new())
                };
            }
            match &mut arr[inner_i] {
                // A bound element holds a shared cell: write through it so the
                // mutation reaches the aliased container (`@h[i] := @inner;
                // @h[i][j] = v` updates `@inner[j]`).
                Value::ContainerRef(_) => {
                    Self::assign_into_nested_container(&mut arr[inner_i], &outer_key, val.clone());
                }
                Value::Array(inner_arr, _) => {
                    if let Ok(j) = outer_key.parse::<usize>() {
                        // Use interior mutation when the inner array is shared
                        // (e.g., by a `ContainerRef` cell from := binding).
                        if Arc::strong_count(inner_arr) > 1 {
                            // SAFETY: aliased in-place mutation of a shared array
                            // (strong_count > 1); see `arc_contents_mut`.
                            let v = &mut unsafe { crate::value::arc_contents_mut(inner_arr) }.items;
                            while v.len() <= j {
                                v.push(Value::Nil);
                            }
                            Value::assign_element_slot(&mut v[j], val.clone());
                        } else {
                            let inner = Arc::make_mut(inner_arr);
                            if j >= inner.len() {
                                let fill = native_fill.clone();
                                inner.resize(j + 1, fill);
                            }
                            Value::assign_element_slot(&mut inner[j], val.clone());
                        }
                    }
                }
                Value::Hash(inner_hash) => {
                    if Arc::strong_count(inner_hash) > 1 {
                        // SAFETY: aliased in-place mutation of a shared hash
                        // (strong_count > 1); see `arc_contents_mut`.
                        let hd = unsafe { crate::value::arc_contents_mut(inner_hash) };
                        Value::hash_insert_through(&mut hd.map, outer_key.clone(), val.clone());
                    } else {
                        let h = Arc::make_mut(inner_hash);
                        Value::hash_insert_through(h, outer_key.clone(), val.clone());
                    }
                }
                _ => {}
            }
            if let Some(updated) = self.get_env_with_main_alias(&var_name) {
                self.update_local_if_exists(code, &var_name, &updated);
            }
            self.stack.push(val);
            return Ok(());
        }

        // Hash-based nested assignment (Hash-in-Hash or Hash-containing-Array)
        // Drop the locals copy first so the Arc refcount is 1.
        // This avoids unnecessary cloning in Arc::make_mut which would
        // change the pointer and break .WHICH identity stability.
        if let Some(slot) = self.find_local_slot(code, &var_name) {
            self.locals[slot] = Value::Nil;
        }
        if let Some(Value::Hash(outer_hash)) = self.env_root_descended_mut(&var_name) {
            // At refcount 1, write in place via the raw pointer (same pattern
            // as the bound-cell Array arm below): `Arc::make_mut` relocates a
            // Weak-guarded (metadata-bearing, e.g. object-hash / `is default`)
            // hash even when it is the only strong holder, which would break
            // `.WHICH` pointer identity. When shared, keep the make_mut COW
            // detach. The cast MUST target `HashData` (not its inner map) —
            // see docs/hashdata-migration-plan.md "Latent UB found & fixed".
            let oh: &mut crate::value::HashData = if Arc::strong_count(outer_hash) == 1 {
                // SAFETY: single strong holder; in-place avoids the make_mut
                // relocation that would break `.WHICH` identity. See
                // `arc_contents_mut`.
                unsafe { crate::value::arc_contents_mut(outer_hash) }
            } else {
                Arc::make_mut(outer_hash)
            };
            // Vivify the missing entry as Array if the OUTER (second) subscript
            // is positional (e.g. `%h<key>[42] = ...`), otherwise as Hash.
            let inner_val = oh.entry(inner_key).or_insert_with(|| {
                if outer_positional {
                    Value::real_array(Vec::new())
                } else {
                    Value::hash(std::collections::HashMap::new())
                }
            });
            Self::assign_into_nested_container(inner_val, &outer_key, val.clone());
        }
        if let Some(updated) = self.get_env_with_main_alias(&var_name) {
            self.update_local_if_exists(code, &var_name, &updated);
        }
        self.stack.push(val);
        Ok(())
    }

    /// Assign `val` into `target[outer_key]`, descending through any chain of
    /// `:=`-bound container cells (`ContainerRef`). Used by the 2-level nested
    /// assign so that a write to a container-valued bound element
    /// (`%h<key><inner> = ...` where `%h<key>` is a cell) mutates the shared,
    /// held container in place instead of being silently dropped.
    pub(super) fn assign_into_nested_container(target: &mut Value, outer_key: &str, val: Value) {
        match target {
            Value::ContainerRef(cell) => {
                let mut guard = cell.lock().unwrap();
                Self::assign_into_nested_container(&mut guard, outer_key, val);
            }
            Value::Array(arr, _) => {
                if let Ok(i) = outer_key.parse::<usize>() {
                    if Arc::strong_count(arr) > 1 {
                        // SAFETY: aliased in-place mutation of a shared array
                        // (strong_count > 1); see `arc_contents_mut`.
                        let v = &mut unsafe { crate::value::arc_contents_mut(arr) }.items;
                        while v.len() <= i {
                            v.push(Value::Nil);
                        }
                        Value::assign_element_slot(&mut v[i], val);
                    } else {
                        let a = Arc::make_mut(arr);
                        if i >= a.len() {
                            a.resize(i + 1, Value::Nil);
                        }
                        Value::assign_element_slot(&mut a[i], val);
                    }
                }
            }
            Value::Hash(h) => {
                Value::hash_insert_through(&mut Arc::make_mut(h).map, outer_key.to_string(), val);
            }
            _ => {}
        }
    }

    /// Follow `current` through any chain of `:=`-bound container cells
    /// (`ContainerRef`), returning a raw pointer to the innermost held value.
    ///
    /// SAFETY: mutsu is single-threaded; the pointer derived from the cell's
    /// mutex data stays valid after the transient guard drops (the held `Value`
    /// is owned by the `Arc`, which the caller keeps alive).
    ///
    /// Cycle-safety (Phase 4): a self-referential bind can make a cell
    /// transitively hold a `ContainerRef` back to itself. A normal cell holds a
    /// Hash/Array (the loop stops immediately); only a cell-holding-cell chain
    /// can loop. Bound the descent depth and stop on overflow so a cyclic bind
    /// terminates instead of hanging.
    unsafe fn descend_container_ref(mut current: *mut Value) -> *mut Value {
        const MAX_DESCENT: usize = 256;
        for _ in 0..MAX_DESCENT {
            let cell = match unsafe { &mut *current } {
                Value::ContainerRef(cell) => cell.clone(),
                _ => return current,
            };
            let mut guard = cell.lock().unwrap();
            current = &mut *guard as *mut Value;
        }
        current
    }

    /// Read a root variable for index-assignment, descending through any
    /// `:=`-bound container cell so a `ContainerRef` root resolves to its held
    /// Hash/Array. Without this, a write to `$x[i]`/`$x<k>` where `$x` is itself
    /// a bound cell would fall through every handler's `Value::Hash`/`Value::Array`
    /// match and be silently dropped (Phase 3).
    ///
    /// SAFETY: single-threaded; the returned reference points into the cell's
    /// mutex data, kept alive by the `Arc` stored in env (see
    /// `descend_container_ref`).
    pub(crate) fn env_root_descended_mut(&mut self, var_name: &str) -> Option<&mut Value> {
        let root = self.env_mut().get_mut(var_name)? as *mut Value;
        let descended = unsafe { Self::descend_container_ref(root) };
        Some(unsafe { &mut *descended })
    }

    /// Deep nested index assignment (3+ levels): @a[i][j][k]... = val
    /// Stack order: [value, idx_outermost, ..., idx_innermost] (innermost on top).
    /// positional_flags_idx is a constant index holding an array of booleans
    /// (innermost to outermost) indicating whether each subscript is positional.
    pub(super) fn exec_index_assign_deep_nested_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        depth: u32,
        positional_flags_idx: u32,
    ) -> Result<(), RuntimeError> {
        let var_name = Self::const_str(code, name_idx).to_string();
        let native_fill = {
            let tc = loan_env!(self, var_type_constraint(&var_name));
            Self::native_fill_for_constraint(tc.as_deref())
        };
        let depth = depth as usize;

        // Pop indices from stack: innermost first (top of stack)
        let mut indices_val: Vec<Value> = Vec::with_capacity(depth);
        for _ in 0..depth {
            indices_val.push(self.stack.pop().unwrap_or(Value::Nil));
        }
        // indices_val[0] = innermost, indices_val[depth-1] = outermost

        let raw_val_for_junction = self.stack.pop().unwrap_or(Value::Nil);

        // Junction / slice OUTERMOST subscript (`%h<a><b>{any('p','q')} = v`):
        // autothread per outer key. The rest of this op handles a single scalar
        // outermost key, so re-dispatch for each expanded key (previously the
        // junction/array was stringified into one garbage entry).
        let outermost = &indices_val[depth - 1];
        if !matches!(&raw_val_for_junction, Value::Pair(n, _) if n == "__mutsu_bind_index_value")
            && matches!(outermost, Value::Junction { .. } | Value::Array(..))
        {
            let outer_keys: Vec<Value> = match outermost {
                Value::Junction { values, .. } => values.as_ref().clone(),
                Value::Array(a, ..) => a.items.to_vec(),
                _ => unreachable!(),
            };
            let vals: Vec<Value> = match (outermost, &raw_val_for_junction) {
                (Value::Junction { .. }, Value::Junction { values: jv, .. }) => jv.as_ref().clone(),
                (Value::Junction { .. }, _) => {
                    vec![raw_val_for_junction.clone(); outer_keys.len()]
                }
                _ => self.assignment_rhs_values(&raw_val_for_junction)?,
            };
            for (i, ok) in outer_keys.into_iter().enumerate() {
                let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                // Re-push for recursion. Stack (bottom→top): value, outermost,
                // ..., innermost. Replace the outermost with the scalar key.
                self.stack.push(v);
                self.stack.push(ok);
                for j in (0..depth - 1).rev() {
                    self.stack.push(indices_val[j].clone());
                }
                self.exec_index_assign_deep_nested_op(
                    code,
                    name_idx,
                    depth as u32,
                    positional_flags_idx,
                )?;
                self.stack.pop();
            }
            self.stack.push(raw_val_for_junction);
            return Ok(());
        }

        let indices: Vec<String> = indices_val.iter().map(|v| v.to_string_value()).collect();
        let val = raw_val_for_junction;

        // LHS binding (`$struct[..]<..>[..] := $scalar`): unwrap the bind
        // payload and promote the deep leaf element to a first-class
        // `ContainerRef` cell shared with the source variable (Phase 2 Stage 2,
        // the symmetric form of RHS element binding). Only a plain scalar source
        // is handled here; an indexed element source (`:= $b[j]`, encoded with
        // `\0idx\0`) is a separate slice. A cell survives COW of any enclosing
        // container, so a later write to either side reaches the other — which
        // the old `BOUND_ARRAY_REF_SENTINEL` by-name back-reference lost at depth.
        let (val, bind_source) = Self::unwrap_bind_index_value(val);
        let bind_source = bind_source.filter(|s| !s.contains("\x00idx\x00"));
        let bind_cell: Option<Arc<std::sync::Mutex<Value>>> = bind_source
            .as_ref()
            .map(|_| Arc::new(std::sync::Mutex::new(val.clone())));

        // Extract positional flags from constant
        let flags_val = code.constants[positional_flags_idx as usize].clone();
        let positional_flags: Vec<bool> = if let Value::Array(arr, _) = &flags_val {
            arr.iter().map(|v| matches!(v, Value::Bool(true))).collect()
        } else {
            vec![true; depth]
        };

        // Invalidate local cache for the variable
        if let Some(slot) = self.find_local_slot(code, &var_name)
            && matches!(self.locals[slot], Value::Array(..) | Value::Hash(..))
        {
            self.locals[slot] = Value::Nil;
        }

        // Ensure the root variable exists
        if !self.env().contains_key(&var_name) {
            let init = if var_name.starts_with('@') {
                Value::real_array(Vec::new())
            } else if var_name.starts_with('%') {
                Value::hash(std::collections::HashMap::new())
            } else if positional_flags[0] {
                Value::real_array(Vec::new())
            } else {
                Value::hash(std::collections::HashMap::new())
            };
            self.env_mut().insert(var_name.clone(), init);
        }

        // Walk down the chain of indices, autovivifying containers as needed.
        // We need a mutable reference to the current container at each level.
        // Use raw pointer traversal to avoid borrow checker issues with nested mutation.
        let root: *mut Value = self.env_mut().get_mut(&var_name).unwrap() as *mut Value;

        let mut current: *mut Value = root;

        // Walk through indices[0..depth-1], autovivifying intermediate containers
        for level in 0..depth {
            let key = &indices[level];
            let is_positional = positional_flags[level];

            // Descend through any `:=`-bound container cell at this level so the
            // traversal/assignment reaches the shared, held container.
            current = unsafe { Self::descend_container_ref(current) };

            if level < depth - 1 {
                // Intermediate level: autovivify and descend
                let next_positional = positional_flags[level + 1];
                unsafe {
                    match &mut *current {
                        Value::Array(arr_arc, _) => {
                            if let Ok(i) = key.parse::<usize>() {
                                let arr = Arc::make_mut(arr_arc);
                                if i >= arr.len() {
                                    let fill = native_fill.clone();
                                    arr.resize(i + 1, fill);
                                }
                                // Autovivify if needed. A `ContainerRef` is a
                                // `:=`-bound cell that holds (and is descended to)
                                // a container on the next iteration; treating it
                                // as "needs vivify" would clobber the binding.
                                let needs_viv = !matches!(
                                    &arr[i],
                                    Value::Array(..) | Value::Hash(..) | Value::ContainerRef(..)
                                );
                                if needs_viv {
                                    arr[i] = if next_positional {
                                        Value::real_array(Vec::new())
                                    } else {
                                        Value::hash(std::collections::HashMap::new())
                                    };
                                }
                                current = &mut arr[i] as *mut Value;
                            }
                        }
                        Value::Hash(hash_arc) => {
                            let hash = Arc::make_mut(hash_arc);
                            if !hash.contains_key(key.as_str()) {
                                let new_val = if next_positional {
                                    Value::real_array(Vec::new())
                                } else {
                                    Value::hash(std::collections::HashMap::new())
                                };
                                hash.insert(key.clone(), new_val);
                            }
                            current = hash.get_mut(key.as_str()).unwrap() as *mut Value;
                        }
                        _ => {
                            // Autovivify the root itself if needed
                            if is_positional {
                                *current = Value::real_array(Vec::new());
                            } else {
                                *current = Value::hash(std::collections::HashMap::new());
                            }
                            // Retry this level
                            match &mut *current {
                                Value::Array(arr_arc, _) => {
                                    if let Ok(i) = key.parse::<usize>() {
                                        let arr = Arc::make_mut(arr_arc);
                                        if i >= arr.len() {
                                            arr.resize(
                                                i + 1,
                                                Value::Package(Symbol::intern("Any")),
                                            );
                                        }
                                        arr[i] = if next_positional {
                                            Value::real_array(Vec::new())
                                        } else {
                                            Value::hash(std::collections::HashMap::new())
                                        };
                                        current = &mut arr[i] as *mut Value;
                                    }
                                }
                                Value::Hash(hash_arc) => {
                                    let hash = Arc::make_mut(hash_arc);
                                    let new_val = if next_positional {
                                        Value::real_array(Vec::new())
                                    } else {
                                        Value::hash(std::collections::HashMap::new())
                                    };
                                    hash.insert(key.clone(), new_val);
                                    current = hash.get_mut(key.as_str()).unwrap() as *mut Value;
                                }
                                _ => {}
                            }
                        }
                    }
                }
            } else {
                // Final level: assign the value. When binding, install the
                // shared cell *directly* (replacing any prior leaf), rather than
                // writing through it — a fresh `:=` rebinds the element to the
                // source's cell.
                let leaf_val = bind_cell
                    .as_ref()
                    .map(|c| Value::ContainerRef(c.clone()))
                    .unwrap_or_else(|| val.clone());
                unsafe {
                    match &mut *current {
                        Value::Array(arr_arc, _) => {
                            if let Ok(i) = key.parse::<usize>() {
                                let arr = Arc::make_mut(arr_arc);
                                if i >= arr.len() {
                                    let fill = native_fill.clone();
                                    arr.resize(i + 1, fill);
                                }
                                if bind_cell.is_some() {
                                    arr[i] = leaf_val;
                                } else {
                                    Value::assign_element_slot(&mut arr[i], leaf_val);
                                }
                            }
                        }
                        Value::Hash(hash_arc) => {
                            let hash = Arc::make_mut(hash_arc);
                            if bind_cell.is_some() {
                                hash.insert(key.clone(), leaf_val);
                            } else {
                                Value::hash_insert_through(hash, key.clone(), leaf_val);
                            }
                        }
                        _ => {
                            // Autovivify at final level
                            if is_positional {
                                let mut arr = Vec::new();
                                if let Ok(i) = key.parse::<usize>() {
                                    let fill = native_fill.clone();
                                    arr.resize(i + 1, fill);
                                    arr[i] = leaf_val;
                                }
                                *current = Value::real_array(arr);
                            } else {
                                let mut h = std::collections::HashMap::new();
                                h.insert(key.clone(), leaf_val);
                                *current = Value::hash(h);
                            }
                        }
                    }
                }
            }
        }

        // Write the shared cell back to the source variable so both sides alias
        // the same container (the symmetric counterpart of the leaf store above).
        if let (Some(src), Some(cell)) = (bind_source, bind_cell) {
            let cell_val = Value::ContainerRef(cell);
            self.set_env_with_main_alias(&src, cell_val.clone());
            self.update_local_if_exists(code, &src, &cell_val);
        }

        // Write the updated root container back into the local slot. The descent
        // above mutated `env`'s root in place (possibly COW-detaching its outer
        // `Arc` via `make_mut`), then the slot was invalidated to `Nil` (above)
        // on the assumption reverse-sync would re-pull it. Without reverse-sync a
        // later read through the locals store would see that stale `Nil` (or an
        // outer `Arc` that no longer reaches the freshly bound cell). Refreshing
        // the local from `env` here makes both stores share the same outer `Arc`
        // spine, so deep `:=` binds and writes stay coherent without a pull.
        if let Some(slot) = self.find_local_slot(code, &var_name)
            && let Some(updated) = self.env().get(&var_name).cloned()
        {
            self.locals[slot] = updated;
        }

        self.stack.push(val);
        Ok(())
    }

    /// Generic index assignment on a stack-computed target.
    /// Stack order: target (bottom), index, value (top).
    /// If the target hash has `__callframe_depth`, routes through set_caller_var.
    pub(super) fn exec_index_assign_generic_op(
        &mut self,
        code: &CompiledCode,
    ) -> Result<(), RuntimeError> {
        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let idx = self.stack.pop().unwrap_or(Value::Nil);
        let target = self.stack.pop().unwrap_or(Value::Nil);
        let key = idx.to_string_value();

        // Detect bind marker (__mutsu_bind_index_value) and extract the actual value
        let (val, bind_source) = match raw_val {
            Value::Pair(ref name, ref payload) if name == "__mutsu_bind_index_value" => {
                match payload.as_ref() {
                    Value::Array(items, ..) if items.len() >= 2 => {
                        let value = items.first().cloned().unwrap_or(Value::Nil);
                        let source = match items.get(1) {
                            Some(Value::Array(srcs, ..)) => srcs.first().and_then(|s| match s {
                                Value::Str(name) if !name.is_empty() => Some((**name).clone()),
                                _ => None,
                            }),
                            _ => None,
                        };
                        (value, source)
                    }
                    other => (other.clone(), None),
                }
            }
            other => (other, None),
        };

        // Phase 2 Stage 2: a `:=` bind to a stack-computed element target
        // (`f()<k> := $s`, `($ref)[i] := $s`) stores a shared `ContainerRef`
        // cell at the element and writes the same cell back to the source
        // variable, so a later write to either side reaches the other. This is
        // the computed-target analogue of the named-handler cell bind (slice
        // 3/4); the old `HashEntryRef`/array element back-reference by-reference into the env
        // was stale (the alias never propagated). Pre-read before any container
        // borrow. An element source (`:= @b[j]`, encoded with `\0`) is left to a
        // later slice unless it already arrived promoted to a cell.
        let bind_cell: Option<BindSourceCell> = if let Value::ContainerRef(cell) = &val {
            Some((None, cell.clone()))
        } else if let Some(source_name) = &bind_source
            && !source_name.contains('\0')
        {
            match self.env().get(source_name) {
                Some(Value::ContainerRef(cell)) => Some((None, cell.clone())),
                _ => Some((
                    Some(source_name.clone()),
                    Arc::new(std::sync::Mutex::new(val.clone())),
                )),
            }
        } else {
            None
        };

        // Junction / slice key on a stack-computed target
        // (`%h<x>{any('p','q')} = v`, `%h<x>{@k} = (...)`). The named-handler op
        // autothreads these; the generic op (computed target) previously
        // stringified the multi-key index and wrote a single garbage entry, so a
        // later read of any real key returned Any. Only plain assignment is
        // handled here (a multi-key `:=` bind is a separate, exotic path).
        if bind_cell.is_none() && matches!(idx, Value::Junction { .. } | Value::Array(..)) {
            let resolved = match &target {
                Value::HashEntryRef { .. } => target.hash_entry_read(),
                Value::Scalar(inner) => inner.as_ref().clone(),
                other => other.clone(),
            };
            if matches!(resolved, Value::Hash(_) | Value::Array(..)) {
                let (keys, vals): (Vec<Value>, Vec<Value>) = match &idx {
                    Value::Junction { values, .. } => {
                        // A scalar RHS is assigned to every junction key; a
                        // junction RHS pairs element-wise.
                        let vals = match &val {
                            Value::Junction { values: jv, .. } => jv.as_ref().clone(),
                            _ => vec![val.clone(); values.len()],
                        };
                        (values.as_ref().clone(), vals)
                    }
                    Value::Array(keys, ..) => {
                        (keys.items.to_vec(), self.assignment_rhs_values(&val)?)
                    }
                    _ => unreachable!(),
                };
                for (i, k) in keys.iter().enumerate() {
                    let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                    self.assign_into_computed_target(&resolved, k, v);
                }
                self.stack.push(val);
                return Ok(());
            }
        }

        match &target {
            Value::Hash(arc) => {
                // Check for callframe .my hash with depth marker
                if let Some(Value::Int(depth)) = arc.get("__callframe_depth") {
                    let depth = *depth as usize;
                    // Strip sigil from key to get bare variable name
                    let bare_name =
                        if key.starts_with('$') || key.starts_with('@') || key.starts_with('%') {
                            &key[1..]
                        } else {
                            &key
                        };
                    loan_env!(self, set_caller_var(bare_name, depth, val.clone()))?;
                    self.stack.push(val);
                    return Ok(());
                }
                // Interior mutation: write into the hash via raw pointer
                // so the change is visible to all holders of the same Arc.
                // In bind mode, store the shared cell at the key; otherwise the
                // plain value.
                let stored = match &bind_cell {
                    Some((_, cell)) => Value::ContainerRef(cell.clone()),
                    None => val.clone(),
                };
                // SAFETY: aliased in-place mutation of a shared hash so the change
                // is visible to all holders of the same Arc; see `arc_contents_mut`.
                let hd = unsafe { crate::value::arc_contents_mut(arc) };
                Value::hash_insert_through(&mut hd.map, key.clone(), stored);
                // For a fresh-cell bind, write the cell back to the source var
                // so both sides alias the same container.
                if let Some((Some(src), cell)) = &bind_cell {
                    let cell_val = Value::ContainerRef(cell.clone());
                    self.set_env_with_main_alias(src, cell_val.clone());
                    self.update_local_if_exists(code, src, &cell_val);
                }
                self.stack.push(val);
            }
            Value::Array(arc, _kind) => {
                // Interior mutation: write into the array via raw pointer
                // so the change is visible to all holders of the same Arc.
                if let Ok(i) = key.parse::<usize>() {
                    // SAFETY: aliased in-place mutation of a shared array so the
                    // change is visible to all holders of the same Arc; see
                    // `arc_contents_mut`.
                    let v = &mut unsafe { crate::value::arc_contents_mut(arc) }.items;
                    while v.len() <= i {
                        v.push(Value::Nil);
                    }
                    match &bind_cell {
                        // Bind mode installs the shared cell at the element;
                        // the same cell is written back to the source var
                        // below so both sides alias.
                        Some((_, cell)) => v[i] = Value::ContainerRef(cell.clone()),
                        // An element source (`:= @b[j]`) not promoted to a
                        // cell installs the payload directly (rare; left to a
                        // later slice).
                        None if bind_source.is_some() => v[i] = val.clone(),
                        None => {
                            // Write THROUGH an existing `:=`-bound cell so an
                            // assignment reached via a stack target (e.g.
                            // `get()<subkey>[1] = …`) updates the shared cell
                            // instead of clobbering it (nested.t 11-12).
                            Value::assign_element_slot(&mut v[i], val.clone());
                        }
                    }
                    // For a fresh-cell bind, write the cell back to the source var.
                    if let Some((Some(src), cell)) = &bind_cell {
                        let cell_val = Value::ContainerRef(cell.clone());
                        self.set_env_with_main_alias(src, cell_val.clone());
                        self.update_local_if_exists(code, src, &cell_val);
                    }
                }
                self.stack.push(val);
            }
            Value::HashEntryRef { .. } => {
                // Resolve the HashEntryRef and assign into the resolved container.
                let resolved = target.hash_entry_read();
                self.stack.push(resolved);
                self.stack.push(idx);
                self.stack.push(val);
                return self.exec_index_assign_generic_op(code);
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Stash" => {
                let package = attributes
                    .as_map()
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                if !package.is_empty() {
                    let key_name = if key.starts_with('$')
                        || key.starts_with('@')
                        || key.starts_with('%')
                        || key.starts_with('&')
                    {
                        key
                    } else {
                        format!("${key}")
                    };
                    let pkg = package.trim_end_matches("::");
                    let fq = if pkg.is_empty() || pkg == "GLOBAL" {
                        key_name
                    } else {
                        format!("{pkg}::{key_name}")
                    };
                    self.env_mut().insert(fq, val.clone());
                }
                self.stack.push(val);
            }
            _ => {
                self.stack.push(val);
            }
        }
        Ok(())
    }

    /// Assign a single value into a stack-computed Hash/Array `target` at `key`
    /// via interior mutation, writing through an existing `:=`-bound
    /// `ContainerRef` cell. Used by junction/slice autothreading in the generic
    /// index-assign op, where the target is a resolved inner container reached
    /// through a nested subscript (`%h<x>{...}`).
    fn assign_into_computed_target(&self, target: &Value, key: &Value, val: Value) {
        match target {
            Value::Hash(arc) => {
                let k = Value::hash_key_encode(key);
                // SAFETY: aliased in-place mutation of a shared hash; see
                // `arc_contents_mut`. No live borrow into the map.
                let hd = unsafe { crate::value::arc_contents_mut(arc) };
                Value::hash_insert_through(&mut hd.map, k, val);
            }
            Value::Array(arc, _) => {
                if let Some(i) = Self::index_to_usize(key) {
                    // SAFETY: aliased in-place mutation of a shared array; see
                    // `arc_contents_mut`.
                    let v = &mut unsafe { crate::value::arc_contents_mut(arc) }.items;
                    while v.len() <= i {
                        v.push(Value::Nil);
                    }
                    Value::assign_element_slot(&mut v[i], val);
                }
            }
            _ => {}
        }
    }

    /// Phase 2 phantom-entry: materialize a missing-key `:=` bind on the first
    /// write through the bound variable. A local holding a `HashEntryRef` deferred
    /// token (single missing key `$e := %m<solo>`, or a multi-key path
    /// `$d := %k<p><q>`) is converted into a shared `ContainerRef` cell: the path
    /// is walk-created (`hash_entry_terminal`) and the cell is installed at the
    /// terminal hash entry, and the local is replaced with the same cell. After
    /// this the bound var and the hash entry alias bidirectionally — the old
    /// plain-value materialization lost the alias so a later cross-write was not
    /// observed (the case-C bug). Returns `true` when it handled `idx`; the
    /// deferred token is unchanged until first write, so `:exists` and pre-write
    /// reads keep their lazy semantics.
    pub(super) fn materialize_bound_slot_to_cell(
        &mut self,
        code: &CompiledCode,
        idx: usize,
        val: Value,
    ) -> bool {
        let cell = match &self.locals[idx] {
            token @ Value::HashEntryRef { .. } => {
                // Walk-create the deferred path (single key for `$e := %m<solo>`,
                // multi-key for `$d := %k<p><q>`) and install the shared cell at
                // the terminal entry so the bound var and the hash entry alias
                // bidirectionally afterwards.
                let Some((arc, key)) = token.hash_entry_terminal() else {
                    return false;
                };
                let cell = Arc::new(std::sync::Mutex::new(val));
                // SAFETY: aliased in-place mutation of a shared hash; see
                // `arc_contents_mut`. No live borrow into the map.
                let hd = unsafe { crate::value::arc_contents_mut(&arc) };
                Value::hash_insert_through(&mut hd.map, key, Value::ContainerRef(cell.clone()));
                cell
            }
            _ => return false,
        };
        self.locals[idx] = Value::ContainerRef(cell);
        self.flush_local_to_env(code, idx);
        true
    }

    // --- Phase 3 Stage 2: scalar instance attributes as cell-direct (slice 1) ---
    //
    // For scalar attribute-twigil locals (`$!x` -> `!x`, `$.x` -> `.x`) the
    // instance's shared attribute cell is the single source of truth. Reads come
    // straight from the cell (so a mutation made in a nested method frame is
    // visible to the caller — the cross-frame bug), and every write mirrors the
    // local back into the cell. This lets the scalar writeback be dropped
    // (`writeback_attributes*` skip scalar attrs). Array/hash attributes still
    // use the materialize+writeback path for now (later slices).

    /// If `name` is an attribute-twigil local — scalar (`!x`/`.x`), array
    /// (`@!x`/`@.x`) or hash (`%!x`/`%.x`) — return `(bare attribute name,
    /// is_private)`. Excludes special vars (`!`, `.`) and internal names. The
    /// cell stores attributes under the bare name, so all six twigil forms of an
    /// attribute resolve to the same cell slot.
    pub(super) fn attr_twigil_base(name: &str) -> Option<(&str, bool)> {
        // Optional `@`/`%` sigil, then the `!` (private) / `.` (public) twigil.
        let rest = name
            .strip_prefix('@')
            .or_else(|| name.strip_prefix('%'))
            .unwrap_or(name);
        let (bare, is_private) = if let Some(b) = rest.strip_prefix('!') {
            (b, true)
        } else if let Some(b) = rest.strip_prefix('.') {
            (b, false)
        } else {
            return None;
        };
        // Attribute names are ordinary identifiers (start alpha/underscore). This
        // filters out `!=`, the bare `!`/`.` special vars, and `__mutsu_` keys.
        match bare.chars().next() {
            Some(c) if c.is_alphabetic() || c == '_' => Some((bare, is_private)),
            _ => None,
        }
    }

    /// Resolve the cell key for an attribute on the given instance, preferring
    /// the method owner class's qualified private key when present (Parent/Child
    /// same-named `$!priv` disambiguation), matching the order used when method
    /// frames materialize attributes. Returns `None` when the attribute does not
    /// exist in the cell (so non-attribute `.foo`/`!foo` lvalues fall through to
    /// the normal local/env handling).
    fn resolve_attr_cell_key(
        &self,
        name: &str,
        attrs: &crate::value::InstanceAttrs,
    ) -> Option<String> {
        let (bare, is_private) = Self::attr_twigil_base(name)?;
        let map = attrs.as_map();
        if is_private && let Some(owner) = self.method_class_stack_top() {
            let qkey = format!("{}\0{}", owner, bare);
            if map.contains_key(&qkey) {
                return Some(qkey);
            }
        }
        if map.contains_key(bare) {
            Some(bare.to_string())
        } else {
            None
        }
    }

    /// Read a scalar attribute straight from `self`'s shared cell. `Some` only
    /// when `name` is a scalar attr-twigil, `self` is a concrete instance, and
    /// the attribute exists in the cell.
    pub(super) fn read_self_attr_cell(&self, name: &str) -> Option<Value> {
        let twigil = self.canonical_attr_twigil(name)?;
        let self_val = self.get_env_with_main_alias("self")?;
        let Value::Instance { attributes, .. } = &self_val else {
            return None;
        };
        let key = self.resolve_attr_cell_key(&twigil, attributes)?;
        attributes.as_map().get(&key).cloned()
    }

    /// Map a variable name to its canonical attribute-twigil form for cell access:
    /// a direct twigil (`!x`/`@.y`/…) maps to itself; a bare sigilless name
    /// (`has $x` → `Var("x")`) resolves through the runtime alias table to its
    /// `!x` twigil. Returns `None` for ordinary (non-attribute) names. The
    /// sigilless lookup is gated on `sigilless_attrs_active` so the common case
    /// (no sigilless attributes) costs only a string check on the hot read path.
    fn canonical_attr_twigil(&self, name: &str) -> Option<String> {
        if Self::attr_twigil_base(name).is_some() {
            return Some(name.to_string());
        }
        if !self.sigilless_attrs_active {
            return None;
        }
        self.sigilless_attr_twigil(name)
    }

    /// Follow the `__mutsu_sigilless_alias::` chain from a bare sigilless name
    /// until it reaches an attribute twigil (`!x`), returning that twigil. The
    /// alias table is bidirectional (`x ↔ !x`), so the `seen` guard prevents a
    /// cycle; returns `None` if the chain has no attribute-twigil link.
    fn sigilless_attr_twigil(&self, name: &str) -> Option<String> {
        let mut current = name.to_string();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(current.clone()) {
            let key = format!("__mutsu_sigilless_alias::{}", current);
            match self.env().get(&key) {
                Some(Value::Str(next)) => {
                    let next = next.to_string();
                    if Self::attr_twigil_base(&next).is_some() {
                        return Some(next);
                    }
                    current = next;
                }
                _ => return None,
            }
        }
        None
    }

    /// Skip mirroring slot values that keep their legacy handling (`:=` bindings,
    /// Proxy accessors, hash/array slot refs, deferred hash access).
    fn is_non_mirrorable_attr_value(val: &Value) -> bool {
        matches!(
            val,
            Value::ContainerRef(_) | Value::Proxy { .. } | Value::HashEntryRef { .. }
        )
    }

    /// Write `val` into `self`'s shared cell for the scalar attribute named
    /// `name` (`!x`/`.x`), resolving the qualified private key when present.
    /// No-op when `name` is not a scalar attr-twigil, `self` is not a concrete
    /// instance, or the attribute does not exist on `self`.
    pub(super) fn write_self_attr_cell(&self, name: &str, val: Value) {
        if Self::attr_twigil_base(name).is_none() {
            return;
        }
        let Some(self_val) = self.get_env_with_main_alias("self") else {
            return;
        };
        let Value::Instance { attributes, .. } = &self_val else {
            return;
        };
        if let Some(key) = self.resolve_attr_cell_key(name, attributes) {
            attributes.insert(key, val);
        }
    }

    /// Mirror the current local slot value into `self`'s shared cell for a scalar
    /// attribute, after the normal write logic has finalized the slot.
    pub(super) fn mirror_attr_local_to_cell(&self, code: &CompiledCode, idx: usize) {
        let Some(name) = code.locals.get(idx) else {
            return;
        };
        if Self::attr_twigil_base(name).is_none()
            || Self::is_non_mirrorable_attr_value(&self.locals[idx])
        {
            return;
        }
        self.write_self_attr_cell(&name.clone(), self.locals[idx].clone());
    }

    /// Mirror the finalized value of the variable named `name` into `self`'s
    /// shared cell, for write ops that dispatch by name (e.g. the name-based
    /// `AssignExpr`). Reads the value back from the local slot or env after the
    /// op completed.
    pub(super) fn mirror_attr_value_to_cell_by_name(&self, code: &CompiledCode, name: &str) {
        if Self::attr_twigil_base(name).is_none() {
            return;
        }
        let val = self
            .find_local_slot(code, name)
            .map(|slot| self.locals[slot].clone())
            .or_else(|| self.get_env_with_main_alias(name));
        let Some(val) = val else {
            return;
        };
        if Self::is_non_mirrorable_attr_value(&val) {
            return;
        }
        self.write_self_attr_cell(name, val);
    }

    /// True if `name` is an array/hash attribute twigil (`@!`/`@.`/`%!`/`%.`).
    fn is_array_hash_attr_twigil(name: &str) -> bool {
        (name.starts_with("@!")
            || name.starts_with("@.")
            || name.starts_with("%!")
            || name.starts_with("%."))
            && Self::attr_twigil_base(name).is_some()
    }

    /// Snapshot the env/shared value of an array/hash attribute variable before a
    /// mutating op, so [`mirror_array_hash_attr_to_cell`] can tell a genuine
    /// mutation (env value changed) from a non-mutating method call (`@!a.join`)
    /// on a stale env copy — mirroring the stale copy would clobber a cross-frame
    /// cell mutation. Returns `None` for non-attribute targets (cheap fast path).
    ///
    /// Also refreshes the env/local copy from `self`'s live cell before the op
    /// runs: a closure-captured env copy is a stale snapshot from closure
    /// creation, so letting the op mutate it (and mirroring the result) would
    /// clobber keys written by earlier calls — `%!h{$k} = $v` inside a returned
    /// closure kept only the last write. After the refresh the op starts from
    /// the live cell value and the mirror's pre-snapshot is that same value.
    pub(super) fn array_hash_attr_env_snapshot(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Option<Value> {
        let name = Self::const_str(code, name_idx);
        if !Self::is_array_hash_attr_twigil(name) {
            return None;
        }
        let name = name.to_string();
        let env_val = self
            .get_env_with_main_alias(&name)
            .or_else(|| self.get_shared_var(&name));
        // `:=` bindings / slot refs keep their legacy env handling.
        if env_val
            .as_ref()
            .is_some_and(Self::is_non_mirrorable_attr_value)
        {
            return env_val;
        }
        if let Some(cell_val) = self.read_self_attr_cell(&name) {
            // Env copies share the cell value's Arc until a copy-on-write fork;
            // pointer inequality means the copy is stale (or absent) — adopt the
            // live cell value so the op mutates current state.
            if !env_val
                .as_ref()
                .is_some_and(|v| Self::same_container_arc(v, &cell_val))
            {
                self.env_mut().insert(name.clone(), cell_val.clone());
                if let Some(slot) = self.find_local_slot(code, &name) {
                    self.locals[slot] = cell_val.clone();
                }
            }
            return Some(cell_val);
        }
        env_val
    }

    /// Cheap container identity check: true when both values are the same
    /// Array/Hash Arc (a clone that has not been copy-on-write forked).
    fn same_container_arc(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Array(x, _), Value::Array(y, _)) => Arc::ptr_eq(x, y),
            (Value::Hash(x), Value::Hash(y)) => Arc::ptr_eq(x, y),
            _ => false,
        }
    }

    /// Mirror an array/hash attribute variable's post-mutation value into `self`'s
    /// shared cell (Phase 3 Stage 2b). Used after the mutating array/hash ops
    /// (`@!a.push`, `@!a[i]=`, `%!h<k>=`, …), which write the new container into
    /// env/shared keyed by `name`. Only fires for `@!`/`@.`/`%!`/`%.` twigils and
    /// only when the env value actually changed from `pre` (so a non-mutating
    /// method like `@!a.join` on a stale env copy does not clobber the cell).
    pub(super) fn mirror_array_hash_attr_to_cell(
        &self,
        code: &CompiledCode,
        name_idx: u32,
        pre: Option<Value>,
    ) {
        let name = Self::const_str(code, name_idx);
        if !Self::is_array_hash_attr_twigil(name) {
            return;
        }
        let name = name.to_string();
        // The mutating ops write the new container into env (or shared_vars).
        let val = self
            .get_env_with_main_alias(&name)
            .or_else(|| self.get_shared_var(&name));
        let Some(val) = val else {
            return;
        };
        // No env change -> either a non-mutating method or a no-op; do not write
        // a possibly-stale env copy over a cross-frame cell mutation.
        if pre.as_ref() == Some(&val) {
            return;
        }
        if Self::is_non_mirrorable_attr_value(&val) {
            return;
        }
        self.write_self_attr_cell(&name, val);
    }

    /// Refresh the local slot from `self`'s cell before a read-modify-write on a
    /// scalar attribute (increment/decrement), so the operation sees a mutation
    /// made in a nested frame rather than the materialized snapshot.
    fn sync_attr_local_from_cell(&mut self, code: &CompiledCode, idx: usize) {
        if self.locals[idx].is_container_ref() {
            return;
        }
        let Some(name) = code.locals.get(idx).cloned() else {
            return;
        };
        if let Some(cell_val) = self.read_self_attr_cell(&name) {
            self.locals[idx] = cell_val;
        }
    }

    /// Name-based wrapper: refresh the slot named `name` from `self`'s cell before
    /// a read-modify-write (used by the increment/decrement ops, which dispatch
    /// by name rather than slot index).
    pub(super) fn sync_attr_local_from_cell_by_name(&mut self, code: &CompiledCode, name: &str) {
        if Self::attr_twigil_base(name).is_none() {
            return;
        }
        if let Some(slot) = self.find_local_slot(code, name) {
            self.sync_attr_local_from_cell(code, slot);
        }
    }

    /// Name-based wrapper: mirror the slot named `name` into `self`'s cell after a
    /// read-modify-write.
    pub(super) fn mirror_attr_local_to_cell_by_name(&self, code: &CompiledCode, name: &str) {
        if Self::attr_twigil_base(name).is_none() {
            return;
        }
        if let Some(slot) = self.find_local_slot(code, name) {
            self.mirror_attr_local_to_cell(code, slot);
        }
    }

    /// Like `exec_get_local_op` but does NOT resolve HashEntryRef.
    /// Pushes the raw local value, preserving container references for `=:=` checks.
    pub(super) fn exec_get_local_raw_op(&mut self, idx: u32) {
        let idx = idx as usize;
        let val = self.locals[idx].clone();
        self.stack.push(val);
    }

    pub(super) fn exec_get_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let idx = idx as usize;
        // Check if this variable has a binding alias (e.g. from $CALLER::foo := $other_var)
        let name = code.locals.get(idx).cloned().unwrap_or_default();
        if let Some(bound_to) = self.resolve_binding(&name) {
            let bound_to = bound_to.to_string();
            if let Some(val) = self.env().get(&bound_to).cloned() {
                self.stack.push(val);
                return Ok(());
            }
        }
        // Attribute locals (!attr) modified by CAS: env holds the authoritative
        // value since sync_locals_from_env skips !-prefixed names for performance.
        if name.starts_with('!')
            && self.is_shared_var_dirty(&name)
            && let Some(val) = self.env().get(&name).cloned()
        {
            self.locals[idx] = val.clone();
            self.stack.push(val);
            return Ok(());
        }
        // Atomic-variable read: skip entirely (a `format!` plus two
        // `var_type_constraint` lookups) when no atomic storage has ever been
        // registered — the common case on this hot local-read path.
        if self.atomic_var_seen() {
            let atomic_name = name.strip_prefix('$').unwrap_or(&name);
            let atomic_name_key = format!("__mutsu_atomic_name::{atomic_name}");
            // Only use the scalar atomic fast path for scalar ($) variables.
            // Array (@) variables with `atomicint` constraint are element-wise
            // atomic and should go through the normal array read path.
            let is_atomic_int = !name.starts_with('@')
                && (loan_env!(self, var_type_constraint(&name)).as_deref() == Some("atomicint")
                    || loan_env!(self, var_type_constraint(atomic_name)).as_deref()
                        == Some("atomicint")
                    || self.get_shared_var(&atomic_name_key).is_some());
            if is_atomic_int {
                let fetched = loan_env!(
                    self,
                    builtin_atomic_fetch_var(&[Value::str(atomic_name.to_string())])
                )?;
                self.locals[idx] = fetched.clone();
                self.stack.push(fetched);
                return Ok(());
            }
        }
        // Atomic array CAS stores the authoritative array under an internal
        // shared key.  Check it first so reads pick up the latest CAS'd value.
        if name.starts_with('@') {
            let atomic_key = format!("__mutsu_atomic_arr::{name}");
            if let Some(shared_val) = self.get_shared_var(&atomic_key) {
                self.locals[idx] = shared_val.clone();
                self.stack.push(shared_val);
                return Ok(());
            }
        }
        // Shared @/% variables may be mutated by sibling threads while this Interpreter
        // still holds an old local snapshot. Prefer the shared copy so reads
        // observe the latest value without forcing array COW on every push.
        if (name.starts_with('@') || name.starts_with('%'))
            && let Some(shared_val) = self.get_shared_var(&name)
        {
            self.stack.push(shared_val);
            return Ok(());
        }
        // Lazy sync: if the local is not a ContainerRef but env has one
        // (e.g., a cross-scope `:=` binding was established during a function/method
        // call and propagated back to env but not to locals), adopt the ContainerRef.
        // Skip for type objects and complex values that should not be replaced.
        if !self.locals[idx].is_container_ref()
            && !matches!(
                self.locals[idx],
                Value::Package(_)
                    | Value::Array(..)
                    | Value::Hash(..)
                    | Value::Sub(..)
                    | Value::Instance { .. }
            )
            && let Some(Value::ContainerRef(arc)) = self.env().get(&name).cloned()
        {
            self.locals[idx] = Value::ContainerRef(arc);
        }
        // Phase 3 Stage 2 (scalar slice): scalar instance attributes read straight
        // from `self`'s shared cell, so a mutation made in a nested method frame
        // is visible here. Gated on a non-container slot so `$!x := outer`
        // bindings keep their ContainerRef handling. The cell lookup returns None
        // for non-attribute names and when `self` is not an instance.
        if !self.locals[idx].is_container_ref()
            && let Some(cell_val) = self.read_self_attr_cell(&name)
        {
            self.locals[idx] = cell_val.clone();
            self.stack.push(cell_val);
            return Ok(());
        }
        let val = self.locals[idx].clone();
        // Resolve a deferred bind token to its current value (Any if the path
        // doesn't exist). The raw local slot is unchanged, so a later write still
        // materializes it; `=:=` reads the raw slot via GetLocalRaw.
        if let Value::HashEntryRef { .. } = &val {
            self.stack.push(val.hash_entry_read());
            return Ok(());
        }
        // Force lazy thunks transparently on access
        if let Value::LazyThunk(ref thunk_data) = val {
            let forced = self.force_lazy_thunk(thunk_data)?;
            self.stack.push(forced);
            return Ok(());
        }
        // Auto-deref ContainerRef: read the inner value for stack use (ContainerRef
        // axis of the decont family). Gate on is_container_ref() to preserve the
        // early return AND keep the non-container hot path move-only (into_deref is
        // never reached for non-ContainerRef values).
        if val.is_container_ref() {
            self.stack.push(val.into_deref());
            return Ok(());
        }
        // Fast path: non-Nil values are always valid — skip env lookup
        if matches!(val, Value::Nil) {
            if let Some(shared_val) = self.get_shared_var(&name) {
                self.stack.push(shared_val);
                return Ok(());
            }
            let is_internal = name.starts_with("__");
            let is_special = matches!(name.as_str(), "_" | "/" | "!" | "¢");
            // Private attribute locals (!attr) are populated directly from
            // instance attributes in fast-path method calls; they may not be
            // in the env (when skip_env_setup is active) but are still valid.
            let is_private_attr =
                name.starts_with('!') && name.len() > 1 && !name.starts_with("__");
            if !is_internal && !is_special && !is_private_attr && !self.env().contains_key(&name) {
                return Err(RuntimeError::new(format!(
                    "X::Undeclared::Symbols: Variable '{name}' is not declared"
                )));
            }
            // `is default(...)`: return the default value instead of Nil.
            if let Some(def) = self.var_default(&name) {
                self.stack.push(def.clone());
                return Ok(());
            }
            if let Some(constraint) = self.var_type_constraint_fast(&name).cloned() {
                let nominal = loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                // Nil type constraint: the type object for Nil is Value::Nil itself,
                // not Value::Package("Nil").
                if nominal == "Nil" {
                    self.stack.push(Value::Nil);
                } else {
                    self.stack.push(Value::Package(Symbol::intern(&nominal)));
                }
                return Ok(());
            }
        }
        self.stack.push(val);
        Ok(())
    }

    /// Box the just-declared scalar local at `idx` into a shared `ContainerRef`
    /// cell placed in BOTH the slot and the env entry, so a directly-nested named
    /// sub that writes the lexical by name (via `SetGlobal` through env) and the
    /// owner that reads it by slot observe one cell — enabling cross-call
    /// accumulation without the `env_dirty` blanket reconcile. The skips mirror
    /// `box_captured_lexicals` exactly: scalars only (`@`/`%`/`&` share already),
    /// never an already-shared cell or a reference/identity-bearing value, and
    /// never a type/`where`-constrained scalar (whose every mutation must re-flow
    /// through the assignment chokepoint, which a cell write-through bypasses).
    fn box_decl_local_cell(&mut self, code: &CompiledCode, idx: usize) {
        let name = &code.locals[idx];
        if name.starts_with('&') {
            return;
        }
        // `@`/`%` containers captured-and-mutated in place by a nested named sub
        // (e.g. a user `trait_mod:<is>` pushing to an outer `@names`) are boxed as
        // a whole-container cell so the sub's by-name mutation and the owner's
        // by-name read alias one cell (docs/captured-outer-cell-sharing.md §7.2).
        if name.starts_with('@') || name.starts_with('%') {
            self.box_decl_local_container_cell(code, idx);
            return;
        }
        if self.locals[idx].is_container_ref() {
            return;
        }
        if matches!(
            self.locals[idx],
            Value::Package(_)
                | Value::Array(..)
                | Value::Hash(..)
                | Value::Sub(..)
                | Value::Instance { .. }
                | Value::Proxy { .. }
        ) {
            return;
        }
        if loan_env!(self, var_type_constraint(name)).is_some()
            || loan_env!(self, var_type_constraint(name.trim_start_matches('$'))).is_some()
        {
            return;
        }
        let container = self.locals[idx].clone().into_container_ref();
        self.locals[idx] = container.clone();
        let nm = code.locals[idx].clone();
        self.env_mut().insert(nm.clone(), container.clone());
        // Track C: keep a running thread's shared snapshot pointing at the cell
        // (mirrors box_captured_lexicals).
        if self.shared_vars_active {
            loan_env!(self, set_shared_var(&nm, container.clone()));
        }
    }

    /// Box a just-declared `@`/`%` container local into a shared `ContainerRef`
    /// cell placed in BOTH the slot and the env entry (the array/hash `:=` cell
    /// shape), so a nested named sub that mutates the container by name and the
    /// owner that reads it by name observe one cell. The mutating-method and
    /// element-assign write-back paths already descend through the cell
    /// (`try_native_array_mut` / `try_native_hash_mut_bound` / `env_root_descended_mut`),
    /// and `GetArrayVar`/`GetHashVar` `into_deref()` the cell on read.
    fn box_decl_local_container_cell(&mut self, code: &CompiledCode, idx: usize) {
        if self.locals[idx].is_container_ref() {
            return;
        }
        if !matches!(self.locals[idx], Value::Array(..) | Value::Hash(..)) {
            return;
        }
        let name = code.locals[idx].clone();
        // Typed containers must keep flowing through the assignment chokepoint.
        if loan_env!(self, var_type_constraint(&name)).is_some()
            || loan_env!(
                self,
                var_type_constraint(name.trim_start_matches(['@', '%']))
            )
            .is_some()
        {
            return;
        }
        let container = self.locals[idx].clone().into_container_ref();
        self.locals[idx] = container.clone();
        self.env_mut().insert(name.clone(), container.clone());
        if self.shared_vars_active {
            loan_env!(self, set_shared_var(&name, container.clone()));
        }
    }

    pub(super) fn exec_set_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        // Named-sub declaration boxing decision: a scalar local that a
        // directly-nested *named sub* writes (`needs_cell_named_sub`) becomes a
        // shared `ContainerRef` cell so the named sub's by-name env writes and the
        // owner's slot reads alias one cell, carrying cross-call accumulation
        // (`via(); via()`) through the cell instead of the `env_dirty` blanket
        // reconcile (see docs/captured-outer-cell-sharing.md).
        //
        // Boxing and the blanket reconcile are MUTUALLY EXCLUSIVE coherence
        // mechanisms — exactly one is active. Gating boxing on
        // `blanket_reconcile_disabled()` keeps the DEFAULT (shipping) build
        // byte-identical to before (reconcile carries coherence, no boxing), while
        // the `MUTSU_NO_BLANKET_RECONCILE` build activates boxing. This is what lets
        // the slice land safely: with the reconcile on, cell-sharing would
        // propagate a captured-outer write that the reconcile happens to drop
        // through some carriers (e.g. `lives-ok { ... }`), and that extra
        // propagation surfaces unrelated latent bugs (a missing `&foo = &foo`
        // self-reference type-check, etc.). Those are fixed as the surface is
        // migrated; until then boxing is exercised only under the toggle, where the
        // pins prove it carries the named-sub accumulation surface.
        //
        // `vardecl_context` is consumed inside the inner handler, so snapshot it
        // here. Gated on a non-empty `needs_cell_named_sub`, so ordinary code (e.g.
        // `fib`) pays nothing. Such a captured local is `needs_env_sync`
        // (non-simple), so the boxing must wrap BOTH the fast and slow inner paths
        // — hence it lives here, not inside one branch. Deliberately keyed on
        // `needs_cell_named_sub`, NOT the closure-driven `needs_cell_locals`:
        // closures are boxed precisely at their creation op, so reusing that set
        // here would over-box unrelated same-named locals (same-named `my` locals
        // share one slot) and break e.g. `let`-restore in a sibling block.
        let box_decl = self.vardecl_context && !code.needs_cell_named_sub.is_empty();
        let r = self.exec_set_local_op_inner(code, idx);
        // Phase 3 Stage 2: write-through scalar attribute writes to the cell.
        if r.is_ok() {
            self.mirror_attr_local_to_cell(code, idx as usize);
            if box_decl
                && let Some(sym) = code.locals_sym.get(idx as usize)
                && code.needs_cell_named_sub.contains(sym)
            {
                self.box_decl_local_cell(code, idx as usize);
            }
        }
        r
    }

    fn exec_set_local_op_inner(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let idx = idx as usize;
        let raw_popped = self.stack.pop().unwrap_or(Value::Nil);
        // Check if we're trying to write to a private instance attribute (!attr)
        // when self is a type object (not an instance). Raku says this should die.
        let local_name = &code.locals[idx];
        if local_name.starts_with('!')
            && local_name.len() > 1
            && let Some(self_val) = self.get_env_with_main_alias("self")
            && !matches!(self_val, Value::Instance { .. } | Value::Mixin(..))
        {
            // self is a type object - determine class name for error message
            let class_name = match &self_val {
                Value::Package(sym) => sym.to_string(),
                other => crate::value::what_type_name(other),
            };
            return Err(RuntimeError::new(format!(
                "Cannot look up attributes in a {} type object. Did you forget a '.new'?",
                class_name
            )));
        }
        let (raw_popped, bind_source) = Self::extract_varref_binding(raw_popped);
        let is_bind = self.bind_context || bind_source.is_some();
        let is_rebind = self.rebind_context;
        let is_constant = self.constant_context;
        let has_explicit_initializer = self.explicit_initializer_context;
        let is_vardecl = self.vardecl_context;
        let scalar_bind = self.scalar_bind_context;
        let array_share = self.array_share_context;
        self.bind_context = false;
        self.scalar_bind_context = false;
        self.rebind_context = false;
        self.constant_context = false;
        self.array_share_context = false;
        self.explicit_initializer_context = false;
        self.vardecl_context = false;
        // Slice 2a/2b: `$scalar = @arr` / `$scalar = %hash` / chained `$r = $q`
        // promotes the source container to a shared `ContainerRef` cell (raku
        // reference semantics). Handled before the decont marker and fast/slow
        // split because the scalar is itemized (NOT a `:=` decont alias) and needs
        // replace-on-reassign semantics distinct from `:=` write-through. Only
        // shares when the source value deref's to an Array/Hash, so a plain
        // `$x = $y` (scalar source) stays a copy.
        let array_share_source = self.array_share_source.take();
        if array_share
            && let Some(src) = array_share_source
            && raw_popped.with_deref(|v| matches!(v, Value::Array(..) | Value::Hash(_)))
        {
            let name = &code.locals[idx];
            if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
                return self.array_share_assign(code, idx, raw_popped, src);
            }
        }
        // Record/clear the decontainerize marker for `$` scalars. A scalar bound
        // (`:=`) to a Positional is not a Scalar container, so `@a = $bound` must
        // flatten (the `ItemizeVar` opcode reads this marker). Plain assignment
        // re-containerizes, clearing any stale marker. Done before the fast/slow
        // split so both paths are covered. `raw_popped` is only borrowed.
        {
            let name = &code.locals[idx];
            self.update_bound_decont_marker(name, scalar_bind || is_bind, &raw_popped);
        }
        // A redeclaration (`my @a` in a new scope) must not inherit the
        // `is default(...)` trait from an earlier same-named variable,
        // since var_defaults is keyed only by name. Drop any stale entry;
        // if the current decl has its own `is default(...)` trait, the
        // trait op will re-set it immediately after.
        if is_vardecl {
            let name = &code.locals[idx];
            self.clear_var_default(name);
            // Clear the deleted-index tracker left over from a previous
            // same-named variable in an outer scope.
            let deleted_key = format!("__mutsu_deleted_index::{}", name);
            self.env_mut().remove(&deleted_key);
            // Clear any sigilless-readonly flag inherited from an outer
            // scope (e.g. a for-loop `\result` shouldn't block `my $result`
            // in a called sub).
            let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
            self.env_mut().remove(&readonly_key);
            // Clear any readonly-parameter flag inherited from a caller/outer
            // scope. `readonly_vars` is keyed by bare name and is NOT cleared on
            // function entry, so a caller's readonly param (`sub f(Str $x){...}`)
            // would otherwise make a callee's freshly-declared `my $x` readonly
            // ("Cannot assign to a readonly variable (x)"). A plain `my $x = ...`
            // always creates a new writable binding (a readonly trait, if any, is
            // re-applied by the trait op that follows). EXCLUDE `:=` binds: a
            // literal-bound scalar (`my $y := 5`) is genuinely readonly and that
            // marking is set as part of the bind — unmarking it here would let a
            // subsequent `$y = 6` slip through. Strip the sigil to match the bare
            // key form used by check_readonly_for_modify.
            if !is_bind && !scalar_bind {
                let bare = name.trim_start_matches(['$', '@', '%', '&']);
                self.unmark_readonly(bare);
            }
            // Replace stale ContainerRef in env with Nil so a new `my $var`
            // doesn't inherit a binding from an earlier scope. Keep the key
            // so saved frame propagation can still find it.
            if matches!(self.env().get(name), Some(Value::ContainerRef(_))) {
                self.env_mut().insert(name.to_string(), Value::Nil);
            }
            // Per-iteration freshness for box-on-capture (lever C Slice 2): if a
            // previous iteration's closure boxed this loop-body `my` into a
            // ContainerRef (now sitting in the slot), the redeclaration is a
            // *fresh binding* — clear the stale cell so the assignment below
            // writes a new plain value instead of writing *through* the old Arc
            // (which would corrupt the prior iteration's captured closure).
            if matches!(self.locals[idx], Value::ContainerRef(_)) {
                self.locals[idx] = Value::Nil;
            }
        }

        // Lazily convert pending alias bind names into local_bind_pairs.
        self.resolve_pending_alias_binds(code);

        // Fast path for simple scalar variables — skip all metadata checks
        if code.simple_locals[idx] && bind_source.is_none() && !is_bind {
            let mut val = raw_popped;
            let name = &code.locals[idx];
            // Lazy sync: if env has a ContainerRef for this variable but the
            // local doesn't, update the local from env. This handles the case
            // where a cross-scope binding was created during a method call and
            // the ContainerRef was propagated to saved_env but not saved_locals.
            if !is_rebind
                && !self.locals[idx].is_container_ref()
                && !is_vardecl
                && let Some(Value::ContainerRef(arc)) = self.env().get(name).cloned()
            {
                self.locals[idx] = Value::ContainerRef(arc);
            }
            // Write through ContainerRef: update inner value without breaking sharing
            if !is_rebind && let Value::ContainerRef(arc) = &self.locals[idx] {
                // Slice 2a: a `=`-array-shared scalar (`$n = @z`) reassigned as a
                // whole (`$n = 5`, `$n = @other` via a fresh share) REPLACES the
                // slot — raku value semantics — instead of writing through the
                // cell shared with the source. Drop the share marker and fall
                // through to the plain-replace path below. (`:=`-bound scalars and
                // `@`/`%` vars keep write-through.)
                let scalar = !name.starts_with('@') && !name.starts_with('%');
                if scalar && self.array_share_active && self.is_array_share_scalar(name) {
                    self.clear_array_share_marker(name);
                } else {
                    let arc = arc.clone();
                    if scalar {
                        val = Self::normalize_scalar_assignment_value(val);
                    }
                    arc.lock().unwrap().clone_from(&val);
                    self.flush_local_to_env(code, idx);
                    return Ok(());
                }
            }
            // If the current value is a Proxy, invoke STORE instead of overwriting
            if let Value::Proxy { storer, .. } = &self.locals[idx]
                && !matches!(storer.as_ref(), Value::Nil)
            {
                let proxy_val = self.locals[idx].clone();
                loan_env!(self, assign_proxy_lvalue(proxy_val, val))?;
                // A Proxy STORE (e.g. `$r := substr-rw($str, ...); $r = ...`) mutates
                // the referent caller lexical (`$str`) by name in env via the STORE
                // closure. For the substr-rw/subbuf-rw/undefine Proxies the STORE
                // records the referent on the retain-on-miss writeback list
                // (`record_caller_var_writeback`); `apply_pending_rw_writeback` drains
                // it precisely.
                self.apply_pending_rw_writeback(code);
                return Ok(());
            }
            // First write through a missing-key `:=` bind (a local holding a
            // HashEntryRef): materialize the path into a
            // shared `ContainerRef` cell so the bound var and the hash entry
            // alias bidirectionally (phantom-entry; replaces the old plain-value
            // materialization that lost the alias).
            if !is_rebind && matches!(self.locals[idx], Value::HashEntryRef { .. }) {
                self.materialize_bound_slot_to_cell(code, idx, val);
                return Ok(());
            }
            if !name.starts_with('@') && !name.starts_with('%') {
                val = Self::normalize_scalar_assignment_value(val);
            }
            if matches!(val, Value::Nil)
                && let Some(def) = self.var_default(name)
            {
                val = def.clone();
            }
            if let Some(constraint) = self.var_type_constraint_fast(name).cloned() {
                if matches!(val, Value::Nil) && self.is_definite_constraint(&constraint) {
                    if has_explicit_initializer {
                        return Err(runtime::utils::type_check_assignment_typed_error(
                            name,
                            &constraint,
                            &val,
                        ));
                    }
                    return Err(RuntimeError::new(format!(
                        "X::Syntax::Variable::MissingInitializer: Variable definition of type {} needs to be given an initializer",
                        constraint
                    )));
                }
                if !matches!(val, Value::Nil) && !self.type_matches_value(&constraint, &val) {
                    return Err(runtime::utils::type_check_assignment_typed_error(
                        name,
                        &constraint,
                        &val,
                    ));
                }
                let val = if !matches!(val, Value::Nil) {
                    loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?
                } else {
                    val
                };
                // Wrap native integer values on assignment (overflow wrapping)
                let val = Self::wrap_native_int_by_constraint(&constraint, val)?;
                self.locals[idx] = val;
            } else {
                self.locals[idx] = val;
            }
            // Track lazy-thunk readonly: mark when storing a LazyThunk,
            // unmark when overwriting a LazyThunk with a non-LazyThunk (rebinding).
            if matches!(self.locals[idx], Value::LazyThunk(..)) {
                self.mark_readonly(name);
            }
            if self.fatal_mode
                && !name.contains("__mutsu_")
                && let Some(err) = self.failure_to_runtime_error_if_unhandled(&self.locals[idx])
            {
                return Err(err);
            }
            // Update env when shared_vars is active; otherwise write through to env.
            if self.shared_vars_active {
                loan_env!(self, set_shared_var(name, self.locals[idx].clone()));
            } else {
                self.flush_local_to_env(code, idx);
            }
            // When rebinding (`$x := expr`), remove old bind pairs and reverse aliases.
            if is_rebind {
                self.local_bind_pairs.retain(|&(source, _)| source != idx);
                let mut aliases_to_remove = Vec::new();
                let prefix = "__mutsu_sigilless_alias::";
                for (k, v) in self.env().iter() {
                    if let Some(_var_name) = k.strip_prefix_str(prefix)
                        && let Value::Str(target) = v
                        && target.as_str() == name
                    {
                        aliases_to_remove.push(*k);
                    }
                }
                for k in aliases_to_remove {
                    self.env_mut().remove_sym(k);
                }
            }
            // Propagate value to variables bound to this one via `:=` binding.
            // Uses local_bind_pairs recorded at binding time to avoid
            // cross-scope name collisions.
            {
                let new_val = self.locals[idx].clone();
                for &(source, target) in &self.local_bind_pairs {
                    if source == idx {
                        self.locals[target] = new_val.clone();
                    }
                }
            }
            // Propagate to env-based alias targets (for `:=` bindings where
            // the source is an env variable like `$_`).
            {
                let alias_key = format!("__mutsu_sigilless_alias::{}", name);
                if let Some(Value::Str(target)) = self.env().get(&alias_key).cloned() {
                    let is_co_local = code.locals.iter().any(|n| n == target.as_str());
                    if !is_co_local {
                        {
                            let __v = self.locals[idx].clone();
                            self.env_mut().insert(target.to_string(), __v);
                        }
                    }
                }
            }
            // Track topic mutations for map rw writeback
            if name == "_" {
                let topic = self.locals[idx].clone();
                self.env_mut()
                    .insert("__mutsu_rw_map_topic__".to_string(), topic);
            }
            self.flush_local_to_env(code, idx);
            return Ok(());
        }

        let name = &code.locals[idx];
        // Capture the old hash Arc before assignment for circular reference fixup.
        let old_hash_arc = if name.starts_with('%') {
            if let Value::Hash(arc) = &self.locals[idx] {
                Some(Arc::as_ptr(arc) as usize)
            } else {
                None
            }
        } else {
            None
        };
        // Capture the old array Arc before assignment for circular reference fixup.
        let old_array_arc = if name.starts_with('@') {
            if let Value::Array(arc, _) = &self.locals[idx] {
                Some(Arc::as_ptr(arc) as usize)
            } else {
                None
            }
        } else {
            None
        };
        let mut val = if name.starts_with('%') {
            if has_explicit_initializer
                && !is_constant
                && !is_bind
                && matches!(raw_popped, Value::Nil)
                && let Some(constraint) = loan_env!(self, var_type_constraint(name))
            {
                return Err(runtime::utils::type_check_assignment_typed_error(
                    name,
                    &constraint,
                    &Value::Nil,
                ));
            }
            // Prevent re-initialization of immutable containers (Mix, Set, Bag)
            if !is_vardecl
                && !is_bind
                && matches!(
                    &self.locals[idx],
                    Value::Mix(_, false) | Value::Set(_, false) | Value::Bag(_, false)
                )
            {
                let type_name = match &self.locals[idx] {
                    Value::Mix(..) => "Mix",
                    Value::Set(..) => "Set",
                    Value::Bag(..) => "Bag",
                    _ => unreachable!(),
                };
                return Err(RuntimeError::assignment_ro_typename(
                    type_name,
                    &self.locals[idx].to_string_value(),
                ));
            }
            if is_bind {
                // `:=` binding preserves containers — skip coercion. But the
                // bound container must conform to a typed-hash variable's
                // declared value type: `my Int %h := <untyped hash>` dies
                // because the RHS is `Hash[Any,Any]`, not `Associative[Int]`.
                self.check_hash_bind_value_type(name, &raw_popped)?;
                raw_popped
            } else if is_constant {
                // `constant %x` preserves Associative containers (Hash, Bag, Set, Mix, Pair).
                // Non-Associative values (Lists) are coerced to Map.
                // Non-Associative Instance objects get .Map called for coercion.
                self.coerce_constant_hash_value(name, raw_popped)?
            } else {
                self.coerce_hash_var_value(name, raw_popped)?
            }
        } else if name.starts_with('@') {
            if has_explicit_initializer
                && !is_constant
                && !is_bind
                && matches!(raw_popped, Value::Nil)
                && let Some(constraint) = loan_env!(self, var_type_constraint(name))
            {
                return Err(runtime::utils::type_check_assignment_typed_error(
                    name,
                    &constraint,
                    &Value::Nil,
                ));
            }
            // Native typed arrays cannot store lazy sequences — check before
            // eager evaluation so the error is raised even if the sequence is
            // infinite.
            if let Some(constraint) = loan_env!(self, var_type_constraint(name))
                && crate::runtime::native_types::is_native_array_element_type(&constraint)
            {
                let is_lazy_value = match &raw_popped {
                    Value::Array(_, kind) => kind.is_lazy(),
                    Value::LazyList(_) | Value::LazyIoLines { .. } => true,
                    _ => false,
                };
                if is_lazy_value {
                    let declared = format!("array[{}]", constraint);
                    return Err(RuntimeError::typed(
                        "X::Cannot::Lazy",
                        [
                            (
                                "message".to_string(),
                                Value::str(format!("Cannot store a lazy list onto a {}", declared)),
                            ),
                            ("action".to_string(), Value::str_from("store")),
                        ]
                        .into_iter()
                        .collect(),
                    ));
                }
            }
            let mut assigned = if is_constant {
                // `constant @x` stores a List, not an Array.
                // Explicit Arrays ([1,2,3]) are preserved.
                // Instance objects that do Positional are kept as-is.
                // Non-Positional objects have .cache called for coercion.
                match raw_popped {
                    Value::Array(items, kind) if kind.is_real_array() => Value::Array(items, kind),
                    Value::Array(items, _) => Value::Array(items, crate::value::ArrayKind::List),
                    Value::Seq(items) => Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(items.to_vec())),
                        crate::value::ArrayKind::List,
                    ),
                    Value::LazyList(list) => {
                        let items = self.force_lazy_list_vm(&list)?;
                        Value::Array(
                            std::sync::Arc::new(crate::value::ArrayData::new(items)),
                            crate::value::ArrayKind::List,
                        )
                    }
                    Value::LazyIoLines { .. } => {
                        let forced = self.force_if_lazy_io_lines(raw_popped)?;
                        let items = runtime::value_to_list(&forced);
                        Value::Array(
                            std::sync::Arc::new(crate::value::ArrayData::new(items)),
                            crate::value::ArrayKind::List,
                        )
                    }
                    Value::Instance { ref class_name, .. } => {
                        // Check if Instance does Positional — if so, keep as-is.
                        let cn = class_name.resolve();
                        let does_positional = matches!(
                            cn.as_str(),
                            "Array"
                                | "List"
                                | "Slip"
                                | "Seq"
                                | "Range"
                                | "Buf"
                                | "Blob"
                                | "utf8"
                                | "buf8"
                                | "buf16"
                                | "buf32"
                        ) || self
                            .class_composed_roles(&cn)
                            .is_some_and(|roles| roles.iter().any(|r| r == "Positional"));
                        if does_positional {
                            raw_popped
                        } else {
                            // Call .cache on non-Positional to coerce.
                            // Skip native methods so user-defined .cache is called.
                            let cached = self.call_method_all_with_fallback(
                                &raw_popped,
                                "cache",
                                &[],
                                true,
                            )?;
                            let cached_val = cached.into_iter().next().unwrap_or(Value::Nil);
                            // Check that .cache returned a Positional
                            let is_pos = matches!(
                                &cached_val,
                                Value::Array(..)
                                    | Value::Seq(_)
                                    | Value::Slip(_)
                                    | Value::LazyList(_)
                                    | Value::LazyIoLines { .. }
                            );
                            if !is_pos {
                                let got_type = crate::runtime::utils::value_type_name(&cached_val);
                                let mut attrs = std::collections::HashMap::new();
                                attrs.insert("got".to_string(), cached_val);
                                attrs.insert(
                                    "expected".to_string(),
                                    Value::Package(crate::symbol::Symbol::intern("Positional")),
                                );
                                attrs.insert(
                                    "message".to_string(),
                                    Value::str(format!(
                                        "Type check failed in assignment to {}; expected Positional but got {}",
                                        name, got_type
                                    )),
                                );
                                let ex = Value::make_instance(
                                    crate::symbol::Symbol::intern("X::TypeCheck"),
                                    attrs,
                                );
                                let mut err = RuntimeError::new(format!(
                                    "Type check failed in assignment to {}; expected Positional but got {}",
                                    name, got_type
                                ));
                                err.exception = Some(Box::new(ex));
                                return Err(err);
                            }
                            // Coerce cached result to List
                            match cached_val {
                                Value::Array(items, _) => {
                                    Value::Array(items, crate::value::ArrayKind::List)
                                }
                                Value::Seq(items) => Value::Array(
                                    std::sync::Arc::new(crate::value::ArrayData::new(
                                        items.to_vec(),
                                    )),
                                    crate::value::ArrayKind::List,
                                ),
                                other => Value::Array(
                                    std::sync::Arc::new(crate::value::ArrayData::new(vec![other])),
                                    crate::value::ArrayKind::List,
                                ),
                            }
                        }
                    }
                    other => Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(vec![other])),
                        crate::value::ArrayKind::List,
                    ),
                }
            } else if is_bind {
                // `:=` binding preserves the container type (e.g. List stays List).
                // Type-check: only Positional values can be bound to @-sigiled vars.
                let is_positional = match &raw_popped {
                    Value::Array(..)
                    | Value::LazyList(_)
                    | Value::LazyIoLines { .. }
                    | Value::Seq(_)
                    | Value::Slip(_)
                    | Value::Range(..)
                    | Value::RangeExcl(..)
                    | Value::RangeExclStart(..)
                    | Value::RangeExclBoth(..)
                    | Value::GenericRange { .. }
                    | Value::Uni { .. }
                    | Value::Nil => true,
                    // Instance objects are Positional only if they implement
                    // the Positional role (or Array subclass etc.), but not
                    // Failure or arbitrary classes.
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } => {
                        let cn = class_name.resolve();
                        matches!(
                            cn.as_str(),
                            "Array"
                                | "List"
                                | "Slip"
                                | "Seq"
                                | "Range"
                                | "Buf"
                                | "Blob"
                                | "utf8"
                                | "buf8"
                                | "buf16"
                                | "buf32"
                        ) || self
                            .class_composed_roles(&cn)
                            .is_some_and(|roles| roles.iter().any(|r| r == "Positional"))
                            || attributes.contains_key("__mutsu_array_storage")
                    }
                    _ => false,
                };
                if !is_positional {
                    let got_type = crate::runtime::utils::value_type_name(&raw_popped);
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("got".to_string(), raw_popped.clone());
                    attrs.insert(
                        "expected".to_string(),
                        Value::Package(crate::symbol::Symbol::intern("Positional")),
                    );
                    attrs.insert("symbol".to_string(), Value::str(name.clone()));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Type check failed in binding; expected Positional but got {}",
                            got_type
                        )),
                    );
                    let ex = Value::make_instance(
                        crate::symbol::Symbol::intern("X::TypeCheck::Binding"),
                        attrs,
                    );
                    let mut err = RuntimeError::new(format!(
                        "Type check failed in binding; expected Positional but got {}",
                        got_type
                    ));
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                match raw_popped {
                    Value::LazyList(ref list) if list.coroutine.is_some() => {
                        // Gather-based lazy list with coroutine: preserve laziness,
                        // tagging it as living in `@` array context so gist/`.WHAT`
                        // render `[...]`/`Array` rather than `(...)`/`Seq`.
                        Value::LazyList(std::sync::Arc::new(list.with_array_context()))
                    }
                    Value::LazyList(list) => Value::real_array(self.force_lazy_list_vm(&list)?),
                    Value::LazyIoLines { .. } => {
                        let forced = self.force_if_lazy_io_lines(raw_popped)?;
                        Value::real_array(runtime::value_to_list(&forced))
                    }
                    other => other,
                }
            } else {
                match raw_popped {
                    Value::LazyList(list) => {
                        match list.env.get(Self::LAZY_ASSIGN_PRESERVE_MARKER) {
                            // Preserved into an `@` array: keep it lazy but tag
                            // array context so gist/`.WHAT` render `[...]`/`Array`.
                            Some(Value::Bool(true)) => {
                                Value::LazyList(std::sync::Arc::new(list.with_array_context()))
                            }
                            _ => Value::real_array(self.force_lazy_list_vm(&list)?),
                        }
                    }
                    Value::LazyIoLines { .. } => {
                        let forced = self.force_if_lazy_io_lines(raw_popped)?;
                        runtime::coerce_to_array(forced)
                    }
                    // An infinite integer range (`1..*`) stays a reify LazyList
                    // instead of being capped to a 100k `ArrayKind::Lazy` Array. (L2)
                    other if runtime::utils::infinite_int_range_to_lazy_array(&other).is_some() => {
                        runtime::utils::infinite_int_range_to_lazy_array(&other).unwrap()
                    }
                    other => {
                        // Resolve bound-element sentinels before coercing to
                        // array.  Assignment (not binding) creates new
                        // containers, so bound refs must be snapshotted.
                        let other = self.resolve_bound_array_elements(other);
                        runtime::coerce_to_array(other)
                    }
                }
            };
            // Preserve shaped array property on re-assignment, but only if the
            // new value is NOT already a shaped array (e.g. from Array.new(:shape(...)))
            let assigned_has_own_shape = crate::runtime::utils::shaped_array_shape(&assigned)
                .is_some()
                || matches!(&assigned, Value::Array(_, crate::value::ArrayKind::Shaped));
            if let Some(shape) = crate::runtime::utils::shaped_array_shape(&self.locals[idx])
                && shape.len() == 1
                && !assigned_has_own_shape
            {
                let items = runtime::value_to_list(&assigned);
                let item_count = items.len();
                let mut shaped_items: Vec<Value> = items.into_iter().take(shape[0]).collect();
                if item_count < shape[0] {
                    shaped_items.resize(shape[0], Value::Nil);
                }
                assigned = Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(shaped_items)),
                    crate::value::ArrayKind::Shaped,
                );
                crate::runtime::utils::mark_shaped_array(&assigned, Some(&shape));
                // Preserve container type metadata
                if let Some(info) = self.container_type_metadata(&self.locals[idx]) {
                    assigned = self.tag_container_metadata(assigned, info);
                }
            }
            let class_name = match &self.locals[idx] {
                Value::Instance { class_name, .. } => Some(*class_name),
                Value::Package(class_name) => Some(*class_name),
                _ => None,
            };
            // When the old local slot holds a Buf/Blob (e.g. from a
            // previous loop iteration or redeclaration), VarDecl (`my @a`)
            // must be allowed to overwrite it. Plain assignments (`@a = ...`)
            // also come through SetLocal; for those, Blob is immutable and
            // Buf coerces through Buf.new to preserve the container type.
            if let Some(class_name) = class_name
                && !is_vardecl
            {
                let class = class_name.resolve();
                if class == "Blob" || class.starts_with("blob") || class.starts_with("Blob[") {
                    return Err(RuntimeError::assignment_ro(None));
                }
                if class == "Buf" || class.starts_with("buf") || class.starts_with("Buf[") {
                    let items = runtime::value_to_list(&assigned)
                        .into_iter()
                        .map(|v| Value::Int(runtime::to_int(&v)))
                        .collect::<Vec<_>>();
                    assigned = self.try_compiled_method_or_interpret(
                        Value::Package(class_name),
                        "new",
                        items,
                    )?;
                }
            }
            assigned
        } else {
            Self::normalize_scalar_assignment_value(raw_popped)
        };
        if matches!(val, Value::Nil)
            && !matches!(self.locals[idx], Value::Nil)
            && let Some(def) = self.var_default(name)
        {
            val = def.clone();
        }
        // For array variables with `is default(X)`, replace Nil elements
        // with the default value (Raku container semantics).
        if name.starts_with('@')
            && let Some(def) = self.var_default(name).cloned()
            && let Value::Array(ref items, kind) = val
        {
            let is_hole =
                |v: &Value| matches!(v, Value::Nil) || matches!(v, Value::Package(n) if n == "Any");
            let has_holes = items.iter().any(is_hole);
            if has_holes {
                let replaced: Vec<Value> = items
                    .iter()
                    .map(|v| if is_hole(v) { def.clone() } else { v.clone() })
                    .collect();
                val = Value::Array(Arc::new(crate::value::ArrayData::new(replaced)), kind);
            }
        }
        // Skip typed container coercion for `:=` binding — it would create
        // a new Arc and lose container identity (e.g. Map metadata).
        if !is_bind && (name.starts_with('@') || name.starts_with('%')) {
            val = self.coerce_typed_container_assignment(name, val, has_explicit_initializer)?;
        }
        if let Some(constraint) = loan_env!(self, var_type_constraint(name))
            && !name.starts_with('%')
            && !name.starts_with('@')
        {
            if matches!(val, Value::Nil) && self.is_definite_constraint(&constraint) {
                if has_explicit_initializer {
                    return Err(runtime::utils::type_check_assignment_typed_error(
                        name,
                        &constraint,
                        &val,
                    ));
                }
                return Err(RuntimeError::new(format!(
                    "X::Syntax::Variable::MissingInitializer: Variable definition of type {} needs to be given an initializer",
                    constraint
                )));
            }
            if !matches!(val, Value::Nil) && !self.type_matches_value(&constraint, &val) {
                return Err(runtime::utils::type_check_assignment_typed_error(
                    name,
                    &constraint,
                    &val,
                ));
            }
            if !matches!(val, Value::Nil) {
                val = loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?;
            }
            // Wrap native integer values on assignment (overflow wrapping)
            val = Self::wrap_native_int_by_constraint(&constraint, val)?;
        }
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        // Only enforce sigilless-readonly when this is NOT a new variable
        // declaration (my $x = ...).  A `my` decl creates a fresh variable
        // that shadows the sigilless one, so it must not be blocked.
        if !is_vardecl
            && matches!(self.env().get(&readonly_key), Some(Value::Bool(true)))
            && !matches!(self.env().get(&alias_key), Some(Value::Str(_)))
        {
            return Err(RuntimeError::assignment_ro(None));
        }
        if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
            loan_env!(self, reset_atomic_var_key(name));
        }
        // When rebinding a variable (`$x := expr`), remove any existing
        // bind pairs where this slot was the source.  Rebinding replaces
        // the container, so previously bound targets must stop tracking.
        if is_rebind {
            self.local_bind_pairs.retain(|&(source, _)| source != idx);
            // Also remove env-based aliases that point TO this variable,
            // so GetLocal alias-following doesn't read the new value.
            let mut aliases_to_remove = Vec::new();
            let prefix = "__mutsu_sigilless_alias::";
            for (k, v) in self.env().iter() {
                if let Some(_var_name) = k.strip_prefix_str(prefix)
                    && let Value::Str(target) = v
                    && target.as_str() == name
                {
                    aliases_to_remove.push(*k);
                }
            }
            for k in aliases_to_remove {
                self.env_mut().remove_sym(k);
            }
        }
        if let Some(source_name) = bind_source {
            let resolved_source = self.resolve_sigilless_alias_source_name(&source_name);
            self.env_mut()
                .insert(alias_key.clone(), Value::str(resolved_source.clone()));
            self.env_mut().insert(readonly_key, Value::Bool(false));
            // Create a shared ContainerRef for cross-scope binding (source in
            // outer call frame) OR same-scope rebinding (`:=` on existing vars).
            // ContainerRef ensures bidirectional container sharing: writing to
            // either variable updates both, matching Raku's binding semantics.
            let source_in_outer_frame = self
                .call_frames
                .iter()
                .any(|f| f.saved_env.contains_key(&resolved_source));
            let source_in_same_scope = code.locals.iter().any(|n| n == &resolved_source);
            // `my @a := @$n` deref-bind (Slice 2c): the parser conflates `@$n`
            // (deref of a scalar `$n` that holds an array by reference) with the
            // array variable `@n`. When no `@n`/`%n` container value exists at
            // runtime but a same-named scalar holds a shared `ContainerRef` cell
            // (a value-alias `my $n = @z` — Slice 2a), bind to THAT cell so `@a`
            // shares the very container `$n`/`@z` references, not a fresh copy.
            // Gate on the runtime value of `@n` (not its presence in the
            // function-wide `code.locals`, which a `my @n` in a *sibling* block
            // would spuriously satisfy via `source_in_same_scope`).
            let source_resolves_to_container = matches!(
                self.env().get(&resolved_source),
                Some(Value::Array(..) | Value::Hash(..) | Value::ContainerRef(_))
            );
            let scalar_source: Option<String> = if source_resolves_to_container {
                None
            } else {
                resolved_source
                    .strip_prefix(['@', '%'])
                    .filter(|bare| matches!(self.env().get(bare), Some(Value::ContainerRef(_))))
                    .map(str::to_string)
            };
            let effective_source = scalar_source
                .clone()
                .unwrap_or_else(|| resolved_source.clone());
            // For a deref-bind via a promoted scalar (`my @a := @$n` / `my %h :=
            // %$m`, Slice 2c), the sigilless alias set above points at the
            // sigil'd source (`@n`/`%m`) which has no container value — only the
            // bare scalar (`n`/`m`) holds the shared cell. Redirect the alias to
            // that effective source so a later element write (`@a[0]=`, `%h<k>=`,
            // which resolves the target *through* the alias) reaches the shared
            // cell rather than autovivifying a fresh, detached container.
            if scalar_source.is_some() {
                self.env_mut()
                    .insert(alias_key.clone(), Value::str(effective_source.clone()));
            }
            // Whole-container `:=` bind (`my @b := @a`, `my %h2 := %h`,
            // `my $ref := @a`): share a single `ContainerRef` cell between the
            // source and target so mutations through either alias (push,
            // element assignment) are observed by both, as in Raku. Without
            // this the two slots only share the inner `Arc`, and a COW mutation
            // (`.push`) detaches them. (Mirrors the element-bind cell pattern in
            // the index-assign path; the cell IS the alias so no
            // `local_bind_pairs` entry is recorded.)
            let val_is_container = matches!(
                val,
                Value::Array(..) | Value::Hash(..) | Value::ContainerRef(_)
            );
            if val_is_container
                && (source_in_same_scope || source_in_outer_frame || scalar_source.is_some())
                && !name.starts_with('&')
            {
                // Reuse the source's existing cell if it already has one (so a
                // third alias `my @c := @b` joins the same cell); otherwise wrap
                // the bound value in a fresh cell.
                let cell = match &val {
                    Value::ContainerRef(arc) => arc.clone(),
                    _ => match self.env().get(&effective_source) {
                        Some(Value::ContainerRef(arc)) => arc.clone(),
                        _ => std::sync::Arc::new(std::sync::Mutex::new(val.clone())),
                    },
                };
                // A bound `@`/`%` variable adopts the *source* container's
                // declared element/key type, not its own (`my Int %a; my Cool
                // %b := %a` ⇒ `%b.of` is `Int`). Propagate the inner container's
                // embedded type metadata to this variable's constraint (same as
                // the typed-bind propagation at the end of the slow path, which
                // the early return below skips).
                if name.starts_with('@') || name.starts_with('%') {
                    let inner = cell.lock().unwrap().clone();
                    if let Some(info) = self.container_type_metadata(&inner)
                        && !info.value_type.is_empty()
                    {
                        let constraint_str = if name.starts_with('%') {
                            if let Some(ref kt) = info.key_type {
                                format!("{}{{{}}}", info.value_type, kt)
                            } else {
                                info.value_type.clone()
                            }
                        } else {
                            info.value_type.clone()
                        };
                        self.vm_set_var_type_constraint(name, Some(constraint_str));
                    } else {
                        // Untyped source: clear any declared constraint inherited
                        // from this variable's own declaration so it does not
                        // over-constrain the shared container.
                        self.vm_set_var_type_constraint(name, None);
                    }
                }
                let container = Value::ContainerRef(cell);
                self.locals[idx] = container.clone();
                if let Some(source_idx) = code.locals.iter().rposition(|n| n == &effective_source) {
                    self.locals[source_idx] = container.clone();
                    self.flush_local_to_env(code, source_idx);
                }
                self.set_env_with_main_alias(&effective_source, container.clone());
                // Propagate the shared cell into saved call frames so the
                // binding survives method returns (env restore) without
                // reverting to a stale value (same as the scalar path below).
                for frame in self.call_frames.iter_mut().rev() {
                    if frame.saved_env.contains_key(&effective_source) {
                        frame
                            .saved_env
                            .insert(effective_source.clone(), container.clone());
                    }
                    for (i, local_name) in code.locals.iter().enumerate() {
                        if local_name == &effective_source && i < frame.saved_locals.len() {
                            frame.saved_locals[i] = container.clone();
                        }
                    }
                }
                self.set_env_with_main_alias(name, container.clone());
                self.flush_local_to_env(code, idx);
                return Ok(());
            }
            // Record local-slot binding pair for write propagation.
            // Only record if the source is also a local in the same code unit,
            // so cross-scope name collisions do not cause spurious propagation.
            if is_vardecl
                && let Some(source_idx) = code.locals.iter().position(|n| n == &resolved_source)
                && source_idx != idx
            {
                self.local_bind_pairs.push((source_idx, idx));
            }
            // Only use ContainerRef for same-scope rebind when the value is a
            // simple scalar (not a type object, array, hash, etc.)
            let val_is_simple_scalar = !matches!(
                val,
                Value::Package(_)
                    | Value::Array(..)
                    | Value::Hash(..)
                    | Value::Sub(..)
                    | Value::Instance { .. }
            );
            if (source_in_outer_frame
                || (is_rebind && source_in_same_scope && val_is_simple_scalar))
                && !name.starts_with('@')
                && !name.starts_with('%')
                && !name.starts_with('&')
            {
                let container = if let Value::ContainerRef(ref arc) = val {
                    Value::ContainerRef(arc.clone())
                } else {
                    val.clone().into_container_ref()
                };
                self.locals[idx] = container.clone();
                // Update source in locals if present
                if let Some(source_idx) = code.locals.iter().rposition(|n| n == &resolved_source) {
                    self.locals[source_idx] = container.clone();
                    self.flush_local_to_env(code, source_idx);
                }
                // Update source in env
                self.env_mut()
                    .insert(resolved_source.clone(), container.clone());
                // Propagate ContainerRef to all saved call frame envs AND locals
                // so the binding survives method returns (env restore) and a
                // later restore doesn't overwrite with stale values.
                for frame in self.call_frames.iter_mut().rev() {
                    if frame.saved_env.contains_key(&resolved_source) {
                        frame
                            .saved_env
                            .insert(resolved_source.clone(), container.clone());
                    }
                    for (i, local_name) in code.locals.iter().enumerate() {
                        if local_name == &resolved_source && i < frame.saved_locals.len() {
                            frame.saved_locals[i] = container.clone();
                        }
                    }
                }
                // Propagate ContainerRef to aliased attribute locals (e.g., when
                // binding sigilless `$x`, also update `!x` so attribute writeback picks it up).
                let alias_key_for_target = format!("__mutsu_sigilless_alias::{}", name);
                if let Some(Value::Str(alias_target)) =
                    self.env().get(&alias_key_for_target).cloned()
                    && let Some(alias_idx) =
                        code.locals.iter().rposition(|n| n == alias_target.as_str())
                {
                    self.locals[alias_idx] = container.clone();
                    self.flush_local_to_env(code, alias_idx);
                }
                self.set_env_with_main_alias(name, container.clone());
                // For `our` variables, persist ContainerRef in our_vars so that
                // subsequent method calls (e.g., get_x) see the binding.
                for (slot, qualified_name) in &code.our_locals {
                    if *slot == idx {
                        self.set_our_var(qualified_name.clone(), container.clone());
                        self.set_our_var(name.to_string(), container.clone());
                    }
                }
                self.flush_local_to_env(code, idx);
                return Ok(());
            }
        }
        // Lazy sync: if the local is not a ContainerRef but env has one
        // (from a cross-scope `:=` binding), adopt the ContainerRef so the
        // write-through below preserves shared container identity.
        // Skip for type objects and complex values.
        if !is_bind
            && !is_rebind
            && !self.locals[idx].is_container_ref()
            && !matches!(
                self.locals[idx],
                Value::Package(_)
                    | Value::Array(..)
                    | Value::Hash(..)
                    | Value::Sub(..)
                    | Value::Instance { .. }
            )
            && let Some(Value::ContainerRef(arc)) = self.env().get(name).cloned()
        {
            self.locals[idx] = Value::ContainerRef(arc);
        }
        // Write through ContainerRef in slow path: update inner value
        if !is_bind
            && !is_rebind
            && let Value::ContainerRef(arc) = &self.locals[idx]
        {
            // Slice 2a: a `=`-array-shared scalar reassigned as a whole REPLACES
            // the slot (raku value semantics); drop the share and fall through to
            // the plain-replace path below instead of writing through the cell.
            let scalar = !name.starts_with('@') && !name.starts_with('%');
            if scalar && self.array_share_active && self.is_array_share_scalar(name) {
                self.clear_array_share_marker(name);
            } else {
                let arc = arc.clone();
                if scalar {
                    val = Self::normalize_scalar_assignment_value(val);
                }
                arc.lock().unwrap().clone_from(&val);
                self.flush_local_to_env(code, idx);
                return Ok(());
            }
        }
        // If the current value is a Proxy, invoke STORE instead of overwriting
        if let Value::Proxy { storer, .. } = &self.locals[idx]
            && !matches!(storer.as_ref(), Value::Nil)
        {
            let proxy_val = self.locals[idx].clone();
            loan_env!(self, assign_proxy_lvalue(proxy_val, val))?;
            // A Proxy STORE wrote the referent caller lexical by name; drain the
            // precise retain-on-miss writeback it recorded instead of the blanket
            // env→locals pull.
            self.apply_pending_rw_writeback(code);
            return Ok(());
        }
        // First write through a missing-key `:=` bind: materialize the path into
        // a shared `ContainerRef` cell (phantom-entry; see
        // `materialize_bound_slot_to_cell`).
        if !is_bind && !is_rebind && matches!(self.locals[idx], Value::HashEntryRef { .. }) {
            self.materialize_bound_slot_to_cell(code, idx, val);
            return Ok(());
        }
        // When binding a Proxy to a variable, update FETCH/STORE closures' captured envs
        // so they can reference the Proxy by its binding variable name (simulating capture-by-ref).
        let val = Self::update_proxy_closure_envs(val, name);
        if self.fatal_mode
            && !name.contains("__mutsu_")
            && let Some(err) = self.failure_to_runtime_error_if_unhandled(&val)
        {
            return Err(err);
        }
        self.locals[idx] = val.clone();
        // When assigning (not binding) to an untyped hash/array variable, drop
        // container type metadata still attached to the assigned value. The
        // value may share its backing `Arc` with a typed source container
        // (`my @a = @typed`), and an untyped declaration must not present its
        // value as typed. Skip for attribute variables (.h, !h) which get
        // typed metadata from the class definition, not var_type_constraints.
        if !is_bind
            && (name.starts_with('%') || name.starts_with('@'))
            && !name.contains('.')
            && !name.contains('!')
            && loan_env!(self, var_type_constraint(name)).is_none()
            && self.container_type_metadata(&val).is_some()
        {
            // Clear the embedded container type metadata in place so an
            // untyped variable never reports a typed element/key constraint.
            let cleared = crate::runtime::Interpreter::clear_hash_type_metadata(std::mem::replace(
                &mut self.locals[idx],
                Value::Nil,
            ));
            self.locals[idx] = cleared;
        }
        // When binding a typed hash/array to a variable, propagate the container's
        // type constraints to the variable so that subsequent element assignments
        // are type-checked (e.g. `my %h := Hash[Int].new; %h<a> = "b"` should die).
        if is_bind
            && (name.starts_with('%') || name.starts_with('@'))
            && let Some(info) = self.container_type_metadata(&val)
            && !info.value_type.is_empty()
        {
            // Build the constraint string that set_var_type_constraint expects.
            // For hash variables, parse_container_constraint expects "ValueType{KeyType}"
            // format for key-typed hashes, and plain "ValueType" otherwise.
            let constraint_str = if name.starts_with('%') {
                if let Some(ref kt) = info.key_type {
                    format!("{}{{{}}}", info.value_type, kt)
                } else {
                    info.value_type.clone()
                }
            } else {
                info.value_type.clone()
            };
            self.vm_set_var_type_constraint(name, Some(constraint_str));
        }
        // Circular hash reference fixup: when assigning to a hash variable,
        // if any values in the new hash reference the old hash (captured on the
        // RHS before assignment), replace them with the new hash's Arc to create
        // a true circular reference (matching Raku container semantics).
        if name.starts_with('%') && !is_bind && !is_constant {
            Self::fixup_circular_hash_refs(&mut self.locals[idx], &old_hash_arc);
        }
        // Circular array reference fixup: when assigning to an array variable,
        // if any elements in the new array reference the old array (captured on the
        // RHS before assignment), replace them with the new array's Arc to create
        // a true circular reference (matching Raku container semantics).
        if name.starts_with('@') && !is_bind && !is_constant {
            Self::fixup_circular_array_refs(&mut self.locals[idx], &old_array_arc);
        }
        // Apply the variable's declared element/key type to the stored
        // container. For hashes this embeds the metadata in `HashData` so it
        // travels with the value through later copy-on-write; for arrays it
        // updates the Arc-pointer side table. Done before the value is cloned
        // and propagated to env/aliases below, so every copy carries it.
        if (name.starts_with('@') || name.starts_with('%'))
            && let Some(value_type) = loan_env!(self, var_type_constraint(name))
        {
            let info = crate::runtime::ContainerTypeInfo {
                declared_type: if name.starts_with('@')
                    && crate::runtime::native_types::is_native_array_element_type(&value_type)
                {
                    Some(format!("array[{value_type}]"))
                } else {
                    None
                },
                value_type,
                key_type: if name.starts_with('%') {
                    loan_env!(self, var_hash_key_constraint(name))
                } else {
                    None
                },
            };
            let stored = std::mem::replace(&mut self.locals[idx], Value::Nil);
            self.locals[idx] = self.tag_container_metadata(stored, info);
        }
        // Use the potentially fixed-up value for env/shared_vars.
        let val = self.locals[idx].clone();
        // Mark variable as readonly when storing a LazyThunk
        if matches!(val, Value::LazyThunk(..)) {
            self.mark_readonly(name);
        }
        if (is_bind || is_constant) && name.starts_with('@') {
            // For `:=` bind and `constant @x`, bypass set_shared_var's
            // List->Array normalization so the container type is preserved.
            self.env_mut().insert(name.to_string(), val.clone());
        } else {
            self.set_env_with_main_alias(name, val.clone());
        }
        if let Some(symbol) = Self::term_symbol_from_name(name) {
            self.env_mut().insert(symbol.to_string(), val.clone());
            let pkg = self.current_package().to_string();
            if pkg != "GLOBAL" {
                self.env_mut()
                    .insert(format!("{pkg}::term:<{symbol}>"), val.clone());
            }
        }
        let mut alias_name = self.env().get(&alias_key).and_then(|v| {
            if let Value::Str(name) = v {
                Some(name.to_string())
            } else {
                None
            }
        });
        let mut seen_aliases = std::collections::HashSet::new();
        while let Some(current_alias) = alias_name {
            if !seen_aliases.insert(current_alias.clone()) {
                break;
            }
            self.update_local_if_exists(code, &current_alias, &val);
            self.env_mut().insert(current_alias.clone(), val.clone());
            // Sigilless attribute write: mirror an attr-twigil alias (`!x`) into
            // self's shared cell (no-op for non-attribute aliases). Stage 2c (ii).
            self.write_self_attr_cell(&current_alias, val.clone());
            // Slice F: a sigilless param (`\target`) aliases a caller variable;
            // record it so the call-site drain writes the env value through to the
            // caller's local slot (without relying on the reverse pull).
            self.pending_rw_writeback_sources
                .push(current_alias.clone());
            let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
            alias_name = self.env().get(&next_key).and_then(|v| {
                if let Value::Str(name) = v {
                    Some(name.to_string())
                } else {
                    None
                }
            });
        }
        // Reverse propagation: when writing to a variable that is the source
        // of another variable's `:=` binding, update the bound variable's
        // local slot too.  For example, `my $y := $x; my $x = 3;` must
        // update $y's slot so it reads 3.
        // Uses local_bind_pairs recorded at binding time to avoid
        // cross-scope name collisions.
        for &(source, target) in &self.local_bind_pairs {
            if source == idx {
                self.locals[target] = val.clone();
            }
        }
        if let Some(attr) = name.strip_prefix('.') {
            self.env_mut().insert(format!("!{}", attr), val.clone());
        } else if let Some(attr) = name.strip_prefix('!') {
            self.env_mut().insert(format!(".{}", attr), val.clone());
        }
        if name == "_"
            && let Some(ref source_var) = self.topic_source_var
            && !source_var.starts_with('@')
            && !source_var.starts_with('%')
        {
            let sv = source_var.clone();
            self.set_env_with_main_alias(&sv, val.clone());
            self.update_local_if_exists(code, &sv, &val);
        }
        Ok(())
    }

    pub(super) fn exec_set_var_dynamic_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        dynamic: bool,
    ) {
        let name = Self::const_str(code, name_idx);
        loan_env!(self, set_var_dynamic(name, dynamic));
        // A fresh declaration without an explicit type must not inherit stale
        // constraints from an earlier lexical with the same name.
        self.vm_set_var_type_constraint(name, None);
        if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
            loan_env!(self, reset_atomic_var_key_decl(name));
        }
        // A fresh @-variable declaration must clear any CAS atomic array
        // state left over from a previous lexical with the same name
        // (e.g. when `my @arr[N]` is declared inside a loop body).
        if name.starts_with('@') {
            self.clear_atomic_array_state(name);
        } else if name.starts_with('%') {
            self.clear_atomic_hash_state(name);
        }
        // Before this loop-body-local declaration overwrites the env entry for
        // `name`, record the outer value it shadows so the enclosing same-named
        // binding can be restored when the loop exits. Conditions are excluded
        // (`loop_cond_active`): a `my` in a `while`/`until`/`loop` condition is
        // enclosing-scoped and often read after the loop. Only record names that
        // already had a binding (a genuine shadow — there is nothing to clobber
        // otherwise), and only the first time in this loop scope so the saved
        // value is the pre-loop one. See Interpreter::loop_local_saved_env.
        //
        // Crucially, only record names the compiler scoped to the loop body — i.e.
        // names with a local slot in `code.locals`. A `my` in a *statement
        // modifier* loop (`(my @a).push: $_ for ^3`) introduces no block, so the
        // compiler scopes it to the *enclosing* unit as an env-only var with no
        // local slot; it is enclosing-scoped (read after the loop) and must NOT be
        // restored. Restoring it would wipe an accumulated `0,1,2` back to its
        // first-iteration value. Genuine body-local shadows (`for {...{ my @a }}`)
        // always get a slot, so this distinguishes the two reliably.
        if !self.loop_cond_active
            && let Some(prev) = self.env().get(name).cloned()
        {
            // Only record a *genuine, live* enclosing binding for restoration.
            // The restore writes back both env and the local slot, so the value
            // being shadowed must be a real outer binding that is backed by a
            // local slot whose value is coherent with env. A leaked or freshly
            // hoisted env entry is NOT: e.g. a statement-modifier `(my @a)` whose
            // name collides in the frame-wide `code.locals` with a *popped* sibling
            // block's slot (`{ my @a=... } { (my @a).push: $_ for ^3 }`) leaves
            // env=[] but that slot reset to Nil — env and slot disagree. Recording
            // it would wipe the accumulated `0,1,2` back to the hoisted empty value
            // at loop exit. Requiring slot==env restores only true shadows
            // (`my @a=1,2,3; for { my @a=7,8 }` keeps the outer `1,2,3`) and leaves
            // enclosing-scoped statement-modifier declarations alone.
            // Also skip names this loop already declared in a prior iteration
            // (`loop_local_vars`, populated below). After iteration 1 of an
            // accumulating statement-modifier loop, env and slot agree on the
            // partial result, which would spuriously look coherent; that value is
            // the loop's own, not an enclosing binding.
            let already_loop_local = self
                .loop_local_vars
                .last()
                .is_some_and(|s| s.contains(name));
            let has_coherent_slot = !already_loop_local
                && code
                    .locals
                    .iter()
                    .enumerate()
                    .any(|(i, n)| n.as_str() == name && self.locals.get(i) == Some(&prev));
            if has_coherent_slot
                && let Some(saved) = self.loop_local_saved_env.last_mut()
                && !saved.contains_key(name)
            {
                saved.insert(name.to_string(), prev);
            }
        }
        // Pre-initialize the variable in the env with a default value so that
        // closures created during the RHS expression can capture it.
        // This enables capture-by-reference patterns like:
        //   my $proxy := Proxy.new(STORE => -> $, \v { $proxy.VAR... })
        //
        // Skip &-sigiled variables here: seeding the lexical environment with
        // `&name = Any` before the RHS runs makes `EVAL(q[sub name() { ... }])`
        // look like a routine redeclaration instead of producing a callable to
        // bind into `my &name = ...`.
        if !name.starts_with('&') && !self.env().contains_key(name) {
            let default = if name.starts_with('@') {
                Value::real_array(Vec::new())
            } else if name.starts_with('%') {
                Value::hash(std::collections::HashMap::new())
            } else {
                Value::Package(crate::symbol::Symbol::intern("Any"))
            };
            self.env_mut().insert(name.to_string(), default);
        }
        // Track this variable as declared within the current block scope.
        // BlockScope restoration uses this to avoid propagating block-local
        // variable values back to the outer scope.
        if let Some(set) = self.block_declared_vars.last_mut() {
            set.insert(name.to_string());
        }
        // Track loop-body declarations so a closure created in the body can mark
        // this name as a per-iteration `owned_capture` (see Interpreter::loop_local_vars).
        if let Some(set) = self.loop_local_vars.last_mut() {
            set.insert(name.to_string());
        }
    }

    pub(super) fn exec_assign_expr_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let r = self.exec_assign_expr_local_op_inner(code, idx);
        // Phase 3 Stage 2: write-through scalar attribute writes to the cell.
        if r.is_ok() {
            self.mirror_attr_local_to_cell(code, idx as usize);
        }
        r
    }

    fn exec_assign_expr_local_op_inner(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let idx = idx as usize;
        // Slice 2a/2b: `$scalar = @arr` / chained `$r = $q` reassignment (the
        // VarDecl form goes through `exec_set_local_op`). Promote the source
        // container to a shared `ContainerRef` cell so structural mutation through
        // either name is seen by both (raku reference semantics), then leave the
        // value on the stack (assignment is an expression). No-op when the source
        // does not deref to an Array/Hash (a plain `$x = $y` stays a copy).
        let array_share_source = self.array_share_source.take();
        if self.array_share_context {
            self.array_share_context = false;
            if let Some(src) = array_share_source {
                let val = self.stack.pop().unwrap_or(Value::Nil);
                if val.with_deref(|v| matches!(v, Value::Array(..) | Value::Hash(_))) && {
                    let name = &code.locals[idx];
                    !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&')
                } {
                    self.array_share_assign(code, idx, val.clone(), src)?;
                    self.stack.push(val);
                    return Ok(());
                }
                // Not an array-share (plain scalar source / `@`/`%` target): push back.
                self.stack.push(val);
            }
        }
        // If the current local is a Proxy, invoke STORE instead of overwriting
        if let Value::Proxy { storer, .. } = &self.locals[idx]
            && !matches!(storer.as_ref(), Value::Nil)
        {
            let val = self.stack.pop().unwrap_or(Value::Nil);
            let proxy_val = self.locals[idx].clone();
            loan_env!(self, assign_proxy_lvalue(proxy_val, val))?;
            // A Proxy STORE wrote the referent caller lexical by name. For
            // substr-rw/subbuf-rw/undefine the STORE recorded the referent precisely
            // (`record_caller_var_writeback`); drain it here so the slot refreshes
            // (see the other Proxy-STORE assign site).
            self.apply_pending_rw_writeback(code);
            return Ok(());
        }

        // Fast path for simple scalar variables — skip all metadata checks
        if code.simple_locals[idx] {
            let mut val = self.stack.pop().unwrap_or(Value::Nil);
            let name = &code.locals[idx];
            // Lazy sync: if the local is not a ContainerRef but env has one
            // (from a cross-scope `:=` binding), adopt the ContainerRef and
            // write through it to preserve shared container identity.
            // Skip for type objects and complex values.
            if !self.locals[idx].is_container_ref()
                && !matches!(
                    self.locals[idx],
                    Value::Package(_)
                        | Value::Array(..)
                        | Value::Hash(..)
                        | Value::Sub(..)
                        | Value::Instance { .. }
                )
                && let Some(Value::ContainerRef(arc)) = self.env().get(name).cloned()
            {
                self.locals[idx] = Value::ContainerRef(arc);
            }
            if let Value::ContainerRef(arc) = &self.locals[idx] {
                // Slice 2a: a `=`-array-shared scalar reassigned as a whole
                // REPLACES the slot (raku value semantics); drop the share and
                // fall through to the plain-replace path below.
                let scalar = !name.starts_with('@') && !name.starts_with('%');
                if scalar && self.array_share_active && self.is_array_share_scalar(name) {
                    self.clear_array_share_marker(name);
                } else {
                    let arc = arc.clone();
                    if scalar {
                        val = Self::normalize_scalar_assignment_value(val);
                    }
                    arc.lock().unwrap().clone_from(&val);
                    self.stack.push(val);
                    self.flush_local_to_env(code, idx);
                    return Ok(());
                }
            }
            if !name.starts_with('@') && !name.starts_with('%') {
                val = Self::normalize_scalar_assignment_value(val);
            }
            if matches!(val, Value::Nil)
                && !matches!(self.locals[idx], Value::Nil)
                && let Some(def) = self.var_default(name)
            {
                val = def.clone();
            }
            if let Some(constraint) = self.var_type_constraint_fast(name).cloned() {
                let val = if matches!(val, Value::Nil) {
                    if constraint == "Mu" {
                        val
                    } else {
                        let nominal =
                            loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                        Value::Package(Symbol::intern(&nominal))
                    }
                } else if !self.type_matches_value(&constraint, &val) {
                    return Err(runtime::utils::type_check_assignment_typed_error(
                        name,
                        &constraint,
                        &val,
                    ));
                } else if !matches!(val, Value::Nil | Value::Package(_)) {
                    loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?
                } else {
                    val
                };
                self.locals[idx] = val.clone();
                self.stack.push(val);
            } else {
                self.locals[idx] = val.clone();
                self.stack.push(val);
            }
            if self.fatal_mode
                && !name.contains("__mutsu_")
                && let Some(err) = self.failure_to_runtime_error_if_unhandled(&self.locals[idx])
            {
                return Err(err);
            }
            // Update env when shared_vars is active; otherwise write through to env.
            if self.shared_vars_active {
                loan_env!(self, set_shared_var(name, self.locals[idx].clone()));
            } else {
                self.flush_local_to_env(code, idx);
            }
            // Track topic mutations for map rw writeback
            if name == "_" {
                let topic = self.locals[idx].clone();
                self.env_mut()
                    .insert("__mutsu_rw_map_topic__".to_string(), topic);
            }
            self.flush_local_to_env(code, idx);
            return Ok(());
        }

        let raw_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = &code.locals[idx];
        self.check_readonly_for_modify(name)?;
        let mut val = if name.starts_with('%') {
            self.coerce_hash_var_value(name, raw_val)?
        } else if name.starts_with('@') {
            let mut assigned = match raw_val {
                Value::LazyList(list) => match list.env.get(Self::LAZY_ASSIGN_PRESERVE_MARKER) {
                    Some(Value::Bool(true)) => Value::LazyList(list),
                    _ => Value::real_array(self.force_lazy_list_vm(&list)?),
                },
                other => runtime::coerce_to_array(other),
            };
            let class_name = match &self.locals[idx] {
                Value::Instance { class_name, .. } => Some(*class_name),
                Value::Package(class_name) => Some(*class_name),
                _ => None,
            };
            if let Some(class_name) = class_name {
                let class = class_name.resolve();
                if class == "Blob" || class.starts_with("blob") || class.starts_with("Blob[") {
                    return Err(RuntimeError::assignment_ro(None));
                }
                if class == "Buf" || class.starts_with("buf") || class.starts_with("Buf[") {
                    let items = runtime::value_to_list(&assigned)
                        .into_iter()
                        .map(|v| Value::Int(runtime::to_int(&v)))
                        .collect::<Vec<_>>();
                    assigned = self.try_compiled_method_or_interpret(
                        Value::Package(class_name),
                        "new",
                        items,
                    )?;
                }
            }
            assigned
        } else {
            Self::normalize_scalar_assignment_value(raw_val)
        };
        if matches!(val, Value::Nil)
            && let Some(def) = self.var_default(name)
        {
            val = def.clone();
        }
        if name.starts_with('@') || name.starts_with('%') {
            val = self.coerce_typed_container_assignment(name, val, false)?;
        }
        if let Some(constraint) = loan_env!(self, var_type_constraint(name))
            && !name.starts_with('%')
            && !name.starts_with('@')
        {
            if matches!(val, Value::Nil) {
                if constraint != "Mu" {
                    // Assigning Nil to a typed variable resets it to the type object
                    let nominal =
                        loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                    val = Value::Package(Symbol::intern(&nominal));
                }
            } else if !self.type_matches_value(&constraint, &val) {
                return Err(runtime::utils::type_check_assignment_typed_error(
                    name,
                    &constraint,
                    &val,
                ));
            }
            if !matches!(val, Value::Nil | Value::Package(_)) {
                val = loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?;
            }
        }
        let readonly_key = format!("__mutsu_sigilless_readonly::{}", name);
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        if matches!(self.env().get(&readonly_key), Some(Value::Bool(true)))
            && !matches!(self.env().get(&alias_key), Some(Value::Str(_)))
        {
            return Err(RuntimeError::assignment_ro(None));
        }
        if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
            loan_env!(self, reset_atomic_var_key(name));
        }
        if self.fatal_mode
            && !name.contains("__mutsu_")
            && let Some(err) = self.failure_to_runtime_error_if_unhandled(&val)
        {
            return Err(err);
        }
        // Apply the variable's declared element/key type to the container value
        // before it is stored, so the embedded `HashData` metadata (or array
        // side-table entry) is carried by every copy below.
        if (name.starts_with('@') || name.starts_with('%'))
            && let Some(value_type) = loan_env!(self, var_type_constraint(name))
        {
            let info = crate::runtime::ContainerTypeInfo {
                declared_type: if name.starts_with('@')
                    && crate::runtime::native_types::is_native_array_element_type(&value_type)
                {
                    Some(format!("array[{value_type}]"))
                } else {
                    None
                },
                value_type,
                key_type: if name.starts_with('%') {
                    loan_env!(self, var_hash_key_constraint(name))
                } else {
                    None
                },
            };
            val = self.tag_container_metadata(val, info);
        }
        self.locals[idx] = val.clone();
        self.set_env_with_main_alias(name, val.clone());
        if let Some(alias_name) = self.env().get(&alias_key).and_then(|v| {
            if let Value::Str(name) = v {
                Some(name.to_string())
            } else {
                None
            }
        }) {
            self.update_local_if_exists(code, &alias_name, &val);
            self.env_mut().insert(alias_name.clone(), val.clone());
            // Slice F: a sigilless param (`\target`) aliases a caller variable;
            // the env write above is the only thing that reaches it. Record the
            // alias target so the call-site drain writes it through to the
            // caller's local slot (no-op if it is not a caller local).
            self.pending_rw_writeback_sources.push(alias_name);
        }
        if let Some(attr) = name.strip_prefix('.') {
            self.env_mut().insert(format!("!{}", attr), val.clone());
        } else if let Some(attr) = name.strip_prefix('!') {
            self.env_mut().insert(format!(".{}", attr), val.clone());
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_get_pseudo_stash_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        if name.strip_suffix("::") == Some("OUTER") {
            // OUTER:: is lexical, not package-based. Expose captured lexical vars
            // from the current interpreter environment as stash entries.
            let mut entries: HashMap<String, Value> = HashMap::new();
            for (key, val) in self.env().iter() {
                let key_str = key.resolve();
                if self.should_hide_from_my_global_stash(&key_str) {
                    continue;
                }
                let display_key = Self::add_sigil_prefix(&key_str);
                entries.insert(display_key, val.clone());
            }
            self.stack.push(Value::Hash(Value::hash_arc(entries)));
            return;
        }
        if name.strip_suffix("::") == Some("OUR") {
            let mut entries: HashMap<String, Value> = HashMap::new();
            for (key, val) in self.our_vars_iter() {
                let display_key = Self::add_sigil_prefix(key);
                entries.insert(display_key, val.clone());
            }
            self.stack.push(Value::Hash(Value::hash_arc(entries)));
            return;
        }
        if let Some(package) = name.strip_suffix("::")
            && package != "MY"
            && !package.is_empty()
        {
            self.stack
                .push(loan_env!(self, package_stash_value(package)));
            return;
        }

        // MY:: pseudo-stash: collect all variable names from current scope.
        let mut entries: HashMap<String, Value> = HashMap::new();
        for (i, var_name) in code.locals.iter().enumerate() {
            let val = self.locals[i].clone();
            let key = Self::add_sigil_prefix(var_name);
            entries.insert(key, val);
        }
        for (key, val) in self.env().iter() {
            let key_str = key.resolve();
            if self.should_hide_from_my_global_stash(&key_str) {
                continue;
            }
            let display_key = Self::add_sigil_prefix(&key_str);
            entries.entry(display_key).or_insert_with(|| val.clone());
        }
        self.stack.push(Value::Hash(Value::hash_arc(entries)));
    }

    /// Build a pseudo-stash hash for a given pseudo-package name.
    /// Used by .WHO dispatch on pseudo-package Package values.
    pub(super) fn build_pseudo_stash(&mut self, code: &CompiledCode, name: &str) -> Value {
        if name == "OUTER" {
            let mut entries: HashMap<String, Value> = HashMap::new();
            for (key, val) in self.env().iter() {
                let key_str = key.resolve();
                if self.should_hide_from_my_global_stash(&key_str) {
                    continue;
                }
                let display_key = Self::add_sigil_prefix(&key_str);
                entries.insert(display_key, val.clone());
            }
            return Value::Hash(Value::hash_arc(entries));
        }
        if name == "OUR" {
            let mut entries: HashMap<String, Value> = HashMap::new();
            for (key, val) in self.our_vars_iter() {
                let display_key = Self::add_sigil_prefix(key);
                entries.insert(display_key, val.clone());
            }
            return Value::Hash(Value::hash_arc(entries));
        }
        if name != "MY" && name != "LEXICAL" {
            return loan_env!(self, package_stash_value(name));
        }
        // MY / LEXICAL: collect locals + env
        let mut entries: HashMap<String, Value> = HashMap::new();
        for (i, var_name) in code.locals.iter().enumerate() {
            let val = self.locals[i].clone();
            let key = Self::add_sigil_prefix(var_name);
            entries.insert(key, val);
        }
        for (key, val) in self.env().iter() {
            let key_str = key.resolve();
            if self.should_hide_from_my_global_stash(&key_str) {
                continue;
            }
            let display_key = Self::add_sigil_prefix(&key_str);
            entries.entry(display_key).or_insert_with(|| val.clone());
        }
        Value::Hash(Value::hash_arc(entries))
    }

    /// Add a sigil prefix to a variable name for display in pseudo-stash.
    /// Names starting with @, %, & already have sigils. Others get $ prefix.
    fn add_sigil_prefix(name: &str) -> String {
        if name.starts_with('$')
            || name.starts_with('@')
            || name.starts_with('%')
            || name.starts_with('&')
        {
            name.to_string()
        } else if name.starts_with('*') || name.starts_with('?') || name.starts_with('!') {
            // Twigil variables like *CWD → $*CWD
            format!("${}", name)
        } else if name.chars().next().is_some_and(|c| c.is_uppercase()) {
            // Type names, package names — no sigil
            name.to_string()
        } else {
            format!("${}", name)
        }
    }

    /// Execute HyperSlice opcode: recursively iterate a hash.
    pub(super) fn exec_hyper_slice_op(&mut self, adverb: u8) -> Result<(), RuntimeError> {
        use crate::ast::HyperSliceAdverb;

        let target = self.stack.pop().unwrap();
        let adverb = match adverb {
            0 => HyperSliceAdverb::Kv,
            1 => HyperSliceAdverb::K,
            2 => HyperSliceAdverb::V,
            3 => HyperSliceAdverb::Tree,
            4 => HyperSliceAdverb::DeepK,
            5 => HyperSliceAdverb::DeepKv,
            _ => HyperSliceAdverb::Kv,
        };

        let hash = match target {
            Value::Hash(h) => h,
            _ => {
                return Err(RuntimeError::new(
                    "Cannot use {**} hyperslice on a non-Hash value".to_string(),
                ));
            }
        };

        let mut result = Vec::new();
        let path: Vec<String> = Vec::new();
        Self::hyperslice_recurse(&hash, &path, adverb, &mut result);
        self.stack.push(Value::array(result));
        Ok(())
    }

    fn hyperslice_recurse(
        hash: &std::collections::HashMap<String, Value>,
        path: &[String],
        adverb: crate::ast::HyperSliceAdverb,
        result: &mut Vec<Value>,
    ) {
        use crate::ast::HyperSliceAdverb;

        for (key, value) in hash.iter() {
            let mut cur_path: Vec<String> = path.to_vec();
            cur_path.push(key.clone());

            match adverb {
                HyperSliceAdverb::Kv => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        result.push(Value::str(key.clone()));
                        result.push(value.clone());
                    }
                }
                HyperSliceAdverb::K => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        result.push(Value::str(key.clone()));
                    }
                }
                HyperSliceAdverb::V => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        result.push(value.clone());
                    }
                }
                HyperSliceAdverb::Tree => {
                    // Tree mode: yield key-value pairs for all entries,
                    // including sub-hashes (as their original Value::Hash)
                    result.push(Value::str(key.clone()));
                    result.push(value.clone());
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    }
                }
                HyperSliceAdverb::DeepK => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        let key_array: Vec<Value> =
                            cur_path.iter().map(|s| Value::str(s.clone())).collect();
                        result.push(Value::array(key_array));
                    }
                }
                HyperSliceAdverb::DeepKv => {
                    if let Value::Hash(inner) = value {
                        Self::hyperslice_recurse(inner, &cur_path, adverb, result);
                    } else {
                        let key_array: Vec<Value> =
                            cur_path.iter().map(|s| Value::str(s.clone())).collect();
                        result.push(Value::array(key_array));
                        result.push(value.clone());
                    }
                }
            }
        }
    }

    /// Execute HyperIndex opcode: drill into nested hash by key path.
    pub(super) fn exec_hyper_index_op(&mut self) -> Result<(), RuntimeError> {
        let keys = self.stack.pop().unwrap();
        let target = self.stack.pop().unwrap();

        let key_list = match keys {
            Value::Array(items, ..) => items,
            Value::Seq(items) => crate::value::Value::array_arc(Arc::new(items.to_vec()).to_vec()),
            _ => crate::value::Value::array_arc(Arc::new(vec![keys]).to_vec()),
        };

        let mut current = target;
        for key in key_list.iter() {
            match current {
                Value::Hash(ref h) => {
                    let k = key.to_string_value();
                    current = h.get(&k).cloned().unwrap_or(Value::Nil);
                }
                _ => {
                    current = Value::Nil;
                    break;
                }
            }
        }

        self.stack.push(current);
        Ok(())
    }

    /// Wrap an integer value to fit within a native integer type's range.
    /// Wrap `val` for storage into the native-integer array named by `var_name`
    /// (e.g. `-1` -> `255` for a `uint8` array). Returns the value unchanged when
    /// the variable is not a native integer array or wrapping does not apply.
    fn wrap_native_int_for_var(&mut self, var_name: &str, val: Value) -> Value {
        if let Some(constraint) = loan_env!(self, var_type_constraint(var_name))
            && crate::runtime::native_types::is_native_int_type(&constraint)
        {
            return Self::wrap_native_int_by_constraint(&constraint, val.clone()).unwrap_or(val);
        }
        val
    }

    /// For non-native constraints or non-integer values, returns the value unchanged.
    pub(super) fn wrap_native_int_by_constraint(
        constraint: &str,
        val: Value,
    ) -> Result<Value, RuntimeError> {
        use crate::runtime::native_types;
        use num_bigint::BigInt as NumBigInt;
        use num_traits::ToPrimitive;

        let (base, _) = crate::runtime::types::strip_type_smiley(constraint);
        if !native_types::is_native_int_type(base) {
            return Ok(val);
        }
        // Full-width signed native types don't wrap — they should throw on overflow.
        if matches!(base, "int" | "int64") {
            if let Value::BigInt(ref n) = val {
                let bits = n.bits();
                return Err(RuntimeError::new(format!(
                    "Cannot unbox {} bit wide bigint into native integer",
                    bits
                )));
            }
            return Ok(val);
        }
        // Full-width unsigned native types: BigInt values that fit in u64 are valid,
        // negative Value::Int values need wrapping (like C unsigned semantics).
        if matches!(base, "uint" | "uint64") {
            if let Value::BigInt(ref n) = val {
                if n.to_u64().is_some() {
                    return Ok(val);
                }
                let bits = n.bits();
                return Err(RuntimeError::new(format!(
                    "Cannot unbox {} bit wide bigint into native integer",
                    bits
                )));
            }
            // Value::Int with negative value needs wrapping to unsigned range
            if let Value::Int(n) = &val
                && *n < 0
            {
                let big_val = NumBigInt::from(*n);
                let wrapped = native_types::wrap_native_int(base, &big_val);
                return Ok(wrapped
                    .to_i64()
                    .map(Value::Int)
                    .unwrap_or_else(|| Value::bigint(wrapped)));
            }
            return Ok(val);
        }
        let big_val = match &val {
            Value::Int(n) => NumBigInt::from(*n),
            Value::BigInt(n) => (**n).clone(),
            _ => return Ok(val),
        };
        if native_types::is_in_native_range(base, &big_val) {
            return Ok(val);
        }
        let wrapped = native_types::wrap_native_int(base, &big_val);
        Ok(wrapped
            .to_i64()
            .map(Value::Int)
            .unwrap_or_else(|| Value::bigint(wrapped)))
    }

    /// Lazily convert pending alias bind names (from closure `:=` bindings)
    /// into local_bind_pairs now that we have access to the outer code.
    fn resolve_pending_alias_binds(&mut self, code: &CompiledCode) {
        if self.pending_alias_bind_names.is_empty() {
            return;
        }
        let pending = std::mem::take(&mut self.pending_alias_bind_names);
        for (target_name, source_name) in pending {
            if let Some(target_idx) = code.locals.iter().position(|n| n == &target_name)
                && let Some(source_idx) = code.locals.iter().position(|n| n == &source_name)
                && source_idx != target_idx
            {
                // Add bidirectional bind pairs: source->target AND target->source.
                // This ensures writes to either variable propagate to the other.
                if !self.local_bind_pairs.contains(&(source_idx, target_idx)) {
                    self.local_bind_pairs.push((source_idx, target_idx));
                }
                if !self.local_bind_pairs.contains(&(target_idx, source_idx)) {
                    self.local_bind_pairs.push((target_idx, source_idx));
                }
                // Sync the target local from the source local or env so the
                // initial value is correct (the closure may have set the
                // target via SetGlobal but locals are stale from the frame
                // restore).
                let source_val = if let Some(v) = self.env().get(&source_name) {
                    v.clone()
                } else {
                    self.locals[source_idx].clone()
                };
                self.locals[target_idx] = source_val;
            }
        }
    }
}
