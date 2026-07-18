use super::*;
use unicode_normalization::UnicodeNormalization;

impl Interpreter {
    /// Container identity (§3.1): compute the value to write THROUGH a shared
    /// `ContainerRef` cell for a whole-container reassignment of an `@`
    /// variable (`@a = ...` when `@a`'s slot holds a shared cell — e.g. a
    /// `:=`-bound alias `my @b := @a` or a loop-var alias `for @src -> @a`).
    /// Coerces the raw RHS to an Array and re-applies the element-type coercion +
    /// `array[T]` metadata so a typed native array keeps its identity
    /// (`@a.WHAT === array[int]`) after the write-through. Non-`@` names return
    /// `raw` unchanged.
    pub(super) fn array_container_writethrough_value(
        &mut self,
        name: &str,
        raw: Value,
        old: &Value,
    ) -> Result<Value, RuntimeError> {
        if !name.starts_with('@') {
            return Ok(raw);
        }
        let mut val = crate::runtime::coerce_to_array(raw);
        val = self.coerce_typed_container_assignment(name, val, false)?;
        // Prefer a declared constraint (`my int @a`); otherwise inherit the type
        // identity of the container currently held by the shared cell. A bound
        // typed array (`for @testing -> @a { @a = ... }`, `my @b := @a`) has no
        // `var_type_constraint` on the alias name — its `array[T]` identity lives
        // in the cell's embedded metadata, which a whole reassignment must keep.
        let info = if let Some(value_type) = loan_env!(self, var_type_constraint(name)) {
            Some(crate::runtime::ContainerTypeInfo {
                declared_type: if crate::runtime::native_types::is_native_array_element_type(
                    &value_type,
                ) {
                    Some(format!("array[{value_type}]"))
                } else {
                    None
                },
                value_type,
                key_type: None,
            })
        } else {
            self.container_type_metadata(old)
        };
        if let Some(info) = info {
            // Coerce elements to the inherited element type (a bound
            // `array[int]` must reject/convert non-int values just like a
            // declared one).
            if crate::runtime::native_types::is_native_array_element_type(&info.value_type)
                && let ValueView::Array(items, kind) = val.view()
            {
                let coerced = self.coerce_typed_array_elements(
                    name,
                    &info.value_type,
                    &items,
                    kind,
                    &|_| None,
                    false,
                )?;
                val = Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(coerced)),
                    kind,
                );
            }
            val = self.tag_container_metadata(val, info);
        }
        Ok(val)
    }

    pub(super) fn normalize_scalar_assignment_value(val: Value) -> Value {
        let is_nilish = |v: &Value| match v.view() {
            ValueView::Nil => true,
            ValueView::Package(sym) => sym.resolve() == "Any",
            _ => false,
        };
        let single_nilish = match val.view() {
            ValueView::Array(items, _) => items.len() == 1 && items.first().is_some_and(is_nilish),
            ValueView::Seq(items) => items.len() == 1 && items.first().is_some_and(is_nilish),
            ValueView::Slip(items) => items.len() == 1 && items.first().is_some_and(is_nilish),
            _ => false,
        };
        if single_nilish { Value::NIL } else { val }
    }

    pub(crate) fn extract_varref_binding(raw_val: Value) -> (Value, Option<String>) {
        match raw_val.as_varref() {
            Some((name, inner, _)) => (inner.clone(), Some(name.resolve())),
            None => (raw_val, None),
        }
    }

    pub(crate) fn resolve_sigilless_alias_source_name(&self, source_name: &str) -> String {
        let mut resolved = source_name.to_string();
        let mut seen = std::collections::HashSet::new();
        while seen.insert(resolved.clone()) {
            let key = format!("__mutsu_sigilless_alias::{}", resolved);
            let Some(ValueView::Str(next)) = self.env().get(&key).map(Value::view) else {
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
    pub(crate) fn try_reconstruct_typed_key(key: &str, target_type: &str) -> Value {
        match target_type {
            "Int" => {
                if let Ok(n) = key.parse::<i64>() {
                    return Value::int(n);
                }
            }
            "Num" => {
                if let Ok(n) = key.parse::<f64>() {
                    return Value::num(n);
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

    /// For an object hash (`my Int %{Int}`) whose key type is a non-`Str` type,
    /// reconstruct the typed key objects from the stringified store keys and
    /// record them in `original_keys`, so `.keys`/`.pairs`/`.raku` report the
    /// real key (`Int(1)`, not `"1"`). No-op for plain / `Str`-keyed hashes and
    /// for hashes that already carry `original_keys`. Used on the SetVarType
    /// path, where the list→hash coercion ran before the key type was known.
    pub(crate) fn populate_object_hash_typed_keys(&self, value: Value) -> Value {
        let ValueView::Hash(map) = value.view() else {
            return value;
        };
        let Some(key_type) = map.key_type.clone() else {
            return value;
        };
        let (base, _) = crate::runtime::types::strip_type_smiley(&key_type);
        if base == "Str" || base == "Any" || base == "Mu" || map.has_typed_keys() {
            return value;
        }
        let mut original_keys = std::collections::HashMap::with_capacity(map.len());
        for key in map.keys() {
            original_keys.insert(key.clone(), Self::try_reconstruct_typed_key(key, base));
        }
        if original_keys.is_empty() {
            return value;
        }
        crate::runtime::utils::set_hash_original_keys(value, original_keys)
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
        // A parallel/lazy sequence RHS (`my T @a = seq.hyper.map(...)` /
        // `.race` / a plain `Seq`/`Slip`) must be reified to its elements so the
        // typed-array element coercion below type-checks each element, instead of
        // rejecting the whole `HyperSeq`/`RaceSeq` as one opaque value ("expected
        // T, got HyperSeq"). `Seq`/`Slip` are usually already reified to an Array
        // upstream, but normalizing them here too is harmless. Only for `@` vars.
        let value = if var_name.starts_with('@') {
            match value.view() {
                ValueView::HyperSeq(items)
                | ValueView::RaceSeq(items)
                | ValueView::Seq(items)
                | ValueView::Slip(items) => Value::real_array(items.to_vec()),
                _ => value,
            }
        } else {
            value
        };
        if var_name.starts_with('@')
            && let Some(constraint) = loan_env!(self, var_type_constraint(var_name))
            && let ValueView::Array(items, kind) = value.view()
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
            return Ok(Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(coerced_items)),
                kind,
            ));
        }

        if var_name.starts_with('%')
            && let ValueView::Hash(map) = value.view()
        {
            // Preserve original keys from the source hash before coercion
            // (coercion creates a new Arc, losing the registration).
            let saved_original_keys =
                runtime::utils::hash_original_keys_snapshot(&Value::hash_with_data(map.clone()));
            let value_constraint = loan_env!(self, var_type_constraint(var_name));
            let key_constraint = loan_env!(self, var_hash_key_constraint(var_name));
            let mut coerced_map = std::collections::HashMap::with_capacity(map.len());
            // For an object hash (`my Int %h{Int}`) whose key type is not the
            // default `Str`, record the reconstructed typed key alongside the
            // stringified store key so `.keys`/`.pairs`/`.raku` report the real
            // key object (`Int(1)`, not `"1"`).
            let track_typed_keys = key_constraint
                .as_deref()
                .map(|kc| coercion_target(kc).unwrap_or_else(|| kc.to_string()))
                .is_some_and(|kt| {
                    let (base, _) = crate::runtime::types::strip_type_smiley(&kt);
                    base != "Str" && base != "Any" && base != "Mu"
                });
            let mut obj_original_keys: std::collections::HashMap<String, Value> =
                std::collections::HashMap::new();
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
                    if track_typed_keys {
                        obj_original_keys.insert(key.clone(), key_as_typed_value);
                    }
                    key.clone()
                } else {
                    key.clone()
                };
                let coerced_val = if let Some(constraint) = &value_constraint {
                    if val.is_nil() {
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
            // Re-embed original keys on the new (coerced) hash. Prefer the freshly
            // reconstructed typed keys (object hash with a non-Str key type);
            // otherwise fall back to any keys carried over from the source hash.
            if track_typed_keys && !obj_original_keys.is_empty() {
                result = runtime::utils::set_hash_original_keys(result, obj_original_keys);
            } else if let Some(orig) = saved_original_keys {
                result = runtime::utils::set_hash_original_keys(result, orig);
            }
            return Ok(result);
        }

        Ok(value)
    }

    /// Coerce/check elements of a typed array, recursing into shaped sub-arrays.
    pub(crate) fn coerce_typed_array_elements(
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
            if native_constraint && matches!(item.view(), ValueView::Package(_)) {
                coerced_items.push(Self::native_fill_for_constraint(Some(constraint)));
                continue;
            }
            if item.is_nil() {
                if let Some(default) = self.var_default(var_name) {
                    coerced_items.push(default.clone());
                } else if explicit_initializer && self.is_definite_constraint(constraint) {
                    return Err(runtime::utils::type_check_element_typed_error(
                        var_name, constraint, item,
                    ));
                } else if native_constraint {
                    // A native element type has no type object; Nil reverts to the
                    // array's numeric/string zero.
                    coerced_items.push(Self::native_fill_for_constraint(Some(constraint)));
                } else {
                    // Raku: Nil assigned to a typed container element reverts to the
                    // element type's default, i.e. the type object itself
                    // (`my Bool @a = Nil` -> Array[Bool].new(Bool)).
                    let base = crate::runtime::types::strip_type_smiley(constraint).0;
                    coerced_items.push(Value::package(crate::symbol::Symbol::intern(base)));
                }
                continue;
            }
            // For shaped arrays, sub-arrays are structural — recurse into them
            if kind == crate::value::ArrayKind::Shaped
                && let ValueView::Array(sub_items, sub_kind) = item.view()
            {
                let sub_coerced = self.coerce_typed_array_elements(
                    var_name,
                    constraint,
                    &sub_items,
                    sub_kind,
                    coercion_target,
                    explicit_initializer,
                )?;
                coerced_items.push(Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(sub_coerced)),
                    sub_kind,
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
                match item.view() {
                    ValueView::Array(items, kind) if !kind.is_real_array() => {
                        Value::array_with_kind(items.clone(), crate::value::ArrayKind::Array)
                    }
                    ValueView::Scalar(inner) => match inner.view() {
                        ValueView::Array(items, kind) if !kind.is_real_array() => {
                            Value::array_with_kind(items.clone(), crate::value::ArrayKind::Array)
                        }
                        ValueView::Array(..) => inner.clone(),
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
    pub(crate) fn resolve_generic_range_for_assign(
        &mut self,
        idx: &Value,
        array_len: usize,
    ) -> Option<Value> {
        if let ValueView::GenericRange {
            start,
            end,
            excl_start,
            excl_end,
        } = idx.view()
        {
            let len = array_len as i64;
            let resolve_endpoint = |vm: &mut Self, val: &Value| -> i64 {
                match val.view() {
                    ValueView::Int(i) => i,
                    ValueView::Sub(data) => {
                        let mut sub_env = data.env.clone();
                        for p in &data.params {
                            sub_env.insert(p.to_string(), Value::int(len));
                        }
                        let saved_env = std::mem::take(vm.env_mut());
                        *vm.env_mut() = sub_env;
                        let result = vm.eval_block_value(&data.body).unwrap_or(Value::NIL);
                        *vm.env_mut() = saved_env;
                        match result.view() {
                            ValueView::Int(i) => i,
                            _ => 0,
                        }
                    }
                    ValueView::Num(f) => f as i64,
                    _ => 0,
                }
            };
            let s = resolve_endpoint(self, start);
            let e = resolve_endpoint(self, end);
            let resolved = if excl_start && excl_end {
                Value::range_excl_both(s, e)
            } else if excl_start {
                Value::range_excl_start(s, e)
            } else if excl_end {
                Value::range_excl(s, e)
            } else {
                Value::range(s, e)
            };
            Some(resolved)
        } else {
            None
        }
    }

    pub(crate) fn slice_indices_from_index(idx: &Value) -> Option<Vec<usize>> {
        match idx.view() {
            ValueView::Range(a, b) => {
                let start = a.max(0);
                let end = b.min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < start {
                    return Some(Vec::new());
                }
                Some(
                    (start..=end)
                        .filter_map(|i| usize::try_from(i).ok())
                        .collect(),
                )
            }
            ValueView::RangeExcl(a, b) => {
                let start = a.max(0);
                let end_excl = b
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
            ValueView::RangeExclStart(a, b) => {
                let start = a.saturating_add(1).max(0);
                let end = b.min(start.saturating_add(Self::MAX_ASSIGN_SLICE_EXPAND));
                if end < start {
                    return Some(Vec::new());
                }
                Some(
                    (start..=end)
                        .filter_map(|i| usize::try_from(i).ok())
                        .collect(),
                )
            }
            ValueView::RangeExclBoth(a, b) => {
                let start = a.saturating_add(1).max(0);
                let end_excl = b
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
    pub(crate) fn make_out_of_range_error(effective_index: i64) -> RuntimeError {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str_from(&format!(
                "Effective index out of range. Is: {}, should be in 0..^Inf",
                effective_index
            )),
        );
        attrs.insert("got".to_string(), Value::int(effective_index));
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
            if let ValueView::Instance { attributes, .. } = v.view()
                && Self::is_buf_value(&v)
                && attributes.contains_key("bytes")
            {
                let str_result = self.try_compiled_method_or_interpret(v, "Str", Vec::new())?;
                result.push_str(&str_result.to_string_value());
                continue;
            }
            // For non-Buf instances, try .Stringy() for string context (Raku spec:
            // string interpolation calls .Str which delegates to .Stringy).
            if let ValueView::Instance { .. } = v.view() {
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
            // A type object (a `Package` value) whose class defines a user
            // `.Stringy`/`.Str` must dispatch it too: `class A { method Str {"foo"} }`
            // then `"$x"` / `"{A}"` renders "foo" (mirrors `coerce_stringy_operand`
            // used by infix `~`). Plain type objects fall through to the default
            // (empty) stringification.
            if let ValueView::Package(name) = v.view() {
                let cn = name.resolve().to_string();
                if self.has_user_method(&cn, "Stringy") {
                    let r =
                        self.try_compiled_method_or_interpret(v.clone(), "Stringy", Vec::new())?;
                    result.push_str(&r.to_string_value());
                    continue;
                }
                if self.has_user_method(&cn, "Str") {
                    let r = self.try_compiled_method_or_interpret(v.clone(), "Str", Vec::new())?;
                    result.push_str(&r.to_string_value());
                    continue;
                }
            }
            result.push_str(&crate::runtime::utils::coerce_to_str(&v));
        }
        self.reconcile_caller_after_internal_dispatch(caller_code);
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
        if let ValueView::Instance { .. } = val.view()
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
            if !new_val.is_nil() && !self.type_matches_value(&constraint, new_val) {
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
        arc: &crate::gc::Gc<std::sync::Mutex<Value>>,
        name: &str,
        increment: bool,
        post: bool,
    ) -> bool {
        let mut guard = arc.lock().unwrap();
        if matches!(guard.view(), ValueView::Instance { .. }) {
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
        if let ValueView::Instance { .. } = val.view()
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
    pub(crate) fn propagate_sigilless_alias_chain(
        &mut self,
        code: &CompiledCode,
        name: &str,
        val: &Value,
    ) {
        // Fast path: no sigilless-parameter alias has ever been registered, so the
        // chain is empty — skip the `format!` + env lookup entirely.
        if !self.sigilless_alias_seen() {
            return;
        }
        let alias_key = format!("__mutsu_sigilless_alias::{}", name);
        let mut alias_name = self.env().get(&alias_key).and_then(|v| {
            if let ValueView::Str(n) = v.view() {
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
                if let ValueView::Str(n) = v.view() {
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
        // §1.5: compile-time-resolved local slot for `name`, when it is a
        // current-scope local. Drives the slot mirror + `local_bind_pairs` source
        // resolution below instead of a by-name `code.locals` search (ambiguous
        // once a name occupies several slots). `None` = non-local / no baked slot.
        slot: Option<u32>,
        new_val: Value,
    ) -> Result<Value, RuntimeError> {
        let new_val = self.maybe_wrap_native_int(name, new_val);
        self.check_incdec_type_constraint(name, &new_val)?;
        self.set_env_with_main_alias(name, new_val.clone());
        // A compound assign / inc-dec to a package-scope free variable (`our $X`
        // or a `package { my $X }` lexical) reached from inside a named sub uses
        // the bare name; mirror the value back into the canonical package store
        // so the mutation persists across calls (the env write above is only the
        // same-frame view). No-op for non-package-scope names.
        self.writeback_package_scope_var(name, &new_val);
        // Track topic mutations for the rw-map writeback (`@a.map({ $_ += 1 })`).
        // A compound assign / inc-dec to `$_` lands here via the fused
        // `AtomicCompoundVar` path; the plain-assign path (`AssignExpr`) records
        // this already, so without it `+=`/`~=`/`++` mutations inside a map block
        // are silently dropped on the source array (only `*=`/`/=`, whose LHS
        // desugars through a `defined`-ternary and so skips fusion, worked).
        if name == "_" {
            self.env_mut()
                .insert("__mutsu_rw_map_topic__".to_string(), new_val.clone());
        }
        self.sync_anon_state_value(name, &new_val);
        // §1.5: mirror into the compile-time-baked slot when present (scope-correct
        // even once a name occupies several slots); fall back to the by-name
        // resolution for a non-local / no-baked-slot target.
        match slot {
            Some(s) if (s as usize) < self.locals.len() => {
                self.locals[s as usize] = new_val.clone();
            }
            _ => self.update_local_if_exists(code, name, &new_val),
        }
        // Slice F: an inc/dec (`$*foo++`) of a caller-declared dynamic variable
        // writes only `env` by name; record it so the call-site drain writes it
        // through to the caller frame's slot (mirrors the SetGlobal path).
        if name.starts_with('*') {
            self.pending_rw_writeback_sources.push(name.to_string());
        }
        // Propagate via local_bind_pairs (for `:=` bindings within this scope
        // or cross-scope bindings resolved by resolve_pending_alias_binds).
        // §1.5: prefer the baked slot as the bind-pair source; fall back to the
        // by-name lookup for a non-local target.
        let source_idx = match slot {
            Some(s) => Some(s as usize),
            None => code.locals.iter().position(|n| n == name),
        };
        if let Some(source_idx) = source_idx {
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
}
