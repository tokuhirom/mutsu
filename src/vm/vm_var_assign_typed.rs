use super::*;
use std::sync::Arc;
use unicode_normalization::UnicodeNormalization;

impl Interpreter {
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

    pub(crate) fn extract_varref_binding(raw_val: Value) -> (Value, Option<String>) {
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
    pub(crate) fn try_reconstruct_typed_key(key: &str, target_type: &str) -> Value {
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
    pub(crate) fn resolve_generic_range_for_assign(
        &mut self,
        idx: &Value,
        array_len: usize,
    ) -> Option<Value> {
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

    pub(crate) fn slice_indices_from_index(idx: &Value) -> Option<Vec<usize>> {
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
    pub(crate) fn make_out_of_range_error(effective_index: i64) -> RuntimeError {
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
    pub(crate) fn propagate_sigilless_alias_chain(
        &mut self,
        code: &CompiledCode,
        name: &str,
        val: &Value,
    ) {
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
}
