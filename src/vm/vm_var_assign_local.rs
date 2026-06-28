use super::*;
use crate::symbol::Symbol;
use std::collections::HashMap;
use std::sync::Arc;

impl Interpreter {
    pub(crate) fn exec_assign_expr_local_op_inner(
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
            // Preserve a shaped (fixed-dimension) LHS across an expression-context
            // reassignment (`is (@arr = ()), ...`), mirroring the statement-form
            // `SetLocal` path: a fixed-dimension array refills to its dimension
            // instead of shrinking. The local slot and the env copy can diverge
            // (the `env_dirty` dual store), so consult whichever currently holds a
            // shaped value.
            let lhs_shape =
                crate::runtime::utils::shaped_array_shape(&self.locals[idx]).or_else(|| {
                    self.get_env_with_main_alias(name)
                        .as_ref()
                        .and_then(crate::runtime::utils::shaped_array_shape)
                });
            let assigned_has_own_shape = crate::runtime::utils::shaped_array_shape(&assigned)
                .is_some()
                || matches!(&assigned, Value::Array(_, crate::value::ArrayKind::Shaped));
            if let Some(shape) = &lhs_shape
                && shape.len() == 1
                && !assigned_has_own_shape
            {
                let items = runtime::value_to_list(&assigned);
                let item_count = items.len();
                let mut shaped_items: Vec<Value> = items.into_iter().take(shape[0]).collect();
                if item_count < shape[0] {
                    // Pad with the element type's default (native arrays: int->0,
                    // num->0e0, str->""), not Nil, so clearing a shaped num array
                    // yields `0 0 0 0` rather than empty slots.
                    let default = {
                        let old = self.locals[idx].clone();
                        self.typed_container_default(&old)
                    };
                    Self::autoviv_resize(&mut shaped_items, shape[0], default)?;
                }
                assigned = Value::Array(
                    Arc::new(crate::value::ArrayData::new(shaped_items)),
                    crate::value::ArrayKind::Shaped,
                );
                crate::runtime::utils::mark_shaped_array(&assigned, Some(shape));
                if let Some(info) = self.container_type_metadata(&self.locals[idx]) {
                    assigned = self.tag_container_metadata(assigned, info);
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
    pub(crate) fn add_sigil_prefix(name: &str) -> String {
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

    pub(crate) fn hyperslice_recurse(
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
    pub(crate) fn wrap_native_int_for_var(&mut self, var_name: &str, val: Value) -> Value {
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
    pub(crate) fn resolve_pending_alias_binds(&mut self, code: &CompiledCode) {
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
