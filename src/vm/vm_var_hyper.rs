use super::*;

impl Interpreter {
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
    pub(super) fn wrap_native_int_for_var(&mut self, var_name: &str, val: Value) -> Value {
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
    pub(super) fn resolve_pending_alias_binds(&mut self, code: &CompiledCode) {
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
