use super::*;

impl Interpreter {
    pub(super) fn exec_get_capture_var_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let name = Self::const_str(code, name_idx);
        let val = if let Some(v) = self.env().get(name).cloned() {
            v
        } else if let Some(key) = name.strip_prefix('<').and_then(|s| s.strip_suffix('>'))
            && let Some(match_val) = self.env().get("/").cloned()
        {
            match &match_val {
                Value::Hash(map) => map.get(key).cloned().unwrap_or(Value::Nil),
                _ => self
                    .try_compiled_method_or_interpret(
                        match_val,
                        "AT-KEY",
                        vec![Value::str(key.to_string())],
                    )
                    .unwrap_or(Value::Nil),
            }
        } else {
            Value::Nil
        };
        self.stack.push(val);
    }

    pub(super) fn exec_get_code_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        let mut val = loan_env!(self, resolve_code_var(name));
        // Fallback for fast-path method dispatch (skip_env_setup=true):
        // &!attr is not set in env, so read directly from self's instance
        // attributes when available.
        if matches!(val, Value::Nil)
            && let Some(attr_name) = name.strip_prefix('!').filter(|n| !n.is_empty())
            && let Some(Value::Instance { attributes, .. }) =
                self.get_env_with_main_alias("self").as_ref()
            && let Some(attr_val) = attributes.as_map().get(attr_name)
        {
            val = attr_val.clone();
        }
        // In Raku, &foo for an undefined routine is a compile-time error.
        // We approximate this at runtime, but only inside EVAL context
        // to avoid breaking code that relies on &name returning Nil for
        // non-existent routines (e.g. custom EXPORT mechanisms).
        if matches!(val, Value::Nil)
            && !name.contains("::")
            && !name.starts_with('?')
            && !name.starts_with('*')
            && matches!(self.env().get("__mutsu_in_eval"), Some(Value::Bool(true)))
        {
            let env_key = format!("&{}", name);
            let is_declared = self.env().contains_key(&env_key);
            if !is_declared {
                let suggestions = self.suggest_routine_names(name);
                return Err(RuntimeError::undeclared_routine_symbols(
                    name,
                    format!("Undeclared routine:\n    {} used at line 1", name),
                    suggestions,
                ));
            }
        }
        self.stack.push(val);
        Ok(())
    }

    pub(super) fn exec_indirect_code_lookup_op(&mut self, code: &CompiledCode, name_idx: u32) {
        let func_name = Self::const_str(code, name_idx).to_string();
        // Pop the package name from the stack (result of evaluating the package expr)
        let package = self.stack.pop().unwrap_or(Value::Nil);
        // Construct a qualified name: "SETTING::OUTER::...::not"
        // resolve_code_var will strip pseudo-package prefixes and resolve to builtin
        let pkg_str = package.to_string_value();
        let qualified = if pkg_str.is_empty() {
            func_name
        } else {
            format!("{}::{}", pkg_str, func_name)
        };
        let val = loan_env!(self, resolve_code_var(&qualified));
        self.stack.push(val);
    }

    /// Execute symbolic variable dereference: $::("name"), @::("name"), %::("name").
    /// Pops the name string from the stack, prepends the sigil, and looks up the variable.
    pub(super) fn exec_symbolic_deref_op(&mut self, code: &CompiledCode, sigil_idx: u32) {
        let sigil = Self::const_str(code, sigil_idx).to_string();
        let name_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = name_val.to_string_value();
        // For $::("x"), look up the bare name "x" (scalars are stored without sigil).
        // For @::("x"), look up "@x" (arrays are stored with sigil).
        // For %::("x"), look up "%x" (hashes are stored with sigil).
        // For &::("x"), resolve as a code variable (function lookup).
        if sigil == "&" {
            let val = loan_env!(self, resolve_code_var(&name));
            self.stack.push(val);
            return;
        }
        // Handle CALLER:: prefix(es) for dynamic variable lookup
        let mut remaining = name.as_str();
        let mut caller_depth = 0usize;
        while let Some(rest) = remaining.strip_prefix("CALLER::") {
            caller_depth += 1;
            remaining = rest;
        }
        if caller_depth > 0 {
            let bare_name = match sigil.as_str() {
                "$" => remaining.to_string(),
                "@" => format!("@{}", remaining),
                "%" => format!("%{}", remaining),
                _ => remaining.to_string(),
            };
            let val = self
                .get_caller_var(&bare_name, caller_depth)
                .unwrap_or(Value::Nil);
            self.stack.push(val);
            return;
        }
        let lookup_name = match sigil.as_str() {
            "$" => name.to_string(),
            "@" => format!("@{}", name),
            "%" => format!("%{}", name),
            _ => name.to_string(),
        };
        let val = self
            .get_env_with_main_alias(&lookup_name)
            .unwrap_or(Value::Nil);
        self.stack.push(val);
    }

    pub(super) fn exec_symbolic_deref_store_op(&mut self, code: &CompiledCode, sigil_idx: u32) {
        let sigil = Self::const_str(code, sigil_idx).to_string();
        let name_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = name_val.to_string_value();
        // Stack: [value] (already below name)
        let raw_value = self.stack.pop().unwrap_or(Value::Nil);
        let store_name = match sigil.as_str() {
            "$" => name.to_string(),
            "@" => format!("@{}", name),
            "%" => format!("%{}", name),
            // Code variables (`&::("infix:<times>")`) must be stored under the
            // `&`-sigilled name so that operator dispatch can find the closure
            // via its `&infix:<...>` env key.
            "&" => format!("&{}", name),
            _ => name.to_string(),
        };
        // For $ sigil (item context), take only first element if value is a list.
        let value = if sigil == "$" {
            match &raw_value {
                Value::Array(items, ..) => items.first().cloned().unwrap_or(Value::Nil),
                _ => raw_value,
            }
        } else {
            raw_value
        };
        self.env_mut().insert(store_name.clone(), value.clone());
        self.update_local_if_exists(code, &store_name, &value);
        // env_dirty substrate (docs/captured-outer-cell-sharing.md §10): a
        // symbolic-deref store (`$::($name) = v`) writes the target lexical by name
        // straight into env. `update_local_if_exists` refreshes the slot only when
        // it lives in *this* frame; inside a carrier (`lives-ok { $::($n) = v }`)
        // the owning slot is the carrier-caller's, reached only via the carrier
        // writeback. Log the name so `writeback_carrier_writes` reconciles it (and
        // set env_dirty) — without this the write is lost once the blanket
        // reconcile is removed (mirrors the regex `:let` path).
        self.note_caller_env_write(&store_name);
        self.stack.push(value);
    }

    pub(super) fn exec_indirect_type_lookup_store_op(&mut self, code: &CompiledCode) {
        let name_val = self.stack.pop().unwrap_or(Value::Nil);
        let name = name_val.to_string_value();
        let value = self.stack.pop().unwrap_or(Value::Nil);
        // ::('$x') stores into the variable named $x.
        // Scalars are stored without the '$' sigil, so strip it.
        // Arrays (@) and hashes (%) are stored with their sigil.
        let store_name = if let Some(bare) = name.strip_prefix('$') {
            bare.to_string()
        } else {
            name.to_string()
        };
        self.env_mut().insert(store_name.clone(), value.clone());
        self.update_local_if_exists(code, &store_name, &value);
        // env_dirty substrate: same as exec_symbolic_deref_store_op — `::('$x') = v`
        // writes the target lexical by name, so log it for the carrier writeback.
        self.note_caller_env_write(&store_name);
        self.stack.push(value);
    }

    pub(super) fn exec_assign_expr_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // A whole array/hash assign (`@!a = (...)`) always replaces the value, so
        // there is no stale-copy hazard; pass `None` as the pre-snapshot to force
        // the mirror.
        let r = self.exec_assign_expr_op_inner(code, name_idx);
        // Phase 3 Stage 2: mirror name-based attribute writes into the shared cell.
        if r.is_ok()
            && let Value::Str(name) = &code.constants[name_idx as usize]
        {
            if name.starts_with('@') || name.starts_with('%') {
                self.mirror_array_hash_attr_to_cell(code, name_idx, None);
            } else {
                let name = name.to_string();
                self.mirror_attr_value_to_cell_by_name(code, &name);
            }
        }
        r
    }

    pub(super) fn exec_get_env_index_op(&mut self, code: &CompiledCode, key_idx: u32) {
        let key = Self::const_str(code, key_idx);
        let val = if let Some(Value::Hash(env_hash)) = self.env().get("%*ENV") {
            env_hash.get(key).cloned().unwrap_or_else(|| {
                std::env::var_os(key)
                    .map(|v| {
                        crate::runtime::builtins_collection::builtin_val(&[Value::str(
                            v.to_string_lossy().to_string(),
                        )])
                    })
                    .unwrap_or(Value::Nil)
            })
        } else if let Some(value) = std::env::var_os(key) {
            crate::runtime::builtins_collection::builtin_val(&[Value::str(
                value.to_string_lossy().to_string(),
            )])
        } else {
            Value::Nil
        };
        self.stack.push(val);
    }

    pub(super) fn exec_exists_env_index_op(&mut self, code: &CompiledCode, key_idx: u32) {
        let key = Self::const_str(code, key_idx);
        let exists = if let Some(Value::Hash(env_hash)) = self.env().get("%*ENV") {
            env_hash.contains_key(key) || std::env::var_os(key).is_some()
        } else {
            std::env::var_os(key).is_some()
        };
        self.stack.push(Value::Bool(exists));
    }

    pub(super) fn exec_exists_expr_op(&mut self) {
        let val = self.stack.pop().unwrap_or(Value::Nil);
        self.stack.push(Value::Bool(val.truthy()));
    }
}
