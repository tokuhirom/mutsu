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
        // A `$` symbolic-deref store is a scalar assignment: it stores the whole
        // value (a list is held itemized), NOT just its first element.
        // `$::('x') = 1, 2` is already parsed item-assignment (RHS is `1`), so a
        // list value here is a genuine `$::('x') = (1, 2)` and must be kept whole.
        let value = if sigil == "$" {
            Self::normalize_scalar_assignment_value(raw_value)
        } else {
            raw_value
        };
        self.env_mut().insert(store_name.clone(), value.clone());
        self.update_local_if_exists(code, &store_name, &value);
        // A package-qualified symbolic store (`$::('Foo::b') = v`) must reach the
        // `our`-linked lexical alias (`$b`) immediately, like a direct
        // `$Foo::b = v`; also persist it in the package store so a later
        // bare/qualified read resolves it.
        self.set_our_var(store_name.clone(), value.clone());
        self.sync_our_local_from_qualified(code, &store_name, &value);
        // env_dirty substrate (docs/captured-outer-cell-sharing.md §10): a
        // symbolic-deref store (`$::($name) = v`) writes the target lexical by name
        // straight into env. `update_local_if_exists` refreshes the slot only when
        // it lives in *this* frame; inside a carrier (`lives-ok { $::($n) = v }`)
        // the owning slot is the carrier-caller's, reached only via the carrier
        // writeback. Log the name so `writeback_carrier_writes` reconciles it (and
        // set env_dirty) — without this the write is lost once the blanket
        // reconcile is removed (mirrors the regex `:let` path).
        self.note_caller_env_write(&store_name);
        // A `$` symbolic-deref store is a scalar assignment, so its rvalue is the
        // *itemized* container value: `flat ($::('x') = 1, 2), ...` keeps the
        // `(1,2)` as one element (matches a plain scalar assignment result).
        let result = if sigil == "$" {
            Self::itemize_value(value)
        } else {
            value
        };
        self.stack.push(result);
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

    /// `.=` metaop on the topic `$_` (`$_ = $_.meth`). Identical to `AssignExpr`
    /// of `_` except that it (1) bypasses the read-only mark a whole-container
    /// topic (`given @a`) places on `$_` — the `.=` metaop is always allowed even
    /// where a plain `$_ = ...` throws X::Assignment::RO — and (2) for such a
    /// whole-container topic, writes the reassigned value straight through to the
    /// `@`/`%` source with container-assignment coercion (`@a = "FOO"` => `["FOO"]`),
    /// so the mutation is visible immediately inside the block.
    pub(super) fn exec_topic_dot_assign_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // Only a whole-container topic (`given @a` / `with %h`) legitimately
        // bypasses the read-only mark: `$_` aliases a mutable `@`/`%` source and
        // the reassigned value is written straight through to it below. When the
        // topic is a genuinely immutable value (`for ^3`, `given 42`, `for 1,2,3`),
        // it is read-only with no write-through target, so `.=` must fail with the
        // usual "Cannot modify an immutable value" error rather than silently
        // bypassing the mark.
        let has_container_source = self.topic_container_source.is_some();
        let was_ro = self.is_readonly("_");
        let bypass = was_ro && has_container_source;
        if bypass {
            self.unmark_readonly("_");
        }
        let r = self.exec_assign_expr_op(code, name_idx);
        if bypass {
            self.mark_readonly("_");
        }
        r?;
        // Whole-container topic (`given @a`/`with %h`): `$_` aliases the container,
        // so the reassigned value is list-/hash-assigned back to the source. The
        // assignment result (the method value, e.g. `"FOO"`) is on the stack top.
        if let Some(src) = self.topic_container_source.clone() {
            let val = self.stack.last().cloned().unwrap_or(Value::Nil);
            let written = if src.starts_with('@') {
                if matches!(val, Value::Array(..)) {
                    val
                } else {
                    crate::runtime::utils::coerce_to_array(val)
                }
            } else if src.starts_with('%') {
                if matches!(val, Value::Hash(_)) {
                    val
                } else {
                    crate::runtime::utils::coerce_to_hash(val)
                }
            } else {
                val
            };
            self.set_env_with_main_alias(&src, written.clone());
            self.update_local_if_exists(code, &src, &written);
            // Keep `$_` (which aliases the container) and the expression value in
            // sync with the coerced container value.
            self.set_env_with_main_alias("_", written.clone());
            self.update_local_if_exists(code, "_", &written);
            if let Some(top) = self.stack.last_mut() {
                *top = written;
            }
        }
        Ok(())
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
