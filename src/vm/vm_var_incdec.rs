use super::*;
use unicode_normalization::UnicodeNormalization;

impl Interpreter {
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
    pub(super) fn propagate_sigilless_alias_chain(
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
            B::Concat => self.exec_concat_op()?,
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
}
