use super::*;

impl Interpreter {
    pub(super) fn exec_num_coerce_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        // Auto-FETCH Proxy containers
        let val = loan_env!(self, auto_fetch_proxy(&val))?;
        // Junction auto-threading for prefix:<+>
        if let Value::Junction { kind, values } = &val {
            let kind = kind.clone();
            let mut results = Vec::new();
            for v in values.iter() {
                self.stack.push(v.clone());
                self.exec_num_coerce_op()?;
                results.push(self.stack.pop().unwrap_or(Value::Nil));
            }
            self.stack.push(Value::junction(kind, results));
            return Ok(());
        }
        // A lazy (infinite-backed) array/list numerifies to its element count,
        // which it cannot report: raku yields an `X::Cannot::Lazy` Failure for
        // prefix `+` (`Cannot .elems a lazy list`).
        if crate::builtins::methods_0arg::is_lazy_count_source(&val) {
            self.stack
                .push(crate::runtime::utils::cannot_lazy_failure("elems"));
            return Ok(());
        }
        // Type objects (Mu, Any, etc.) cannot be numerically coerced
        if let Value::Package(name) = &val
            && matches!(name.resolve().as_str(), "Mu" | "Any")
        {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller prefix:<+>({}:U); none of these signatures matches:\n    (\\a)",
                name.resolve()
            )));
        }
        if matches!(
            &val,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
        ) {
            return Err(RuntimeError::new(
                "Cannot resolve caller Numeric(Sub:D: ); none of these signatures matches:\n    (Mu:U \\v: *%_)",
            ));
        }
        // If the value is an Instance, try calling the Numeric method
        if let Value::Instance { .. } = &val {
            // Slice F: a user `Numeric` method can mutate a captured-outer caller
            // lexical (`my $c; method Numeric { $c++; ... }`); this op-level
            // redispatch has no surrounding CallMethod op to drain the writeback,
            // so capture the caller frame's code and reconcile after (see
            // coerce_numeric_bridge_value).
            let caller_code = self.current_code;
            let result = self.try_compiled_method_or_interpret(val.clone(), "Numeric", vec![]);
            self.reconcile_caller_after_internal_dispatch(caller_code);
            if let Ok(result) = result {
                self.stack.push(result);
                return Ok(());
            }
        }
        // Force a lazy IO words/lines iterator so numeric coercion counts its
        // elements (e.g. `+$fh.words` / `+$fh.lines`) rather than yielding 0.
        let val = if matches!(&val, Value::LazyIoLines { .. }) {
            self.force_if_lazy_io_lines(val)?
        } else {
            val
        };
        // Force LazyList before numeric coercion so we can count elements.
        // If the lazy list has a known element count (e.g. n! for permutations),
        // use that directly without materializing.
        let val = if let Value::LazyList(ll) = &val {
            if let Some(count) = &ll.elems_count {
                self.stack.push(count.clone());
                return Ok(());
            }
            let items = self.force_lazy_list_vm(ll)?;
            Value::Seq(std::sync::Arc::new(items))
        } else {
            val
        };
        if let Value::Str(s) = &val {
            let trimmed = s.trim();
            if trimmed.is_empty() {
                self.stack.push(Value::Int(0));
                return Ok(());
            }
            if let Some(v) = crate::runtime::str_numeric::parse_raku_str_to_numeric(trimmed) {
                self.stack.push(v);
            } else {
                // A bad numeric string yields a lazy `X::Str::Numeric` Failure
                // carrying `source`/`pos`/`reason`/`source-indicator` attributes.
                self.stack
                    .push(crate::builtins::methods_0arg::str_numeric_failure(s));
            }
            return Ok(());
        }
        let result = crate::runtime::utils::coerce_to_numeric(val);
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_str_coerce_op(&mut self) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap();
        // Auto-FETCH Proxy containers
        let val = loan_env!(self, auto_fetch_proxy(&val))?;
        // Mu itself has no Str candidate — stringifying it is a hard
        // error (Rakudo dies with `Cannot resolve caller prefix:<~>(Mu:U)`).
        if let Value::Package(name) = &val
            && name.resolve() == "Mu"
        {
            return Err(RuntimeError::new(format!(
                "Cannot resolve caller prefix:<~>({}:U); none of these signatures matches:\n    (\\a)",
                name.resolve()
            )));
        }
        // Any and other Cool-descended type objects stringify to the empty
        // string with a warning (Rakudo emits `Use of uninitialized value
        // of type Any in string context.`).
        if let Value::Package(name) = &val
            && name.resolve() == "Any"
        {
            let msg = format!(
                "Use of uninitialized value of type {} in string context.\nMethods .^name, .raku, .gist, or .say can be used to stringify it to something meaningful.",
                name.resolve()
            );
            return Err(RuntimeError::warn_signal_with_resume(
                msg,
                Value::str(String::new()),
            ));
        }
        // Stringifying an unhandled Failure throws
        if let Some(err) = self.failure_to_runtime_error_if_unhandled(&val) {
            return Err(err);
        }
        // Check for user-defined prefix:<~> multi sub first (operator overloading).
        // This must come before .Stringy()/.Str() to avoid infinite recursion when
        // .Stringy() is defined as `{ ~self }` which delegates to prefix:<~>.
        {
            let args = vec![val.clone()];
            if let Some(def) = loan_env!(self, resolve_function_with_types("prefix:<~>", &args)) {
                let empty_fns = std::collections::HashMap::new();
                let result = self.compile_and_call_function_def(&def, args, &empty_fns)?;
                self.stack.push(result);
                return Ok(());
            }
        }
        // If the value is an Instance, try calling the Stringy method, then Str
        if let Value::Instance { .. } = &val {
            // Slice F: a user `Stringy`/`Str` method can mutate a captured-outer
            // caller lexical; reconcile its writeback to the caller's slot (see
            // coerce_numeric_bridge_value).
            let caller_code = self.current_code;
            let stringy = self.try_compiled_method_or_interpret(val.clone(), "Stringy", vec![]);
            self.reconcile_caller_after_internal_dispatch(caller_code);
            if let Ok(result) = stringy {
                self.stack.push(result);
                return Ok(());
            }
            let caller_code = self.current_code;
            let str_r = self.try_compiled_method_or_interpret(val.clone(), "Str", vec![]);
            self.reconcile_caller_after_internal_dispatch(caller_code);
            if let Ok(result) = str_r {
                self.stack.push(result);
                return Ok(());
            }
        }
        // Force LazyList before stringification
        if let Value::LazyList(_) = &val {
            let result = self.try_compiled_method_or_interpret(val, "Str", vec![])?;
            self.stack.push(result);
            return Ok(());
        }
        // Resolve bound-element sentinels inside arrays before stringification
        let val = self.resolve_bound_array_elements(val);
        self.stack
            .push(Value::str(crate::runtime::utils::coerce_to_str(&val)));
        Ok(())
    }

    pub(super) fn exec_upto_range_op(&mut self) {
        let val = Self::scalarize_range_endpoint(self.stack.pop().unwrap());
        let numeric = if val.is_numeric() {
            val
        } else {
            runtime::coerce_to_numeric(val)
        };
        let result = match numeric {
            Value::Int(i) => Value::RangeExcl(0, i),
            Value::Num(_)
            | Value::Rat(_, _)
            | Value::FatRat(_, _)
            | Value::BigRat(_, _)
            | Value::BigInt(_) => Value::GenericRange {
                start: Arc::new(Value::Int(0)),
                end: Arc::new(numeric),
                excl_start: false,
                excl_end: true,
            },
            _ => Value::RangeExcl(0, 0),
        };
        self.stack.push(result);
    }

    pub(super) fn exec_pre_increment_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // Phase 3 Stage 2: scalar attribute increments read-modify-write the cell.
        let attr_name = Self::const_str(code, name_idx).to_string();
        self.sync_attr_local_from_cell_by_name(code, &attr_name);
        let r = self.exec_pre_increment_op_inner(code, name_idx);
        if r.is_ok() {
            self.mirror_attr_local_to_cell_by_name(code, &attr_name);
        }
        r
    }

    fn exec_pre_increment_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        if let Some(r) = self.try_slotless_attr_incdec(code, name, true, true) {
            return r;
        }
        if name.starts_with('!')
            && let Some(slot) = self.find_local_slot(code, name)
            && !matches!(self.locals[slot], Value::Proxy { .. })
        {
            // ContainerRef: increment through the shared arc (e.g. `$!attr := outer_var`).
            if let Value::ContainerRef(ref arc) = self.locals[slot].clone() {
                if self.atomic_container_incdec(arc, name, true, false) {
                    return Ok(());
                }
                let inner = arc.lock().unwrap().clone();
                let val = self.normalize_incdec_source_with_type(name, inner);
                let new_val = self.increment_value_smart(&val)?;
                arc.lock().unwrap().clone_from(&new_val);
                self.stack.push(new_val);
                return Ok(());
            }
            let raw_val = self.locals[slot].clone();
            let val = self.normalize_incdec_source_with_type(name, raw_val);
            let new_val = self.increment_value_smart(&val)?;
            self.locals[slot] = new_val.clone();
            self.flush_local_to_env(code, slot);
            // Propagate via sigilless alias chain (e.g. `$!attr := outer_var`).
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
                self.set_env_with_main_alias(&current_alias, new_val.clone());
                self.update_local_if_exists(code, &current_alias, &new_val);
                let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
                alias_name = self.env().get(&next_key).and_then(|v| {
                    if let Value::Str(n) = v {
                        Some(n.to_string())
                    } else {
                        None
                    }
                });
            }
            self.stack.push(new_val);
            return Ok(());
        }
        let val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        // ContainerRef (box-on-capture / `:=`): mutate the shared cell in place so
        // closures over this lexical observe the change and the smart string/Int
        // increment semantics are preserved (the slot holds the same Arc).
        if let Value::ContainerRef(ref arc) = val {
            if self.atomic_container_incdec(arc, name, true, false) {
                return Ok(());
            }
            let inner = arc.lock().unwrap().clone();
            let v = self.normalize_incdec_source_with_type(name, inner);
            let new_val = self.increment_value_smart(&v)?;
            arc.lock().unwrap().clone_from(&new_val);
            self.stack.push(new_val);
            return Ok(());
        }
        let val = self.normalize_incdec_source_with_type(name, val);
        let new_val = self.increment_value_smart(&val)?;
        self.check_incdec_type_constraint(name, &new_val)?;
        self.set_env_with_main_alias(name, new_val.clone());
        self.sync_anon_state_value(name, &new_val);
        self.update_local_if_exists(code, name, &new_val);
        self.stack.push(new_val);
        Ok(())
    }

    pub(super) fn exec_pre_decrement_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        // Phase 3 Stage 2: scalar attribute decrements read-modify-write the cell.
        let attr_name = Self::const_str(code, name_idx).to_string();
        self.sync_attr_local_from_cell_by_name(code, &attr_name);
        let r = self.exec_pre_decrement_op_inner(code, name_idx);
        if r.is_ok() {
            self.mirror_attr_local_to_cell_by_name(code, &attr_name);
        }
        r
    }

    fn exec_pre_decrement_op_inner(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx);
        if let Some(r) = self.try_slotless_attr_incdec(code, name, false, true) {
            return r;
        }
        if name.starts_with('!')
            && let Some(slot) = self.find_local_slot(code, name)
            && !matches!(self.locals[slot], Value::Proxy { .. })
        {
            // ContainerRef: decrement through the shared arc (e.g. `$!attr := outer_var`).
            if let Value::ContainerRef(ref arc) = self.locals[slot].clone() {
                if self.atomic_container_incdec(arc, name, false, false) {
                    return Ok(());
                }
                let inner = arc.lock().unwrap().clone();
                let val = self.normalize_incdec_source_with_type(name, inner);
                let new_val = self.decrement_value_smart(&val)?;
                arc.lock().unwrap().clone_from(&new_val);
                self.stack.push(new_val);
                return Ok(());
            }
            let raw_val = self.locals[slot].clone();
            let val = self.normalize_incdec_source_with_type(name, raw_val);
            let new_val = self.decrement_value_smart(&val)?;
            self.locals[slot] = new_val.clone();
            self.flush_local_to_env(code, slot);
            // Propagate via sigilless alias chain (e.g. `$!attr := outer_var`).
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
                self.set_env_with_main_alias(&current_alias, new_val.clone());
                self.update_local_if_exists(code, &current_alias, &new_val);
                let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
                alias_name = self.env().get(&next_key).and_then(|v| {
                    if let Value::Str(n) = v {
                        Some(n.to_string())
                    } else {
                        None
                    }
                });
            }
            self.stack.push(new_val);
            return Ok(());
        }
        let val = self
            .get_env_with_main_alias(name)
            .or_else(|| self.anon_state_value(name))
            .unwrap_or(Value::Int(0));
        // ContainerRef (box-on-capture / `:=`): mutate the shared cell in place so
        // closures over this lexical observe the change and the smart string/Int
        // decrement semantics are preserved (the slot holds the same Arc).
        if let Value::ContainerRef(ref arc) = val {
            if self.atomic_container_incdec(arc, name, false, false) {
                return Ok(());
            }
            let inner = arc.lock().unwrap().clone();
            let v = self.normalize_incdec_source_with_type(name, inner);
            let new_val = self.decrement_value_smart(&v)?;
            arc.lock().unwrap().clone_from(&new_val);
            self.stack.push(new_val);
            return Ok(());
        }
        let val = self.normalize_incdec_source_with_type(name, val);
        let new_val = self.decrement_value_smart(&val)?;
        self.check_incdec_type_constraint(name, &new_val)?;
        self.set_env_with_main_alias(name, new_val.clone());
        self.sync_anon_state_value(name, &new_val);
        self.update_local_if_exists(code, name, &new_val);
        self.stack.push(new_val);
        Ok(())
    }
}
