use super::*;

impl Interpreter {
    pub(crate) fn is_exceptional_block_exit(err: &RuntimeError) -> bool {
        if err.is_fail() {
            return true;
        }
        if err.return_value.is_some() {
            return false;
        }
        !(err.is_last()
            || err.is_next()
            || err.is_redo()
            || err.is_goto()
            || err.is_proceed()
            || err.is_succeed()
            || err.is_leave
            || err.is_resume()
            || err.is_react_done())
    }

    pub(super) fn should_run_success_queue(
        body_result: &Result<(), RuntimeError>,
        current_value: Option<Value>,
    ) -> bool {
        match body_result {
            Ok(()) => current_value.unwrap_or(Value::Nil).truthy(),
            Err(e) if !Self::is_exceptional_block_exit(e) => {
                e.return_value.clone().unwrap_or(Value::Nil).truthy()
            }
            Err(_) => false,
        }
    }

    /// Execute the CheckPhaser opcode: pop TOS, throw X::Phaser::PrePost if falsy.
    /// `condition` is the phaser condition's source text (e.g. `0`) when known.
    pub(super) fn exec_check_phaser_op(
        &mut self,
        is_pre: bool,
        condition: Option<String>,
    ) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap_or(Value::Nil);
        if !val.truthy() {
            let phaser_name = if is_pre { "PRE" } else { "POST" };
            let condition = condition.unwrap_or_default();
            let mut attrs = std::collections::HashMap::new();
            attrs.insert(
                "phaser".to_string(),
                Value::Str(Arc::new(phaser_name.to_string())),
            );
            attrs.insert(
                "condition".to_string(),
                Value::Str(Arc::new(condition.clone())),
            );
            let exception =
                Value::make_instance(crate::symbol::Symbol::intern("X::Phaser::PrePost"), attrs);
            // raku: "Precondition '<cond>' failed" / "Postcondition '<cond>' failed".
            let kind = if is_pre {
                "Precondition"
            } else {
                "Postcondition"
            };
            let mut err = RuntimeError::new(format!("{} '{}' failed", kind, condition));
            err.exception = Some(Box::new(exception));
            return Err(err);
        }
        Ok(())
    }

    pub(super) fn exec_do_block_expr_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        label: &Option<String>,
        scope_isolate: bool,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let body_start = *ip + 1;
        let end = body_end as usize;
        let label = label.clone();
        let stack_base = self.stack.len();
        let once_scope = self.next_once_scope_id();
        self.push_once_scope(once_scope);
        self.push_enum_scope();
        let saved_env = if scope_isolate {
            Some((self.env().clone(), self.locals.clone()))
        } else {
            None
        };
        // Only catch loop control signals (last/next/redo) when the do block
        // has an explicit label that matches. Unlabeled do blocks should let
        // these signals propagate to the enclosing loop construct.
        let has_label = label.is_some();
        let result = loop {
            match self.run_range(code, body_start, end, compiled_fns) {
                Ok(()) => break Ok(()),
                Err(e) if e.is_redo() && has_label && Self::label_matches(&e.label, &label) => {
                    self.stack.truncate(stack_base);
                    continue;
                }
                Err(e) if e.is_next() && has_label && Self::label_matches(&e.label, &label) => {
                    self.stack.truncate(stack_base);
                    self.stack.push(Value::Slip(std::sync::Arc::new(vec![])));
                    break Ok(());
                }
                Err(e)
                    if e.is_leave
                        && e.leave_callable_id.is_none()
                        && e.leave_routine.is_none()
                        && Self::label_matches(&e.label, &label) =>
                {
                    self.stack.truncate(stack_base);
                    self.stack.push(
                        e.return_value
                            .unwrap_or(Value::Slip(std::sync::Arc::new(vec![]))),
                    );
                    break Ok(());
                }
                Err(e) if e.is_last() && has_label && Self::label_matches(&e.label, &label) => {
                    self.stack.truncate(stack_base);
                    self.stack.push(
                        e.return_value
                            .unwrap_or(Value::Slip(std::sync::Arc::new(vec![]))),
                    );
                    break Ok(());
                }
                Err(e) => break Err(e),
            }
        };
        self.pop_enum_scope();
        self.pop_once_scope();
        // Restore scope if scope_isolate is true
        if let Some((saved_env, saved_locals)) = saved_env {
            let block_result = self.stack.pop().unwrap_or(Value::Nil);
            // Preserve hash variables declared or re-declared inside the
            // block so that inline `my %h` declarations (e.g. in
            // `:into(my %h := :{})`) remain visible in the outer scope.
            // Only hash variables are preserved to avoid side effects
            // on other container types.
            let current_env = self.env().clone();
            let mut new_vars: Vec<(Symbol, Value)> = Vec::new();
            for (name, value) in current_env.iter() {
                if !name.starts_with("%") || name.starts_with("%*") {
                    continue;
                }
                let is_new_or_changed = match saved_env.get_sym(*name) {
                    None => true,
                    Some(saved_val) => match (value, saved_val) {
                        (Value::Hash(a), Value::Hash(b)) => !std::sync::Arc::ptr_eq(a, b),
                        _ => std::mem::discriminant(value) != std::mem::discriminant(saved_val),
                    },
                };
                if is_new_or_changed {
                    new_vars.push((*name, value.clone()));
                }
            }
            let restored_env = saved_env.clone();
            *self.env_mut() = restored_env;
            // Re-insert newly declared user variables
            for (name, value) in new_vars {
                self.env_mut().insert_sym(name, value);
            }
            self.locals = saved_locals;
            // Re-read locals from env for variables that use env as the primary
            // store (non-simple locals). Simple locals use dirty-tracked SetLocal
            // and their saved_locals state is authoritative; re-reading from env
            // would overwrite them with stale env values (e.g. loop control flags
            // like `__mutsu_loop_first_N` that were updated inside the loop body
            // but not yet flushed to env before this DoBlock snapshot).
            for (idx, name) in code.locals.iter().enumerate() {
                if code.simple_locals[idx] {
                    // Simple locals: env may be stale; saved_locals is correct.
                    continue;
                }
                if let Some(val) = self.env().get(name).cloned() {
                    self.locals[idx] = val;
                }
            }
            self.stack.push(block_result);
        }
        *ip = end;
        result
    }

    pub(super) fn exec_once_expr_op(
        &mut self,
        code: &CompiledCode,
        key_idx: u32,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let scope =
            loan_env!(self, current_once_scope()).unwrap_or_else(|| self.next_once_scope_id());
        let site_key = Self::const_str(code, key_idx);
        let cache_key = format!("{scope}::{site_key}");
        if let Some(value) = self.get_once_value(&cache_key).cloned() {
            self.stack.push(value);
            *ip = body_end as usize;
            return Ok(());
        }

        let body_start = *ip + 1;
        let end = body_end as usize;
        let stack_base = self.stack.len();
        self.run_range(code, body_start, end, compiled_fns)?;
        let value = if self.stack.len() > stack_base {
            self.stack.pop().unwrap_or(Value::Nil)
        } else {
            Value::Nil
        };
        self.set_once_value(cache_key, value.clone());
        self.stack.push(value);
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_let_save_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        index_mode: bool,
        is_temp: bool,
    ) {
        let name = Self::const_str(code, name_idx).to_string();
        if index_mode {
            let _idx_val = self.stack.pop().unwrap_or(Value::Int(0));
        }
        let old_val = self
            .get_env_with_main_alias(&name)
            .or_else(|| {
                code.locals
                    .iter()
                    .position(|n| n == &name)
                    .map(|i| self.locals[i].clone())
            })
            .unwrap_or(Value::Nil);
        // A boxed (shared-cell) scalar saves its INNER value, decoupled from the
        // cell: otherwise the snapshot would be the same Arc that the dynamic-scope
        // write mutates, so the restore would see the modified value, not the
        // original (named-sub captured-outer boxing — see
        // docs/captured-outer-cell-sharing.md). The matching write-through restore
        // is in `restore_let_value`. Gated on the same toggle as the boxing it
        // supports, so the default build is byte-identical to before.
        let old_val = match old_val {
            Value::ContainerRef(arc) => arc.lock().unwrap().clone(),
            v => v,
        };
        // For temp, deep-copy Array/Hash so the snapshot is independent of
        // future mutations (Arc is shared, so a shallow clone wouldn't work).
        let save_val = if is_temp {
            Self::deep_copy_value(&old_val)
        } else {
            old_val
        };
        self.let_saves_push(name, save_val, is_temp);
    }

    /// Recursively deep-copy a Value so that Array/Hash snapshots are
    /// independent of future in-place mutations (the inner Arc would otherwise
    /// be shared).
    fn deep_copy_value(val: &Value) -> Value {
        match val {
            Value::Array(arc_vec, kind) => {
                let copied: Vec<Value> = arc_vec.iter().map(Self::deep_copy_value).collect();
                Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(copied)),
                    *kind,
                )
            }
            Value::Hash(arc_map) => {
                let copied: std::collections::HashMap<String, Value> = arc_map
                    .iter()
                    .map(|(k, v)| (k.clone(), Self::deep_copy_value(v)))
                    .collect();
                Value::Hash(Value::hash_arc(copied))
            }
            other => other.clone(),
        }
    }

    pub(super) fn exec_let_block_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let mark = self.let_saves_len();
        let body_start = *ip + 1;
        let end = body_end as usize;
        match self.run_range(code, body_start, end, compiled_fns) {
            Ok(()) => {
                let topic = self.env().get("_").cloned().unwrap_or(Value::Nil);
                let success = Self::is_let_success(&topic);
                loan_env!(self, resolve_let_saves_on_success(mark, success));
                // `let`/`temp` restore writes the saved value back into `env` only;
                // the matching local slot still holds the in-block value. The restore
                // recorded each restored name precisely (`restore_let_value`); drain
                // it so the frame's slots refresh.
                self.apply_pending_rw_writeback(code);
            }
            Err(e) => {
                loan_env!(self, restore_let_saves(mark));
                self.apply_pending_rw_writeback(code);
                return Err(e);
            }
        }
        *ip = end;
        Ok(())
    }
}
