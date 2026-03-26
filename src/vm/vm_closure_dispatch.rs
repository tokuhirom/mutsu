use super::*;

impl VM {
    /// Find the first positional argument that is a Junction and whose corresponding
    /// parameter type constraint does not accept Junction (i.e., needs auto-threading).
    /// Returns the index of that argument, or None if no auto-threading is needed.
    fn find_junction_autothread_arg(
        &self,
        data: &crate::value::SubData,
        args: &[Value],
    ) -> Option<usize> {
        // Collect positional param_defs (skip named)
        let positional_params: Vec<&crate::ast::ParamDef> = data
            .param_defs
            .iter()
            .filter(|pd| !pd.named && !pd.slurpy && !pd.double_slurpy)
            .collect();

        // Also filter out named args from the args list to get positional args
        let mut positional_idx = 0usize;
        for (i, arg) in args.iter().enumerate() {
            // Skip named args (pairs where key matches a named param)
            if let Value::Pair(_, _) = arg {
                // Check if this is a named argument
                let is_named = data.param_defs.iter().any(|pd| {
                    pd.named
                        && if let Value::Pair(key, _) = arg {
                            pd.name.trim_start_matches('$').trim_start_matches(':') == key.as_str()
                        } else {
                            false
                        }
                });
                if is_named {
                    continue;
                }
            }

            if let Value::Junction { .. } = arg {
                // Check if the corresponding param accepts Junction
                if let Some(pd) = positional_params.get(positional_idx) {
                    let constraint = pd.type_constraint.as_deref().unwrap_or(
                        if pd.name.starts_with('$') || pd.name.is_empty() {
                            "Any" // implicit Any for $-sigiled params
                        } else {
                            "Mu" // no constraint for other sigils
                        },
                    );
                    // Mu and Junction accept junctions directly
                    if constraint != "Mu" && constraint != "Junction" {
                        return Some(i);
                    }
                }
                // If no param_def found (extra args), implicit Any rejects Junction
                else if positional_idx >= positional_params.len() {
                    // Slurpy or extra - don't auto-thread
                }
            }
            positional_idx += 1;
        }
        None
    }

    /// Call a compiled closure (Value::Sub with compiled_code).
    pub(super) fn call_compiled_closure(
        &mut self,
        data: &crate::value::SubData,
        cc: &CompiledCode,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        let (mut args, callsite_line) = self.interpreter.sanitize_call_args(&args);
        if callsite_line.is_some() {
            self.interpreter.set_pending_callsite_line(callsite_line);
        }

        // Apply assumed args from .assuming() (same logic as tree-walker call_sub_value)
        if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
            let mut positional = Vec::new();
            let mut named = data.assumed_named.clone();
            let mut incoming_positional = Vec::new();
            for arg in &args {
                if let Value::Pair(key, boxed) = arg {
                    named.insert(key.clone(), *boxed.clone());
                } else {
                    incoming_positional.push(arg.clone());
                }
            }
            let mut incoming_idx = 0usize;
            for assumed in &data.assumed_positional {
                let is_placeholder = matches!(assumed, Value::Whatever)
                    || matches!(assumed, Value::Num(f) if f.is_infinite())
                    || matches!(assumed, Value::Rat(_, 0));
                if is_placeholder {
                    if incoming_idx < incoming_positional.len() {
                        positional.push(incoming_positional[incoming_idx].clone());
                        incoming_idx += 1;
                    }
                } else {
                    positional.push(assumed.clone());
                }
            }
            positional.extend(incoming_positional.into_iter().skip(incoming_idx));
            for (key, value) in named {
                positional.push(Value::Pair(key, Box::new(value)));
            }
            args = positional;
        }

        // Junction auto-threading: if any positional argument is a Junction and the
        // corresponding parameter's type constraint does not accept Junction (i.e., is
        // not Mu or Junction), call the closure once per eigenvalue and collect results
        // into a new Junction of the same kind.
        if let Some(junction_idx) = self.find_junction_autothread_arg(data, &args)
            && let Value::Junction { kind, values } = &args[junction_idx]
        {
            let kind = kind.clone();
            let values = values.clone();
            let mut results = Vec::with_capacity(values.len());
            for eigenvalue in values.iter() {
                let mut threaded_args = args.clone();
                threaded_args[junction_idx] = eigenvalue.clone();
                results.push(self.call_compiled_closure(data, cc, threaded_args, compiled_fns)?);
            }
            return Ok(Value::junction(kind, results));
        }

        self.push_call_frame();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;
        let saved_state_scope = self.state_scope_id;
        self.state_scope_id = Some(data.id);

        self.interpreter.inject_pending_callsite_line();

        // Merge captured environment into current env (or_insert = don't overwrite existing)
        for (k, v) in &data.env {
            self.interpreter
                .env_mut()
                .entry(k.clone())
                .or_insert_with(|| v.clone());
        }

        self.interpreter.push_caller_env();

        // Push Sub value to block_stack for callframe().code
        // Also set &?BLOCK as a weak self-reference (mirrors resolution.rs)
        let block_arc = std::sync::Arc::new(crate::value::SubData {
            package: data.package,
            name: data.name,
            params: data.params.clone(),
            param_defs: data.param_defs.clone(),
            body: vec![],
            is_rw: data.is_rw,
            is_raw: data.is_raw,
            env: data.env.clone(),
            assumed_positional: data.assumed_positional.clone(),
            assumed_named: data.assumed_named.clone(),
            id: data.id,
            empty_sig: data.empty_sig,
            is_bare_block: data.is_bare_block,
            compiled_code: data.compiled_code.clone(),
            deprecated_message: data.deprecated_message.clone(),
            source_line: data.source_line,
            source_file: data.source_file.clone(),
        });
        self.interpreter.env_mut().insert(
            "&?BLOCK".to_string(),
            Value::WeakSub(std::sync::Arc::downgrade(&block_arc)),
        );
        self.interpreter.push_block(Value::Sub(block_arc));

        // Push routine info for leave/return/when targeting.
        // For pointy blocks, we push a special marker name so that
        // &?ROUTINE can skip it and see the enclosing routine.
        if cc.is_pointy_block {
            self.interpreter
                .push_routine(data.package.resolve(), "<pointy-block>".to_string());
        } else {
            self.interpreter
                .push_routine(data.package.resolve(), data.name.resolve());
        }
        self.interpreter.env_mut().insert(
            "__mutsu_callable_id".to_string(),
            Value::Int(data.id as i64),
        );

        if data.empty_sig && !args.is_empty() {
            self.interpreter.pop_routine();
            self.interpreter.pop_block();
            self.interpreter.pop_caller_env();
            self.stack.truncate(saved_stack_depth);
            let frame = self.pop_call_frame();
            *self.interpreter.env_mut() = frame.saved_env;
            return Err(Interpreter::reject_args_for_empty_sig(&args));
        }

        // Bind parameters
        let rw_bindings =
            match self
                .interpreter
                .bind_function_args_values(&data.param_defs, &data.params, &args)
            {
                Ok(bindings) => bindings,
                Err(e) => {
                    self.interpreter.pop_routine();
                    self.interpreter.pop_block();
                    self.interpreter.pop_caller_env();
                    self.stack.truncate(saved_stack_depth);
                    let frame = self.pop_call_frame();
                    *self.interpreter.env_mut() = frame.saved_env;
                    return Err(Interpreter::enhance_binding_error(
                        e,
                        &data.name.resolve(),
                        &data.param_defs,
                        &args,
                    ));
                }
            };

        // Handle implicit $_ for bare blocks (no explicit params, single arg)
        let uses_positional = data.params.iter().any(|p| p != "_" && !p.starts_with(':'));
        if !uses_positional
            && !data.params.is_empty()
            && data.params.iter().all(|p| p.starts_with('$'))
        {
            // Named params with placeholders: handled by bind_function_args_values
        } else if !uses_positional && !args.is_empty() {
            if let Some(first) = args.iter().find(|v| !matches!(v, Value::Pair(_, _))) {
                self.interpreter
                    .env_mut()
                    .insert("_".to_string(), first.clone());
            }
        } else if data.params.is_empty() && args.is_empty() && data.name == "" {
            let caller_topic = self.call_frames.last().unwrap().saved_env.get("_").cloned();
            if let Some(topic) = caller_topic {
                self.interpreter.env_mut().insert("_".to_string(), topic);
            }
        }

        // Raku: $! is scoped per routine — fresh Nil on entry
        if !data.name.resolve().is_empty() {
            self.interpreter
                .env_mut()
                .insert("!".to_string(), Value::Nil);
        }

        self.locals = vec![Value::Nil; cc.locals.len()];
        for (i, local_name) in cc.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Load persisted state variable values using scoped keys
        // (state_scope_id is set to data.id above, so scoped_state_key
        // will generate closure-instance-specific keys automatically).
        for (slot, key) in &cc.state_locals {
            let scoped_key = self.scoped_state_key(key);
            if let Some(val) = self.interpreter.get_state_var(&scoped_key) {
                self.locals[*slot] = val.clone();
            }
        }

        let let_mark = self.interpreter.let_saves_len();
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        while ip < cc.ops.len() {
            match self.exec_one(cc, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(mut e) if e.is_leave => {
                    let routine_key = format!("{}::{}", data.package, data.name);
                    let matches_frame = if let Some(target_id) = e.leave_callable_id {
                        target_id == data.id
                    } else if let Some(target_routine) = e.leave_routine.as_ref() {
                        target_routine == &routine_key
                    } else {
                        e.label.is_none()
                    };
                    if matches_frame {
                        e.is_leave = false;
                        e.is_last = false;
                        let ret_val = e.return_value.unwrap_or(Value::Nil);
                        explicit_return = Some(ret_val.clone());
                        self.stack.truncate(saved_stack_depth);
                        self.stack.push(ret_val);
                        self.interpreter.discard_let_saves(let_mark);
                        result = Ok(());
                        break;
                    }
                    self.interpreter.restore_let_saves(let_mark);
                    result = Err(e);
                    break;
                }
                Err(e) if e.is_succeed => {
                    // `when`/`default` succeed signals are caught at the
                    // enclosing block boundary (sub, method, or pointy block).
                    let ret_val = e.return_value.unwrap_or(Value::Nil);
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.interpreter.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(e) if e.return_value.is_some() => {
                    // Pointy blocks (`-> { }`) are NOT routine boundaries.
                    // `return` in a pointy block propagates up to the
                    // enclosing routine (sub/method).
                    if cc.is_pointy_block {
                        self.interpreter.restore_let_saves(let_mark);
                        result = Err(e);
                        break;
                    }
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.interpreter.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
                    fail_bypass = true;
                    let failure = self.interpreter.fail_error_to_failure_value(&e);
                    self.interpreter.restore_let_saves(let_mark);
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    self.interpreter.restore_let_saves(let_mark);
                    result = Err(e);
                    break;
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        // Sync state variables back using scoped keys
        for (slot, key) in &cc.state_locals {
            let local_name = &cc.locals[*slot];
            let val = self
                .interpreter
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            let scoped_key = self.scoped_state_key(key);
            self.interpreter.set_state_var(scoped_key, val);
        }

        // Restore the previous state scope
        self.state_scope_id = saved_state_scope;

        self.interpreter.pop_routine();
        self.interpreter.pop_block();

        if self.env_dirty {
            self.sync_locals_from_env(cc);
        }

        // Sync locals back to env so captured variable changes are visible
        for (i, local_name) in cc.locals.iter().enumerate() {
            if !local_name.is_empty() {
                self.interpreter
                    .env_mut()
                    .insert(local_name.clone(), self.locals[i].clone());
            }
        }

        // Environment writeback: merge changes back to caller
        let frame = self.pop_call_frame();
        let mut restored_env = frame.saved_env;
        self.interpreter
            .pop_caller_env_with_writeback(&mut restored_env);
        self.interpreter
            .apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        let rw_sources: std::collections::HashSet<String> = rw_bindings
            .iter()
            .map(|(_, source)| source.clone())
            .collect();
        let captured_names: std::collections::HashSet<&str> =
            data.env.keys().map(|s| s.as_str()).collect();
        // Write back captured-variable changes, but NOT the closure's own
        // parameters/locals (which live in cc.locals).  Without this filter,
        // recursive &?BLOCK calls clobber the outer frame's $n, etc.
        let local_names: std::collections::HashSet<&str> =
            cc.locals.iter().map(|s| s.as_str()).collect();
        for (k, v) in self.interpreter.env().iter() {
            if k != "_"
                && k != "@_"
                && !rw_sources.contains(k)
                && (restored_env.contains_key(k)
                    || captured_names.contains(k.as_str())
                    || k.starts_with("__mutsu_predictive_seq_iter::")
                    || k.starts_with("__mutsu_sigilless_alias::!"))
                && (!local_names.contains(k.as_str()) || captured_names.contains(k.as_str()))
            {
                restored_env.insert(k.clone(), v.clone());
            }
        }
        self.interpreter
            .merge_sigilless_alias_writes(&mut restored_env, self.interpreter.env());
        *self.interpreter.env_mut() = restored_env;

        let return_spec = data.env.get("__mutsu_return_type").and_then(|v| match v {
            Value::Str(s) => Some(s.to_string()),
            _ => None,
        });

        match result {
            Ok(()) if fail_bypass => Ok(ret_val),
            Ok(()) => {
                // For closures, absorb `return` — don't re-propagate as error.
                let base_val = explicit_return.unwrap_or(ret_val);
                self.interpreter
                    .finalize_return_with_spec(Ok(base_val), return_spec.as_deref())
            }
            Err(e) => Err(e),
        }
    }
}
