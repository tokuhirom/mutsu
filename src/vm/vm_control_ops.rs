use super::*;
use crate::symbol::Symbol;

pub(super) struct ForLoopSpec {
    pub(super) param_idx: Option<u32>,
    pub(super) param_local: Option<u32>,
    pub(super) body_end: u32,
    pub(super) label: Option<String>,
    pub(super) arity: u32,
    pub(super) collect: bool,
    pub(super) restore_topic: bool,
    pub(super) threaded: bool,
    pub(super) is_rw: bool,
    pub(super) do_writeback: bool,
    pub(super) rw_param_names: Vec<String>,
    pub(super) kv_mode: bool,
    pub(super) source_var_names: Vec<String>,
    pub(super) autothread_junctions: bool,
}

pub(super) struct WhileLoopSpec {
    pub(super) cond_end: u32,
    pub(super) body_end: u32,
    pub(super) label: Option<String>,
    pub(super) collect: bool,
    pub(super) isolate_topic: bool,
}

pub(super) struct CStyleLoopSpec {
    pub(super) cond_end: u32,
    pub(super) step_start: u32,
    pub(super) body_end: u32,
    pub(super) label: Option<String>,
    pub(super) collect: bool,
}

impl VM {
    fn collect_loop_value(coll: &mut Vec<Value>, value: Value) {
        match value {
            Value::Slip(items) => coll.extend(items.iter().cloned()),
            other => coll.push(other),
        }
    }

    fn control_signal_topic_value(signal: &RuntimeError) -> Option<Value> {
        let class_name = if signal.is_last {
            Some("CX::Last")
        } else if signal.is_next {
            Some("CX::Next")
        } else if signal.is_redo {
            Some("CX::Redo")
        } else if signal.is_proceed {
            Some("CX::Proceed")
        } else if signal.is_succeed {
            Some("CX::Succeed")
        } else if signal.is_warn {
            Some("CX::Warn")
        } else if signal.is_return {
            Some("CX::Return")
        } else {
            None
        }?;

        let mut attrs = std::collections::HashMap::new();
        attrs.insert(
            "message".to_string(),
            Value::str(if signal.message.is_empty() {
                class_name.to_string()
            } else {
                signal.message.clone()
            }),
        );
        if let Some(label) = signal.label.as_ref() {
            attrs.insert("label".to_string(), Value::str(label.clone()));
        }
        Some(Value::make_instance(Symbol::intern(class_name), attrs))
    }

    pub(super) fn exec_while_loop_op(
        &mut self,
        code: &CompiledCode,
        spec: &WhileLoopSpec,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let cond_start = *ip + 1;
        let body_start = spec.cond_end as usize;
        let loop_end = spec.body_end as usize;
        let stack_base = if spec.collect {
            Some(self.stack.len())
        } else {
            None
        };
        let mut collected = if spec.collect { Some(Vec::new()) } else { None };

        'while_loop: loop {
            self.run_range(code, cond_start, body_start, compiled_fns)?;
            let cond_val = self.stack.pop().unwrap();
            if !cond_val.truthy() {
                break;
            }
            let topic_before_body = if spec.isolate_topic {
                Some(self.interpreter.env().get("_").cloned())
            } else {
                None
            };
            'body_redo: loop {
                match self.run_range(code, body_start, loop_end, compiled_fns) {
                    Ok(()) => {
                        // Sync state variables modified in this iteration so
                        // the next iteration's StateVarInit sees updated values.
                        if !code.state_locals.is_empty() {
                            self.sync_state_locals_in_range(code, body_start, loop_end);
                        }
                        if let Some(saved_topic) = &topic_before_body {
                            if let Some(v) = saved_topic.clone() {
                                self.interpreter.env_mut().insert("_".to_string(), v);
                            } else {
                                self.interpreter.env_mut().remove("_");
                            }
                        }
                        if let Some(ref mut coll) = collected {
                            let base = stack_base.unwrap();
                            if self.stack.len() > base {
                                Self::collect_loop_value(coll, self.stack.pop().unwrap());
                            }
                            self.stack.truncate(base);
                        }
                        // Process pending DESTROY submethods at loop iteration boundaries,
                        // mimicking GC-like behavior so DESTROY fires during execution.
                        self.interpreter.run_pending_instance_destroys()?;
                        break 'body_redo;
                    }
                    Err(e) if e.is_succeed => {
                        if let Some(saved_topic) = &topic_before_body {
                            if let Some(v) = saved_topic.clone() {
                                self.interpreter.env_mut().insert("_".to_string(), v);
                            } else {
                                self.interpreter.env_mut().remove("_");
                            }
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo && Self::label_matches(&e.label, &spec.label) => {
                        if let Some(saved_topic) = &topic_before_body {
                            if let Some(v) = saved_topic.clone() {
                                self.interpreter.env_mut().insert("_".to_string(), v);
                            } else {
                                self.interpreter.env_mut().remove("_");
                            }
                        }
                        continue 'body_redo;
                    }
                    Err(e)
                        if e.is_leave
                            && e.leave_callable_id.is_none()
                            && e.leave_routine.is_none()
                            && Self::label_matches(&e.label, &spec.label) =>
                    {
                        if let Some(v) = e.return_value {
                            if let Some(ref mut coll) = collected {
                                Self::collect_loop_value(coll, v.clone());
                            } else {
                                self.interpreter.env_mut().insert("_".to_string(), v);
                            }
                        }
                        break 'while_loop;
                    }
                    Err(e) if e.is_last && Self::label_matches(&e.label, &spec.label) => {
                        break 'while_loop;
                    }
                    Err(e) if e.is_next && Self::label_matches(&e.label, &spec.label) => {
                        if let Some(saved_topic) = &topic_before_body {
                            if let Some(v) = saved_topic.clone() {
                                self.interpreter.env_mut().insert("_".to_string(), v);
                            } else {
                                self.interpreter.env_mut().remove("_");
                            }
                        }
                        break 'body_redo;
                    }
                    Err(e) => return Err(e),
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        if let Some(coll) = collected {
            self.stack.push(Value::array(coll));
        }
        *ip = loop_end;
        Ok(())
    }

    pub(super) fn exec_for_loop_op(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let iterable = self.stack.pop().unwrap();

        // Handle lazy IO lines: iterate by pulling one line at a time
        // so that $fh.tell reflects the current read position.
        if let Value::LazyIoLines { ref handle, kv } = iterable {
            let body_start = *ip + 1;
            let loop_end = spec.body_end as usize;
            self.exec_for_loop_lazy_io_lines(
                code,
                spec,
                handle,
                kv,
                body_start,
                loop_end,
                compiled_fns,
            )?;
            *ip = loop_end;
            return Ok(());
        }

        let raw_items = if let Value::LazyList(ref ll) = iterable {
            self.force_lazy_list_vm(ll)?
        } else {
            runtime::value_to_list(&iterable)
        };
        // When autothread_junctions is set (parameter typed as Any or more
        // specific), expand Junction items into their eigenstates so each
        // eigenstate is iterated separately.
        let items = if spec.autothread_junctions {
            let mut expanded = Vec::new();
            for item in raw_items {
                if let Value::Junction { values, .. } = &item {
                    for v in values.iter() {
                        expanded.push(v.clone());
                    }
                } else {
                    expanded.push(item);
                }
            }
            expanded
        } else {
            raw_items
        };
        self.env_dirty = true;
        let body_start = *ip + 1;
        let loop_end = spec.body_end as usize;

        if spec.threaded {
            // race for / hyper for: run the loop body in a spawned thread
            // so that $*THREAD.id returns a different value.
            let result = std::thread::scope(|s| {
                s.spawn(|| {
                    self.exec_for_loop_body(code, spec, &items, body_start, loop_end, compiled_fns)
                })
                .join()
                .unwrap_or_else(|_| Err(RuntimeError::new("thread panicked in race/hyper for")))
            });
            *ip = loop_end;
            return result;
        }

        self.exec_for_loop_body(code, spec, &items, body_start, loop_end, compiled_fns)?;
        *ip = loop_end;
        Ok(())
    }

    fn exec_for_loop_body(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        items: &[Value],
        body_start: usize,
        loop_end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let param_name = spec
            .param_idx
            .map(|idx| match &code.constants[idx as usize] {
                Value::Str(s) => s.to_string(),
                _ => unreachable!("ForLoop param must be a string constant"),
            });

        let arity = spec.arity.max(1) as usize;
        let writes_back_topic =
            spec.param_idx.is_none() && spec.param_local.is_none() && spec.arity <= 1;
        let rw_writeback = spec.do_writeback;
        let chunked_items: Vec<Value> = if arity > 1 {
            items
                .chunks(arity)
                .map(|chunk| Value::array(chunk.to_vec()))
                .collect()
        } else {
            items.to_vec()
        };
        let stack_base = if spec.collect {
            Some(self.stack.len())
        } else {
            None
        };
        let mut collected = if spec.collect { Some(Vec::new()) } else { None };
        let mut deferred_container_refs: Vec<(usize, String)> = Vec::new();
        let saved_topic = spec
            .restore_topic
            .then(|| self.interpreter.env().get("_").cloned())
            .flatten();
        let saved_topic_source = self.topic_source_var.take();
        let container_binding = self.container_ref_var.take();
        // Capture hash key order before the loop so writeback uses the
        // original key order even after the hash is mutated during iteration.
        let hash_keys_for_writeback: Option<Vec<String>> = if rw_writeback {
            container_binding
                .as_ref()
                .filter(|s| s.starts_with('%'))
                .and_then(|source| {
                    if let Some(Value::Hash(hash_items)) = self.get_env_with_main_alias(source) {
                        Some(hash_items.keys().cloned().collect())
                    } else {
                        None
                    }
                })
        } else {
            None
        };
        'for_loop: for (idx, item) in chunked_items.into_iter().enumerate() {
            self.topic_source_var = if writes_back_topic {
                container_binding.clone()
            } else {
                None
            };
            // Only set $_ when no named parameter is given (for @list { ... })
            // When -> $k is used, $_ should remain from the enclosing scope
            if param_name.is_none() {
                self.interpreter
                    .env_mut()
                    .insert("_".to_string(), item.clone());
            }
            if let Some(ref name) = param_name {
                self.interpreter
                    .env_mut()
                    .insert(name.clone(), item.clone());
                // Create non-twigil alias for placeholder params: $^a → $a
                if let Some(bare) = name.strip_prefix("&^") {
                    self.interpreter
                        .env_mut()
                        .insert(format!("&{}", bare), item.clone());
                } else if let Some(bare) = name.strip_prefix('^') {
                    self.interpreter
                        .env_mut()
                        .insert(bare.to_string(), item.clone());
                }
            }
            if let Some(slot) = spec.param_local {
                self.locals[slot as usize] = item.clone();
            }
            // Mark named params readonly when not in rw mode
            if !spec.is_rw
                && let Some(ref name) = param_name
            {
                self.interpreter.mark_readonly(name);
            }
            'body_redo: loop {
                match self.run_range(code, body_start, loop_end, compiled_fns) {
                    Ok(()) => {
                        // Sync state variables modified in this iteration so
                        // that StateVarInit in the next iteration sees the
                        // updated values.
                        if !code.state_locals.is_empty() {
                            self.sync_state_locals_in_range(code, body_start, loop_end);
                        }
                        if writes_back_topic {
                            self.write_back_for_topic_item(code, &container_binding, idx);
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &param_name,
                            idx,
                        );
                        if let Some(ref mut coll) = collected {
                            let base = stack_base.unwrap();
                            if self.stack.len() > base {
                                let val = self.stack.pop().unwrap();
                                let deferred_ref = self.container_ref_var.take();
                                let coll_start_len = coll.len();
                                Self::collect_loop_value(coll, val);
                                if let Some(name) = deferred_ref
                                    && coll.len() == coll_start_len + 1
                                {
                                    deferred_container_refs.push((coll_start_len, name));
                                }
                            }
                            // Drain any extra values pushed during this iteration
                            self.stack.truncate(base);
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_succeed => {
                        if writes_back_topic {
                            self.write_back_for_topic_item(code, &container_binding, idx);
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &param_name,
                            idx,
                        );
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo && Self::label_matches(&e.label, &spec.label) => {
                        if param_name.is_none() {
                            self.interpreter
                                .env_mut()
                                .insert("_".to_string(), item.clone());
                        }
                        if let Some(ref name) = param_name {
                            self.interpreter
                                .env_mut()
                                .insert(name.clone(), item.clone());
                        }
                        if let Some(slot) = spec.param_local {
                            self.locals[slot as usize] = item.clone();
                        }
                        continue 'body_redo;
                    }
                    Err(e)
                        if e.is_leave
                            && e.leave_callable_id.is_none()
                            && e.leave_routine.is_none()
                            && Self::label_matches(&e.label, &spec.label) =>
                    {
                        if writes_back_topic {
                            self.write_back_for_topic_item(code, &container_binding, idx);
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &param_name,
                            idx,
                        );
                        if let Some(v) = e.return_value {
                            if let Some(ref mut coll) = collected {
                                Self::collect_loop_value(coll, v.clone());
                            } else {
                                self.interpreter
                                    .env_mut()
                                    .insert("_".to_string(), v.clone());
                                // Push return value on stack so enclosing compiled
                                // closures can see it as the block result.
                                self.stack.push(v);
                            }
                        }
                        break 'for_loop;
                    }
                    Err(e) if e.is_last && Self::label_matches(&e.label, &spec.label) => {
                        if writes_back_topic {
                            self.write_back_for_topic_item(code, &container_binding, idx);
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &param_name,
                            idx,
                        );
                        break 'for_loop;
                    }
                    Err(e) if e.is_next && Self::label_matches(&e.label, &spec.label) => {
                        if writes_back_topic {
                            self.write_back_for_topic_item(code, &container_binding, idx);
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &param_name,
                            idx,
                        );
                        break 'body_redo;
                    }
                    Err(e) => {
                        // Unmark readonly before propagating error
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.interpreter.readonly_vars_mut().remove(name);
                        }
                        if spec.restore_topic {
                            match saved_topic.clone() {
                                Some(v) => {
                                    self.interpreter.env_mut().insert("_".to_string(), v);
                                }
                                None => {
                                    self.interpreter.env_mut().remove("_");
                                }
                            }
                        }
                        return Err(e);
                    }
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        // Unmark readonly params after loop completion
        if !spec.is_rw
            && let Some(ref name) = param_name
        {
            self.interpreter.readonly_vars_mut().remove(name);
        }
        self.topic_source_var = saved_topic_source;
        if spec.restore_topic {
            match saved_topic {
                Some(v) => {
                    self.interpreter.env_mut().insert("_".to_string(), v);
                }
                None => {
                    self.interpreter.env_mut().remove("_");
                }
            }
        }
        if let Some(coll) = collected {
            let mut coll = coll;
            for (idx, name) in deferred_container_refs {
                if idx < coll.len()
                    && let Some(v) = self.get_env_with_main_alias(&name)
                {
                    coll[idx] = v;
                }
            }
            self.stack.push(Value::array(coll));
        }
        Ok(())
    }

    /// Iterate lazily over IO lines from a file handle.
    /// Reads one line at a time so that $fh.tell reflects the current position.
    #[allow(clippy::too_many_arguments)]
    fn exec_for_loop_lazy_io_lines(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        handle: &Value,
        kv: bool,
        body_start: usize,
        loop_end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let param_name = spec
            .param_idx
            .map(|idx| match &code.constants[idx as usize] {
                Value::Str(s) => s.to_string(),
                _ => unreachable!("ForLoop param must be a string constant"),
            });

        let arity = spec.arity.max(1) as usize;
        let _writes_back_topic =
            spec.param_idx.is_none() && spec.param_local.is_none() && spec.arity <= 1;
        let saved_topic = spec
            .restore_topic
            .then(|| self.interpreter.env().get("_").cloned())
            .flatten();
        let saved_topic_source = self.topic_source_var.take();
        let mut collected = if spec.collect { Some(Vec::new()) } else { None };
        let stack_base = if spec.collect {
            Some(self.stack.len())
        } else {
            None
        };

        self.env_dirty = true;
        let mut line_index: i64 = 0;

        'for_loop: loop {
            // Read the next line from the handle
            let line = self.interpreter.read_line_from_handle_value(handle)?;
            let Some(line_str) = line else {
                break 'for_loop; // EOF
            };
            let line_val = Value::str(line_str);

            // Build the item for this iteration
            let item = if kv {
                // .kv mode: produce [index, value] pairs.
                // With arity 2, chunk into pairs for `-> \k, \v`.
                if arity >= 2 {
                    // The for-loop expects individual items that get chunked.
                    // We produce two items per line: index and value.
                    // Run the body once with the pair as an array.
                    let pair = Value::array(vec![Value::Int(line_index), line_val]);
                    line_index += 1;
                    pair
                } else {
                    // arity 1: interleave index and value as separate iterations
                    // This is unusual for .kv in a for loop but handle it.
                    // We'd need to run the body twice per line, once for index
                    // and once for value. For now, produce a flat pair.
                    let pair = Value::array(vec![Value::Int(line_index), line_val]);
                    line_index += 1;
                    pair
                }
            } else {
                line_index += 1;
                line_val
            };

            // Set up parameters
            if param_name.is_none() {
                self.interpreter
                    .env_mut()
                    .insert("_".to_string(), item.clone());
            }
            if let Some(ref name) = param_name {
                self.interpreter
                    .env_mut()
                    .insert(name.clone(), item.clone());
                if let Some(bare) = name.strip_prefix("&^") {
                    self.interpreter
                        .env_mut()
                        .insert(format!("&{}", bare), item.clone());
                } else if let Some(bare) = name.strip_prefix('^') {
                    self.interpreter
                        .env_mut()
                        .insert(bare.to_string(), item.clone());
                }
            }
            if let Some(slot) = spec.param_local {
                self.locals[slot as usize] = item.clone();
            }
            if !spec.is_rw
                && let Some(ref name) = param_name
            {
                self.interpreter.mark_readonly(name);
            }

            'body_redo: loop {
                match self.run_range(code, body_start, loop_end, compiled_fns) {
                    Ok(()) => {
                        if !code.state_locals.is_empty() {
                            self.sync_state_locals_in_range(code, body_start, loop_end);
                        }
                        if let Some(ref mut coll) = collected {
                            let base = stack_base.unwrap();
                            if self.stack.len() > base {
                                let val = self.stack.pop().unwrap();
                                Self::collect_loop_value(coll, val);
                            }
                            self.stack.truncate(base);
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_succeed => {
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo && Self::label_matches(&e.label, &spec.label) => {
                        if param_name.is_none() {
                            self.interpreter
                                .env_mut()
                                .insert("_".to_string(), item.clone());
                        }
                        if let Some(ref name) = param_name {
                            self.interpreter
                                .env_mut()
                                .insert(name.clone(), item.clone());
                        }
                        if let Some(slot) = spec.param_local {
                            self.locals[slot as usize] = item.clone();
                        }
                        continue 'body_redo;
                    }
                    Err(e) if e.is_last && Self::label_matches(&e.label, &spec.label) => {
                        break 'for_loop;
                    }
                    Err(e) if e.is_next && Self::label_matches(&e.label, &spec.label) => {
                        break 'body_redo;
                    }
                    Err(e) => {
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.interpreter.readonly_vars_mut().remove(name);
                        }
                        if spec.restore_topic {
                            match saved_topic.clone() {
                                Some(v) => {
                                    self.interpreter.env_mut().insert("_".to_string(), v);
                                }
                                None => {
                                    self.interpreter.env_mut().remove("_");
                                }
                            }
                        }
                        return Err(e);
                    }
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }

        // Cleanup
        if !spec.is_rw
            && let Some(ref name) = param_name
        {
            self.interpreter.readonly_vars_mut().remove(name);
        }
        self.topic_source_var = saved_topic_source;
        if spec.restore_topic {
            match saved_topic {
                Some(v) => {
                    self.interpreter.env_mut().insert("_".to_string(), v);
                }
                None => {
                    self.interpreter.env_mut().remove("_");
                }
            }
        }
        if let Some(coll) = collected {
            self.stack.push(Value::array(coll));
        }
        Ok(())
    }

    fn write_back_for_topic_item(
        &mut self,
        code: &CompiledCode,
        source_var: &Option<String>,
        idx: usize,
    ) {
        let Some(source) = source_var else {
            return;
        };
        if !source.starts_with('@') {
            return;
        }
        let Some(current_topic) = self.interpreter.env().get("_").cloned() else {
            return;
        };
        let Some(Value::Array(items, kind)) = self.get_env_with_main_alias(source) else {
            return;
        };
        if idx >= items.len() {
            return;
        }
        let mut updated = items.to_vec();
        updated[idx] = current_topic;
        let updated_value = Value::Array(std::sync::Arc::new(updated), kind);
        self.set_env_with_main_alias(source, updated_value.clone());
        self.update_local_if_exists(code, source, &updated_value);
    }

    /// Write back modified loop variable to the original scalar variable.
    /// Used when iterating over a list of scalar variables like `for ($a, $b, $c)`.
    fn write_back_to_source_var(
        &mut self,
        code: &CompiledCode,
        source_var_names: &[String],
        param_name: &Option<String>,
        idx: usize,
    ) {
        if source_var_names.is_empty() || idx >= source_var_names.len() {
            return;
        }
        let var_name = param_name.as_deref().unwrap_or("_");
        let Some(current_val) = self.interpreter.env().get(var_name).cloned() else {
            return;
        };
        let target = &source_var_names[idx];
        self.interpreter
            .env_mut()
            .insert(target.clone(), current_val.clone());
        self.update_local_if_exists(code, target, &current_val);
    }

    /// Write back the named rw param to the source container at the given index.
    #[allow(clippy::too_many_arguments)]
    fn write_back_for_rw_param(
        &mut self,
        code: &CompiledCode,
        source_var: &Option<String>,
        param_name: &Option<String>,
        rw_param_names: &[String],
        idx: usize,
        arity: usize,
        kv_mode: bool,
        hash_keys: &Option<Vec<String>>,
    ) {
        let Some(source) = source_var else {
            return;
        };
        if source.starts_with('@') || source.starts_with('%') {
            // For hash sources, read as array for writeback
            let source_val = self.get_env_with_main_alias(source);
            let (items, kind) = if source.starts_with('@') {
                if let Some(Value::Array(items, kind)) = source_val {
                    (items, kind)
                } else {
                    return;
                }
            } else {
                // Hash: can't do positional writeback easily; use hash-specific logic
                if kv_mode && arity > 1 && rw_param_names.len() >= 2 {
                    // For %hash.kv -> $key, $val is rw: read $key and $val, update hash
                    let key_name = &rw_param_names[0];
                    let val_name = &rw_param_names[1];
                    if let Some(key) = self.interpreter.env().get(key_name).cloned()
                        && let Some(val) = self.interpreter.env().get(val_name).cloned()
                        && let Some(Value::Hash(hash_items)) = self.get_env_with_main_alias(source)
                    {
                        let mut updated = hash_items.as_ref().clone();
                        let key_str = key.to_string_value();
                        updated.insert(key_str, val);
                        let updated_value = Value::Hash(std::sync::Arc::new(updated));
                        self.set_env_with_main_alias(source, updated_value.clone());
                        self.update_local_if_exists(code, source, &updated_value);
                    }
                } else if !kv_mode {
                    // %hash.values -> $val is rw: positional writeback using pre-captured key order
                    let var_name = param_name.as_deref().unwrap_or("_");
                    if let Some(keys) = hash_keys
                        && let Some(val) = self.interpreter.env().get(var_name).cloned()
                        && let Some(Value::Hash(hash_items)) = self.get_env_with_main_alias(source)
                        && idx < keys.len()
                    {
                        let mut updated = hash_items.as_ref().clone();
                        updated.insert(keys[idx].clone(), val);
                        let updated_value = Value::Hash(std::sync::Arc::new(updated));
                        self.set_env_with_main_alias(source, updated_value.clone());
                        self.update_local_if_exists(code, source, &updated_value);
                    }
                }
                return;
            };
            if kv_mode && arity > 1 && rw_param_names.len() >= 2 {
                // .kv mode: chunk is [key, val]. Write back val at key position.
                let val_name = &rw_param_names[1];
                if let Some(val) = self.interpreter.env().get(val_name).cloned()
                    && idx < items.len()
                {
                    let mut updated = items.to_vec();
                    updated[idx] = val;
                    let updated_value = Value::Array(std::sync::Arc::new(updated), kind);
                    self.set_env_with_main_alias(source, updated_value.clone());
                    self.update_local_if_exists(code, source, &updated_value);
                }
            } else if arity > 1 && !rw_param_names.is_empty() {
                // Multi-param rw: read each named param and write back to the array
                let base = idx * arity;
                let mut updated = items.to_vec();
                for (j, pname) in rw_param_names.iter().enumerate() {
                    if base + j < updated.len()
                        && let Some(val) = self.interpreter.env().get(pname).cloned()
                    {
                        updated[base + j] = val;
                    }
                }
                let updated_value = Value::Array(std::sync::Arc::new(updated), kind);
                self.set_env_with_main_alias(source, updated_value.clone());
                self.update_local_if_exists(code, source, &updated_value);
            } else {
                // Single-param rw: read the named param (or $_) and write back
                let var_name = param_name.as_deref().unwrap_or("_");
                let Some(current_val) = self.interpreter.env().get(var_name).cloned() else {
                    return;
                };
                if idx >= items.len() {
                    return;
                }
                let mut updated = items.to_vec();
                updated[idx] = current_val;
                let updated_value = Value::Array(std::sync::Arc::new(updated), kind);
                self.set_env_with_main_alias(source, updated_value.clone());
                self.update_local_if_exists(code, source, &updated_value);
            }
        } else if source.starts_with('%') {
            // Hash writeback: not straightforward by index; skip for now
        } else {
            // Scalar binding: write back directly
            // For kv_mode with arity > 1 on a Pair source (e.g. `for $pair.kv -> $key, $value is rw`),
            // read the value from the second rw param and reconstruct the Pair.
            if kv_mode && arity > 1 && rw_param_names.len() >= 2 {
                if let Some(existing) = self.get_env_with_main_alias(source) {
                    let val_name = &rw_param_names[1];
                    if let Some(val) = self.interpreter.env().get(val_name).cloned() {
                        let writeback_val = match existing {
                            Value::Pair(key, _) => Value::Pair(key, Box::new(val)),
                            Value::ValuePair(key, _) => Value::ValuePair(key, Box::new(val)),
                            _ => return,
                        };
                        self.set_env_with_main_alias(source, writeback_val.clone());
                        self.update_local_if_exists(code, source, &writeback_val);
                    }
                }
                return;
            }
            let var_name = param_name.as_deref().unwrap_or("_");
            let Some(current_val) = self.interpreter.env().get(var_name).cloned() else {
                return;
            };
            // If the source variable holds a Pair, update only the pair's value
            // (this handles `for $pair.value -> $v is rw { ... }`)
            let writeback_val = if let Some(existing) = self.get_env_with_main_alias(source) {
                match existing {
                    Value::Pair(key, _) => Value::Pair(key, Box::new(current_val.clone())),
                    Value::ValuePair(key, _) => {
                        Value::ValuePair(key, Box::new(current_val.clone()))
                    }
                    _ => current_val.clone(),
                }
            } else {
                current_val.clone()
            };
            self.set_env_with_main_alias(source, writeback_val.clone());
            self.update_local_if_exists(code, source, &writeback_val);
        }
    }

    pub(super) fn exec_cstyle_loop_op(
        &mut self,
        code: &CompiledCode,
        spec: &CStyleLoopSpec,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let cond_start = *ip + 1;
        let body_start = spec.cond_end as usize;
        let step_begin = spec.step_start as usize;
        let loop_end = spec.body_end as usize;
        let stack_base = if spec.collect {
            Some(self.stack.len())
        } else {
            None
        };
        let mut collected = if spec.collect { Some(Vec::new()) } else { None };

        'c_loop: loop {
            self.run_range(code, cond_start, body_start, compiled_fns)?;
            let cond_val = self.stack.pop().unwrap();
            if !cond_val.truthy() {
                break;
            }
            'body_redo: loop {
                match self.run_range(code, body_start, step_begin, compiled_fns) {
                    Ok(()) => {
                        if !code.state_locals.is_empty() {
                            self.sync_state_locals_in_range(code, body_start, step_begin);
                        }
                        if let Some(ref mut coll) = collected {
                            let base = stack_base.unwrap();
                            if self.stack.len() > base {
                                Self::collect_loop_value(coll, self.stack.pop().unwrap());
                            }
                            self.stack.truncate(base);
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_succeed => {
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo && Self::label_matches(&e.label, &spec.label) => {
                        continue 'body_redo;
                    }
                    Err(e)
                        if e.is_leave
                            && e.leave_callable_id.is_none()
                            && e.leave_routine.is_none()
                            && Self::label_matches(&e.label, &spec.label) =>
                    {
                        if let Some(v) = e.return_value {
                            if let Some(ref mut coll) = collected {
                                Self::collect_loop_value(coll, v.clone());
                            } else {
                                self.interpreter.env_mut().insert("_".to_string(), v);
                            }
                        }
                        break 'c_loop;
                    }
                    Err(e) if e.is_last && Self::label_matches(&e.label, &spec.label) => {
                        break 'c_loop;
                    }
                    Err(e) if e.is_next && Self::label_matches(&e.label, &spec.label) => {
                        break 'body_redo;
                    }
                    Err(e) => return Err(e),
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
            self.run_range(code, step_begin, loop_end, compiled_fns)?;
        }
        if let Some(coll) = collected {
            self.stack.push(Value::array(coll));
        }
        *ip = loop_end;
        Ok(())
    }

    pub(super) fn exec_given_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let topic = self.stack.pop().unwrap();
        let body_start = *ip + 1;
        let end = body_end as usize;
        let stack_base = self.stack.len();

        let saved_topic = self.interpreter.env().get("_").cloned();
        let saved_when = self.interpreter.when_matched();
        let saved_topic_source = self.topic_source_var.take();
        let container_binding = self.container_ref_var.take();
        self.topic_source_var = container_binding.clone();
        self.interpreter.env_mut().insert("_".to_string(), topic);
        self.interpreter.set_when_matched(false);

        let mut inner_ip = body_start;
        while inner_ip < end {
            if let Err(e) = self.exec_one(code, &mut inner_ip, compiled_fns) {
                if e.is_succeed {
                    self.stack.truncate(stack_base);
                    if let Some(v) = e.return_value {
                        self.stack.push(v);
                    }
                    self.interpreter.set_when_matched(saved_when);
                    if let Some(v) = saved_topic.clone() {
                        self.interpreter.env_mut().insert("_".to_string(), v);
                    } else {
                        self.interpreter.env_mut().remove("_");
                    }
                    self.topic_source_var = saved_topic_source;
                    *ip = end;
                    return Ok(());
                }
                self.interpreter.set_when_matched(saved_when);
                if let Some(v) = saved_topic {
                    self.interpreter.env_mut().insert("_".to_string(), v);
                } else {
                    self.interpreter.env_mut().remove("_");
                }
                self.topic_source_var = saved_topic_source;
                return Err(e);
            }
            if self.interpreter.when_matched() || self.interpreter.is_halted() {
                break;
            }
        }
        if self.stack.len() > stack_base {
            let last = self.stack.pop().unwrap_or(Value::Nil);
            self.stack.truncate(stack_base);
            self.stack.push(last);
        }

        self.interpreter.set_when_matched(saved_when);
        if let Some(v) = saved_topic {
            self.interpreter.env_mut().insert("_".to_string(), v);
        } else {
            self.interpreter.env_mut().remove("_");
        }
        self.topic_source_var = saved_topic_source;
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_do_given_expr_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let topic = self.stack.pop().unwrap_or(Value::Nil);
        let body_start = *ip + 1;
        let end = body_end as usize;

        let saved_topic = self.interpreter.env().get("_").cloned();
        let saved_when = self.interpreter.when_matched();
        self.interpreter.env_mut().insert("_".to_string(), topic);
        self.interpreter.set_when_matched(false);

        let mut last = Value::Nil;
        let stack_base = self.stack.len();
        let body_result = self.run_range(code, body_start, end, compiled_fns);
        match body_result {
            Ok(()) => {
                if self.stack.len() > stack_base {
                    last = self.stack.pop().unwrap_or(Value::Nil);
                }
                self.stack.truncate(stack_base);
            }
            Err(e) if e.is_succeed => {
                if let Some(v) = e.return_value {
                    last = v;
                }
                self.container_ref_var = e.container_name;
                self.interpreter.set_when_matched(true);
            }
            Err(e) => {
                self.interpreter.set_when_matched(saved_when);
                if let Some(v) = saved_topic {
                    self.interpreter.env_mut().insert("_".to_string(), v);
                } else {
                    self.interpreter.env_mut().remove("_");
                }
                return Err(e);
            }
        }

        self.interpreter.set_when_matched(saved_when);
        if let Some(v) = saved_topic {
            self.interpreter.env_mut().insert("_".to_string(), v);
        } else {
            self.interpreter.env_mut().remove("_");
        }
        self.stack.push(last);
        self.env_dirty = true;
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_when_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let cond_val = self.stack.pop().unwrap();
        let body_start = *ip + 1;
        let end = body_end as usize;

        // Num(Inf) represents Whatever (*) which always matches in `when *`
        let matches = if matches!(&cond_val, Value::Num(v) if v.is_infinite() && v.is_sign_positive())
        {
            true
        } else {
            let topic = self
                .interpreter
                .env()
                .get("_")
                .cloned()
                .unwrap_or(Value::Nil);
            match cond_val {
                Value::Sub(_) | Value::Routine { .. } => {
                    let (_params, param_defs) = self.interpreter.callable_signature(&cond_val);
                    if !param_defs.is_empty() {
                        let mut positional_required = 0usize;
                        let mut positional_total = 0usize;
                        for pd in &param_defs {
                            if pd.named || pd.traits.iter().any(|t| t == "invocant") {
                                continue;
                            }
                            if pd.slurpy || pd.double_slurpy {
                                positional_total = positional_total.max(1);
                                continue;
                            }
                            positional_total += 1;
                            if pd.required || (!pd.optional_marker && pd.default.is_none()) {
                                positional_required += 1;
                            }
                        }
                        if positional_required > 1 {
                            return Err(RuntimeError::new(
                                "when condition Callable with arity > 1 is not allowed",
                            ));
                        }
                        let call_args = if positional_total == 0 {
                            vec![]
                        } else {
                            vec![topic.clone()]
                        };
                        self.interpreter
                            .call_sub_value(cond_val.clone(), call_args, false)
                            .map(|v| v.truthy())?
                    } else {
                        // Builtin/proto callables without explicit signature metadata:
                        // keep smartmatch behavior.
                        self.vm_smart_match(&topic, &cond_val)
                    }
                }
                _ => self.vm_smart_match(&topic, &cond_val),
            }
        };
        if matches {
            let mut did_proceed = false;
            match self.run_range(code, body_start, end, compiled_fns) {
                Ok(()) => {}
                Err(e) if e.is_proceed => {
                    did_proceed = true;
                }
                Err(e) if e.is_succeed => {
                    self.interpreter.set_when_matched(true);
                    return Err(e);
                }
                Err(e) => return Err(e),
            }
            if !did_proceed {
                self.interpreter.set_when_matched(true);
                let last = self.stack.last().cloned().unwrap_or(Value::Nil);
                let mut sig = RuntimeError::succeed_signal();
                sig.return_value = Some(last);
                sig.container_name = self.container_ref_var.take();
                return Err(sig);
            }
        }
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_default_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let body_start = *ip + 1;
        let end = body_end as usize;
        match self.run_range(code, body_start, end, compiled_fns) {
            Ok(()) => {}
            Err(e) if e.is_succeed => {
                self.interpreter.set_when_matched(true);
                return Err(e);
            }
            Err(e) => return Err(e),
        }
        self.interpreter.set_when_matched(true);
        let last = self.stack.last().cloned().unwrap_or(Value::Nil);
        let mut sig = RuntimeError::succeed_signal();
        sig.return_value = Some(last);
        sig.container_name = self.container_ref_var.take();
        *ip = end;
        Err(sig)
    }

    pub(super) fn exec_repeat_loop_op(
        &mut self,
        code: &CompiledCode,
        cond_end: u32,
        body_end: u32,
        label: &Option<String>,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let body_start = *ip + 1;
        let cond_start = cond_end as usize;
        let loop_end = body_end as usize;

        let mut first = true;
        'repeat_loop: loop {
            if !first {
                self.run_range(code, cond_start, loop_end, compiled_fns)?;
                let cond_val = self.stack.pop().unwrap();
                if !cond_val.truthy() {
                    break;
                }
            }
            first = false;
            'body_redo: loop {
                match self.run_range(code, body_start, cond_start, compiled_fns) {
                    Ok(()) => {
                        if !code.state_locals.is_empty() {
                            self.sync_state_locals_in_range(code, body_start, cond_start);
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo && Self::label_matches(&e.label, label) => {
                        continue 'body_redo;
                    }
                    Err(e)
                        if e.is_leave
                            && e.leave_callable_id.is_none()
                            && e.leave_routine.is_none()
                            && Self::label_matches(&e.label, label) =>
                    {
                        if let Some(v) = e.return_value {
                            self.interpreter.env_mut().insert("_".to_string(), v);
                        }
                        break 'repeat_loop;
                    }
                    Err(e) if e.is_last && Self::label_matches(&e.label, label) => {
                        break 'repeat_loop;
                    }
                    Err(e) if e.is_next && Self::label_matches(&e.label, label) => {
                        break 'body_redo;
                    }
                    Err(e) => return Err(e),
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        *ip = loop_end;
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_try_catch_op(
        &mut self,
        code: &CompiledCode,
        catch_start: u32,
        control_start: u32,
        body_end: u32,
        explicit_catch: bool,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let saved_depth = self.stack.len();
        let let_mark = self.interpreter.let_saves_len();
        let body_start = *ip + 1;
        let catch_begin = catch_start as usize;
        let control_begin = control_start as usize;
        let end = body_end as usize;
        match self.run_range(code, body_start, catch_begin, compiled_fns) {
            Ok(()) => {
                // Successful try resets $! to Nil
                self.interpreter
                    .env_mut()
                    .insert("!".to_string(), Value::Nil);
                self.interpreter.discard_let_saves(let_mark);
                *ip = end;
                Ok(())
            }
            Err(e) if e.is_return => {
                self.interpreter.discard_let_saves(let_mark);
                if control_begin < end {
                    self.stack.truncate(saved_depth);
                    let saved_topic = self.interpreter.env().get("_").cloned();
                    if let Some(signal_topic) = Self::control_signal_topic_value(&e) {
                        self.interpreter
                            .env_mut()
                            .insert("_".to_string(), signal_topic);
                    }
                    let saved_when = self.interpreter.when_matched();
                    self.interpreter.set_when_matched(false);
                    match self.run_range(code, control_begin, end, compiled_fns) {
                        Ok(()) => {
                            self.stack.truncate(saved_depth);
                            self.stack.push(Value::Nil);
                        }
                        Err(control_err) if control_err.is_succeed => {
                            self.stack.truncate(saved_depth);
                            self.stack.push(Value::Nil);
                        }
                        Err(control_err) => return Err(control_err),
                    }
                    self.interpreter.set_when_matched(saved_when);
                    if let Some(v) = saved_topic {
                        self.interpreter.env_mut().insert("_".to_string(), v);
                    } else {
                        self.interpreter.env_mut().remove("_");
                    }
                    *ip = end;
                    Ok(())
                } else {
                    Err(e)
                }
            }
            Err(e) if e.return_value.is_some() && !e.is_succeed => {
                self.interpreter.discard_let_saves(let_mark);
                Err(e)
            }
            Err(e)
                if (e.is_last
                    || e.is_next
                    || e.is_redo
                    || e.is_proceed
                    || e.is_succeed
                    || e.is_warn)
                    && control_begin < end =>
            {
                self.interpreter.discard_let_saves(let_mark);
                self.stack.truncate(saved_depth);
                let saved_topic = self.interpreter.env().get("_").cloned();
                if let Some(signal_topic) = Self::control_signal_topic_value(&e) {
                    self.interpreter
                        .env_mut()
                        .insert("_".to_string(), signal_topic);
                }
                let saved_when = self.interpreter.when_matched();
                self.interpreter.set_when_matched(false);
                match self.run_range(code, control_begin, end, compiled_fns) {
                    Ok(()) => {}
                    Err(e) if e.is_succeed => {}
                    Err(e) => return Err(e),
                }
                self.interpreter.set_when_matched(saved_when);
                if let Some(v) = saved_topic {
                    self.interpreter.env_mut().insert("_".to_string(), v);
                } else {
                    self.interpreter.env_mut().remove("_");
                }
                *ip = end;
                Ok(())
            }
            Err(e) => {
                // Exception — restore let saves
                self.interpreter.restore_let_saves(let_mark);
                self.env_dirty = true;
                if catch_begin >= control_begin {
                    return Err(e);
                }
                self.stack.truncate(saved_depth);
                let err_val = if let Some(ex) = e.exception.as_ref() {
                    *ex.clone()
                } else {
                    let mut exc_attrs = std::collections::HashMap::new();
                    exc_attrs.insert("message".to_string(), Value::str(e.message.clone()));
                    Value::make_instance(Symbol::intern("Exception"), exc_attrs)
                };
                let saved_topic = self.interpreter.env().get("_").cloned();
                self.interpreter
                    .env_mut()
                    .insert("!".to_string(), err_val.clone());
                self.interpreter.env_mut().insert("_".to_string(), err_val);
                let saved_when = self.interpreter.when_matched();
                self.interpreter.set_when_matched(false);
                let catch_stack_base = self.stack.len();
                let when_handled =
                    match self.run_range(code, catch_begin, control_begin, compiled_fns) {
                        Ok(()) => self.interpreter.when_matched(),
                        // succeed from `when` inside CATCH means exception was handled
                        Err(catch_err) if catch_err.is_succeed => {
                            // Truncate values left by default body, then push Nil
                            // (Raku: try { die; CATCH { default { "caught" } } } returns Nil)
                            self.stack.truncate(catch_stack_base);
                            self.stack.push(Value::Nil);
                            true
                        }
                        // .resume called inside CATCH: resume execution after the die
                        Err(catch_err) if catch_err.is_resume => {
                            self.stack.truncate(catch_stack_base);
                            self.interpreter.set_when_matched(saved_when);
                            if let Some(v) = saved_topic {
                                self.interpreter.env_mut().insert("_".to_string(), v);
                            } else {
                                self.interpreter.env_mut().remove("_");
                            }
                            // Resume from the instruction after die
                            if let Some(resume_point) = self.resume_ip.take() {
                                // Run from the resume point to the end of the try body
                                match self.run_range(code, resume_point, catch_begin, compiled_fns)
                                {
                                    Ok(()) => {}
                                    Err(resume_err) => return Err(resume_err),
                                }
                            }
                            *ip = end;
                            return Ok(());
                        }
                        Err(catch_err) => return Err(catch_err),
                    };
                self.interpreter.set_when_matched(saved_when);
                if let Some(v) = saved_topic {
                    self.interpreter.env_mut().insert("_".to_string(), v);
                } else {
                    self.interpreter.env_mut().remove("_");
                }
                // If there's an explicit CATCH block but no `when`/`default`
                // matched, re-throw the exception (Raku semantics).
                if explicit_catch && !when_handled {
                    return Err(e);
                }
                *ip = end;
                Ok(())
            }
        }
    }
}
