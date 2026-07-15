use super::vm_control_ops::ForLoopSpec;
use super::*;

impl Interpreter {
    /// Iterate lazily over a gather-based LazyList.
    /// Pulls items one at a time via `force_lazy_list_vm_n`, avoiding
    /// full materialization of potentially infinite sequences.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_for_loop_lazy_gather(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        ll: &crate::gc::Gc<crate::value::LazyList>,
        body_start: usize,
        loop_end: usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        self.exec_for_loop_lazy_gather_from(code, spec, ll, 0, body_start, loop_end, compiled_fns)
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_for_loop_lazy_gather_from(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        ll: &crate::gc::Gc<crate::value::LazyList>,
        start_idx: usize,
        body_start: usize,
        loop_end: usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        // The caller holds the `Gc<LazyList>` handle; clone it directly. (Pre-GC
        // this reconstructed an `Arc<LazyList>` from a `&LazyList` via
        // `Arc::from_raw` — unsound now that the value lives inside a `GcBox`.)
        let ll_arc = ll.clone();
        let param_name = spec
            .param_idx
            .map(|idx| match code.constants[idx as usize].view() {
                ValueView::Str(s) => s.to_string(),
                _ => unreachable!("ForLoop param must be a string constant"),
            });
        let saved_topic = spec
            .restore_topic
            .then(|| self.env().get("_").cloned())
            .flatten();
        let saved_topic_source = self.topic_source_var.take();
        let was_topic_readonly = self.is_readonly("_");

        if !spec.is_rw {
            if let Some(ref name) = param_name {
                if !name.starts_with('@') && !name.starts_with('%') {
                    self.mark_readonly(name);
                }
            } else if spec.multi_param_names.is_empty() {
                // A multi-param loop binds through `$_`; the body's bind statements
                // must stay writable, so `$_` is not the read-only topic here.
                self.mark_readonly("_");
            }
        }

        // `for $lazy -> $a, $b, $c` consumes `arity` elements per iteration and binds
        // them as one chunk, exactly as the eager path does.
        let arity = spec.arity.max(1) as usize;
        let mut idx: usize = start_idx;
        'for_loop: loop {
            // Force enough elements for one iteration's chunk
            let items = self.force_lazy_list_vm_n(ll, idx + arity)?;
            if idx >= items.len() {
                break; // No more elements
            }
            let item = if arity > 1 {
                let end = (idx + arity).min(items.len());
                Value::array(items[idx..end].to_vec())
            } else {
                items[idx].clone()
            };
            idx += arity;

            self.topic_source_var = None;
            if param_name.is_none() {
                self.env_mut().insert("_".to_string(), item.clone());
            }
            if let Some(ref name) = param_name {
                self.env_mut().insert(name.clone(), item.clone());
            }
            if let Some(slot) = spec.param_local {
                self.locals[slot as usize] = item.clone();
            }
            // The body of a multi-param loop starts with bind statements that assign
            // into the params, so they must not stay read-only from an outer scope.
            for mp_name in &spec.multi_param_names {
                self.unmark_readonly(mp_name);
                let key = format!("__mutsu_sigilless_readonly::{}", mp_name);
                self.env_mut().insert(key, Value::FALSE);
            }
            'body_redo: loop {
                let body_res = self.run_range(code, body_start, loop_end, compiled_fns);
                // State mutations persist on every exit path (`next`/`redo`/
                // `last`/exception), not just normal completion.
                if !code.state_locals.is_empty() {
                    self.sync_state_locals_in_range(code, body_start, loop_end);
                }
                match body_res {
                    Ok(()) => {
                        break 'body_redo;
                    }
                    Err(e) if e.is_succeed() => {
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo() && Self::label_matches(&e.label, &spec.label) => {
                        if param_name.is_none() {
                            self.env_mut().insert("_".to_string(), item.clone());
                        }
                        if let Some(ref name) = param_name {
                            self.env_mut().insert(name.clone(), item.clone());
                        }
                        if let Some(slot) = spec.param_local {
                            self.locals[slot as usize] = item.clone();
                        }
                        continue 'body_redo;
                    }
                    Err(e)
                        if e.is_leave
                            && e.leave_callable_id().is_none()
                            && e.leave_routine().is_none()
                            && Self::label_matches(&e.label, &spec.label) =>
                    {
                        break 'for_loop;
                    }
                    Err(e) if e.is_last() && Self::label_matches(&e.label, &spec.label) => {
                        break 'for_loop;
                    }
                    Err(e) if e.is_next() && Self::label_matches(&e.label, &spec.label) => {
                        break 'body_redo;
                    }
                    Err(e)
                        if e.message
                            == crate::runtime::Interpreter::LAZY_GATHER_TAKE_LIMIT_SIGNAL =>
                    {
                        // Save lazy-gather for-loop state for coroutine resumption.
                        self.gather_for_loop_resume =
                            Some(crate::value::ForLoopResumeState::LazyGather {
                                lazy_list: ll_arc.clone(),
                                next_index: idx,
                            });
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.unmark_readonly(name);
                        }
                        if !was_topic_readonly {
                            self.unmark_readonly("_");
                        }
                        self.topic_source_var = saved_topic_source;
                        if spec.restore_topic {
                            match saved_topic {
                                Some(v) => {
                                    self.env_mut().insert("_".to_string(), v);
                                }
                                None => {
                                    self.env_mut().remove("_");
                                }
                            }
                        }
                        return Err(e);
                    }
                    Err(e) => {
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.unmark_readonly(name);
                        }
                        if !was_topic_readonly {
                            self.unmark_readonly("_");
                        }
                        if spec.restore_topic {
                            match saved_topic.clone() {
                                Some(v) => {
                                    self.env_mut().insert("_".to_string(), v);
                                }
                                None => {
                                    self.env_mut().remove("_");
                                }
                            }
                        }
                        return Err(e);
                    }
                }
            }
            if self.is_halted() {
                break;
            }
        }

        if !spec.is_rw
            && let Some(ref name) = param_name
        {
            self.unmark_readonly(name);
        }
        if !was_topic_readonly {
            self.unmark_readonly("_");
        }
        self.topic_source_var = saved_topic_source;
        if spec.restore_topic {
            match saved_topic {
                Some(v) => {
                    self.env_mut().insert("_".to_string(), v);
                }
                None => {
                    self.env_mut().remove("_");
                }
            }
        }
        Ok(())
    }

    /// Iterate lazily over IO lines from a file handle.
    /// Reads one line at a time so that $fh.tell reflects the current position.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_for_loop_lazy_io_lines(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        handle: &Value,
        kv: bool,
        words: bool,
        body_start: usize,
        loop_end: usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let param_name = spec
            .param_idx
            .map(|idx| match code.constants[idx as usize].view() {
                ValueView::Str(s) => s.to_string(),
                _ => unreachable!("ForLoop param must be a string constant"),
            });

        let arity = spec.arity.max(1) as usize;
        let _writes_back_topic =
            spec.param_idx.is_none() && spec.param_local.is_none() && spec.arity <= 1;
        let saved_topic = spec
            .restore_topic
            .then(|| self.env().get("_").cloned())
            .flatten();
        let saved_topic_source = self.topic_source_var.take();
        let mut collected = if spec.collect { Some(Vec::new()) } else { None };
        let stack_base = if spec.collect {
            Some(self.stack.len())
        } else {
            None
        };

        let mut line_index: i64 = 0;

        'for_loop: loop {
            // Read the next record (word or line) from the handle
            let line = if words {
                self.read_word_from_handle_value(handle)?
            } else {
                loan_env!(self, read_line_from_handle_value(handle))?
            };
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
                    let pair = Value::array(vec![Value::int(line_index), line_val]);
                    line_index += 1;
                    pair
                } else {
                    // arity 1: interleave index and value as separate iterations
                    // This is unusual for .kv in a for loop but handle it.
                    // We'd need to run the body twice per line, once for index
                    // and once for value. For now, produce a flat pair.
                    let pair = Value::array(vec![Value::int(line_index), line_val]);
                    line_index += 1;
                    pair
                }
            } else {
                line_index += 1;
                line_val
            };

            // Set up parameters
            if param_name.is_none() {
                self.env_mut().insert("_".to_string(), item.clone());
            }
            if let Some(ref name) = param_name {
                self.env_mut().insert(name.clone(), item.clone());
                if let Some(bare) = name.strip_prefix("&^") {
                    self.env_mut().insert(format!("&{}", bare), item.clone());
                } else if let Some(bare) = name.strip_prefix('^') {
                    self.env_mut().insert(bare.to_string(), item.clone());
                }
            }
            if let Some(slot) = spec.param_local {
                self.locals[slot as usize] = item.clone();
            }
            if !spec.is_rw
                && let Some(ref name) = param_name
            {
                self.mark_readonly(name);
            }

            'body_redo: loop {
                let body_res = self.run_range(code, body_start, loop_end, compiled_fns);
                // State mutations persist on every exit path (`next`/`redo`/
                // `last`/exception), not just normal completion.
                if !code.state_locals.is_empty() {
                    self.sync_state_locals_in_range(code, body_start, loop_end);
                }
                match body_res {
                    Ok(()) => {
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
                    Err(e) if e.is_succeed() => {
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo() && Self::label_matches(&e.label, &spec.label) => {
                        if param_name.is_none() {
                            self.env_mut().insert("_".to_string(), item.clone());
                        }
                        if let Some(ref name) = param_name {
                            self.env_mut().insert(name.clone(), item.clone());
                        }
                        if let Some(slot) = spec.param_local {
                            self.locals[slot as usize] = item.clone();
                        }
                        continue 'body_redo;
                    }
                    Err(e) if e.is_last() && Self::label_matches(&e.label, &spec.label) => {
                        break 'for_loop;
                    }
                    Err(e) if e.is_next() && Self::label_matches(&e.label, &spec.label) => {
                        break 'body_redo;
                    }
                    Err(e) => {
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.unmark_readonly(name);
                        }
                        if spec.restore_topic {
                            match saved_topic.clone() {
                                Some(v) => {
                                    self.env_mut().insert("_".to_string(), v);
                                }
                                None => {
                                    self.env_mut().remove("_");
                                }
                            }
                        }
                        return Err(e);
                    }
                }
            }
            if self.is_halted() {
                break;
            }
        }

        // Cleanup
        if !spec.is_rw
            && let Some(ref name) = param_name
        {
            self.unmark_readonly(name);
        }
        self.topic_source_var = saved_topic_source;
        if spec.restore_topic {
            match saved_topic {
                Some(v) => {
                    self.env_mut().insert("_".to_string(), v);
                }
                None => {
                    self.env_mut().remove("_");
                }
            }
        }
        if let Some(coll) = collected {
            self.stack.push(Value::array(coll));
        }
        Ok(())
    }
}
