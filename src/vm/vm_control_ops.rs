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
    /// When true, `-> {}` was used — throw on any items.
    pub(super) explicit_zero_params: bool,
    /// Names of multi-param bindings (for `-> $a, \b, $c` loops).
    /// Used to temporarily clear sigilless readonly flags before binding.
    pub(super) multi_param_names: Vec<String>,
    /// When true (`.pairs`/`.antipairs`), the loop variable is a `Pair` wrapping
    /// the element, so the plain per-element source writeback is suppressed.
    pub(super) loop_var_wraps_element: bool,
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
        // User-defined classes doing X::Control carry their original
        // exception instance. Surface it directly so CONTROL blocks see the
        // real type rather than a generic CX::Done wrapper.
        if signal.is_done
            && let Some(ex) = signal.exception.as_ref()
        {
            return Some((**ex).clone());
        }
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
        } else if signal.is_take {
            Some("CX::Take")
        } else if signal.is_emit {
            Some("CX::Emit")
        } else if signal.is_done || signal.is_react_done {
            Some("CX::Done")
        } else if signal.is_return {
            Some("CX::Return")
        } else {
            None
        }?;

        let mut attrs = std::collections::HashMap::new();
        // `warn` appends a "\n  in block ..." location annotation to its
        // message for the default uncaught-warn printer. When a CONTROL
        // block observes the CX::Warn, `.message` should be just the user
        // text without the location, so strip the appended suffix.
        let message_text = if signal.message.is_empty() {
            class_name.to_string()
        } else if signal.is_warn {
            signal
                .message
                .split_once("\n  in block ")
                .map(|(user, _)| user.to_string())
                .unwrap_or_else(|| signal.message.clone())
        } else {
            signal.message.clone()
        };
        attrs.insert("message".to_string(), Value::str(message_text));
        if let Some(label) = signal.label.as_ref() {
            attrs.insert("label".to_string(), Value::str(label.clone()));
        }
        Some(Value::make_instance(Symbol::intern(class_name), attrs))
    }

    /// Push a fresh loop-body declaration scope so `my` declarations inside the
    /// body register as loop-local (see `VM::loop_local_vars`). A closure created
    /// in the body marks such free variables as `owned_captures`, giving Raku's
    /// per-iteration binding semantics. Must be balanced by `pop_loop_local_scope`
    /// on every exit path.
    pub(super) fn push_loop_local_scope(&mut self) {
        self.loop_local_vars.push(std::collections::HashSet::new());
        self.loop_local_saved_env
            .push(std::collections::HashMap::new());
    }

    /// Pop the loop-body declaration scope pushed by `push_loop_local_scope` and
    /// restore the env entries for loop-body-local `my` names to the values they
    /// shadowed before the loop (or remove names that did not exist before). This
    /// stops a block-local `my $x` inside the loop body from leaking its
    /// last-iteration value into — or clobbering an enclosing same-named — outer
    /// binding (the name-keyed env half of the dual store). The compiler already
    /// gave a shadowing declaration a fresh slot, so the slot half is isolated;
    /// any outer slot sharing the name re-syncs from the restored env on next read.
    pub(super) fn pop_loop_local_scope(&mut self, code: &CompiledCode) {
        self.loop_local_vars.pop();
        if let Some(saved) = self.loop_local_saved_env.pop() {
            for (name, val) in saved {
                // `saved` only holds names that shadowed an existing outer binding
                // (recorded in exec_set_local_op), so restoring the captured value
                // re-exposes the outer `$x` clobbered by the loop body's `my $x`.
                self.env_mut().insert(name.clone(), val.clone());
                // Restore the local slot too: loop bodies mark every local
                // `needs_env_sync`, so the shadowing `my` wrote the outer var's
                // shared slot. `GetLocal`'s fast path returns a non-Nil slot value
                // without consulting env, so an env-only restore would still read
                // the stale last-iteration value. Mirror the restored value into
                // every slot carrying this name.
                for (idx, slot_name) in code.locals.iter().enumerate() {
                    if slot_name == &name && idx < self.locals.len() {
                        self.locals[idx] = val.clone();
                    }
                }
            }
        }
    }

    /// Run an `if`/`unless`/`else` branch body that declares a block-local `my`
    /// under the loop bodies' shadow-only restore (see `push_loop_local_scope` /
    /// `pop_loop_local_scope`). This fixes the body-local `my` *clobber*
    /// (`my $x=99; if c { my $x=5 }; say $x` must print `99`, not `5`) without
    /// the full env restore of `BlockScope` — which would also revert a `:=`
    /// binding the branch makes to an *outer* variable (the regression that sent
    /// the if/else case to Slice 3b). The branch runs once, so reusing the
    /// per-iteration `owned_captures`/box-on-capture machinery is harmless and
    /// keeps closure capture of the branch-local correct.
    pub(super) fn exec_block_local_scope_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let body_start = *ip + 1;
        let body_end = body_end as usize;
        self.push_loop_local_scope();
        let res = self.run_range(code, body_start, body_end, compiled_fns);
        // Restore shadowed outer bindings on every exit path (including errors
        // such as `next`/`last`/`return`/exceptions) so the scope stays balanced.
        self.pop_loop_local_scope(code);
        res?;
        *ip = body_end;
        Ok(())
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

        // When resuming a gather coroutine that suspended inside this loop, the
        // marker is consumed here; the loop's state lives in locals/env so
        // re-entering from the top (cond re-check) continues correctly.
        if matches!(
            self.gather_for_loop_resume,
            Some(crate::value::ForLoopResumeState::CStyleLoop)
        ) {
            self.gather_for_loop_resume = None;
        }

        // Track loop-body declarations for per-iteration closure capture
        // (owned_captures, incl. `while my $x = ...` condition declarations).
        // Balanced by pop on every exit path.
        self.push_loop_local_scope();

        'while_loop: loop {
            self.loop_cond_active = true;
            let cond_res = self.run_range(code, cond_start, body_start, compiled_fns);
            self.loop_cond_active = false;
            if let Err(e) = cond_res {
                self.pop_loop_local_scope(code);
                return Err(e);
            }
            let cond_val = self.stack.pop().unwrap();
            if !cond_val.truthy() {
                break;
            }
            let topic_before_body = if spec.isolate_topic {
                Some(self.env().get("_").cloned())
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
                                self.env_mut().insert("_".to_string(), v);
                            } else {
                                self.env_mut().remove("_");
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
                        if let Err(e) = self.interpreter.run_pending_instance_destroys() {
                            self.pop_loop_local_scope(code);
                            return Err(e);
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_succeed => {
                        if let Some(saved_topic) = &topic_before_body {
                            if let Some(v) = saved_topic.clone() {
                                self.env_mut().insert("_".to_string(), v);
                            } else {
                                self.env_mut().remove("_");
                            }
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo && Self::label_matches(&e.label, &spec.label) => {
                        if let Some(saved_topic) = &topic_before_body {
                            if let Some(v) = saved_topic.clone() {
                                self.env_mut().insert("_".to_string(), v);
                            } else {
                                self.env_mut().remove("_");
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
                                self.env_mut().insert("_".to_string(), v);
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
                                self.env_mut().insert("_".to_string(), v);
                            } else {
                                self.env_mut().remove("_");
                            }
                        }
                        break 'body_redo;
                    }
                    Err(e)
                        if e.message
                            == crate::runtime::Interpreter::LAZY_GATHER_TAKE_LIMIT_SIGNAL =>
                    {
                        // Gather coroutine suspend inside a `while`/`until` loop.
                        // Park a marker so the outer forcer keeps `*ip` on this
                        // loop opcode (it stays unchanged on this Err path),
                        // letting us re-enter and continue from locals/env state.
                        self.gather_for_loop_resume =
                            Some(crate::value::ForLoopResumeState::CStyleLoop);
                        self.pop_loop_local_scope(code);
                        return Err(e);
                    }
                    Err(e) => {
                        self.pop_loop_local_scope(code);
                        return Err(e);
                    }
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        self.pop_loop_local_scope(code);
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
        // Check for gather coroutine resume state. A `CStyleLoop` marker belongs
        // to a `loop`/`while` opcode, not this for-loop, so leave it in place.
        if !matches!(
            self.gather_for_loop_resume,
            Some(crate::value::ForLoopResumeState::CStyleLoop)
        ) && let Some(resume) = self.gather_for_loop_resume.take()
        {
            let body_start = *ip + 1;
            let loop_end = spec.body_end as usize;
            match resume {
                crate::value::ForLoopResumeState::IntRange {
                    current,
                    end_val,
                    inclusive,
                } => {
                    self.exec_for_loop_int_range(
                        code,
                        spec,
                        current,
                        end_val,
                        inclusive,
                        body_start,
                        loop_end,
                        compiled_fns,
                    )?;
                    *ip = loop_end;
                    return Ok(());
                }
                crate::value::ForLoopResumeState::List { items, next_index } => {
                    self.exec_for_loop_body(
                        code,
                        spec,
                        &items,
                        body_start,
                        loop_end,
                        compiled_fns,
                        next_index,
                    )?;
                    *ip = loop_end;
                    return Ok(());
                }
                crate::value::ForLoopResumeState::LazyGather {
                    lazy_list,
                    next_index,
                } => {
                    self.exec_for_loop_lazy_gather_from(
                        code,
                        spec,
                        &lazy_list,
                        next_index,
                        body_start,
                        loop_end,
                        compiled_fns,
                    )?;
                    *ip = loop_end;
                    return Ok(());
                }
                // Guarded out above: a CStyleLoop marker is never taken here.
                crate::value::ForLoopResumeState::CStyleLoop => unreachable!(),
            }
        }

        let iterable = self.stack.pop().unwrap();

        // rw aggregate view: `for @a.grep(...) { $_++ }` iterates over a grep
        // result whose elements alias slots of the original array. If the
        // iterable carries a registered grep-view binding, remember the source
        // array + matched indices so the loop can write modified topics back.
        self.for_grep_view = match &iterable {
            Value::Array(items, _) => items
                .grep_source
                .as_deref()
                .map(|gv| (gv.source.clone(), gv.indices.clone(), gv.source_kind)),
            _ => None,
        };

        // Handle lazy IO lines: iterate by pulling one line at a time
        // so that $fh.tell reflects the current read position.
        if let Value::LazyIoLines {
            ref handle,
            kv,
            words,
        } = iterable
        {
            let body_start = *ip + 1;
            let loop_end = spec.body_end as usize;
            self.exec_for_loop_lazy_io_lines(
                code,
                spec,
                handle,
                kv,
                words,
                body_start,
                loop_end,
                compiled_fns,
            )?;
            *ip = loop_end;
            return Ok(());
        }

        // Fast path for integer range iteration: avoid materializing the entire range.
        // Applies when the range is simple (arity 1, not threaded, no writeback, no collect).
        if !spec.threaded && !spec.collect && !spec.do_writeback && spec.arity <= 1 && !spec.kv_mode
        {
            let int_range = match &iterable {
                Value::Range(a, b) => Some((*a, *b, true)), // a..b (inclusive)
                Value::RangeExcl(a, b) => Some((*a, *b, false)), // a..^b (exclusive end)
                Value::RangeExclStart(a, b) => Some((*a + 1, *b, true)),
                Value::RangeExclBoth(a, b) => Some((*a + 1, *b, false)),
                _ => None,
            };
            if let Some((start, end_val, inclusive)) = int_range {
                let body_start = *ip + 1;
                let loop_end = spec.body_end as usize;
                self.exec_for_loop_int_range(
                    code,
                    spec,
                    start,
                    end_val,
                    inclusive,
                    body_start,
                    loop_end,
                    compiled_fns,
                )?;
                *ip = loop_end;
                return Ok(());
            }
        }

        // For gather-based, sequence-spec, or lazy map/grep pipeline LazyList
        // iterables, iterate lazily by pulling items one at a time. This avoids
        // materializing infinite sequences.
        if let Value::LazyList(ref ll) = iterable
            && (ll.coroutine.is_some() || ll.sequence_spec.is_some() || ll.lazy_pipe.is_some())
        {
            let body_start = *ip + 1;
            let loop_end = spec.body_end as usize;
            self.exec_for_loop_lazy_gather(code, spec, ll, body_start, loop_end, compiled_fns)?;
            *ip = loop_end;
            return Ok(());
        }

        let raw_items = if let Value::LazyList(ref ll) = iterable {
            self.force_lazy_list_vm(ll)?
        } else if let Value::Channel(ref ch) = iterable {
            // Drain the channel synchronously, blocking on receive until the
            // channel is closed. Propagate any failure as an exception so the
            // surrounding `start { }` / `try` can observe it.
            let mut items = Vec::new();
            loop {
                match ch.receive_result() {
                    Ok(Value::Nil) => break,
                    Ok(v) => items.push(v),
                    Err(cause) => {
                        let ex = crate::runtime::Interpreter::as_exception_value(cause);
                        let mut err = RuntimeError::new(ex.to_string_value());
                        err.exception = Some(Box::new(ex));
                        return Err(err);
                    }
                }
            }
            items
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
        // When `-> {}` was used (explicit zero params), throw if any items would be passed.
        if spec.explicit_zero_params && !items.is_empty() {
            return Err(RuntimeError::new(format!(
                "Too many positionals passed; expected 0 arguments but got {}",
                items.len()
            )));
        }
        self.env_dirty = true;
        let body_start = *ip + 1;
        let loop_end = spec.body_end as usize;

        if spec.threaded {
            // race for / hyper for: run the loop body in a spawned thread
            // so that $*THREAD.id returns a different value.
            let result = std::thread::scope(|s| {
                s.spawn(|| {
                    self.exec_for_loop_body(
                        code,
                        spec,
                        &items,
                        body_start,
                        loop_end,
                        compiled_fns,
                        0,
                    )
                })
                .join()
                .unwrap_or_else(|_| Err(RuntimeError::new("thread panicked in race/hyper for")))
            });
            *ip = loop_end;
            return result;
        }

        self.exec_for_loop_body(code, spec, &items, body_start, loop_end, compiled_fns, 0)?;
        *ip = loop_end;
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn exec_for_loop_body(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        items: &[Value],
        body_start: usize,
        loop_end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
        resume_index: usize,
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
        // A plain (non-rw, non-copy) named loop variable aliases the source
        // element in Raku: `for @m -> @row { @row.push(9) }` and
        // `for @m -> $row { $row.push(9) }` both mutate `@m` (a scalar binding a
        // container can still mutate the container, though not rebind it).
        // Mirror the topic writeback for it. `is copy` sets `is_rw` (suppressing
        // this), and `<->` rw uses `rw_writeback` instead. The unchanged-value
        // guard in `write_back_for_topic_item` keeps read-only loops O(n).
        let writes_back_named_param =
            !spec.is_rw && !rw_writeback && spec.arity <= 1 && param_name.is_some();
        // `.pairs`/`.antipairs` loop variables are `Pair`s wrapping the element,
        // not the element itself — writing one back would overwrite the source
        // element with the Pair (S32-array/pairs.t 14). The Pair's rw `.value`
        // alias handles propagation (and immutability for Mix), so suppress the
        // plain writeback here while keeping the source tag.
        let writes_back_loop_var =
            (writes_back_topic || writes_back_named_param) && !spec.loop_var_wraps_element;
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
            .then(|| self.env().get("_").cloned())
            .flatten();
        let saved_topic_source = self.topic_source_var.take();
        let saved_quanthash_bind = std::mem::take(&mut self.quanthash_bind_params);
        let container_binding = self.container_ref_var.take();
        // rw aggregate view writeback (`for @a.grep(...) { $_++ }`): take the
        // grep-view binding captured by exec_for_loop_op. When present and the
        // loop writes back its topic / rw param, modified values are mirrored to
        // the source array's matched slots after the loop (by Arc identity).
        // Only applies when the iterable is a transient grep result flowing
        // directly into the loop. If `container_binding` is set, the iterable is
        // a named array variable (e.g. `my @g = @a.grep(...); for @g {...}`)
        // whose `=` assignment decontainerized the rw aliases — the name-based
        // writeback owns that array, so the grep view must not leak into @a.
        let grep_view = if container_binding.is_none() {
            self.for_grep_view.take()
        } else {
            self.for_grep_view = None;
            None
        };
        let mut grep_view_writes: Vec<(usize, Value)> = Vec::new();
        let container_reversed = self.container_ref_reversed;
        self.container_ref_reversed = false;
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
        // Save multi-param values and readonly state so they can be restored
        // after the loop (inner loops must not clobber outer scope bindings).
        let saved_multi_params: Vec<(String, Option<Value>, bool, Option<Value>)> = spec
            .multi_param_names
            .iter()
            .map(|name| {
                let val = self.env().get(name).cloned();
                let was_readonly = self.interpreter.readonly_vars().contains(name);
                let sigilless_key = format!("__mutsu_sigilless_readonly::{}", name);
                let sigilless_ro = self.env().get(&sigilless_key).cloned();
                (name.clone(), val, was_readonly, sigilless_ro)
            })
            .collect();
        // Save the single named loop param (`for ... -> $x`) too, so a loop in a
        // called sub that reuses the same variable name does not clobber an outer
        // loop's binding of that name (the env keys these by bare name). Skip
        // `@`/`%` sigils, which bind a shared mutable container the body may
        // legitimately reassign, and skip the rw case (handled via writeback).
        let saved_param: Option<(String, Option<Value>)> = param_name
            .as_ref()
            .filter(|n| !n.starts_with('@') && !n.starts_with('%'))
            .map(|name| (name.clone(), self.env().get(name).cloned()));
        // Track loop-body declarations for per-iteration closure capture
        // (owned_captures). Balanced by pop on every exit.
        self.push_loop_local_scope();
        // Determine if the implicit topic ($_) should be read-only.
        // Only mark $_ readonly when iterating over a known immutable collection
        // (Mix, Set, Bag). This blocks `.value = ...` mutations on pairs from
        // immutable collections while keeping $_ writable for expression results,
        // multi-param for loops, and Scalar containers holding plain values.
        let topic_readonly =
            !spec.is_rw && param_name.is_none() && spec.multi_param_names.is_empty() && {
                match &container_binding {
                    None => false,
                    Some(name) => {
                        if let Some(val) = self.get_env_with_main_alias(name) {
                            matches!(val, Value::Mix(_, _) | Value::Set(_, _) | Value::Bag(_, _))
                        } else {
                            false
                        }
                    }
                }
            };
        let total_items = chunked_items.len();
        'for_loop: for (idx, item) in chunked_items.into_iter().enumerate().skip(resume_index) {
            self.topic_source_var = if writes_back_topic {
                container_binding.clone()
            } else {
                None
            };
            // Only set $_ when no named parameter is given (for @list { ... })
            // When -> $k is used, $_ should remain from the enclosing scope
            if param_name.is_none() {
                self.env_mut().insert("_".to_string(), item.clone());
            }
            if let Some(ref name) = param_name {
                self.env_mut().insert(name.clone(), item.clone());
                // Create non-twigil alias for placeholder params: $^a → $a
                if let Some(bare) = name.strip_prefix("&^") {
                    self.env_mut().insert(format!("&{}", bare), item.clone());
                } else if let Some(bare) = name.strip_prefix('^') {
                    self.env_mut().insert(bare.to_string(), item.clone());
                }
            }
            if let Some(slot) = spec.param_local {
                self.locals[slot as usize] = item.clone();
            }
            // Mark implicit $_ readonly when source is immutable.
            // Also set a deep-readonly flag so that method-lvalue
            // assignments like .value = ... are blocked too.
            if topic_readonly {
                self.interpreter.mark_readonly("_");
                self.env_mut()
                    .insert("__mutsu_deep_readonly::_".to_string(), Value::Bool(true));
            }
            // Mark named params readonly when not in rw mode.
            // Skip @-sigil and %-sigil params: they bind to a mutable
            // Array/Hash container, so assignments like `@a = values` must
            // be allowed (matching Raku semantics).
            if !spec.is_rw
                && let Some(ref name) = param_name
                && !name.starts_with('@')
                && !name.starts_with('%')
            {
                self.interpreter.mark_readonly(name);
            }
            // `%`-sigil for-loop bindings preserve a QuantHash value (and keep
            // its type across a `%a = ...pairs` reset) instead of coercing it to
            // a plain Hash — Raku binds params, it does not assign-coerce them.
            self.quanthash_bind_params = spec
                .multi_param_names
                .iter()
                .chain(param_name.iter())
                .filter(|n| n.starts_with('%'))
                .cloned()
                .collect();
            // Temporarily clear readonly flags for multi-param names
            // so the bind stmts (Stmt::Assign) at the start of the body can
            // re-bind variables that may be readonly from an outer scope.
            for mp_name in &spec.multi_param_names {
                // Clear regular readonly flag
                self.interpreter.unmark_readonly(mp_name);
                // Clear sigilless readonly flag
                let key = format!("__mutsu_sigilless_readonly::{}", mp_name);
                self.env_mut().insert(key, Value::Bool(false));
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
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                            );
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
                        if let Some((_, ref indices, _)) = grep_view
                            && arity == 1
                            && (writes_back_topic || rw_writeback)
                            && let Some(&src_idx) = indices.get(idx)
                            && let Some(v) = self.grep_view_topic_value(
                                writes_back_topic,
                                rw_writeback,
                                &param_name,
                            )
                        {
                            grep_view_writes.push((src_idx, v));
                        }
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
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                            );
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
                        if let Some((_, ref indices, _)) = grep_view
                            && arity == 1
                            && (writes_back_topic || rw_writeback)
                            && let Some(&src_idx) = indices.get(idx)
                            && let Some(v) = self.grep_view_topic_value(
                                writes_back_topic,
                                rw_writeback,
                                &param_name,
                            )
                        {
                            grep_view_writes.push((src_idx, v));
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo && Self::label_matches(&e.label, &spec.label) => {
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
                            && e.leave_callable_id.is_none()
                            && e.leave_routine.is_none()
                            && Self::label_matches(&e.label, &spec.label) =>
                    {
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                            );
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
                                self.env_mut().insert("_".to_string(), v.clone());
                                // Push return value on stack so enclosing compiled
                                // closures can see it as the block result.
                                self.stack.push(v);
                            }
                        }
                        break 'for_loop;
                    }
                    Err(e) if e.is_last && Self::label_matches(&e.label, &spec.label) => {
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                            );
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
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                            );
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
                    Err(e)
                        if e.message
                            == crate::runtime::Interpreter::LAZY_GATHER_TAKE_LIMIT_SIGNAL =>
                    {
                        // Save for-loop state for gather coroutine resumption.
                        self.gather_for_loop_resume =
                            Some(crate::value::ForLoopResumeState::List {
                                items: items.to_vec(),
                                next_index: idx + 1,
                            });
                        if topic_readonly {
                            self.interpreter.unmark_readonly("_");
                            self.env_mut().remove("__mutsu_deep_readonly::_");
                        }
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.interpreter.readonly_vars_mut().remove(name);
                        }
                        self.topic_source_var = saved_topic_source;
                        self.quanthash_bind_params = saved_quanthash_bind.clone();
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
                        self.pop_loop_local_scope(code);
                        return Err(e);
                    }
                    Err(e) => {
                        // Unmark readonly before propagating error
                        if topic_readonly {
                            self.interpreter.unmark_readonly("_");
                            self.env_mut().remove("__mutsu_deep_readonly::_");
                        }
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.interpreter.readonly_vars_mut().remove(name);
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
                        self.pop_loop_local_scope(code);
                        return Err(e);
                    }
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        // rw aggregate-view writeback: mirror modified topics/params back to the
        // source array's matched slots (by Arc identity, so the originating
        // `@a` variable observes the mutation), e.g. `for @a.grep(...) { $_++ }`.
        if let Some((source, _, source_kind)) = grep_view
            && !grep_view_writes.is_empty()
        {
            let mut source_items = source.to_vec();
            for (src_idx, val) in grep_view_writes {
                if src_idx < source_items.len() {
                    source_items[src_idx] = val;
                }
            }
            loan_env!(
                self,
                overwrite_array_items_by_identity_for_vm(&source, source_items, source_kind,)
            );
            self.env_dirty = true;
        }
        // Unmark readonly topic after loop completion
        if topic_readonly {
            self.interpreter.unmark_readonly("_");
            self.env_mut().remove("__mutsu_deep_readonly::_");
        }
        // Unmark readonly params after loop completion
        if !spec.is_rw
            && let Some(ref name) = param_name
        {
            self.interpreter.readonly_vars_mut().remove(name);
        }
        // Restore saved multi-param values and readonly state
        for (name, saved_val, was_readonly, sigilless_ro) in saved_multi_params {
            if let Some(v) = saved_val {
                self.env_mut().insert(name.clone(), v);
            } else {
                self.env_mut().remove(&name);
            }
            if was_readonly {
                self.interpreter.mark_readonly(&name);
            } else {
                self.interpreter.unmark_readonly(&name);
            }
            let sigilless_key = format!("__mutsu_sigilless_readonly::{}", name);
            if let Some(ro_val) = sigilless_ro {
                self.env_mut().insert(sigilless_key, ro_val);
            } else {
                self.env_mut().remove(&sigilless_key);
            }
        }
        // Defer restoring the single named loop param's prior binding until
        // after the loop's LAST/post phasers have run — they must still observe
        // the param at its final iteration value (e.g.
        // `for 1,2 -> $x { LAST { say $x } }` must see 2). The paired
        // `RestoreForParam` opcode (emitted right after the post phasers) pops
        // this and applies it. Only pushed here on normal completion, which
        // keeps it balanced with that opcode; an early return/exception from the
        // body exits before this point, so no entry is pushed and the matching
        // opcode is likewise skipped as the frame unwinds.
        if let Some(entry) = saved_param {
            self.for_param_restore_stack.push(entry);
        }
        self.pop_loop_local_scope(code);
        self.topic_source_var = saved_topic_source;
        self.quanthash_bind_params = saved_quanthash_bind.clone();
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

    /// Fast path for iterating over integer ranges (e.g., `for ^N`, `for 0..N`).
    /// Avoids materializing the entire range as a Vec<Value> by using a counter.
    #[allow(clippy::too_many_arguments)]
    fn exec_for_loop_int_range(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        start: i64,
        end_val: i64,
        inclusive: bool,
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
        let saved_topic = spec
            .restore_topic
            .then(|| self.env().get("_").cloned())
            .flatten();
        let saved_topic_source = self.topic_source_var.take();
        let was_topic_readonly = self.interpreter.readonly_vars().contains("_");

        // Save the single named loop param (`for ... -> $x`) prior binding so it
        // does not leak past the loop into the enclosing scope. Without this, a
        // closure created in the body and called *after* the loop would read the
        // leaked final value from the call-site env (which shadows its correctly
        // frozen captured value via the don't-overwrite merge) instead of its own
        // per-iteration binding. Mirrors `exec_for_loop_body`'s `saved_param`:
        // restored after the loop's LAST/post phasers via the `RestoreForParam`
        // opcode (the compiler emits it for any single non-@/% named param), so
        // the push below must balance that pop on normal completion.
        let saved_param: Option<(String, Option<Value>)> = param_name
            .as_ref()
            .filter(|n| !n.starts_with('@') && !n.starts_with('%'))
            .map(|name| (name.clone(), self.env().get(name).cloned()));

        // Track loop-body declarations so closures created in the body capture
        // them per-iteration (owned_captures). Balanced by pop on every exit.
        self.push_loop_local_scope();

        self.env_dirty = true;
        // When resuming a gather coroutine, start from the saved position.
        let mut i = if let Some(crate::value::ForLoopResumeState::IntRange { current, .. }) =
            self.gather_for_loop_resume.take()
        {
            current
        } else {
            start
        };

        // Pre-mark readonly before the loop to avoid per-iteration HashSet
        // insertions. The for loop parameter is readonly for the duration.
        // Skip @-sigil and %-sigil params (mutable containers).
        if !spec.is_rw {
            if let Some(ref name) = param_name {
                if !name.starts_with('@') && !name.starts_with('%') {
                    self.interpreter.mark_readonly(name);
                }
            } else {
                self.interpreter.mark_readonly("_");
            }
        }

        // Check if we can skip the per-iteration env insert. When the loop
        // has a local slot for the parameter AND the body doesn't reference
        // the parameter from the env (closures, etc.), we can use just the
        // local slot for much better performance in tight loops.
        let use_local_only = spec.param_local.is_some() && param_name.is_none();

        // Use <= for inclusive ranges instead of end_val + 1 to avoid overflow
        // when end_val is i64::MAX
        'for_loop: while if inclusive { i <= end_val } else { i < end_val } {
            let item = Value::Int(i);
            self.topic_source_var = None;

            if !use_local_only && param_name.is_none() {
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
            'body_redo: loop {
                match self.run_range(code, body_start, loop_end, compiled_fns) {
                    Ok(()) => {
                        if !code.state_locals.is_empty() {
                            self.sync_state_locals_in_range(code, body_start, loop_end);
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &param_name,
                            (i - start) as usize,
                        );
                        break 'body_redo;
                    }
                    Err(e) if e.is_succeed => {
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo && Self::label_matches(&e.label, &spec.label) => {
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
                            && e.leave_callable_id.is_none()
                            && e.leave_routine.is_none()
                            && Self::label_matches(&e.label, &spec.label) =>
                    {
                        if let Some(v) = e.return_value {
                            self.env_mut().insert("_".to_string(), v.clone());
                            self.stack.push(v);
                        }
                        break 'for_loop;
                    }
                    Err(e) if e.is_last && Self::label_matches(&e.label, &spec.label) => {
                        break 'for_loop;
                    }
                    Err(e) if e.is_next && Self::label_matches(&e.label, &spec.label) => {
                        break 'body_redo;
                    }
                    Err(e)
                        if e.message
                            == crate::runtime::Interpreter::LAZY_GATHER_TAKE_LIMIT_SIGNAL =>
                    {
                        // Save for-loop state for gather coroutine resumption.
                        self.gather_for_loop_resume =
                            Some(crate::value::ForLoopResumeState::IntRange {
                                current: i.saturating_add(1),
                                end_val,
                                inclusive,
                            });
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.interpreter.readonly_vars_mut().remove(name);
                        }
                        if !was_topic_readonly {
                            self.interpreter.unmark_readonly("_");
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
                        // Gather suspend: pop (body resumes and re-pushes).
                        self.pop_loop_local_scope(code);
                        return Err(e);
                    }
                    Err(e) => {
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.interpreter.readonly_vars_mut().remove(name);
                        }
                        if !was_topic_readonly {
                            self.interpreter.unmark_readonly("_");
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
                        self.pop_loop_local_scope(code);
                        return Err(e);
                    }
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
            // Use checked_add to avoid overflow when i is i64::MAX
            match i.checked_add(1) {
                Some(next) => i = next,
                None => break, // i was i64::MAX, no more values possible
            }
        }
        // Unmark readonly params after loop completion
        if !spec.is_rw
            && let Some(ref name) = param_name
        {
            self.interpreter.readonly_vars_mut().remove(name);
        }
        if !was_topic_readonly {
            self.interpreter.unmark_readonly("_");
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
        // Defer restoring the named loop param's prior binding to the paired
        // `RestoreForParam` opcode (after the post/LAST phasers), matching
        // `exec_for_loop_body`. Only reached on normal completion / `last` /
        // `leave` (the early `return Err(...)` paths above skip this, so the
        // push/pop stay balanced as the frame unwinds past `RestoreForParam`).
        if let Some(entry) = saved_param {
            self.for_param_restore_stack.push(entry);
        }
        self.pop_loop_local_scope(code);
        Ok(())
    }

    /// Iterate lazily over a gather-based LazyList.
    /// Pulls items one at a time via `force_lazy_list_vm_n`, avoiding
    /// full materialization of potentially infinite sequences.
    #[allow(clippy::too_many_arguments)]
    fn exec_for_loop_lazy_gather(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        ll: &crate::value::LazyList,
        body_start: usize,
        loop_end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        self.exec_for_loop_lazy_gather_from(code, spec, ll, 0, body_start, loop_end, compiled_fns)
    }

    #[allow(clippy::too_many_arguments)]
    fn exec_for_loop_lazy_gather_from(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        ll: &crate::value::LazyList,
        start_idx: usize,
        body_start: usize,
        loop_end: usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let ll_arc = {
            // Get the Arc from somewhere. We need to find the LazyList Arc.
            // Since we have a &LazyList reference, we reconstruct the Arc
            // by looking at the cache pointer identity. The caller should
            // have the Arc available. For now, we create a cheap wrapper
            // that shares the same cache/coroutine via raw pointer.
            // Actually, we need to find a way to get the Arc.
            // Let's use std::sync::Arc::from_raw/into_raw trick.
            // Since ll comes from Value::LazyList(Arc<LazyList>), we can
            // reconstruct the Arc from the pointer.
            // SAFETY: The caller holds an Arc<LazyList> on the stack,
            // keeping the refcount >= 1 for the duration of this call.
            unsafe {
                let ptr = ll as *const crate::value::LazyList;
                std::sync::Arc::increment_strong_count(ptr);
                std::sync::Arc::from_raw(ptr)
            }
        };
        let param_name = spec
            .param_idx
            .map(|idx| match &code.constants[idx as usize] {
                Value::Str(s) => s.to_string(),
                _ => unreachable!("ForLoop param must be a string constant"),
            });
        let saved_topic = spec
            .restore_topic
            .then(|| self.env().get("_").cloned())
            .flatten();
        let saved_topic_source = self.topic_source_var.take();
        let was_topic_readonly = self.interpreter.readonly_vars().contains("_");

        self.env_dirty = true;

        if !spec.is_rw {
            if let Some(ref name) = param_name {
                if !name.starts_with('@') && !name.starts_with('%') {
                    self.interpreter.mark_readonly(name);
                }
            } else {
                self.interpreter.mark_readonly("_");
            }
        }

        let mut idx: usize = start_idx;
        'for_loop: loop {
            // Force one more element from the lazy list
            let items = self.force_lazy_list_vm_n(ll, idx + 1)?;
            if idx >= items.len() {
                break; // No more elements
            }
            let item = items[idx].clone();
            idx += 1;

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
            'body_redo: loop {
                match self.run_range(code, body_start, loop_end, compiled_fns) {
                    Ok(()) => {
                        if !code.state_locals.is_empty() {
                            self.sync_state_locals_in_range(code, body_start, loop_end);
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_succeed => {
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo && Self::label_matches(&e.label, &spec.label) => {
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
                            && e.leave_callable_id.is_none()
                            && e.leave_routine.is_none()
                            && Self::label_matches(&e.label, &spec.label) =>
                    {
                        break 'for_loop;
                    }
                    Err(e) if e.is_last && Self::label_matches(&e.label, &spec.label) => {
                        break 'for_loop;
                    }
                    Err(e) if e.is_next && Self::label_matches(&e.label, &spec.label) => {
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
                            self.interpreter.readonly_vars_mut().remove(name);
                        }
                        if !was_topic_readonly {
                            self.interpreter.unmark_readonly("_");
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
                            self.interpreter.readonly_vars_mut().remove(name);
                        }
                        if !was_topic_readonly {
                            self.interpreter.unmark_readonly("_");
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
            if self.interpreter.is_halted() {
                break;
            }
        }

        if !spec.is_rw
            && let Some(ref name) = param_name
        {
            self.interpreter.readonly_vars_mut().remove(name);
        }
        if !was_topic_readonly {
            self.interpreter.unmark_readonly("_");
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
    fn exec_for_loop_lazy_io_lines(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        handle: &Value,
        kv: bool,
        words: bool,
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
            .then(|| self.env().get("_").cloned())
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
            // Read the next record (word or line) from the handle
            let line = if words {
                self.interpreter.read_word_from_handle_value(handle)?
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

    /// Read the loop's current writeback value (implicit `$_` or a single rw
    /// param) for rw aggregate-view (grep) writeback.
    fn grep_view_topic_value(
        &self,
        writes_back_topic: bool,
        rw_writeback: bool,
        param_name: &Option<String>,
    ) -> Option<Value> {
        if writes_back_topic {
            self.env().get("_").cloned()
        } else if rw_writeback {
            let var = param_name.as_deref().unwrap_or("_");
            self.env().get(var).cloned()
        } else {
            None
        }
    }

    /// Write a mutated whole-container topic back to its source variable after a
    /// `given`/`with` block: `given @a { .push(4) }` aliases `@a`, so a container
    /// mutation through `$_` must propagate. Only for `@`/`%`-sigil sources
    /// (whole-container topicalization); skipped when the topic is unchanged
    /// (same `Arc`) so a read-only `given` stays a no-op.
    fn write_back_given_topic(
        &mut self,
        code: &CompiledCode,
        source: &Option<String>,
        pointy_param: &Option<String>,
    ) {
        let Some(source) = source else {
            return;
        };
        if !source.starts_with('@') && !source.starts_with('%') {
            return;
        }
        // For a pointy block, write back the bound parameter's final value
        // (`@p` after `@p.push`); otherwise the topic `$_`.
        let current = match pointy_param {
            Some(p) => self.get_env_with_main_alias(p),
            None => self.env().get("_").cloned(),
        };
        let Some(current) = current else {
            return;
        };
        if let Some(orig) = self.get_env_with_main_alias(source)
            && Self::loop_var_unchanged(&current, &orig)
        {
            return;
        }
        self.set_env_with_main_alias(source, current.clone());
        self.update_local_if_exists(code, source, &current);
    }

    /// Write the final `$_` back to an lvalue container element (`given %h<k>`
    /// / `given @a[i]`). The element source is `(container var, index,
    /// positional)`. This makes both `$_ = ...` (whole reassign) and container
    /// mutations (`.push`) propagate to the original element, matching Raku's
    /// aliasing of an element topic.
    fn write_back_element_source(
        &mut self,
        code: &CompiledCode,
        src: &(String, Value, bool),
        pointy_param: &Option<String>,
    ) {
        let (container, index, _positional) = src;
        // For a pointy block, write back the bound parameter's final value;
        // otherwise the topic `$_`.
        let current = match pointy_param {
            Some(p) => self.get_env_with_main_alias(p),
            None => self.env().get("_").cloned(),
        };
        let Some(current) = current else {
            return;
        };
        let key = match index {
            Value::Int(i) => i.to_string(),
            Value::Str(s) => s.as_ref().clone(),
            other => other.to_string_value(),
        };
        let Some(mut cval) = self.get_env_with_main_alias(container) else {
            return;
        };
        Self::assign_into_nested_container(&mut cval, &key, current);
        self.set_env_with_main_alias(container, cval.clone());
        self.update_local_if_exists(code, container, &cval);
    }

    /// Whether the loop variable still holds the same binding as the source
    /// element, so writing it back would be a no-op. Conservative: returns
    /// `true` only when provably unchanged (same container `Arc`, so in-place
    /// mutation already reached the source; or an equal immutable scalar).
    /// Anything else returns `false` and falls through to the full writeback.
    fn loop_var_unchanged(current: &Value, source_elem: &Value) -> bool {
        use std::sync::Arc;
        match (current, source_elem) {
            (Value::Array(a, _), Value::Array(b, _)) => Arc::ptr_eq(a, b),
            (Value::Hash(a), Value::Hash(b)) => Arc::ptr_eq(a, b),
            (Value::Str(a), Value::Str(b)) => Arc::ptr_eq(a, b) || a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            _ => false,
        }
    }

    fn write_back_for_topic_item(
        &mut self,
        code: &CompiledCode,
        source_var: &Option<String>,
        param_name: &Option<String>,
        idx: usize,
        reversed: bool,
        total_items: usize,
    ) {
        let Some(source) = source_var else {
            return;
        };
        if !source.starts_with('@') {
            return;
        }
        // The loop variable written back to the source element is the named
        // param (`for @m -> @row {...}` / `-> $row {...}` — Raku aliases the
        // element, so `.push`/`@row[0]=v` propagate), or otherwise the implicit
        // topic `$_` (`for @m {...}`).
        let loop_var = param_name.as_deref().unwrap_or("_");
        let Some(current_topic) = self.env().get(loop_var).cloned() else {
            return;
        };
        let Some(Value::Array(items, kind)) = self.get_env_with_main_alias(source) else {
            return;
        };
        let actual_idx = if reversed && total_items > 0 {
            total_items - 1 - idx
        } else {
            idx
        };
        if actual_idx >= items.len() {
            return;
        }
        // Skip the O(n) source rebuild when the loop variable is provably
        // unchanged — otherwise even a read-only loop (`for @a { say $_ }`) or
        // an empty body would rebuild the whole backing array every iteration,
        // making the loop O(n^2). When the value is the same (same container
        // Arc, so any in-place mutation is already visible in the source, or an
        // equal immutable scalar), the writeback is a no-op.
        if Self::loop_var_unchanged(&current_topic, &items[actual_idx]) {
            return;
        }
        let mut updated = items.to_vec();
        updated[actual_idx] = current_topic;
        let updated_value = Value::Array(
            std::sync::Arc::new(crate::value::ArrayData::new(updated)),
            kind,
        );
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
        let Some(current_val) = self.env().get(var_name).cloned() else {
            return;
        };
        let target = &source_var_names[idx];
        self.env_mut().insert(target.clone(), current_val.clone());
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
                    if let Some(key) = self.env().get(key_name).cloned()
                        && let Some(val) = self.env().get(val_name).cloned()
                        && let Some(Value::Hash(hash_items)) = self.get_env_with_main_alias(source)
                    {
                        let mut updated = hash_items.as_ref().clone();
                        let key_str = key.to_string_value();
                        updated.insert(key_str, val);
                        let updated_value = Value::Hash(Value::hash_arc(updated));
                        self.set_env_with_main_alias(source, updated_value.clone());
                        self.update_local_if_exists(code, source, &updated_value);
                    }
                } else if !kv_mode {
                    // %hash.values -> $val is rw: positional writeback using pre-captured key order
                    let var_name = param_name.as_deref().unwrap_or("_");
                    if let Some(keys) = hash_keys
                        && let Some(val) = self.env().get(var_name).cloned()
                        && let Some(Value::Hash(hash_items)) = self.get_env_with_main_alias(source)
                        && idx < keys.len()
                    {
                        let mut updated = hash_items.as_ref().clone();
                        updated.insert(keys[idx].clone(), val);
                        let updated_value = Value::Hash(Value::hash_arc(updated));
                        self.set_env_with_main_alias(source, updated_value.clone());
                        self.update_local_if_exists(code, source, &updated_value);
                    }
                }
                return;
            };
            if kv_mode && arity > 1 && rw_param_names.len() >= 2 {
                // .kv mode: chunk is [key, val]. Write back val at key position.
                let val_name = &rw_param_names[1];
                if let Some(val) = self.env().get(val_name).cloned()
                    && idx < items.len()
                {
                    let mut updated = items.to_vec();
                    updated[idx] = val;
                    let updated_value = Value::Array(
                        std::sync::Arc::new(crate::value::ArrayData::new(updated)),
                        kind,
                    );
                    self.set_env_with_main_alias(source, updated_value.clone());
                    self.update_local_if_exists(code, source, &updated_value);
                }
            } else if arity > 1 && !rw_param_names.is_empty() {
                // Multi-param rw: read each named param and write back to the array
                let base = idx * arity;
                let mut updated = items.to_vec();
                for (j, pname) in rw_param_names.iter().enumerate() {
                    if base + j < updated.len()
                        && let Some(val) = self.env().get(pname).cloned()
                    {
                        updated[base + j] = val;
                    }
                }
                let updated_value = Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(updated)),
                    kind,
                );
                self.set_env_with_main_alias(source, updated_value.clone());
                self.update_local_if_exists(code, source, &updated_value);
            } else {
                // Single-param rw: read the named param (or $_) and write back
                let var_name = param_name.as_deref().unwrap_or("_");
                let Some(current_val) = self.env().get(var_name).cloned() else {
                    return;
                };
                if idx >= items.len() {
                    return;
                }
                // Skip the O(n) rebuild when the rw variable is unchanged — a
                // read-only `<->` loop (`for @big <-> $x { $s += $x }`) would
                // otherwise be O(n^2). See `loop_var_unchanged`.
                if Self::loop_var_unchanged(&current_val, &items[idx]) {
                    return;
                }
                let mut updated = items.to_vec();
                updated[idx] = current_val;
                let updated_value = Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(updated)),
                    kind,
                );
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
                    if let Some(val) = self.env().get(val_name).cloned() {
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
            let Some(current_val) = self.env().get(var_name).cloned() else {
                return;
            };
            // If the source variable holds a Pair, update only the pair's value
            // (this handles `for $pair.value -> $v is rw { ... }`). The source may
            // be a `ContainerRef` (a closure-captured / `:=`-bound `$pair`); deref
            // it to inspect the Pair and write back THROUGH the shared container so
            // we don't clobber `$pair` itself with a bare scalar.
            let raw_source = self.get_env_with_main_alias(source);
            let writeback_val = match raw_source.as_ref().map(|v| v.deref_container()) {
                Some(Value::Pair(key, _)) => Value::Pair(key, Box::new(current_val.clone())),
                Some(Value::ValuePair(key, _)) => {
                    Value::ValuePair(key, Box::new(current_val.clone()))
                }
                _ => current_val.clone(),
            };
            if let Some(Value::ContainerRef(arc)) = &raw_source {
                arc.lock().unwrap().clone_from(&writeback_val);
            } else {
                self.set_env_with_main_alias(source, writeback_val.clone());
                self.update_local_if_exists(code, source, &writeback_val);
            }
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

        // When resuming a gather coroutine that suspended inside this loop, the
        // marker is consumed here; the loop's actual state lives in locals/env
        // so re-entering from the top (cond re-check) continues correctly.
        if matches!(
            self.gather_for_loop_resume,
            Some(crate::value::ForLoopResumeState::CStyleLoop)
        ) {
            self.gather_for_loop_resume = None;
        }

        // Track loop-body declarations for per-iteration closure capture
        // (owned_captures). Balanced by pop on every exit path.
        self.push_loop_local_scope();

        'c_loop: loop {
            self.loop_cond_active = true;
            let cond_res = self.run_range(code, cond_start, body_start, compiled_fns);
            self.loop_cond_active = false;
            if let Err(e) = cond_res {
                self.pop_loop_local_scope(code);
                return Err(e);
            }
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
                                self.env_mut().insert("_".to_string(), v);
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
                    Err(e)
                        if e.message
                            == crate::runtime::Interpreter::LAZY_GATHER_TAKE_LIMIT_SIGNAL =>
                    {
                        // Gather coroutine suspend inside a `loop`/`while`/C-style
                        // loop. Run this iteration's step now (it would otherwise
                        // run after the body's normal completion below), so that
                        // re-entering the loop opcode on resume continues at the
                        // condition check exactly where normal flow would. Then
                        // park a marker so the outer forcer keeps `*ip` on this
                        // loop opcode (it stays unchanged on this Err path).
                        if let Err(step_err) =
                            self.run_range(code, step_begin, loop_end, compiled_fns)
                        {
                            self.pop_loop_local_scope(code);
                            return Err(step_err);
                        }
                        self.gather_for_loop_resume =
                            Some(crate::value::ForLoopResumeState::CStyleLoop);
                        self.pop_loop_local_scope(code);
                        return Err(e);
                    }
                    Err(e) => {
                        self.pop_loop_local_scope(code);
                        return Err(e);
                    }
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
            if let Err(e) = self.run_range(code, step_begin, loop_end, compiled_fns) {
                self.pop_loop_local_scope(code);
                return Err(e);
            }
        }
        self.pop_loop_local_scope(code);
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
        topic_readonly: bool,
        pointy_param_idx: Option<u32>,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let topic = self.stack.pop().unwrap();
        // For a pointy block (`given @a -> @p { ... }`), the writeback reads the
        // bound parameter's final value rather than `$_` (Raku binds `@p` to the
        // topic but leaves `$_` undefined). `is copy` is not recorded here, so it
        // copies and does not write back.
        let pointy_param: Option<String> =
            pointy_param_idx.map(|idx| match &code.constants[idx as usize] {
                Value::Str(s) => s.to_string(),
                other => other.to_string_value(),
            });
        let body_start = *ip + 1;
        let end = body_end as usize;
        let stack_base = self.stack.len();

        let saved_topic = self.env().get("_").cloned();
        let saved_when = self.interpreter.when_matched();
        let saved_topic_source = self.topic_source_var.take();
        let saved_element_source = self.element_source.take();
        let container_binding = self.container_ref_var.take();
        // An element-source topic (`given %h<k>` / `given @a[i]`) aliases an
        // lvalue element: the final `$_` is written back to that element below,
        // so `$_ = ...` (whole reassign) AND `.push` both propagate. Don't set
        // `topic_source_var` (that is for whole-variable writeback).
        let element_source = saved_element_source.clone();
        if element_source.is_none() {
            self.topic_source_var = container_binding.clone();
        }
        self.env_mut().insert("_".to_string(), topic);
        loan_env!(self, set_when_matched(false));
        // A read-only topic (`given @a` / `given 42` / `given expr()`) forbids
        // `$_ = ...`; container *mutation* (`.push`) is still allowed and is
        // written back to the source below. A bare scalar var (`given $x`) is rw,
        // and an element source (handled above) is rw too.
        //
        // A pointy block (`given @a -> @p`) leaves `$_` undefined in Raku and
        // makes `@p` a fully-writable alias of the source (`@p = (...)`,
        // `@p[0]=v`, and `@p.push` all propagate). So when a pointy param is
        // present, don't mark `$_` read-only: that would propagate read-only to
        // `@p` through its `@p := $_` bind and block element assignment.
        let mark_ro = topic_readonly
            && pointy_param.is_none()
            && !self.interpreter.readonly_vars().contains("_");
        if mark_ro {
            self.interpreter.mark_readonly("_");
        }

        let restore = |this: &mut Self, write_back: bool| {
            if mark_ro {
                this.interpreter.unmark_readonly("_");
            }
            if write_back {
                if let Some(src) = &element_source {
                    this.write_back_element_source(code, src, &pointy_param);
                } else {
                    this.write_back_given_topic(code, &container_binding, &pointy_param);
                }
            }
            this.interpreter.set_when_matched(saved_when);
            if let Some(v) = saved_topic.clone() {
                this.interpreter.env_mut().insert("_".to_string(), v);
            } else {
                this.interpreter.env_mut().remove("_");
            }
            // A pointy parameter (`-> @p`) is block-scoped in Raku, but mutsu
            // desugars it to a global `@p := $_` whose alias/bound markers would
            // otherwise leak past this block. Clear them so a later block reusing
            // the name (e.g. `given @c -> @p is copy { ... }`, a plain assign that
            // would otherwise follow the stale `__mutsu_sigilless_alias::@p` and
            // corrupt `$_`) starts clean. Done after the writeback above, which
            // still reads the parameter's final value.
            if let Some(p) = &pointy_param {
                this.interpreter
                    .env_mut()
                    .remove(&format!("__mutsu_sigilless_alias::{}", p));
                this.interpreter
                    .env_mut()
                    .remove(&format!("__mutsu_sigilless_readonly::{}", p));
                this.interpreter
                    .env_mut()
                    .remove(&format!("__mutsu_bound_decont::{}", p));
                this.interpreter.unmark_readonly(p);
            }
            this.topic_source_var = saved_topic_source.clone();
            // `element_source` is a one-shot signal set by `TagElementSource`
            // immediately before this `Given`, so consuming it must clear it (not
            // restore the just-set value). Re-setting `saved_element_source` here
            // leaked the element source to the next `given`, which then routed its
            // whole-container writeback through `write_back_element_source` and
            // dropped the mutation (a non-element `given @a -> @p` after a
            // `given %h<k>` would not propagate `@p.push`).
            this.element_source = None;
        };

        let mut inner_ip = body_start;
        while inner_ip < end {
            if let Err(e) = self.exec_one(code, &mut inner_ip, compiled_fns) {
                if e.is_succeed {
                    self.stack.truncate(stack_base);
                    if let Some(v) = e.return_value {
                        self.stack.push(v);
                    }
                    restore(self, true);
                    *ip = end;
                    return Ok(());
                }
                restore(self, false);
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

        restore(self, true);
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

        let saved_topic = self.env().get("_").cloned();
        let saved_when = self.interpreter.when_matched();
        self.env_mut().insert("_".to_string(), topic);
        loan_env!(self, set_when_matched(false));

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
                loan_env!(self, set_when_matched(true));
            }
            Err(e) => {
                loan_env!(self, set_when_matched(saved_when));
                if let Some(v) = saved_topic {
                    self.env_mut().insert("_".to_string(), v);
                } else {
                    self.env_mut().remove("_");
                }
                return Err(e);
            }
        }

        loan_env!(self, set_when_matched(saved_when));
        if let Some(v) = saved_topic {
            self.env_mut().insert("_".to_string(), v);
        } else {
            self.env_mut().remove("_");
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
            let topic = self.env().get("_").cloned().unwrap_or(Value::Nil);
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
                            if pd.slurpy || pd.double_slurpy || pd.onearg {
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
                        self.vm_call_sub_value(cond_val.clone(), call_args, false)
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
                    loan_env!(self, set_when_matched(true));
                    return Err(e);
                }
                // The `when` matched, so record the match before propagating any
                // other control signal (e.g. an is_return produced by `done`
                // inside the block). Otherwise a `when` body that exits via a
                // control flow signal would lose the fact that it matched.
                Err(e) => {
                    loan_env!(self, set_when_matched(true));
                    return Err(e);
                }
            }
            if !did_proceed {
                loan_env!(self, set_when_matched(true));
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
                loan_env!(self, set_when_matched(true));
                return Err(e);
            }
            Err(e) => return Err(e),
        }
        loan_env!(self, set_when_matched(true));
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

        // Track loop-body declarations for per-iteration closure capture
        // (owned_captures). Balanced by pop on every exit path.
        self.push_loop_local_scope();

        let mut first = true;
        'repeat_loop: loop {
            if !first {
                self.loop_cond_active = true;
                let cond_res = self.run_range(code, cond_start, loop_end, compiled_fns);
                self.loop_cond_active = false;
                if let Err(e) = cond_res {
                    self.pop_loop_local_scope(code);
                    return Err(e);
                }
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
                            self.env_mut().insert("_".to_string(), v);
                        }
                        break 'repeat_loop;
                    }
                    Err(e) if e.is_last && Self::label_matches(&e.label, label) => {
                        break 'repeat_loop;
                    }
                    Err(e) if e.is_next && Self::label_matches(&e.label, label) => {
                        break 'body_redo;
                    }
                    Err(e) => {
                        self.pop_loop_local_scope(code);
                        return Err(e);
                    }
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }
        self.pop_loop_local_scope(code);
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
        // Reset any leftover resume_ip from previous exception handling so a
        // later .resume cannot accidentally jump into a sibling scope.
        let saved_resume_ip = self.resume_ip.take();
        let result = self.exec_try_catch_op_inner(
            code,
            catch_start,
            control_start,
            body_end,
            explicit_catch,
            ip,
            compiled_fns,
        );
        // Restore the outer resume_ip so an outer .resume sees the right
        // point. When the inner block escaped with an error (e.g. rethrow
        // of CX::Warn), keep the inner resume_ip so an outer handler can
        // resume at the original call site.
        if result.is_ok() {
            self.resume_ip = saved_resume_ip;
        }
        result
    }

    #[allow(clippy::too_many_arguments)]
    fn exec_try_catch_op_inner(
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
        let has_control = control_begin < end;
        if has_control {
            self.interpreter.control_handler_depth += 1;
        }
        // Guard the protected body with a panic->X:: boundary so an internal
        // Rust panic (overflow/OOB/unwrap) raised anywhere inside it becomes a
        // catchable exception routed to the CATCH handler, instead of crashing.
        let body_result = self.run_range_guarded(code, body_start, catch_begin, compiled_fns);
        if has_control {
            self.interpreter.control_handler_depth -= 1;
        }
        match body_result {
            Ok(()) => {
                // Successful try resets $! to Nil
                self.env_mut().insert("!".to_string(), Value::Nil);
                self.interpreter.discard_let_saves(let_mark);
                // A `try` that completes normally but yields a soft Failure value
                // (e.g. the result of an expression that returned a Failure rather
                // than throwing) handles that Failure: its value is now "caught",
                // so subsequent uses must not re-throw the stored exception.
                if let Some(top) = self.stack.last()
                    && matches!(top, Value::Instance { class_name, .. } if class_name == "Failure")
                {
                    top.mark_failure_handled();
                }
                *ip = end;
                Ok(())
            }
            Err(e) if e.is_return => {
                self.interpreter.discard_let_saves(let_mark);
                if control_begin < end {
                    self.stack.truncate(saved_depth);
                    let saved_topic = self.env().get("_").cloned();
                    if let Some(signal_topic) = Self::control_signal_topic_value(&e) {
                        self.env_mut().insert("_".to_string(), signal_topic);
                    }
                    let saved_when = self.interpreter.when_matched();
                    loan_env!(self, set_when_matched(false));
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
                    loan_env!(self, set_when_matched(saved_when));
                    if let Some(v) = saved_topic {
                        self.env_mut().insert("_".to_string(), v);
                    } else {
                        self.env_mut().remove("_");
                    }
                    *ip = end;
                    Ok(())
                } else {
                    Err(e)
                }
            }
            Err(e)
                if e.return_value.is_some()
                    && !e.is_succeed
                    && !e.is_warn
                    && !e.is_take
                    && !e.is_emit =>
            {
                self.interpreter.discard_let_saves(let_mark);
                Err(e)
            }
            // Control signals (warn, last, next, redo, etc.) without a CONTROL
            // block must propagate up — `try` alone does not catch them.
            Err(e)
                if (e.is_last
                    || e.is_next
                    || e.is_redo
                    || e.is_proceed
                    || e.is_succeed
                    || e.is_warn
                    || e.is_take
                    || e.is_emit
                    || e.is_done
                    || e.is_react_done)
                    && control_begin >= end =>
            {
                self.interpreter.discard_let_saves(let_mark);
                Err(e)
            }
            Err(e)
                if (e.is_last
                    || e.is_next
                    || e.is_redo
                    || e.is_proceed
                    || e.is_succeed
                    || e.is_warn
                    || e.is_take
                    || e.is_emit
                    || e.is_done
                    || e.is_react_done)
                    && control_begin < end =>
            {
                self.interpreter.discard_let_saves(let_mark);
                self.stack.truncate(saved_depth);
                let saved_topic = self.env().get("_").cloned();
                let saved_when = self.interpreter.when_matched();
                let mut pending_err = e;
                loop {
                    if let Some(signal_topic) = Self::control_signal_topic_value(&pending_err) {
                        self.env_mut().insert("_".to_string(), signal_topic);
                    }
                    loan_env!(self, set_when_matched(false));
                    let control_result = self.run_range(code, control_begin, end, compiled_fns);
                    let next_resume = match control_result {
                        Ok(()) => None,
                        Err(ref ce) if ce.is_succeed => None,
                        // Re-throwing a CX::Warn from CONTROL acts like the
                        // default warn handler: print to stderr and resume.
                        Err(ce) if ce.is_warn => {
                            if !self.interpreter.warning_suppressed() {
                                self.interpreter.write_warn_to_stderr(&ce.message);
                            }
                            self.resume_ip.take()
                        }
                        Err(ce) if ce.is_resume => {
                            let _ = ce;
                            self.resume_ip.take()
                        }
                        Err(ce) => {
                            loan_env!(self, set_when_matched(saved_when));
                            if let Some(v) = saved_topic {
                                self.env_mut().insert("_".to_string(), v);
                            } else {
                                self.env_mut().remove("_");
                            }
                            return Err(ce);
                        }
                    };
                    match next_resume {
                        None => break,
                        Some(resume_point) => {
                            loan_env!(self, set_when_matched(saved_when));
                            if let Some(v) = saved_topic.clone() {
                                self.env_mut().insert("_".to_string(), v);
                            } else {
                                self.env_mut().remove("_");
                            }
                            // A resumable *warn* raised mid-expression (e.g. a Nil
                            // coercion: `Nil.ords`, `Nil.Int`) carries the value
                            // the suspended call should evaluate to. The stack was
                            // truncated to the block base above, so push that value
                            // back so execution continues from the call site with
                            // the call's result in place. Gate strictly on `is_warn`:
                            // other control signals (take/emit/done) also carry a
                            // `return_value` for their own machinery, which must NOT
                            // land on the VM operand stack here.
                            if pending_err.is_warn
                                && let Some(rv) = pending_err.return_value.take()
                            {
                                self.stack.push(rv);
                            }
                            if has_control {
                                self.interpreter.control_handler_depth += 1;
                            }
                            let body_result =
                                self.run_range(code, resume_point, catch_begin, compiled_fns);
                            if has_control {
                                self.interpreter.control_handler_depth -= 1;
                            }
                            match body_result {
                                Ok(()) => {
                                    *ip = end;
                                    return Ok(());
                                }
                                Err(new_err)
                                    if new_err.is_last
                                        || new_err.is_next
                                        || new_err.is_redo
                                        || new_err.is_proceed
                                        || new_err.is_succeed
                                        || new_err.is_warn
                                        || new_err.is_take
                                        || new_err.is_emit
                                        || new_err.is_done
                                        || new_err.is_react_done =>
                                {
                                    pending_err = new_err;
                                    continue;
                                }
                                Err(new_err) => return Err(new_err),
                            }
                        }
                    }
                }
                loan_env!(self, set_when_matched(saved_when));
                if let Some(v) = saved_topic {
                    self.env_mut().insert("_".to_string(), v);
                } else {
                    self.env_mut().remove("_");
                }
                *ip = end;
                Ok(())
            }
            Err(e) => {
                // Exception — restore let saves
                loan_env!(self, restore_let_saves(let_mark));
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
                    if let Some(line) = e.line {
                        exc_attrs.insert("line".to_string(), Value::Int(line as i64));
                    }
                    if let Some(ref bt) = e.backtrace {
                        // Build a Backtrace object from the string for
                        // legacy errors that only have a string backtrace.
                        let bt_val = Self::backtrace_value_from_string(bt);
                        exc_attrs.insert("backtrace".to_string(), bt_val);
                    }
                    // An untyped runtime error surfaces as X::AdHoc in Raku (the
                    // class a bare `die "msg"` produces), not the abstract base
                    // Exception. X::AdHoc IS-A Exception, so `throws-like
                    // ..., Exception` / `isa-ok $!, Exception` still match.
                    Value::make_instance(Symbol::intern("X::AdHoc"), exc_attrs)
                };
                let saved_topic = self.env().get("_").cloned();
                self.env_mut().insert("!".to_string(), err_val.clone());
                self.env_mut().insert("_".to_string(), err_val);
                let saved_when = self.interpreter.when_matched();
                loan_env!(self, set_when_matched(false));
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
                            loan_env!(self, set_when_matched(saved_when));
                            if let Some(v) = saved_topic {
                                self.env_mut().insert("_".to_string(), v);
                            } else {
                                self.env_mut().remove("_");
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
                // Propagate when_handled upward so an enclosing CATCH region
                // can detect that this nested CATCH (e.g., a CATCH inside a
                // CATCH) handled the exception.
                self.interpreter
                    .set_when_matched(saved_when || when_handled);
                if let Some(v) = saved_topic {
                    self.env_mut().insert("_".to_string(), v);
                } else {
                    self.env_mut().remove("_");
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
