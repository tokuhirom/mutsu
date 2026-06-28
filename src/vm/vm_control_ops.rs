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
    /// When true (`%h.values` / `$b.values`), the loop variable aliases the
    /// container's value, so a `$_ = ...` topic writeback updates the source
    /// (plain Hash or mutable MixHash/BagHash) by key order.
    pub(super) values_mode: bool,
    /// Bare source array name for `for @a` (live-array iteration). See the
    /// `OpCode::ForLoop` field of the same name.
    pub(super) single_array_source: Option<String>,
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

impl Interpreter {
    pub(super) fn collect_loop_value(coll: &mut Vec<Value>, value: Value) {
        match value {
            Value::Slip(items) => coll.extend(items.iter().cloned()),
            other => coll.push(other),
        }
    }

    /// `for` over a non-itemized Blob/Buf iterates its bytes (raku: a `Blob`
    /// value, a `Blob:D`-typed param, or a `:=`-bound Blob yields its bytes).
    /// Returns the byte items when `iterable` is such a Blob, else `None` (so
    /// the caller falls back to `value_to_list`).
    ///
    /// Two shapes reach this point:
    /// - a bare Blob value (`for Blob.new(...)`), possibly behind a
    ///   `ContainerRef`;
    /// - the compiler's `for $scalar` → `[$scalar]` wrap, a single-element
    ///   `List`-kind array whose lone element is a Blob (this is how a Blob
    ///   *param* — the MIME::Base64 case — arrives).
    ///
    /// mutsu has no itemization marker for Blobs, so an *itemized* Blob
    /// (`my $b = Blob.new(...)`, `($blob,)`) is indistinguishable from a
    /// non-itemized one and also expands here — a divergence from raku (should
    /// be one item) that no roast Buf/Blob test exercises.
    pub(super) fn for_blob_byte_items(iterable: &Value) -> Option<Vec<Value>> {
        let deref = iterable.deref_container();
        if let Some(bytes) = crate::runtime::Interpreter::buf_as_byte_items(&deref) {
            return Some(bytes);
        }
        if let Value::Array(items, ArrayKind::List) = &deref
            && items.len() == 1
        {
            let elem = items[0].deref_container();
            return crate::runtime::Interpreter::buf_as_byte_items(&elem);
        }
        None
    }

    pub(crate) fn control_signal_topic_value(signal: &RuntimeError) -> Option<Value> {
        // User-defined classes doing X::Control carry their original
        // exception instance. Surface it directly so CONTROL blocks see the
        // real type rather than a generic CX::Done wrapper.
        if signal.is_done()
            && let Some(ex) = signal.exception.as_ref()
        {
            return Some((**ex).clone());
        }
        let class_name = if signal.is_last() {
            Some("CX::Last")
        } else if signal.is_next() {
            Some("CX::Next")
        } else if signal.is_redo() {
            Some("CX::Redo")
        } else if signal.is_proceed() {
            Some("CX::Proceed")
        } else if signal.is_succeed() {
            Some("CX::Succeed")
        } else if signal.is_warn() {
            Some("CX::Warn")
        } else if signal.is_take() {
            Some("CX::Take")
        } else if signal.is_emit() {
            Some("CX::Emit")
        } else if signal.is_done() || signal.is_react_done() {
            Some("CX::Done")
        } else if signal.is_return() {
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
        } else if signal.is_warn() {
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
    /// body register as loop-local (see `Interpreter::loop_local_vars`). A closure created
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
                        if let Err(e) = loan_env!(self, run_pending_instance_destroys()) {
                            self.pop_loop_local_scope(code);
                            return Err(e);
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_succeed() => {
                        if let Some(saved_topic) = &topic_before_body {
                            if let Some(v) = saved_topic.clone() {
                                self.env_mut().insert("_".to_string(), v);
                            } else {
                                self.env_mut().remove("_");
                            }
                        }
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo() && Self::label_matches(&e.label, &spec.label) => {
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
                    Err(e) if e.is_last() && Self::label_matches(&e.label, &spec.label) => {
                        break 'while_loop;
                    }
                    Err(e) if e.is_next() && Self::label_matches(&e.label, &spec.label) => {
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
            if self.is_halted() {
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
}
