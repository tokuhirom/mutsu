use super::vm_control_ops::ForLoopSpec;
use super::*;

impl Interpreter {
    /// Fast path for iterating over integer ranges (e.g., `for ^N`, `for 0..N`).
    /// Avoids materializing the entire range as a Vec<Value> by using a counter.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_for_loop_int_range(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        start: i64,
        end_val: i64,
        inclusive: bool,
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
        let saved_topic = spec
            .restore_topic
            .then(|| self.env().get("_").cloned())
            .flatten();
        let saved_topic_source = self.topic_source_var.take();
        let was_topic_readonly = self.is_readonly("_");

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

        // When resuming a gather coroutine, start from the saved position and
        // restore the chained inner state (a loop nested in this body) into
        // the slot so the next loop op encountered resumes too. A state with a
        // different `loop_ip` belongs to a different loop op — leave it there.
        let mut i = start;
        let this_code_id = code.ops.as_ptr() as usize;
        if self.gather_for_loop_resume.as_ref().is_some_and(|s| {
            s.code_id() == Some(this_code_id) && s.loop_ip() == Some(body_start - 1)
        }) && let Some(crate::value::ForLoopResumeState::IntRange {
            current,
            resume_body_ip,
            inner,
            ..
        }) = self.gather_for_loop_resume.take()
        {
            i = current;
            self.gather_for_loop_resume = inner.map(|b| *b);
            self.gather_resume_body_ip = resume_body_ip;
        }
        // Nested-resume entry: when the slot holds a state for a loop nested
        // INSIDE this body (its loop_ip lies in the body range), the resumed
        // iteration's first body run starts AT that loop op — re-running the
        // ops before it would replay completed sibling loops / side effects.
        let mut nested_entry: Option<usize> = self.gather_resume_body_ip.take().or_else(|| {
            self.gather_for_loop_resume
                .as_ref()
                .filter(|s| s.code_id() == Some(this_code_id))
                .and_then(|s| s.loop_ip())
                .filter(|lip| *lip > body_start && *lip < loop_end)
        });

        // Pre-mark readonly before the loop to avoid per-iteration HashSet
        // insertions. The for loop parameter is readonly for the duration.
        // Skip @-sigil and %-sigil params (mutable containers).
        if !spec.is_rw {
            if let Some(ref name) = param_name {
                if !name.starts_with('@') && !name.starts_with('%') {
                    self.mark_readonly(name);
                }
            } else {
                self.mark_readonly("_");
            }
        }

        // Check if we can skip the per-iteration env insert. When the loop
        // has a local slot for the parameter AND the body doesn't reference
        // the parameter from the env (closures, etc.), we can use just the
        // local slot for much better performance in tight loops.
        let use_local_only = spec.param_local.is_some() && param_name.is_none();

        // Pre-intern the per-iteration env keys once (J4d): `Env::insert` is
        // `note_env_key` + `Symbol::intern` + `insert_sym`, and interning the
        // same key (plus a `String` alloc for it) on every iteration profiled
        // at ~3% of a hot numeric loop. The careted-placeholder alias pair is
        // resolved here too, so the loop body is one or two `insert_sym`s.
        let topic_sym = (!use_local_only && param_name.is_none()).then(|| {
            crate::env::note_env_key("_");
            crate::symbol::Symbol::intern("_")
        });
        let param_sym = param_name.as_ref().map(|name| {
            crate::env::note_env_key(name);
            crate::symbol::Symbol::intern(name)
        });
        let caret_alias_sym = param_name.as_ref().and_then(|name| {
            let alias = if let Some(bare) = name.strip_prefix("&^") {
                format!("&{}", bare)
            } else if let Some(bare) = name.strip_prefix('^') {
                bare.to_string()
            } else {
                return None;
            };
            crate::env::note_env_key(&alias);
            Some(crate::symbol::Symbol::intern(&alias))
        });

        // Use <= for inclusive ranges instead of end_val + 1 to avoid overflow
        // when end_val is i64::MAX
        'for_loop: while if inclusive { i <= end_val } else { i < end_val } {
            let item = Value::int(i);
            self.topic_source_var = None;

            if let Some(sym) = topic_sym {
                self.env_mut().insert_sym(sym, item.clone());
            }
            if let Some(sym) = param_sym {
                self.env_mut().insert_sym(sym, item.clone());
                if let Some(alias) = caret_alias_sym {
                    self.env_mut().insert_sym(alias, item.clone());
                }
            }
            if let Some(slot) = spec.param_local {
                self.locals[slot as usize] = item.clone();
            }
            'body_redo: loop {
                let run_start = nested_entry.take().unwrap_or(body_start);
                let body_res = self.run_range(code, run_start, loop_end, compiled_fns);
                // State mutations persist on every exit path (`next`/`redo`/
                // `last`/exception), not just normal completion.
                if !code.state_locals.is_empty() {
                    self.sync_state_locals_in_range(code, body_start, loop_end);
                }
                match body_res {
                    Ok(()) => {
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &spec.source_var_locals,
                            &param_name,
                            (i - start) as usize,
                        );
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
                        if let Some(v) = e.return_value {
                            self.env_mut().insert("_".to_string(), v.clone());
                            self.stack.push(v);
                        }
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
                        // Save for-loop state for gather coroutine resumption.
                        // A state already in the slot belongs to a loop nested
                        // inside this body: chain it (and re-enter the CURRENT
                        // iteration so that loop can continue) instead of
                        // overwriting it.
                        let mut e = e;
                        let code_id = code.ops.as_ptr() as usize;
                        let nested = if self.gather_for_loop_resume.as_ref().is_some_and(|st| {
                            st.is_lexically_nested_in(code_id, body_start, loop_end)
                        }) {
                            self.gather_for_loop_resume.take()
                        } else {
                            None
                        };
                        let take_site = e.take_suspend_site().filter(|(cid, t)| {
                            *cid == code_id && *t >= body_start && *t < loop_end
                        });
                        if take_site.is_some() {
                            e.set_take_suspend_site(None);
                        }
                        let resume_body_ip = take_site.map(|(_, t)| t + 1);
                        self.gather_for_loop_resume =
                            Some(crate::value::ForLoopResumeState::IntRange {
                                current: if nested.is_some() || resume_body_ip.is_some() {
                                    i
                                } else {
                                    i.saturating_add(1)
                                },
                                end_val,
                                inclusive,
                                code_id,
                                loop_ip: body_start - 1,
                                resume_body_ip,
                                inner: nested.map(Box::new),
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
                        // Gather suspend: pop (body resumes and re-pushes).
                        self.pop_loop_local_scope(code);
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
                        self.pop_loop_local_scope(code);
                        return Err(e);
                    }
                }
            }
            if self.is_halted() {
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
}
