use super::vm_control_ops::CStyleLoopSpec;
use super::*;

impl Interpreter {
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
                    Err(e) if e.is_succeed() => {
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo() && Self::label_matches(&e.label, &spec.label) => {
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
                    Err(e) if e.is_last() && Self::label_matches(&e.label, &spec.label) => {
                        break 'c_loop;
                    }
                    Err(e) if e.is_next() && Self::label_matches(&e.label, &spec.label) => {
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
            if self.is_halted() {
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
                    Err(e) if e.is_redo() && Self::label_matches(&e.label, label) => {
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
                    Err(e) if e.is_last() && Self::label_matches(&e.label, label) => {
                        break 'repeat_loop;
                    }
                    Err(e) if e.is_next() && Self::label_matches(&e.label, label) => {
                        break 'body_redo;
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
        *ip = loop_end;
        Ok(())
    }
}
