use super::*;

pub(super) struct ForLoopSpec {
    pub(super) param_idx: Option<u32>,
    pub(super) param_local: Option<u32>,
    pub(super) body_end: u32,
    pub(super) label: Option<String>,
}

pub(super) struct CStyleLoopSpec {
    pub(super) cond_end: u32,
    pub(super) step_start: u32,
    pub(super) body_end: u32,
    pub(super) label: Option<String>,
}

impl VM {
    pub(super) fn exec_while_loop_op(
        &mut self,
        code: &CompiledCode,
        cond_end: u32,
        body_end: u32,
        label: &Option<String>,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let cond_start = *ip + 1;
        let body_start = cond_end as usize;
        let loop_end = body_end as usize;

        'while_loop: loop {
            self.run_range(code, cond_start, body_start, compiled_fns)?;
            let cond_val = self.stack.pop().unwrap();
            if !cond_val.truthy() {
                break;
            }
            'body_redo: loop {
                match self.run_range(code, body_start, loop_end, compiled_fns) {
                    Ok(()) => break 'body_redo,
                    Err(e) if e.is_redo && Self::label_matches(&e.label, label) => {
                        continue 'body_redo;
                    }
                    Err(e) if e.is_last && Self::label_matches(&e.label, label) => {
                        break 'while_loop;
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

    pub(super) fn exec_for_loop_op(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let iterable = self.stack.pop().unwrap();
        let items = if let Value::LazyList(ref ll) = iterable {
            self.interpreter.force_lazy_list_bridge(ll)?
        } else {
            runtime::value_to_list(&iterable)
        };
        self.sync_locals_from_env(code);
        let body_start = *ip + 1;
        let loop_end = spec.body_end as usize;
        let param_name = spec
            .param_idx
            .map(|idx| match &code.constants[idx as usize] {
                Value::Str(s) => s.clone(),
                _ => unreachable!("ForLoop param must be a string constant"),
            });

        'for_loop: for item in items {
            self.interpreter
                .env_mut()
                .insert("_".to_string(), item.clone());
            if let Some(ref name) = param_name {
                self.interpreter
                    .env_mut()
                    .insert(name.clone(), item.clone());
            }
            if let Some(slot) = spec.param_local {
                self.locals[slot as usize] = item.clone();
            }
            'body_redo: loop {
                match self.run_range(code, body_start, loop_end, compiled_fns) {
                    Ok(()) => break 'body_redo,
                    Err(e) if e.is_redo && Self::label_matches(&e.label, &spec.label) => {
                        self.interpreter
                            .env_mut()
                            .insert("_".to_string(), item.clone());
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

        'c_loop: loop {
            self.run_range(code, cond_start, body_start, compiled_fns)?;
            let cond_val = self.stack.pop().unwrap();
            if !cond_val.truthy() {
                break;
            }
            'body_redo: loop {
                match self.run_range(code, body_start, step_begin, compiled_fns) {
                    Ok(()) => break 'body_redo,
                    Err(e) if e.is_redo && Self::label_matches(&e.label, &spec.label) => {
                        continue 'body_redo;
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

        let saved_topic = self.interpreter.env().get("_").cloned();
        let saved_when = self.interpreter.when_matched();
        self.interpreter.env_mut().insert("_".to_string(), topic);
        self.interpreter.set_when_matched(false);

        let mut inner_ip = body_start;
        while inner_ip < end {
            if let Err(e) = self.exec_one(code, &mut inner_ip, compiled_fns) {
                if e.is_succeed {
                    self.interpreter.set_when_matched(saved_when);
                    if let Some(v) = saved_topic.clone() {
                        self.interpreter.env_mut().insert("_".to_string(), v);
                    } else {
                        self.interpreter.env_mut().remove("_");
                    }
                    *ip = end;
                    return Ok(());
                }
                self.interpreter.set_when_matched(saved_when);
                if let Some(v) = saved_topic {
                    self.interpreter.env_mut().insert("_".to_string(), v);
                } else {
                    self.interpreter.env_mut().remove("_");
                }
                return Err(e);
            }
            if self.interpreter.when_matched() || self.interpreter.is_halted() {
                break;
            }
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
        let body_result = self.run_range(code, body_start, end, compiled_fns);
        match body_result {
            Ok(()) => {}
            Err(e) if e.is_succeed => {
                if let Some(v) = e.return_value {
                    last = v;
                }
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
        self.sync_locals_from_env(code);
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

        let topic = self
            .interpreter
            .env()
            .get("_")
            .cloned()
            .unwrap_or(Value::Nil);
        if self.interpreter.smart_match_values(&topic, &cond_val) {
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
                let last = self
                    .interpreter
                    .env()
                    .get("_")
                    .cloned()
                    .unwrap_or(Value::Nil);
                let mut sig = RuntimeError::succeed_signal();
                sig.return_value = Some(last);
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
        self.run_range(code, body_start, end, compiled_fns)?;
        self.interpreter.set_when_matched(true);
        *ip = end;
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
                    Ok(()) => break 'body_redo,
                    Err(e) if e.is_redo && Self::label_matches(&e.label, label) => {
                        continue 'body_redo;
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

    pub(super) fn exec_try_catch_op(
        &mut self,
        code: &CompiledCode,
        catch_start: u32,
        control_start: u32,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let saved_depth = self.stack.len();
        let body_start = *ip + 1;
        let catch_begin = catch_start as usize;
        let control_begin = control_start as usize;
        let end = body_end as usize;
        match self.run_range(code, body_start, catch_begin, compiled_fns) {
            Ok(()) => {
                *ip = end;
                Ok(())
            }
            Err(e) if e.return_value.is_some() => Err(e),
            Err(e)
                if (e.is_last || e.is_next || e.is_redo || e.is_proceed || e.is_succeed)
                    && control_begin < end =>
            {
                self.stack.truncate(saved_depth);
                let saved_when = self.interpreter.when_matched();
                self.interpreter.set_when_matched(false);
                self.run_range(code, control_begin, end, compiled_fns)?;
                self.interpreter.set_when_matched(saved_when);
                *ip = end;
                Ok(())
            }
            Err(e) => {
                if catch_begin >= control_begin {
                    return Err(e);
                }
                self.stack.truncate(saved_depth);
                let mut exc_attrs = std::collections::HashMap::new();
                exc_attrs.insert("message".to_string(), Value::Str(e.message.clone()));
                let err_val = Value::make_instance("Exception".to_string(), exc_attrs);
                let saved_topic = self.interpreter.env().get("_").cloned();
                self.interpreter
                    .env_mut()
                    .insert("!".to_string(), err_val.clone());
                self.interpreter.env_mut().insert("_".to_string(), err_val);
                let saved_when = self.interpreter.when_matched();
                self.interpreter.set_when_matched(false);
                self.run_range(code, catch_begin, control_begin, compiled_fns)?;
                self.interpreter.set_when_matched(saved_when);
                if let Some(v) = saved_topic {
                    self.interpreter.env_mut().insert("_".to_string(), v);
                } else {
                    self.interpreter.env_mut().remove("_");
                }
                *ip = end;
                Ok(())
            }
        }
    }
}
