use super::*;

pub(super) struct ForLoopSpec {
    pub(super) param_idx: Option<u32>,
    pub(super) param_local: Option<u32>,
    pub(super) body_end: u32,
    pub(super) label: Option<String>,
    pub(super) arity: u32,
    pub(super) collect: bool,
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

        let arity = spec.arity.max(1) as usize;
        let chunked_items: Vec<Value> = if arity > 1 {
            items
                .chunks(arity)
                .map(|chunk| Value::array(chunk.to_vec()))
                .collect()
        } else {
            items
        };
        let stack_base = if spec.collect {
            Some(self.stack.len())
        } else {
            None
        };
        let mut collected = if spec.collect { Some(Vec::new()) } else { None };
        'for_loop: for item in chunked_items {
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
            'body_redo: loop {
                match self.run_range(code, body_start, loop_end, compiled_fns) {
                    Ok(()) => {
                        if let Some(ref mut coll) = collected {
                            let base = stack_base.unwrap();
                            if self.stack.len() > base {
                                let val = self.stack.pop().unwrap();
                                coll.push(val);
                            }
                            // Drain any extra values pushed during this iteration
                            self.stack.truncate(base);
                        }
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
                    if let Some(v) = e.return_value {
                        self.stack.push(v);
                    }
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
            self.interpreter.smart_match_values(&topic, &cond_val)
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
            Err(e) if e.return_value.is_some() => {
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
                if e.is_warn {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::Str(e.message.clone()));
                    let warn_obj = Value::make_instance("CX::Warn".to_string(), attrs);
                    self.interpreter.env_mut().insert("_".to_string(), warn_obj);
                }
                let saved_when = self.interpreter.when_matched();
                self.interpreter.set_when_matched(false);
                match self.run_range(code, control_begin, end, compiled_fns) {
                    Ok(()) => {}
                    Err(e) if e.is_succeed => {}
                    Err(e) => return Err(e),
                }
                self.interpreter.set_when_matched(saved_when);
                if e.is_warn {
                    if let Some(v) = saved_topic {
                        self.interpreter.env_mut().insert("_".to_string(), v);
                    } else {
                        self.interpreter.env_mut().remove("_");
                    }
                }
                *ip = end;
                Ok(())
            }
            Err(e) => {
                // Exception — restore let saves
                self.interpreter.restore_let_saves(let_mark);
                self.sync_locals_from_env(code);
                if catch_begin >= control_begin {
                    return Err(e);
                }
                self.stack.truncate(saved_depth);
                let err_val = if let Some(ex) = e.exception.as_ref() {
                    *ex.clone()
                } else {
                    let mut exc_attrs = std::collections::HashMap::new();
                    exc_attrs.insert("message".to_string(), Value::Str(e.message.clone()));
                    Value::make_instance("Exception".to_string(), exc_attrs)
                };
                let saved_topic = self.interpreter.env().get("_").cloned();
                self.interpreter
                    .env_mut()
                    .insert("!".to_string(), err_val.clone());
                self.interpreter.env_mut().insert("_".to_string(), err_val);
                let saved_when = self.interpreter.when_matched();
                self.interpreter.set_when_matched(false);
                match self.run_range(code, catch_begin, control_begin, compiled_fns) {
                    Ok(()) => {}
                    // succeed from `when` inside CATCH means exception was handled
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
        }
    }
}
