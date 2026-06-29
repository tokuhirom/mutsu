use super::*;

impl Interpreter {
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
        let saved_when = self.when_matched();
        let saved_topic_source = self.topic_source_var.take();
        let saved_container_source = self.topic_container_source.take();
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
        // A whole-container topic (`given @a` / `with %h`): `$_` aliases the whole
        // container, so a `.=` metaop on the topic (TopicDotAssign) writes the
        // reassigned value straight through to the `@`/`%` source. Record it so the
        // `.=`-on-`$_` opcode can do that (an element loop never sets this).
        if element_source.is_none()
            && let Some(src) = &container_binding
            && (src.starts_with('@') || src.starts_with('%'))
        {
            self.topic_container_source = Some(src.clone());
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
        let mark_ro =
            topic_readonly && pointy_param.is_none() && !self.readonly_vars().contains("_");
        if mark_ro {
            self.mark_readonly("_");
        }

        let restore = |this: &mut Self, write_back: bool| {
            if mark_ro {
                this.unmark_readonly("_");
            }
            if write_back {
                if let Some(src) = &element_source {
                    this.write_back_element_source(code, src, &pointy_param);
                } else {
                    this.write_back_given_topic(code, &container_binding, &pointy_param);
                }
            }
            this.set_when_matched(saved_when);
            if let Some(v) = saved_topic.clone() {
                this.env_mut().insert("_".to_string(), v);
            } else {
                this.env_mut().remove("_");
            }
            // A pointy parameter (`-> @p`) is block-scoped in Raku, but mutsu
            // desugars it to a global `@p := $_` whose alias/bound markers would
            // otherwise leak past this block. Clear them so a later block reusing
            // the name (e.g. `given @c -> @p is copy { ... }`, a plain assign that
            // would otherwise follow the stale `__mutsu_sigilless_alias::@p` and
            // corrupt `$_`) starts clean. Done after the writeback above, which
            // still reads the parameter's final value.
            if let Some(p) = &pointy_param {
                this.env_mut()
                    .remove(&format!("__mutsu_sigilless_alias::{}", p));
                this.env_mut()
                    .remove(&format!("__mutsu_sigilless_readonly::{}", p));
                this.env_mut()
                    .remove(&format!("__mutsu_bound_decont::{}", p));
                this.unmark_readonly(p);
            }
            this.topic_source_var = saved_topic_source.clone();
            this.topic_container_source = saved_container_source.clone();
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
                if e.is_succeed() {
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
            if self.when_matched() || self.is_halted() {
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
        let saved_when = self.when_matched();
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
            Err(e) if e.is_succeed() => {
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
                    let (_params, param_defs) = self.callable_signature(&cond_val);
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
                Err(e) if e.is_proceed() => {
                    did_proceed = true;
                }
                Err(e) if e.is_succeed() => {
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
            Err(e) if e.is_succeed() => {
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
}
