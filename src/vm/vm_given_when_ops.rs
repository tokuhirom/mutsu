use super::*;

impl Interpreter {
    pub(super) fn exec_given_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        topic_readonly: bool,
        pointy_param_idx: Option<u32>,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let topic = self.stack.pop().unwrap();
        // For a pointy block (`given @a -> @p { ... }`), the writeback reads the
        // bound parameter's final value rather than `$_` (Raku binds `@p` to the
        // topic but leaves `$_` undefined). `is copy` is not recorded here, so it
        // copies and does not write back.
        let pointy_param: Option<String> = pointy_param_idx.map(|idx| {
            let constant = &code.constants[idx as usize];
            match constant.view() {
                ValueView::Str(s) => s.to_string(),
                _ => constant.to_string_value(),
            }
        });
        let body_start = *ip + 1;
        let end = body_end as usize;
        let stack_base = self.stack.len();

        // Arm capture of the pointy param's final value: its own `VarDecl`
        // makes `exec_block_local_scope_op` treat it as an ordinary vanishing
        // `my`, so that pass captures the slot's live value right before
        // Nil-resetting it, for this op's writeback below to read. Found by
        // peeking the compiled body for the first `SetLocalDecl` — always
        // this param's own synthetic declaration, since `pointy_topic_bind`
        // inserts it as the body's very first statement, ahead of any nested
        // construct's own declarations. See `given_pointy_capture_slots`'s doc
        // comment for why slot identity (not name) is what disambiguates.
        let pointy_capture_slot = pointy_param.as_ref().and_then(|_| {
            code.ops[body_start..end].iter().find_map(|op| match op {
                OpCode::SetLocalDecl { slot, .. } => Some(*slot as usize),
                _ => None,
            })
        });
        if let Some(slot) = pointy_capture_slot {
            self.given_pointy_capture_slots.push(slot);
            self.given_pointy_captured.push(None);
        }

        let saved_topic = self.env().get("_").cloned();
        let saved_when = self.when_matched();
        let saved_topic_source = self.topic_source_var.take();
        let saved_container_source = self.topic_container_source.take();
        let saved_element_source = self.element_source.take();
        let container_binding_full = self.take_container_ref_for(code);
        let container_source_slot = container_binding_full.as_ref().and_then(|(_, s)| *s);
        let container_binding = container_binding_full.map(|(n, _)| n);
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
        // The value the topic was bound to on entry. For an element-source topic
        // (`given $x<k>` / `given @a[i]`), the writeback below re-stores `$_` into
        // that element; if the body never changed `$_`, that writeback is a no-op
        // that must be skipped — re-storing into a *read-only* aggregate (a grammar
        // Match subcapture, `given $cc<scheme>`) would otherwise autovivify and
        // clobber the whole `$cc`. Keep the entry value to detect "unchanged".
        let element_orig = if element_source.is_some() {
            Some(topic.clone())
        } else {
            None
        };
        // When `$_` already occupies a local slot (a `sub f ($_) {...}` parameter
        // or a `my $_`), the body reads and writes that slot — it is the
        // authoritative half under the (B) per-store env-write gate, and the env
        // mirror alone is not seen. Mirror the topic into the slot on entry (and
        // restore it on exit) so `with $x { $_ }` inside such a sub reads the
        // topic, not the stale outer `$_`. `code.locals` positions never move
        // within a frame, so the slot index stays valid across the body.
        let topic_local_slot = self.find_local_slot(code, "_");
        let saved_local_topic = topic_local_slot.map(|s| self.locals[s].clone());
        if let Some(slot) = topic_local_slot {
            self.locals[slot] = topic.clone();
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
        // `@p` through its synthetic bound declaration and block element
        // assignment.
        let mark_ro = topic_readonly && pointy_param.is_none() && !self.is_readonly("_");
        if mark_ro {
            self.mark_readonly("_");
        }

        // Depth of the pointy-topic scope stack at body entry. A `when`-succeed (or
        // other control break) inside a `given`-body `if EXPR -> $_ { ... }` unwinds
        // past that if's `ExitPointyTopic`, leaving its `(saved $_, saved source)`
        // on the stack — so `$_` would still hold the pointy value and the given's
        // element writeback below would flush THAT (not the real topic) back to the
        // source. Drain any such leftover scopes here, restoring `$_` and
        // `topic_source_var` to the given's own topic before the writeback reads it.
        let saved_pointy_depth = self.topic_source_save_stack.len();
        let restore = move |this: &mut Self, write_back: bool| {
            // Body execution (including any nested `BlockLocalScope`) has
            // finished, so both stacks are safe to pop — pushed above in
            // strict LIFO order with body execution, so this always matches
            // that push. `captured` is the pointy param's final value,
            // filled in by `exec_block_local_scope_op` right before it
            // Nil-reset the param's own slot.
            let captured = if pointy_capture_slot.is_some() {
                this.given_pointy_capture_slots.pop();
                this.given_pointy_captured.pop().flatten()
            } else {
                None
            };
            while this.topic_source_save_stack.len() > saved_pointy_depth {
                let (saved_topic, saved_source) = this.topic_source_save_stack.pop().unwrap();
                this.env_mut().insert("_".to_string(), saved_topic);
                this.topic_source_var = saved_source;
            }
            if mark_ro {
                this.unmark_readonly("_");
            }
            if write_back {
                if let Some(src) = &element_source {
                    this.write_back_element_source(
                        code,
                        src,
                        &pointy_param,
                        element_orig.as_ref(),
                        captured.clone(),
                    );
                } else {
                    this.write_back_given_topic(
                        code,
                        &container_binding,
                        container_source_slot,
                        &pointy_param,
                        captured.clone(),
                    );
                }
            }
            this.set_when_matched(saved_when);
            if let Some(v) = saved_topic.clone() {
                this.env_mut().insert("_".to_string(), v);
            } else {
                this.env_mut().remove("_");
            }
            // Restore the outer `$_` local slot (the sub `$_` param / `my $_`
            // shadowed by this given/with) to its entry value.
            if let Some(slot) = topic_local_slot {
                this.locals[slot] = saved_local_topic.clone().unwrap_or(Value::NIL);
            }
            // A pointy parameter (`-> @p`) is block-scoped in Raku, but its
            // runtime alias/bound markers would otherwise leak past this block.
            // Clear them so a later block reusing
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
                // A pointy param (`-> @p`) is block-scoped: its aliased container
                // value must NOT linger in `@p`'s env/local slot past the block.
                // If it does, a later block reusing the name — especially an
                // `is copy` copy-assign (`given @c -> @p is copy {...}`) — would
                // find the previous block's source container still sitting in the
                // slot and, under whole-container in-place reassignment (§3),
                // clobber that unrelated source. Clearing the value (the alias
                // markers alone are not enough) keeps the next reuse clean. Done
                // after the writeback above, which already read `@p`'s final
                // value. A scalar pointy param needs no such reset here: its
                // exact-slot Nil-reset already happened inside
                // `exec_block_local_scope_op` (see `given_pointy_capture_slots`'s doc
                // comment) — repeating it here by name would risk hitting a
                // same-named outer variable the pointy param shadows instead.
                if p.starts_with('@') || p.starts_with('%') {
                    this.env_mut().remove(p.as_str());
                    this.update_local_if_exists(code, p, &Value::NIL);
                }
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
                    // A statement `given` always yields exactly one stack value
                    // (mirroring `exec_do_given_expr_op`), so statement-position
                    // compilers can pair it with an unconditional `Pop`.
                    self.stack.push(e.return_value.unwrap_or(Value::NIL));
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
        // Always net exactly +1 stack value (Nil when the body left none), so
        // the compiler's statement-position `Pop` never eats an unrelated value.
        let last = if self.stack.len() > stack_base {
            self.stack.pop().unwrap_or(Value::NIL)
        } else {
            Value::NIL
        };
        self.stack.truncate(stack_base);
        self.stack.push(last);

        restore(self, true);
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_do_given_expr_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let topic = self.stack.pop().unwrap_or(Value::NIL);
        let body_start = *ip + 1;
        let end = body_end as usize;

        let saved_topic = self.env().get("_").cloned();
        // A prior lexical `$_` declaration (for example, from
        // `given ... -> $_ is copy`) gives topic reads a local slot. Keep that
        // slot synchronized with the expression-form topic just as `Given`
        // does, otherwise a following `S/// with EXPR` reads the stale lexical
        // value instead of EXPR.
        let topic_local_slot = self.find_local_slot(code, "_");
        let saved_local_topic = topic_local_slot.map(|s| self.locals[s].clone());
        let saved_when = self.when_matched();
        // An element-source topic (`do given @a[i]` — what `.=Int with @a[i]`
        // desugars to, see modifier.rs) aliases an lvalue element: `$_`'s final
        // value is written back to that element below, matching the
        // statement-position `Given`'s handling (`exec_given_op`). This is a
        // one-shot signal set by `TagElementSource` immediately before this op,
        // so it is cleared (not restored) once consumed.
        let element_source = self.element_source.take();
        let element_orig = element_source.is_some().then(|| topic.clone());
        // Consume the topic's `TagContainerRef` signal (emitted right before this
        // op by `do given @c`). It must NOT survive into the body: a nested
        // `for @c[$slice] { }` — whose slice iterable emits no source tag of its
        // own — would otherwise pick up this stale `container_ref_var` and write
        // its loop values back into `@c` at the wrong (0-based) indices, shifting
        // the array (`do given @l { for @l[1..2] -> $e {} }` corrupted `@l`).
        // Whole-container topic writeback is not done by the expression form
        // (in-place `$_[0]=…`/`.push` propagate through the shared container), so
        // dropping the signal is sufficient; it only scopes topic-source tags to
        // the body and is restored afterwards.
        let saved_container_ref = self.take_container_ref_for(code);
        let saved_topic_source = self.topic_source_var.take();
        let saved_container_source = self.topic_container_source.take();
        if let Some((src, _)) = &saved_container_ref
            && element_source.is_none()
        {
            self.topic_source_var = Some(src.clone());
            if src.starts_with('@') || src.starts_with('%') {
                self.topic_container_source = Some(src.clone());
            }
        }
        if let Some(slot) = topic_local_slot {
            self.locals[slot] = topic.clone();
        }
        self.env_mut().insert("_".to_string(), topic);
        loan_env!(self, set_when_matched(false));

        let mut last = Value::NIL;
        let stack_base = self.stack.len();
        let body_result = self.run_range(code, body_start, end, compiled_fns);
        match body_result {
            Ok(()) => {
                if self.stack.len() > stack_base {
                    last = self.stack.pop().unwrap_or(Value::NIL);
                }
                self.stack.truncate(stack_base);
            }
            Err(mut e) if e.is_succeed() => {
                // Take the container name before moving `return_value` out (a
                // method borrow of `e` cannot coexist with a partial move).
                // The signal carries only the name — no compile-time slot.
                self.container_ref_var = e
                    .take_container_name()
                    .map(|n| (n, None, Self::resume_code_fp(code)));
                if let Some(v) = e.return_value {
                    last = v;
                }
                loan_env!(self, set_when_matched(true));
            }
            Err(e) => {
                loan_env!(self, set_when_matched(saved_when));
                if let Some(v) = saved_topic {
                    self.env_mut().insert("_".to_string(), v);
                } else {
                    self.env_mut().remove("_");
                }
                if let Some(slot) = topic_local_slot {
                    self.locals[slot] = saved_local_topic.clone().unwrap_or(Value::NIL);
                }
                self.topic_source_var = saved_topic_source;
                self.topic_container_source = saved_container_source;
                self.element_source = None;
                return Err(e);
            }
        }

        if let Some(src) = &element_source {
            self.write_back_element_source(code, src, &None, element_orig.as_ref(), None);
        }
        loan_env!(self, set_when_matched(saved_when));
        if let Some(v) = saved_topic {
            self.env_mut().insert("_".to_string(), v);
        } else {
            self.env_mut().remove("_");
        }
        if let Some(slot) = topic_local_slot {
            self.locals[slot] = saved_local_topic.unwrap_or(Value::NIL);
        }
        self.topic_source_var = saved_topic_source;
        self.topic_container_source = saved_container_source;
        self.element_source = None;
        self.stack.push(last);
        *ip = end;
        Ok(())
    }

    pub(super) fn exec_when_op(
        &mut self,
        code: &CompiledCode,
        body_end: u32,
        ip: &mut usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let cond_val = self.stack.pop().unwrap();
        let body_start = *ip + 1;
        let end = body_end as usize;

        // Num(Inf) represents Whatever (*) which always matches in `when *`
        let matches = if matches!(cond_val.view(), ValueView::Num(v) if v.is_infinite() && v.is_sign_positive())
        {
            true
        } else {
            let topic = self.env().get("_").cloned().unwrap_or(Value::NIL);
            match cond_val.view() {
                ValueView::Sub(_) | ValueView::Routine { .. } => {
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
                let last = self.stack.last().cloned().unwrap_or(Value::NIL);
                let mut sig = RuntimeError::succeed_signal();
                sig.return_value = Some(last);
                sig.set_container_name(self.take_container_ref_for(code).map(|(n, _)| n));
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
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        let body_start = *ip + 1;
        let end = body_end as usize;
        match self.run_range(code, body_start, end, compiled_fns) {
            Ok(()) => {}
            // `proceed` inside a `default` falls through WITHOUT the default
            // matching: suppress the succeed signal and continue past the block
            // (a `default` has no further candidate), so the enclosing `given`
            // ends normally and execution resumes after it. Mirrors the
            // `is_proceed` handling in `exec_when_op`.
            Err(e) if e.is_proceed() => {
                *ip = end;
                return Ok(());
            }
            Err(e) if e.is_succeed() => {
                loan_env!(self, set_when_matched(true));
                return Err(e);
            }
            Err(e) => return Err(e),
        }
        loan_env!(self, set_when_matched(true));
        let last = self.stack.last().cloned().unwrap_or(Value::NIL);
        let mut sig = RuntimeError::succeed_signal();
        sig.return_value = Some(last);
        sig.set_container_name(self.take_container_ref_for(code).map(|(n, _)| n));
        *ip = end;
        Err(sig)
    }
}
