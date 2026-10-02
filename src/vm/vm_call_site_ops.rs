//! Per-site bodies of the opcodes that both `exec_one_dispatch` and a Tier A
//! JIT shim (`vm_jit_helpers.rs`) execute.
//!
//! Each function here is the *whole* behavior of one opcode at one call site
//! (source-line sync, the `use fatal` argument check, the dispatch, the
//! resume-point recording, the `is rw` writeback), so the interpreter arm and
//! the JIT shim cannot drift: the arm only advances `ip`, and the shim only
//! adapts the result to its `extern "C"` status. Before this module the shims
//! were hand-kept copies of the arms and had already lost the `use fatal`
//! check and `Return`'s EVAL-context target stamp (#9452).
//!
//! Every function takes the opcode's own `ip` (the index of the opcode in
//! `code.ops`) and never advances it; the caller does. The per-opcode
//! `// Cost:` lines stay on the dispatch arms.

use super::*;

impl Interpreter {
    /// Record `ip + 1` as the resume point of a call that raised `e`, so a
    /// control signal (a resumable `warn`, a `.throw`) can be `.resume`d after
    /// the call site. An existing resume point is kept: when the call is
    /// itself a `.resume`/`.rethrow` re-raising a signal, the original point
    /// (e.g. after `warn`) must survive.
    #[inline]
    fn record_call_resume_point(&mut self, code: &CompiledCode, ip: usize, e: &RuntimeError) {
        if !e.is_resume() && self.resume_ip.is_none() {
            self.resume_ip = Some((Self::resume_code_fp(code), ip + 1));
        }
    }

    /// `OpCode::CallFunc` / `OpCode::CallFuncNamed` at `code.ops[ip]`.
    pub(super) fn exec_call_func_site(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        compiled_fns: &CompiledFns,
    ) -> Result<(), RuntimeError> {
        self.sync_source_line(code, ip);
        let r = match &code.ops[ip] {
            OpCode::CallFunc {
                name_idx,
                arity,
                arg_sources_idx,
                literal_native_args,
            } => {
                // `use fatal`: explode an unhandled Failure produced by one of
                // this call's argument expressions before the callee runs.
                // See `explode_if_fatal_failure_in_call_args`.
                self.explode_if_fatal_failure_in_call_args(
                    Self::const_str(code, *name_idx),
                    *arity as usize,
                )?;
                self.exec_call_func_op(
                    code,
                    *name_idx,
                    *arity,
                    *arg_sources_idx,
                    *literal_native_args,
                    compiled_fns,
                )
            }
            OpCode::CallFuncNamed {
                name_idx,
                arity,
                spec_idx,
                arg_sources_idx,
                literal_native_args,
            } => {
                self.explode_if_fatal_failure_in_call_args(
                    Self::const_str(code, *name_idx),
                    *arity as usize,
                )?;
                self.exec_call_func_named_op(
                    code,
                    *name_idx,
                    *arity,
                    *spec_idx,
                    *arg_sources_idx,
                    *literal_native_args,
                    compiled_fns,
                )
            }
            _ => unreachable!("exec_call_func_site on a non-CallFunc opcode"),
        };
        r.inspect_err(|e| self.record_call_resume_point(code, ip, e))
    }

    /// `OpCode::CallMethod` at `code.ops[ip]`.
    pub(super) fn exec_call_method_site(
        &mut self,
        code: &CompiledCode,
        ip: usize,
    ) -> Result<(), RuntimeError> {
        let OpCode::CallMethod {
            name_idx,
            arity,
            modifier_idx,
            quoted,
            arg_sources_idx,
        } = &code.ops[ip]
        else {
            unreachable!("exec_call_method_site on a non-CallMethod opcode")
        };
        self.sync_source_line(code, ip);
        // `use fatal`: the receiver sits below the `arity` argument values on
        // the stack, so this only ever scans the arguments, not the invocant.
        // A method can never be `require` (a bareword sub), so pass "".
        self.explode_if_fatal_failure_in_call_args("", *arity as usize)?;
        // ADR-0072: `$ex.throw` is a resumable throw site. Remember where the
        // receiver+arguments start so a handler that resumes inline -- with
        // every Rust frame still live, so `.resume` continues with the next
        // statement of the *calling* body -- can leave the call's single `Any`
        // value in their place.
        let throw_base = self
            .method_name_is_resumable_throw(code, *name_idx)
            .then(|| self.stack.len().saturating_sub(*arity as usize + 1));
        match self.exec_call_method_op(
            code,
            *name_idx,
            *arity,
            *modifier_idx,
            *quoted,
            *arg_sources_idx,
        ) {
            Ok(()) => {}
            Err(e) if throw_base.is_some() && !e.is_resume() => match self.try_catch_inline(e) {
                Ok(v) => {
                    self.stack.truncate(throw_base.unwrap_or(0));
                    self.stack.push(v);
                }
                Err(e) => {
                    if self.resume_ip.is_none() {
                        self.resume_ip = Some((Self::resume_code_fp(code), ip + 1));
                    }
                    return Err(e);
                }
            },
            Err(e) => {
                self.record_call_resume_point(code, ip, &e);
                return Err(e);
            }
        }
        // Slice F: write any `is rw` method-param writeback through to the
        // caller's local slot (no-op unless the dispatch recorded one).
        self.apply_pending_rw_writeback(code);
        // A `Grammar.parse` may run embedded regex `{ ... }` blocks that wrote
        // caller lexicals into `env`; reconcile them into slots.
        self.drain_pending_local_updates_after_call(code);
        Ok(())
    }

    /// `OpCode::CallMethodMut` / `OpCode::CallMethodDynamicMut` at
    /// `code.ops[ip]`.
    pub(super) fn exec_call_method_mut_site(
        &mut self,
        code: &CompiledCode,
        ip: usize,
    ) -> Result<(), RuntimeError> {
        match &code.ops[ip] {
            OpCode::CallMethodMut {
                name_idx,
                arity,
                target_name_idx,
                modifier_idx,
                quoted,
                arg_sources_idx,
            } => self.call_method_mut_site_around(code, ip, *arity, *target_name_idx, |vm| {
                vm.exec_call_method_mut_op(
                    code,
                    *name_idx,
                    *arity,
                    *target_name_idx,
                    *modifier_idx,
                    *quoted,
                    *arg_sources_idx,
                )
            }),
            // The run-time-named spelling (`$var."$name"(...)`) shares every
            // step around the dispatch -- above all the rebound-receiver
            // writeback, without which a delegated `push` on an `is Array`
            // instance never reached the caller's slot (#9454).
            OpCode::CallMethodDynamicMut {
                arity,
                target_name_idx,
                modifier_idx,
                quoted,
                arg_sources_idx,
            } => self.call_method_mut_site_around(code, ip, *arity, *target_name_idx, |vm| {
                vm.exec_call_method_dynamic_mut_op(
                    code,
                    *arity,
                    *target_name_idx,
                    *modifier_idx,
                    *quoted,
                    *arg_sources_idx,
                )
            }),
            _ => unreachable!("exec_call_method_mut_site on a non-CallMethodMut opcode"),
        }
    }

    /// Everything a mutating method call on a named receiver does around its
    /// dispatch `f`: see `exec_call_method_mut_site`.
    fn call_method_mut_site_around(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        arity: u32,
        target_name_idx: u32,
        f: impl FnOnce(&mut Self) -> Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        self.sync_source_line(code, ip);
        crate::alloc_scope_named!(_sc_cmm_pre, "op:CallMethodMut:pre");
        // `use fatal`: see `exec_call_method_site`.
        self.explode_if_fatal_failure_in_call_args("", arity as usize)?;
        let pre = self.attr_env_snapshot(code, target_name_idx);
        // The receiver's env binding before the call, so the writeback below
        // can tell whether this method actually rebound it (see there).
        // Compared with `same_binding` -- O(1), and it never walks container
        // contents the way `PartialEq` would.
        let receiver_before: Option<Option<Value>> = (!Self::const_str(code, target_name_idx)
            .is_empty())
        .then(|| self.env().get_sym(code.const_sym(target_name_idx)).cloned());
        crate::alloc_scope_end!(_sc_cmm_pre);
        crate::alloc_scope_named!(_sc_cmm_disp, "op:CallMethodMut:dispatch");
        f(self).inspect_err(|e| self.record_call_resume_point(code, ip, e))?;
        // Slice F (env<->locals coherence): a mutating method updates the
        // receiver in env by name (`$s.push` on an `is Array`-backed instance
        // reassigns `env[$s]`; the ~15 `env_mut().insert(target, ..)` branches
        // in exec_call_method_mut_op) and relied on the reverse
        // `sync_locals_from_env` pull to refresh the caller's local slot. Write
        // the receiver through to its slot here so it stays coherent without
        // the pull. (`apply_pending_rw_writeback` mirrors the reverse pull's
        // HashEntryRef-skip invariant.)
        //
        // ONLY when the call actually REBOUND `env[receiver]`. This used to
        // fire after every method call on a named receiver, and
        // `apply_pending_rw_writeback` copies `env[name]` into the local slot
        // by name -- but a frame's env also carries every same-named binding it
        // inherited from its caller (the callee env is the flattened caller
        // plus its own writes; parameters live in slots, not in env). So on an
        // unchanged receiver it copied the CALLER's variable over the callee's
        // parameter. A self-recursive routine is exactly that shape:
        //
        //     sub f($tree, $d) { ... ; f($tree[1], $d + 1) }
        //
        // Every frame has a `tree`, so *any* method call on `$tree` in the
        // callee (`.defined`, `.gist`, even inside a `say`) silently reverted
        // `$tree` to the caller's node, the descent never reached a leaf, and
        // the recursion ran until the Rust stack gave out (roast
        // integration/99problems-51-to-60.t P57 -- a stack overflow that was
        // really an infinite recursion). Under the per-store env-write the same
        // unconditional pull also froze a hot `$io .= succ` loop on a stale
        // decl-seed `env[io]` once the chunk went native.
        //
        // A method that mutates the receiver in place through its `Gc` (rather
        // than rebinding the name) leaves the bits equal, and that is correct:
        // the slot already holds the very same `Gc`.
        crate::alloc_scope_end!(_sc_cmm_disp);
        crate::alloc_scope_named!(_sc_cmm_post, "op:CallMethodMut:post");
        if let Some(before) = receiver_before {
            let after = self.env().get_sym(code.const_sym(target_name_idx));
            let rebound = match (&before, after) {
                (Some(b), Some(a)) => !b.same_binding(a),
                (None, None) => false,
                _ => true,
            };
            if rebound {
                self.pending_rw_writeback_sources
                    .push(Self::const_str(code, target_name_idx).to_string());
            }
        }
        self.apply_pending_rw_writeback(code);
        self.drain_pending_local_updates_after_call(code);
        self.mirror_attr_env_to_cell(code, target_name_idx, pre);
        crate::alloc_scope_end!(_sc_cmm_post);
        Ok(())
    }

    /// `OpCode::Return`. `Ok(())` means a lexically rebound `&return` ran and
    /// pushed its result, so execution continues at the next opcode; otherwise
    /// the return signal (or the rebound call's error) is returned.
    pub(super) fn exec_return_site(&mut self, code: &CompiledCode) -> Result<(), RuntimeError> {
        let val = self.stack.pop().unwrap_or(Value::NIL);
        // Check if &return has been lexically rebound; if so, call the rebound
        // function instead of performing a built-in return. Pre-interned
        // (`wk::rebound_return`) and gated on the process-global latch
        // (`env::return_rebound_possible`): this runs on every return, and with
        // no rebinding anywhere the lookup is a miss that walks every overlay
        // tier plus the global base (~2% of `bench-fib`).
        if crate::env::return_rebound_possible()
            && let Some(rebound) = self
                .env()
                .get_sym(crate::symbol::wk::rebound_return())
                .cloned()
            && matches!(
                rebound.view(),
                ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
            )
        {
            let result = self.vm_call_on_value(rebound, vec![val], None)?;
            self.stack.push(result);
            return Ok(());
        }
        let mut err = RuntimeError::return_signal(val);
        // ADR-0037 Slice 4: an EVAL unit whose `context => $ctx` named a live
        // routine bakes that routine's id onto its own `CompiledCode`
        // (`compile_block_value_opts`); stamp it here so the signal unwinds
        // past any intervening routine boundary to the frame this id names,
        // instead of being caught by the first one it reaches.
        if let Some(target_id) = code.eval_context_target_callable_id {
            err.set_return_target_callable_id(Some(target_id));
        }
        Err(err)
    }

    /// `OpCode::JumpIfFalse` condition: pops the tested value and reports
    /// whether the jump is taken (falsy). Failures tested for truthiness (the
    /// `&&` operator) are marked handled -- the original below the dup too,
    /// on the taken path.
    #[inline]
    pub(super) fn jump_if_false_taken(&mut self) -> bool {
        Self::mark_failure_handled_on_stack(&mut self.stack);
        let val = self.stack.pop().unwrap();
        if !self.eval_truthy(&val) {
            Self::mark_failure_handled_on_stack(&mut self.stack);
            true
        } else {
            false
        }
    }

    /// `OpCode::JumpIfTrue` condition: PEEKS the tested value (it stays on the
    /// stack on both paths -- `||`-style short-circuit); true when truthy.
    #[inline]
    pub(super) fn jump_if_true_taken(&mut self) -> bool {
        Self::mark_failure_handled_on_stack(&mut self.stack);
        let val = self.stack.last().unwrap().clone();
        self.eval_truthy(&val)
    }

    /// `OpCode::JumpIfNotNil` condition: PEEKS the tested value (kept on the
    /// stack on both paths -- `//`-style short-circuit); true when defined.
    #[inline]
    pub(super) fn jump_if_not_nil_taken(&mut self) -> bool {
        Self::mark_failure_handled_on_stack(&mut self.stack);
        let val = self.stack.last().unwrap().clone();
        self.value_is_defined_dispatch(&val)
    }

    /// `OpCode::StateVarInitGuard` condition, keyed on the opcode's `key_idx`:
    /// when the state var is already initialized, pushes the `NIL` placeholder
    /// `StateVarInit` discards and returns true (skip the RHS initializer);
    /// otherwise returns false (fall through and run it).
    #[inline]
    pub(super) fn state_var_init_guard_taken(&mut self, key_idx: u32) -> bool {
        let base_key = crate::symbol::Symbol::from_id(key_idx);
        let scoped_key = self.scoped_state_key(base_key);
        if self.get_state_var(scoped_key).is_some() {
            self.stack.push(Value::NIL);
            true
        } else {
            false
        }
    }
}
