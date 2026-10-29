use super::*;
use crate::ast::{CallArg, Expr, Stmt};

impl Interpreter {
    fn try_eval_simple_protect_expr(
        &self,
        outer_code: &CompiledCode,
        expr: &Expr,
    ) -> Option<Value> {
        let local_value = |name: &str| {
            outer_code
                .locals
                .iter()
                .position(|candidate| candidate == name)
                .and_then(|slot| self.locals.get(slot).cloned())
        };
        match expr {
            Expr::Literal(value) => Some(value.clone()),
            // Decont via into_deref: a captured-mutated `$` scalar may be boxed
            // into a ContainerRef (see box_captured_lexicals), and this direct
            // locals/env read bypasses the GetLocal/GetGlobal chokepoint, so it
            // must deref here to avoid leaking a raw cell into the method
            // fast-path receiver/args.
            Expr::Var(name) => local_value(name)
                .or_else(|| self.get_env_with_main_alias(name))
                .map(Value::into_deref),
            Expr::ArrayVar(name) => {
                let sigiled = format!("@{name}");
                local_value(&sigiled).or_else(|| self.get_env_with_main_alias(&sigiled))
            }
            Expr::HashVar(name) => {
                let sigiled = format!("%{name}");
                local_value(&sigiled).or_else(|| self.get_env_with_main_alias(&sigiled))
            }
            _ => None,
        }
    }

    pub(super) fn try_exec_simple_shared_protect_block(
        &mut self,
        outer_code: &CompiledCode,
        code_val: &Value,
    ) -> Result<Option<Value>, RuntimeError> {
        let ValueView::Sub(data) = code_val.view() else {
            return Ok(None);
        };
        let [Stmt::Call { name, args, .. }] = data.body.as_slice() else {
            return Ok(None);
        };
        let method = name.as_str();
        if method != "push" {
            return Ok(None);
        }
        let Some(CallArg::Positional(Expr::ArrayVar(target_name))) = args.first() else {
            return Ok(None);
        };
        let sigiled_target = format!("@{target_name}");
        if !self.threads.shared_vars_active
            || !matches!(
                self.get_shared_var(&sigiled_target)
                    .as_ref()
                    .map(Value::view),
                Some(ValueView::Array(..))
            )
        {
            return Ok(None);
        }
        let mut values = Vec::with_capacity(args.len().saturating_sub(1));
        for arg in args.iter().skip(1) {
            let CallArg::Positional(expr) = arg else {
                return Ok(None);
            };
            let Some(value) = self.try_eval_simple_protect_expr(outer_code, expr) else {
                return Ok(None);
            };
            values.push(value);
        }
        if let Some(result) = loan_env!(
            self,
            push_to_existing_shared_array(&sigiled_target, values.clone())
        ) {
            return Ok(Some(result));
        }
        let Some(target_value) =
            self.try_eval_simple_protect_expr(outer_code, &Expr::ArrayVar(target_name.clone()))
        else {
            return Ok(None);
        };
        let result = loan_env!(
            self,
            push_to_shared_var(&sigiled_target, values, &target_value)
        );
        Ok(Some(result))
    }

    /// Execute a protect block inline in the current Interpreter, avoiding the overhead
    /// of creating a new Interpreter.
    pub(super) fn exec_protect_block_inline(
        &mut self,
        outer_code: &CompiledCode,
        code_val: &Value,
    ) -> Result<Value, RuntimeError> {
        let outer_local_slots: std::collections::HashMap<&str, usize> = outer_code
            .locals
            .iter()
            .enumerate()
            .map(|(idx, name)| (name.as_str(), idx))
            .collect();
        let (block_cc, block_fns, sub_data, captured_bindings, writeback_bindings) = match code_val
            .view()
        {
            // Running the block inline in THIS frame is only sound for a block
            // literal of this frame (`$lock.protect: { ... }`): its captured
            // names are this frame's locals. A block created elsewhere and
            // passed in (`method !protect(&code) { $!lock.protect: &code }`)
            // closes over ITS creator's lexicals, and seeding them from this
            // frame read the parameter `&code` — the block itself — for the
            // block's own free `&code` (FINALIZER's `register`). Call it as
            // the closure it is instead.
            ValueView::Sub(data) if !outer_code.owns_closure_body(&data.body) => {
                return self.call_sub_value(code_val.clone(), Vec::new());
            }
            ValueView::Sub(data) => {
                let (block_cc, block_fns, captured_bindings, writeback_bindings, captured_names) =
                    self.get_or_compile_protect_block_with_slots(&data);
                self.sync_shared_vars_for_names(captured_names.iter().map(|name| name.as_str()));
                // Keep an owned handle: a view guard's borrow cannot
                // outlive the match, so clone the Gc (refcount bump only).
                (
                    block_cc,
                    block_fns,
                    Some(data.clone()),
                    captured_bindings,
                    writeback_bindings,
                )
            }
            _ => {
                // TODO: Handle non-Sub protect blocks (e.g. WeakSub, Routine)
                // in the Interpreter. Currently these are rare and delegate to interpreter.
                return self.call_protect_block(code_val);
            }
        };
        let captured_env = sub_data.as_ref().map(|data| &data.env);

        // RAII (`MarkContextGuard`,
        // `todo/deep/mark-context-flags-leak-across-live-call-boundary.md`):
        // this inline exec is another call boundary that runs a callee's
        // compiled block body without going through `vm_run_loop.rs`'s
        // nested-run save/restore -- isolate the "mark context" one-shot
        // flag family so a caller's pending `:=` mark does not leak into the
        // protect block's own vardecl/store opcodes.
        let _mark_context_guard = crate::vm::vm_call_state_guard::MarkContextGuard::new(self);
        // Save/swap stack and locals for the block
        let saved_locals_base = self.locals.push_frame(0);
        let saved_stack = std::mem::take(&mut self.stack);
        // A detached copy of the enclosing frame's slots. This path reads the
        // enclosing lexicals through `outer_local_slots` *and* writes the
        // block's free-variable writes back into them, then restores the whole
        // array on exit — so the copy is the semantics, not an artifact of the
        // old per-frame `Vec` (ADR-0077 Slice 2 kept it deliberately; making it
        // copy-free means writing through to the live region below `base`,
        // which is a behavior change on the panic path and belongs in its own
        // slice).
        let mut saved_locals = self.locals.frame_slots(&saved_locals_base).to_vec();

        // Initialize locals for the block
        self.locals.refill_slots(block_cc.locals.len());
        if captured_env.is_some() {
            for (slot, name) in captured_bindings.iter() {
                if (name.starts_with('@') || name.starts_with('%'))
                    && self.get_shared_var(name).is_some()
                {
                    // Leave shared collections unmaterialized in locals.
                    // GetLocal will read the shared value on demand.
                    continue;
                }
                if let Some(outer_slot) = outer_local_slots.get(name.as_str())
                    && let Some(val) = saved_locals.get(*outer_slot)
                {
                    self.locals[*slot] = val.clone();
                    continue;
                }
                if let Some(val) = self.env().get(name) {
                    self.locals[*slot] = val.clone();
                }
            }
        }

        // Bind `self` for the block to the *enclosing* method's invocant. The
        // block runs inline in the current env, and a free `self` read
        // (GetSelfOrNoSelf / a bare `self!priv` invocant) consults env "self".
        // Without this, env "self" still holds the OUTER caller's self — e.g.
        // `Repo.update` calls `$fetcher.fetch`, whose `$lock.protect: {...}`
        // block does `self!try-load`, and self would wrongly resolve to the Repo
        // rather than the Fetch that lexically encloses the block. Prefer the
        // enclosing frame's `self` local, then the block's captured env.
        let enclosing_self = outer_local_slots
            .get("self")
            .and_then(|s| saved_locals.get(*s))
            .filter(|v| !matches!(v.view(), ValueView::Nil))
            .cloned()
            .or_else(|| {
                sub_data
                    .as_ref()
                    .and_then(|data| data.fixtures.as_ref())
                    .and_then(|f| f.invocant.clone())
            });
        let fixtures_mark = enclosing_self
            .as_ref()
            .map(|s| self.begin_scoped_invocant(s.clone()));

        // Execute the block's opcodes inline. Install the BLOCK's own captured
        // upvalue array for the duration: this inline path bypasses closure
        // dispatch, so without the swap a `GetUpvalue` in the block body
        // indexes the ENCLOSING closure's array — an index collision hands it
        // an unrelated capture (`$l.protect({ $r += $i })` read the enclosing
        // frame's upvalue 0, the Lock cell, as `$i`). Latent while upvalue
        // arrays held `Some` only for rare cells; exposed when ADR-0025 made
        // Instance-holding captures boxable.
        let saved_upvalues = std::mem::replace(
            &mut self.upvalues,
            sub_data
                .as_ref()
                .map(|d| d.upvalues.clone())
                .unwrap_or_default(),
        );
        let mut sub_ip = 0;
        let mut exec_err = None;
        let polls = crate::vm::vm_poll::DispatchPolls::current();
        while sub_ip < block_cc.ops.len() {
            if let Err(e) = self.exec_one_backedge_polled(&block_cc, &mut sub_ip, &block_fns, polls)
            {
                exec_err = Some(e);
                break;
            }
        }
        self.upvalues = saved_upvalues;

        // The block runs inline in the current env, so a free variable it
        // assigns (`$x = 99` where `$x` is a captured outer lexical) is written
        // via SetGlobal straight into env — never as a block-local, so the
        // writeback_bindings loop below (which only handles block-locals) misses
        // it. Reconcile the *outer frame's* local slot for each such free-var
        // write from env, so the caller sees the mutation (e.g. a value assigned
        // inside `$lock.protect: { ... }`).
        for sym in &block_cc.free_var_writes {
            sym.with_str(|name| {
                if let Some(outer_slot) = outer_local_slots.get(name)
                    && let Some(val) = self.env().get(name).cloned()
                    && let Some(target) = saved_locals.get_mut(*outer_slot)
                {
                    *target = val;
                }
            });
        }
        // Sync locals back to env
        if let Some(captured) = captured_env {
            for (slot, name) in writeback_bindings.iter() {
                if matches!(
                    self.get_shared_var(name).as_ref().map(Value::view),
                    Some(ValueView::Array(..) | ValueView::Hash(..))
                ) {
                    continue;
                }
                if let Some(outer_slot) = outer_local_slots.get(name.as_str())
                    && let Some(target) = saved_locals.get_mut(*outer_slot)
                {
                    *target = self.locals[*slot].clone();
                }
                if captured.contains_key(name)
                    && !matches!(
                        self.get_shared_var(name).as_ref().map(Value::view),
                        Some(ValueView::Array(..) | ValueView::Hash(..))
                    )
                {
                    {
                        let __v = self.locals[*slot].clone();
                        self.env_mut().insert(name.clone(), __v);
                    }
                }
            }
        }

        // Restore the caller's `self` binding (see the enclosing-self setup above).
        if let Some(mark) = fixtures_mark {
            self.end_scoped_fixtures(mark);
        }

        // Get return value before restoring state
        let ret_val = self.stack.pop().unwrap_or(Value::NIL);

        // Restore outer state: close the block's frame, then write the enclosing
        // frame's slots back from the copy the writeback above mutated.
        self.locals.pop_frame(saved_locals_base);
        self.locals.refill_from(&saved_locals);
        self.stack = saved_stack;

        match exec_err {
            Some(e) => Err(e),
            None => Ok(ret_val),
        }
    }

    /// Check if a method candidate has a wrap chain from `^lookup().candidates[N].wrap()`.
    /// If so, dispatch through the wrapper and return Some(result).
    ///
    /// ADR-0019 E9b-2: builds a SINGLE `MethodDispatchFrame` (below-outermost
    /// wrappers, the winner as an ordinary resolved `Candidate`, then the MRO
    /// tail) instead of a separate `WrapDispatchFrame` + by-name "original"
    /// re-entry — the P1 fix
    /// (`todo/tickets/wrap-chain-skipped-inside-foreign-wrap-dispatch.md`):
    /// no `is_inside_wrap_dispatch()` guard is needed anymore, since the
    /// "original" is never re-entered by name. Mirrors
    /// `class_dispatch.rs`'s `run_instance_method_celled` wrap branch.
    pub(crate) fn check_method_wrap_chain(
        &mut self,
        cn: &str,
        owner_sym: crate::symbol::Symbol,
        method: &str,
        method_def: &crate::runtime::MethodDef,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let outermost =
            self.enter_method_wrap_chain(cn, method, owner_sym, method_def, args, target.clone())?;
        let mut call_args = vec![target.clone()];
        call_args.extend(args.to_vec());
        self.shift_arg_sources_for_wrap_invocant();
        let result = self.call_method_wrapper(outermost, call_args);
        self.pop_method_dispatch();
        self.pop_method_samewith_context();
        Some(result)
    }
}
