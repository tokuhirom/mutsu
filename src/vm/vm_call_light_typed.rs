use super::*;

impl Interpreter {
    /// Lightweight compiled function call that avoids the heavyweight frame
    /// management (push_call_frame/env clone, Sub value creation, block/routine
    /// push, callable_id lookup). Binds parameters directly to locals slots
    /// without touching the env HashMap, maximizing performance for hot loops.
    pub(super) fn call_compiled_function_light(
        &mut self,
        cf: &CompiledFunction,
        args: &[Value],
        compiled_fns: &CompiledFns,
    ) -> Result<Value, RuntimeError> {
        // GC safepoint (§9.2a `call`): the light-call boundary skips
        // push_call_frame, so it emits the call safepoint itself.
        crate::gc::gc_safepoint(crate::gc::SafepointKind::Call);
        self.record_cf_deprecation(cf);
        // Eligibility (`is_light_call_eligible`) guarantees every param is
        // named, which is exactly when the plan is precomputed. A hand-built
        // chunk without one takes the full named dispatch instead.
        let Some(plan) = cf.named_call_plan.as_deref() else {
            let pkg = self.current_package().to_string();
            let name = String::new();
            return self.call_compiled_function_named(cf, args.to_vec(), compiled_fns, &pkg, &name);
        };
        // Save caller locals and create callee locals
        let saved_locals = std::mem::take(&mut self.locals);
        // Isolate the caller's loop-body-local declaration scope (mirrors
        // vm_call_fast.rs / the positional-light path). Without this a callee's
        // body-local `my $x` — e.g. a recursive call from inside the caller's
        // `while` loop — registers in the caller's active loop-local scope and is
        // clobbered at the caller's loop exit. Restored on every exit path.
        let saved_loop_local_vars = std::mem::take(&mut self.loop_local_vars);
        let saved_loop_local_saved_env = std::mem::take(&mut self.loop_local_saved_env);
        // Isolate the caller's block-scope `my`-declaration tracking (see the
        // matching comment in `call_compiled_function_positional_light`): a
        // callee's routine-level `my $x` must not register in the caller's active
        // `BlockScope` frame and be reverted at the caller's block exit.
        let saved_block_declared_vars = std::mem::take(&mut self.block_declared_vars);

        // Scoped-overlay (docs/vm-dual-store.md Slice 6): install an empty
        // born-owned overlay over the caller. Param / alias / @_ env writes below
        // land in a fresh map and are discarded by dropping the overlay on return
        // (callee-local names) or merged overlay-only (captured-outer writes).
        //
        // Frame reuse (ADR-0004 J4d, mirrored from the positional-light path):
        // when the caller's live env is itself a scoped child whose overlay is
        // still the process-shared empty singleton, keep it in place — the
        // clone + scoped_child + drop round-trip is pure overhead. The shared
        // singleton doubles as a write detector: the body's first by-name write
        // un-shares it, and the unwind arm below replays the return merge in
        // place.
        let caller_env = if self.env().overlay_is_shared_empty() {
            None
        } else {
            let parent = self.env().clone();
            Some(std::mem::replace(
                self.env_mut(),
                crate::env::Env::scoped_child(parent),
            ))
        };

        let num_locals = cf.code.locals.len();
        self.locals = self.take_locals_from_pool(num_locals);

        // Borrow-deref a possibly-VarRef-wrapped argument without cloning it.
        #[inline]
        fn deref_arg(arg: &Value) -> &Value {
            match arg.view() {
                ValueView::VarRef { value, .. } => value,
                _ => arg,
            }
        }
        // Find the value of the last argument pair whose key is `key`.
        #[inline]
        fn find_named<'a>(args: &'a [Value], key: &str) -> Option<&'a Value> {
            args.iter()
                .rev()
                .find_map(|arg| match deref_arg(arg).view() {
                    ValueView::Pair(k, v) if k.as_str() == key => Some(v),
                    _ => None,
                })
        }

        // Bind named parameters to their precomputed locals slots; mirror into
        // the overlay env only when a name-based reader needs it (reflective
        // access anywhere / closure capture via needs_env_sync) or when the
        // value is `Nil` (the GetLocal handler treats a `Nil` slot as
        // possibly-undeclared and verifies via env.contains_key).
        let write_all_params = crate::opcode::reflective_name_access_possible();
        for (i, npb) in plan.params.iter().enumerate() {
            let mut found_val: Option<&Value> = find_named(args, &npb.match_key);
            if found_val.is_none() {
                for key in &npb.alias_keys {
                    found_val = find_named(args, key);
                    if found_val.is_some() {
                        break;
                    }
                }
            }
            if found_val.is_none() {
                for key in &npb.outer_alias_keys {
                    found_val = find_named(args, key);
                    if found_val.is_some() {
                        break;
                    }
                }
            }
            let Some(v) = found_val else {
                if npb.required {
                    // Unwind (missing required named param => runtime X::AdHoc).
                    match caller_env {
                        Some(caller_env) => self.set_env(caller_env),
                        None => {
                            if !self.env().overlay_is_shared_empty() {
                                self.env_mut().retain_overlay(|_, _| false);
                            }
                        }
                    }
                    let used = std::mem::replace(&mut self.locals, saved_locals);
                    self.recycle_locals(used);
                    self.loop_local_vars = saved_loop_local_vars;
                    self.loop_local_saved_env = saved_loop_local_saved_env;
                    self.block_declared_vars = saved_block_declared_vars;
                    return Err(RuntimeError::typed_msg(
                        "X::AdHoc",
                        format!(
                            "Required named parameter '{}' not passed",
                            cf.param_defs[i].name
                        ),
                    ));
                }
                // Missing optional named param: bind `Nil` into the overlay
                // env so the param SHADOWS a same-named caller lexical (the
                // locals seed below reads through to the caller otherwise)
                // and so GetLocal's Nil-slot "declared?" probe finds it.
                match cf.param_name_syms.get(i) {
                    Some(sym) => {
                        self.env_mut().insert_sym(*sym, Value::NIL);
                    }
                    None => {
                        self.env_mut()
                            .insert(cf.param_defs[i].name.clone(), Value::NIL);
                    }
                }
                continue;
            };
            let v = v.clone();
            // A sub_signature rename (`:color(:$colour)`) binds every inner
            // name to the value as well.
            for (alias_name, alias_slot) in &npb.alias_binds {
                if let Some(slot) = alias_slot {
                    self.locals[*slot] = v.clone();
                }
                self.env_mut().insert(alias_name.clone(), v.clone());
            }
            if let Some(slot) = npb.slot {
                self.locals[slot] = v.clone();
            }
            if write_all_params || npb.needs_env || v.is_nil() {
                match cf.param_name_syms.get(i) {
                    Some(sym) => {
                        self.env_mut().insert_sym(*sym, v);
                    }
                    None => {
                        self.env_mut().insert(cf.param_defs[i].name.clone(), v);
                    }
                }
            }
        }

        // Mark parameters as readonly (eligibility excludes `is rw/copy/raw`
        // traits, so every param is immutable). Save the existing readonly
        // state to restore after the call.
        let saved_readonly = self.enter_readonly_frame();
        for (i, pd) in cf.param_defs.iter().enumerate() {
            if !pd.name.is_empty()
                && !pd.sigilless
                && !pd.name.starts_with('!')
                && !pd.name.starts_with('.')
            {
                // `param_name_syms` is `param_defs[i].name` pre-interned; fall
                // back to interning for a chunk that never precomputed it.
                match cf.param_name_syms.get(i) {
                    Some(sym) => self.mark_readonly_sym(*sym),
                    None => self.mark_readonly(&pd.name),
                }
            }
        }

        // Set @_ only if the function body uses it (has a @_ local).
        if plan.uses_arg_array {
            let plain_args: Vec<Value> = args
                .iter()
                .map(deref_arg)
                .filter(|a| !matches!(a.view(), ValueView::Pair(..)))
                .cloned()
                .collect();
            self.env_mut()
                .insert("@_".to_string(), Value::array(plain_args));
        }

        // Seed the remaining (non-param) locals by reading through to the
        // caller env, matching the prior env().get() semantics. `locals_sym`
        // is `locals` pre-interned at finalize(), so the seed hashes a `u32`
        // per local instead of the name string. A slot already bound above is
        // non-Nil unless the bound value itself was Nil — in which case the
        // param was also mirrored into the overlay env (the `is_nil` gate), so
        // the read-through finds that same value and stays coherent.
        if cf.code.locals_sym.len() == num_locals {
            for (i, sym) in cf.code.locals_sym.iter().enumerate() {
                if self.locals[i].is_nil()
                    && let Some(val) = self.env().get_sym(*sym)
                {
                    self.locals[i] = val.clone();
                }
            }
        } else {
            for (i, local_name) in cf.code.locals.iter().enumerate() {
                if self.locals[i].is_nil()
                    && let Some(val) = self.env().get(local_name)
                {
                    self.locals[i] = val.clone();
                }
            }
        }

        let saved_stack_depth = self.stack.len();
        let let_mark = self.let_saves_len();
        // Frame-less path: roll back the line the body's ops advanced to manually.
        let saved_line = self.cur_source_line;
        // Run the body under the routine's declaring package (set after the
        // required-param early returns above). Restored after the env merge below.
        let saved_package = self.enter_routine_package(cf);

        // Execute the function body
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        while ip < cf.code.ops.len() {
            // JIT entry (ADR-0004 J2): same hook as vm_call_light.rs — at body
            // start, run the whole body natively when the chunk is hot and
            // Tier A-compilable; the native outcome threads through the same
            // match arms below.
            let step = if ip == 0
                && let Some(r) = crate::vm::vm_jit::try_enter(self, &cf.code, compiled_fns)
            {
                ip = cf.code.ops.len();
                r
            } else {
                self.exec_one(&cf.code, &mut ip, compiled_fns)
            };
            match step {
                Ok(()) => {}
                Err(e) if e.return_value.is_some() => {
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.resolve_let_saves_on_success(let_mark, true);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail() => {
                    fail_bypass = true;
                    let failure = self.fail_error_to_failure_value(&e);
                    loan_env!(self, restore_let_saves(let_mark));
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
            }
            if self.is_halted() {
                break;
            }
        }

        // Natural fall-through completion (no explicit return / fail / error
        // break arm): restore any `temp` bindings the body introduced so a
        // `sub f { temp $x = ... }` with no explicit return does not leak the
        // temporized value into the caller's scope.
        if result.is_ok() && explicit_return.is_none() && !fail_bypass {
            self.resolve_let_saves_on_success(let_mark, true);
        }

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::NIL)
            } else {
                Value::NIL
            }
        } else {
            Value::NIL
        };

        self.stack.truncate(saved_stack_depth);

        self.cur_source_line = saved_line;
        // Restore locals
        let used = std::mem::replace(&mut self.locals, saved_locals);
        self.recycle_locals(used);
        self.loop_local_vars = saved_loop_local_vars;
        self.loop_local_saved_env = saved_loop_local_saved_env;
        self.block_declared_vars = saved_block_declared_vars;

        // Restore readonly vars
        self.exit_readonly_frame(saved_readonly);

        // Restore the caller env, merging the overlay (the callee's own writes)
        // back: a write to a captured outer variable (not a declared local /
        // param of this function) persists in the caller; the function's
        // params/locals/aliases are dropped with the overlay. This replaces the
        // per-key `modified_env_keys` save/restore.
        match caller_env {
            Some(caller_env) => {
                let scoped = std::mem::replace(self.env_mut(), caller_env);
                for (k, v) in scoped.overlay_iter() {
                    if *k == "_"
                        || *k == "@_"
                        || *k == "%_"
                        || *k == "__mutsu_callable_id"
                        || k.with_str(|s| s.starts_with('?'))
                    {
                        continue;
                    }
                    if !cf.is_callee_local_sym(*k) {
                        self.env_mut().insert_sym(*k, v.clone());
                    }
                }
            }
            None => {
                // Reused frame: the caller's overlay was the shared empty
                // singleton at entry. If the latch is still armed the body
                // never wrote env — nothing to merge or restore. Otherwise
                // every overlay entry is a write made by this call; replay the
                // swap path's return merge in place — keep captured-outer
                // writes (already sitting in the caller's env) and drop
                // callee-locals and the per-frame private names.
                if !self.env().overlay_is_shared_empty() {
                    self.env_mut().retain_overlay(|k, _| {
                        !(*k == "_"
                            || *k == "@_"
                            || *k == "%_"
                            || *k == "__mutsu_callable_id"
                            || k.with_str(|s| s.starts_with('?'))
                            || cf.is_callee_local_sym(*k))
                    });
                }
            }
        }
        self.leave_routine_package(saved_package);

        // Slice F (env<->locals coherence): record the captured-outer variables
        // this body writes so the call-site op writes them straight through to the
        // caller's local slots (see `call_compiled_function_positional_light`).
        for sym in &cf.code.free_var_writes {
            sym.with_str(|fname| {
                if fname != "_" && fname != "@_" && fname != "%_" {
                    self.pending_rw_writeback_sources.push(fname.to_string());
                }
            });
        }

        match result {
            Ok(()) if fail_bypass => Ok(ret_val),
            Ok(()) => {
                if let Some(v) = explicit_return {
                    Ok(v)
                } else {
                    Ok(ret_val)
                }
            }
            Err(e) => Err(e),
        }
    }

    /// A return value that may *be* (or carry) a routine declared inside the
    /// callee body — i.e. a `my sub` that escaped by being returned. When the
    /// body declares an inner routine and returns one of these, its registry
    /// entry must survive the call so it stays callable by name (e.g. `my &bar
    /// := producer()` then `bar(...)`). Any other return value means the inner
    /// routine did not escape via the return slot and can be cleaned up.
    pub(super) fn return_value_escapes_routine(v: &Value) -> bool {
        matches!(
            v.view(),
            ValueView::Sub(_)
                | ValueView::WeakSub(_)
                | ValueView::Routine { .. }
                | ValueView::Mixin(..)
        )
    }
}
