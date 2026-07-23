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
        func_name: &str,
    ) -> Result<Value, RuntimeError> {
        self.call_compiled_function_light_spec(cf, args, compiled_fns, func_name, None)
    }

    /// [`Self::call_compiled_function_light`] with an optional out-of-band
    /// named-args spec (a `CallFuncNamed` site): the positions listed in the
    /// spec are named-arg VALUES (never boxed into Pairs), everything else is
    /// positional. In-band Pair args can coexist (e.g. a constant `:flag`);
    /// on a duplicate key the higher argument position wins, matching the
    /// all-in-band ordering.
    pub(super) fn call_compiled_function_light_spec(
        &mut self,
        cf: &CompiledFunction,
        args: &[Value],
        compiled_fns: &CompiledFns,
        func_name: &str,
        named_spec: Option<&crate::opcode::NamedArgsSpec>,
    ) -> Result<Value, RuntimeError> {
        // GC safepoint (§9.2a `call`): the light-call boundary skips
        // push_call_frame, so it emits the call safepoint itself.
        crate::gc::gc_safepoint(crate::gc::SafepointKind::Call);
        self.record_cf_deprecation(cf);
        // Eligibility (`is_light_call_eligible`) guarantees at least one named
        // param, which is exactly when the plan is precomputed. A hand-built
        // chunk without one takes the full named dispatch instead.
        let Some(plan) = cf.named_call_plan.as_deref() else {
            let pkg = self.current_package().to_string();
            return self.call_compiled_function_named(
                cf,
                args.to_vec(),
                compiled_fns,
                &pkg,
                func_name,
            );
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
        // Find the last in-band argument pair whose key is `key` (with its
        // argument position, so a spec hit at a later position can win).
        #[inline]
        fn find_named_pos<'a>(args: &'a [Value], key: &str) -> Option<(usize, &'a Value)> {
            args.iter()
                .enumerate()
                .rev()
                .find_map(|(i, arg)| match deref_arg(arg).view() {
                    ValueView::Pair(k, v) if k.as_str() == key => Some((i, v)),
                    _ => None,
                })
        }
        // Named lookup over BOTH sources — in-band Pair args and the
        // out-of-band spec — with the higher argument position winning on a
        // duplicate key (Raku's last-one-wins, preserved across the split).
        // The spec compare is by `Symbol` when the caller has one interned
        // (the plan's main match key); alias keys compare by string.
        let find_named = |key_sym: Option<Symbol>, key: &str| -> Option<&Value> {
            let inband = find_named_pos(args, key);
            let spec_hit = named_spec.and_then(|s| {
                s.entries
                    .iter()
                    .rev()
                    .find(|e| match key_sym {
                        Some(sym) => e.sym == sym,
                        None => e.key == key,
                    })
                    .map(|e| (e.pos as usize, &args[e.pos as usize]))
            });
            match (inband, spec_hit) {
                (Some((ip, iv)), Some((sp, sv))) => Some(if sp > ip { sv } else { iv }),
                (Some((_, v)), None) | (None, Some((_, v))) => Some(v),
                (None, None) => None,
            }
        };
        // Whether argument position `pos` carries an out-of-band named value.
        let in_spec = |pos: usize| {
            named_spec.is_some_and(|s| s.entries.iter().any(|e| e.pos as usize == pos))
        };

        // Bind parameters to their precomputed locals slots; mirror into
        // the overlay env only when a name-based reader needs it (reflective
        // access anywhere / closure capture via needs_env_sync) or when the
        // value is `Nil` (the GetLocal handler treats a `Nil` slot as
        // possibly-undeclared and verifies via env.contains_key).
        let write_all_params = crate::opcode::reflective_name_access_possible();
        let mut positional_idx = 0usize;
        let mut bind_err: Option<RuntimeError> = None;
        'bind: for (i, pb) in plan.params.iter().enumerate() {
            // Bind this param's value into slot + (gated) env under its name.
            macro_rules! bind_value {
                ($slot:expr, $needs_env:expr, $v:expr) => {{
                    let v: Value = $v;
                    if let Some(slot) = $slot {
                        self.locals[slot] = v.clone();
                    }
                    if write_all_params || $needs_env || v.is_nil() {
                        match cf.param_name_syms.get(i) {
                            Some(sym) => {
                                self.env_mut().insert_sym(*sym, v);
                            }
                            None => {
                                self.env_mut().insert(cf.param_defs[i].name.clone(), v);
                            }
                        }
                    }
                }};
            }
            let npb = match pb {
                crate::opcode::LightParamBind::Named(npb) => npb,
                crate::opcode::LightParamBind::Positional(ppb) => {
                    // Advance past named args — in-band Pairs and out-of-band
                    // spec positions — plus the synthetic callsite-line marker
                    // (a parenthesized Pair compiles to ValuePair and stays
                    // positional).
                    while positional_idx < args.len()
                        && (in_spec(positional_idx)
                            || matches!(
                                deref_arg(&args[positional_idx]).view(),
                                ValueView::Pair(..)
                            )
                            || Self::is_callsite_line_marker(&args[positional_idx]))
                    {
                        positional_idx += 1;
                    }
                    if positional_idx < args.len() {
                        let val = deref_arg(&args[positional_idx]).clone();
                        positional_idx += 1;
                        if let Some(ref tc) = cf.param_defs[i].type_constraint
                            && !Self::fast_type_check(&val, tc)
                        {
                            let got = runtime::value_type_name(&val);
                            let msg = format!(
                                "Type check failed in binding ${}: expected {}, got {}",
                                cf.param_defs[i].name, tc, got
                            );
                            let mut attrs = Self::type_check_argument_attrs(
                                func_name,
                                &cf.param_defs,
                                args,
                                msg,
                            );
                            attrs.insert("expected".to_string(), Value::str(tc.to_string()));
                            attrs.insert("got".to_string(), Value::str(got.to_string()));
                            bind_err = Some(RuntimeError::typed("X::TypeCheck::Argument", attrs));
                            break 'bind;
                        }
                        bind_value!(ppb.slot, ppb.needs_env, val);
                    } else if ppb.required {
                        let got = args
                            .iter()
                            .enumerate()
                            .filter(|(i, a)| {
                                !in_spec(*i)
                                    && !matches!(deref_arg(a).view(), ValueView::Pair(..))
                                    && !Self::is_callsite_line_marker(a)
                            })
                            .count();
                        let msg = format!(
                            "Too few positionals passed; expected {} arguments but got {}",
                            plan.positional_count, got
                        );
                        bind_err = Some(RuntimeError::typed(
                            "X::TypeCheck::Argument",
                            Self::type_check_argument_attrs(func_name, &cf.param_defs, args, msg),
                        ));
                        break 'bind;
                    } else {
                        // Missing optional positional: shadow like the named
                        // case below, seeding the param's type object (Any/Mu
                        // for untyped) like the slow path does.
                        bind_value!(
                            ppb.slot,
                            true,
                            Self::missing_optional_param_value(&cf.param_defs[i])
                        );
                    }
                    continue;
                }
            };
            let mut found_val: Option<&Value> = find_named(Some(npb.match_key_sym), &npb.match_key);
            if found_val.is_none() {
                for key in &npb.alias_keys {
                    found_val = find_named(None, key);
                    if found_val.is_some() {
                        break;
                    }
                }
            }
            if found_val.is_none() {
                for key in &npb.outer_alias_keys {
                    found_val = find_named(None, key);
                    if found_val.is_some() {
                        break;
                    }
                }
            }
            let Some(v) = found_val else {
                if npb.required {
                    // Missing required named param => runtime X::AdHoc.
                    bind_err = Some(RuntimeError::typed_msg(
                        "X::AdHoc",
                        format!(
                            "Required named parameter '{}' not passed",
                            cf.param_defs[i].name
                        ),
                    ));
                    break 'bind;
                }
                // Missing optional named param: bind the param's type object
                // (Any/Mu for untyped, like the slow path) into the slot and
                // the overlay env so the param SHADOWS a same-named caller
                // lexical (the locals seed below reads through to the caller
                // otherwise).
                let seed = Self::missing_optional_param_value(&cf.param_defs[i]);
                // A `:color(:$colour)` alias chain also declares its inner
                // variable(s); when the param is unsupplied they must still be
                // in scope (undefined), else the body's `$colour` read throws.
                for (alias_name, alias_slot) in &npb.alias_binds {
                    if let Some(slot) = alias_slot {
                        self.locals[*slot] = seed.clone();
                    }
                    self.env_mut().insert(alias_name.clone(), seed.clone());
                }
                bind_value!(npb.slot, true, seed);
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
            bind_value!(npb.slot, npb.needs_env, v);
        }
        // Surplus positional args are an arity error (mixed signatures only —
        // all-named signatures keep their historical lax behavior for a stray
        // positional; the full dispatch path is just as lax there).
        if bind_err.is_none() && plan.positional_count > 0 {
            let mut idx = positional_idx;
            while idx < args.len() {
                let is_named_or_marker = in_spec(idx)
                    || matches!(deref_arg(&args[idx]).view(), ValueView::Pair(..))
                    || Self::is_callsite_line_marker(&args[idx]);
                if !is_named_or_marker {
                    let got = args
                        .iter()
                        .enumerate()
                        .filter(|(i, a)| {
                            !in_spec(*i)
                                && !matches!(deref_arg(a).view(), ValueView::Pair(..))
                                && !Self::is_callsite_line_marker(a)
                        })
                        .count();
                    let msg = format!(
                        "Too many positionals passed; expected {} arguments but got {}",
                        plan.positional_count, got
                    );
                    bind_err = Some(RuntimeError::typed(
                        "X::TypeCheck::Argument",
                        Self::type_check_argument_attrs(func_name, &cf.param_defs, args, msg),
                    ));
                    break;
                }
                idx += 1;
            }
        }
        if let Some(e) = bind_err {
            // Unwind: restore the caller frame exactly as the success path does.
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
            return Err(e);
        }

        // Mark parameters as readonly (eligibility excludes `is rw/copy/raw`
        // traits, so every param is immutable). Save the existing readonly
        // state to restore after the call.
        let saved_readonly = self.enter_readonly_frame();
        // A routine gets a fresh, writable `$_` — clear any readonly mark leaked
        // from the caller's topic (see vm_call_light.rs for the full rationale);
        // the param loop below re-marks `_` for an explicit `$_` param.
        if cf.code.is_routine && !self.no_readonly_vars() {
            self.unmark_readonly("_");
        }
        // Raku: a routine gets its own `$_` = `(Any)`, not the caller's topic.
        // Shadow the caller's topic with Any before the body reads it (gated on
        // `reads_topic` so a topic-free routine pays nothing). See vm_call_light.
        if cf.code.reads_topic
            && cf.code.is_routine
            && !cf.param_defs.iter().any(|pd| pd.name == "_")
        {
            let any_val = Value::package(crate::symbol::Symbol::intern("Any"));
            if let Some(slot) = cf.code.locals.iter().position(|n| n == "_") {
                self.locals[slot] = any_val.clone();
            }
            self.env_mut().insert("_".to_string(), any_val);
        }
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
                .enumerate()
                .filter(|(i, a)| {
                    !in_spec(*i) && !matches!(deref_arg(a).view(), ValueView::Pair(..))
                })
                .map(|(_, a)| deref_arg(a).clone())
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

        // A routine body is its own topicalizer for a bare `when`/`default`: a
        // matching `when` sets the global `when_matched` flag, which must not
        // leak to an enclosing `given`/`with` body (see vm_call_light.rs for the
        // full rationale). Reset for the body; restore the caller's value below.
        let saved_when_matched = self.when_matched();
        if cf.code.is_routine {
            self.set_when_matched(false);
        }
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

        // Restore the caller's `when_matched` — a bare `when` inside this
        // routine body must not leak its match state to an enclosing given/with.
        if cf.code.is_routine {
            self.set_when_matched(saved_when_matched);
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
