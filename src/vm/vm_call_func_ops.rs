use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Names of builtin listops/functions that a same-named user-defined
    /// subroutine may shadow. When both exist, the user sub wins.
    ///
    /// This is intentionally narrow: most user subs don't conflict with a
    /// builtin, and unconditionally routing every `has_function` name through
    /// `call_function_fallback` changes dispatch in ways that affect things
    /// like MAIN/GENERATE-USAGE handling.
    /// True if the `&name` value in env comes from a lexical override
    /// (e.g. `sub callit(&foo) { ... }`) rather than the normal package
    /// binding for the named sub. A lexical override has a different
    /// identity (its stored `SubData.name` does not match `name`) — either
    /// because it's an anonymous block passed as `&foo`, or because it's a
    /// different sub with the same parameter name.
    pub(super) fn env_callable_is_lexical_override(val: &Value, name: &str) -> bool {
        if let Value::Sub(sub) = val {
            let stored = sub.name.resolve();
            // Anonymous block or mismatched name => lexical override.
            stored.is_empty() || stored != name
        } else {
            false
        }
    }

    /// Control-flow / dispatch-control names that must never be taken over by
    /// the lexical `&`-var Interpreter dispatch: the interpreter's call_function match
    /// implements their non-local semantics (loop control, gather/take,
    /// multi-dispatch redirection), and a lexical `&return`-style binding
    /// dispatched as a plain closure would lose them (or recurse infinitely).
    fn is_control_flow_function_name(name: &str) -> bool {
        matches!(
            name,
            "return"
                | "return-rw"
                | "take"
                | "take-rw"
                | "emit"
                | "done"
                | "last"
                | "next"
                | "redo"
                | "proceed"
                | "succeed"
                | "leave"
                | "die"
                | "fail"
                | "warn"
                | "exit"
                | "callsame"
                | "nextsame"
                | "callwith"
                | "nextwith"
                | "samewith"
                | "nextcallee"
                | "lastcall"
                | "make"
                | "start"
        )
    }

    /// Resolve a *pure* lexical `&name` callable for Interpreter-native dispatch
    /// (Track A): a `&code` parameter binding (local slot) or a `my &f = ...`
    /// env binding, for a name with NO same-named package sub / proto / multi
    /// (the shadow case is handled separately via `lexical_override` in
    /// `exec_call_func_op`). Restricted to plain `Sub`/`WeakSub` values —
    /// `Routine` (builtin references like `&r = &return`), `Mixin` (CALL-ME)
    /// and anything else keep the interpreter terminal, whose dispatch handles
    /// their special semantics. Returns `None` for builtin / interpreter-handled
    /// / carrier / control-flow names so precedence is unchanged.
    pub(super) fn lexical_amp_var_callable(
        &mut self,
        code: Option<&CompiledCode>,
        name: &str,
    ) -> Option<Value> {
        if Self::is_control_flow_function_name(name)
            || crate::runtime::Interpreter::is_builtin_function(name)
            || Self::is_interpreter_carrier_function(name)
            || self.is_interpreter_handled_function(name)
            || self.has_function(name)
            || self.has_proto(name)
            || self.has_multi_candidates_cached(name)
        {
            return None;
        }
        let ampname = format!("&{}", name);
        let candidate = code
            .and_then(|c| self.locals_get_by_name(c, &ampname))
            .or_else(|| self.env().get(&ampname).cloned());
        candidate.filter(|v| matches!(v, Value::Sub(_) | Value::WeakSub(_)))
    }

    /// Resolve a lexical `&infix:<op>` override (a `&infix:<op>` parameter or a
    /// `my &infix:<op>` binding) that shadows the package-level operator. Returns
    /// the bound callable when present, else `None`.
    pub(super) fn lexical_infix_override(
        &mut self,
        code: &CompiledCode,
        infix_name: &str,
    ) -> Option<Value> {
        let ampname = format!("&{}", infix_name);
        let candidate = self
            .locals_get_by_name(code, &ampname)
            .or_else(|| self.env().get(&ampname).cloned());
        candidate.filter(|v| matches!(v, Value::Sub(_) | Value::WeakSub(_) | Value::Mixin(..)))
    }

    pub(super) fn exec_call_func_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_function_dispatch();
        // If this name is used as a `&`-sigil parameter anywhere, a lexical
        // `&name` binding in the current frame may shadow a same-named package
        // sub. The name-keyed light-call caches cannot represent that, so bypass
        // them and let the slow path's `lexical_override` resolve correctly.
        // Guarded by `is_empty()` so the common (no `&`-param) case is free.
        let skip_name_caches = if self.amp_param_shadowed_names.is_empty() {
            false
        } else {
            let name_str = Self::const_str(code, name_idx);
            self.amp_param_shadowed_names
                .contains(&Symbol::intern(name_str))
        };
        // Ultra-fast path: positional light-call cache for positional-only functions.
        if !skip_name_caches {
            let name_str = Self::const_str(code, name_idx);
            let name_sym = Symbol::intern(name_str);
            if self.pos_light_call_cache_gen == self.fn_resolve_gen {
                if let Some((cached_key, cached_fp)) = self.pos_light_call_cache.get(&name_sym)
                    && let Some(cf) = compiled_fns.get(cached_key.as_str())
                    && cf.fingerprint == *cached_fp
                {
                    let arity_usize = arity as usize;
                    if self.stack.len() >= arity_usize {
                        // Check if any arg is a Junction -- if so, skip the fast path
                        // to allow junction auto-threading.
                        let stack_args = &self.stack[self.stack.len() - arity_usize..];
                        let has_junction = stack_args
                            .iter()
                            .any(|v| matches!(v, Value::Junction { .. }));
                        // Slice 2d: array/hash into a plain `$` param must share the
                        // caller's container -> fall through to the slow path.
                        let share_into_scalar =
                            Self::call_shares_container_into_scalar_param(cf, stack_args);
                        if !has_junction && !share_into_scalar {
                            let start = self.stack.len() - arity_usize;
                            let args: Vec<Value> = self.stack.drain(start..).collect();
                            // Extract callsite line for deprecation tracking
                            let cl = crate::runtime::Interpreter::peek_callsite_line(&args);
                            if cl.is_some() {
                                loan_env!(self, set_pending_callsite_line(cl));
                            }
                            let result = self.call_compiled_function_positional_light(
                                cf,
                                &args,
                                compiled_fns,
                                name_str,
                            )?;
                            self.stack.push(result);
                            // Compiled path: positional_light's scoped-overlay
                            // merge already sets env_dirty iff a captured-outer
                            // write happened. A blanket set here would force a
                            // redundant locals pull on every pure call (e.g. fib).
                            return Ok(());
                        }
                    }
                }
            } else {
                self.pos_light_call_cache.clear();
                self.pos_light_call_cache_gen = self.fn_resolve_gen;
            }
        }

        // Light-call cache check for named-param functions.
        if !skip_name_caches {
            let name_str = Self::const_str(code, name_idx);
            let name_sym = Symbol::intern(name_str);
            if self.light_call_cache_gen == self.fn_resolve_gen {
                if let Some((cached_key, cached_fp)) = self.light_call_cache.get(&name_sym)
                    && let Some(cf) = compiled_fns.get(cached_key.as_str())
                    && cf.fingerprint == *cached_fp
                {
                    let arity_usize = arity as usize;
                    if self.stack.len() >= arity_usize
                        && !Self::call_shares_container_into_scalar_param(
                            cf,
                            &self.stack[self.stack.len() - arity_usize..],
                        )
                    {
                        let start = self.stack.len() - arity_usize;
                        let args: Vec<Value> = self.stack.drain(start..).collect();
                        // Extract callsite line for deprecation tracking
                        let cl = crate::runtime::Interpreter::peek_callsite_line(&args);
                        if cl.is_some() {
                            loan_env!(self, set_pending_callsite_line(cl));
                        }
                        let result = self.call_compiled_function_light(cf, args, compiled_fns)?;
                        self.stack.push(result);
                        // Compiled path: light's scoped-overlay merge already
                        // signals env_dirty only on a captured-outer write.
                        return Ok(());
                    }
                }
            } else {
                self.light_call_cache.clear();
                self.light_call_cache_gen = self.fn_resolve_gen;
            }
        }

        // OTF-compiled function cache check: for user-defined functions that
        // were compiled on-the-fly (not in compiled_fns), use the cached
        // compiled form to avoid the expensive interpreter fallback.
        // We take() the CF from the cache to avoid holding a borrow on self,
        // then put it back after the call.
        if !skip_name_caches {
            let name_str = Self::const_str(code, name_idx);
            let name_sym = Symbol::intern(name_str);
            if self.otf_call_cache_gen == self.fn_resolve_gen {
                // Skip this type-blind name-keyed fast cache for multi names: the
                // right candidate depends on argument types, which this cache
                // does not key on (the multi fork resolves per-call via
                // resolve_function_with_types instead). Guard the lookup, not the
                // gen check, so a multi call never clears the whole cache.
                if !self.has_multi_candidates_cached(name_str)
                    && let Some(cf) = self.otf_call_cache.remove(&name_sym)
                    && !cf.has_inner_subs
                {
                    let arity_usize = arity as usize;
                    if self.stack.len() >= arity_usize {
                        let start = self.stack.len() - arity_usize;
                        let args: Vec<Value> = self.stack.drain(start..).collect();

                        // Extract callsite line for deprecation tracking
                        let cl = crate::runtime::Interpreter::peek_callsite_line(&args);
                        if cl.is_some() {
                            loan_env!(self, set_pending_callsite_line(cl));
                        }

                        // Slice 2d: array/hash passed to a plain `$` param must
                        // share the caller's container -> force the slow binding
                        // path (the slot-only fast paths bind a detached copy).
                        let share_into_scalar =
                            Self::call_shares_container_into_scalar_param(&cf, &args);
                        let result =
                            if !share_into_scalar && Self::is_light_call_eligible(&cf, name_str) {
                                self.call_compiled_function_light(&cf, args, compiled_fns)
                            } else if !share_into_scalar
                                && Self::is_positional_light_call_eligible(&cf, name_str)
                            {
                                self.call_compiled_function_positional_light(
                                    &cf,
                                    &args,
                                    compiled_fns,
                                    name_str,
                                )
                            } else {
                                let pkg = self.current_package().to_string();
                                self.push_samewith_context(name_str, None);
                                let pushed_dispatch =
                                    loan_env!(self, push_multi_dispatch_frame(name_str, &args));
                                let r = self.call_compiled_function_named(
                                    &cf,
                                    args,
                                    compiled_fns,
                                    &pkg,
                                    name_str,
                                );
                                self.pop_samewith_context();
                                if pushed_dispatch {
                                    self.pop_multi_dispatch();
                                }
                                r
                            };
                        // Put CF back in cache
                        self.otf_call_cache.insert(name_sym, cf);
                        self.stack.push(result?);
                        // Slice 6.3 step 2: all three sub-cases now signal env_dirty
                        // precisely — light / positional_light via their scoped-overlay
                        // merge, and the named sub-case via call_compiled_function_named's
                        // return merge. No blanket mark needed.
                        return Ok(());
                    }
                    // Put CF back if we couldn't use it (stack underflow)
                    self.otf_call_cache.insert(name_sym, cf);
                }
            } else {
                self.otf_call_cache.clear();
                self.otf_call_cache_gen = self.fn_resolve_gen;
            }
        }

        // If there's a lexical `&name` override — either as a compiled local
        // slot (e.g. from a `&foo` parameter binding) or in the env — it
        // shadows package-level subs. Skip the fast path and dispatch via
        // the lexical callable below.
        let lexical_override: Option<Value> = {
            let name_str = Self::const_str(code, name_idx);
            // Only look for a lexical override when there is actually a
            // same-named package sub to shadow. When no package sub exists,
            // the normal dispatch path already handles lexical `&name`
            // bindings correctly (via its own env lookup), and avoiding
            // this branch prevents regressions where dispatching through
            // `call_sub_value` behaves differently (e.g. dynamic `$*ERR`
            // handling for `note` inside a caller-provided block).
            if self.has_proto(name_str)
                || self.has_multi_candidates(name_str)
                || !self.has_function(name_str)
            {
                None
            } else {
                let ampname = format!("&{}", name_str);
                // First check local slots (parameter bindings live here).
                let from_local = self.locals_get_by_name(code, &ampname);
                let candidate = from_local.or_else(|| self.env().get(&ampname).cloned());
                candidate.filter(|v| Self::env_callable_is_lexical_override(v, name_str))
            }
        };
        let has_lexical_override = lexical_override.is_some();
        // Early fast path: for cached zero-arg compiled functions, skip ALL the
        // expensive arg processing, CALL-ME check, wrap chain check, autothread, etc.
        // Only the callsite line pair (if present) needs to be popped from the stack.
        if !has_lexical_override && arity <= 1 {
            let name_str = Self::const_str(code, name_idx);
            let name_sym = Symbol::intern(name_str);
            let cache_key = (name_sym, 0usize, Vec::<String>::new());
            let use_cache = !self.has_multi_candidates_cached(name_str);
            if use_cache
                && self.fn_resolve_cache_gen == self.fn_resolve_gen
                && self.wrap_sub_id_for_name(name_str).is_none()
                && !loan_env!(self, routine_is_test_assertion_by_name(name_str, &[]))
                && let Some((cached_key, cached_fp, _)) = self.fn_resolve_cache.get(&cache_key)
                && let Some(cf) = compiled_fns.get(cached_key.as_str())
                && cf.fingerprint == *cached_fp
                && Self::is_fast_call_eligible(cf, name_str)
                && !cf.is_raw
            {
                // Pop the callsite pair arg(s) from the stack and extract callsite line
                let arity = arity as usize;
                if self.stack.len() >= arity && arity > 0 {
                    let start = self.stack.len() - arity;
                    let popped: Vec<Value> = self.stack.drain(start..).collect();
                    let cl = crate::runtime::Interpreter::peek_callsite_line(&popped);
                    if cl.is_some() {
                        loan_env!(self, set_pending_callsite_line(cl));
                    }
                }
                let result = self.call_compiled_function_fast(cf, compiled_fns)?;
                self.stack.push(result);
                // Slice 6.3 step 2: no blanket env_dirty here. call_compiled_function_fast
                // now signals env_dirty precisely: for a function WITH locals via its
                // scoped-overlay / clone merge (captured-outer write only), and for a
                // 0-local function via the compile-time `has_env_writes` gate. A pure
                // 0-arg call (`sub f { 42 }`) no longer forces a per-call
                // O(caller-locals) locals pull.
                return Ok(());
            }
        }
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("Interpreter stack underflow in CallFunc"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        // val() uses capture semantics (Mu |) — preserve Slip as a single arg.
        let preserve_slip_entirely = name == "val";
        let preserve_empty_slip = Self::preserve_empty_slip_arg(&name);
        let args = if preserve_slip_entirely {
            raw_args
        } else {
            let mut args = Vec::new();
            for arg in raw_args {
                Self::append_flattened_call_arg(&mut args, arg, preserve_empty_slip);
            }
            args
        };
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        let arg_sources = if arg_sources.as_ref().is_some_and(|sources| {
            sources.len() != args.len()
                && !(args.is_empty()
                    && sources.len() == 1
                    && sources.first().is_some_and(|name| name.is_some()))
        }) {
            None
        } else {
            arg_sources
        };
        let args = self.normalize_call_args_for_target(&name, args);
        let (args, callsite_line) = self.sanitize_call_args(&args);
        // Don't auto-FETCH Proxy args for control flow builtins that must preserve containers,
        // or when in lvalue assignment context (e.g. f() = 42 calls f with in_lvalue_assignment=true).
        let skip_proxy_fetch = matches!(
            name.as_str(),
            "return-rw"
                | "return"
                | "die"
                | "fail"
                | "leave"
                | "__mutsu_assign_method_lvalue"
                | "__mutsu_index_assign_method_lvalue"
        ) || self.in_lvalue_assignment;
        let args = if skip_proxy_fetch {
            args
        } else {
            self.auto_fetch_proxy_args(args)?
        };
        loan_env!(self, set_pending_callsite_line(callsite_line));
        // Check if there's a CALL-ME override from trait_mod mixin
        let call_me_override =
            self.env()
                .get(&format!("&{}", name))
                .cloned()
                .and_then(|callable| {
                    if let Value::Mixin(_, ref mixins) = callable {
                        let has_call_me = mixins.keys().any(|key| {
                            key.strip_prefix("__mutsu_role__")
                                .is_some_and(|rn| self.role_has_method(rn, "CALL-ME"))
                        });
                        if has_call_me {
                            return Some(callable);
                        }
                    }
                    None
                });
        // Junction auto-threading for function call arguments:
        // If any positional arg is a Junction and the function parameter doesn't accept
        // Junction (i.e., not typed as Mu or Junction), auto-thread over the junction.
        if let Some(autothread_result) =
            self.maybe_autothread_func_call(code, &name, &args, &arg_sources, compiled_fns)?
        {
            self.stack.push(autothread_result);
            self.env_dirty = true;
            return Ok(());
        }

        // Check wrap chain for named function calls
        if let Some(sub_id) = self.wrap_sub_id_for_name(&name)
            && !self.is_wrap_dispatching(sub_id)
            && let Some(sub_val) = self.get_wrapped_sub(&name)
        {
            let result = self.vm_call_sub_value(sub_val, args, false)?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }

        // Lexical `&name` binding (e.g. from `sub callit(&foo) { foo(1) }`)
        // takes precedence over package-level compiled subs. Dispatch
        // Interpreter-natively via `vm_call_on_value` (same as the pure-lexical case in
        // `dispatch_func_call_inner`, Track A): `call_compiled_closure` roots
        // the closure frame at the live caller env (scoped_child) so dynamic
        // vars (`my $*ERR` in the caller) stay visible, and first-class
        // instance cells make mutating methods on caller-held instances visible
        // across frames. The override value is always a `Value::Sub`
        // (`env_callable_is_lexical_override`), so this never reaches the
        // interpreter terminal.
        if let Some(callable) = lexical_override {
            let result = self.vm_call_on_value(callable, args, Some(compiled_fns))?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }
        let result = self.dispatch_func_call_inner(
            code,
            &name,
            args,
            arg_sources,
            call_me_override,
            compiled_fns,
        )?;
        self.stack.push(result);
        // env_dirty is now managed inside dispatch_func_call_inner: the
        // interpreter / native fallback branches set it (they mutate env by
        // name), while the compiled fast paths (positional_light / light /
        // named) rely on their own scoped-overlay merge to signal env_dirty
        // only when a captured-outer write actually happened. This stops a pure
        // compiled call (e.g. `fib`) from forcing a redundant locals pull per
        // call.
        Ok(())
    }

    /// Expression-level call with capture slip: regular args + 1 slip arg on stack.
    /// The slip arg is flattened into the argument list, result is pushed.
    pub(super) fn exec_call_func_slip_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        regular_arity: u32,
        _arg_sources_idx: Option<u32>,
        slip_pos: Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_function_dispatch();
        let name = Self::const_str(code, name_idx).to_string();
        let total = regular_arity as usize + 1; // +1 for the slip value
        if self.stack.len() < total {
            return Err(RuntimeError::new(
                "Interpreter stack underflow in CallFuncSlip",
            ));
        }
        // When slip_pos is known, args are in source order on the stack.
        // Pop all values, expand the slip at its position.
        let stack_start = self.stack.len() - total;
        let raw_args: Vec<Value> = self.stack.drain(stack_start..).collect();
        let mut args: Vec<Value> = Vec::new();
        if let Some(pos) = slip_pos {
            let pos = pos as usize;
            for (i, val) in raw_args.into_iter().enumerate() {
                if i == pos {
                    Self::append_slip_value(&mut args, val);
                } else {
                    args.push(val);
                }
            }
        } else {
            // Legacy behavior: slip is last on stack (compiled last)
            let slip_val = raw_args.last().cloned().unwrap_or(Value::Nil);
            args.extend(raw_args.into_iter().take(regular_arity as usize));
            Self::append_slip_value(&mut args, slip_val);
        }
        let args = self.normalize_call_args_for_target(&name, args);
        let (args, callsite_line) = self.sanitize_call_args(&args);
        loan_env!(self, set_pending_callsite_line(callsite_line));
        // A lexical `&name` parameter (or `my &name`) that shadows a same-named
        // package sub wins over the package sub, even when the call slips its
        // args (`op(|@args)`). This mirrors the `lexical_override` handling in
        // `exec_call_func_op`; without it the `find_compiled_function` branch
        // below would pick the package sub and ignore the shadow. Only grabbed
        // when a same-named package sub actually exists (otherwise the
        // pure-lexical path in the final `else` handles it).
        if self.has_function(&name) && !self.has_proto(&name) && !self.has_multi_candidates(&name) {
            let ampname = format!("&{}", name);
            let from_local = self.locals_get_by_name(code, &ampname);
            let candidate = from_local.or_else(|| self.env().get(&ampname).cloned());
            if let Some(callable) =
                candidate.filter(|v| Self::env_callable_is_lexical_override(v, &name))
            {
                let result = self.vm_call_on_value(callable, args, Some(compiled_fns))?;
                self.stack.push(result);
                self.env_dirty = true;
                return Ok(());
            }
        }
        if !self.has_proto(&name)
            && let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args)
        {
            let cf_auto_fetch = !cf.is_raw;
            let pkg = self.current_package().to_string();
            let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
            let result = self.maybe_fetch_rw_proxy(result, cf_auto_fetch)?;
            self.stack.push(result);
            // Slice 6.3 step 2: precise env_dirty from the named-call merge.
        } else if loan_env!(self, user_function_matches_call(&name, &args)) {
            // A user-defined sub shadows a same-named builtin (③ PR-2). OTF-compile
            // the resolved def to bytecode when it is a plain single candidate and
            // simple enough; otherwise keep tree-walking. Restricted to genuine
            // builtin shadows (this branch also catches ordinary module/dynamic user
            // subs whose args match — those keep tree-walking, since
            // def_is_otf_compilable doesn't catch every interpreter-needing
            // construct). proto / multi keep going through call_function_fallback so
            // candidate dispatch stays correct. Must not fall through to the native
            // arm below (the shadowed builtin).
            if crate::runtime::Interpreter::is_builtin_function(&name)
                && !self.has_proto(&name)
                && !self.has_multi_candidates_cached(&name)
                && let Some(def) = loan_env!(self, resolve_function_with_types(&name, &args))
                && Self::def_is_otf_compilable(&def)
            {
                let result = self.compile_and_call_function_def(&def, args, compiled_fns)?;
                self.stack.push(result);
            } else {
                crate::vm::vm_stats::record_function_fallback(&name);
                let result = self.vm_call_function_fallback(&name, &args)?;
                let result = loan_env!(self, maybe_fetch_rw_proxy(result, true))?;
                self.stack.push(result);
            }
            self.env_dirty = true;
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            self.stack.push(native_result?);
        } else if let Some(callable) = self.lexical_amp_var_callable(Some(code), &name) {
            // Pure lexical `&name` callable invoked with a slip (`op(|@args)`):
            // dispatch Interpreter-natively via vm_call_on_value, same as the non-slip
            // case in `dispatch_func_call_inner` (Track A). Builtin priority is
            // preserved because try_native_function already ran above, and
            // lexical_amp_var_callable excludes builtin / package-sub names.
            let result = self.vm_call_on_value(callable, args, Some(compiled_fns))?;
            self.stack.push(result);
            self.env_dirty = true;
        } else {
            // Sync Interpreter locals to env before spawning threads so closures capture them
            if name == "start" {
                self.sync_env_from_locals(code);
            }
            let result = self.call_function_compiled_first(&name, args, compiled_fns)?;
            let result = loan_env!(self, maybe_fetch_rw_proxy(result, true))?;
            self.stack.push(result);
            self.env_dirty = true;
        }
        Ok(())
    }

    pub(super) fn exec_call_on_value_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_function_dispatch();
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new(
                "Interpreter stack underflow in CallOnValue",
            ));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, false);
        }
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        let arg_sources = if arg_sources.as_ref().is_some_and(|s| s.len() != args.len()) {
            None
        } else {
            arg_sources
        };
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("Interpreter stack underflow in CallOnValue target".to_string())
        })?;

        // Resolve slot refs to their underlying values before dispatch
        let target = match &target {
            Value::HashSlotRef { .. } => target.hash_slot_read(),
            _ => target,
        };

        // Upgrade WeakSub (e.g., &?BLOCK) to strong Sub before dispatch
        let target = if let Value::WeakSub(ref weak) = target {
            match weak.upgrade() {
                Some(strong) => Value::Sub(strong),
                None => Value::Nil,
            }
        } else {
            target
        };

        let sub_is_rw = if let Value::Sub(ref data) = target {
            data.is_rw
        } else {
            false
        };
        self.set_pending_call_arg_sources(arg_sources);
        let result = self.vm_call_on_value(target, args, Some(compiled_fns));
        self.set_pending_call_arg_sources(None);
        let result = result?;
        let result = loan_env!(self, maybe_fetch_rw_proxy(result, sub_is_rw))?;
        self.stack.push(result);
        self.env_dirty = true;
        Ok(())
    }

    pub(super) fn exec_call_on_code_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_function_dispatch();
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new(
                "Interpreter stack underflow in CallOnCodeVar",
            ));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, false);
        }
        let (args, callsite_line) = self.sanitize_call_args(&args);
        loan_env!(self, set_pending_callsite_line(callsite_line));
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        let arg_sources = if arg_sources.as_ref().is_some_and(|s| s.len() != args.len()) {
            None
        } else {
            arg_sources
        };
        // resolve_code_var handles pseudo-package stripping internally
        let mut target = loan_env!(self, resolve_code_var(&name));
        // Fallback for fast-path method dispatch (skip_env_setup=true):
        // &!attr is not set in env, so read directly from self's instance
        // attributes when available.
        if matches!(target, Value::Nil)
            && let Some(attr_name) = name.strip_prefix('!').filter(|n| !n.is_empty())
            && let Some(Value::Instance { attributes, .. }) =
                self.get_env_with_main_alias("self").as_ref()
            && let Some(attr_val) = attributes.as_map().get(attr_name)
        {
            target = attr_val.clone();
        }
        let result = if !matches!(target, Value::Nil) {
            let sub_is_rw = if let Value::Sub(ref data) = target {
                data.is_rw
            } else {
                false
            };
            self.set_pending_call_arg_sources(arg_sources.clone());
            let result = self.vm_call_on_value(target, args, Some(compiled_fns));
            self.set_pending_call_arg_sources(None);
            let result = result?;
            loan_env!(self, maybe_fetch_rw_proxy(result, sub_is_rw))?
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?
        } else if !self.has_proto(&name)
            && let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args)
        {
            let cf_auto_fetch = !cf.is_raw;
            let pkg = self.current_package().to_string();
            self.set_pending_call_arg_sources(arg_sources.clone());
            let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name);
            self.set_pending_call_arg_sources(None);
            let result = result?;
            loan_env!(self, maybe_fetch_rw_proxy(result, cf_auto_fetch))?
        } else {
            // Sync Interpreter locals to env before spawning threads so closures capture them
            if name == "start" {
                self.sync_env_from_locals(code);
            }
            self.set_pending_call_arg_sources(arg_sources);
            let result = self.call_function_compiled_first(&name, args, compiled_fns);
            self.set_pending_call_arg_sources(None);
            result?
        };
        let result = loan_env!(self, maybe_fetch_rw_proxy(result, true))?;
        self.stack.push(result);
        self.env_dirty = true;
        Ok(())
    }

    /// Inner dispatch for function calls. Handles CALL-ME override, compiled functions,
    /// native functions, and interpreter fallback. Returns the result value.
    pub(super) fn dispatch_func_call_inner(
        &mut self,
        code: &CompiledCode,
        name: &str,
        args: Vec<Value>,
        arg_sources: Option<Vec<Option<String>>>,
        call_me_override: Option<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        if let Some(callable) = call_me_override {
            let result = self.try_compiled_method_or_interpret(callable, "CALL-ME", args);
            self.env_dirty = true;
            let result = result?;
            loan_env!(self, maybe_fetch_rw_proxy(result, true))
        } else {
            self.set_pending_call_arg_sources(arg_sources.clone());
            let compiled = if !self.has_proto(name) {
                self.find_compiled_function(compiled_fns, name, &args)
            } else {
                None
            };
            self.set_pending_call_arg_sources(None);
            if let Some(cf) = compiled {
                // Try positional light call path first (ultra-fast, no env clone).
                // Skip for multi functions since the cache doesn't differentiate by arg types.
                if Self::is_positional_light_call_eligible(cf, name)
                    && !Self::call_shares_container_into_scalar_param(cf, &args)
                    && !self.has_multi_candidates_cached(name)
                    && !loan_env!(self, routine_is_test_assertion_by_name(name, &args))
                    && self.wrap_sub_id_for_name(name).is_none()
                {
                    let name_sym = Symbol::intern(name);
                    if !self.pos_light_call_cache.contains_key(&name_sym) {
                        for (key, func) in compiled_fns {
                            if std::ptr::eq(func, cf) {
                                self.pos_light_call_cache
                                    .insert(name_sym, (key.clone(), cf.fingerprint));
                                break;
                            }
                        }
                    }
                    let result =
                        self.call_compiled_function_positional_light(cf, &args, compiled_fns, name);
                    let result = result?;
                    return loan_env!(self, maybe_fetch_rw_proxy(result, true));
                }
                // Try light call path for simple functions in tight loops.
                // This avoids the expensive env clone/restore cycle.
                if Self::is_light_call_eligible(cf, name)
                    && !Self::call_shares_container_into_scalar_param(cf, &args)
                    && !loan_env!(self, routine_is_test_assertion_by_name(name, &args))
                    && self.wrap_sub_id_for_name(name).is_none()
                {
                    // Populate light-call cache so subsequent calls skip resolution
                    let name_sym = Symbol::intern(name);
                    if !self.light_call_cache.contains_key(&name_sym) {
                        // Find the compiled_fns key for this function
                        for (key, func) in compiled_fns {
                            if std::ptr::eq(func, cf) {
                                self.light_call_cache
                                    .insert(name_sym, (key.clone(), cf.fingerprint));
                                break;
                            }
                        }
                    }
                    let result = self.call_compiled_function_light(cf, args, compiled_fns);
                    let result = result?;
                    return loan_env!(self, maybe_fetch_rw_proxy(result, true));
                }
                self.set_pending_call_arg_sources(arg_sources.clone());
                let pushed_dispatch = loan_env!(self, push_multi_dispatch_frame(name, &args));
                self.push_samewith_context(name, None);
                // Use the function's defining package so that lookups inside the
                // function body resolve against the correct namespace.
                let pkg = if let Some(cached_pkg) = self.cached_fn_package(name, args.len()) {
                    cached_pkg
                } else {
                    let resolved_def = loan_env!(self, resolve_function_with_types(name, &args));
                    if let Some(ref def) = resolved_def {
                        let cl = crate::runtime::Interpreter::peek_callsite_line(&args)
                            .or_else(|| self.pending_callsite_line());
                        loan_env!(self, check_deprecation_for_def_with_line(def, cl));
                    }
                    resolved_def
                        .map(|def| def.package.resolve())
                        .unwrap_or_else(|| self.current_package().to_string())
                };
                let cf_auto_fetch = !cf.is_raw;
                let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, name);
                self.set_pending_call_arg_sources(None);
                self.pop_samewith_context();
                if pushed_dispatch {
                    self.pop_multi_dispatch();
                }
                // Slice 6.3 step 2: no blanket mark. call_compiled_function_named
                // now signals env_dirty precisely — its return merge sets it when a
                // captured-outer / `is rw` writeback (or an `is raw` return) actually
                // wrote a caller-aliasing value. A pure heavy-signature call (default
                // param, return type, where-constraint) no longer forces a per-call
                // O(caller-locals) pull.
                let result = result?;
                loan_env!(self, maybe_fetch_rw_proxy(result, cf_auto_fetch))
            } else {
                // Interpreter / native fallback paths route through the
                // tree-walking interpreter, which can mutate the shared env by
                // name (globals, dynamic vars, captured-outer writes). Mark env
                // dirty so the caller re-syncs its locals. The compiled fast
                // paths above instead rely on their own scoped-overlay merge to
                // signal env_dirty only when a captured-outer write happened, so
                // a pure compiled call no longer forces a per-call locals pull.
                self.env_dirty = true;
                if self.has_multi_candidates_cached(name) && !self.has_proto(name) {
                    // User-defined multi candidates take priority over builtins.
                    // Resolve the winning candidate Interpreter-side via the same resolver
                    // call_function_fallback uses (③ PR-3, ledger §2). When the
                    // winner is unambiguous and OTF-compilable, run it as compiled
                    // bytecode instead of tree-walking through the interpreter.
                    // For functions, ambiguity is signalled by returning None +
                    // a pending_dispatch_error (dispatch.rs choose_best_matching_
                    // candidate), so a Some(def) here is already an unambiguous
                    // winner. Clear any stale pending error first (mirrors
                    // resolve_function_with_alias) so a prior call's ambiguity
                    // can't leak. Non-otf-compilable (where/default/code-param) and
                    // no-match/ambiguous all fall through to call_function_fallback,
                    // which re-resolves and raises X::Multi::Ambiguous / NoMatch.
                    // The selected candidate's own redispatch (`nextsame`/`callsame`/
                    // `callwith`) still works because compile_and_call_function_def
                    // pushes the same multi-dispatch frame the interpreter would.
                    // Skip names the interpreter must handle natively even when a
                    // multi candidate is registered for them: native Test routines
                    // (is-eqv/is-deeply/…) register multi stubs but are implemented
                    // in Rust, so OTF-compiling the stub bypasses the native handler
                    // and corrupts behaviour (regressed S16-io/words.t,
                    // S32-io/slurp.t via is-eqv). Mirrors the non-builtin OTF path's
                    // is_interpreter_handled_function gate below.
                    let _ = self.take_pending_dispatch_error();
                    if !self.is_interpreter_handled_function(name)
                        && let Some(def) = loan_env!(self, resolve_function_with_types(name, &args))
                        && Self::def_is_otf_compilable(&def)
                        && !Self::function_body_declares_state(&def.body)
                    {
                        self.compile_and_call_function_def(&def, args, compiled_fns)
                    } else {
                        crate::vm::vm_stats::record_function_fallback(name);
                        self.set_pending_call_arg_sources(arg_sources);
                        let result = self.vm_call_function_fallback(name, &args);
                        self.set_pending_call_arg_sources(None);
                        let result = result?;
                        loan_env!(self, maybe_fetch_rw_proxy(result, true))
                    }
                } else if loan_env!(self, user_function_matches_call(name, &args)) {
                    // A user-defined sub shadows a same-named builtin (③ PR-2). When
                    // the resolved def is a plain single candidate that is
                    // OTF-compilable, run it as compiled bytecode — but resolve it
                    // explicitly and DO NOT fall through to the native arm below
                    // (which would pick the shadowed builtin). proto / multi cases
                    // (this branch is reached by proto'd multis, since the non-proto
                    // multi fork above did not fire) must keep going through
                    // call_function_fallback so candidate dispatch stays correct;
                    // complex-bodied / complex-signature shadows likewise tree-walk.
                    //
                    // Restrict the OTF takeover to genuine builtin shadows: this
                    // branch is also reached by ordinary module/dynamic user subs
                    // (not in compiled_fns) whose args strictly match, and
                    // def_is_otf_compilable does not catch every construct that needs
                    // the interpreter (e.g. a nested `sub` whose `when` control flow
                    // must not escape the enclosing routine — Test::Util's
                    // is-deeply-junction). Those keep tree-walking unchanged.
                    if crate::runtime::Interpreter::is_builtin_function(name)
                        && !self.has_proto(name)
                        && !self.has_multi_candidates_cached(name)
                        && let Some(def) = loan_env!(self, resolve_function_with_types(name, &args))
                        && Self::def_is_otf_compilable(&def)
                    {
                        self.compile_and_call_function_def(&def, args, compiled_fns)
                    } else {
                        crate::vm::vm_stats::record_function_fallback(name);
                        self.set_pending_call_arg_sources(arg_sources);
                        let result = self.vm_call_function_fallback(name, &args);
                        self.set_pending_call_arg_sources(None);
                        let result = result?;
                        loan_env!(self, maybe_fetch_rw_proxy(result, true))
                    }
                } else if let Some(native_result) =
                    self.try_native_function(Symbol::intern(name), &args)
                {
                    native_result
                } else if !self.is_interpreter_handled_function(name)
                && !self.has_multi_candidates_cached(name)
                && let Some(def) = loan_env!(self, resolve_function_with_types(name, &args))
                // Only OTF-compile simple functions: no default params, no
                // code params (&foo), no where constraints, no closures.
                && Self::def_is_otf_compilable(&def)
                {
                    self.compile_and_call_function_def(&def, args, compiled_fns)
                } else if let Some(result) = self.try_native_test_function(name, &args) {
                    // Dispatch Test functions straight to their typed handler (lever A).
                    result
                } else if let Some(callable) = self.lexical_amp_var_callable(Some(code), name) {
                    // Pure lexical `&name` callable (a `&code` parameter or
                    // `my &f = ...` with no same-named package sub): dispatch
                    // Interpreter-natively via vm_call_on_value instead of the interpreter
                    // terminal (Track A, ledger §2). Builtin priority is preserved
                    // because try_native_function already ran above. Dynamic vars
                    // (`my $*ERR` in the caller) stay visible because
                    // call_compiled_closure roots the closure frame at the live
                    // caller env (scoped_child) and the captured-env merge is
                    // or_insert (parent-chain aware), so it never shadows them.
                    self.vm_call_on_value(callable, args, Some(compiled_fns))
                } else {
                    // Sync Interpreter locals to env before spawning threads so closures capture them
                    if name == "start" {
                        self.sync_env_from_locals(code);
                    }
                    // EVAL/EVALFILE compile to bytecode and run on a sub-Interpreter, and
                    // pseudo-package reads are reflective env lookups: the
                    // interpreter is a carrier here, not a tree-walk fallback.
                    // CARRIER (is_interpreter_carrier_function) vs TODO: compile to
                    // bytecode (else branch = true tree-walk function fallback). The
                    // record_* split already tracks this at runtime. See ledger §2/§C.
                    if Self::is_interpreter_carrier_function(name) {
                        crate::vm::vm_stats::record_function_carrier(name);
                    } else {
                        crate::vm::vm_stats::record_function_fallback(name);
                    }
                    self.set_pending_call_arg_sources(arg_sources);
                    let result = self.vm_call_function(name, args);
                    self.set_pending_call_arg_sources(None);
                    // Interpreter function calls (e.g. `require`) may register
                    // new subs — invalidate function resolution caches.
                    self.fn_resolve_gen += 1;
                    // substr-rw returns a Proxy that must be preserved (not auto-FETCHed)
                    let auto_fetch = name != "substr-rw";
                    let result = result?;
                    loan_env!(self, maybe_fetch_rw_proxy(result, auto_fetch))
                }
            }
        }
    }

    /// Whether a resolved `FunctionDef` is simple enough to compile on-the-fly to
    /// bytecode and run via the Interpreter (instead of tree-walking it through the
    /// interpreter): a plain body and no default/where/code-signature/`&`-code
    /// params. Shared by the non-shadow OTF branch and the builtin-shadow forks
    /// (③ PR-2) so the same compilability gate is applied consistently.
    pub(super) fn def_is_otf_compilable(def: &crate::ast::FunctionDef) -> bool {
        !Self::function_body_needs_interpreter(&def.body)
            && def.param_defs.iter().all(|pd| {
                pd.default.is_none()
                    && pd.where_constraint.is_none()
                    && pd.code_signature.is_none()
                    && !pd.name.starts_with('&')
            })
    }

    /// Check if a function body contains constructs that require
    /// the full interpreter path (class/role declarations, start blocks).
    fn function_body_needs_interpreter(body: &[crate::ast::Stmt]) -> bool {
        use crate::ast::Stmt;
        for stmt in body {
            match stmt {
                Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } => return true,
                Stmt::Expr(expr) if Self::expr_needs_interpreter(expr) => return true,
                _ => {}
            }
        }
        false
    }

    fn expr_needs_interpreter(expr: &crate::ast::Expr) -> bool {
        use crate::ast::Expr;
        match expr {
            Expr::DoStmt(stmt) => Self::function_body_needs_interpreter(std::slice::from_ref(stmt)),
            Expr::Block(body) => Self::function_body_needs_interpreter(body),
            Expr::MethodCall { target, args, .. } => {
                Self::expr_needs_interpreter(target)
                    || args.iter().any(Self::expr_needs_interpreter)
            }
            Expr::Call { name, args } => {
                // start blocks need the interpreter for proper thread spawning
                name.resolve() == "start" || args.iter().any(Self::expr_needs_interpreter)
            }
            _ => false,
        }
    }

    /// True if the body declares a `state` variable anywhere (recursing through
    /// nested blocks). A multi candidate whose body uses `state` must NOT be
    /// OTF-compiled in the multi fork: signature alternates
    /// `(A) | (B) { state $x }` share one state cell via a compile-time
    /// state_group, but the OTF cache keys on the body fingerprint (which
    /// includes the per-alternate signature), so each alternate would get its
    /// own state and the sharing would break. Keep those on the interpreter,
    /// which honors the shared state_group. (Single-candidate OTF subs are
    /// unaffected — their fingerprint is stable across calls — but excluding any
    /// state-bearing multi body is the safe, simple guard.)
    pub(super) fn function_body_declares_state(body: &[crate::ast::Stmt]) -> bool {
        body.iter().any(Self::stmt_declares_state)
    }

    fn stmt_declares_state(stmt: &crate::ast::Stmt) -> bool {
        use crate::ast::Stmt;
        match stmt {
            Stmt::VarDecl { is_state, .. } => *is_state,
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                Self::function_body_declares_state(then_branch)
                    || Self::function_body_declares_state(else_branch)
            }
            Stmt::While { body, .. }
            | Stmt::For { body, .. }
            | Stmt::Loop { body, .. }
            | Stmt::Given { body, .. }
            | Stmt::When { body, .. }
            | Stmt::Whenever { body, .. }
            | Stmt::React { body, .. }
            | Stmt::Subtest { body, .. } => Self::function_body_declares_state(body),
            Stmt::Block(body) | Stmt::SyntheticBlock(body) | Stmt::Default(body) => {
                Self::function_body_declares_state(body)
            }
            Stmt::Expr(e) => Self::expr_declares_state(e),
            _ => false,
        }
    }

    fn expr_declares_state(expr: &crate::ast::Expr) -> bool {
        use crate::ast::Expr;
        match expr {
            Expr::Block(body) => Self::function_body_declares_state(body),
            Expr::DoStmt(stmt) => Self::stmt_declares_state(stmt),
            _ => false,
        }
    }
}
