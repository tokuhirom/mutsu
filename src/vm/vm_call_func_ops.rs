use super::*;
use crate::symbol::Symbol;

impl VM {
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

    pub(super) fn is_user_shadowable_builtin(name: &str) -> bool {
        matches!(
            name,
            "first" | "grep" | "map" | "sort" | "reverse" | "unique" | "last" | "head" | "tail"
        )
    }

    pub(super) fn exec_call_func_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        // Ultra-fast path: positional light-call cache for positional-only functions.
        {
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
                        let has_junction = self.stack[self.stack.len() - arity_usize..]
                            .iter()
                            .any(|v| matches!(v, Value::Junction { .. }));
                        if !has_junction {
                            let start = self.stack.len() - arity_usize;
                            let args: Vec<Value> = self.stack.drain(start..).collect();
                            // Extract callsite line for deprecation tracking
                            let cl = crate::runtime::Interpreter::peek_callsite_line(&args);
                            if cl.is_some() {
                                self.interpreter.set_pending_callsite_line(cl);
                            }
                            let result = self.call_compiled_function_positional_light(
                                cf,
                                &args,
                                compiled_fns,
                            )?;
                            self.stack.push(result);
                            self.env_dirty = true;
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
        {
            let name_str = Self::const_str(code, name_idx);
            let name_sym = Symbol::intern(name_str);
            if self.light_call_cache_gen == self.fn_resolve_gen {
                if let Some((cached_key, cached_fp)) = self.light_call_cache.get(&name_sym)
                    && let Some(cf) = compiled_fns.get(cached_key.as_str())
                    && cf.fingerprint == *cached_fp
                {
                    let arity_usize = arity as usize;
                    if self.stack.len() >= arity_usize {
                        let start = self.stack.len() - arity_usize;
                        let args: Vec<Value> = self.stack.drain(start..).collect();
                        // Extract callsite line for deprecation tracking
                        let cl = crate::runtime::Interpreter::peek_callsite_line(&args);
                        if cl.is_some() {
                            self.interpreter.set_pending_callsite_line(cl);
                        }
                        let result = self.call_compiled_function_light(cf, args, compiled_fns)?;
                        self.stack.push(result);
                        self.env_dirty = true;
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
        {
            let name_str = Self::const_str(code, name_idx);
            let name_sym = Symbol::intern(name_str);
            if self.otf_call_cache_gen == self.fn_resolve_gen {
                if let Some(cf) = self.otf_call_cache.remove(&name_sym)
                    && !cf.has_inner_subs
                {
                    let arity_usize = arity as usize;
                    if self.stack.len() >= arity_usize {
                        let start = self.stack.len() - arity_usize;
                        let args: Vec<Value> = self.stack.drain(start..).collect();

                        // Extract callsite line for deprecation tracking
                        let cl = crate::runtime::Interpreter::peek_callsite_line(&args);
                        if cl.is_some() {
                            self.interpreter.set_pending_callsite_line(cl);
                        }

                        let result = if Self::is_light_call_eligible(&cf, name_str) {
                            self.call_compiled_function_light(&cf, args, compiled_fns)
                        } else if Self::is_positional_light_call_eligible(&cf, name_str) {
                            self.call_compiled_function_positional_light(&cf, &args, compiled_fns)
                        } else {
                            let pkg = self.interpreter.current_package().to_string();
                            self.interpreter.push_samewith_context(name_str, None);
                            let pushed_dispatch =
                                self.interpreter.push_multi_dispatch_frame(name_str, &args);
                            let r = self.call_compiled_function_named(
                                &cf,
                                args,
                                compiled_fns,
                                &pkg,
                                name_str,
                            );
                            self.interpreter.pop_samewith_context();
                            if pushed_dispatch {
                                self.interpreter.pop_multi_dispatch();
                            }
                            r
                        };
                        // Put CF back in cache
                        self.otf_call_cache.insert(name_sym, cf);
                        self.stack.push(result?);
                        self.env_dirty = true;
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
        self.ensure_env_synced(code);
        let lexical_override: Option<Value> = {
            let name_str = Self::const_str(code, name_idx);
            // Only look for a lexical override when there is actually a
            // same-named package sub to shadow. When no package sub exists,
            // the normal dispatch path already handles lexical `&name`
            // bindings correctly (via its own env lookup), and avoiding
            // this branch prevents regressions where dispatching through
            // `call_sub_value` behaves differently (e.g. dynamic `$*ERR`
            // handling for `note` inside a caller-provided block).
            if self.interpreter.has_proto(name_str)
                || self.interpreter.has_multi_candidates(name_str)
                || !self.interpreter.has_function(name_str)
            {
                None
            } else {
                let ampname = format!("&{}", name_str);
                // First check local slots (parameter bindings live here).
                let from_local = self.locals_get_by_name(code, &ampname);
                let candidate =
                    from_local.or_else(|| self.interpreter.env().get(&ampname).cloned());
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
                && self.interpreter.wrap_sub_id_for_name(name_str).is_none()
                && !self
                    .interpreter
                    .routine_is_test_assertion_by_name(name_str, &[])
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
                        self.interpreter.set_pending_callsite_line(cl);
                    }
                }
                self.ensure_env_synced(code);
                let result = self.call_compiled_function_fast(cf, compiled_fns)?;
                self.stack.push(result);
                self.env_dirty = true;
                return Ok(());
            }
        }
        self.ensure_env_synced(code);
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("VM stack underflow in CallFunc"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let preserve_empty_slip = Self::preserve_empty_slip_arg(&name);
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, preserve_empty_slip);
        }
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
        let (args, callsite_line) = self.interpreter.sanitize_call_args(&args);
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
        ) || self.interpreter.in_lvalue_assignment;
        let args = if skip_proxy_fetch {
            args
        } else {
            self.auto_fetch_proxy_args(args)?
        };
        self.interpreter.set_pending_callsite_line(callsite_line);
        // Check if there's a CALL-ME override from trait_mod mixin
        let call_me_override = self
            .interpreter
            .env()
            .get(&format!("&{}", name))
            .cloned()
            .and_then(|callable| {
                if let Value::Mixin(_, ref mixins) = callable {
                    let has_call_me = mixins.keys().any(|key| {
                        key.strip_prefix("__mutsu_role__")
                            .is_some_and(|rn| self.interpreter.role_has_method(rn, "CALL-ME"))
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
        if let Some(sub_id) = self.interpreter.wrap_sub_id_for_name(&name)
            && !self.interpreter.is_wrap_dispatching(sub_id)
            && let Some(sub_val) = self.interpreter.get_wrapped_sub(&name)
        {
            let result = self.interpreter.call_sub_value(sub_val, args, false)?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }

        // Lexical `&name` binding (e.g. from `sub callit(&foo) { foo(1) }`)
        // takes precedence over package-level compiled subs.
        if let Some(callable) = lexical_override {
            let result = self.interpreter.call_sub_value(callable, args, false)?;
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
        self.env_dirty = true;
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
        self.ensure_env_synced(code);
        let name = Self::const_str(code, name_idx).to_string();
        let total = regular_arity as usize + 1; // +1 for the slip value
        if self.stack.len() < total {
            return Err(RuntimeError::new("VM stack underflow in CallFuncSlip"));
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
        let (args, callsite_line) = self.interpreter.sanitize_call_args(&args);
        self.interpreter.set_pending_callsite_line(callsite_line);
        if !self.interpreter.has_proto(&name)
            && let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args)
        {
            let cf_auto_fetch = !cf.is_raw;
            let pkg = self.interpreter.current_package().to_string();
            let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
            let result = self
                .interpreter
                .maybe_fetch_rw_proxy(result, cf_auto_fetch)?;
            self.stack.push(result);
            self.env_dirty = true;
        } else if Self::is_user_shadowable_builtin(&name) && self.interpreter.has_function(&name) {
            // A user-defined sub shadows a same-named builtin listop.
            let result = self.interpreter.call_function_fallback(&name, &args)?;
            let result = self.interpreter.maybe_fetch_rw_proxy(result, true)?;
            self.stack.push(result);
            self.env_dirty = true;
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            self.stack.push(native_result?);
        } else {
            // Sync VM locals to env before spawning threads so closures capture them
            if name == "start" {
                self.sync_env_from_locals(code);
            }
            let result = self.call_function_compiled_first(&name, args, compiled_fns)?;
            let result = self.interpreter.maybe_fetch_rw_proxy(result, true)?;
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
        self.ensure_env_synced(code);
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new("VM stack underflow in CallOnValue"));
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
            RuntimeError::new("VM stack underflow in CallOnValue target".to_string())
        })?;

        // Resolve slot refs to their underlying values before dispatch
        let target = match &target {
            Value::HashSlotRef { .. } => target.hash_slot_read(),
            Value::ArraySlotRef { .. } => target.array_slot_read(),
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
        self.interpreter.set_pending_call_arg_sources(arg_sources);
        let result = self.vm_call_on_value(target, args, Some(compiled_fns));
        self.interpreter.set_pending_call_arg_sources(None);
        let result = self.interpreter.maybe_fetch_rw_proxy(result?, sub_is_rw)?;
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
        self.ensure_env_synced(code);
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("VM stack underflow in CallOnCodeVar"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, false);
        }
        let (args, callsite_line) = self.interpreter.sanitize_call_args(&args);
        self.interpreter.set_pending_callsite_line(callsite_line);
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        let arg_sources = if arg_sources.as_ref().is_some_and(|s| s.len() != args.len()) {
            None
        } else {
            arg_sources
        };
        // resolve_code_var handles pseudo-package stripping internally
        let target = self.interpreter.resolve_code_var(&name);
        let result = if !matches!(target, Value::Nil) {
            let sub_is_rw = if let Value::Sub(ref data) = target {
                data.is_rw
            } else {
                false
            };
            self.interpreter
                .set_pending_call_arg_sources(arg_sources.clone());
            let result = self.vm_call_on_value(target, args, Some(compiled_fns));
            self.interpreter.set_pending_call_arg_sources(None);
            self.interpreter.maybe_fetch_rw_proxy(result?, sub_is_rw)?
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?
        } else if !self.interpreter.has_proto(&name)
            && let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args)
        {
            let cf_auto_fetch = !cf.is_raw;
            let pkg = self.interpreter.current_package().to_string();
            self.interpreter
                .set_pending_call_arg_sources(arg_sources.clone());
            let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name);
            self.interpreter.set_pending_call_arg_sources(None);
            self.interpreter
                .maybe_fetch_rw_proxy(result?, cf_auto_fetch)?
        } else {
            // Sync VM locals to env before spawning threads so closures capture them
            if name == "start" {
                self.sync_env_from_locals(code);
            }
            self.interpreter.set_pending_call_arg_sources(arg_sources);
            let result = self.call_function_compiled_first(&name, args, compiled_fns);
            self.interpreter.set_pending_call_arg_sources(None);
            result?
        };
        let result = self.interpreter.maybe_fetch_rw_proxy(result, true)?;
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
            self.interpreter.maybe_fetch_rw_proxy(result?, true)
        } else {
            self.interpreter
                .set_pending_call_arg_sources(arg_sources.clone());
            let compiled = if !self.interpreter.has_proto(name) {
                self.find_compiled_function(compiled_fns, name, &args)
            } else {
                None
            };
            self.interpreter.set_pending_call_arg_sources(None);
            if let Some(cf) = compiled {
                // Try positional light call path first (ultra-fast, no env clone).
                // Skip for multi functions since the cache doesn't differentiate by arg types.
                if Self::is_positional_light_call_eligible(cf, name)
                    && !self.has_multi_candidates_cached(name)
                    && !self
                        .interpreter
                        .routine_is_test_assertion_by_name(name, &args)
                    && self.interpreter.wrap_sub_id_for_name(name).is_none()
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
                        self.call_compiled_function_positional_light(cf, &args, compiled_fns);
                    return self.interpreter.maybe_fetch_rw_proxy(result?, true);
                }
                // Try light call path for simple functions in tight loops.
                // This avoids the expensive env clone/restore cycle.
                if Self::is_light_call_eligible(cf, name)
                    && !self
                        .interpreter
                        .routine_is_test_assertion_by_name(name, &args)
                    && self.interpreter.wrap_sub_id_for_name(name).is_none()
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
                    return self.interpreter.maybe_fetch_rw_proxy(result?, true);
                }
                self.interpreter
                    .set_pending_call_arg_sources(arg_sources.clone());
                let pushed_dispatch = self.interpreter.push_multi_dispatch_frame(name, &args);
                self.interpreter.push_samewith_context(name, None);
                // Use the function's defining package so that lookups inside the
                // function body resolve against the correct namespace.
                let pkg = if let Some(cached_pkg) = self.cached_fn_package(name, args.len()) {
                    cached_pkg
                } else {
                    let resolved_def = self.interpreter.resolve_function_with_types(name, &args);
                    if let Some(ref def) = resolved_def {
                        let cl = crate::runtime::Interpreter::peek_callsite_line(&args)
                            .or_else(|| self.interpreter.pending_callsite_line());
                        self.interpreter
                            .check_deprecation_for_def_with_line(def, cl);
                    }
                    resolved_def
                        .map(|def| def.package.resolve())
                        .unwrap_or_else(|| self.interpreter.current_package().to_string())
                };
                let cf_auto_fetch = !cf.is_raw;
                let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, name);
                self.interpreter.set_pending_call_arg_sources(None);
                self.interpreter.pop_samewith_context();
                if pushed_dispatch {
                    self.interpreter.pop_multi_dispatch();
                }
                self.interpreter
                    .maybe_fetch_rw_proxy(result?, cf_auto_fetch)
            } else if self.has_multi_candidates_cached(name) && !self.interpreter.has_proto(name) {
                // User-defined multi candidates take priority over builtins.
                // Call call_function_fallback directly to bypass the builtin match
                // in call_function, which would shadow user-defined multi subs.
                self.interpreter.set_pending_call_arg_sources(arg_sources);
                let result = self.interpreter.call_function_fallback(name, &args);
                self.interpreter.set_pending_call_arg_sources(None);
                self.interpreter.maybe_fetch_rw_proxy(result?, true)
            } else if Self::is_user_shadowable_builtin(name) && self.interpreter.has_function(name)
            {
                // A user-defined sub shadows a same-named builtin listop.
                // Route through the interpreter fallback so the user sub is
                // invoked rather than the builtin.
                self.interpreter.set_pending_call_arg_sources(arg_sources);
                let result = self.interpreter.call_function_fallback(name, &args);
                self.interpreter.set_pending_call_arg_sources(None);
                self.interpreter.maybe_fetch_rw_proxy(result?, true)
            } else if let Some(native_result) =
                self.try_native_function(Symbol::intern(name), &args)
            {
                native_result
            } else if !self.is_interpreter_handled_function(name)
                && !self.has_multi_candidates_cached(name)
                && let Some(def) = self.interpreter.resolve_function_with_types(name, &args)
                && !Self::function_body_needs_interpreter(&def.body)
                // Only OTF-compile simple functions: no default params, no
                // code params (&foo), no where constraints, no closures.
                && def.param_defs.iter().all(|pd| {
                    pd.default.is_none()
                        && pd.where_constraint.is_none()
                        && pd.code_signature.is_none()
                        && !pd.name.starts_with('&')
                })
            {
                // On-the-fly compile: compile the function definition to
                // bytecode and execute via the VM, avoiding the slow
                // tree-walking interpreter path. The compiled result is
                // cached in otf_compile_cache for subsequent calls.
                // Skip functions with class/role declarations or complex
                // parameter signatures as they need the full interpreter
                // path for correct behavior.
                self.compile_and_call_function_def(&def, args, compiled_fns)
            } else {
                // Sync VM locals to env before spawning threads so closures capture them
                if name == "start" {
                    self.sync_env_from_locals(code);
                }
                self.interpreter.set_pending_call_arg_sources(arg_sources);
                let result = self.interpreter.call_function(name, args);
                self.interpreter.set_pending_call_arg_sources(None);
                // Interpreter function calls (e.g. `require`) may register
                // new subs — invalidate function resolution caches.
                self.fn_resolve_gen += 1;
                // substr-rw returns a Proxy that must be preserved (not auto-FETCHed)
                let auto_fetch = name != "substr-rw";
                self.interpreter.maybe_fetch_rw_proxy(result?, auto_fetch)
            }
        }
    }

    /// Check if a function body contains constructs that require
    /// the full interpreter path (class/role declarations, start blocks).
    fn function_body_needs_interpreter(body: &[crate::ast::Stmt]) -> bool {
        use crate::ast::Stmt;
        for stmt in body {
            match stmt {
                Stmt::ClassDecl { .. } | Stmt::RoleDecl { .. } => return true,
                Stmt::Expr(expr) => {
                    if Self::expr_needs_interpreter(expr) {
                        return true;
                    }
                }
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
}
