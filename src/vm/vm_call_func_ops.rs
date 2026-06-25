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
        // NativeCall: a sub declared `is native(...)` is dispatched through C
        // FFI rather than running its (`{ * }`) Raku body. The registry is
        // empty in the overwhelmingly common case, so this guard is free.
        if !self.native_call_specs.is_empty() {
            let name_str = Self::const_str(code, name_idx);
            if let Some(spec) = self.native_call_specs.get(name_str).cloned() {
                let arity_usize = arity as usize;
                if self.stack.len() < arity_usize {
                    return Err(RuntimeError::new(format!(
                        "NativeCall: '{}' called with too few arguments on the stack",
                        spec.symbol
                    )));
                }
                let start = self.stack.len() - arity_usize;
                let mut args: Vec<Value> = self.stack.drain(start..).collect();
                // Drop the synthetic callsite-line marker the compiler may append.
                args.retain(|a| !Self::is_callsite_line_marker(a));
                let result = crate::runtime::nativecall::call_native(&spec, &args)?;
                self.stack.push(result);
                return Ok(());
            }
        }
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
                            // Slice F: drain any captured-outer writes the body
                            // recorded through to this caller frame's local slots
                            // (the slow dispatch path drains too; the cached fast
                            // path must as well or a second call's write is lost).
                            // The env_dirty-gated reconcile also catches a
                            // *nested* callee's captured-outer write (multi-frame
                            // accumulation), which the single-frame drain misses.
                            // The reconcile is free for a pure call (env not
                            // dirtied — e.g. fib).
                            self.drain_and_reconcile_after_cached_call(code);
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
                    // Slice 2d (named follow-up): an `@`/`%` variable passed by
                    // name to a plain `$` named param must share the caller's
                    // container; the named light path binds a copy. Decode the
                    // arg sources lazily (only on a named-cache hit) so the
                    // common path pays nothing.
                    let named_share = self.stack.len() >= arity_usize && {
                        let decoded = self.decode_arg_sources(code, arg_sources_idx);
                        Self::call_shares_container_into_named_scalar_param(
                            cf,
                            &self.stack[self.stack.len() - arity_usize..],
                            decoded.as_deref(),
                        )
                    };
                    if self.stack.len() >= arity_usize
                        && !named_share
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
                        // Slice F: drain captured-outer writes through to this
                        // caller frame's local slots (see the positional-light
                        // cached path above). The env_dirty-gated reconcile also
                        // covers a nested callee's captured-outer write
                        // (multi-frame accumulation).
                        self.drain_and_reconcile_after_cached_call(code);
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
                        // The named variant (`f(n => @a)` into `:$n`) needs the
                        // arg-source table to find the caller variable, so decode
                        // it here (only on an otf-cache hit).
                        let decoded_sources = self.decode_arg_sources(code, arg_sources_idx);
                        let share_into_scalar =
                            Self::call_shares_container_into_scalar_param(&cf, &args);
                        let named_share = Self::call_shares_container_into_named_scalar_param(
                            &cf,
                            &args,
                            decoded_sources.as_deref(),
                        );
                        let result = if !share_into_scalar
                            && !named_share
                            && Self::is_light_call_eligible(&cf, name_str)
                        {
                            self.call_compiled_function_light(&cf, args, compiled_fns)
                        } else if !share_into_scalar
                            && !named_share
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
                            // The named-share writeback reads the arg sources;
                            // make them available to bind_function_args_values.
                            self.set_pending_call_arg_sources(decoded_sources);
                            let r = self.call_compiled_function_named(
                                &cf,
                                args,
                                compiled_fns,
                                &pkg,
                                name_str,
                            );
                            self.set_pending_call_arg_sources(None);
                            self.pop_samewith_context();
                            if pushed_dispatch {
                                self.pop_multi_dispatch();
                            }
                            r
                        };
                        // Put CF back in cache
                        self.otf_call_cache.insert(name_sym, cf);
                        let result = result?;
                        // Slice F: drain this frame's recorded captured-outer
                        // writes, plus (env_dirty-gated) reconcile to catch a
                        // nested callee's captured-outer write that the
                        // single-frame drain misses (multi-frame accumulation).
                        self.drain_and_reconcile_after_cached_call(code);
                        self.stack.push(result);
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
                let result = match self.call_compiled_function_fast(cf, compiled_fns) {
                    Ok(v) => v,
                    Err(e) => {
                        // Slice F (exception-escape coherence): an exceptional exit
                        // (`die`/`fail`) still ran the callee's UNDO/LEAVE phasers,
                        // which can mutate a captured-outer variable (e.g. `UNDO {
                        // $ng ~= "U" }`). The body recorded those writes into
                        // `pending_rw_writeback_sources`; drain them to this caller's
                        // local slots *before* propagating the error, exactly as the
                        // Ok path does, so the reverse `sync_locals_from_env` pull is
                        // not required for coherence.
                        self.apply_pending_rw_writeback(code);
                        return Err(e);
                    }
                };
                self.stack.push(result);
                // Slice F: write any captured-outer variables the callee mutated
                // straight through to this caller's local slots, so they stay
                // coherent without the reverse `sync_locals_from_env` pull. The
                // env_dirty-gated reconcile additionally catches a *nested*
                // callee's captured-outer write (`via()` -> `bump-outer()` ->
                // `$acc`), which the single-frame drain discards one frame too
                // deep, so `via(); via()` accumulates correctly.
                self.drain_and_reconcile_after_cached_call(code);
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
            // Slice F: the threaded eigenstate calls may have mutated captured-outer
            // variables (`sub j($x) { $count++ }`); write their accumulated final
            // env values through to this caller's local slots so they stay coherent
            // without the reverse `sync_locals_from_env` pull.
            self.apply_pending_rw_writeback(code);
            return Ok(());
        }

        // Check wrap chain for named function calls
        if let Some(sub_id) = self.wrap_sub_id_for_name(&name)
            && !self.is_wrap_dispatching(sub_id)
            && let Some(sub_val) = self.get_wrapped_sub(&name)
        {
            let result = self.vm_call_sub_value(sub_val, args, false)?;
            // Slice F (multi-frame coherence): a wrapper closure (`&f.wrap(-> {
            // $seen = True; callsame })`) mutates a captured caller lexical by name.
            // The closure dispatch recorded it precisely (`pending_*_writeback`);
            // drain it so the caller's slot refreshes without the blanket env→locals
            // pull (env_dirty-removal substrate). The blanket reconcile stays as the
            // fallback (no-op under the substrate harness).
            self.apply_pending_rw_writeback(code);
            self.stack.push(result);
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
            return Ok(());
        }
        // Slice F (env<->locals coherence, docs/env-locals-coherence.md): the
        // lvalue-method writeback builtins (`$p.value = X` / `.value--`,
        // `@a.head = v`, `%h.AT-KEY(k) = v`, `@a.first(...) = v`, ...) mutate
        // their target variable in `env` *by name* (`self.env.insert(var, ...)`)
        // and rely on the reverse pull to refresh the caller's local slot. The
        // target variable name is the 5th argument. Capture it so we can write
        // the new env value straight through to the local slot after dispatch,
        // keeping locals coherent without depending on the `env_dirty` backstop.
        let lvalue_writeback_target = match name.as_str() {
            "__mutsu_assign_method_lvalue" | "__mutsu_index_assign_method_lvalue" => args
                .get(4)
                .map(|v| v.to_string_value())
                .filter(|s| !s.is_empty()),
            _ => None,
        };
        let result = match self.dispatch_func_call_inner(
            code,
            &name,
            args,
            arg_sources,
            call_me_override,
            compiled_fns,
        ) {
            Ok(v) => v,
            Err(e) => {
                // Slice F (exception-escape coherence): an exceptional exit still
                // ran the callee's UNDO/LEAVE phasers, which can mutate a
                // captured-outer variable. Drain those recorded writes to the
                // caller's local slots before propagating the error.
                self.apply_pending_rw_writeback(code);
                return Err(e);
            }
        };
        if let Some(target) = lvalue_writeback_target
            && let Some(slot) = self.find_local_slot(code, &target)
            // Mirror the reverse pull's invariant: never clobber a live
            // `HashEntryRef` binding slot with a plain env copy.
            && !matches!(self.locals[slot], Value::HashEntryRef { .. })
            && let Some(val) = self.env().get(&target).cloned()
        {
            self.locals[slot] = val;
        }
        // Slice F: write any `is rw` parameter writeback through to the caller's
        // local slot (see `apply_pending_rw_writeback`).
        self.apply_pending_rw_writeback(code);
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
            if !self.has_proto(&name)
                && !self.has_multi_candidates_cached(&name)
                && let Some(def) = loan_env!(self, resolve_function_with_types(&name, &args))
                && if crate::runtime::Interpreter::is_builtin_function(&name) {
                    // Genuine builtin shadow: strict gate (no default param —
                    // name-cache pollution hazard, PR #3546).
                    Self::def_is_otf_compilable(&def)
                } else {
                    // Non-builtin module/dynamic single sub: defaults are
                    // name-cache-safe here (no builtin to mis-bind); interpreter-
                    // coupled bodies/signatures stay excluded.
                    Self::def_is_otf_compilable_module_single(&def)
                }
            {
                let result = self.compile_and_call_function_def(&def, args, compiled_fns)?;
                self.stack.push(result);
            } else {
                crate::vm::vm_stats::record_function_fallback(&name);
                let result = self.vm_call_function_fallback(&name, &args)?;
                let result = loan_env!(self, maybe_fetch_rw_proxy(result, true))?;
                self.stack.push(result);
            }
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
        } else {
            // Sync Interpreter locals to env before spawning threads so closures capture them
            if name == "start" {
                self.sync_env_from_locals(code);
            }
            let result = self.call_function_compiled_first(&name, args, compiled_fns)?;
            let result = loan_env!(self, maybe_fetch_rw_proxy(result, true))?;
            self.stack.push(result);
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
            Value::HashEntryRef { .. } => target.hash_entry_read(),
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
        self.apply_pending_rw_writeback(code);
        self.stack.push(result);
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
        self.apply_pending_rw_writeback(code);
        self.stack.push(result);
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
        if name == "__PROTO_DISPATCH__" {
            // `{*}` inside a compiled proto body (ledger §D): the proto-dispatch
            // marker rewritten by `rewrite_proto_dispatch_stmts`. Resolve and run
            // the winning multi candidate VM-natively (compiled bytecode) instead
            // of bouncing through interpreter `call_proto_dispatch` + `run_block`.
            return self.vm_call_proto_dispatch(code, compiled_fns);
        }
        if let Some(callable) = call_me_override {
            let result = self.try_compiled_method_or_interpret(callable, "CALL-ME", args);
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
                    && !Self::call_shares_container_into_named_scalar_param(
                        cf,
                        &args,
                        arg_sources.as_deref(),
                    )
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
                if self.has_proto(name)
                    && let Some(def) = self.vm_resolve_trivial_proto_candidate(name, &args)
                {
                    // VM-native proto dispatch (ledger §D, multi-dispatch VM-ization):
                    // a trivial-body proto (`proto foo {*}` / bodyless) resolves its
                    // winning multi candidate via the VM-owned registry (phase ②) and
                    // runs it as compiled bytecode, bypassing the tree-walk proto body
                    // + `__PROTO_DISPATCH__` round-trip and the candidate body's own
                    // `run_block`. Non-trivial proto bodies, unresolved/ambiguous
                    // candidates, and non-OTF-compilable candidates return None from
                    // the resolver above and fall through to the interpreter, which
                    // produces the proper dispatch result / X::Multi::NoMatch error.
                    // `nextsame`/`callsame`/`callwith`/`samewith` from the selected
                    // candidate still work because compile_and_call_function_def pushes
                    // the same multi-dispatch + samewith frames the interpreter would.
                    self.compile_and_call_function_def(&def, args, compiled_fns)
                } else if self.has_proto(name)
                    && let Some(result) =
                        self.vm_try_run_nontrivial_proto_body(name, args.clone(), compiled_fns)
                {
                    // VM-native non-trivial proto body (ledger §D): a proto with a
                    // real body (`proto foo($x) { say "x"; {*} }`) runs that body as
                    // compiled bytecode instead of tree-walking it. The `{*}` inside
                    // still redispatches to the winning multi candidate through the
                    // existing proto-dispatch handler. Non-OTF-eligible protos return
                    // None and fall through to the interpreter unchanged.
                    result
                } else if self.has_multi_candidates_cached(name) && !self.has_proto(name) {
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
                        // A genuine multi candidate: the name is multi-cached, so
                        // `compile_and_call_function_def` never name-caches this
                        // candidate — a default param is safe here (unlike the
                        // single/builtin-shadow paths). See
                        // `def_is_otf_compilable_multi_candidate`.
                        && Self::def_is_otf_compilable_multi_candidate(&def)
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
                    if !self.has_proto(name)
                        && !self.has_multi_candidates_cached(name)
                        && let Some(def) = loan_env!(self, resolve_function_with_types(name, &args))
                        && if crate::runtime::Interpreter::is_builtin_function(name) {
                            // Genuine builtin shadow: strict gate (no default —
                            // name-cache pollution hazard, PR #3546).
                            Self::def_is_otf_compilable(&def)
                        } else {
                            // Non-builtin module/dynamic single sub: defaults are
                            // name-cache-safe here (no builtin to mis-bind), but
                            // interpreter-coupled bodies/signatures stay excluded.
                            Self::def_is_otf_compilable_module_single(&def)
                        }
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
                } else if let Some(result) = self.try_native_json_function(name, &args) {
                    // Dispatch JSON::Fast / JSON::Tiny `to-json` / `from-json`
                    // to the native implementation (runtime/json.rs).
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
                } else if let Some(result) = self.try_native_io_function(name, &args) {
                    // File/FS builtin function (`slurp`/`open`/`unlink`/…). Every
                    // user-sub resolution path (compiled_fns / multi / user_function_
                    // matches / OTF) was tried above, so a user `sub slurp` still wins;
                    // reaching here means the builtin operating on the VM-owned
                    // io_handles store + filesystem. Dispatch it natively instead of
                    // recording a tree-walk fallback (§D state ownership ③, function
                    // forms). The `builtin_*` impls are exactly what call_function
                    // routes to (no arg-sources: FS routines have no rw params) =>
                    // byte-identical.
                    let result = result?;
                    loan_env!(self, maybe_fetch_rw_proxy(result, true))
                } else if let Some(op) = name
                    .strip_prefix("infix:<")
                    .and_then(|s| s.strip_suffix('>'))
                {
                    // Builtin operator-as-function `infix:<op>(...)` (what `&infix:<+>`,
                    // `[+]`, hyper and `reduce` lower to). Every user-defined operator
                    // path (compiled_fns / multi / user_function_matches / OTF) was
                    // tried above, so reaching here means the builtin operator —
                    // dispatch it straight to the native `call_infix_routine` handler
                    // instead of recording a tree-walk fallback. This mirrors
                    // `call_function_fallback`'s infix arm exactly (the big
                    // `call_function` match has no infix arm, so both reach the same
                    // `call_infix_routine` on the same `self`), with the same
                    // arg-sources + rw-proxy handling => byte-identical. §D state
                    // ownership: the operator handlers are native Rust on VM state.
                    let normalized = if op == "\u{2212}" { "-" } else { op };
                    self.set_pending_call_arg_sources(arg_sources);
                    let result = self.call_infix_routine(normalized, &args);
                    self.set_pending_call_arg_sources(None);
                    let result = result?;
                    loan_env!(self, maybe_fetch_rw_proxy(result, true))
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
    /// Resolve the winning multi candidate for a *trivial-body* proto so the VM
    /// can run it as compiled bytecode instead of falling back to the tree-walk
    /// proto body + `__PROTO_DISPATCH__` round-trip (ledger §D, multi-dispatch
    /// VM-ization). Returns `None` — leaving the existing interpreter fallback in
    /// place — whenever the bypass would not be byte-identical:
    ///
    /// - the name is interpreter-handled (e.g. a proto'd native Test routine);
    /// - the proto has a *non-trivial* body (statements around `{*}`), which must
    ///   still run;
    /// - no candidate resolves, or resolution is ambiguous (the interpreter then
    ///   raises the proper `X::Multi::NoMatch` / `X::Multi::Ambiguous`);
    /// - the winning candidate is not OTF-compilable, or declares `state` (whose
    ///   shared-cell identity the OTF body-fingerprint cache cannot preserve).
    pub(super) fn vm_resolve_trivial_proto_candidate(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<crate::ast::FunctionDef> {
        use crate::ast::{Expr, Stmt};
        if self.is_interpreter_handled_function(name) {
            return None;
        }
        let proto = self.resolve_proto_function(name)?;
        // A trivial proto body is empty (bodyless proto) or exactly `{*}` — only
        // then is bypassing the body safe. `{*}` parses to `Stmt::Expr(Whatever)`.
        // The compiler prepends line-tracking `SetLine` markers (no runtime
        // effect on dispatch), so ignore those when judging triviality.
        let significant: Vec<&Stmt> = proto
            .body
            .iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect();
        let trivial = significant.is_empty()
            || (significant.len() == 1 && matches!(significant[0], Stmt::Expr(Expr::Whatever)));
        if !trivial {
            return None;
        }
        // The proto's OWN signature is a gate: `proto f(Int $x) {*}` rejects a
        // `Str` arg even when a candidate (`multi f($)`) would accept it
        // (S06-multi/proto.t: "proto signature is checked"). Bypassing the body
        // would skip that check, so only proceed when the args satisfy the proto
        // signature; otherwise fall back so the interpreter raises the proper
        // X::TypeCheck::Argument. An empty proto signature accepts anything.
        if !proto.param_defs.is_empty() && !self.method_args_match(args, &proto.param_defs) {
            return None;
        }
        // Ambiguity is signalled by `None` + a pending dispatch error; clear any
        // stale one first (mirrors the non-proto multi fork) so a prior call's
        // ambiguity can't leak into this resolution.
        let _ = self.take_pending_dispatch_error();
        let def = self.resolve_proto_candidate_with_types(name, args)?;
        if def.empty_sig && !args.is_empty() {
            return None;
        }
        // A trivial-proto candidate is a genuine multi candidate (same caching
        // profile in `compile_and_call_function_def` regardless of defaults), so a
        // default param is safe to OTF here — see
        // `def_is_otf_compilable_multi_candidate`.
        if !Self::def_is_otf_compilable_multi_candidate(&def)
            || Self::function_body_declares_state(&def.body)
        {
            return None;
        }
        Some(def)
    }

    /// Run a *non-trivial-body* proto (`proto foo($x) { say "x"; {*} }`) as
    /// compiled bytecode instead of tree-walking its body through the interpreter
    /// (ledger §D, multi-dispatch VM-ization). The proto body is rewritten so each
    /// `{*}` becomes a `__PROTO_DISPATCH__()` call, then compiled and run like any
    /// routine: its `{*}` redispatch still resolves and runs the winning multi
    /// candidate through the existing proto-dispatch handler (reached from the
    /// compiled body's `__PROTO_DISPATCH__` call), so behaviour — including the
    /// candidate's `nextsame`/`callsame` — is byte-identical to the interpreter.
    ///
    /// Returns `None` (leaving the interpreter fallback in place) whenever the
    /// bypass would not be safe / byte-identical:
    /// - the name is interpreter-handled, or has no proto / a *trivial* body
    ///   (handled by `vm_resolve_trivial_proto_candidate` instead);
    /// - the args do not satisfy the proto's own signature (the interpreter then
    ///   raises the proper `X::TypeCheck::Argument`);
    /// - the proto's signature or (rewritten) body is not OTF-compilable, or the
    ///   body declares `state` (whose shared-cell identity the fingerprint cache
    ///   cannot preserve).
    pub(super) fn vm_try_run_nontrivial_proto_body(
        &mut self,
        name: &str,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Option<Result<Value, RuntimeError>> {
        use crate::ast::{Expr, Stmt};
        if self.is_interpreter_handled_function(name) {
            return None;
        }
        let proto = self.resolve_proto_function(name)?;
        // Only handle *non-trivial* bodies here; the trivial (bodyless / `{*}`-only)
        // case is the trivial resolver's job. A bodyless proto (`def.body.empty`)
        // dispatches implicitly and must not be compiled here.
        let significant: Vec<&Stmt> = proto
            .body
            .iter()
            .filter(|s| !matches!(s, Stmt::SetLine(_)))
            .collect();
        let trivial = significant.is_empty()
            || (significant.len() == 1 && matches!(significant[0], Stmt::Expr(Expr::Whatever)));
        if trivial {
            return None;
        }
        // The proto's OWN signature is a gate (same as the trivial path): bypassing
        // to compiled code must still reject args the proto signature forbids.
        if !proto.param_defs.is_empty() && !self.method_args_match(&args, &proto.param_defs) {
            return None;
        }
        // Rewrite `{*}` -> `__PROTO_DISPATCH__()` and require the resulting body +
        // the proto's own signature to be OTF-compilable. `state` in the proto body
        // would break the body-fingerprint state cache, so exclude it too.
        let rewritten = crate::runtime::Interpreter::rewrite_proto_dispatch_stmts(&proto.body);
        let mut proto_def = proto.clone();
        proto_def.body = rewritten;
        if !Self::def_is_otf_compilable(&proto_def)
            || Self::function_body_declares_state(&proto_def.body)
        {
            return None;
        }
        // Compile the proto body and run it like a routine. `{*}` redispatch reads
        // the args from `proto_dispatch_stack` (the ORIGINAL proto args, matching
        // the interpreter's `call_proto_function`), so push that before the body
        // runs and pop after. No multi-dispatch frame is pushed for the proto body
        // itself — it is the dispatcher, not a candidate; the candidate's own
        // `nextsame` frame is set up by the proto-dispatch handler when `{*}` runs.
        let cf = self.otf_compile_function_def(&proto_def);
        let pkg = proto_def.package.resolve();
        self.push_proto_dispatch_frame(name.to_string(), args.clone());
        let result = self.call_compiled_function_named(&cf, args, compiled_fns, &pkg, name);
        self.pop_proto_dispatch_frame();
        Some(result)
    }

    /// VM-native `{*}` redispatch (ledger §D, multi-dispatch VM-ization step ②③):
    /// reached when a compiled proto body executes its rewritten `__PROTO_DISPATCH__()`
    /// call. Resolves the winning multi candidate from the proto-dispatch frame's
    /// original args and runs it as compiled bytecode via
    /// `compile_and_call_function_def` — the same path the trivial-proto fork uses —
    /// so the candidate body and its `nextsame`/`callsame`/`samewith` redispatch all
    /// run VM-natively instead of tree-walking through interpreter
    /// `call_proto_dispatch` and `run_block`. (The `is rw` writeback *through* a
    /// non-trivial proto body is a separate pre-existing gap — the proto-dispatch
    /// frame carries the proto's original args, not the caller's containers — and is
    /// unchanged here; it fails identically on the interpreter path.)
    ///
    /// Falls back to the interpreter's `call_proto_dispatch` (which owns the full
    /// `X::Multi::NoMatch` / `X::Multi::Ambiguous` reporting, `proto method`
    /// invocant handling, and the tree-walk `run_block`) whenever a VM-native run
    /// would not be byte-identical: a `proto method` `{*}` (invocant context), no
    /// resolvable / ambiguous candidate, an empty-sig candidate called with args, or
    /// a non-OTF-compilable / `state`-declaring candidate.
    pub(super) fn vm_call_proto_dispatch(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        let Some((proto_name, args, method_ctx)) = self.proto_dispatch_last() else {
            // `{*}` outside a proto — let the interpreter raise the proper error.
            return self.loan_env_for(|i| i.call_proto_dispatch());
        };
        // `proto method` redispatch needs the invocant + one-shot `proto_method_skip`
        // bookkeeping the interpreter owns; only proto *subs* run compiled here.
        if method_ctx.is_some() {
            return self.loan_env_for(|i| i.call_proto_dispatch());
        }
        // Clear any stale pending dispatch error (mirrors the trivial-proto fork)
        // so a prior call's ambiguity can't leak into this resolution.
        let _ = self.take_pending_dispatch_error();
        // `{*}` rw-redispatch (ledger §D): when the proto declares a scalar
        // `is rw`/`is raw` parameter, Rakudo redispatches `{*}` using the proto's
        // CURRENT (body-mutated) parameter, so a candidate's own rw write chains
        // back through the proto parameter to the caller. Rebuild the args from
        // the proto's current parameter values (live body locals) and pass
        // arg_sources naming the proto params, so the candidate's writeback lands
        // in the proto frame and the proto's own rw binding propagates it to the
        // caller at proto exit. `None` => unchanged (the common non-rw case).
        let (args, rw_arg_sources) = match self
            .resolve_proto_function(&proto_name)
            .and_then(|proto| self.proto_rw_redispatch_args(&proto.param_defs, &args, Some(code)))
        {
            Some((rebuilt, sources)) => (rebuilt, Some(sources)),
            None => (args, None),
        };
        // For rw redispatch the rebuilt args are plain values; a candidate's
        // `is rw` param requires a *writable* argument, which the multi-dispatch
        // writability check satisfies from `pending_call_arg_sources` (a named
        // source is as good as a VarRef). Set it before resolving so the rw
        // candidate matches, and clear it again on any interpreter fallthrough.
        let had_rw_sources = rw_arg_sources.is_some();
        if had_rw_sources {
            self.set_pending_call_arg_sources(rw_arg_sources);
        }
        if let Some(def) = self.resolve_proto_candidate_with_types(&proto_name, &args)
            && (!def.empty_sig || args.is_empty())
            // A proto candidate is a genuine multi candidate: its caching profile
            // in `compile_and_call_function_def` is identical with or without a
            // default param (caching keys on `has_multi_candidates_cached`, not on
            // defaults), so default-bearing candidates are as safe to OTF here as
            // the non-default ones already are. Permit defaults (see
            // `def_is_otf_compilable_multi_candidate`).
            && Self::def_is_otf_compilable_multi_candidate(&def)
            && !Self::function_body_declares_state(&def.body)
        {
            // pending_call_arg_sources is still set (resolution only reads it);
            // `compile_and_call_function_def`'s bind consumes it for the rw chain.
            let result = self.compile_and_call_function_def(&def, args, compiled_fns);
            if had_rw_sources {
                self.set_pending_call_arg_sources(None);
            }
            return result;
        }
        // No candidate / ambiguous / empty-sig-with-args / non-OTF / state: the
        // interpreter re-resolves and produces the exact error or tree-walk result.
        if had_rw_sources {
            self.set_pending_call_arg_sources(None);
        }
        self.loan_env_for(|i| i.call_proto_dispatch())
    }

    /// `{*}` rw-redispatch helper (ledger §D, multi-dispatch VM-ization): Rakudo
    /// redispatches `{*}` using the proto's CURRENT parameter, so a candidate's
    /// own `is rw` write chains back through the (possibly body-mutated) proto
    /// parameter to the caller's container. mutsu instead passes the proto's
    /// entry-time args, so a candidate's rw write either targets the caller
    /// directly (colliding with the proto's own writeback) or is lost.
    ///
    /// When the proto declares a scalar `is rw`/`is raw` positional parameter and
    /// its signature is a simple all-positional one, return rebuilt args (read
    /// from the proto's current parameter values — the live body locals when
    /// `code` is given, else env) and arg_sources naming the proto params, so the
    /// candidate binds its rw param with `source = <proto param>` and its
    /// writeback lands in the proto frame. Returns `None` (no rebuild, current
    /// behavior) for protos without a scalar rw/raw param or with a non-simple
    /// signature.
    pub(crate) fn proto_rw_redispatch_args(
        &self,
        proto_param_defs: &[crate::ast::ParamDef],
        orig_args: &[Value],
        code: Option<&CompiledCode>,
    ) -> Option<(Vec<Value>, Vec<Option<String>>)> {
        // The fixed positional params: drop the invocant and the variadic /
        // named catch-alls (a `proto method` always carries an implicit `%_`),
        // so a simple positional signature still qualifies. We only rebuild when
        // these fixed params exactly consume the call's positional args.
        let positional: Vec<&crate::ast::ParamDef> = proto_param_defs
            .iter()
            .filter(|pd| {
                !pd.is_invocant
                    && !pd.named
                    && !pd.slurpy
                    && !pd.double_slurpy
                    && !pd.onearg
                    && pd.sub_signature.is_none()
                    && pd.name != "%_"
                    && pd.name != "@_"
            })
            .collect();
        // All args must be positional, and the fixed positional params must
        // exactly consume them (no slurpy mopping up extras).
        if positional.len() != orig_args.len()
            || orig_args.iter().any(|a| {
                matches!(
                    crate::runtime::types::unwrap_varref_value(a.clone()),
                    Value::Pair(..) | Value::ValuePair(..)
                )
            })
        {
            return None;
        }
        // A scalar param is stored sigil-less, so its name starts with an ASCII
        // letter / `_` (not `@`/`%`/`&`). Require at least one scalar rw/raw param
        // — the only case where the current value differs or a writeback link is
        // needed; `@`/`%` rw params propagate in-place by name already.
        let is_scalar_rw = |pd: &crate::ast::ParamDef| {
            pd.traits.iter().any(|t| t == "rw" || t == "raw")
                && pd.name != "_"
                && pd
                    .name
                    .as_bytes()
                    .first()
                    .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
        };
        if !positional.iter().any(|pd| is_scalar_rw(pd)) {
            return None;
        }
        let mut new_args = Vec::with_capacity(positional.len());
        let mut sources = Vec::with_capacity(positional.len());
        for (pd, orig) in positional.iter().zip(orig_args.iter()) {
            if is_scalar_rw(pd) {
                // Rebuild from the proto's CURRENT parameter value: a scalar rw
                // param is slot-only mid-body (not yet flushed to env), so read
                // the live body local slot first, then env, then the entry-time
                // arg as a last resort. arg_sources names the proto param so the
                // candidate's writeback chains back through it.
                let cur = code
                    .and_then(|c| c.locals.iter().position(|n| n == &pd.name))
                    .map(|slot| self.locals[slot].clone())
                    .or_else(|| self.env().get(&pd.name).cloned())
                    .unwrap_or_else(|| crate::runtime::types::unwrap_varref_value(orig.clone()));
                new_args.push(crate::runtime::types::unwrap_varref_value(cur));
                sources.push(Some(pd.name.clone()));
            } else {
                // Non-rw params keep their original argument (a VarRef / container
                // carries the caller's aliasing for `@`/`%`/`is raw`-by-name
                // params); rebuilding to a plain current value would sever it.
                new_args.push(orig.clone());
                sources.push(None);
            }
        }
        Some((new_args, sources))
    }

    pub(super) fn def_is_otf_compilable(def: &crate::ast::FunctionDef) -> bool {
        // `where` constraints are NOT excluded: the winning multi candidate is
        // resolved by `resolve_function_with_types` / `resolve_proto_candidate_with_types`,
        // which already evaluate `where` (via `args_match_param_types`) to pick
        // the winner, so the resolved def already satisfies its `where`. The
        // compiled binding path (`call_compiled_function_named` ->
        // `bind_function_args_values`) re-checks `where` and raises the same
        // `X::TypeCheck::Binding::Parameter` the interpreter would on failure
        // (for single candidates), and merges the `&name` Sub's captured env so
        // a `where` referencing closure variables resolves them — byte-identical
        // to the interpreter fallback (ledger §D, multi-dispatch VM-ization).
        //
        // A `&callback` parameter is also NOT excluded: it binds and is invoked
        // (`cb()`, `cb($x)`) exactly like any compiled local, including blocks,
        // `&name`-passed subs, and closures over outer lexicals. Only a `&cb` with
        // an explicit code signature (`&cb:(Int)`, `code_signature`) and a param
        // with a default value stay excluded (the former still has a separate
        // resolution-ambiguity gap; the latter is the deferred default-OTF case).
        !Self::function_body_needs_interpreter(&def.body)
            && def
                .param_defs
                .iter()
                .all(|pd| pd.default.is_none() && pd.code_signature.is_none())
    }

    /// Like `def_is_otf_compilable`, but also permits a parameter with a default
    /// value. Safe ONLY at a genuine *multi*-candidate dispatch site (the
    /// `has_multi_candidates_cached` branch): there `compile_and_call_function_def`
    /// does NOT name-cache the compiled candidate (the name is multi-cached, so
    /// its `!has_multi_candidates_cached` guard is false), so a default-bearing
    /// candidate cannot pollute the name-keyed `otf_call_cache` and mis-bind a
    /// later same-named call — the builtin-shadow hazard that deferred the blanket
    /// default-OTF (PR #3546: Test::Util's `our sub run(Str, Str = '')` shadowing
    /// the `run` builtin). The compiled binding (`bind_function_args_values`)
    /// evaluates defaults exactly as the interpreter does, so this is
    /// byte-identical (ledger §D, multi-dispatch VM-ization). (For non-builtin
    /// single module/dynamic subs, the name-cache is also safe — see
    /// `def_is_otf_compilable_module_single`, which carries extra body/signature
    /// gates those subs need.)
    pub(super) fn def_is_otf_compilable_multi_candidate(def: &crate::ast::FunctionDef) -> bool {
        !Self::function_body_needs_interpreter(&def.body)
            && def.param_defs.iter().all(|pd| pd.code_signature.is_none())
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

    /// Whether a *non-builtin* single module/dynamic sub (reached via the
    /// `user_function_matches_call` branch) is safe to OTF-compile to bytecode
    /// instead of tree-walking through `call_function_fallback`.
    ///
    /// A genuine builtin shadow is gated by `def_is_otf_compilable` (PR-2), but
    /// an ordinary module sub may contain interpreter-coupled constructs that
    /// `def_is_otf_compilable` does not catch, whose semantics are NOT preserved
    /// when the def is compiled standalone on-the-fly:
    ///   - a nested routine decl whose non-local control flow must target the
    ///     nested routine, not escape it (Test::Util's `is-deeply-junction`'s
    ///     nested `junction-guts` + `when`),
    ///   - a `state` variable shared across `start` threads (a routine's state
    ///     lives in a shared cell that the tree-walk path owns — t/concurrent-state-var),
    ///   - `EVAL`/`EVALFILE` with a `CALLER::` context, `subtest`, `CATCH`/
    ///     `CONTROL` handlers, phasers, `start` (all couple to the interpreter's
    ///     caller / test / dispatch context — Test::Util's `throws-like-any`),
    ///   - `is rw`/`is raw`/sigilless params whose alias writeback to the caller
    ///     must survive across an `EVAL` boundary (t/sigilless-params).
    ///
    /// Defaults ARE allowed (name-cache-safe: no same-named builtin to mis-bind,
    /// and a single candidate always resolves to this def). Bodies and
    /// signatures that pass this gate compile and run identically OTF vs
    /// precompiled (ledger §D, multi-dispatch VM-ization). Being too strict here
    /// is harmless — it just keeps the sub on the interpreter fallback.
    ///
    /// `is test-assertion` IS allowed: `call_compiled_function_named` pushes the
    /// test-assertion line context, so an assertion failing inside an OTF-compiled
    /// helper reports the same caller line the interpreter path would (whatever
    /// the parser stamped on the call's caller-line marker).
    pub(super) fn def_is_otf_compilable_module_single(def: &crate::ast::FunctionDef) -> bool {
        !def.is_rw && !def.is_raw
            // A return type — especially a coercion return (`--> Foo:D()`) or a
            // definite/subset constraint — drives extra return-time dispatch
            // (COERCE / type-check) that the standalone OTF compile does not
            // reproduce identically when several such subs coexist
            // (roast/S12-coercion/coercion-return.t). Keep them on the interpreter.
            && def.return_type.is_none()
            && def.param_defs.iter().all(|pd| {
                pd.code_signature.is_none()
                    && !pd.sigilless
                    && pd.sub_signature.is_none()
                    && pd.traits.is_empty()
            })
            && !Self::function_body_declares_state(&def.body)
            && !Self::module_otf_body_needs_interpreter(&def.body)
    }

    /// Recursively detect interpreter-coupled statements/expressions in a module
    /// sub body that block standalone OTF compilation (see
    /// `def_is_otf_compilable_module_single`). Conservative: any unrecognized
    /// nesting that could hide a risky construct keeps the sub on the
    /// interpreter, so a missed case only costs a fallback, never correctness.
    fn module_otf_body_needs_interpreter(body: &[crate::ast::Stmt]) -> bool {
        body.iter().any(Self::module_otf_stmt_needs_interpreter)
    }

    fn module_otf_stmt_needs_interpreter(stmt: &crate::ast::Stmt) -> bool {
        use crate::ast::Stmt;
        match stmt {
            // Nested decls, subtests, catch/control handlers, and phasers couple
            // to the interpreter's dispatch / caller / test context.
            Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::SubDecl { .. }
            | Stmt::ProtoDecl { .. }
            | Stmt::TokenDecl { .. }
            | Stmt::Subtest { .. }
            | Stmt::Catch(_)
            | Stmt::Control(_)
            | Stmt::Phaser { .. } => true,
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                Self::module_otf_body_needs_interpreter(then_branch)
                    || Self::module_otf_body_needs_interpreter(else_branch)
            }
            Stmt::While { body, .. }
            | Stmt::For { body, .. }
            | Stmt::Loop { body, .. }
            | Stmt::Given { body, .. }
            | Stmt::When { body, .. }
            | Stmt::Whenever { body, .. }
            | Stmt::React { body, .. } => Self::module_otf_body_needs_interpreter(body),
            Stmt::Block(body) | Stmt::SyntheticBlock(body) | Stmt::Default(body) => {
                Self::module_otf_body_needs_interpreter(body)
            }
            Stmt::Expr(e) => Self::module_otf_expr_needs_interpreter(e),
            _ => false,
        }
    }

    fn module_otf_expr_needs_interpreter(expr: &crate::ast::Expr) -> bool {
        use crate::ast::Expr;
        match expr {
            Expr::PhaserExpr { .. } | Expr::Once { .. } => true,
            Expr::DoStmt(stmt) => Self::module_otf_stmt_needs_interpreter(stmt),
            Expr::Block(body) => Self::module_otf_body_needs_interpreter(body),
            Expr::MethodCall { target, args, .. } => {
                Self::module_otf_expr_needs_interpreter(target)
                    || args.iter().any(Self::module_otf_expr_needs_interpreter)
            }
            Expr::Call { name, args } => {
                let n = name.resolve();
                // start spawns a thread; EVAL/EVALFILE run on a sub-Interpreter
                // with a CALLER:: context that the standalone OTF compile cannot
                // reconstruct.
                n == "start"
                    || n == "EVAL"
                    || n == "EVALFILE"
                    || args.iter().any(Self::module_otf_expr_needs_interpreter)
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
