use super::*;
use crate::runtime::types::unwrap_varref_value;
use crate::symbol::Symbol;

impl VM {
    /// Cached version of `interpreter.has_multi_candidates()`.
    /// Uses `fn_resolve_gen` for invalidation so it's O(1) on cache hit.
    pub(super) fn has_multi_candidates_cached(&mut self, name: &str) -> bool {
        if self.multi_candidates_cache_gen != self.fn_resolve_gen {
            self.multi_candidates_cache.clear();
            self.multi_candidates_cache_gen = self.fn_resolve_gen;
        }
        let sym = Symbol::intern(name);
        if let Some(&cached) = self.multi_candidates_cache.get(&sym) {
            return cached;
        }
        let result = self.interpreter.has_multi_candidates(name);
        self.multi_candidates_cache.insert(sym, result);
        result
    }

    /// Try compiled function dispatch first, then native, then on-the-fly compile,
    /// then interpreter fallback. Returns the result of whichever path succeeds.
    pub(super) fn call_function_compiled_first(
        &mut self,
        name: &str,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        if let Some(cf) = self.find_compiled_function(compiled_fns, name, &args) {
            let pkg = self.interpreter.current_package().to_string();
            return self.call_compiled_function_named(cf, args, compiled_fns, &pkg, name);
        }
        if let Some(native_result) =
            self.try_native_function(crate::symbol::Symbol::intern(name), &args)
        {
            return native_result;
        }
        // Try resolving the function definition and compiling on-the-fly.
        // Skip functions that need special interpreter handling.
        if !self.is_interpreter_handled_function(name)
            && let Some(def) = self.interpreter.resolve_function_with_types(name, &args)
        {
            return self.compile_and_call_function_def(&def, args, compiled_fns);
        }
        self.interpreter.call_function(name, args)
    }

    /// Compile a FunctionDef on-the-fly to bytecode and execute via the VM.
    /// This avoids the interpreter's tree-walking execution path.
    #[allow(dead_code)]
    pub(super) fn compile_and_call_function_def(
        &mut self,
        def: &crate::ast::FunctionDef,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        self.interpreter.check_deprecation_for_def(def);
        let name = def.name.resolve();
        let pkg = def.package.resolve();

        let fingerprint =
            crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);

        // Use cached compilation if available, otherwise compile on-the-fly.
        // Caching is essential to preserve state variable identity across calls.
        // The cache key includes the package name because compile-time
        // pseudo-variables like $?PACKAGE depend on the compilation context.
        let cache_key = {
            let mut hasher = std::hash::DefaultHasher::new();
            std::hash::Hash::hash(&fingerprint, &mut hasher);
            std::hash::Hash::hash(&pkg, &mut hasher);
            std::hash::Hasher::finish(&hasher)
        };
        let cf = if let Some(cached) = self.otf_compile_cache.get(&cache_key) {
            cached.clone()
        } else {
            let cc = {
                let mut compiler = crate::compiler::Compiler::new();
                if !pkg.is_empty() && pkg != "GLOBAL" {
                    compiler.set_current_package(pkg.to_string());
                }
                compiler.compile_routine_closure_body(&def.params, &def.param_defs, &def.body)
            };
            let mut cf = CompiledFunction {
                code: cc,
                params: def.params.clone(),
                param_defs: def.param_defs.clone(),
                return_type: def.return_type.clone(),
                fingerprint,
                empty_sig: def.empty_sig,
                is_rw: def.is_rw,
                is_raw: def.is_raw,
                param_local_slots: None,
                has_inner_subs: false,
                named_param_slots: None,
            };
            cf.precompute_param_local_slots();
            cf.precompute_named_param_slots();
            cf.detect_inner_subs();
            self.otf_compile_cache.insert(cache_key, cf.clone());
            cf
        };

        // Cache by name for fast lookup in exec_call_func_op
        let name_sym = Symbol::intern(&name);
        self.otf_call_cache.insert(name_sym, cf.clone());
        self.otf_call_cache_gen = self.fn_resolve_gen;

        // Set up samewith and multi-dispatch context that call_compiled_function_named
        // expects the caller to manage (mirrors exec_call_fn_op).
        self.interpreter.push_samewith_context(&name, None);
        let pushed_dispatch = self.interpreter.push_multi_dispatch_frame(&name, &args);

        let result = self.call_compiled_function_named(&cf, args, compiled_fns, &pkg, &name);

        self.interpreter.pop_samewith_context();
        if pushed_dispatch {
            self.interpreter.pop_multi_dispatch();
        }

        result
    }

    /// Check if a function name is handled by the interpreter's Rust code
    /// rather than by compiling its AST body. This includes test functions
    /// (implemented in runtime/test_functions.rs), internal `__mutsu_*` functions,
    /// and pseudo-package qualified names that need special resolution.
    pub(super) fn is_interpreter_handled_function(&self, name: &str) -> bool {
        // Test functions are implemented as Rust methods, not via AST
        if self.interpreter.test_mode_active()
            && crate::runtime::Interpreter::is_test_function_name(name)
        {
            return true;
        }
        // Internal functions are dispatched by the interpreter's call_function match
        if name.starts_with("__mutsu_") {
            return true;
        }
        // Pseudo-package qualified names need interpreter's special resolution
        // (SETTING::, OUTER::, CALLER::, etc.)
        if name.contains("SETTING::")
            || name.contains("OUTER::")
            || name.contains("CALLER::")
            || name.contains("DYNAMIC::")
        {
            return true;
        }
        false
    }

    pub(super) fn find_compiled_function<'a>(
        &mut self,
        compiled_fns: &'a HashMap<String, CompiledFunction>,
        name: &str,
        args: &[Value],
    ) -> Option<&'a CompiledFunction> {
        // Pseudo-package names need interpreter's special resolution
        if self.is_interpreter_handled_function(name) {
            return None;
        }
        self.find_compiled_function_inner(compiled_fns, name, args)
    }

    /// Get the cached package for a function, if available.
    pub(super) fn cached_fn_package(&self, name: &str, arity: usize) -> Option<String> {
        // Don't attempt type-based cache lookup for package; fall through to full resolution.
        // The cache key includes type signature which we don't have here.
        let _ = (name, arity);
        None
    }

    fn find_compiled_function_inner<'a>(
        &mut self,
        compiled_fns: &'a HashMap<String, CompiledFunction>,
        name: &str,
        args: &[Value],
    ) -> Option<&'a CompiledFunction> {
        let arity = args.len();
        let name_sym = Symbol::intern(name);
        // Build a type signature for cache key to handle multi dispatch correctly
        let type_sig: Vec<String> = args
            .iter()
            .map(|v| runtime::value_type_name(v).to_string())
            .collect();
        let cache_key = (name_sym, arity, type_sig.clone());
        // Check the resolution cache first to avoid expensive resolve_function_with_types.
        // Skip cache for multi functions since subset type dispatch depends on values.
        let use_cache = !self.has_multi_candidates_cached(name);
        if use_cache && self.fn_resolve_cache_gen == self.fn_resolve_gen {
            if let Some((cached_key, cached_fp, _)) = self.fn_resolve_cache.get(&cache_key)
                && let Some(cf) = compiled_fns.get(cached_key.as_str())
                && cf.fingerprint == *cached_fp
            {
                return Some(cf);
            }
        } else if self.fn_resolve_cache_gen != self.fn_resolve_gen {
            self.fn_resolve_cache.clear();
            self.fn_resolve_cache_gen = self.fn_resolve_gen;
        }
        let expected_fingerprint = self
            .interpreter
            .resolve_function_with_types(name, args)
            .map(|def| {
                crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body)
            });
        // If runtime resolution fails, avoid reusing stale compiled cache entries.
        // This can happen across repeated EVAL calls that redefine the same routine name.
        let expected_fingerprint = expected_fingerprint?;
        let matches_resolved = |cf: &CompiledFunction| cf.fingerprint == expected_fingerprint;
        let pkg = self.interpreter.current_package();
        let type_sig: Vec<String> = args
            .iter()
            .map(|v| runtime::value_type_name(v).to_string())
            .collect();
        // Try all key patterns and remember which one matched for caching
        let mut found_key: Option<String>;
        if name.contains("::") {
            let key_typed = format!("{name}/{arity}:{}", type_sig.join(","));
            if compiled_fns.get(&key_typed).is_some_and(&matches_resolved) {
                found_key = Some(key_typed);
            } else {
                let key_fp = format!("{name}/{}#{:x}", arity, expected_fingerprint);
                if compiled_fns.get(&key_fp).is_some_and(&matches_resolved) {
                    found_key = Some(key_fp);
                } else {
                    let key_arity = format!("{name}/{arity}");
                    if compiled_fns.get(&key_arity).is_some_and(&matches_resolved) {
                        found_key = Some(key_arity);
                    } else if compiled_fns.get(name).is_some_and(&matches_resolved) {
                        found_key = Some(name.to_string());
                    } else {
                        found_key = None;
                    }
                }
            }
            // If not found directly, try qualifying with the current package
            // when the prefix package is visible in the current scope.
            if found_key.is_none() && pkg != "GLOBAL" {
                let prefix_visible = if let Some((pkg_prefix, _)) = name.rsplit_once("::") {
                    self.interpreter.env().get(pkg_prefix).is_some()
                        || self
                            .interpreter
                            .env()
                            .get(&format!("{}::{}", pkg, pkg_prefix))
                            .is_some()
                } else {
                    false
                };
                if prefix_visible {
                    let qname = format!("{}::{}", pkg, name);
                    let key_typed = format!("{qname}/{arity}:{}", type_sig.join(","));
                    if compiled_fns.get(&key_typed).is_some_and(&matches_resolved) {
                        found_key = Some(key_typed);
                    } else {
                        let key_fp = format!("{qname}/{}#{:x}", arity, expected_fingerprint);
                        if compiled_fns.get(&key_fp).is_some_and(&matches_resolved) {
                            found_key = Some(key_fp);
                        } else {
                            let key_arity = format!("{qname}/{arity}");
                            if compiled_fns.get(&key_arity).is_some_and(&matches_resolved) {
                                found_key = Some(key_arity);
                            } else if compiled_fns.get(&qname).is_some_and(&matches_resolved) {
                                found_key = Some(qname);
                            }
                        }
                    }
                }
            }
        } else {
            let key_typed = format!("{}::{}/{}:{}", pkg, name, arity, type_sig.join(","));
            if compiled_fns.get(&key_typed).is_some_and(&matches_resolved) {
                found_key = Some(key_typed);
            } else {
                let key_fp = format!("{}::{}/{}#{:x}", pkg, name, arity, expected_fingerprint);
                if compiled_fns.get(&key_fp).is_some_and(&matches_resolved) {
                    found_key = Some(key_fp);
                } else {
                    let key_arity = format!("{}::{}/{}", pkg, name, arity);
                    if compiled_fns.get(&key_arity).is_some_and(&matches_resolved) {
                        found_key = Some(key_arity);
                    } else {
                        let key_simple = format!("{}::{}", pkg, name);
                        if compiled_fns.get(&key_simple).is_some_and(&matches_resolved) {
                            found_key = Some(key_simple);
                        } else {
                            // Try with positional-only arity (excluding Pair named args)
                            let pos_arity = args
                                .iter()
                                .filter(|a| !matches!(a, Value::Pair(..)))
                                .count();
                            if pos_arity != arity {
                                let key_pos_fp = format!(
                                    "{}::{}/{}#{:x}",
                                    pkg, name, pos_arity, expected_fingerprint
                                );
                                if compiled_fns.get(&key_pos_fp).is_some_and(&matches_resolved) {
                                    found_key = Some(key_pos_fp);
                                } else {
                                    found_key = None;
                                }
                            } else {
                                found_key = None;
                            }
                        }
                        if found_key.is_none() && pkg != "GLOBAL" {
                            let key_fp_global =
                                format!("GLOBAL::{}/{}#{:x}", name, arity, expected_fingerprint);
                            if compiled_fns
                                .get(&key_fp_global)
                                .is_some_and(&matches_resolved)
                            {
                                found_key = Some(key_fp_global);
                            } else {
                                let key_global = format!("GLOBAL::{}", name);
                                if compiled_fns.get(&key_global).is_some_and(&matches_resolved) {
                                    found_key = Some(key_global);
                                } else {
                                    // Try with positional-only arity (excluding Pair named args)
                                    let pos_arity = args
                                        .iter()
                                        .filter(|a| !matches!(a, Value::Pair(..)))
                                        .count();
                                    if pos_arity != arity {
                                        let key_pos = format!(
                                            "GLOBAL::{}/{}#{:x}",
                                            name, pos_arity, expected_fingerprint
                                        );
                                        if compiled_fns.get(&key_pos).is_some_and(&matches_resolved)
                                        {
                                            found_key = Some(key_pos);
                                        } else {
                                            found_key = None;
                                        }
                                    } else {
                                        found_key = None;
                                    }
                                }
                            }
                        } else {
                            found_key = None;
                        }
                    }
                }
            }
        }
        if let Some(key) = found_key {
            // Cache the resolution result for future lookups
            let cached_pkg = self
                .interpreter
                .resolve_function_with_types(name, args)
                .map(|def| def.package.resolve())
                .unwrap_or_else(|| self.interpreter.current_package().to_string());
            if use_cache {
                self.fn_resolve_cache
                    .insert(cache_key, (key.clone(), expected_fingerprint, cached_pkg));
            }
            compiled_fns.get(&key)
        } else {
            None
        }
    }

    /// Fast path for calling simple compiled functions.
    /// Eligible when: zero args, no params, no return type spec, not a test assertion,
    /// package is GLOBAL. Skips block_stack, routine_stack, caller_env, readonly vars,
    /// and callframe bookkeeping for significant performance gains in tight loops.
    ///
    /// Unlike `call_compiled_function_named`, this does NOT save/restore the env via
    /// Arc clone. Instead, it runs the function in-place and cleans up the function's
    /// local variables from env afterward. This avoids the expensive deep clone
    /// triggered by Arc::make_mut when the function body mutates env.
    pub(super) fn call_compiled_function_fast(
        &mut self,
        cf: &CompiledFunction,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        // Only save env when there are local variables to clean up.
        // When the function has no locals, the env save/restore is a
        // no-op (nothing to remove), but still causes an Arc clone that
        // raises the refcount and triggers O(env_size) deep clones on
        // any env write inside the function body (e.g. `$ = expr`).
        let has_locals = !cf.code.locals.is_empty();
        let saved_env = if has_locals {
            Some(self.interpreter.clone_env())
        } else {
            None
        };
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack_depth = self.stack.len();
        let saved_env_dirty = self.env_dirty;
        let saved_locals_dirty = self.locals_dirty;
        self.env_dirty = false;
        self.locals_dirty = false;

        // Raku: routines get their own $_ initialized to (Any).
        let saved_topic = if cf.code.is_routine {
            let old = self.interpreter.env().get("_").cloned();
            self.interpreter.env_mut().insert(
                "_".to_string(),
                Value::Package(crate::symbol::Symbol::intern("Any")),
            );
            old
        } else {
            None
        };

        // Reuse locals vec to avoid per-call allocation
        let num_locals = cf.code.locals.len();
        self.locals.clear();
        self.locals.resize(num_locals, Value::Nil);
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Load persisted state variable values
        for (slot, key) in &cf.code.state_locals {
            if let Some(val) = self.interpreter.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        let let_mark = self.interpreter.let_saves_len();
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        while ip < cf.code.ops.len() {
            match self.exec_one(&cf.code, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(e) if e.return_value.is_some() => {
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.interpreter.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
                    fail_bypass = true;
                    let failure = self.interpreter.fail_error_to_failure_value(&e);
                    self.interpreter.restore_let_saves(let_mark);
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    self.interpreter.restore_let_saves(let_mark);
                    result = Err(e);
                    break;
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        // Sync state variables back to persistent storage. Prefer the env
        // value over locals because methods like .push() mutate the Value
        // in the env in-place, and the locals copy may be stale.
        for (slot, key) in &cf.code.state_locals {
            let local_name = &cf.code.locals[*slot];
            let val = self
                .interpreter
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            self.interpreter.set_state_var(key.clone(), val);
        }

        // Restore state
        self.locals = saved_locals;
        self.env_dirty = saved_env_dirty;
        self.locals_dirty = saved_locals_dirty;

        // Restore env: if env was mutated, merge non-local changes back.
        // When has_locals is false, saved_env is None and no restore is needed
        // (functions without locals cannot leak local variables into the
        // caller's env — any env writes they do are intentional global state).
        if let Some(saved_env) = saved_env {
            if saved_env.ptr_eq(self.interpreter.env()) {
                // No env changes, nothing to merge
            } else {
                let local_names: std::collections::HashSet<&String> =
                    cf.code.locals.iter().collect();
                let mut restored_env = saved_env;
                for (k, v) in self.interpreter.env().iter() {
                    if !local_names.contains(k) {
                        restored_env.insert(k.clone(), v.clone());
                    }
                }
                *self.interpreter.env_mut() = restored_env;
            }
        }

        // Restore caller's $_ after routine call.
        if cf.code.is_routine {
            match saved_topic {
                Some(v) => {
                    self.interpreter.env_mut().insert("_".to_string(), v);
                }
                None => {
                    self.interpreter.env_mut().remove("_");
                }
            }
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

    /// Check if a compiled function is eligible for the fast call path.
    /// Returns true for simple functions that don't need the full call machinery.
    /// State variables are supported: both `state_locals` load/sync and anonymous
    /// state variable sync (via `sync_anon_state_value` at the opcode level) work
    /// correctly in the fast path.
    pub(super) fn is_fast_call_eligible(cf: &CompiledFunction, fn_name: &str) -> bool {
        cf.params.is_empty()
            && cf.param_defs.is_empty()
            && cf.return_type.is_none()
            && !fn_name.is_empty()
    }

    /// Check if a compiled function is eligible for the light call path.
    /// Light calls avoid the expensive env clone/restore cycle by directly
    /// binding parameters and restoring them after the call. This is safe
    /// for simple functions that don't need callframe/caller introspection,
    /// don't have where constraints, state variables, or return type checks.
    /// Check if a compiled function is eligible for the light call path.
    /// The light call skips the heavyweight env clone/restore, Sub value
    /// creation, block/routine push, and callable_id lookup. It does inline
    /// argument binding with minimal overhead.
    ///
    /// Currently restricted to functions where ALL param_defs are named
    /// (no positional params) to avoid correctness issues with positional
    /// argument binding edge cases (optional arrays, @_, VarRef unwrapping, etc.).
    /// Also excludes functions with legacy placeholder params (cf.params).
    pub(super) fn is_light_call_eligible(cf: &CompiledFunction, fn_name: &str) -> bool {
        !fn_name.is_empty()
            && cf.return_type.is_none()
            && cf.code.state_locals.is_empty()
            && !cf.is_rw
            && !cf.is_raw
            && !cf.empty_sig
            && !cf.param_defs.is_empty()
            && cf.param_defs.iter().all(|pd| {
                pd.named
                    && pd.where_constraint.is_none()
                    && !pd.slurpy
                    && !pd.double_slurpy
                    && pd.default.is_none()
                    && pd.type_constraint.is_none()
                    && pd.code_signature.is_none()
                    && !pd.sigilless
                    && !pd.is_invocant
                    && pd.traits.is_empty()
            })
    }

    /// Check if eligible for the positional light call path.
    /// This path avoids push_call_frame, Sub value creation, block/routine push,
    /// callable_id lookup, and full bind_function_args_values. Parameters are
    /// bound to pre-computed local slots and written to env.
    pub(super) fn is_positional_light_call_eligible(cf: &CompiledFunction, fn_name: &str) -> bool {
        !fn_name.is_empty()
            && cf.code.state_locals.is_empty()
            && !cf.is_rw
            && !cf.is_raw
            && !cf.empty_sig
            && cf.param_local_slots.is_some()
            // Exclude functions with inner closures/blocks (may use phasers, closures, etc.)
            && !cf.has_inner_subs
            // Only allow return types that light_return_type_check can handle
            && cf
                .return_type
                .as_deref()
                .is_none_or(Self::is_fast_type_name)
            && !cf.param_defs.is_empty()
            && cf.param_defs.iter().all(|pd| {
                !pd.named
                    && pd.where_constraint.is_none()
                    && !pd.slurpy
                    && !pd.double_slurpy
                    && pd.default.is_none()
                    && !pd.optional_marker
                    && pd.code_signature.is_none()
                    && !pd.sigilless
                    && !pd.is_invocant
                    && pd.traits.is_empty()
                    && pd.sub_signature.is_none()
                    // Only allow basic type constraints that fast_type_check handles.
                    // Excludes subset types, type captures (::T), parametric roles, etc.
                    && pd
                        .type_constraint
                        .as_deref()
                        .is_none_or(Self::is_fast_type_name)
                    // Exclude @/% params which need special collection semantics
                    // and & params which need callable binding
                    && !pd.name.starts_with('@')
                    && !pd.name.starts_with('%')
                    && !pd.name.starts_with('&')
                    // Exclude internal/anonymous params (__ANON_STATE__, __type_only__, etc.)
                    && !pd.name.starts_with("__")
                    // Exclude dynamic variable params ($*var)
                    && !pd.name.starts_with('*')
                    // Exclude $_ topic param (used implicitly by regex, ff, etc.)
                    && pd.name != "_"
            })
    }

    /// Ultra-fast call for simple positional-only functions (e.g. `sub fib($n)`).
    /// Avoids push_call_frame, Sub value creation, block/routine push,
    /// callable_id lookup, and full bind_function_args_values. Parameters are
    /// bound directly to pre-computed local slots AND written to env so that
    /// closures and dynamic lookups work correctly.
    pub(super) fn call_compiled_function_positional_light(
        &mut self,
        cf: &CompiledFunction,
        args: &[Value],
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        let param_slots = cf.param_local_slots.as_ref().unwrap();
        let positional_count = param_slots.len();
        let actual_count = args.len();

        if actual_count < positional_count {
            let required = cf
                .param_defs
                .iter()
                .filter(|pd| !pd.named && pd.required)
                .count();
            if actual_count < required {
                return Err(RuntimeError::new(format!(
                    "Too few positionals passed; expected {} arguments but got {}",
                    positional_count, actual_count
                )));
            }
        }

        let saved_locals = std::mem::take(&mut self.locals);
        let saved_locals_dirty = self.locals_dirty;
        let saved_env_dirty = self.env_dirty;
        self.locals_dirty = false;
        self.env_dirty = false;

        let num_locals = cf.code.locals.len();
        self.locals.clear();
        self.locals.resize(num_locals, Value::Nil);

        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }

        // Save old env values for params so we can restore them later.
        // Also write param values to both locals and env.
        let mut saved_param_env: Vec<(String, Option<Value>)> =
            Vec::with_capacity(positional_count);
        // Save and restore readonly vars
        let saved_readonly = self.interpreter.save_readonly_vars();
        for (param_idx, slot) in param_slots.iter().enumerate() {
            if param_idx < actual_count {
                let val = crate::runtime::types::unwrap_varref_value(args[param_idx].clone());
                // Type check param if constraint exists
                if let Some(ref tc) = cf.param_defs[param_idx].type_constraint
                    && !Self::fast_type_check(&val, tc)
                {
                    self.interpreter.restore_readonly_vars(saved_readonly);
                    for (name, old_val) in saved_param_env {
                        match old_val {
                            Some(v) => {
                                self.interpreter.env_mut().insert(name, v);
                            }
                            None => {
                                self.interpreter.env_mut().remove(&name);
                            }
                        }
                    }
                    self.locals = saved_locals;
                    self.locals_dirty = saved_locals_dirty;
                    self.env_dirty = saved_env_dirty;
                    return Err(RuntimeError::new(format!(
                        "Type check failed in binding ${}: expected {}, got {}",
                        cf.param_defs[param_idx].name,
                        tc,
                        runtime::value_type_name(&val)
                    )));
                }
                self.locals[*slot] = val.clone();
                let param_name = &cf.param_defs[param_idx].name;
                saved_param_env.push((
                    param_name.clone(),
                    self.interpreter.env().get(param_name).cloned(),
                ));
                self.interpreter.env_mut().insert(param_name.clone(), val);
                // Mark params as readonly by default (Raku semantics)
                self.interpreter.mark_readonly(param_name);
            }
        }

        let saved_stack_depth = self.stack.len();
        let let_mark = self.interpreter.let_saves_len();

        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        while ip < cf.code.ops.len() {
            match self.exec_one(&cf.code, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(e) if e.return_value.is_some() => {
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.interpreter.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
                    fail_bypass = true;
                    let failure = self.interpreter.fail_error_to_failure_value(&e);
                    self.interpreter.restore_let_saves(let_mark);
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    self.interpreter.restore_let_saves(let_mark);
                    result = Err(e);
                    break;
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        // Return type check (if specified). Allows type objects, Nil, and Failure through.
        if result.is_ok()
            && let Some(ref rt) = cf.return_type
        {
            let check_val = explicit_return.as_ref().unwrap_or(&ret_val);
            if !Self::light_return_type_check(check_val, rt) {
                self.locals = saved_locals;
                self.locals_dirty = saved_locals_dirty;
                self.env_dirty = saved_env_dirty;
                self.interpreter.restore_readonly_vars(saved_readonly);
                for (name, old_val) in saved_param_env {
                    match old_val {
                        Some(v) => {
                            self.interpreter.env_mut().insert(name, v);
                        }
                        None => {
                            self.interpreter.env_mut().remove(&name);
                        }
                    }
                }
                return Err(RuntimeError::new(format!(
                    "Type check failed for return value; expected {}, got {}",
                    rt,
                    runtime::value_type_name(check_val)
                )));
            }
        }

        self.locals = saved_locals;
        self.locals_dirty = saved_locals_dirty;
        self.env_dirty = saved_env_dirty;

        // Restore readonly vars
        self.interpreter.restore_readonly_vars(saved_readonly);
        // Restore env param values
        for (name, old_val) in saved_param_env {
            match old_val {
                Some(v) => {
                    self.interpreter.env_mut().insert(name, v);
                }
                None => {
                    self.interpreter.env_mut().remove(&name);
                }
            }
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

    /// Check if a type name is one of the basic types that fast_type_check handles.
    fn is_fast_type_name(name: &str) -> bool {
        matches!(
            name,
            "Int" | "Str" | "Num" | "Bool" | "Rat" | "Any" | "Mu" | "Cool"
        )
    }

    /// Return type check that handles type objects, Nil, and Failure passthrough.
    fn light_return_type_check(val: &Value, type_name: &str) -> bool {
        // Nil and Failure always pass return type checks
        if matches!(val, Value::Nil) {
            return true;
        }
        if let Value::Instance { class_name, .. } = val
            && class_name.resolve() == "Failure"
        {
            return true;
        }
        // Type objects (Package values) that match the return type pass
        if let Value::Package(sym) = val {
            return sym.resolve() == type_name;
        }
        Self::fast_type_check(val, type_name)
    }

    /// Fast type check for common types.
    fn fast_type_check(val: &Value, type_name: &str) -> bool {
        match type_name {
            "Int" => matches!(val, Value::Int(_) | Value::BigInt(_)),
            "Str" => matches!(val, Value::Str(_)),
            "Num" => matches!(val, Value::Num(_)),
            "Bool" => matches!(val, Value::Bool(_)),
            "Rat" => matches!(val, Value::Rat(_, _)),
            "Any" | "Mu" | "Cool" => true,
            _ => {
                let actual = runtime::value_type_name(val);
                actual == type_name
            }
        }
    }

    /// Lightweight compiled function call that avoids the heavyweight frame
    /// management (push_call_frame/env clone, Sub value creation, block/routine
    /// push, callable_id lookup). Binds parameters directly to locals slots
    /// without touching the env HashMap, maximizing performance for hot loops.
    pub(super) fn call_compiled_function_light(
        &mut self,
        cf: &CompiledFunction,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        // Save caller locals and create callee locals
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_locals_dirty = self.locals_dirty;
        let saved_env_dirty = self.env_dirty;
        self.locals_dirty = false;
        self.env_dirty = false;

        let num_locals = cf.code.locals.len();
        self.locals = vec![Value::Nil; num_locals];

        // Track which env keys we modify so we can restore them.
        let mut modified_env_keys: Vec<(String, Option<Value>)> =
            Vec::with_capacity(cf.param_defs.len());

        // Bind parameters directly to locals slots and env.
        let mut positional_idx = 0usize;
        for pd in &cf.param_defs {
            let param_name = &pd.name;
            let value = if pd.named {
                // Named parameter: search args for matching Pair
                let match_key = pd
                    .name
                    .strip_prefix('@')
                    .or_else(|| pd.name.strip_prefix('%'))
                    .unwrap_or(&pd.name);
                let match_key = match_key
                    .strip_prefix('!')
                    .or_else(|| match_key.strip_prefix('.'))
                    .unwrap_or(match_key);

                let mut found_val: Option<Value> = None;

                // Try matching the param name directly
                for arg in args.iter().rev() {
                    let arg = unwrap_varref_value(arg.clone());
                    if let Value::Pair(key, val) = arg
                        && key == match_key
                    {
                        found_val = Some(*val.clone());
                        break;
                    }
                }

                // Try alias matching via sub_signature (e.g. :color(:$colour))
                if found_val.is_none()
                    && let Some(ref sub_params) = pd.sub_signature
                {
                    for sub_pd in sub_params {
                        if found_val.is_some() {
                            break;
                        }
                        if !sub_pd.named {
                            continue;
                        }
                        let inner_key = sub_pd.name.strip_prefix(':').unwrap_or(&sub_pd.name);
                        for arg in args.iter().rev() {
                            let arg = unwrap_varref_value(arg.clone());
                            if let Value::Pair(key, val) = arg
                                && key == inner_key
                            {
                                found_val = Some(*val.clone());
                                break;
                            }
                        }
                    }
                }
                // Try outer_sub_signature aliases
                if found_val.is_none()
                    && let Some(ref outer) = pd.outer_sub_signature
                {
                    for outer_pd in outer {
                        if found_val.is_some() {
                            break;
                        }
                        let outer_name = outer_pd
                            .name
                            .trim_start_matches(|c: char| "$@%&".contains(c));
                        for arg in args.iter().rev() {
                            let arg = unwrap_varref_value(arg.clone());
                            if let Value::Pair(key, val) = arg
                                && key == outer_name
                            {
                                found_val = Some(*val.clone());
                                break;
                            }
                        }
                    }
                }

                if let Some(v) = found_val {
                    // If there's a sub_signature (rename), also bind inner params.
                    // e.g. :color(:$colour) — bind both "color" and "colour".
                    if let Some(ref sub_params) = pd.sub_signature {
                        for sub_pd in sub_params {
                            let sub_name = &sub_pd.name;
                            if let Some(slot) = cf.code.locals.iter().position(|n| n == sub_name) {
                                self.locals[slot] = v.clone();
                            }
                            modified_env_keys.push((
                                sub_name.clone(),
                                self.interpreter.env().get(sub_name).cloned(),
                            ));
                            self.interpreter
                                .env_mut()
                                .insert(sub_name.clone(), v.clone());
                        }
                    }
                    Some(v)
                } else if pd.required {
                    self.locals = saved_locals;
                    self.locals_dirty = saved_locals_dirty;
                    self.env_dirty = saved_env_dirty;
                    return Err(RuntimeError::new(format!(
                        "Required named parameter '{}' not passed",
                        param_name
                    )));
                } else {
                    None
                }
            } else {
                // Positional parameter: skip Pair args (they are named)
                while positional_idx < args.len() {
                    let unwrapped = unwrap_varref_value(args[positional_idx].clone());
                    if !matches!(&unwrapped, Value::Pair(..)) {
                        break;
                    }
                    positional_idx += 1;
                }
                if positional_idx < args.len() {
                    let val = unwrap_varref_value(args[positional_idx].clone());
                    positional_idx += 1;
                    Some(val)
                } else if pd.required {
                    self.locals = saved_locals;
                    self.locals_dirty = saved_locals_dirty;
                    self.env_dirty = saved_env_dirty;
                    return Err(RuntimeError::new(format!(
                        "Too few positionals passed; expected {} arguments but got {}",
                        cf.param_defs.iter().filter(|p| !p.named).count(),
                        args.iter()
                            .filter(|a| !matches!(a, Value::Pair(..)))
                            .count()
                    )));
                } else {
                    None
                }
            };

            // Set both locals slot and env for the param.
            // Locals are needed for GetLocal (fast path), env is needed for
            // closures that capture the variable and for GetLocal's Nil
            // fallback check (which errors on undeclared variables).
            let bound_val = value.unwrap_or(Value::Nil);
            if let Some(slot) = cf.code.locals.iter().position(|n| n == param_name) {
                self.locals[slot] = bound_val.clone();
            }
            modified_env_keys.push((
                param_name.clone(),
                self.interpreter.env().get(param_name).cloned(),
            ));
            self.interpreter
                .env_mut()
                .insert(param_name.clone(), bound_val);
        }

        // Mark parameters as readonly (by default, params are immutable in Raku).
        // Save existing readonly state so we can restore it after the call.
        let saved_readonly = self.interpreter.save_readonly_vars();
        for pd in &cf.param_defs {
            if !pd.name.is_empty()
                && !pd.sigilless
                && !pd.name.starts_with('!')
                && !pd.name.starts_with('.')
            {
                let has_mutable_trait = pd
                    .traits
                    .iter()
                    .any(|t| t == "rw" || t == "copy" || t == "raw");
                if !has_mutable_trait {
                    self.interpreter.mark_readonly(&pd.name);
                }
            }
        }

        // Set @_ only if the function body uses it (has @_ in locals)
        if cf.code.locals.iter().any(|n| n == "@_") {
            let plain_args: Vec<Value> = args
                .iter()
                .filter(|a| !matches!(unwrap_varref_value((*a).clone()), Value::Pair(..)))
                .map(|a| unwrap_varref_value(a.clone()))
                .collect();
            modified_env_keys.push(("@_".to_string(), self.interpreter.env().get("@_").cloned()));
            self.interpreter
                .env_mut()
                .insert("@_".to_string(), Value::array(plain_args));
        }

        // Handle legacy placeholder params (e.g. $^a, $^b from cf.params)
        if cf.param_defs.is_empty() && !cf.params.is_empty() {
            let mut placeholder_pos = 0usize;
            let plain_args: Vec<Value> = args
                .iter()
                .filter(|a| !matches!(unwrap_varref_value((*a).clone()), Value::Pair(..)))
                .map(|a| unwrap_varref_value(a.clone()))
                .collect();
            for param in &cf.params {
                if placeholder_pos < plain_args.len() {
                    let val = plain_args[placeholder_pos].clone();
                    placeholder_pos += 1;
                    // Set in locals
                    if let Some(slot) = cf.code.locals.iter().position(|n| n == param) {
                        self.locals[slot] = val.clone();
                    }
                    // Also set in env for the placeholder and its de-careted alias
                    modified_env_keys
                        .push((param.clone(), self.interpreter.env().get(param).cloned()));
                    self.interpreter
                        .env_mut()
                        .insert(param.clone(), val.clone());
                    // Create de-careted alias: ^foo -> foo, ^k -> k
                    if let Some(bare) = param.strip_prefix('^') {
                        let bare = bare.to_string();
                        if let Some(slot) = cf.code.locals.iter().position(|n| *n == bare) {
                            self.locals[slot] = val.clone();
                        }
                        modified_env_keys
                            .push((bare.clone(), self.interpreter.env().get(&bare).cloned()));
                        self.interpreter.env_mut().insert(bare, val);
                    }
                }
            }
        }

        // For any locals not yet set from params, try to initialize from env
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if matches!(self.locals[i], Value::Nil)
                && let Some(val) = self.interpreter.env().get(local_name)
            {
                self.locals[i] = val.clone();
            }
        }

        let saved_stack_depth = self.stack.len();
        let let_mark = self.interpreter.let_saves_len();

        // Execute the function body
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        while ip < cf.code.ops.len() {
            match self.exec_one(&cf.code, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(e) if e.return_value.is_some() => {
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.interpreter.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
                    fail_bypass = true;
                    let failure = self.interpreter.fail_error_to_failure_value(&e);
                    self.interpreter.restore_let_saves(let_mark);
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    self.interpreter.restore_let_saves(let_mark);
                    result = Err(e);
                    break;
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        // Restore locals
        self.locals = saved_locals;
        self.locals_dirty = saved_locals_dirty;
        self.env_dirty = saved_env_dirty;

        // Restore readonly vars
        self.interpreter.restore_readonly_vars(saved_readonly);

        // Restore modified env keys
        for (name, old_val) in modified_env_keys {
            match old_val {
                Some(v) => {
                    self.interpreter.env_mut().insert(name, v);
                }
                None => {
                    self.interpreter.env_mut().remove(&name);
                }
            }
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

    pub(super) fn call_compiled_function_named(
        &mut self,
        cf: &CompiledFunction,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
        fn_package: &str,
        fn_name: &str,
    ) -> Result<Value, RuntimeError> {
        let (args, callsite_line) = self.interpreter.sanitize_call_args(&args);
        if callsite_line.is_some() {
            self.interpreter.set_pending_callsite_line(callsite_line);
        }
        // Inject callsite line BEFORE push_call_frame so the parent env
        // contains the updated ?LINE. This avoids triggering Arc::make_mut
        // deep clone after the env Arc is shared with the call frame.
        self.interpreter.inject_pending_callsite_line();
        self.push_call_frame();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;
        let return_spec = cf.return_type.clone();

        self.interpreter.push_caller_env();

        // Push Sub value to block_stack for callframe().code
        let sub_val = Value::make_sub(
            Symbol::intern(fn_package),
            Symbol::intern(fn_name),
            cf.params.clone(),
            cf.param_defs.clone(),
            vec![],
            false,
            self.interpreter.env().clone(),
        );
        self.interpreter.push_block(sub_val);

        // Always push a routine frame so that &?ROUTINE works inside anonymous
        // subs too. Use "<anon>" as a sentinel name when fn_name is empty.
        let routine_push_name = if fn_name.is_empty() {
            "<anon>".to_string()
        } else {
            fn_name.to_string()
        };
        self.interpreter.push_routine_with_location(
            fn_package.to_string(),
            routine_push_name,
            self.current_source_line(),
            self.current_source_file(),
        );
        let mut callable_id: Option<u64> = None;
        if !fn_name.is_empty() {
            let callable_key = format!("__mutsu_callable_id::{fn_package}::{fn_name}");
            let resolved_callable_id = self
                .interpreter
                .env()
                .get(&callable_key)
                .and_then(|v| match v {
                    Value::Int(i) => Some(*i),
                    _ => None,
                })
                .unwrap_or(0);
            callable_id = (resolved_callable_id != 0).then_some(resolved_callable_id as u64);
            // Only insert __mutsu_callable_id when non-zero; readers handle
            // the missing/None case correctly. This avoids triggering
            // Arc::make_mut deep clone on the CoW env for simple functions.
            if resolved_callable_id != 0 {
                self.interpreter.env_mut().insert(
                    "__mutsu_callable_id".to_string(),
                    Value::Int(resolved_callable_id),
                );
            }
        }
        let is_test_assertion = if fn_name.is_empty() {
            false
        } else {
            self.interpreter
                .routine_is_test_assertion_by_name(fn_name, &args)
        };
        let pushed_assertion = self
            .interpreter
            .push_test_assertion_context(is_test_assertion);

        if cf.empty_sig && !args.is_empty() {
            self.interpreter.pop_routine();
            self.interpreter
                .pop_test_assertion_context(pushed_assertion);
            self.interpreter.pop_caller_env();
            self.stack.truncate(saved_stack_depth);
            let frame = self.pop_call_frame();
            drop(frame);
            return Err(Interpreter::reject_args_for_empty_sig(&args));
        }

        // Set current_package to the function's defining package so that default
        // value expressions can resolve package-scoped functions (e.g. &double).
        let saved_package = self.interpreter.current_package().to_string();
        if !fn_package.is_empty() && fn_package != "GLOBAL" {
            self.interpreter.set_current_package(fn_package.to_string());
        }
        // When the function has where constraints and there is a &name Sub in
        // env (which carries closure env), merge the Sub's captured variables
        // into the current env so where-constraint expressions can access them.
        if !fn_name.is_empty() && cf.param_defs.iter().any(|pd| pd.where_constraint.is_some()) {
            let ampname = format!("&{}", fn_name);
            if let Some(Value::Sub(ref sub_data)) = self.interpreter.env().get(&ampname).cloned() {
                for (k, v) in &sub_data.env {
                    // Skip internal variables, parameters, and sigiled variables
                    // that belong to the calling scope. Only merge simple lexical
                    // variables that the where constraint might reference.
                    if !k.starts_with("__mutsu_")
                        && !k.starts_with("?")
                        && !k.starts_with("!")
                        && k != "_"
                        && k != "@_"
                        && k != "%_"
                    {
                        self.interpreter.env_mut().insert(k.clone(), v.clone());
                    }
                }
            }
        }
        // Skip bind_function_args_values for 0-arg functions with no params,
        // avoiding the @_ env insert that triggers Arc::make_mut deep clone.
        let rw_bindings = if args.is_empty() && cf.param_defs.is_empty() && cf.params.is_empty() {
            vec![]
        } else {
            match self
                .interpreter
                .bind_function_args_values(&cf.param_defs, &cf.params, &args)
            {
                Ok(bindings) => bindings,
                Err(e) => {
                    self.interpreter.set_current_package(saved_package);
                    self.interpreter.pop_routine();
                    self.interpreter
                        .pop_test_assertion_context(pushed_assertion);
                    self.interpreter.pop_caller_env();
                    self.stack.truncate(saved_stack_depth);
                    let frame = self.pop_call_frame();
                    *self.interpreter.env_mut() = frame.saved_env;
                    return Err(Interpreter::enhance_binding_error(
                        e,
                        fn_name,
                        &cf.param_defs,
                        &args,
                    ));
                }
            }
        };
        self.interpreter
            .prepare_definite_return_slot(return_spec.as_deref());

        // Raku: $! is scoped per routine — fresh Nil on entry.
        // Only insert if $! isn't already Nil, to avoid triggering
        // Arc::make_mut deep clone on the CoW env.
        if !fn_name.is_empty() {
            let needs_reset = self
                .interpreter
                .env()
                .get("!")
                .is_some_and(|v| !matches!(v, Value::Nil));
            if needs_reset {
                self.interpreter
                    .env_mut()
                    .insert("!".to_string(), Value::Nil);
            }
        }

        // Raku: routines get their own $_ initialized to (Any).
        if cf.code.is_routine && !cf.param_defs.iter().any(|pd| pd.name == "_") {
            self.interpreter.env_mut().insert(
                "_".to_string(),
                Value::Package(crate::symbol::Symbol::intern("Any")),
            );
        }

        self.locals = vec![Value::Nil; cf.code.locals.len()];
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Load persisted state variable values
        for (slot, key) in &cf.code.state_locals {
            if let Some(val) = self.interpreter.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        let let_mark = self.interpreter.let_saves_len();
        let mut ip = 0;
        let mut result = Ok(());
        let mut explicit_return: Option<Value> = None;
        let mut fail_bypass = false;
        while ip < cf.code.ops.len() {
            match self.exec_one(&cf.code, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(mut e) if e.is_leave => {
                    let routine_key = format!("{fn_package}::{fn_name}");
                    let matches_frame = if let Some(target_id) = e.leave_callable_id {
                        Some(target_id) == callable_id
                    } else if let Some(target_routine) = e.leave_routine.as_ref() {
                        target_routine == &routine_key
                    } else {
                        e.label.is_none()
                    };
                    if matches_frame {
                        e.is_leave = false;
                        e.is_last = false;
                        let ret_val = e.return_value.unwrap_or(Value::Nil);
                        explicit_return = Some(ret_val.clone());
                        self.stack.truncate(saved_stack_depth);
                        self.stack.push(ret_val);
                        self.interpreter.discard_let_saves(let_mark);
                        result = Ok(());
                        break;
                    }
                    self.interpreter.restore_let_saves(let_mark);
                    result = Err(e);
                    break;
                }
                Err(e) if e.return_value.is_some() => {
                    // Non-local return: if the signal targets a specific callable,
                    // only catch it if this routine is the target.
                    if let Some(target_id) = e.return_target_callable_id
                        && callable_id != Some(target_id)
                    {
                        self.interpreter.restore_let_saves(let_mark);
                        result = Err(e);
                        break;
                    }
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.interpreter.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
                    // fail() — restore let saves and return a Failure value
                    fail_bypass = true;
                    let failure = self.interpreter.fail_error_to_failure_value(&e);
                    self.interpreter.restore_let_saves(let_mark);
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(failure);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    self.interpreter.restore_let_saves(let_mark);
                    result = Err(e);
                    break;
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }

        let mut ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        };
        // Raku semantics: `sub foo(...) { ... }` as the last statement
        // of a block returns the Sub. If the return value is Nil/Any and
        // the last opcode was RegisterSub, create the Sub value.
        if result.is_ok()
            && (matches!(ret_val, Value::Nil)
                || matches!(&ret_val, Value::Package(n) if n.resolve() == "Any"))
            && let Some(crate::opcode::OpCode::RegisterSub(idx)) = cf.code.ops.last()
            && let crate::ast::Stmt::SubDecl {
                name: sub_name,
                params,
                param_defs,
                body,
                is_rw,
                ..
            } = &cf.code.stmt_pool[*idx as usize]
        {
            ret_val = Value::make_sub(
                Symbol::intern(self.interpreter.current_package()),
                *sub_name,
                params.clone(),
                param_defs.clone(),
                body.clone(),
                *is_rw,
                self.interpreter.env().clone(),
            );
        }

        self.stack.truncate(saved_stack_depth);

        // Sync state variables back to persistent storage.
        // Read from env first (methods like push update env directly),
        // falling back to locals.
        for (slot, key) in &cf.code.state_locals {
            let local_name = &cf.code.locals[*slot];
            let val = self
                .interpreter
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            self.interpreter.set_state_var(key.clone(), val);
        }

        self.interpreter.set_current_package(saved_package);
        self.interpreter.pop_routine();
        self.interpreter
            .pop_test_assertion_context(pushed_assertion);
        self.interpreter.pop_block();
        let effective_return_spec = return_spec
            .as_deref()
            .map(|spec| self.interpreter.resolved_type_capture_name(spec));

        let frame = self.pop_call_frame();
        let restored_env = frame.saved_env;
        // Fast path: if the env wasn't mutated during the call (Arc still shared),
        // we can skip the expensive env merge and just restore directly.
        if restored_env.ptr_eq(self.interpreter.env()) {
            self.interpreter.pop_caller_env();
        } else {
            let mut restored_env = restored_env;
            self.interpreter
                .pop_caller_env_with_writeback(&mut restored_env);
            self.interpreter
                .apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
            let rw_sources: std::collections::HashSet<String> = rw_bindings
                .iter()
                .map(|(_, source)| source.clone())
                .collect();
            let local_names: std::collections::HashSet<&String> = cf.code.locals.iter().collect();
            for (k, v) in self.interpreter.env().iter() {
                if k == "_" || k == "@_" || k == "%_" {
                    continue;
                }
                if restored_env.contains_key(k)
                    && !local_names.contains(k)
                    && !rw_sources.contains(k)
                {
                    restored_env.insert(k.clone(), v.clone());
                }
            }
            *self.interpreter.env_mut() = restored_env;
        }

        match result {
            Ok(()) if fail_bypass => Ok(ret_val),
            Ok(()) => {
                let base_result = if let Some(v) = explicit_return {
                    let mut e = RuntimeError::new("return");
                    e.return_value = Some(v);
                    Err(e)
                } else {
                    Ok(ret_val)
                };
                self.interpreter
                    .finalize_return_with_spec(base_result, effective_return_spec.as_deref())
            }
            Err(e) => Err(e),
        }
    }
}
