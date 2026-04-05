use super::*;
use crate::symbol::Symbol;

impl VM {
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
        let cf = if let Some(cached) = self.otf_compile_cache.get(&fingerprint) {
            cached.clone()
        } else {
            let cc = {
                let mut compiler = crate::compiler::Compiler::new();
                if !pkg.is_empty() && pkg != "GLOBAL" {
                    compiler.set_current_package(pkg.to_string());
                }
                compiler.compile_routine_closure_body(&def.params, &def.param_defs, &def.body)
            };
            let cf = CompiledFunction {
                code: cc,
                params: def.params.clone(),
                param_defs: def.param_defs.clone(),
                return_type: def.return_type.clone(),
                fingerprint,
                empty_sig: def.empty_sig,
                is_rw: def.is_rw,
                is_raw: def.is_raw,
            };
            self.otf_compile_cache.insert(fingerprint, cf.clone());
            cf
        };

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
        let use_cache = !self.interpreter.has_multi_candidates(name);
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
        let found_key: Option<String>;
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
                        } else if pkg != "GLOBAL" {
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
                                    found_key = None;
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

        let mut callable_id: Option<u64> = None;
        if !fn_name.is_empty() {
            self.interpreter
                .push_routine(fn_package.to_string(), fn_name.to_string());
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
            if !fn_name.is_empty() {
                self.interpreter.pop_routine();
            }
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
                    if !fn_name.is_empty() {
                        self.interpreter.pop_routine();
                    }
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
        if !fn_name.is_empty() {
            self.interpreter.pop_routine();
        }
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
