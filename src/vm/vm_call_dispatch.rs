use super::*;
use crate::runtime::types::unwrap_varref_value;
use crate::symbol::Symbol;

impl Interpreter {
    /// Record a deprecation event for a compiled function if it has deprecation info.
    fn record_cf_deprecation(&self, cf: &CompiledFunction) {
        if let Some((ref kind, ref name, ref package, ref msg)) = cf.deprecated_info {
            let cl = self.pending_callsite_line();
            let file = self
                .env()
                .get("*PROGRAM-NAME")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let line = cl
                .or_else(|| {
                    self.env().get("?LINE").and_then(|v| match v {
                        Value::Int(i) => Some(*i),
                        _ => None,
                    })
                })
                .unwrap_or(0);
            crate::runtime::deprecation::record_deprecation(kind, name, package, msg, &file, line);
        }
    }

    /// Cached version of the Interpreter-native [`Self::has_multi_candidates`].
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
        let result = self.has_multi_candidates(name);
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
            let pkg = self.current_package().to_string();
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
            && let Some(def) = loan_env!(self, resolve_function_with_types(name, &args))
        {
            return self.compile_and_call_function_def(&def, args, compiled_fns);
        }
        // Dispatch Test functions straight to their typed handler (lever A).
        if let Some(result) = self.try_native_test_function(name, &args) {
            return result;
        }
        // Builtin operator-as-function `infix:<op>(...)` (e.g. `&infix:<+>`, the
        // routine `[+]`/hyper/`reduce` lower to). Any user-defined operator was
        // already resolved above (compiled_fns / OTF), so reaching here means the
        // builtin operator — dispatch it straight to the native `call_infix_routine`
        // handler instead of recording a tree-walk fallback. This mirrors
        // `call_function_fallback`'s infix arm exactly (the big `call_function` match
        // has no infix arm, so both reach the same `call_infix_routine` on the same
        // `self`); `sanitize_call_args` only strips the Test callsite marker, which
        // operator routines never carry, so the result is byte-identical. §D state
        // ownership: the operator handlers are native Rust on the VM's own state.
        if let Some(op) = name
            .strip_prefix("infix:<")
            .and_then(|s| s.strip_suffix('>'))
        {
            let normalized = if op == "\u{2212}" { "-" } else { op };
            return self.call_infix_routine(normalized, &args);
        }
        // File/FS builtin function (`slurp`/`open`/…): user subs were resolved above
        // (compiled_fns / OTF), so dispatch the builtin natively on the VM-owned
        // io_handles + filesystem instead of recording a tree-walk fallback
        // (§D state ownership ③, function forms). Byte-identical to call_function's
        // IO arms (same `builtin_*` impls, same `self`).
        if let Some(result) = self.try_native_io_function(name, &args) {
            return result;
        }
        // Pure list/coercion builtin function (`val`/`list`/`slip`/`hash`): user subs
        // resolved above, so dispatch the builtin natively instead of a tree-walk
        // fallback (§D(b) dispatch chain). Byte-identical to call_function's arms.
        if let Some(result) = self.try_native_collection_function(name, &args) {
            return result;
        }
        // CARRIER (EVAL/pseudo-package) vs TODO: compile to bytecode (else branch =
        // true tree-walk function fallback). See ledger §2/§C.
        if Self::is_interpreter_carrier_function(name) {
            crate::vm::vm_stats::record_function_carrier(name);
        } else {
            crate::vm::vm_stats::record_function_fallback(name);
        }
        self.vm_call_function(name, args)
    }

    /// Compile a FunctionDef on-the-fly to bytecode and execute via the Interpreter.
    /// This avoids the interpreter's tree-walking execution path.
    #[allow(dead_code)]
    /// Compile a `FunctionDef` on-the-fly to a `CompiledFunction`, caching by the
    /// body fingerprint (+ package, which scopes compile-time pseudo-variables
    /// like `$?PACKAGE`). Caching is essential to preserve state-variable identity
    /// across calls. Shared by `compile_and_call_function_def` and the non-trivial
    /// proto-body runner (ledger §D, multi-dispatch VM-ization), so both go through
    /// the same compile/cache path.
    pub(super) fn otf_compile_function_def(
        &mut self,
        def: &crate::ast::FunctionDef,
    ) -> CompiledFunction {
        let pkg = def.package.resolve();
        let fingerprint =
            crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body);
        let cache_key = {
            let mut hasher = std::hash::DefaultHasher::new();
            std::hash::Hash::hash(&fingerprint, &mut hasher);
            std::hash::Hash::hash(&pkg, &mut hasher);
            std::hash::Hasher::finish(&hasher)
        };
        if let Some(cached) = self.otf_compile_cache.get(&cache_key) {
            return cached.clone();
        }
        let cc = {
            let mut compiler = crate::compiler::Compiler::new();
            if !pkg.is_empty() && pkg != "GLOBAL" {
                compiler.set_current_package(pkg.to_string());
            }
            // Resolve $?DISTRIBUTION from the function's defining package
            compiler.current_distribution = self
                .package_distributions
                .get(&pkg)
                .cloned()
                .or_else(|| self.current_distribution.clone());
            compiler.compile_routine_closure_body(&def.params, &def.param_defs, &def.body)
        };
        let deprecated_info = def.deprecated_message.as_ref().map(|msg| {
            let kind = if def.is_method { "Method" } else { "Sub" };
            (
                kind.to_string(),
                def.name.resolve(),
                def.package.resolve(),
                msg.clone(),
            )
        });
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
            declares_inner_routines: false,
            named_param_slots: None,
            deprecated_info,
            declared_locals: None,
        };
        cf.precompute_param_local_slots();
        cf.precompute_named_param_slots();
        cf.detect_inner_subs();
        cf.compute_declared_locals();
        self.otf_compile_cache.insert(cache_key, cf.clone());
        cf
    }

    pub(super) fn compile_and_call_function_def(
        &mut self,
        def: &crate::ast::FunctionDef,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        // Use the pending callsite line for deprecation tracking,
        // since ?LINE in env may not reflect the call site yet.
        let callsite_line = crate::runtime::Interpreter::peek_callsite_line(&args)
            .or_else(|| self.pending_callsite_line());
        loan_env!(
            self,
            check_deprecation_for_def_with_line(def, callsite_line)
        );
        let name = def.name.resolve();
        let pkg = def.package.resolve();

        let cf = self.otf_compile_function_def(def);

        // Cache by name for fast lookup in exec_call_func_op — but never for a
        // multi name. The name-keyed cache is type-blind, so caching one multi
        // candidate under the bare name would make a later call with different
        // argument types wrongly reuse it. Multi candidates are still cached by
        // body fingerprint in `otf_compile_cache` above (safe, per-candidate).
        if !self.has_multi_candidates_cached(&name) {
            let name_sym = Symbol::intern(&name);
            self.otf_call_cache.insert(name_sym, cf.clone());
            self.otf_call_cache_gen = self.fn_resolve_gen;
        }

        // Set up samewith and multi-dispatch context that call_compiled_function_named
        // expects the caller to manage (mirrors exec_call_fn_op).
        self.push_samewith_context(&name, None);
        let pushed_dispatch = loan_env!(self, push_multi_dispatch_frame(&name, &args));

        let result = self.call_compiled_function_named(&cf, args, compiled_fns, &pkg, &name);

        self.pop_samewith_context();
        if pushed_dispatch {
            self.pop_multi_dispatch();
        }

        result
    }

    /// Check if a function name is handled by the interpreter's Rust code
    /// rather than by compiling its AST body. This includes test functions
    /// (implemented in runtime/test_functions.rs), internal `__mutsu_*` functions,
    /// and pseudo-package qualified names that need special resolution.
    pub(super) fn is_interpreter_handled_function(&self, name: &str) -> bool {
        // Test functions are implemented as Rust methods, not via AST
        if self.test_mode_active() && crate::runtime::Interpreter::is_test_function_name(name) {
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

    /// Whether a name that reaches the interpreter does so as a *carrier* rather
    /// than as a tree-walk fallback. `EVAL`/`EVALFILE` compile their source to
    /// bytecode and run it on a sub-Interpreter (`eval_block_value` -> `run_compiled_block`);
    /// pseudo-package reads (`CALLER::`/`OUTER::`/`SETTING::`/`DYNAMIC::`) are
    /// reflective env lookups. Neither tree-walks user code, so they are counted
    /// in a separate stats bucket (lever A). The remaining coupling — that the
    /// shared env/classes/roles registries are owned by the `Interpreter` struct
    /// — is a lever B (state ownership) concern, not a dispatch fallback.
    pub(super) fn is_interpreter_carrier_function(name: &str) -> bool {
        name == "EVAL"
            || name == "EVALFILE"
            || name.contains("SETTING::")
            || name.contains("OUTER::")
            || name.contains("CALLER::")
            || name.contains("DYNAMIC::")
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
        let resolved_def = loan_env!(self, resolve_function_with_types(name, args));
        let expected_fingerprint = resolved_def.as_ref().map(|def| {
            crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body)
        });
        // If runtime resolution fails, avoid reusing stale compiled cache entries.
        // This can happen across repeated EVAL calls that redefine the same routine name.
        let expected_fingerprint = expected_fingerprint?;
        let def_arity = resolved_def
            .as_ref()
            .map(|def| {
                def.param_defs
                    .iter()
                    .filter(|pd| !pd.named && !pd.slurpy)
                    .count()
            })
            .unwrap_or(arity);
        let matches_resolved = |cf: &CompiledFunction| cf.fingerprint == expected_fingerprint;
        let pkg = self.current_package();
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
                    self.env().get(pkg_prefix).is_some()
                        || self
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
        // Fallback: when call arity differs from definition arity (e.g. optional
        // params), try the definition's param count to find the compiled function.
        if found_key.is_none() && def_arity != arity {
            let key_fp = format!("{}::{}/{}#{:x}", pkg, name, def_arity, expected_fingerprint);
            if compiled_fns.get(&key_fp).is_some_and(&matches_resolved) {
                found_key = Some(key_fp);
            } else if pkg != "GLOBAL" {
                let key_fp_global =
                    format!("GLOBAL::{}/{}#{:x}", name, def_arity, expected_fingerprint);
                if compiled_fns
                    .get(&key_fp_global)
                    .is_some_and(&matches_resolved)
                {
                    found_key = Some(key_fp_global);
                }
            }
        }
        if let Some(key) = found_key {
            // Cache the resolution result for future lookups
            let cached_pkg = resolved_def
                .map(|def| def.package.resolve())
                .unwrap_or_else(|| self.current_package().to_string());
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
        self.record_cf_deprecation(cf);
        // A routine declared directly in this body is lexical; snapshot the
        // registry so it is removed on return unless it escaped (see
        // `call_compiled_function_named` / `return_value_escapes_routine`).
        let routine_snapshot = if cf.declares_inner_routines {
            Some(self.snapshot_routine_registry())
        } else {
            None
        };
        // Only save env when there are local variables to clean up.
        // When the function has no locals, the env save/restore is a
        // no-op (nothing to remove), but still causes an Arc clone that
        // raises the refcount and triggers O(env_size) deep clones on
        // any env write inside the function body (e.g. `$ = expr`).
        let has_locals = !cf.code.locals.is_empty();
        // Scoped-overlay pilot (docs/vm-dual-store.md Slice 6): instead of cloning
        // the whole caller env and merging non-local writes back with an
        // O(full-env) scan, install an empty scoped overlay over the caller. The
        // callee's writes accumulate in the overlay; on return we merge the
        // *overlay only* (the callee's actual writes) back, and discard
        // callee-local writes for free. Gated to bodies that never capture or
        // iterate the env for a full lexical view: no inner subs (no
        // closure/thread/block creation) and no reflective by-name access
        // (EVAL / CALLER:: / symbolic deref / pseudo-stash).
        let use_scoped =
            has_locals && !cf.has_inner_subs && !crate::opcode::reflective_name_access_possible();
        let caller_env: Option<Env> = if use_scoped {
            // Chain a child over the whole caller env (itself possibly scoped):
            // no flatten, so nested fast calls don't pay the O(env) merge.
            let parent = self.env().clone();
            let scoped = crate::env::Env::scoped_child(parent);
            Some(std::mem::replace(self.env_mut(), scoped))
        } else {
            None
        };
        let saved_env = if !use_scoped && has_locals {
            crate::vm::vm_stats::record_clone_env();
            Some(self.clone_env())
        } else {
            None
        };
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack_depth = self.stack.len();

        // Raku: routines get their own $_ initialized to (Any).
        let saved_topic = if cf.code.is_routine {
            let old = self.env().get("_").cloned();
            self.env_mut().insert(
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
            if let Some(val) = self.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Load persisted state variable values
        for (slot, key) in &cf.code.state_locals {
            if let Some(val) = self.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        let let_mark = self.let_saves_len();
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
                    self.resolve_let_saves_on_success(let_mark, true);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
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
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            loan_env!(self, set_state_var(key.clone(), val));
        }

        // Flush any dirty locals to env before restoring, so that captured
        // outer variable modifications (e.g. $a++ where $a is from outer scope)
        // are visible in env for the merge step below.

        // Restore state
        self.locals = saved_locals;

        // Restore env: if env was mutated, merge non-local changes back.
        // When has_locals is false, saved_env is None and no restore is needed
        // (functions without locals cannot leak local variables into the
        // caller's env — any env writes they do are intentional global state).
        if let Some(caller_env) = caller_env {
            // Scoped path: restore the caller env, then merge the callee's
            // overlay (its own writes only) back, dropping callee-local writes.
            let local_names: std::collections::HashSet<&str> =
                if let Some(ref declared) = cf.declared_locals {
                    declared.iter().map(|s| s.as_str()).collect()
                } else {
                    cf.code.locals.iter().map(|s| s.as_str()).collect()
                };
            let scoped = std::mem::replace(self.env_mut(), caller_env);
            for (k, v) in scoped.overlay_iter() {
                // The callee's private topic / arg array / routine-id must not
                // leak to the caller (the caller env already holds its own).
                if *k == "_" || *k == "@_" || *k == "%_" || *k == "__mutsu_callable_id" {
                    continue;
                }
                if !k.with_str(|s| local_names.contains(s)) {
                    self.env_mut().insert_sym(*k, v.clone());
                }
            }
        } else if let Some(saved_env) = saved_env {
            if saved_env.ptr_eq(self.env()) {
                // No env changes, nothing to merge
            } else {
                // Use declared_locals to only filter out function-local vars.
                // Captured outer variables should propagate their modifications.
                let local_names: std::collections::HashSet<&str> =
                    if let Some(ref declared) = cf.declared_locals {
                        declared.iter().map(|s| s.as_str()).collect()
                    } else {
                        cf.code.locals.iter().map(|s| s.as_str()).collect()
                    };
                let mut restored_env = saved_env;
                for (k, v) in self.env().iter() {
                    if !k.with_str(|s| local_names.contains(s)) {
                        restored_env.insert_sym(*k, v.clone());
                    }
                }
                *self.env_mut() = restored_env;
                // Mark env as dirty so caller re-syncs its locals from env.
                // This is needed when captured outer variables were modified
                // by the function (e.g. $a++ where $a is from the caller's scope).
            }
        } else {
            // Slice 6.3 step 2: the no-merge case — a 0-local function (no overlay,
            // no env clone). Its body wrote directly to the live caller env, so we
            // can't detect a captured-outer write by a merge. Gate on the
            // compile-time `has_env_writes` flag instead of the old blanket
            // post-call mark: a body that performs NO env write (`sub f { 42 }`,
            // no assign / increment / nested call / registration) cannot dirty a
            // caller slot — the dispatch's routine `_` is restored above — so it
            // needs no pull. Only a body that can write env forces the re-sync.
            if cf.code.has_env_writes {}
        }

        // Restore caller's $_ after routine call. In the scoped path the caller
        // env already retains its own `_` (the callee's `_` lived in the dropped
        // overlay and is skipped by the merge above), so this only runs for the
        // non-scoped clone path.
        if cf.code.is_routine && !use_scoped {
            match saved_topic {
                Some(v) => {
                    self.env_mut().insert("_".to_string(), v);
                }
                None => {
                    self.env_mut().remove("_");
                }
            }
        }

        // Slice F (env<->locals coherence): record the captured-outer variables
        // this body writes so the fast-call site writes their new env values
        // straight through to the caller's local slots (`apply_pending_rw_writeback`),
        // dropping the dependency on the reverse `sync_locals_from_env` pull. This
        // is the 0-arg-function analog of the closure free-var writeback (#3307):
        // `sub bump { $acc = $acc + 5 }` mutates the enclosing `$acc` by name, and
        // without this the caller's `$acc` slot stays stale when the reverse pull
        // is disabled. `free_var_writes` is the compile-time set of free vars this
        // body (or a nested closure) writes, so a pure body (`sub f { 42 }`) records
        // nothing and pays no cost. The fast-call site drains this immediately on
        // return, placing each source in the directly-enclosing caller's slot.
        // The topic (`_`/`@_`/`%_`) is excluded: it is the per-call loop/topic
        // alias, never a caller lexical to write back. (Cross-frame propagation —
        // a write a nested callee performed against a grand-ancestor's variable, as
        // in `note-twice`->`note-x`->`$log` or 0-arg recursion — still relies on the
        // reverse pull: a single-level call-site drain cannot reach it, and carrying
        // sources up the stack collides with lazy-iteration topics.)
        for sym in &cf.code.free_var_writes {
            sym.with_str(|name| {
                if name != "_" && name != "@_" && name != "%_" {
                    self.pending_rw_writeback_sources.push(name.to_string());
                }
            });
        }

        let final_result = match result {
            Ok(()) if fail_bypass => Ok(ret_val),
            Ok(()) => {
                if let Some(v) = explicit_return {
                    Ok(v)
                } else {
                    Ok(ret_val)
                }
            }
            Err(e) => Err(e),
        };
        // Restore the routine registry (removing this body's lexical routines)
        // unless an inner routine escaped via the return value.
        if let Some(snapshot) = routine_snapshot {
            match &final_result {
                Ok(v) if Self::return_value_escapes_routine(v) => {}
                _ => self.restore_routine_registry(snapshot),
            }
        }
        final_result
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

    /// Slice 2d: detect a call that passes an array/hash *value* to a plain
    /// readonly scalar `$` param. Raku binds the same mutable container in that
    /// case (`$n.push` / `$n[0]=` / `my @a := @$n` all mutate the caller's
    /// container), which the slot-only fast paths (light / positional_light)
    /// cannot express — they bind a detached copy into a local slot. Such calls
    /// must take the slow `bind_function_args_values` path, which promotes the
    /// param to a shared cell and registers the rw writeback. The common case
    /// (scalar args to `$` params, e.g. `fib($n)`) returns false cheaply, so the
    /// fast paths are preserved.
    pub(super) fn call_shares_container_into_scalar_param(
        cf: &CompiledFunction,
        args: &[Value],
    ) -> bool {
        // Cheapest rejection first: most calls pass non-container args (e.g.
        // `fib($n-1)`), so check the arg value before touching the param meta.
        args.iter().enumerate().any(|(i, arg)| {
            Self::arg_is_container_value(arg)
                && cf.param_defs.get(i).is_some_and(|pd| {
                    Self::is_plain_scalar_param_name(&pd.name)
                        && !pd.slurpy
                        && !pd.double_slurpy
                        && !pd.is_invocant
                        && !pd.sigilless
                        && pd.traits.is_empty()
                        && pd.sub_signature.is_none()
                })
        })
    }

    /// True when `arg` is a varref-capture for an `@`/`%` *array/hash variable*
    /// holding an `Array`/`Hash`. Only an `@`/`%` source needs the cell-promotion:
    /// the fast paths copy the array out of the `@`-variable into the param slot
    /// and detach it. A `$`-scalar source (`my $aref = [0]; f($aref)`) already
    /// shares the array by reference (`$aref[0]++` propagates without promotion),
    /// and a non-variable literal (`f([1,2])`) has no caller to share with — both
    /// must keep the fast path. A variable arg reaches the binder as a
    /// `__mutsu_varref_*` capture, so peek inside without cloning.
    pub(super) fn arg_is_container_value(arg: &Value) -> bool {
        let Value::Capture { positional, named } = arg else {
            return false;
        };
        positional.is_empty()
            && matches!(
                named.get("__mutsu_varref_name"),
                Some(Value::Str(name)) if name.starts_with('@') || name.starts_with('%')
            )
            && matches!(
                named.get("__mutsu_varref_value"),
                Some(Value::Array(..)) | Some(Value::Hash(..))
            )
    }

    /// Method analogue of `call_shares_container_into_scalar_param`: true when
    /// an array/hash *variable* argument is passed into a plain readonly scalar
    /// `$` param of a method. Raku binds the same mutable container, so such a
    /// call must take the slow `bind_function_args_values` path (which promotes
    /// the param to a shared cell and writes it back) instead of the slot-only
    /// fast path that detaches a copy. The alignment differs from the sub case:
    /// a method's `param_defs` include the invocant, but `args` do not — so we
    /// skip invocant (and named) params and align positional params to `args`
    /// by an independent counter.
    pub(super) fn method_shares_container_into_scalar_param(
        &self,
        method_def: &crate::runtime::MethodDef,
        args: &[Value],
    ) -> bool {
        // Unlike the sub path, method-call arguments are NOT wrapped in varref
        // captures — the source variable name lives in the separate
        // `arg_sources` table (`set_pending_call_arg_sources`). So a plain
        // `Array`/`Hash` value paired with an `@`/`%` source name in that table
        // is the method-call equivalent of the sub path's varref container arg.
        let arg_sources = self.pending_call_arg_sources();
        let mut arg_idx = 0usize;
        for pd in &method_def.param_defs {
            if pd.is_invocant || pd.traits.iter().any(|t| t == "invocant") || pd.named {
                continue;
            }
            let Some(arg) = args.get(arg_idx) else {
                break;
            };
            let src = arg_sources
                .and_then(|s| s.get(arg_idx))
                .and_then(|n| n.as_ref());
            arg_idx += 1;
            let eligible_scalar_param = Self::is_plain_scalar_param_name(&pd.name)
                && !pd.slurpy
                && !pd.double_slurpy
                && !pd.sigilless
                && pd.traits.is_empty()
                && pd.sub_signature.is_none();
            if !eligible_scalar_param {
                continue;
            }
            // A varref capture carries its own `@`/`%` source name; a plain
            // container value needs the source name from `arg_sources`. Either
            // way the binder promotes the bound value to a shared cell.
            let varref_container = Self::arg_is_container_value(arg);
            let plain_container_with_source = matches!(arg, Value::Array(..) | Value::Hash(..))
                && src.is_some_and(|n| n.starts_with('@') || n.starts_with('%'));
            if varref_container || plain_container_with_source {
                return true;
            }
        }
        false
    }

    /// A plain readonly scalar (`$`) parameter is stored sigil-less in
    /// `ParamDef::name` (e.g. `$n` -> `"n"`), unlike `@`/`%`/`&` params which
    /// keep their sigil. Detect it by a leading identifier char, excluding the
    /// twigil/dynamic forms (`$!x` -> `"!x"`, `$*x` -> `"*x"`, `$.x` -> `".x"`)
    /// and the `$_` topic.
    pub(super) fn is_plain_scalar_param_name(name: &str) -> bool {
        name != "_"
            && name
                .as_bytes()
                .first()
                .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
    }

    /// True when an `@`/`%` *variable* is passed by name to a plain readonly
    /// scalar `$` *named* param (`sub f(:$n) { $n.push }` called as
    /// `f(n => @a)`). Raku binds the same mutable container; the slot-only
    /// named light-call path binds a copy whose `.push` COW-detaches, so such a
    /// call must take the slow `bind_function_args_values` path (which promotes
    /// the bound value to a shared cell and registers the rw writeback — see the
    /// named branch in binding.rs). The source variable name is encoded
    /// "key=source" in `arg_sources` (`positional_arg_source_name`); a
    /// `$`-scalar source (`:$n` over `my $n = @a`) is excluded (it shares by
    /// reference already, like the positional scalar source).
    pub(super) fn call_shares_container_into_named_scalar_param(
        cf: &CompiledFunction,
        args: &[Value],
        arg_sources: Option<&[Option<String>]>,
    ) -> bool {
        let Some(sources) = arg_sources else {
            return false;
        };
        args.iter().enumerate().any(|(i, arg)| {
            let unwrapped = unwrap_varref_value(arg.clone());
            let Value::Pair(key, val) = &unwrapped else {
                return false;
            };
            if !matches!(**val, Value::Array(..) | Value::Hash(..)) {
                return false;
            }
            let has_container_source = sources
                .get(i)
                .and_then(|s| s.as_ref())
                .and_then(|enc| enc.split_once('='))
                .is_some_and(|(_, src)| src.starts_with('@') || src.starts_with('%'));
            if !has_container_source {
                return false;
            }
            cf.param_defs.iter().any(|pd| {
                pd.named
                    && Self::named_param_share_match_key(pd) == key.as_str()
                    && Self::is_eligible_named_scalar_share_param(pd)
            })
        })
    }

    /// The Pair key a named param matches (`:$n` -> "n", `:foo($x)` -> "foo").
    /// Scalar named params are stored sigil-less, so this strips a leading `:`.
    fn named_param_share_match_key(pd: &crate::ast::ParamDef) -> &str {
        pd.name.strip_prefix(':').unwrap_or(&pd.name)
    }

    /// Eligibility mirror of binding.rs `named_scalar_container_share_eligible`:
    /// a plain readonly scalar named param (not `@`/`%`/`&`, no attr twigil, not
    /// `$_`, no `is copy`/`is rw`/`is raw`, not slurpy, no sub-signature).
    fn is_eligible_named_scalar_share_param(pd: &crate::ast::ParamDef) -> bool {
        let bare = Self::named_param_share_match_key(pd);
        !pd.traits
            .iter()
            .any(|t| t == "copy" || t == "rw" || t == "raw")
            && !pd.slurpy
            && !pd.double_slurpy
            && pd.sub_signature.is_none()
            && bare != "_"
            && !bare.starts_with(['@', '%', '&', '!', '.'])
            && bare
                .as_bytes()
                .first()
                .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
    }

    /// Ultra-fast call for simple positional-only functions (e.g. `sub fib($n)`).
    /// Avoids push_call_frame, Sub value creation, block/routine push,
    /// callable_id lookup, and full bind_function_args_values. Parameters are
    /// bound directly to pre-computed local slots AND written to env so that
    /// closures and dynamic lookups work correctly.
    /// Build the common `X::TypeCheck::Argument` attribute map (message,
    /// objname, signature, arguments) shared by the positional-light arity and
    /// type-mismatch error paths. Type-mismatch callers additionally insert
    /// `expected`/`got`. Mirrors the interpreter path in
    /// `Interpreter::enhance_binding_error`.
    fn type_check_argument_attrs(
        func_name: &str,
        param_defs: &[crate::ast::ParamDef],
        args: &[Value],
        message: String,
    ) -> std::collections::HashMap<String, Value> {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message));
        attrs.insert("objname".to_string(), Value::str(func_name.to_string()));
        attrs.insert(
            "signature".to_string(),
            Value::str(crate::runtime::Interpreter::build_signature_string(
                param_defs,
            )),
        );
        let arg_type_values: Vec<Value> = crate::runtime::Interpreter::arg_type_names(args)
            .into_iter()
            .map(Value::str)
            .collect();
        attrs.insert("arguments".to_string(), Value::array(arg_type_values));
        attrs
    }

    pub(super) fn call_compiled_function_positional_light(
        &mut self,
        cf: &CompiledFunction,
        args: &[Value],
        compiled_fns: &HashMap<String, CompiledFunction>,
        func_name: &str,
    ) -> Result<Value, RuntimeError> {
        self.record_cf_deprecation(cf);
        let param_slots = cf.param_local_slots.as_ref().unwrap();
        let positional_count = param_slots.len();
        let actual_count = args.len();

        // Every positional-light-eligible parameter is a mandatory positional
        // (no default, optional `?`, or slurpy -- see
        // `is_positional_light_call_eligible`), so any shortfall is a "too few
        // positionals" arity error. Report it as a typed X::TypeCheck::Argument
        // carrying objname/signature/arguments, matching the interpreter path.
        if actual_count < positional_count {
            let msg = format!(
                "Too few positionals passed; expected {} arguments but got {}",
                positional_count, actual_count
            );
            return Err(RuntimeError::typed(
                "X::TypeCheck::Argument",
                Self::type_check_argument_attrs(func_name, &cf.param_defs, args, msg),
            ));
        }

        let saved_locals = std::mem::take(&mut self.locals);

        // Scoped-overlay (docs/vm-dual-store.md Slice 6): install an empty
        // born-owned overlay over the caller. Param / local env writes land in a
        // fresh map and are discarded by dropping the overlay on return (for
        // callee-local names) or merged overlay-only (for captured-outer writes).
        // This replaces the previous name-keyed save/restore juggling
        // (saved_env_locals / saved_param_env) with a single O(callee-writes)
        // merge -- the function's own params/locals never pollute the caller env.
        let parent = self.env().clone();
        let caller_env = std::mem::replace(self.env_mut(), crate::env::Env::scoped_child(parent));

        let num_locals = cf.code.locals.len();
        self.locals.clear();
        self.locals.resize(num_locals, Value::Nil);

        // Read-through to the caller (parent tier) for the initial value of a
        // local that shadows a same-named caller variable, matching the prior
        // env().get() semantics.
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if let Some(val) = self.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }

        let saved_readonly = self.save_readonly_vars();
        // Bind params to slots. Also write the param into the overlay when a
        // name-based reader needs it (reflective access anywhere / GetGlobal /
        // closure capture via needs_env_sync), or when it is `Nil` (the GetLocal
        // handler treats a `Nil` slot as possibly-undeclared and verifies via
        // env.contains_key). The overlay write is born-owned (no caller-env fork)
        // and is dropped on return, so no per-param save/restore is needed.
        let write_all_params = crate::opcode::reflective_name_access_possible();
        for (param_idx, slot) in param_slots.iter().enumerate() {
            if param_idx < actual_count {
                let val = crate::runtime::types::unwrap_varref_value(args[param_idx].clone());
                if let Some(ref tc) = cf.param_defs[param_idx].type_constraint
                    && !Self::fast_type_check(&val, tc)
                {
                    self.restore_readonly_vars(saved_readonly);
                    self.set_env(caller_env);
                    self.locals = saved_locals;
                    {
                        let param_name = &cf.param_defs[param_idx].name;
                        let got = runtime::value_type_name(&val);
                        let msg = format!(
                            "Type check failed in binding ${}: expected {}, got {}",
                            param_name, tc, got
                        );
                        let mut attrs =
                            Self::type_check_argument_attrs(func_name, &cf.param_defs, args, msg);
                        attrs.insert("expected".to_string(), Value::str(tc.to_string()));
                        attrs.insert("got".to_string(), Value::str(got.to_string()));
                        return Err(RuntimeError::typed("X::TypeCheck::Argument", attrs));
                    }
                }
                let param_name = &cf.param_defs[param_idx].name;
                self.locals[*slot] = val.clone();
                let needs_env = write_all_params
                    || matches!(val, Value::Nil)
                    || cf.code.needs_env_sync.get(*slot).copied().unwrap_or(true);
                if needs_env {
                    self.env_mut().insert(param_name.clone(), val);
                }
                self.mark_readonly(&cf.param_defs[param_idx].name);
            }
        }
        // Bind-time param values that a name-based reader can observe were
        // already written into the (born-owned) overlay env above when
        // `needs_env` held. A slot-only param (read solely via GetLocal) never
        // reaches env, so no env mirror is needed here. The bind value is already
        // coherent; any later reassignment in the body writes through to env via
        // the SetLocal path (`flush_local_to_env`).

        let saved_stack_depth = self.stack.len();
        let let_mark = self.let_saves_len();

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
                    self.resolve_let_saves_on_success(let_mark, true);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
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

        self.locals = saved_locals;
        self.restore_readonly_vars(saved_readonly);

        // Restore the caller env and merge the overlay (the callee's own writes)
        // back: a write to a captured outer variable (not a declared local of
        // this function) persists in the caller; the function's params/locals are
        // dropped with the overlay. This replaces the prior per-name save/restore.
        {
            let local_names: std::collections::HashSet<&str> =
                if let Some(ref declared) = cf.declared_locals {
                    declared.iter().map(|s| s.as_str()).collect()
                } else {
                    cf.code.locals.iter().map(|s| s.as_str()).collect()
                };
            let scoped = std::mem::replace(self.env_mut(), caller_env);
            for (k, v) in scoped.overlay_iter() {
                // The callee's private topic / arg array / routine id, and the
                // per-frame contextual vars (`?LINE`/`?FILE`/...), must not
                // propagate to the caller (which retains its own). Skipping the
                // `?`-prefixed ones also avoids spurious `env_dirty` churn from
                // the per-statement `?LINE` write on every recursive call.
                if *k == "_"
                    || *k == "@_"
                    || *k == "%_"
                    || *k == "__mutsu_callable_id"
                    || k.with_str(|s| s.starts_with('?'))
                {
                    continue;
                }
                if !k.with_str(|s| local_names.contains(s)) {
                    self.env_mut().insert_sym(*k, v.clone());
                }
            }
        }

        // Return type check (if specified). Allows type objects, Nil, and Failure through.
        if result.is_ok()
            && let Some(ref rt) = cf.return_type
        {
            let check_val = explicit_return.as_ref().unwrap_or(&ret_val);
            if !Self::light_return_type_check(check_val, rt) {
                return Err(RuntimeError::new(format!(
                    "Type check failed for return value; expected {}, got {}",
                    rt,
                    runtime::value_type_name(check_val)
                )));
            }
        }

        // Slice F (env<->locals coherence): record the captured-outer variables
        // this body writes so the call-site op writes their new env values straight
        // through to the caller's local slots, dropping the dependency on the
        // reverse `sync_locals_from_env` pull. Mirrors the fast-call (#3317) and
        // named-dispatch (#3323) paths: `sub take($n) { $seen = $n }` writes its
        // enclosing `$seen` by name. `free_var_writes` is empty for a pure body
        // (no cost); the topic is excluded as a per-call alias.
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
        self.record_cf_deprecation(cf);
        // Save caller locals and create callee locals
        let saved_locals = std::mem::take(&mut self.locals);

        // Scoped-overlay (docs/vm-dual-store.md Slice 6): install an empty
        // born-owned overlay over the caller. Param / alias / @_ env writes below
        // land in a fresh map and are discarded by dropping the overlay on return
        // (callee-local names) or merged overlay-only (captured-outer writes),
        // replacing the per-key save/restore the `modified_env_keys` list did.
        let parent = self.env().clone();
        let caller_env = std::mem::replace(self.env_mut(), crate::env::Env::scoped_child(parent));

        let num_locals = cf.code.locals.len();
        self.locals = vec![Value::Nil; num_locals];

        // Bind parameters directly to locals slots and the overlay env.
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
                            self.env_mut().insert(sub_name.clone(), v.clone());
                        }
                    }
                    Some(v)
                } else if pd.required {
                    self.set_env(caller_env);
                    self.locals = saved_locals;
                    // Missing required named parameter is a runtime X::AdHoc in
                    // Raku (see binding.rs); carry the typed exception so it does
                    // not fall back to the bare "Exception" default.
                    return Err(RuntimeError::typed_msg(
                        "X::AdHoc",
                        format!("Required named parameter '{}' not passed", param_name),
                    ));
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
                    self.set_env(caller_env);
                    self.locals = saved_locals;
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
            self.env_mut().insert(param_name.clone(), bound_val);
        }

        // Mark parameters as readonly (by default, params are immutable in Raku).
        // Save existing readonly state so we can restore it after the call.
        let saved_readonly = self.save_readonly_vars();
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
                    self.mark_readonly(&pd.name);
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
            self.env_mut()
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
                    // Also set in (overlay) env for the placeholder and its alias
                    self.env_mut().insert(param.clone(), val.clone());
                    // Create de-careted alias: ^foo -> foo, ^k -> k
                    if let Some(bare) = param.strip_prefix('^') {
                        let bare = bare.to_string();
                        if let Some(slot) = cf.code.locals.iter().position(|n| *n == bare) {
                            self.locals[slot] = val.clone();
                        }
                        self.env_mut().insert(bare, val);
                    }
                }
            }
        }

        // For any locals not yet set from params, try to initialize from env
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if matches!(self.locals[i], Value::Nil)
                && let Some(val) = self.env().get(local_name)
            {
                self.locals[i] = val.clone();
            }
        }

        let saved_stack_depth = self.stack.len();
        let let_mark = self.let_saves_len();

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
                    self.resolve_let_saves_on_success(let_mark, true);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
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

        // Restore readonly vars
        self.restore_readonly_vars(saved_readonly);

        // Restore the caller env, merging the overlay (the callee's own writes)
        // back: a write to a captured outer variable (not a declared local /
        // param of this function) persists in the caller; the function's
        // params/locals/aliases are dropped with the overlay. This replaces the
        // per-key `modified_env_keys` save/restore.
        {
            let local_names: std::collections::HashSet<&str> =
                if let Some(ref declared) = cf.declared_locals {
                    declared.iter().map(|s| s.as_str()).collect()
                } else {
                    cf.code.locals.iter().map(|s| s.as_str()).collect()
                };
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
                if !k.with_str(|s| local_names.contains(s)) {
                    self.env_mut().insert_sym(*k, v.clone());
                }
            }
        }

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
    fn return_value_escapes_routine(v: &Value) -> bool {
        matches!(
            v,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } | Value::Mixin(..)
        )
    }

    pub(super) fn call_compiled_function_named(
        &mut self,
        cf: &CompiledFunction,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
        fn_package: &str,
        fn_name: &str,
    ) -> Result<Value, RuntimeError> {
        // A routine declared directly in this body is lexical to the call.
        // Snapshot the routine registry around the (multi-exit) body so the
        // lexical routine is removed on return — UNLESS it escapes by being
        // returned (then its registry entry must survive so it stays callable
        // by name). The snapshot is cheap relative to the rare case it guards.
        if !cf.declares_inner_routines {
            return self.call_compiled_function_named_inner(
                cf,
                args,
                compiled_fns,
                fn_package,
                fn_name,
            );
        }
        let snapshot = self.snapshot_routine_registry();
        let result =
            self.call_compiled_function_named_inner(cf, args, compiled_fns, fn_package, fn_name);
        match &result {
            Ok(v) if Self::return_value_escapes_routine(v) => {}
            _ => self.restore_routine_registry(snapshot),
        }
        result
    }

    fn call_compiled_function_named_inner(
        &mut self,
        cf: &CompiledFunction,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
        fn_package: &str,
        fn_name: &str,
    ) -> Result<Value, RuntimeError> {
        // Slice 6.3 step 2: signal env_dirty *precisely* (like
        // call_compiled_function_fast) instead of relying on a blanket post-call
        // mark at the call site. Save the caller's incoming dirtiness; the body's
        // own nested-call dirtiness is about the callee env (subsumed by the merge
        // below), so it is reset before the body and recomputed from what the
        // merge actually wrote back to the caller env.
        // Slice F: the rw-writeback list is drained by the call-site op right
        // after each dispatch returns, so it must hold *only* this call's
        // sources on return. Clear any leftover from a sibling whose call site
        // did not drain (a non-rw path), so it can never be written into the
        // wrong slot. Nested calls in the body self-drain via their own ExecCall
        // ops, leaving the list empty before this frame records its own sources.
        self.pending_rw_writeback_sources.clear();
        let (args, callsite_line) = self.sanitize_call_args(&args);
        if callsite_line.is_some() {
            loan_env!(self, set_pending_callsite_line(callsite_line));
        }
        // Record deprecation for cached compiled functions
        self.record_cf_deprecation(cf);
        // Inject callsite line BEFORE push_call_frame so the parent env
        // contains the updated ?LINE. This avoids triggering Arc::make_mut
        // deep clone after the env Arc is shared with the call frame.
        loan_env!(self, inject_pending_callsite_line());
        self.push_call_frame();
        let saved_stack_depth = self.call_frames.last().unwrap().saved_stack_depth;
        let return_spec = cf.return_type.clone();

        loan_env!(self, push_caller_env());

        // Push Sub value to block_stack for callframe().code
        let sub_val = Value::make_sub(
            Symbol::intern(fn_package),
            Symbol::intern(fn_name),
            cf.params.clone(),
            cf.param_defs.clone(),
            vec![],
            false,
            // Flatten: this Sub is pushed for callframe().code introspection and
            // must expose the full lexical view, not a scoped overlay.
            self.clone_env(),
        );
        self.push_block(sub_val);

        // Scoped-overlay (docs/vm-dual-store.md Slice 6): install an empty
        // born-owned overlay over the caller now that sub_val / push_caller_env
        // captured the flat caller. Callee setup/body env writes land in the
        // overlay; on return the merge iterates it overlay-only into the restored
        // caller env. frame.saved_env holds the flat caller for restoration.
        {
            let parent = self.env().clone();
            self.set_env(crate::env::Env::scoped_child(parent));
        }

        // Always push a routine frame so that &?ROUTINE works inside anonymous
        // subs too. Use "<anon>" as a sentinel name when fn_name is empty.
        let routine_push_name = if fn_name.is_empty() {
            "<anon>".to_string()
        } else {
            fn_name.to_string()
        };
        self.push_routine_with_location(
            fn_package.to_string(),
            routine_push_name,
            self.current_source_line(),
            self.current_source_file(),
        );
        let mut callable_id: Option<u64> = None;
        if !fn_name.is_empty() {
            let callable_key = format!("__mutsu_callable_id::{fn_package}::{fn_name}");
            let resolved_callable_id = self
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
                self.env_mut().insert(
                    "__mutsu_callable_id".to_string(),
                    Value::Int(resolved_callable_id),
                );
            }
        }
        let is_test_assertion = if fn_name.is_empty() {
            false
        } else {
            loan_env!(self, routine_is_test_assertion_by_name(fn_name, &args))
        };
        let pushed_assertion = self.push_test_assertion_context(is_test_assertion);

        if cf.empty_sig && !args.is_empty() {
            self.pop_routine();
            self.pop_test_assertion_context(pushed_assertion);
            self.pop_caller_env();
            self.stack.truncate(saved_stack_depth);
            let frame = self.pop_call_frame();
            // Drop the scoped overlay, restoring the caller env.
            self.set_env(frame.saved_env);
            return Err(Interpreter::reject_args_for_empty_sig(&args));
        }

        // Set current_package to the function's defining package so that default
        // value expressions can resolve package-scoped functions (e.g. &double).
        let saved_package = self.current_package().to_string();
        if !fn_package.is_empty() && fn_package != "GLOBAL" {
            self.set_current_package(fn_package.to_string());
        }
        // When the function has where constraints and there is a &name Sub in
        // env (which carries closure env), merge the Sub's captured variables
        // into the current env so where-constraint expressions can access them.
        if !fn_name.is_empty() && cf.param_defs.iter().any(|pd| pd.where_constraint.is_some()) {
            let ampname = format!("&{}", fn_name);
            if let Some(Value::Sub(ref sub_data)) = self.env().get(&ampname).cloned() {
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
                        self.env_mut().insert_sym(*k, v.clone());
                    }
                }
            }
        }
        // Skip bind_function_args_values for 0-arg functions with no params,
        // avoiding the @_ env insert that triggers Arc::make_mut deep clone.
        let rw_bindings = if args.is_empty() && cf.param_defs.is_empty() && cf.params.is_empty() {
            vec![]
        } else {
            match loan_env!(
                self,
                bind_function_args_values(&cf.param_defs, &cf.params, &args)
            ) {
                Ok(bindings) => bindings,
                Err(e) => {
                    self.set_current_package(saved_package);
                    self.pop_routine();
                    self.pop_test_assertion_context(pushed_assertion);
                    self.pop_caller_env();
                    self.stack.truncate(saved_stack_depth);
                    let frame = self.pop_call_frame();
                    *self.env_mut() = frame.saved_env;
                    return Err(Interpreter::enhance_binding_error(
                        e,
                        fn_name,
                        &cf.param_defs,
                        &args,
                    ));
                }
            }
        };
        self.prepare_definite_return_slot(return_spec.as_deref());

        // Raku: $! is scoped per routine — fresh Nil on entry.
        // Only insert if $! isn't already Nil, to avoid triggering
        // Arc::make_mut deep clone on the CoW env.
        if !fn_name.is_empty() {
            let needs_reset = self
                .env()
                .get("!")
                .is_some_and(|v| !matches!(v, Value::Nil));
            if needs_reset {
                self.env_mut().insert("!".to_string(), Value::Nil);
            }
        }

        // Raku: routines get their own $_ initialized to (Any).
        if cf.code.is_routine && !cf.param_defs.iter().any(|pd| pd.name == "_") {
            self.env_mut().insert(
                "_".to_string(),
                Value::Package(crate::symbol::Symbol::intern("Any")),
            );
        }

        self.locals = vec![Value::Nil; cf.code.locals.len()];
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if let Some(val) = self.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Load persisted state variable values
        for (slot, key) in &cf.code.state_locals {
            if let Some(val) = self.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        let let_mark = self.let_saves_len();
        // Body-internal env_dirty (from nested calls) concerns the callee env,
        // which the return merge reconciles; reset so the post-merge value
        // reflects only what was actually written back to the caller.
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
                        self.resolve_let_saves_on_success(let_mark, true);
                        result = Ok(());
                        break;
                    }
                    loan_env!(self, restore_let_saves(let_mark));
                    result = Err(e);
                    break;
                }
                Err(e) if e.return_value.is_some() => {
                    // Non-local return: if the signal targets a specific callable,
                    // only catch it if this routine is the target.
                    if let Some(target_id) = e.return_target_callable_id
                        && callable_id != Some(target_id)
                    {
                        loan_env!(self, restore_let_saves(let_mark));
                        result = Err(e);
                        break;
                    }
                    let ret_val = e.return_value.unwrap();
                    explicit_return = Some(ret_val.clone());
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.resolve_let_saves_on_success(let_mark, true);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
                    // fail() — restore let saves and return a Failure value
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
                Symbol::intern(&self.current_package()),
                *sub_name,
                params.clone(),
                param_defs.clone(),
                body.clone(),
                *is_rw,
                // Flatten: a Sub returned as a value is dispatched cross-scope.
                self.clone_env(),
            );
        }

        self.stack.truncate(saved_stack_depth);

        // Sync state variables back to persistent storage.
        // Read from env first (methods like push update env directly),
        // falling back to locals.
        for (slot, key) in &cf.code.state_locals {
            let local_name = &cf.code.locals[*slot];
            let val = self
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            loan_env!(self, set_state_var(key.clone(), val));
        }

        // A scalar `is rw` / `is raw` parameter (`$a is rw`, stored sigil-less as
        // `a`) is bound to a slot-only local in the body (read/written via
        // GetLocal/SetLocal), so a `$a = …` write never reaches env
        // (`flush_local_to_env` skips it: needs_env_sync is false).
        // `apply_rw_bindings_to_env` (below) reads the param's value from env to
        // write it back to the caller's variable, so flush the scalar rw-param
        // slots into env now — while `self.locals` is still the callee's array,
        // before `pop_call_frame` restores the caller's locals. `@`/`%` container
        // params are intentionally excluded: their in-place mutations (`@x.push`,
        // `@x.splice`, ...) go *through* env by name already, so the local slot
        // holds a stale copy and flushing it would clobber the live container.
        if !rw_bindings.is_empty() {
            for (param_name, _source) in &rw_bindings {
                if param_name.starts_with(['@', '%', '&']) {
                    continue;
                }
                if let Some(slot) = cf.code.locals.iter().position(|n| n == param_name) {
                    let final_val = self.locals[slot].clone();
                    self.env_mut().insert(param_name.clone(), final_val);
                }
            }
        }

        self.set_current_package(saved_package);
        self.pop_routine();
        self.pop_test_assertion_context(pushed_assertion);
        self.pop_block();
        let effective_return_spec = return_spec
            .as_deref()
            .map(|spec| loan_env!(self, resolved_type_capture_name(spec)));

        let frame = self.pop_call_frame();
        let restored_env = frame.saved_env;
        // Slice 6.3 step 2: track whether the merge actually wrote a *caller-slot-
        // aliasing* value back, so env_dirty (a caller locals re-sync) is set only
        // when needed. A plain-lexical writeback (captured-outer mutation) or an
        // `is rw` param writeback can alias a caller local slot; dynamic-var
        // writeback (pop_caller_env_with_writeback) targets `$*x` names that have
        // no compiled slot, so it never obliges a pull.
        // Fast path: if the env wasn't mutated during the call (Arc still shared),
        // we can skip the expensive env merge and just restore directly.
        if restored_env.ptr_eq(self.env()) {
            self.pop_caller_env();
        } else {
            let mut restored_env = restored_env;
            self.pop_caller_env_with_writeback(&mut restored_env);
            loan_env!(
                self,
                apply_rw_bindings_to_env(&rw_bindings, &mut restored_env)
            );
            let rw_sources: std::collections::HashSet<String> = rw_bindings
                .iter()
                .map(|(_, source)| source.clone())
                .collect();
            // Slice F: record the caller-source names just written back so the
            // call-site op (which holds the caller's `code`) can write each value
            // straight through to the caller's local slot, keeping it coherent
            // without relying on the reverse `sync_locals_from_env` pull.
            if !rw_sources.is_empty() {
                self.pending_rw_writeback_sources
                    .extend(rw_sources.iter().cloned());
                // Slice F (carrier completeness, open-question #2): when this sub
                // runs *inside a carrier* (an interpreter routine like
                // `lives-ok { f(@a, $x) }` executing it), the rw writeback above
                // mutated a caller lexical (`$x`) by name through a path the
                // carrier does not otherwise log (`set_env_with_main_alias`). Log
                // it into the active carrier set too, so the carrier's
                // `writeback_carrier_writes` reconciles the carrier-caller's slot
                // — otherwise the write is invisible to the carrier and the slot
                // stays stale once the reverse pull is gone.
                if self.carrier_writes.is_some() {
                    for source in &rw_sources {
                        self.note_caller_env_write(source);
                    }
                }
            }
            // Use declared_locals (vars declared via `my` or as params) instead
            // of all locals. Captured outer variables should propagate their
            // modifications back to the caller.
            let local_names: std::collections::HashSet<&str> =
                if let Some(ref declared) = cf.declared_locals {
                    declared.iter().map(|s| s.as_str()).collect()
                } else {
                    cf.code.locals.iter().map(|s| s.as_str()).collect()
                };
            for (k, v) in self.env().iter() {
                if *k == "_" || *k == "@_" || *k == "%_" {
                    continue;
                }
                // __mutsu_callable_id must not leak from callee back to
                // caller; it identifies the current routine scope for
                // non-local return targeting.
                if *k == "__mutsu_callable_id" {
                    continue;
                }
                if restored_env.contains_key_sym(*k)
                    && !k.with_str(|s| local_names.contains(s))
                    && !k.with_str(|s| rw_sources.contains(s))
                {
                    restored_env.insert_sym(*k, v.clone());
                }
            }
            *self.env_mut() = restored_env;
        }

        // Slice F (env<->locals coherence): record the captured-outer variables
        // this body writes so the call-site op writes their new env values
        // straight through to the caller's local slots (`apply_pending_rw_writeback`),
        // dropping the dependency on the reverse `sync_locals_from_env` pull. This
        // mirrors the 0-arg fast-call path (#3317): a qualified/`our` sub reached by
        // name (`module M { our sub foo() { $called = True } }`) goes through this
        // path rather than the fast path, but writes its enclosing `$called` the
        // same way. `free_var_writes` is the compile-time set of free vars this
        // body writes, so a pure body records nothing and pays no cost. The topic
        // (`_`/`@_`/`%_`) is excluded — it is a per-call alias, never a caller
        // lexical. (Cross-frame propagation still relies on the reverse pull.)
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
                let base_result = if let Some(v) = explicit_return {
                    let mut e = RuntimeError::new("return");
                    e.return_value = Some(v);
                    Err(e)
                } else {
                    Ok(ret_val)
                };
                loan_env!(
                    self,
                    finalize_return_with_spec(base_result, effective_return_spec.as_deref())
                )
            }
            Err(e) => Err(e),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn carrier_functions_are_not_tree_walk_fallbacks() {
        // EVAL/EVALFILE compile to bytecode and run on a sub-Interpreter; pseudo-package
        // reads are reflective env lookups. Both enter the interpreter as a
        // carrier, so they are classified out of the tree-walk fallback metric.
        assert!(Interpreter::is_interpreter_carrier_function("EVAL"));
        assert!(Interpreter::is_interpreter_carrier_function("EVALFILE"));
        assert!(Interpreter::is_interpreter_carrier_function(
            "Foo::CALLER::bar"
        ));
        assert!(Interpreter::is_interpreter_carrier_function("OUTER::x"));
        assert!(Interpreter::is_interpreter_carrier_function("SETTING::y"));
        assert!(Interpreter::is_interpreter_carrier_function("DYNAMIC::z"));

        // Genuine user/builtin subs are real fallbacks when they reach the
        // interpreter, not carriers.
        assert!(!Interpreter::is_interpreter_carrier_function("say"));
        assert!(!Interpreter::is_interpreter_carrier_function("my-sub"));
        assert!(!Interpreter::is_interpreter_carrier_function("evaluate"));
    }
}
