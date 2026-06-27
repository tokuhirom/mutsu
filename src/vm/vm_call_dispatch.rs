use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Record a deprecation event for a compiled function if it has deprecation info.
    pub(super) fn record_cf_deprecation(&self, cf: &CompiledFunction) {
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
