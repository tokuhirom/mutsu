use super::*;
use crate::symbol::Symbol;

/// Process-global L2 behind the per-interpreter `otf_compile_cache`, keyed by
/// the same `(body fingerprint ^ package)` key. See the L2 comment in
/// [`Interpreter::otf_compile_function_def`] for why it exists (spawn-heavy
/// loops re-OTF-compiling per task) and the `state` exclusion that keeps it
/// semantics-preserving. Guarded by a plain `Mutex`: it is touched only on the
/// per-interpreter cache's MISS path (once per body per interpreter), never per
/// call.
fn global_otf_cache() -> &'static std::sync::Mutex<rustc_hash::FxHashMap<u64, Arc<CompiledFunction>>>
{
    static CACHE: std::sync::OnceLock<
        std::sync::Mutex<rustc_hash::FxHashMap<u64, Arc<CompiledFunction>>>,
    > = std::sync::OnceLock::new();
    CACHE.get_or_init(|| std::sync::Mutex::new(rustc_hash::FxHashMap::default()))
}

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
            let line = cl.unwrap_or(self.cur_source_line);
            crate::runtime::deprecation::record_deprecation(kind, name, package, msg, &file, line);
        }
    }

    /// Cached version of the Interpreter-native [`Self::has_multi_candidates`].
    /// Uses `fn_resolve_gen` for invalidation so it's O(1) on cache hit.
    pub(super) fn has_multi_candidates_cached(&mut self, name: &str) -> bool {
        self.has_multi_candidates_cached_sym(Symbol::intern(name))
    }

    /// `has_multi_candidates_cached` for a callsite that already holds the
    /// name's pre-interned `Symbol` (every `CallFunc` does, via
    /// `CompiledCode::const_sym`). The cache is `Symbol`-keyed, so taking the
    /// `&str` form only to re-`intern` it hashed the name string on every call —
    /// it profiled as the `hash_one` + `memcmp` pair on the OTF dispatch path.
    pub(super) fn has_multi_candidates_cached_sym(&mut self, sym: Symbol) -> bool {
        if self.multi_candidates_cache_gen != self.fn_resolve_gen {
            self.multi_candidates_cache.clear();
            self.multi_candidates_cache_gen = self.fn_resolve_gen;
        }
        if let Some(&cached) = self.multi_candidates_cache.get(&sym) {
            return cached;
        }
        let result = self.has_multi_candidates(&sym.resolve());
        self.multi_candidates_cache.insert(sym, result);
        result
    }

    /// The bare-name lookup context a `has_proto` / `has_declared_function` /
    /// `has_multi_function` answer depends on: `bare_name_packages()` is fully
    /// determined by the current package and the innermost routine frame's
    /// lexical package, so `(those two, name)` is a sound memo key.
    #[inline]
    fn bare_name_ctx_key(&self, name_sym: Symbol) -> (Symbol, Option<Symbol>, Symbol) {
        (
            self.current_package_sym(),
            self.routine_stack_top().and_then(|f| f.lexical_package),
            name_sym,
        )
    }

    /// Cached [`Self::has_proto`]. The uncached probe runs 3+ times per
    /// `CallFunc` dispatch and pays a `Vec<String>` allocation plus two
    /// `format!`s per candidate package each time — it profiled as the
    /// `alloc::fmt::format` + `StrSearcher::new` + malloc cluster on the
    /// ripemd hot loop. Invalidated by `Registry::proto_generation()`;
    /// package-context sensitivity is carried in the key (see
    /// [`Self::bare_name_ctx_key`]), so this is strictly conservative.
    pub(crate) fn has_proto_cached(&mut self, name: &str) -> bool {
        let pgen = self.registry().proto_generation();
        if self.has_proto_cache_gen != pgen {
            self.has_proto_cache.clear();
            self.has_proto_cache_gen = pgen;
        }
        let key = self.bare_name_ctx_key(Symbol::intern(name));
        if let Some(&cached) = self.has_proto_cache.get(&key) {
            return cached;
        }
        let result = self.has_proto(name);
        self.has_proto_cache.insert(key, result);
        result
    }

    /// Cached [`Self::has_declared_function`]; guarded by `fn_resolve_gen`
    /// like `multi_candidates_cache`, with the package context in the key.
    pub(crate) fn has_declared_function_cached(&mut self, name: &str) -> bool {
        if self.declared_fn_cache_gen != self.fn_resolve_gen {
            self.declared_fn_cache.clear();
            self.declared_fn_cache_gen = self.fn_resolve_gen;
        }
        let key = self.bare_name_ctx_key(Symbol::intern(name));
        if let Some(&cached) = self.declared_fn_cache.get(&key) {
            return cached;
        }
        let result = self.has_declared_function(name);
        self.declared_fn_cache.insert(key, result);
        result
    }

    /// Cached [`Self::has_multi_function`]; guarded by `fn_resolve_gen`, with
    /// the package context in the key. The uncached probe resolves EVERY
    /// registry function key to a `String` and prefix-compares it, per call.
    pub(crate) fn has_multi_function_cached(&mut self, name: &str) -> bool {
        if self.multi_fn_cache_gen != self.fn_resolve_gen {
            self.multi_fn_cache.clear();
            self.multi_fn_cache_gen = self.fn_resolve_gen;
        }
        let key = self.bare_name_ctx_key(Symbol::intern(name));
        if let Some(&cached) = self.multi_fn_cache.get(&key) {
            return cached;
        }
        let result = self.has_multi_function(name);
        self.multi_fn_cache.insert(key, result);
        result
    }

    /// Try compiled function dispatch first, then native, then on-the-fly compile,
    /// then interpreter fallback. Returns the result of whichever path succeeds.
    pub(super) fn call_function_compiled_first(
        &mut self,
        name: &str,
        args: Vec<Value>,
        compiled_fns: &CompiledFns,
    ) -> Result<Value, RuntimeError> {
        if let Some(cf) = self.find_compiled_function(compiled_fns, name, &args) {
            let pkg = self.current_package().to_string();
            // Prefer the routine's own nested-sub table over the caller's
            // (ADR-0019 C6e-3c) — see `compile_and_call_function_def`.
            let fns = cf.compiled_fns.as_deref().unwrap_or(compiled_fns);
            return self.call_compiled_function_named(cf, args, fns, &pkg, name);
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
            // Prefer the cross-thread shared captured body for a `state`-bearing
            // module sub so its `state` cell stays shared across threads (the
            // per-call OTF recompile below gives each thread a distinct cell).
            if let Some(shared) = self.imported_state_body_for_def(&def) {
                let pkg = self.current_package().to_string();
                return self.call_shared_state_body(&shared, args, compiled_fns, &pkg, name);
            }
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
            return self.call_infix_routine(Self::normalize_unicode_infix(op), &args);
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
    pub(crate) fn otf_compile_function_def(
        &mut self,
        def: &crate::ast::FunctionDef,
    ) -> Arc<CompiledFunction> {
        let fingerprint = def.body_fingerprint();
        // The key discriminates (body, defining package), and both halves are
        // already integers: the fingerprint is memoized on the def, and the
        // package is an interned `Symbol`, so its id identifies its string
        // exactly. Mixing them is a couple of ALU ops.
        //
        // It used to SipHash the fingerprint plus a freshly allocated package
        // `String`. That is per *call* for a caller with no plan-attached
        // bytecode — a user `infix:` operator in a reduce recompiled nothing but
        // still paid the hash on every step, which profiled as 2.6% of a
        // `[mm] 1 .. 200` run (ADR-0019 C6d-1).
        let cache_key = fingerprint ^ (def.package.id() as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15);
        if let Some(cached) = self.otf_compile_cache.get(&cache_key) {
            return cached.clone();
        }
        // L2: process-global content-addressed cache. A spawned task starts with
        // an EMPTY per-interpreter cache, so a spawn-heavy loop (Digest::RIPEMD's
        // per-block `start`) re-OTF-compiled the same sub in every task — each
        // recompile is a fresh `CompiledCode` identity, which resets the chunk's
        // JIT hotness state (`JitCodeState` clones to default) and re-stamps
        // `BEGIN` site memos, so the hot body re-paid the interpreter warmup AND
        // a Cranelift compile per task while never accumulating heat. Body
        // identity is only semantically observable through `state` cells (a
        // per-thread body gives per-thread cells), so `state`-declaring defs
        // stay per-interpreter and everything else shares one body process-wide
        // — the same sharing `imported_compiled_fns` and plan-attached
        // `def.compiled` bodies already do.
        let shareable = !Self::routine_body_facts(def).declares_state;
        if shareable && let Some(cached) = global_otf_cache().lock().unwrap().get(&cache_key) {
            let cached = Arc::clone(cached);
            self.otf_compile_cache
                .insert(cache_key, Arc::clone(&cached));
            return cached;
        }
        let pkg = def.package.resolve();
        // `compiler` compiles ONLY `def`'s own body, so anything it accumulates
        // in `compiled_functions` is exactly this routine's own nested-sub
        // subtree — attach it below so a later detached-value call of this
        // routine can resolve its nested `RegisterSub` keys (ADR-0019 C6e-3c).
        let (cc, own_compiled_fns) = {
            let mut compiler = crate::compiler::Compiler::new();
            if !pkg.is_empty() && pkg != "GLOBAL" {
                compiler.set_current_package(pkg.to_string());
            }
            // Resolve $?DISTRIBUTION from the function's defining package (or an
            // enclosing module's distribution for a nested package / role method).
            compiler.current_distribution = self.resolve_package_distribution(&pkg);
            let cc = compiler.compile_routine_closure_body(
                &def.params,
                &def.param_defs,
                &def.body,
                def.is_rw || def.is_raw,
            );
            (cc, compiler.take_compiled_functions())
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
            source_file: def.source_file.clone(),
            params: def.params.clone(),
            param_defs: def.param_defs.clone(),
            return_type: def.return_type.clone(),
            fingerprint,
            empty_sig: def.empty_sig,
            is_rw: def.is_rw,
            is_raw: def.is_raw,
            is_cached: def.is_cached,
            param_local_slots: None,
            has_inner_subs: false,
            declares_inner_routines: false,
            named_call_plan: None,
            deprecated_info,
            declared_locals: None,
            param_name_syms: Vec::new(),
            param_fast_types: Vec::new(),
            param_itemize_on_bind: Vec::new(),
            return_fast_type: None,
            package: pkg.clone(),
            compiled_fns: (!own_compiled_fns.is_empty())
                .then(|| std::sync::Arc::new(own_compiled_fns)),
            memo_cache: std::sync::Arc::new(std::sync::Mutex::new(Vec::new())),
            package_sym_cache: std::sync::OnceLock::new(),
            source_file_sym_cache: std::sync::OnceLock::new(),
        };
        cf.precompute_param_local_slots();
        cf.precompute_named_call_plan();
        cf.precompute_param_name_syms();
        cf.detect_inner_subs();
        cf.compute_declared_locals();
        let cf = Arc::new(cf);
        // Publish through the global cache first and keep its winner: two tasks
        // racing the same compile converge on ONE body identity, so later tasks
        // (and the JIT hotness counters on that body) all share it.
        let cf = if shareable {
            Arc::clone(
                global_otf_cache()
                    .lock()
                    .unwrap()
                    .entry(cache_key)
                    .or_insert(cf),
            )
        } else {
            cf
        };
        self.otf_compile_cache.insert(cache_key, Arc::clone(&cf));
        cf
    }

    pub(crate) fn compile_and_call_function_def(
        &mut self,
        def: &crate::ast::FunctionDef,
        args: Vec<Value>,
        compiled_fns: &CompiledFns,
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

        let (cf, compiled_from_plan) = match &def.compiled {
            Some(compiled) => (Arc::clone(compiled), true),
            None => (self.otf_compile_function_def(def), false),
        };

        // Cache by name for fast lookup in exec_call_func_op — but never for a
        // multi name. The name-keyed cache is type-blind, so caching one multi
        // candidate under the bare name would make a later call with different
        // argument types wrongly reuse it. Multi candidates are still cached by
        // body fingerprint in `otf_compile_cache` above (safe, per-candidate).
        if !compiled_from_plan && !self.has_multi_candidates_cached(&name) {
            let name_sym = Symbol::intern(&name);
            // Keyed to the *callsite* package, not `def.package`: the cache
            // answers "what does this bare name mean here", and a module's
            // non-exported sub means nothing outside its own package. The
            // defining package rides along so a cache hit runs the body under
            // the same package this (uncached) call does.
            let cur_pkg_sym = self.current_package_sym();
            self.otf_call_cache
                .insert(name_sym, (cur_pkg_sym, def.package, Arc::clone(&cf)));
            self.otf_call_cache_gen = self.fn_resolve_gen;
        }

        // Set up samewith and multi-dispatch context that call_compiled_function_named
        // expects the caller to manage (mirrors exec_call_fn_op).
        self.push_samewith_context(&name, None, None);
        let pushed_dispatch = loan_env!(self, push_multi_dispatch_frame(&name, &args));

        // Prefer the routine's own nested-sub table over the caller's: a
        // caller with no table of its own (e.g. `sub EXPORT` dispatch) must
        // still resolve `cf`'s own nested `RegisterSub` keys (ADR-0019
        // C6e-3c), and a plan-compiled/OTF-compiled `cf` always carries the
        // table it was compiled alongside.
        let fns = cf.compiled_fns.as_deref().unwrap_or(compiled_fns);
        let result = self.call_compiled_function_named(&cf, args, fns, &pkg, &name);

        self.pop_samewith_context();
        if pushed_dispatch {
            self.pop_multi_dispatch();
        }

        result
    }

    /// Call an *already resolved* routine definition as bytecode, from a runtime
    /// caller that owns no `CompiledFns` table of its own — a user-defined
    /// operator, a reduce or hyper step over one, or `MAIN`.
    ///
    /// This replaced the interpreter entry `call_function_def`, whose body run was
    /// `run_block(&def.body)`: not a tree walk, but a fresh compile of the
    /// routine's AST on *every* call (ADR-0019 C6d-1). It runs the bytecode the
    /// declaration plan already attached to the routine, falling back to one
    /// memoized on-the-fly compile when the plan attached none.
    ///
    /// Deliberately NOT `compile_and_call_function_def`, for a weaker version of
    /// the reason the multi-deferral caller avoids it: that entry pushes a samewith
    /// context and a fresh *multi-dispatch frame*, and building a candidate list per
    /// call is pure overhead here, because the caller has already resolved the
    /// candidate it wants. A/B'd on a reduce over a two-candidate user `infix:`, it
    /// cost measurably more than `call_compiled_function_named` — the entry just
    /// below that setup, which also matches the semantics of the interpreter entry
    /// this replaces exactly (`call_function_def` pushed neither stack).
    pub(crate) fn call_routine_def(
        &mut self,
        def: &crate::ast::FunctionDef,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // `as_str`, not `resolve`: the latter allocates a `String` per call.
        let pkg = def.package.as_str();
        let name = def.name.as_str();
        let cf = match &def.compiled {
            Some(compiled) => Arc::clone(compiled),
            None => self.otf_compile_function_def(def),
        };
        // Prefer the routine's own nested-sub table (ADR-0019 C6e-3c) over an
        // empty one: this caller owns no `CompiledFns` of its own to offer.
        let empty_fns = CompiledFns::default();
        let fns = cf.compiled_fns.as_deref().unwrap_or(&empty_fns);
        self.call_compiled_function_named(&cf, args, fns, pkg, name)
    }

    /// Check if a function name is handled by the interpreter's Rust code
    /// rather than by compiling its AST body. This includes test functions
    /// (implemented in runtime/test_functions.rs), internal `__mutsu_*` functions,
    /// and pseudo-package qualified names that need special resolution.
    pub(super) fn is_interpreter_handled_function(&self, name: &str) -> bool {
        // Test functions are implemented as Rust methods, not via AST.
        // Under MUTSU_REAL_TEST=1 the real Test.rakumod / Test::Util functions are
        // proper user-defined Raku subs and must be resolved through the normal
        // compiled-function path first; the native handlers are only the fallback
        // when no user declaration exists (try_native_test_function checks this).
        if self.test_mode_active()
            && !crate::runtime::Interpreter::real_test_module_enabled()
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

    /// Whether a name that reaches the interpreter does so as a *carrier* rather
    /// than as a tree-walk fallback. `EVAL`/`EVALFILE` compile their source to
    /// bytecode and run it on a sub-Interpreter (`eval_block_value` -> `run_compiled_block`);
    /// pseudo-package reads (`CALLER::`/`OUTER::`/`SETTING::`/`DYNAMIC::`) are
    /// reflective env lookups. Neither tree-walks user code, so they are counted
    /// in a separate stats bucket (lever A). The remaining coupling — that the
    /// shared env/classes/roles registries are owned by the `Interpreter` struct
    /// — is a lever B (state ownership) concern, not a dispatch fallback.
    ///
    /// The re-dispatch primitives (`samewith`/`callsame`/`nextsame`/`callwith`/
    /// `nextwith`/`nextcallee`/`lastcall`) are carriers too: each re-enters normal
    /// dispatch (`call_function` / `call_sub_value` / `call_method_with_values`) to
    /// run the *next / same* candidate, and that candidate body runs **compiled**
    /// (function OTF and method-execution are both VM-native — ledger §D). So the
    /// interpreter carries the re-dispatch primitive but does not tree-walk the
    /// user code it redispatches to; counting them as tree-walk fallbacks overstated
    /// the §2 function-fallback metric.
    pub(super) fn is_interpreter_carrier_function(name: &str) -> bool {
        matches!(
            name,
            "EVAL"
                | "EVALFILE"
                | "samewith"
                | "callsame"
                | "nextsame"
                | "callwith"
                | "nextwith"
                | "nextcallee"
                | "lastcall"
        ) || name.contains("SETTING::")
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
