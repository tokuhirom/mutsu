//! Method-body compilation, `resolve_code_var`, and smart-match/sequence eval.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Compile a single method/submethod body to bytecode in place if it is not
    /// already compiled. Shared by the bulk registration pass and the on-demand
    /// compile in `run_resolved_method_compiled_or_treewalk`. An empty body is
    /// compiled too (to a trivial body returning Nil/self) so that empty `BUILD`/
    /// `TWEAK`/`method foo {}` stubs no longer fall through to the tree-walk
    /// `run_instance_method_resolved` — leaving only delegation forwarders there.
    ///
    /// Seeds the compiler's distribution context so `$?DISTRIBUTION` inside the
    /// method body resolves to the owning module's distribution (rather than Nil).
    /// The caller resolves the dist via `resolve_package_distribution` before
    /// taking the `&mut` registry borrow.
    pub(crate) fn compile_method_def_in_place_with_dist(
        def: &mut super::MethodDef,
        package_name: &str,
        distribution: Option<Value>,
    ) {
        // A delegation forwarder (`handles`) has a synthesized/empty body and must
        // keep its delegation routing — compiling its empty body would make paths
        // that check `compiled_code.is_some()` run the empty body (returning Nil)
        // instead of forwarding.
        if def.compiled_code.is_some() || def.delegation.is_some() {
            return;
        }
        crate::vm::vm_stats::record_method_body_runtime_compile();
        let mut compiler = crate::compiler::Compiler::new();
        let method_package = def
            .original_role
            .as_deref()
            .or(def.role_origin.as_deref())
            .unwrap_or(package_name);
        compiler.set_current_package(method_package.to_string());
        compiler.current_distribution = distribution;
        // A method always carries an implicit `*%_` / `*@_` slurpy, so `%_` / `@_`
        // are valid lexicals throughout the body (including a nested signature-less
        // `do {}` block). Mark the method context so the do-block placeholder check
        // permits them.
        compiler.lexically_in_method = true;
        let mut method_params = vec![
            "self".to_string(),
            "__ANON_STATE__".to_string(),
            "?CLASS".to_string(),
            "?ROLE".to_string(),
        ];
        method_params.extend(def.params.iter().cloned());
        let mut cc =
            compiler.compile_routine_closure_body(&method_params, &def.param_defs, &def.body);
        cc.compute_may_capture_outer_vars();
        cc.compute_needs_env_sync();
        def.compiled_code = Some(std::sync::Arc::new(cc));
        // Carry forward any CompiledFunction produced while compiling the body
        // (e.g. a `sub` declared inside the method) so dispatch can resolve its
        // routine key at call time instead of seeing an empty functions table.
        if !compiler.compiled_functions_ref().is_empty() {
            def.compiled_fns = Some(std::sync::Arc::new(compiler.take_compiled_functions()));
        }
    }

    fn compile_methods_for_map(
        methods: &mut HashMap<String, Vec<super::MethodDef>>,
        package_name: &str,
        distribution: Option<Value>,
    ) {
        for overloads in methods.values_mut() {
            for def in overloads.iter_mut() {
                Self::compile_method_def_in_place_with_dist(
                    def,
                    package_name,
                    distribution.clone(),
                );
            }
        }
    }

    /// Check all methods in a class for assignment to native-typed read-only
    /// parameters. Returns the first error found, or None.
    pub(crate) fn check_class_native_readonly_param_errors(
        &self,
        class_name: &str,
    ) -> Option<crate::value::RuntimeError> {
        // No user-code re-entry here (only the static compiler check runs), so a
        // let-bound guard is safe.
        let registry = self.registry();
        let class_def = registry.classes.get(class_name)?;
        for overloads in class_def.methods.values() {
            for def in overloads {
                if let Some(err_val) =
                    crate::compiler::Compiler::check_native_readonly_param_assignment(
                        &def.param_defs,
                        &def.body,
                    )
                {
                    let msg = if let ValueView::Instance { attributes, .. } = err_val.view() {
                        attributes
                            .as_map()
                            .get("message")
                            .map(|v| v.to_string_value())
                            .unwrap_or_else(|| "Cannot assign to readonly variable".to_string())
                    } else {
                        "Cannot assign to readonly variable".to_string()
                    };
                    let mut err = crate::value::RuntimeError::new(msg);
                    err.exception = Some(Box::new(err_val));
                    return Some(err);
                }
            }
        }
        None
    }

    /// Compile method bodies for a given class using the bytecode compiler.
    pub(crate) fn compile_class_methods(&mut self, class_name: &str) {
        let dist = self.resolve_package_distribution(class_name);
        if let Some(class_def) = self.registry_mut().classes.get_mut(class_name) {
            Self::compile_methods_for_map(&mut class_def.methods, class_name, dist);
        }
        self.registry_mut().sync_user_method_entries(class_name);
    }

    /// Compile method bodies for a given role.
    pub(crate) fn compile_role_methods(&mut self, role_name: &str) {
        let dist = self.resolve_package_distribution(role_name);
        if let Some(role_def) = self.registry_mut().roles.get_mut(role_name) {
            Self::compile_methods_for_map(&mut role_def.methods, role_name, dist);
        }
    }

    pub(crate) fn smart_match_values(&mut self, left: &Value, right: &Value) -> bool {
        self.smart_match(left, right)
    }

    pub(crate) fn eval_sequence_values(
        &mut self,
        left: Value,
        right: Value,
        exclude_end: bool,
    ) -> Result<Value, RuntimeError> {
        let result = self.eval_sequence(left, right, exclude_end)?;
        // The `...` operator returns a Seq in Raku, not a List/Array.
        // Convert finite Array results to Seq; LazyList results stay as-is.
        if let ValueView::Array(items, _) = result.view() {
            Ok(Value::seq_arc(std::sync::Arc::new(items.to_vec())))
        } else {
            Ok(result)
        }
    }

    /// Build a first-class `Sub` value from a resolved `FunctionDef`, capturing
    /// the current env so the callable outlives its defining scope. Shared by the
    /// operator and ordinary code-var resolution paths.
    ///
    /// The returned `Sub`'s `id` is stabilized to the routine's REGISTRATION
    /// clone id (`__mutsu_callable_id::Pkg::name`, the same env marker
    /// `Self::sub_state_scope_id` already trusts for `state`-variable scoping)
    /// when a registration record is visible, instead of always minting a
    /// fresh id via `next_instance_id()`. Without this, every bareword mention
    /// of `&f` built a brand-new `SubData` with a brand-new id, so `&f.WHICH`
    /// changed on every read and a `.wrap()` chain keyed on one mention's id
    /// (`resolution_call_sub.rs`'s `wrap_chains.get(&data.id)`) was invisible
    /// to a direct call through any OTHER mention
    /// (`todo/tickets/code-var-mention-remakes-the-sub.md`). The clone-id
    /// marker already gives exactly the right granularity for free: it is set
    /// once for a top-level/class-method sub (stable for the program's
    /// lifetime, matching raku), and re-set on every `RegisterSub` execution
    /// for a sub nested inside another routine's body (a fresh id per
    /// invocation of the enclosing routine, also matching raku — verified
    /// against `raku` directly: a nested `my sub` closes fresh per call even
    /// when it captures nothing from the enclosing scope). A def with no
    /// visible registration record (a synthesized/EVAL-installed def, or one
    /// looked up before its `RegisterSub` ran) falls back to today's
    /// fresh-mint behavior, unchanged.
    pub(crate) fn sub_value_from_function_def(&self, def: crate::runtime::FunctionDef) -> Value {
        let mut captured_env = self.env.clone();
        if let Some(ref return_type) = def.return_type {
            captured_env.insert(
                "__mutsu_return_type".to_string(),
                Value::str(return_type.clone()),
            );
        }
        if def.is_method {
            captured_env.insert(
                "__mutsu_callable_type".to_string(),
                Value::str_from("Method"),
            );
        }
        let empty_sig = def.empty_sig;
        let stable_id = self.registration_clone_id(&def.package.resolve(), &def.name.resolve());
        // The routine's own bytecode rides along, so calling this code object runs
        // compiled code instead of re-compiling the AST body copied below
        // (ADR-0019 C6c).
        let compiled_routine = def.compiled.clone();
        let mut sub_val = Value::make_sub_for_routine(
            def.package,
            def.name,
            def.params,
            def.param_defs,
            def.body,
            def.is_rw,
            captured_env,
            compiled_routine,
        );
        // Preserve empty_sig from the FunctionDef (arity checks, e.g. sort
        // rejecting 0-arity callables) and stabilize the id, in one rewrap.
        if (empty_sig || stable_id.is_some())
            && let ValueView::Sub(data) = sub_val.view()
        {
            let mut new_data = (**data).clone();
            new_data.empty_sig = empty_sig;
            if let Some(id) = stable_id {
                new_data.id = id;
            }
            sub_val = Value::sub_value(crate::gc::Gc::new(new_data));
        }
        // Restore any role ever composed onto this routine (`.^mixin(Role)`,
        // or a trait handler's `$r does Role`) — this is a fresh rebuild from
        // the registry, not the same object the composition ran on, so it
        // does not carry the role by itself. See
        // `Interpreter::materialize_routine_mixins_shared`.
        self.materialize_routine_mixins_shared(sub_val, &def.package.resolve(), &def.name.resolve())
    }

    /// The registration clone id for a named routine `package::name`, i.e. the
    /// env marker `__mutsu_callable_id::package::name` that `RegisterSub`
    /// refreshes on every execution (see `Self::sub_state_scope_id`, which
    /// performs the identical lookup keyed off an already-built `SubData`).
    /// `None` when `name` is empty or no registration record is visible from
    /// the current env.
    fn registration_clone_id(&self, package: &str, name: &str) -> Option<u64> {
        if name.is_empty() {
            return None;
        }
        let key = format!("__mutsu_callable_id::{}::{}", package, name);
        self.env
            .get(&key)
            .and_then(|v| v.as_int())
            .filter(|i| *i != 0)
            .map(|i| i as u64)
    }

    pub(crate) fn resolve_code_var(&self, name: &str) -> Value {
        // Handle package-qualified names: strip pseudo-package prefixes and
        // resolve the bare function name.
        let bare_name = Self::strip_pseudo_packages(name);
        let has_packages = bare_name != name;
        // GLOBAL::/OUR:: are package namespaces that do NOT contain CORE symbols,
        // unlike the lexical/core pseudo-packages (CORE, SETTING, MY, OUTER, ...).
        // A builtin name qualified through them is undefined (roast pseudo-6c:
        // `!defined(&GLOBAL::say)`), so suppress the builtin fast-paths below.
        let core_visible = !has_packages || !Self::innermost_pseudo_is_package_only(name);
        // An operator is looked up under its bare categorical name: the scope was
        // already selected by the pseudo-package prefix, which is how
        // `&CALLER::LEXICAL::("infix:<+>")` reaches the built-in operator.
        let normalized_name = Self::normalize_categorical_operator_name(bare_name);
        if core_visible
            && (normalized_name.starts_with("infix:<")
                || normalized_name.starts_with("prefix:<")
                || normalized_name.starts_with("postfix:<"))
            && normalized_name.ends_with('>')
        {
            // A concrete operator sub bound in env — a `my &infix:<op>` binding or
            // one installed by a custom `sub EXPORT` (`Map.new: '&infix:<op>' =>
            // &infix:<op>`) — must win over the by-name GLOBAL routine ref.
            let var_key = format!("&{}", normalized_name);
            if let Some(val) = self.env.get(&var_key) {
                match val.view() {
                    ValueView::WeakSub(weak) => {
                        return match weak.upgrade() {
                            Some(strong) => Value::sub_value(strong),
                            None => Value::NIL,
                        };
                    }
                    ValueView::Sub(_) | ValueView::Instance { .. } | ValueView::Mixin(..) => {
                        return val.clone();
                    }
                    _ => {}
                }
            }
            // A user operator sub with a single concrete def must become a
            // first-class Sub that outlives its defining scope. The by-name GLOBAL
            // routine ref only resolves through `call_function`, which fails once
            // the defining scope (e.g. a custom `sub EXPORT`) is gone, and then
            // re-dispatches the operator by name forever (infinite recursion).
            // Proto/multi operators keep the routine ref so multi-dispatch runs.
            if !self.has_proto(&normalized_name)
                && !self.has_multi_candidates(&normalized_name)
                && let Some(def) = self
                    .resolve_function(&normalized_name)
                    .map(|a| (*a).clone())
            {
                return self.sub_value_from_function_def(def);
            }
            return Value::routine_parts(
                Symbol::intern("GLOBAL"),
                Symbol::intern(&normalized_name),
                false,
            );
        }
        let lookup_name = bare_name.strip_prefix('*').unwrap_or(bare_name);
        if bare_name == "?ROUTINE" {
            // Skip pointy-block entries to find the enclosing routine
            let entry = self
                .routine_stack
                .iter()
                .rev()
                .find(|frame| frame.name != "<pointy-block>");
            if let Some(frame) = entry {
                // Anonymous subs are pushed with "<anon>" as the sentinel name.
                // Return the block_stack Sub directly so callers can invoke it.
                if frame.name.is_empty() || frame.name == "<anon>" {
                    if let Some(val) = self.block_stack.last().cloned()
                        && matches!(val.view(), ValueView::Sub(_))
                    {
                        return val;
                    }
                    return Value::NIL;
                }
                return Value::routine_parts(
                    Symbol::intern(&frame.package),
                    Symbol::intern(&frame.name),
                    false,
                );
            }
            return Value::NIL;
        }
        // When SETTING:: (or similar) pseudo-packages are present, resolve to
        // the builtin directly — these refer to the outer setting scope, not
        // user-defined overrides.
        // When pseudo-package qualifiers are present (e.g. SETTING::), resolve
        // to the builtin directly, bypassing user-defined overrides.
        if has_packages && core_visible && Self::is_builtin_function(lookup_name) {
            return Value::routine_parts(
                Symbol::intern("GLOBAL"),
                Symbol::intern(lookup_name),
                false,
            );
        }
        // For &-sigil private attribute access (e.g. &!m), the attribute
        // value is stored in env as "!m" (not "&!m"), so check directly.
        if bare_name.starts_with('!')
            && let Some(val) = self.env.get(bare_name)
        {
            return val.clone();
        }
        // Check if stored as a variable first (my &f = ...)
        let var_key = format!("&{}", bare_name);
        if let Some(val) = self.env.get(&var_key) {
            // Upgrade WeakSub references (e.g., &?BLOCK) to strong Sub
            if let ValueView::WeakSub(weak) = val.view() {
                return match weak.upgrade() {
                    Some(strong) => Value::sub_value(strong),
                    None => Value::NIL,
                };
            }
            return val.clone();
        }
        // `return` is a control-flow keyword that also resolves as &return
        // so that it can be rebound (proxied return pattern).
        if bare_name == "return" {
            return Value::routine_parts(Symbol::intern("GLOBAL"), Symbol::intern("return"), false);
        }
        // Look up as a function reference (including multi subs).
        // When pseudo-packages are present (e.g. OUR::, GLOBAL::), also check
        // our_scoped_functions for subs defined in EVAL that don't leak into
        // the regular functions map.
        let def = self
            .resolve_function(lookup_name)
            .map(|a| (*a).clone())
            .or_else(|| {
                if has_packages {
                    let fq = format!("{}::{}", self.current_package(), lookup_name);
                    self.registry()
                        .our_scoped_functions
                        .get(&Symbol::intern(&fq))
                        .map(|d| (**d).clone())
                        .or_else(|| {
                            let global_fq = format!("GLOBAL::{}", lookup_name);
                            self.registry()
                                .our_scoped_functions
                                .get(&Symbol::intern(&global_fq))
                                .map(|d| (**d).clone())
                        })
                } else {
                    None
                }
            });
        let is_multi = if def.is_none() && !self.has_proto(lookup_name) {
            // Check if there are multi-dispatch variants (stored with arity/type suffixes)
            let prefix_local = format!("{}::{}/", self.current_package(), lookup_name);
            let prefix_global = format!("GLOBAL::{}/", lookup_name);
            self.registry().functions.keys().any(|k| {
                let ks = k.resolve();
                ks.starts_with(&prefix_local) || ks.starts_with(&prefix_global)
            })
        } else {
            false
        };
        if is_multi {
            // Multi subs: create a Sub that captures all candidates so the
            // callable works even after the defining scope exits.
            let candidates = self.resolve_all_multi_candidates(lookup_name);
            let mut candidate_subs = Vec::new();
            for cand in &candidates {
                let captured_env = self.env.clone();
                let sub_val = Value::make_sub_for_routine(
                    cand.package,
                    cand.name,
                    cand.params.clone(),
                    cand.param_defs.clone(),
                    cand.body.clone(),
                    cand.is_rw,
                    captured_env,
                    cand.compiled.clone(),
                );
                candidate_subs.push(sub_val);
            }
            let mut dispatcher_env = self.env.clone();
            dispatcher_env.insert(
                "__mutsu_multi_dispatch_candidates".to_string(),
                Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(candidate_subs)),
                    crate::value::ArrayKind::List,
                ),
            );
            dispatcher_env.insert(
                "__mutsu_multi_dispatch_name".to_string(),
                Value::str(lookup_name.to_string()),
            );
            Value::make_sub(
                Symbol::intern(&self.current_package()),
                Symbol::intern(lookup_name),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                false,
                dispatcher_env,
            )
        } else if self.has_proto(lookup_name)
            || self.resolve_token_defs(lookup_name).is_some()
            || self.has_proto_token(lookup_name)
        {
            Value::routine_parts(
                Symbol::intern(&self.current_package()),
                Symbol::intern(lookup_name),
                self.resolve_token_defs(lookup_name).is_some() || self.has_proto_token(lookup_name),
            )
        } else if let Some(def) = def {
            self.sub_value_from_function_def(def)
        } else if core_visible && Self::is_builtin_function(lookup_name) {
            Value::routine_parts(Symbol::intern("GLOBAL"), Symbol::intern(lookup_name), false)
        } else if Self::is_mop_macro_function(lookup_name) {
            // The MOP pseudo-methods `WHAT`/`HOW`/`VAR` are also first-class
            // callables in Raku — `&WHAT`, `.map(&WHAT)`, `my $f = &WHAT` — not
            // just call-syntax macros. They dispatch through the builtin-function
            // path (see `builtins.rs`), so expose them as Routine values here.
            Value::routine_parts(Symbol::intern("GLOBAL"), Symbol::intern(lookup_name), false)
        } else if self.test_module_loaded() && Self::is_test_function_name(lookup_name) {
            // Test-framework functions (&is-deeply, &pass, ...) are implemented
            // as Rust methods (runtime/test_functions.rs), not declared subs, so
            // the function-def lookup above misses them. Expose them as Routine
            // values so `my &fn = &is-deeply; fn(...)` dispatches through the
            // Routine call path, which routes test names to the Test dispatcher.
            Value::routine_parts(Symbol::intern("GLOBAL"), Symbol::intern(lookup_name), false)
        } else if self.json_module_loaded() && matches!(lookup_name, "to-json" | "from-json") {
            // Native JSON routines (runtime/json.rs) have no declared sub either;
            // expose them as Routines so `&from-json` / `$str.&from-json` work.
            // The Routine call path falls through to try_native_json_function.
            Value::routine_parts(Symbol::intern("GLOBAL"), Symbol::intern(lookup_name), false)
        } else if bare_name.starts_with('*') {
            // Dynamic code vars (&*foo) can point to routines that are resolved
            // at call time (including builtins not listed in is_builtin_function).
            Value::routine_parts(Symbol::intern("GLOBAL"), Symbol::intern(lookup_name), false)
        } else {
            Value::NIL
        }
    }

    /// True when the innermost (last) stripped pseudo-package prefix is a
    /// package namespace (GLOBAL/OUR) rather than a lexical/core scope. Such
    /// namespaces do not contain CORE symbols, so a builtin name qualified
    /// through them is undefined (roast pseudo-6c: `!defined(&GLOBAL::say)`).
    /// `GLOBAL::CORE::not` still sees CORE because CORE is the innermost prefix.
    fn innermost_pseudo_is_package_only(name: &str) -> bool {
        let pseudo = [
            "SETTING", "CALLER", "CALLERS", "OUTER", "OUTERS", "CORE", "GLOBAL", "LEXICAL", "MY",
            "OUR", "DYNAMIC", "UNIT",
        ];
        let mut rest = name;
        let mut last: Option<&str> = None;
        loop {
            let mut found = false;
            for pkg in &pseudo {
                if let Some(after) = rest.strip_prefix(pkg)
                    && let Some(after) = after.strip_prefix("::")
                {
                    rest = after;
                    last = Some(pkg);
                    found = true;
                    break;
                }
            }
            if !found {
                break;
            }
        }
        matches!(last, Some("GLOBAL") | Some("OUR"))
    }

    /// Strip pseudo-package prefixes (SETTING::, OUTER::, CALLER::, CORE::, etc.)
    /// from a qualified name and return the final bare function name.
    pub(crate) fn strip_pseudo_packages(name: &str) -> &str {
        let pseudo = [
            "SETTING", "CALLER", "CALLERS", "OUTER", "OUTERS", "CORE", "GLOBAL", "LEXICAL", "MY",
            "OUR", "DYNAMIC", "UNIT",
        ];
        let mut rest = name;
        loop {
            let mut found = false;
            for pkg in &pseudo {
                if let Some(after) = rest.strip_prefix(pkg)
                    && let Some(after) = after.strip_prefix("::")
                {
                    rest = after;
                    found = true;
                    break;
                }
            }
            if !found {
                break;
            }
        }
        rest
    }
}
