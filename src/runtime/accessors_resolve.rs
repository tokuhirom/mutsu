//! Method-body compilation, `resolve_code_var`, and smart-match/sequence eval.
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Compile all uncompiled method bodies in a method map.
    /// Compile a single method/submethod body to bytecode in place if it is not
    /// already compiled. Shared by the bulk registration pass and the on-demand
    /// compile in `run_resolved_method_compiled_or_treewalk`. An empty body is
    /// compiled too (to a trivial body returning Nil/self) so that empty `BUILD`/
    /// `TWEAK`/`method foo {}` stubs no longer fall through to the tree-walk
    /// `run_instance_method_resolved` — leaving only delegation forwarders there.
    pub(crate) fn compile_method_def_in_place(def: &mut super::MethodDef, package_name: &str) {
        // A delegation forwarder (`handles`) has a synthesized/empty body and must
        // keep its delegation routing — compiling its empty body would make paths
        // that check `compiled_code.is_some()` run the empty body (returning Nil)
        // instead of forwarding.
        if def.compiled_code.is_some() || def.delegation.is_some() {
            return;
        }
        let mut compiler = crate::compiler::Compiler::new();
        let method_package = def
            .original_role
            .as_deref()
            .or(def.role_origin.as_deref())
            .unwrap_or(package_name);
        compiler.set_current_package(method_package.to_string());
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
    }

    fn compile_methods_for_map(
        methods: &mut HashMap<String, Vec<super::MethodDef>>,
        package_name: &str,
    ) {
        for overloads in methods.values_mut() {
            for def in overloads.iter_mut() {
                Self::compile_method_def_in_place(def, package_name);
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
                    let msg = if let Value::Instance { attributes, .. } = &err_val {
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
        if let Some(class_def) = self.registry_mut().classes.get_mut(class_name) {
            Self::compile_methods_for_map(&mut class_def.methods, class_name);
        }
    }

    /// Compile method bodies for a given role.
    pub(crate) fn compile_role_methods(&mut self, role_name: &str) {
        if let Some(role_def) = self.registry_mut().roles.get_mut(role_name) {
            Self::compile_methods_for_map(&mut role_def.methods, role_name);
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
        match result {
            Value::Array(items, _) => Ok(Value::Seq(std::sync::Arc::new(items.to_vec()))),
            other => Ok(other),
        }
    }

    pub(crate) fn resolve_code_var(&self, name: &str) -> Value {
        let normalized_name = Self::normalize_categorical_operator_name(name);
        if (normalized_name.starts_with("infix:<")
            || normalized_name.starts_with("prefix:<")
            || normalized_name.starts_with("postfix:<"))
            && normalized_name.ends_with('>')
        {
            return Value::Routine {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern(&normalized_name),
                is_regex: false,
            };
        }
        // Handle package-qualified names: strip pseudo-package prefixes and
        // resolve the bare function name.
        let bare_name = Self::strip_pseudo_packages(name);
        let has_packages = bare_name != name;
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
                        && matches!(val, Value::Sub(_))
                    {
                        return val;
                    }
                    return Value::Nil;
                }
                return Value::Routine {
                    package: Symbol::intern(&frame.package),
                    name: Symbol::intern(&frame.name),
                    is_regex: false,
                };
            }
            return Value::Nil;
        }
        // When SETTING:: (or similar) pseudo-packages are present, resolve to
        // the builtin directly — these refer to the outer setting scope, not
        // user-defined overrides.
        // When pseudo-package qualifiers are present (e.g. SETTING::), resolve
        // to the builtin directly, bypassing user-defined overrides.
        if has_packages && Self::is_builtin_function(lookup_name) {
            return Value::Routine {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern(lookup_name),
                is_regex: false,
            };
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
            if let Value::WeakSub(weak) = val {
                return match weak.upgrade() {
                    Some(strong) => Value::Sub(strong),
                    None => Value::Nil,
                };
            }
            return val.clone();
        }
        // `return` is a control-flow keyword that also resolves as &return
        // so that it can be rebound (proxied return pattern).
        if bare_name == "return" {
            return Value::Routine {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern("return"),
                is_regex: false,
            };
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
                let sub_val = Value::make_sub(
                    cand.package,
                    cand.name,
                    cand.params.clone(),
                    cand.param_defs.clone(),
                    cand.body.clone(),
                    cand.is_rw,
                    captured_env,
                );
                candidate_subs.push(sub_val);
            }
            let mut dispatcher_env = self.env.clone();
            dispatcher_env.insert(
                "__mutsu_multi_dispatch_candidates".to_string(),
                Value::Array(
                    std::sync::Arc::new(crate::value::ArrayData::new(candidate_subs)),
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
            Value::Routine {
                package: Symbol::intern(&self.current_package()),
                name: Symbol::intern(lookup_name),
                is_regex: self.resolve_token_defs(lookup_name).is_some()
                    || self.has_proto_token(lookup_name),
            }
        } else if let Some(def) = def {
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
            let mut sub_val = Value::make_sub(
                def.package,
                def.name,
                def.params,
                def.param_defs,
                def.body,
                def.is_rw,
                captured_env,
            );
            // Preserve empty_sig from the FunctionDef so that arity checks
            // (e.g. sort rejecting 0-arity callables) work correctly.
            if empty_sig && let Value::Sub(ref data) = sub_val {
                let mut new_data = (**data).clone();
                new_data.empty_sig = true;
                sub_val = Value::Sub(Arc::new(new_data));
            }
            sub_val
        } else if Self::is_builtin_function(lookup_name) {
            Value::Routine {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern(lookup_name),
                is_regex: false,
            }
        } else if bare_name.starts_with('*') {
            // Dynamic code vars (&*foo) can point to routines that are resolved
            // at call time (including builtins not listed in is_builtin_function).
            Value::Routine {
                package: Symbol::intern("GLOBAL"),
                name: Symbol::intern(lookup_name),
                is_regex: false,
            }
        } else {
            Value::Nil
        }
    }

    /// Strip pseudo-package prefixes (SETTING::, OUTER::, CALLER::, CORE::, etc.)
    /// from a qualified name and return the final bare function name.
    pub(crate) fn strip_pseudo_packages(name: &str) -> &str {
        let pseudo = [
            "SETTING", "CALLER", "OUTER", "CORE", "GLOBAL", "MY", "OUR", "DYNAMIC", "UNIT",
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
