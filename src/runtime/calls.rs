use super::*;
use crate::ast::FunctionDef;

impl Interpreter {
    /// Check if a function has the `is DEPRECATED` trait and record a deprecation event.
    pub(crate) fn check_deprecation_for_def(&self, def: &FunctionDef) {
        if let Some(ref msg) = def.deprecated_message {
            let file = self
                .env
                .get("*PROGRAM-NAME")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let line = self
                .env
                .get("?LINE")
                .and_then(|v| match v {
                    Value::Int(i) => Some(*i),
                    _ => None,
                })
                .unwrap_or(0);
            let kind = if def.is_method { "Method" } else { "Sub" };
            let pkg = def.package.resolve();
            super::deprecation::record_deprecation(
                kind,
                &def.name.resolve(),
                &pkg,
                msg,
                &file,
                line,
            );
        }
    }

    /// Check deprecation for a method call using name, package, and message.
    pub(crate) fn check_deprecation_for_method(&self, name: &str, package: &str, message: &str) {
        let file = self
            .env
            .get("*PROGRAM-NAME")
            .map(|v| v.to_string_value())
            .unwrap_or_default();
        let line = self
            .env
            .get("?LINE")
            .and_then(|v| match v {
                Value::Int(i) => Some(*i),
                _ => None,
            })
            .unwrap_or(0);
        super::deprecation::record_deprecation("Method", name, package, message, &file, line);
    }

    pub(crate) fn routine_writeback_excluded_names(
        def: &FunctionDef,
    ) -> std::collections::HashSet<String> {
        let mut names: std::collections::HashSet<String> = def
            .param_defs
            .iter()
            .filter_map(|pd| {
                if pd.name.is_empty() || pd.name.starts_with('@') || pd.name.starts_with('%') {
                    None
                } else if let Some(name) = pd.name.strip_prefix(':') {
                    Some(name.to_string())
                } else {
                    Some(pd.name.clone())
                }
            })
            .collect();
        // Also exclude sub_signature parameter names (array unpacking) so that
        // recursive calls don't write back inner-scope values to the caller.
        for pd in &def.param_defs {
            Self::collect_sub_signature_names(&pd.sub_signature, &mut names);
        }
        for stmt in &def.body {
            if let Stmt::VarDecl { name, .. } = stmt {
                names.insert(name.clone());
            }
        }
        names
    }

    /// Recursively collect variable names from sub_signature parameters.
    fn collect_sub_signature_names(
        sub_sig: &Option<Vec<crate::ast::ParamDef>>,
        names: &mut std::collections::HashSet<String>,
    ) {
        if let Some(params) = sub_sig {
            for sp in params {
                if !sp.name.is_empty() {
                    names.insert(sp.name.clone());
                }
                Self::collect_sub_signature_names(&sp.sub_signature, names);
            }
        }
    }

    /// Call a specific FunctionDef directly, bypassing the built-in function dispatch.
    /// Used for user-defined operator overrides.
    pub(crate) fn call_function_def(
        &mut self,
        def: &FunctionDef,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.check_deprecation_for_def(def);
        let (args, callsite_line) = self.sanitize_call_args(args);
        self.test_pending_callsite_line = callsite_line;
        if def.empty_sig && !args.is_empty() {
            return Err(Self::reject_args_for_empty_sig(&args));
        }
        let saved_env = self.env.clone();
        let saved_readonly = self.save_readonly_vars();
        if let Some(line) = self.test_pending_callsite_line {
            self.env.insert("?LINE".to_string(), Value::Int(line));
        }
        self.push_caller_env();
        // Set current_package to the function's defining package so that default
        // value expressions can resolve package-scoped functions (e.g. &double).
        let saved_package = self.current_package().to_string();
        let def_package = def.package.resolve();
        if !def_package.is_empty() && def_package != "GLOBAL" {
            self.set_current_package(def_package);
        }
        let return_spec = self.routine_return_spec_by_name(&def.name.resolve());
        let rw_bindings = match self.bind_function_args_values(&def.param_defs, &def.params, &args)
        {
            Ok(bindings) => bindings,
            Err(e) => {
                self.set_current_package(saved_package);
                self.pop_caller_env();
                self.env = saved_env;
                self.restore_readonly_vars(saved_readonly);
                return Err(Self::enhance_binding_error(
                    e,
                    &def.name.resolve(),
                    &def.param_defs,
                    &args,
                ));
            }
        };
        // When the function's package is not GLOBAL, the Compiler qualifies
        // bare variable references as $Package::name.  Ensure parameter bindings
        // are also reachable via those qualified names.
        let cur_pkg = self.current_package().to_string();
        if cur_pkg != "GLOBAL" {
            let qualified_aliases: Vec<(String, Value)> = def
                .param_defs
                .iter()
                .filter_map(|pd| {
                    let bare = pd
                        .name
                        .strip_prefix('$')
                        .or_else(|| pd.name.strip_prefix('@'))
                        .or_else(|| pd.name.strip_prefix('%'))
                        .or_else(|| pd.name.strip_prefix('&'));
                    if let Some(bare) = bare {
                        self.env.get(&pd.name).cloned().map(|v| {
                            let sigil = &pd.name[..pd.name.len() - bare.len()];
                            (format!("{}{}::{}", sigil, cur_pkg, bare), v)
                        })
                    } else if !pd.name.is_empty()
                        && !pd.name.starts_with('_')
                        && !pd.name.starts_with('!')
                        && !pd.name.contains("::")
                    {
                        self.env
                            .get(&pd.name)
                            .cloned()
                            .map(|v| (format!("{}::{}", cur_pkg, pd.name), v))
                    } else {
                        None
                    }
                })
                .collect();
            for (key, val) in qualified_aliases {
                self.env.insert(key, val);
            }
        }
        // Push Sub value to block_stack so callframe().code works for nested calls
        let sub_val = Value::make_sub(
            def.package,
            def.name,
            def.params.clone(),
            def.param_defs.clone(),
            def.body.clone(),
            def.is_rw,
            self.env.clone(),
        );
        self.block_stack.push(sub_val);
        let pushed_assertion = self.push_test_assertion_context(def.is_test_assertion);
        self.routine_stack
            .push((def.package.resolve(), def.name.resolve()));
        // Set __mutsu_callable_id so blocks defined inside this routine
        // capture the correct target for non-local return.
        let callable_key = format!("__mutsu_callable_id::{}::{}", def.package, def.name);
        if let Some(Value::Int(id)) = self.env.get(&callable_key).cloned() {
            self.env
                .insert("__mutsu_callable_id".to_string(), Value::Int(id));
        }
        self.prepare_definite_return_slot(return_spec.as_deref());
        let result = self.run_block(&def.body);
        self.routine_stack.pop();
        self.block_stack.pop();
        self.pop_test_assertion_context(pushed_assertion);
        self.set_current_package(saved_package);
        let effective_return_spec = return_spec
            .as_deref()
            .map(|spec| self.resolved_type_capture_name(spec));
        let implicit_return = self.env.get("_").cloned().unwrap_or(Value::Nil);
        let mut restored_env = saved_env;
        self.pop_caller_env_with_writeback(&mut restored_env);
        let excluded_names = Self::routine_writeback_excluded_names(def);
        for (k, v) in self.env.iter() {
            let scalar_writeback = restored_env.contains_key(k)
                && !excluded_names.contains(k)
                && !matches!(
                    v,
                    Value::Array(..)
                        | Value::Hash(..)
                        | Value::Sub(..)
                        | Value::WeakSub(..)
                        | Value::Routine { .. }
                );
            if k != "_"
                && k != "@_"
                && k != "%_"
                && ((restored_env.contains_key(k)
                    && !excluded_names.contains(k)
                    && matches!(v, Value::Array(..) | Value::Hash(..)))
                    || scalar_writeback
                    || k.starts_with("__mutsu_var_meta::"))
            {
                restored_env.insert(k.clone(), v.clone());
            }
        }
        self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        self.merge_sigilless_alias_writes(&mut restored_env, &self.env);
        self.env = restored_env;
        self.restore_readonly_vars(saved_readonly);
        let call_result = match result {
            Ok(()) => Ok(implicit_return),
            Err(e) => Err(e),
        };
        // Non-local return targeting a different callable: propagate without absorbing
        if let Err(ref e) = call_result
            && e.return_value.is_some()
            && let Some(_target_id) = e.return_target_callable_id
        {
            // The callable_id for this function is resolved from env.
            let callable_key = format!("__mutsu_callable_id::{}::{}", def.package, def.name);
            let my_id = self.env.get(&callable_key).and_then(|v| match v {
                Value::Int(i) => Some(*i as u64),
                _ => None,
            });
            if my_id != e.return_target_callable_id {
                return call_result;
            }
        }
        let finalized =
            self.finalize_return_with_spec(call_result, effective_return_spec.as_deref());
        finalized.and_then(|v| self.maybe_fetch_rw_proxy(v, def.is_rw))
    }

    pub(crate) fn exec_call(&mut self, name: &str, args: Vec<Value>) -> Result<(), RuntimeError> {
        let (args, callsite_line) = self.sanitize_call_args(&args);
        self.test_pending_callsite_line = callsite_line;
        // Delegate test functions to the unified test_functions.rs
        if let Some(_result) = self.call_test_function(name, &args)? {
            return Ok(());
        }
        match name {
            "make" => {
                let value = if args.is_empty() {
                    Value::Nil
                } else {
                    Self::positional_value_required(&args, 0, "make expects value")?.clone()
                };
                self.env.insert("made".to_string(), value.clone());
                self.action_made = Some(value);
            }
            "made" => {
                let _ = self.env.get("made");
            }
            _ => {
                // Check wrap chain for named function calls
                if let Some(sub_id) = self.wrap_sub_id_for_name(name)
                    && !self.is_wrap_dispatching(sub_id)
                    && let Some(sub_val) = self.get_wrapped_sub(name)
                {
                    let result = self.call_sub_value(sub_val, args, false)?;
                    self.env.insert("_".to_string(), result);
                    return Ok(());
                }
                let def_opt = self.resolve_function_with_alias(name, &args);
                if let Some(def) = def_opt {
                    self.check_deprecation_for_def(&def);
                    if def.empty_sig && !args.is_empty() {
                        return Err(Self::reject_args_for_empty_sig(&args));
                    }
                    let saved_env = self.env.clone();
                    let saved_readonly = self.save_readonly_vars();
                    if let Some(line) = self.test_pending_callsite_line {
                        self.env.insert("?LINE".to_string(), Value::Int(line));
                    }
                    self.push_caller_env();
                    let saved_package = self.current_package().to_string();
                    let def_package = def.package.resolve();
                    if !def_package.is_empty() && def_package != "GLOBAL" {
                        self.set_current_package(def_package);
                    }
                    let return_spec = self.routine_return_spec_by_name(&def.name.resolve());
                    let rw_bindings =
                        match self.bind_function_args_values(&def.param_defs, &def.params, &args) {
                            Ok(bindings) => bindings,
                            Err(e) => {
                                self.set_current_package(saved_package);
                                self.pop_caller_env();
                                self.env = saved_env;
                                self.restore_readonly_vars(saved_readonly);
                                return Err(Self::enhance_binding_error(
                                    e,
                                    &def.name.resolve(),
                                    &def.param_defs,
                                    &args,
                                ));
                            }
                        };
                    let sub_val = Value::make_sub(
                        def.package,
                        def.name,
                        def.params.clone(),
                        def.param_defs.clone(),
                        def.body.clone(),
                        def.is_rw,
                        self.env.clone(),
                    );
                    self.block_stack.push(sub_val);
                    let pushed_assertion = self.push_test_assertion_context(def.is_test_assertion);
                    self.routine_stack
                        .push((def.package.resolve(), def.name.resolve()));
                    self.prepare_definite_return_slot(return_spec.as_deref());
                    let result = self.run_block(&def.body);
                    self.routine_stack.pop();
                    self.block_stack.pop();
                    self.pop_test_assertion_context(pushed_assertion);
                    self.set_current_package(saved_package);
                    let implicit_return = self.env.get("_").cloned().unwrap_or(Value::Nil);
                    let mut restored_env = saved_env;
                    self.pop_caller_env_with_writeback(&mut restored_env);
                    let excluded_names = Self::routine_writeback_excluded_names(&def);
                    for (k, v) in self.env.iter() {
                        let scalar_writeback = restored_env.contains_key(k)
                            && !excluded_names.contains(k)
                            && !matches!(
                                v,
                                Value::Array(..)
                                    | Value::Hash(..)
                                    | Value::Sub(..)
                                    | Value::WeakSub(..)
                                    | Value::Routine { .. }
                            );
                        if k != "_"
                            && k != "@_"
                            && k != "%_"
                            && ((restored_env.contains_key(k)
                                && matches!(v, Value::Array(..) | Value::Hash(..)))
                                || scalar_writeback
                                || k.starts_with("__mutsu_var_meta::"))
                        {
                            restored_env.insert(k.clone(), v.clone());
                        }
                    }
                    self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
                    self.merge_sigilless_alias_writes(&mut restored_env, &self.env);
                    let effective_return_spec = return_spec
                        .as_deref()
                        .map(|spec| self.resolved_type_capture_name(spec));
                    self.env = restored_env;
                    self.restore_readonly_vars(saved_readonly);
                    let call_result = match result {
                        Ok(()) => Ok(implicit_return),
                        Err(e) => Err(e),
                    };
                    self.finalize_return_with_spec(call_result, effective_return_spec.as_deref())?;
                } else if let Some(err) = self.take_pending_dispatch_error() {
                    return Err(err);
                } else if self.has_proto(name) {
                    // Build a detailed error with call profile and candidate signatures
                    let arg_types: Vec<String> = args
                        .iter()
                        .filter(|a| !matches!(a, Value::Pair(..) | Value::ValuePair(..)))
                        .map(|a| {
                            let tn = super::value_type_name(a);
                            if !matches!(a, Value::Nil) {
                                format!("{}:D", tn)
                            } else {
                                tn.to_string()
                            }
                        })
                        .collect();
                    let call_profile = format!("{}({})", name, arg_types.join(", "));
                    let sig_lines = self.collect_multi_candidate_signatures(name, args.len());
                    let sig_list = if sig_lines.is_empty() {
                        String::new()
                    } else {
                        format!(":\n{}", sig_lines.join("\n"))
                    };
                    let message = format!(
                        "Cannot resolve caller {}; none of these signatures matches{}",
                        call_profile, sig_list
                    );
                    let mut err = RuntimeError::new(format!(
                        "No matching candidates for proto sub: {}",
                        name
                    ));
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("message".to_string(), Value::str(message));
                    err.exception = Some(Box::new(Value::make_instance(
                        Symbol::intern("X::Multi::NoMatch"),
                        attrs,
                    )));
                    return Err(err);
                } else {
                    return Err(RuntimeError::new(format!("Unknown call: {}", name)));
                }
            }
        }
        Ok(())
    }

    /// Enhance a binding error with function name, call profile, and signature info.
    pub(crate) fn enhance_binding_error(
        err: RuntimeError,
        func_name: &str,
        param_defs: &[crate::ast::ParamDef],
        args: &[Value],
    ) -> RuntimeError {
        // Don't enhance errors that are already enhanced or are control flow
        if err.is_return || err.is_last || err.is_next || func_name.is_empty() {
            return err;
        }
        // Build call profile: func_name(Type1, Type2, ...)
        let arg_types: Vec<String> = args
            .iter()
            .filter(|a| {
                !matches!(
                    a,
                    Value::Pair(k, _) if k == "__mutsu_test_callsite_line"
                )
            })
            .filter(|a| !matches!(a, Value::Pair(..) | Value::ValuePair(..)))
            .map(|a| super::value_type_name(a).to_string())
            .collect();
        let call_profile = format!("{}({})", func_name, arg_types.join(", "));

        // Build signature string: (Type $name, ...)
        let sig_parts: Vec<String> = param_defs
            .iter()
            .filter(|pd| !pd.traits.iter().any(|t| t == "invocant"))
            .map(|pd| {
                let sigil = if pd.name.starts_with('@')
                    || pd.name.starts_with('%')
                    || pd.name.starts_with('&')
                {
                    ""
                } else if pd.sigilless {
                    "\\"
                } else {
                    "$"
                };
                if pd.name == "__type_only__" {
                    // Type-only param: show just the type constraint
                    return pd.type_constraint.as_deref().unwrap_or("Any").to_string();
                }
                let name_part = if pd.name == "__ANON_STATE__" {
                    "$".to_string()
                } else {
                    format!("{}{}", sigil, pd.name)
                };
                if let Some(tc) = &pd.type_constraint {
                    format!("{} {}", tc, name_part)
                } else {
                    name_part
                }
            })
            .collect();
        let signature = format!("({})", sig_parts.join(", "));

        // Enhance the error message, preserving the original for exception type matching
        let enhanced_msg = format!(
            "Calling {} will never work with declared signature {}\n  {}",
            call_profile, signature, err.message
        );
        let mut enhanced = RuntimeError::new(enhanced_msg.clone());
        // For binding type-check errors on regular calls, wrap as X::TypeCheck::Argument
        // Only do this when the error has no existing exception and the message is
        // about a type-only parameter (where the parameter name IS the type constraint),
        // or about arity mismatch.
        let is_arity_error = err.message.contains("Too few positionals passed")
            || err.message.contains("Too many positionals passed");
        // Also detect named parameter type mismatches with simple builtin types
        // (e.g. `sub foo(Int $x) {}; foo("hi")` should be X::TypeCheck::Argument),
        // but NOT when type captures are involved (e.g. `sub foo(::T, T $a, T $b)`)
        // and NOT when the constraint is a user-defined type (subset, class, etc.).
        let has_type_captures = param_defs
            .iter()
            .any(|pd| pd.name.starts_with("::") || pd.name == "__type_capture__");
        let is_type_only_mismatch = err.exception.is_none()
            && !has_type_captures
            && err
                .message
                .contains("X::TypeCheck::Binding::Parameter: Type check failed")
            && param_defs.iter().any(|pd| {
                pd.type_constraint.as_ref().is_some_and(|tc| {
                    matches!(
                        tc.as_str(),
                        "Int"
                            | "Str"
                            | "Bool"
                            | "Num"
                            | "Rat"
                            | "Complex"
                            | "Real"
                            | "Numeric"
                            | "Cool"
                            | "Any"
                            | "Mu"
                            | "IO"
                            | "Regex"
                            | "Callable"
                            | "Positional"
                            | "Associative"
                            | "Range"
                            | "Match"
                            | "Pair"
                            | "List"
                            | "Array"
                            | "Hash"
                            | "Set"
                            | "Bag"
                            | "Mix"
                            | "Junction"
                            | "Seq"
                            | "Supply"
                            | "Promise"
                            | "Channel"
                    )
                })
            });
        if (is_arity_error || is_type_only_mismatch) && err.exception.is_none() {
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("message".to_string(), Value::str(enhanced_msg.clone()));
            attrs.insert("objname".to_string(), Value::str(func_name.to_string()));
            attrs.insert("signature".to_string(), Value::str(signature));
            let arg_type_values: Vec<Value> =
                arg_types.iter().map(|t| Value::str(t.clone())).collect();
            attrs.insert("arguments".to_string(), Value::array(arg_type_values));
            enhanced.exception = Some(Box::new(Value::make_instance(
                crate::symbol::Symbol::intern("X::TypeCheck::Argument"),
                attrs,
            )));
        } else if let Some(ex) = err.exception {
            // Update the exception object's message attribute so $! shows the enhanced message
            if let Value::Instance {
                class_name,
                ref attributes,
                ..
            } = *ex
            {
                let mut new_attrs: std::collections::HashMap<String, Value> = attributes
                    .iter()
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect();
                new_attrs.insert("message".to_string(), Value::str(enhanced_msg));
                enhanced.exception = Some(Box::new(Value::make_instance(class_name, new_attrs)));
            } else {
                enhanced.exception = Some(ex);
            }
        }
        enhanced.hint = err.hint;
        enhanced
    }
}
