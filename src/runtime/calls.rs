use super::*;
use crate::ast::FunctionDef;

impl Interpreter {
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
        for stmt in &def.body {
            if let Stmt::VarDecl { name, .. } = stmt {
                names.insert(name.clone());
            }
        }
        names
    }

    /// Call a specific FunctionDef directly, bypassing the built-in function dispatch.
    /// Used for user-defined operator overrides.
    pub(crate) fn call_function_def(
        &mut self,
        def: &FunctionDef,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
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
        let return_spec = self.routine_return_spec_by_name(&def.name);
        let rw_bindings = match self.bind_function_args_values(&def.param_defs, &def.params, &args)
        {
            Ok(bindings) => bindings,
            Err(e) => {
                self.pop_caller_env();
                self.env = saved_env;
                self.restore_readonly_vars(saved_readonly);
                return Err(e);
            }
        };
        // Push Sub value to block_stack so callframe().code works for nested calls
        let sub_val = Value::make_sub(
            def.package.clone(),
            def.name.clone(),
            def.params.clone(),
            def.param_defs.clone(),
            def.body.clone(),
            def.is_rw,
            self.env.clone(),
        );
        self.block_stack.push(sub_val);
        let pushed_assertion = self.push_test_assertion_context(def.is_test_assertion);
        self.routine_stack
            .push((def.package.clone(), def.name.clone()));
        self.prepare_definite_return_slot(return_spec.as_deref());
        let result = self.run_block(&def.body);
        self.routine_stack.pop();
        self.block_stack.pop();
        self.pop_test_assertion_context(pushed_assertion);
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
        let finalized = self.finalize_return_with_spec(call_result, return_spec.as_deref());
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
                self.env.insert("made".to_string(), value);
            }
            "made" => {
                let _ = self.env.get("made");
            }
            _ => {
                let def_opt = self.resolve_function_with_alias(name, &args);
                if let Some(def) = def_opt {
                    if def.empty_sig && !args.is_empty() {
                        return Err(Self::reject_args_for_empty_sig(&args));
                    }
                    let saved_env = self.env.clone();
                    let saved_readonly = self.save_readonly_vars();
                    if let Some(line) = self.test_pending_callsite_line {
                        self.env.insert("?LINE".to_string(), Value::Int(line));
                    }
                    self.push_caller_env();
                    let return_spec = self.routine_return_spec_by_name(&def.name);
                    let rw_bindings =
                        match self.bind_function_args_values(&def.param_defs, &def.params, &args) {
                            Ok(bindings) => bindings,
                            Err(e) => {
                                self.pop_caller_env();
                                self.env = saved_env;
                                self.restore_readonly_vars(saved_readonly);
                                return Err(e);
                            }
                        };
                    let sub_val = Value::make_sub(
                        def.package.clone(),
                        def.name.clone(),
                        def.params.clone(),
                        def.param_defs.clone(),
                        def.body.clone(),
                        def.is_rw,
                        self.env.clone(),
                    );
                    self.block_stack.push(sub_val);
                    let pushed_assertion = self.push_test_assertion_context(def.is_test_assertion);
                    self.routine_stack
                        .push((def.package.clone(), def.name.clone()));
                    self.prepare_definite_return_slot(return_spec.as_deref());
                    let result = self.run_block(&def.body);
                    self.routine_stack.pop();
                    self.block_stack.pop();
                    self.pop_test_assertion_context(pushed_assertion);
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
                    self.env = restored_env;
                    self.restore_readonly_vars(saved_readonly);
                    let call_result = match result {
                        Ok(()) => Ok(implicit_return),
                        Err(e) => Err(e),
                    };
                    self.finalize_return_with_spec(call_result, return_spec.as_deref())?;
                } else if self.has_proto(name) {
                    return Err(RuntimeError::new(format!(
                        "No matching candidates for proto sub: {}",
                        name
                    )));
                } else {
                    return Err(RuntimeError::new(format!("Unknown call: {}", name)));
                }
            }
        }
        Ok(())
    }
}
