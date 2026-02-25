use super::*;
use crate::ast::FunctionDef;

impl Interpreter {
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
        let rw_bindings = self.bind_function_args_values(&def.param_defs, &def.params, &args)?;
        let pushed_assertion = self.push_test_assertion_context(def.is_test_assertion);
        self.routine_stack
            .push((def.package.clone(), def.name.clone()));
        let result = self.run_block(&def.body);
        self.routine_stack.pop();
        self.pop_test_assertion_context(pushed_assertion);
        let implicit_return = self.env.get("_").cloned();
        let mut restored_env = saved_env;
        self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
        self.merge_sigilless_alias_writes(&mut restored_env, &self.env);
        self.env = restored_env;
        match result {
            Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
            Err(e) => Err(e),
            Ok(()) => Ok(implicit_return.unwrap_or(Value::Nil)),
        }
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
                    let rw_bindings =
                        self.bind_function_args_values(&def.param_defs, &def.params, &args)?;
                    let pushed_assertion = self.push_test_assertion_context(def.is_test_assertion);
                    self.routine_stack
                        .push((def.package.clone(), def.name.clone()));
                    let result = self.run_block(&def.body);
                    self.routine_stack.pop();
                    self.pop_test_assertion_context(pushed_assertion);
                    let mut restored_env = saved_env;
                    self.apply_rw_bindings_to_env(&rw_bindings, &mut restored_env);
                    self.merge_sigilless_alias_writes(&mut restored_env, &self.env);
                    self.env = restored_env;
                    match result {
                        Err(e) if e.return_value.is_some() => {}
                        Err(e) => return Err(e),
                        Ok(_) => {}
                    }
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
