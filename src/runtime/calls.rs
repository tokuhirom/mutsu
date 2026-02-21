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
        let saved_env = self.env.clone();
        self.bind_function_args_values(&def.param_defs, &def.params, args)?;
        self.routine_stack
            .push((def.package.clone(), def.name.clone()));
        let result = self.run_block(&def.body);
        self.routine_stack.pop();
        let implicit_return = self.env.get("_").cloned();
        self.env = saved_env;
        match result {
            Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
            Err(e) => Err(e),
            Ok(()) => Ok(implicit_return.unwrap_or(Value::Nil)),
        }
    }

    pub(crate) fn exec_call(&mut self, name: &str, args: Vec<Value>) -> Result<(), RuntimeError> {
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
                    let saved_env = self.env.clone();
                    self.bind_function_args_values(&def.param_defs, &def.params, &args)?;
                    self.routine_stack
                        .push((def.package.clone(), def.name.clone()));
                    let result = self.run_block(&def.body);
                    self.routine_stack.pop();
                    self.env = saved_env;
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
