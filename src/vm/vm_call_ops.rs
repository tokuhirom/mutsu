use super::*;

impl VM {
    pub(super) fn exec_call_func_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        if let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args) {
            let pkg = self.interpreter.current_package().to_string();
            let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
            self.stack.push(result);
            self.sync_locals_from_env(code);
        } else if let Some(native_result) = Self::try_native_function(&name, &args) {
            self.stack.push(native_result?);
        } else {
            let result = self.interpreter.call_function(&name, args)?;
            self.stack.push(result);
            self.sync_locals_from_env(code);
        }
        Ok(())
    }

    pub(super) fn exec_call_method_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        modifier_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let method = Self::const_str(code, name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
        let arity = arity as usize;
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        let target = self.stack.pop().unwrap();
        let call_result =
            if let Some(native_result) = Self::try_native_method(&target, &method, &args) {
                native_result
            } else {
                self.interpreter
                    .call_method_with_values(target, &method, args)
            };
        match modifier.as_deref() {
            Some("?") => {
                self.stack.push(call_result.unwrap_or(Value::Nil));
            }
            _ => {
                self.stack.push(call_result?);
                self.sync_locals_from_env(code);
            }
        }
        Ok(())
    }

    pub(super) fn exec_call_method_mut_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let method = Self::const_str(code, name_idx).to_string();
        let target_name = Self::const_str(code, target_name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
        let arity = arity as usize;
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        let target = self.stack.pop().unwrap();
        let call_result =
            if let Some(native_result) = Self::try_native_method(&target, &method, &args) {
                native_result
            } else {
                self.interpreter
                    .call_method_mut_with_values(&target_name, target, &method, args)
            };
        match modifier.as_deref() {
            Some("?") => {
                self.stack.push(call_result.unwrap_or(Value::Nil));
            }
            _ => {
                self.stack.push(call_result?);
                self.sync_locals_from_env(code);
            }
        }
        Ok(())
    }

    pub(super) fn exec_call_on_value_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
    ) -> Result<(), RuntimeError> {
        let arity = arity as usize;
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        let target = self.stack.pop().unwrap_or(Value::Nil);
        let result = self.interpreter.eval_call_on_value(target, args)?;
        self.stack.push(result);
        self.sync_locals_from_env(code);
        Ok(())
    }

    pub(super) fn exec_call_on_code_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        let key = format!("&{}", name);
        let result = if self.interpreter.env().contains_key(&key) {
            let target = self.interpreter.resolve_code_var(&name);
            self.interpreter.eval_call_on_value(target, args)?
        } else if let Some(native_result) = Self::try_native_function(&name, &args) {
            native_result?
        } else {
            self.interpreter.call_function(&name, args)?
        };
        self.stack.push(result);
        self.sync_locals_from_env(code);
        Ok(())
    }

    pub(super) fn exec_exec_call_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        if let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args) {
            let pkg = self.interpreter.current_package().to_string();
            let _result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
            self.sync_locals_from_env(code);
        } else if let Some(native_result) = Self::try_native_function(&name, &args) {
            native_result?;
        } else {
            self.interpreter.exec_call_values(&name, args)?;
            self.sync_locals_from_env(code);
        }
        Ok(())
    }

    pub(super) fn exec_exec_call_pairs_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        self.interpreter.exec_call_pairs_values(&name, args)?;
        self.sync_locals_from_env(code);
        Ok(())
    }

    pub(super) fn exec_exec_call_mixed_op(
        &mut self,
        code: &CompiledCode,
        stmt_idx: u32,
    ) -> Result<(), RuntimeError> {
        let stmt = &code.stmt_pool[stmt_idx as usize];
        if let Stmt::Call { name, args } = stmt {
            let eval_value_count = args
                .iter()
                .filter(|arg| Self::call_arg_has_eval_value(arg))
                .count();
            let start = self.stack.len().saturating_sub(eval_value_count);
            let eval_values: Vec<Value> = self.stack.drain(start..).collect();
            self.interpreter
                .exec_call_mixed_values(name, args, eval_values)?;
            self.sync_locals_from_env(code);
            Ok(())
        } else {
            Err(RuntimeError::new("ExecCallMixed expects Call"))
        }
    }
}
