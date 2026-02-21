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
        if self.stack.len() < arity {
            return Err(RuntimeError::new("VM stack underflow in CallFunc"));
        }
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
        let method_raw = Self::const_str(code, name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
        // For ^ (meta) modifier, prepend ^ to method name for dispatch
        let method = if modifier.as_deref() == Some("^") {
            format!("^{}", method_raw)
        } else {
            method_raw
        };
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new("VM stack underflow in CallMethod"));
        }
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("VM stack underflow in CallMethod target".to_string())
        })?;
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
            Some("+") => {
                // .+method: must succeed, wraps result in a list
                let val = call_result?;
                self.stack.push(Value::array(vec![val]));
                self.sync_locals_from_env(code);
            }
            Some("*") => {
                // .*method: wraps in list if found, empty list if not
                match call_result {
                    Ok(val) => self.stack.push(Value::array(vec![val])),
                    Err(_) => self.stack.push(Value::array(vec![])),
                }
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
        let method_raw = Self::const_str(code, name_idx).to_string();
        let target_name = Self::const_str(code, target_name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
        let method = if modifier.as_deref() == Some("^") {
            format!("^{}", method_raw)
        } else {
            method_raw
        };
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new("VM stack underflow in CallMethodMut"));
        }
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("VM stack underflow in CallMethodMut target".to_string())
        })?;
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
            Some("+") => {
                let val = call_result?;
                self.stack.push(Value::array(vec![val]));
                self.sync_locals_from_env(code);
            }
            Some("*") => match call_result {
                Ok(val) => self.stack.push(Value::array(vec![val])),
                Err(_) => self.stack.push(Value::array(vec![])),
            },
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
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new("VM stack underflow in CallOnValue"));
        }
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("VM stack underflow in CallOnValue target".to_string())
        })?;
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
        if self.stack.len() < arity {
            return Err(RuntimeError::new("VM stack underflow in CallOnCodeVar"));
        }
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        // resolve_code_var handles pseudo-package stripping internally
        let target = self.interpreter.resolve_code_var(&name);
        let result = if !matches!(target, Value::Nil) {
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

    pub(super) fn exec_hyper_method_call_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
    ) -> Result<(), RuntimeError> {
        let method = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new("VM stack underflow in HyperMethodCall"));
        }
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        let target = self
            .stack
            .pop()
            .ok_or_else(|| RuntimeError::new("VM stack underflow in HyperMethodCall target"))?;
        let items = crate::runtime::value_to_list(&target);
        let mut results = Vec::with_capacity(items.len());
        for item in &items {
            let call_result =
                if let Some(native_result) = Self::try_native_method(item, &method, &args) {
                    native_result?
                } else {
                    self.interpreter
                        .call_method_with_values(item.clone(), &method, args.clone())?
                };
            results.push(call_result);
        }
        self.stack.push(Value::array(results));
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
        if self.stack.len() < arity {
            return Err(RuntimeError::new("VM stack underflow in ExecCall"));
        }
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
        if self.stack.len() < arity {
            return Err(RuntimeError::new("VM stack underflow in ExecCallPairs"));
        }
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        self.interpreter.exec_call_pairs_values(&name, args)?;
        self.sync_locals_from_env(code);
        Ok(())
    }
}
