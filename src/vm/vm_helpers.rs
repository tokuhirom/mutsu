use super::*;

impl VM {
    pub(super) fn const_str(code: &CompiledCode, idx: u32) -> &str {
        match &code.constants[idx as usize] {
            Value::Str(s) => s.as_str(),
            _ => unreachable!("expected string constant"),
        }
    }

    pub(super) fn is_builtin_type(name: &str) -> bool {
        matches!(
            name,
            "Hash"
                | "Array"
                | "Int"
                | "Num"
                | "Rat"
                | "FatRat"
                | "Complex"
                | "Str"
                | "Bool"
                | "Pair"
                | "Map"
                | "Set"
                | "Bag"
                | "Mix"
                | "List"
                | "Seq"
                | "Range"
                | "Any"
                | "Mu"
                | "Cool"
                | "Real"
                | "Numeric"
                | "Stringy"
                | "Positional"
                | "Associative"
                | "Failure"
                | "Exception"
                | "Order"
                | "Version"
                | "Nil"
                | "Regex"
                | "Block"
                | "Routine"
                | "Sub"
                | "Method"
                | "IO"
                | "Proc"
                | "Slip"
        )
    }

    pub(super) fn label_matches(error_label: &Option<String>, loop_label: &Option<String>) -> bool {
        error_label.as_deref() == loop_label.as_deref() || error_label.is_none()
    }

    pub(super) fn eval_binary_with_junctions(
        &mut self,
        left: Value,
        right: Value,
        f: fn(&mut VM, Value, Value) -> Result<Value, RuntimeError>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Junction { kind, values } = left {
            let results: Result<Vec<Value>, RuntimeError> = values
                .into_iter()
                .map(|v| self.eval_binary_with_junctions(v, right.clone(), f))
                .collect();
            return Ok(Value::Junction {
                kind,
                values: results?,
            });
        }
        if let Value::Junction { kind, values } = right {
            let results: Result<Vec<Value>, RuntimeError> = values
                .into_iter()
                .map(|v| self.eval_binary_with_junctions(left.clone(), v, f))
                .collect();
            return Ok(Value::Junction {
                kind,
                values: results?,
            });
        }
        f(self, left, right)
    }

    pub(super) fn smart_match_op(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        Ok(Value::Bool(
            self.interpreter.smart_match_values(&left, &right),
        ))
    }

    pub(super) fn not_smart_match_op(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        Ok(Value::Bool(
            !self.interpreter.smart_match_values(&left, &right),
        ))
    }

    pub(super) fn sync_locals_from_env(&mut self, code: &CompiledCode) {
        for (i, name) in code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(name) {
                self.locals[i] = val.clone();
            }
        }
    }

    pub(super) fn find_local_slot(&self, code: &CompiledCode, name: &str) -> Option<usize> {
        code.locals.iter().position(|n| n == name)
    }

    pub(super) fn update_local_if_exists(&mut self, code: &CompiledCode, name: &str, val: &Value) {
        if let Some(slot) = self.find_local_slot(code, name) {
            self.locals[slot] = val.clone();
        }
    }

    pub(super) fn try_native_method(
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if args.len() == 2 {
            return crate::builtins::native_method_2arg(target, method, &args[0], &args[1]);
        }
        if args.len() == 1 {
            return crate::builtins::native_method_1arg(target, method, &args[0]);
        }
        if !args.is_empty() {
            return None;
        }
        crate::builtins::native_method_0arg(target, method)
    }

    pub(super) fn try_native_function(
        name: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        crate::builtins::native_function(name, args)
    }

    pub(super) fn find_compiled_function<'a>(
        &self,
        compiled_fns: &'a HashMap<String, CompiledFunction>,
        name: &str,
        args: &[Value],
    ) -> Option<&'a CompiledFunction> {
        let pkg = self.interpreter.current_package();
        let arity = args.len();
        let type_sig: Vec<String> = args
            .iter()
            .map(|v| runtime::value_type_name(v).to_string())
            .collect();
        let key_typed = format!("{}::{}/{}:{}", pkg, name, arity, type_sig.join(","));
        if let Some(cf) = compiled_fns.get(&key_typed) {
            return Some(cf);
        }
        let key_arity = format!("{}::{}/{}", pkg, name, arity);
        if let Some(cf) = compiled_fns.get(&key_arity) {
            return Some(cf);
        }
        let key_simple = format!("{}::{}", pkg, name);
        if let Some(cf) = compiled_fns.get(&key_simple) {
            return Some(cf);
        }
        if pkg != "GLOBAL" {
            let key_global = format!("GLOBAL::{}", name);
            if let Some(cf) = compiled_fns.get(&key_global) {
                return Some(cf);
            }
        }
        if name.contains("::") {
            compiled_fns.get(name)
        } else {
            None
        }
    }

    pub(super) fn call_compiled_function_named(
        &mut self,
        cf: &CompiledFunction,
        args: Vec<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
        fn_package: &str,
        fn_name: &str,
    ) -> Result<Value, RuntimeError> {
        let saved_env = self.interpreter.env().clone();
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack_depth = self.stack.len();

        if !fn_name.is_empty() {
            self.interpreter
                .push_routine(fn_package.to_string(), fn_name.to_string());
        }

        self.interpreter
            .bind_function_args_values(&cf.param_defs, &cf.params, &args)?;

        self.locals = vec![Value::Nil; cf.code.locals.len()];
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }

        let mut ip = 0;
        let mut result = Ok(());
        while ip < cf.code.ops.len() {
            match self.exec_one(&cf.code, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(e) if e.return_value.is_some() => {
                    let ret_val = e.return_value.unwrap();
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    result = Err(e);
                    break;
                }
            }
            if self.interpreter.is_halted() {
                break;
            }
        }

        let ret_val = if result.is_ok() {
            if self.stack.len() > saved_stack_depth {
                self.stack.pop().unwrap_or(Value::Nil)
            } else {
                Value::Nil
            }
        } else {
            Value::Nil
        };

        self.stack.truncate(saved_stack_depth);

        if !fn_name.is_empty() {
            self.interpreter.pop_routine();
        }

        self.locals = saved_locals;
        *self.interpreter.env_mut() = saved_env;

        match result {
            Ok(()) => Ok(ret_val),
            Err(e) => Err(e),
        }
    }
}
