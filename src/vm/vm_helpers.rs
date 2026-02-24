use super::*;

impl VM {
    fn main_unqualified_name(name: &str) -> Option<String> {
        for sigil in ["$", "@", "%", "&"] {
            let prefix = format!("{sigil}Main::");
            if let Some(rest) = name.strip_prefix(&prefix) {
                return Some(format!("{sigil}{rest}"));
            }
        }
        None
    }

    fn main_qualified_name(name: &str) -> Option<String> {
        for sigil in ["$", "@", "%", "&"] {
            if let Some(rest) = name.strip_prefix(sigil)
                && !rest.contains("::")
            {
                return Some(format!("{sigil}Main::{rest}"));
            }
        }
        None
    }

    /// Look up a sigiled variable name (e.g. "@z") in the locals array by its
    /// bare (sigil-stripped) name.  This handles function parameters that are
    /// stored without sigils in the compiled-function locals.
    pub(super) fn get_local_by_bare_name(&self, code: &CompiledCode, name: &str) -> Option<Value> {
        // Strip the leading sigil (@, %)
        let bare = name.strip_prefix('@').or_else(|| name.strip_prefix('%'))?;
        // Respect lexical shadowing by resolving the most recently-declared local.
        let idx = code.locals.iter().rposition(|n| n == bare)?;
        Some(self.locals.get(idx)?.clone())
    }

    pub(super) fn get_env_with_main_alias(&self, name: &str) -> Option<Value> {
        if let Some(val) = self.interpreter.env().get(name) {
            return Some(val.clone());
        }
        if let Some(alias) = Self::main_unqualified_name(name) {
            return self.interpreter.env().get(&alias).cloned();
        }
        if let Some(qualified) = Self::main_qualified_name(name) {
            return self.interpreter.env().get(&qualified).cloned();
        }
        None
    }

    pub(super) fn set_env_with_main_alias(&mut self, name: &str, value: Value) {
        self.interpreter
            .env_mut()
            .insert(name.to_string(), value.clone());
        if let Some(alias) = Self::main_unqualified_name(name) {
            self.interpreter.env_mut().insert(alias, value);
            return;
        }
        if let Some(qualified) = Self::main_qualified_name(name)
            && self.interpreter.env().contains_key(&qualified)
        {
            self.interpreter.env_mut().insert(qualified, value);
        }
    }

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
                | "Uni"
                | "Version"
                | "Nil"
                | "Regex"
                | "Block"
                | "Routine"
                | "Sub"
                | "Callable"
                | "Method"
                | "IO"
                | "Proc"
                | "Slip"
                | "Duration"
                | "Date"
                | "DateTime"
                | "ObjAt"
                | "Code"
                | "Capture"
                | "Junction"
                | "Match"
                | "Signature"
                | "Parameter"
                | "WhateverCode"
                | "HyperWhatever"
                | "Stash"
                | "Scalar"
                | "SetHash"
                | "BagHash"
                | "MixHash"
                | "Grammar"
                | "Submethod"
                | "Label"
                | "Lock"
                | "Semaphore"
                | "Whatever"
                | "Instant"
                | "Buf"
                | "Blob"
                | "blob8"
                | "buf8"
                | "blob16"
                | "buf16"
                | "blob32"
                | "buf32"
                | "blob64"
                | "buf64"
                | "Endian"
                | "Kernel"
                | "CX::Warn"
                | "X::AdHoc"
                | "CompUnit::DependencySpecification"
        )
    }

    /// Check if a name is a type with a smiley suffix (:U, :D, :_).
    pub(super) fn is_type_with_smiley(name: &str, interp: &crate::runtime::Interpreter) -> bool {
        let (base, smiley) = crate::runtime::types::strip_type_smiley(name);
        if smiley.is_none() {
            return false;
        }
        interp.has_class(base) || Self::is_builtin_type(base)
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
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(v, right.clone(), f))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        if let Value::Junction { kind, values } = right {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .cloned()
                .map(|v| self.eval_binary_with_junctions(left.clone(), v, f))
                .collect();
            return Ok(Value::junction(kind, results?));
        }
        f(self, left, right)
    }

    pub(super) fn smart_match_op(
        &mut self,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        let is_regex = matches!(&right, Value::Regex(_));
        let matched = self.interpreter.smart_match_values(&left, &right);
        if is_regex {
            // For regex smartmatch, return the Match object (from $/) or Nil
            if matched {
                Ok(self
                    .interpreter
                    .env()
                    .get("/")
                    .cloned()
                    .unwrap_or(Value::Nil))
            } else {
                Ok(Value::Nil)
            }
        } else {
            Ok(Value::Bool(matched))
        }
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
        let bypass_supply_extrema_fastpath = matches!(method, "max" | "min")
            && args.len() <= 1
            && (matches!(
                target,
                Value::Instance { class_name, .. } if class_name == "Supply"
            ) || matches!(target, Value::Package(name) if name == "Supply"));
        let bypass_supplier_supply_fastpath = method == "Supply"
            && args.is_empty()
            && matches!(
                target,
                Value::Instance { class_name, .. } if class_name == "Supplier"
            );
        if bypass_supply_extrema_fastpath || bypass_supplier_supply_fastpath {
            return None;
        }
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
        &mut self,
        compiled_fns: &'a HashMap<String, CompiledFunction>,
        name: &str,
        args: &[Value],
    ) -> Option<&'a CompiledFunction> {
        let expected_fingerprint = self
            .interpreter
            .resolve_function_with_types(name, args)
            .map(|def| {
                crate::ast::function_body_fingerprint(&def.params, &def.param_defs, &def.body)
            });
        // If runtime resolution fails, avoid reusing stale compiled cache entries.
        // This can happen across repeated EVAL calls that redefine the same routine name.
        let expected_fingerprint = expected_fingerprint?;
        let matches_resolved = |cf: &CompiledFunction| cf.fingerprint == expected_fingerprint;
        let pkg = self.interpreter.current_package();
        let arity = args.len();
        let type_sig: Vec<String> = args
            .iter()
            .map(|v| runtime::value_type_name(v).to_string())
            .collect();
        let key_typed = format!("{}::{}/{}:{}", pkg, name, arity, type_sig.join(","));
        if let Some(cf) = compiled_fns.get(&key_typed)
            && matches_resolved(cf)
        {
            return Some(cf);
        }
        let key_arity = format!("{}::{}/{}", pkg, name, arity);
        if let Some(cf) = compiled_fns.get(&key_arity)
            && matches_resolved(cf)
        {
            return Some(cf);
        }
        let key_simple = format!("{}::{}", pkg, name);
        if let Some(cf) = compiled_fns.get(&key_simple)
            && matches_resolved(cf)
        {
            return Some(cf);
        }
        if pkg != "GLOBAL" {
            let key_global = format!("GLOBAL::{}", name);
            if let Some(cf) = compiled_fns.get(&key_global)
                && matches_resolved(cf)
            {
                return Some(cf);
            }
        }
        if name.contains("::") {
            compiled_fns.get(name).filter(|cf| matches_resolved(cf))
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

        if let Err(e) =
            self.interpreter
                .bind_function_args_values(&cf.param_defs, &cf.params, &args)
        {
            if !fn_name.is_empty() {
                self.interpreter.pop_routine();
            }
            self.stack.truncate(saved_stack_depth);
            self.locals = saved_locals;
            *self.interpreter.env_mut() = saved_env;
            return Err(e);
        }

        self.locals = vec![Value::Nil; cf.code.locals.len()];
        for (i, local_name) in cf.code.locals.iter().enumerate() {
            if let Some(val) = self.interpreter.env().get(local_name) {
                self.locals[i] = val.clone();
            }
        }
        // Load persisted state variable values
        for (slot, key) in &cf.code.state_locals {
            if let Some(val) = self.interpreter.get_state_var(key) {
                self.locals[*slot] = val.clone();
            }
        }

        let let_mark = self.interpreter.let_saves_len();
        let mut ip = 0;
        let mut result = Ok(());
        while ip < cf.code.ops.len() {
            match self.exec_one(&cf.code, &mut ip, compiled_fns) {
                Ok(()) => {}
                Err(e) if e.return_value.is_some() => {
                    let ret_val = e.return_value.unwrap();
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(ret_val);
                    self.interpreter.discard_let_saves(let_mark);
                    result = Ok(());
                    break;
                }
                Err(e) if e.is_fail => {
                    // fail() — restore let saves and return Nil
                    self.interpreter.restore_let_saves(let_mark);
                    self.stack.truncate(saved_stack_depth);
                    self.stack.push(Value::Nil);
                    result = Ok(());
                    break;
                }
                Err(e) => {
                    self.interpreter.restore_let_saves(let_mark);
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

        // Sync state variables back to persistent storage.
        // Read from env first (methods like push update env directly),
        // falling back to locals.
        for (slot, key) in &cf.code.state_locals {
            let local_name = &cf.code.locals[*slot];
            let val = self
                .interpreter
                .env()
                .get(local_name)
                .cloned()
                .unwrap_or_else(|| self.locals[*slot].clone());
            self.interpreter.set_state_var(key.clone(), val);
        }

        if !fn_name.is_empty() {
            self.interpreter.pop_routine();
        }

        let mut restored_env = saved_env;
        let local_names: std::collections::HashSet<&String> = cf.code.locals.iter().collect();
        for (k, v) in self.interpreter.env().iter() {
            if restored_env.contains_key(k) && !local_names.contains(k) {
                restored_env.insert(k.clone(), v.clone());
            }
        }
        self.locals = saved_locals;
        *self.interpreter.env_mut() = restored_env;

        match result {
            Ok(()) => Ok(ret_val),
            Err(e) => Err(e),
        }
    }
}
