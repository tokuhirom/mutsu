use super::*;
use crate::symbol::Symbol;
use std::sync::Arc;

impl VM {
    fn append_slip_item(args: &mut Vec<Value>, item: &Value) {
        match item {
            Value::Capture { positional, named } => {
                args.extend(positional.iter().cloned());
                for (k, v) in named.iter() {
                    args.push(Value::Pair(k.clone(), Box::new(v.clone())));
                }
            }
            Value::Hash(map) => {
                for (k, v) in map.iter() {
                    args.push(Value::Pair(k.clone(), Box::new(v.clone())));
                }
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                args.extend(crate::runtime::utils::value_to_list(item));
            }
            other => args.push(other.clone()),
        }
    }

    fn append_flattened_call_arg(args: &mut Vec<Value>, arg: Value) {
        match arg {
            Value::Slip(items) => {
                for item in items.iter() {
                    Self::append_slip_item(args, item);
                }
            }
            other => args.push(other),
        }
    }

    fn append_slip_value(args: &mut Vec<Value>, slip_val: Value) {
        match slip_val {
            Value::Array(elements, ..) => {
                args.extend(elements.iter().cloned());
            }
            Value::Capture { positional, named } => {
                args.extend(positional);
                for (k, v) in named {
                    args.push(Value::Pair(k, Box::new(v)));
                }
            }
            Value::Slip(items) => {
                for item in items.iter() {
                    Self::append_slip_item(args, item);
                }
            }
            Value::Hash(map) => {
                for (k, v) in map.iter() {
                    args.push(Value::Pair(k.clone(), Box::new(v.clone())));
                }
            }
            Value::Range(..)
            | Value::RangeExcl(..)
            | Value::RangeExclStart(..)
            | Value::RangeExclBoth(..)
            | Value::GenericRange { .. } => {
                args.extend(crate::runtime::utils::value_to_list(&slip_val));
            }
            other => {
                args.push(other);
            }
        }
    }

    fn decode_arg_sources(
        &self,
        code: &CompiledCode,
        arg_sources_idx: Option<u32>,
    ) -> Option<Vec<Option<String>>> {
        let idx = arg_sources_idx?;
        let Value::Array(items, ..) = &code.constants[idx as usize] else {
            return None;
        };
        Some(
            items
                .iter()
                .map(|item| match item {
                    Value::Str(name) => Some(name.clone()),
                    _ => None,
                })
                .collect(),
        )
    }

    fn unwrap_var_ref_value(value: Value) -> Value {
        if let Value::Capture { positional, named } = &value
            && positional.is_empty()
            && let Some(Value::Str(_)) = named.get("__mutsu_varref_name")
            && let Some(inner) = named.get("__mutsu_varref_value")
        {
            return inner.clone();
        }
        value
    }

    fn normalize_call_args_for_target(&mut self, name: &str, raw_args: Vec<Value>) -> Vec<Value> {
        let plain_args: Vec<Value> = raw_args
            .iter()
            .cloned()
            .map(Self::unwrap_var_ref_value)
            .collect();
        if self.interpreter.has_declared_function(name)
            || self.interpreter.has_multi_function(name)
            || self.interpreter.has_proto(name)
        {
            raw_args
        } else {
            plain_args
        }
    }

    fn rewrite_method_name(method_raw: &str, modifier: Option<&str>) -> String {
        match modifier {
            Some("^") => format!("^{}", method_raw),
            Some("!") if method_raw.contains("::") => method_raw.to_string(),
            Some("!") => format!("!{}", method_raw),
            _ => method_raw.to_string(),
        }
    }

    fn call_method_all_with_fallback(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
        skip_native: bool,
    ) -> Result<Vec<Value>, RuntimeError> {
        if !skip_native
            && let Some(native_result) =
                self.try_native_method(target, Symbol::intern(method), args)
        {
            return Ok(vec![native_result?]);
        }
        self.interpreter
            .call_method_all_with_values(target.clone(), method, args.to_vec())
    }

    fn call_method_mut_with_temp_target(
        &mut self,
        item: &Value,
        method: &str,
        args: Vec<Value>,
        slot: usize,
    ) -> Result<(Value, Value), RuntimeError> {
        let temp_name = format!("__mutsu_hyper_target_{slot}");
        self.interpreter
            .env_mut()
            .insert(temp_name.clone(), item.clone());
        let result =
            self.interpreter
                .call_method_mut_with_values(&temp_name, item.clone(), method, args)?;
        let updated = self
            .interpreter
            .env()
            .get(&temp_name)
            .cloned()
            .unwrap_or_else(|| item.clone());
        self.interpreter.env_mut().remove(&temp_name);
        Ok((result, updated))
    }

    fn call_method_all_with_temp_target(
        &mut self,
        item: &Value,
        method: &str,
        args: Vec<Value>,
        slot: usize,
    ) -> Result<(Vec<Value>, Value), RuntimeError> {
        let temp_name = format!("__mutsu_hyper_target_{slot}");
        self.interpreter
            .env_mut()
            .insert(temp_name.clone(), item.clone());
        let result = self
            .interpreter
            .call_method_all_with_values(item.clone(), method, args)?;
        let updated = self
            .interpreter
            .env()
            .get(&temp_name)
            .cloned()
            .unwrap_or_else(|| item.clone());
        self.interpreter.env_mut().remove(&temp_name);
        Ok((result, updated))
    }

    pub(super) fn exec_call_func_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("VM stack underflow in CallFunc"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg);
        }
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        let arg_sources = if arg_sources.as_ref().is_some_and(|s| s.len() != args.len()) {
            None
        } else {
            arg_sources
        };
        let args = self.normalize_call_args_for_target(&name, args);
        let (args, callsite_line) = self.interpreter.sanitize_call_args(&args);
        self.interpreter.set_pending_callsite_line(callsite_line);
        // Check if there's a CALL-ME override from trait_mod mixin
        let call_me_override = self
            .interpreter
            .env()
            .get(&format!("&{}", name))
            .cloned()
            .and_then(|callable| {
                if let Value::Mixin(_, ref mixins) = callable {
                    let has_call_me = mixins.keys().any(|key| {
                        key.strip_prefix("__mutsu_role__")
                            .is_some_and(|rn| self.interpreter.role_has_method(rn, "CALL-ME"))
                    });
                    if has_call_me {
                        return Some(callable);
                    }
                }
                None
            });
        if let Some(callable) = call_me_override {
            let result = self
                .interpreter
                .call_method_with_values(callable, "CALL-ME", args);
            let result = self.interpreter.maybe_fetch_rw_proxy(result?, true)?;
            self.stack.push(result);
            self.sync_locals_from_env(code);
        } else if !self.interpreter.has_proto(&name)
            && let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args)
        {
            self.interpreter
                .set_pending_call_arg_sources(arg_sources.clone());
            let pkg = self.interpreter.current_package().to_string();
            let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name);
            self.interpreter.set_pending_call_arg_sources(None);
            let result = self.interpreter.maybe_fetch_rw_proxy(result?, true)?;
            self.stack.push(result);
            self.sync_locals_from_env(code);
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            self.stack.push(native_result?);
        } else {
            self.interpreter.set_pending_call_arg_sources(arg_sources);
            let result = self.interpreter.call_function(&name, args);
            self.interpreter.set_pending_call_arg_sources(None);
            let result = self.interpreter.maybe_fetch_rw_proxy(result?, true)?;
            self.stack.push(result);
            self.sync_locals_from_env(code);
        }
        Ok(())
    }

    /// Expression-level call with capture slip: regular args + 1 slip arg on stack.
    /// The slip arg is flattened into the argument list, result is pushed.
    pub(super) fn exec_call_func_slip_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        regular_arity: u32,
        _arg_sources_idx: Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let total = regular_arity as usize + 1; // +1 for the slip value
        if self.stack.len() < total {
            return Err(RuntimeError::new("VM stack underflow in CallFuncSlip"));
        }
        // Pop slip value (top of stack), then regular args
        let slip_val = self.stack.pop().unwrap();
        let regular_start = self.stack.len() - regular_arity as usize;
        let mut args: Vec<Value> = self.stack.drain(regular_start..).collect();
        Self::append_slip_value(&mut args, slip_val);
        let args = self.normalize_call_args_for_target(&name, args);
        let (args, callsite_line) = self.interpreter.sanitize_call_args(&args);
        self.interpreter.set_pending_callsite_line(callsite_line);
        if !self.interpreter.has_proto(&name)
            && let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args)
        {
            let pkg = self.interpreter.current_package().to_string();
            let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
            let result = self.interpreter.maybe_fetch_rw_proxy(result, true)?;
            self.stack.push(result);
            self.sync_locals_from_env(code);
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            self.stack.push(native_result?);
        } else {
            let result = self.interpreter.call_function(&name, args)?;
            let result = self.interpreter.maybe_fetch_rw_proxy(result, true)?;
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
        quoted: bool,
    ) -> Result<(), RuntimeError> {
        let method_raw = Self::const_str(code, name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
        let method = Self::rewrite_method_name(&method_raw, modifier.as_deref());
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new("VM stack underflow in CallMethod"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg);
        }
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("VM stack underflow in CallMethod target".to_string())
        })?;
        // Junction auto-threading: thread method calls over junction values
        if let Value::Junction { kind, values } = &target
            && !matches!(
                method.as_str(),
                "Bool"
                    | "so"
                    | "WHAT"
                    | "^name"
                    | "gist"
                    | "Str"
                    | "defined"
                    | "THREAD"
                    | "raku"
                    | "perl"
            )
        {
            let kind = kind.clone();
            let mut results = Vec::new();
            for v in values.iter() {
                let r = if let Some(nr) = self.try_native_method(v, Symbol::intern(&method), &args)
                {
                    nr?
                } else {
                    self.interpreter
                        .call_method_with_values(v.clone(), &method, args.clone())?
                };
                results.push(r);
            }
            let junction_result = Value::Junction {
                kind,
                values: Arc::new(results),
            };
            self.stack.push(junction_result);
            return Ok(());
        }

        // When the method name was quoted (e.g. ."DEFINITE"()), skip the native
        // pseudo-method fast path so user-defined methods are called instead.
        let mut skip_native = method == "VAR"
            || (quoted
                && matches!(
                    method.as_str(),
                    "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
                ));
        // Also skip native if the target has a user-defined method with this name,
        // but NOT for pseudo-methods like DEFINITE, WHAT, etc. which are macros.
        if !skip_native
            && !matches!(
                method.as_str(),
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            )
        {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.as_str()),
                Value::Package(name) => Some(name.as_str()),
                _ => None,
            };
            if let Some(cn) = class_name
                && self.interpreter.has_user_method(cn, &method)
            {
                skip_native = true;
            }
        }
        if !skip_native
            && matches!(method.as_str(), "AT-KEY" | "keys")
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "Stash")
        {
            skip_native = true;
        }
        if quoted
            && skip_native
            && matches!(
                method.as_str(),
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            )
        {
            self.interpreter.skip_pseudo_method_native = Some(method.clone());
        }
        let target_for_mod = target.clone();
        let args_for_mod = args.clone();
        let call_result = if !skip_native {
            if let Some(native_result) =
                self.try_native_method(&target, Symbol::intern(&method), &args)
            {
                native_result
            } else {
                self.interpreter
                    .call_method_with_values(target, &method, args)
            }
        } else {
            self.interpreter
                .call_method_with_values(target, &method, args)
        };
        match modifier.as_deref() {
            Some("?") => {
                self.stack.push(call_result.unwrap_or(Value::Nil));
            }
            Some("+") => {
                let vals = self.call_method_all_with_fallback(
                    &target_for_mod,
                    &method,
                    &args_for_mod,
                    skip_native,
                )?;
                self.stack.push(Value::array(vals));
                self.sync_locals_from_env(code);
            }
            Some("*") => {
                match self.call_method_all_with_fallback(
                    &target_for_mod,
                    &method,
                    &args_for_mod,
                    skip_native,
                ) {
                    Ok(vals) => self.stack.push(Value::array(vals)),
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

    pub(super) fn exec_call_method_dynamic_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
    ) -> Result<(), RuntimeError> {
        let arity = arity as usize;
        if self.stack.len() < arity + 2 {
            return Err(RuntimeError::new("VM stack underflow in CallMethodDynamic"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg);
        }
        let name_val = self
            .stack
            .pop()
            .ok_or_else(|| RuntimeError::new("VM stack underflow in CallMethodDynamic name"))?;
        let target = self
            .stack
            .pop()
            .ok_or_else(|| RuntimeError::new("VM stack underflow in CallMethodDynamic target"))?;
        let call_result = if matches!(
            &name_val,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
        ) {
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(target);
            call_args.extend(args);
            self.interpreter.call_sub_value(name_val, call_args, false)
        } else {
            let method = name_val.to_string_value();
            if let Some(native_result) =
                self.try_native_method(&target, Symbol::intern(&method), &args)
            {
                native_result
            } else {
                self.interpreter
                    .call_method_with_values(target, &method, args)
            }
        };
        self.stack.push(call_result?);
        self.sync_locals_from_env(code);
        Ok(())
    }

    pub(super) fn exec_call_method_mut_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
    ) -> Result<(), RuntimeError> {
        let method_raw = Self::const_str(code, name_idx).to_string();
        let target_name = Self::const_str(code, target_name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
        let method = Self::rewrite_method_name(&method_raw, modifier.as_deref());
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new("VM stack underflow in CallMethodMut"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg);
        }
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("VM stack underflow in CallMethodMut target".to_string())
        })?;
        // Junction auto-threading: thread method calls over junction values
        if let Value::Junction { kind, values } = &target
            && !matches!(
                method.as_str(),
                "Bool"
                    | "so"
                    | "WHAT"
                    | "^name"
                    | "gist"
                    | "Str"
                    | "defined"
                    | "THREAD"
                    | "raku"
                    | "perl"
            )
        {
            let kind = kind.clone();
            let mut results = Vec::new();
            for v in values.iter() {
                let r = if let Some(nr) = self.try_native_method(v, Symbol::intern(&method), &args)
                {
                    nr?
                } else {
                    self.interpreter
                        .call_method_with_values(v.clone(), &method, args.clone())?
                };
                results.push(r);
            }
            let junction_result = Value::Junction {
                kind,
                values: Arc::new(results),
            };
            self.stack.push(junction_result);
            return Ok(());
        }
        let mut skip_native = quoted
            && matches!(
                method.as_str(),
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            );
        // Also skip native if the target has a user-defined method with this name,
        // but NOT for pseudo-methods like DEFINITE, WHAT, etc. which are macros.
        if !skip_native
            && !matches!(
                method.as_str(),
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            )
        {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.as_str()),
                Value::Package(name) => Some(name.as_str()),
                _ => None,
            };
            if let Some(cn) = class_name
                && self.interpreter.has_user_method(cn, &method)
            {
                skip_native = true;
            }
        }
        if !skip_native
            && matches!(method.as_str(), "AT-KEY" | "keys")
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "Stash")
        {
            skip_native = true;
        }
        if skip_native {
            self.interpreter.skip_pseudo_method_native = Some(method.clone());
        }
        let call_result = if !skip_native {
            if let Some(native_result) =
                self.try_native_method(&target, Symbol::intern(&method), &args)
            {
                native_result
            } else {
                self.interpreter
                    .call_method_mut_with_values(&target_name, target, &method, args)
            }
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
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new("VM stack underflow in CallOnValue"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg);
        }
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        let arg_sources = if arg_sources.as_ref().is_some_and(|s| s.len() != args.len()) {
            None
        } else {
            arg_sources
        };
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("VM stack underflow in CallOnValue target".to_string())
        })?;
        self.interpreter.set_pending_call_arg_sources(arg_sources);
        let result = self.interpreter.eval_call_on_value(target, args);
        self.interpreter.set_pending_call_arg_sources(None);
        let result = self.interpreter.maybe_fetch_rw_proxy(result?, true)?;
        self.stack.push(result);
        self.sync_locals_from_env(code);
        Ok(())
    }

    pub(super) fn exec_call_on_code_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("VM stack underflow in CallOnCodeVar"));
        }
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        let (args, callsite_line) = self.interpreter.sanitize_call_args(&args);
        self.interpreter.set_pending_callsite_line(callsite_line);
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        let arg_sources = if arg_sources.as_ref().is_some_and(|s| s.len() != args.len()) {
            None
        } else {
            arg_sources
        };
        // resolve_code_var handles pseudo-package stripping internally
        let target = self.interpreter.resolve_code_var(&name);
        let result = if !matches!(target, Value::Nil) {
            self.interpreter
                .set_pending_call_arg_sources(arg_sources.clone());
            let result = self.interpreter.eval_call_on_value(target, args);
            self.interpreter.set_pending_call_arg_sources(None);
            result?
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?
        } else {
            self.interpreter.set_pending_call_arg_sources(arg_sources);
            let result = self.interpreter.call_function(&name, args);
            self.interpreter.set_pending_call_arg_sources(None);
            result?
        };
        let result = self.interpreter.maybe_fetch_rw_proxy(result, true)?;
        self.stack.push(result);
        self.sync_locals_from_env(code);
        Ok(())
    }

    pub(super) fn exec_hyper_method_call_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
    ) -> Result<(), RuntimeError> {
        let method_raw = Self::const_str(code, name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
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
        let mut items = crate::runtime::value_to_list(&target);
        let mut results = Vec::with_capacity(items.len());
        for (idx, item) in items.iter_mut().enumerate() {
            let method = Self::rewrite_method_name(&method_raw, modifier.as_deref());
            let mut skip_native = method == "VAR"
                || (quoted
                    && matches!(
                        method.as_str(),
                        "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
                    ));
            if !skip_native
                && !matches!(
                    method.as_str(),
                    "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
                )
            {
                let class_name = match item {
                    Value::Instance { class_name, .. } => Some(class_name.as_str()),
                    Value::Package(name) => Some(name.as_str()),
                    _ => None,
                };
                if let Some(cn) = class_name
                    && self.interpreter.has_user_method(cn, &method)
                {
                    skip_native = true;
                }
            }
            let item_args = args.clone();
            match modifier.as_deref() {
                Some("?") => {
                    let val = if !skip_native {
                        if let Some(native_result) =
                            self.try_native_method(item, Symbol::intern(&method), &item_args)
                        {
                            native_result.unwrap_or(Value::Package("Any".to_string()))
                        } else {
                            match self
                                .call_method_mut_with_temp_target(item, &method, item_args, idx)
                            {
                                Ok((v, updated)) => {
                                    *item = updated;
                                    v
                                }
                                Err(_) => Value::Package("Any".to_string()),
                            }
                        }
                    } else {
                        match self.call_method_mut_with_temp_target(item, &method, item_args, idx) {
                            Ok((v, updated)) => {
                                *item = updated;
                                v
                            }
                            Err(_) => Value::Package("Any".to_string()),
                        }
                    };
                    results.push(val);
                }
                Some("+") => {
                    let vals = if !skip_native {
                        if let Some(native_result) =
                            self.try_native_method(item, Symbol::intern(&method), &item_args)
                        {
                            vec![native_result?]
                        } else {
                            let (v, updated) = self
                                .call_method_all_with_temp_target(item, &method, item_args, idx)?;
                            *item = updated;
                            v
                        }
                    } else {
                        let (v, updated) =
                            self.call_method_all_with_temp_target(item, &method, item_args, idx)?;
                        *item = updated;
                        v
                    };
                    results.push(Value::array(vals));
                }
                Some("*") => {
                    if !skip_native
                        && let Some(native_result) =
                            self.try_native_method(item, Symbol::intern(&method), &item_args)
                    {
                        match native_result {
                            Ok(v) => results.push(Value::array(vec![v])),
                            Err(_) => results.push(Value::array(vec![])),
                        }
                    } else {
                        match self.call_method_all_with_temp_target(item, &method, item_args, idx) {
                            Ok((vals, updated)) => {
                                *item = updated;
                                results.push(Value::array(vals));
                            }
                            Err(_) => results.push(Value::array(vec![])),
                        }
                    }
                }
                _ => {
                    let val = if !skip_native {
                        if let Some(native_result) =
                            self.try_native_method(item, Symbol::intern(&method), &item_args)
                        {
                            native_result?
                        } else {
                            let (v, updated) = self
                                .call_method_mut_with_temp_target(item, &method, item_args, idx)?;
                            *item = updated;
                            v
                        }
                    } else {
                        let (v, updated) =
                            self.call_method_mut_with_temp_target(item, &method, item_args, idx)?;
                        *item = updated;
                        v
                    };
                    results.push(val);
                }
            }
        }
        if let Value::Array(existing, is_array) = &target {
            self.interpreter.overwrite_array_items_by_identity_for_vm(
                existing,
                items.clone(),
                *is_array,
            );
            if let Some((source, indices, source_is_array)) =
                crate::runtime::utils::get_grep_view_binding(existing)
            {
                let mut source_items = source.to_vec();
                for (filtered_idx, source_idx) in indices.iter().enumerate() {
                    if filtered_idx < items.len() && *source_idx < source_items.len() {
                        source_items[*source_idx] = items[filtered_idx].clone();
                    }
                }
                self.interpreter.overwrite_array_items_by_identity_for_vm(
                    &source,
                    source_items,
                    source_is_array,
                );
            }
            self.sync_locals_from_env(code);
        }
        self.stack.push(Value::array(results));
        Ok(())
    }

    pub(super) fn exec_hyper_method_call_dynamic_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        modifier_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx).to_string());
        let arity = arity as usize;
        if self.stack.len() < arity + 2 {
            return Err(RuntimeError::new(
                "VM stack underflow in HyperMethodCallDynamic",
            ));
        }
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        let name_val = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("VM stack underflow in HyperMethodCallDynamic name")
        })?;
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("VM stack underflow in HyperMethodCallDynamic target")
        })?;
        let mut items = crate::runtime::value_to_list(&target);
        let mut results = Vec::with_capacity(items.len());
        let method = (!matches!(
            &name_val,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
        ))
        .then(|| {
            let method_raw = name_val.to_string_value();
            Self::rewrite_method_name(&method_raw, modifier.as_deref())
        });
        for (idx, item) in items.iter_mut().enumerate() {
            let item_args = args.clone();
            if matches!(
                &name_val,
                Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
            ) {
                let mut call_args = Vec::with_capacity(item_args.len() + 1);
                call_args.push(item.clone());
                call_args.extend(item_args);
                match modifier.as_deref() {
                    Some("?") => {
                        results.push(
                            self.interpreter
                                .call_sub_value(name_val.clone(), call_args, false)
                                .unwrap_or(Value::Package("Any".to_string())),
                        );
                    }
                    Some("+") => {
                        let val =
                            self.interpreter
                                .call_sub_value(name_val.clone(), call_args, false)?;
                        results.push(Value::array(vec![val]));
                    }
                    Some("*") => {
                        match self
                            .interpreter
                            .call_sub_value(name_val.clone(), call_args, false)
                        {
                            Ok(v) => results.push(Value::array(vec![v])),
                            Err(_) => results.push(Value::array(vec![])),
                        }
                    }
                    _ => {
                        results.push(self.interpreter.call_sub_value(
                            name_val.clone(),
                            call_args,
                            false,
                        )?);
                    }
                }
                continue;
            }
            let method = method
                .as_ref()
                .expect("method string exists for non-callables");
            match modifier.as_deref() {
                Some("?") => {
                    let val = if let Some(native_result) =
                        self.try_native_method(item, Symbol::intern(method), &item_args)
                    {
                        native_result.unwrap_or(Value::Package("Any".to_string()))
                    } else {
                        match self.call_method_mut_with_temp_target(item, method, item_args, idx) {
                            Ok((v, updated)) => {
                                *item = updated;
                                v
                            }
                            Err(_) => Value::Package("Any".to_string()),
                        }
                    };
                    results.push(val);
                }
                Some("+") => {
                    let vals = if let Some(native_result) =
                        self.try_native_method(item, Symbol::intern(method), &item_args)
                    {
                        vec![native_result?]
                    } else {
                        let (v, updated) =
                            self.call_method_all_with_temp_target(item, method, item_args, idx)?;
                        *item = updated;
                        v
                    };
                    results.push(Value::array(vals));
                }
                Some("*") => {
                    if let Some(native_result) =
                        self.try_native_method(item, Symbol::intern(method), &item_args)
                    {
                        match native_result {
                            Ok(v) => results.push(Value::array(vec![v])),
                            Err(_) => results.push(Value::array(vec![])),
                        }
                    } else {
                        match self.call_method_all_with_temp_target(item, method, item_args, idx) {
                            Ok((vals, updated)) => {
                                *item = updated;
                                results.push(Value::array(vals));
                            }
                            Err(_) => results.push(Value::array(vec![])),
                        }
                    }
                }
                _ => {
                    let val = if let Some(native_result) =
                        self.try_native_method(item, Symbol::intern(method), &item_args)
                    {
                        native_result?
                    } else {
                        let (v, updated) =
                            self.call_method_mut_with_temp_target(item, method, item_args, idx)?;
                        *item = updated;
                        v
                    };
                    results.push(val);
                }
            }
        }
        if let Value::Array(existing, is_array) = &target {
            self.interpreter.overwrite_array_items_by_identity_for_vm(
                existing,
                items.clone(),
                *is_array,
            );
            self.sync_locals_from_env(code);
        }
        self.stack.push(Value::array(results));
        Ok(())
    }

    pub(super) fn exec_exec_call_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("VM stack underflow in ExecCall"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            match arg {
                Value::Slip(items) => {
                    for item in items.iter() {
                        match item {
                            Value::Capture { positional, named } => {
                                args.extend(positional.iter().cloned());
                                for (k, v) in named.iter() {
                                    args.push(Value::Pair(k.clone(), Box::new(v.clone())));
                                }
                            }
                            other => args.push(other.clone()),
                        }
                    }
                }
                other => args.push(other),
            }
        }
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        let arg_sources = if arg_sources.as_ref().is_some_and(|s| s.len() != args.len()) {
            None
        } else {
            arg_sources
        };
        let args = self.normalize_call_args_for_target(&name, args);
        let (args, callsite_line) = self.interpreter.sanitize_call_args(&args);
        self.interpreter.set_pending_callsite_line(callsite_line);
        if let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args) {
            self.interpreter
                .set_pending_call_arg_sources(arg_sources.clone());
            let pkg = self.interpreter.current_package().to_string();
            let call_result =
                self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name);
            self.interpreter.set_pending_call_arg_sources(None);
            call_result?;
            self.sync_locals_from_env(code);
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?;
        } else {
            self.interpreter.set_pending_call_arg_sources(arg_sources);
            let exec_result = self.interpreter.exec_call_values(&name, args);
            self.interpreter.set_pending_call_arg_sources(None);
            exec_result?;
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

    /// Execute a call with capture slip: regular args + 1 slip arg on stack.
    /// The slip arg is flattened into the argument list.
    pub(super) fn exec_exec_call_slip_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        regular_arity: u32,
        _arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let name = Self::const_str(code, name_idx).to_string();
        let total = regular_arity as usize + 1; // +1 for the slip value
        if self.stack.len() < total {
            return Err(RuntimeError::new("VM stack underflow in ExecCallSlip"));
        }
        // Pop slip value (top of stack), then regular args
        let slip_val = self.stack.pop().unwrap();
        let regular_start = self.stack.len() - regular_arity as usize;
        let mut args: Vec<Value> = self.stack.drain(regular_start..).collect();
        Self::append_slip_value(&mut args, slip_val);
        // Try native function first (same as non-slip call path)
        if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            self.stack.push(native_result?);
            self.sync_locals_from_env(code);
            return Ok(());
        }
        self.interpreter.exec_call_pairs_values(&name, args)?;
        self.sync_locals_from_env(code);
        Ok(())
    }
}
