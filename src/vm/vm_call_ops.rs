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

    fn append_flattened_call_arg(args: &mut Vec<Value>, arg: Value, preserve_empty_slip: bool) {
        match arg {
            Value::Slip(items) => {
                if preserve_empty_slip && items.is_empty() {
                    args.push(Value::Slip(items));
                    return;
                }
                for item in items.iter() {
                    Self::append_slip_item(args, item);
                }
            }
            other => args.push(other),
        }
    }

    fn preserve_empty_slip_arg(name: &str) -> bool {
        matches!(
            name,
            "infix:<andthen>"
                | "infix:<notandthen>"
                | "andthen"
                | "notandthen"
                | "__mutsu_andthen_finalize"
        )
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

    /// Auto-FETCH any Proxy values in function call arguments.
    fn auto_fetch_proxy_args(&mut self, args: Vec<Value>) -> Result<Vec<Value>, RuntimeError> {
        let mut out = Vec::with_capacity(args.len());
        for arg in args {
            out.push(self.interpreter.auto_fetch_proxy(&arg)?);
        }
        Ok(out)
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
                    Value::Str(name) => Some(name.to_string()),
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
        self.ensure_env_synced(code);
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("VM stack underflow in CallFunc"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let preserve_empty_slip = Self::preserve_empty_slip_arg(&name);
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, preserve_empty_slip);
        }
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        let arg_sources = if arg_sources.as_ref().is_some_and(|s| s.len() != args.len()) {
            None
        } else {
            arg_sources
        };
        let args = self.normalize_call_args_for_target(&name, args);
        let (args, callsite_line) = self.interpreter.sanitize_call_args(&args);
        // Don't auto-FETCH Proxy args for control flow builtins that must preserve containers,
        // or when in lvalue assignment context (e.g. f() = 42 calls f with in_lvalue_assignment=true).
        let skip_proxy_fetch = matches!(
            name.as_str(),
            "return-rw" | "return" | "die" | "fail" | "leave" | "__mutsu_assign_method_lvalue"
        ) || self.interpreter.in_lvalue_assignment;
        let args = if skip_proxy_fetch {
            args
        } else {
            self.auto_fetch_proxy_args(args)?
        };
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
        // Check wrap chain for named function calls
        if let Some(sub_id) = self.interpreter.wrap_sub_id_for_name(&name)
            && !self.interpreter.is_wrap_dispatching(sub_id)
            && let Some(sub_val) = self.interpreter.get_wrapped_sub(&name)
        {
            let result = self.interpreter.call_sub_value(sub_val, args, false)?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }
        if let Some(callable) = call_me_override {
            let result = self
                .interpreter
                .call_method_with_values(callable, "CALL-ME", args);
            let result = self.interpreter.maybe_fetch_rw_proxy(result?, true)?;
            self.stack.push(result);
            self.env_dirty = true;
        } else if !self.interpreter.has_proto(&name)
            && let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args)
        {
            self.interpreter
                .set_pending_call_arg_sources(arg_sources.clone());
            let pushed_dispatch = self.interpreter.push_multi_dispatch_frame(&name, &args);
            self.interpreter.push_samewith_context(&name, None);
            let pkg = self.interpreter.current_package().to_string();
            let cf_auto_fetch = !cf.is_raw;
            let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name);
            self.interpreter.set_pending_call_arg_sources(None);
            self.interpreter.pop_samewith_context();
            if pushed_dispatch {
                self.interpreter.pop_multi_dispatch();
            }
            let result = self
                .interpreter
                .maybe_fetch_rw_proxy(result?, cf_auto_fetch)?;
            self.stack.push(result);
            self.env_dirty = true;
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            self.stack.push(native_result?);
        } else {
            // Sync VM locals to env before spawning threads so closures capture them
            if name == "start" {
                self.sync_env_from_locals(code);
            }
            self.interpreter.set_pending_call_arg_sources(arg_sources);
            let result = self.interpreter.call_function(&name, args);
            self.interpreter.set_pending_call_arg_sources(None);
            let result = self.interpreter.maybe_fetch_rw_proxy(result?, true)?;
            self.stack.push(result);
            self.env_dirty = true;
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
        self.ensure_env_synced(code);
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
            let cf_auto_fetch = !cf.is_raw;
            let pkg = self.interpreter.current_package().to_string();
            let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
            let result = self
                .interpreter
                .maybe_fetch_rw_proxy(result, cf_auto_fetch)?;
            self.stack.push(result);
            self.env_dirty = true;
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            self.stack.push(native_result?);
        } else {
            // Sync VM locals to env before spawning threads so closures capture them
            if name == "start" {
                self.sync_env_from_locals(code);
            }
            let result = self.interpreter.call_function(&name, args)?;
            let result = self.interpreter.maybe_fetch_rw_proxy(result, true)?;
            self.stack.push(result);
            self.env_dirty = true;
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
        self.ensure_env_synced(code);
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
        let preserve_empty_slip = Self::preserve_empty_slip_arg(&method);
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, preserve_empty_slip);
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
                    self.try_compiled_method_or_interpret(v.clone(), &method, args.clone())?
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

        // Fast path for Lock::Async.protect — execute block inline in current VM
        if method == "protect"
            && args.len() == 1
            && let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
            && (class_name.resolve() == "Lock::Async" || class_name.resolve() == "Lock")
        {
            let lock_id = match attributes.get("lock-id") {
                Some(Value::Int(id)) if *id > 0 => *id as u64,
                _ => {
                    return Err(RuntimeError::new(
                        "Lock.protect called on Lock without lock-id",
                    ));
                }
            };
            let lock = crate::runtime::native_methods::lock_runtime_by_id(lock_id)
                .ok_or_else(|| RuntimeError::new("Lock.protect could not find lock state"))?;
            let me = crate::runtime::native_methods::current_thread_id();
            crate::runtime::native_methods::acquire_lock(&lock, me)?;
            let code_val = args.into_iter().next().unwrap_or(Value::Nil);
            let result = self.exec_protect_block_inline(code, &code_val);
            let _ = crate::runtime::native_methods::release_lock(&lock, me);
            self.stack.push(result?);
            self.env_dirty = true;
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
        let is_junction_target = match &target {
            Value::Junction { .. } => true,
            Value::Scalar(inner) => matches!(inner.as_ref(), Value::Junction { .. }),
            _ => false,
        };
        if matches!(method.as_str(), "gist" | "raku" | "perl") && is_junction_target {
            skip_native = true;
        }
        // Also skip native if the target has a user-defined method with this name,
        // but NOT for pseudo-methods like DEFINITE, WHAT, etc. which are macros.
        if !skip_native
            && !matches!(
                method.as_str(),
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            )
        {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name
                && self.interpreter.has_user_method(&cn, &method)
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
        if !skip_native
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "Proc::Async")
            && matches!(
                method.as_str(),
                "start"
                    | "kill"
                    | "write"
                    | "close-stdin"
                    | "ready"
                    | "print"
                    | "say"
                    | "command"
                    | "started"
                    | "w"
                    | "pid"
                    | "stdout"
                    | "stderr"
                    | "Supply"
            )
        {
            skip_native = true;
        }
        if !skip_native
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "IterationBuffer")
            && matches!(
                method.as_str(),
                "elems"
                    | "AT-POS"
                    | "BIND-POS"
                    | "push"
                    | "unshift"
                    | "List"
                    | "Slip"
                    | "Seq"
                    | "append"
                    | "prepend"
                    | "clear"
            )
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
        // Auto-FETCH Proxy containers for non-meta method calls
        // Skip auto-FETCH for Proxy subclass attribute access and decontainerized proxies
        let target = if let Value::Proxy {
            subclass,
            decontainerized,
            ..
        } = &target
            && !decontainerized
            && !matches!(
                method.as_str(),
                "VAR" | "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "REPR" | "DEFINITE"
            ) {
            let has_subclass_attr = if let Some((_, attrs)) = subclass {
                attrs.lock().unwrap().contains_key(method.as_str())
            } else {
                false
            };
            if has_subclass_attr {
                target
            } else {
                self.interpreter.auto_fetch_proxy(&target)?
            }
        } else {
            target
        };
        let target_for_mod = target.clone();
        let args_for_mod = args.clone();
        let call_result = if !skip_native {
            if let Some(native_result) =
                self.try_native_method(&target, Symbol::intern(&method), &args)
            {
                native_result
            } else {
                self.try_compiled_method_or_interpret(target, &method, args)
            }
        } else {
            self.try_compiled_method_or_interpret(target, &method, args)
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
                self.env_dirty = true;
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
                self.env_dirty = true;
            }
        }
        Ok(())
    }

    /// Try compiled method fast path; fall back to interpreter.
    fn try_compiled_method_or_interpret(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance { class_name, .. } = &target {
            let class = class_name.resolve();
            if self.interpreter.is_native_method(&class, method) {
                return self
                    .interpreter
                    .call_method_with_values(target, method, args);
            }
        }
        // Pseudo-methods must always go through the interpreter which handles
        // them specially — never intercept via the compiled fast path.
        if matches!(
            method,
            "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
        ) {
            return self
                .interpreter
                .call_method_with_values(target, method, args);
        }
        // Private method fast path: resolve private candidate and run compiled code
        // when caller context clearly allows direct dispatch.
        if method.starts_with('!') {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name {
                let resolved = self
                    .interpreter
                    .resolve_private_method_for_vm(&cn, method, &args);
                if let Some((owner_class, method_def)) = resolved {
                    let caller_allowed = self
                        .interpreter
                        .can_fast_dispatch_private_method_vm(&owner_class);
                    if caller_allowed && let Some(ref cc) = method_def.compiled_code {
                        let cc = cc.clone();
                        let target_id = match &target {
                            Value::Instance { id, .. } => Some(*id),
                            _ => None,
                        };
                        let attributes = match &target {
                            Value::Instance { attributes, .. } => (**attributes).clone(),
                            _ => std::collections::HashMap::new(),
                        };
                        let invocant_for_dispatch = if attributes.is_empty() {
                            Value::Package(crate::symbol::Symbol::intern(&cn))
                        } else {
                            target.clone()
                        };
                        let pushed_dispatch = self.interpreter.push_method_dispatch_frame(
                            &cn,
                            method,
                            &args,
                            invocant_for_dispatch,
                        );
                        let invocant = Some(target);
                        let empty_fns = HashMap::new();
                        let method_result = self.call_compiled_method(
                            &cn,
                            &owner_class,
                            method,
                            &method_def,
                            &cc,
                            attributes,
                            args,
                            invocant,
                            &empty_fns,
                        );
                        if pushed_dispatch {
                            self.interpreter.pop_method_dispatch();
                        }
                        self.interpreter.pop_method_samewith_context();
                        let (result, new_attrs) = method_result?;
                        if let Some(id) = target_id {
                            self.interpreter.overwrite_instance_bindings_by_identity(
                                &cn,
                                id,
                                new_attrs.clone(),
                            );
                            if let Value::Proxy { ref fetcher, .. } = result {
                                return self
                                    .interpreter
                                    .proxy_fetch(fetcher, None, &cn, &new_attrs, id);
                            }
                        }
                        return Ok(result);
                    }
                }
            }
        }
        // Only attempt compiled path for Instance or Package targets
        let class_name = match &target {
            Value::Instance { class_name, .. } => Some(class_name.resolve()),
            Value::Package(name) => Some(name.resolve()),
            _ => None,
        };
        if let Some(cn) = class_name
            && let Some((owner_class, method_def)) = self
                .interpreter
                .resolve_method_with_owner(&cn, method, &args)
            && let Some(ref cc) = method_def.compiled_code
        {
            let cc = cc.clone();
            let target_id = match &target {
                Value::Instance { id, .. } => Some(*id),
                _ => None,
            };
            let attributes = match &target {
                Value::Instance { attributes, .. } => (**attributes).clone(),
                _ => std::collections::HashMap::new(),
            };
            // Set up dispatch frame for nextsame/callsame support
            let invocant_for_dispatch = if attributes.is_empty() {
                Value::Package(crate::symbol::Symbol::intern(&cn))
            } else {
                target.clone()
            };
            let pushed_dispatch = self.interpreter.push_method_dispatch_frame(
                &cn,
                method,
                &args,
                invocant_for_dispatch,
            );
            let invocant = Some(target);
            // Method bodies are compiled independently; function calls
            // within them resolve through the interpreter fallback.
            let empty_fns = HashMap::new();
            let method_result = self.call_compiled_method(
                &cn,
                &owner_class,
                method,
                &method_def,
                &cc,
                attributes,
                args,
                invocant,
                &empty_fns,
            );
            if pushed_dispatch {
                self.interpreter.pop_method_dispatch();
            }
            self.interpreter.pop_method_samewith_context();
            let (result, new_attrs) = method_result?;
            // Propagate attribute mutations to all bindings of this instance
            if let Some(id) = target_id {
                self.interpreter.overwrite_instance_bindings_by_identity(
                    &cn,
                    id,
                    new_attrs.clone(),
                );
                // Auto-FETCH if the method returned a Proxy
                if let Value::Proxy { ref fetcher, .. } = result {
                    return self
                        .interpreter
                        .proxy_fetch(fetcher, None, &cn, &new_attrs, id);
                }
            }
            return Ok(result);
        }
        self.interpreter
            .call_method_with_values(target, method, args)
    }

    fn try_compiled_method_mut_or_interpret(
        &mut self,
        target_name: &str,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Instance { class_name, .. } = &target {
            let class = class_name.resolve();
            if self.interpreter.is_native_method(&class, method) {
                return self.interpreter.call_method_mut_with_values(
                    target_name,
                    target,
                    method,
                    args,
                );
            }
        }
        if matches!(
            method,
            "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
        ) {
            return self
                .interpreter
                .call_method_mut_with_values(target_name, target, method, args);
        }
        if method.starts_with('!') {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name {
                let resolved = self
                    .interpreter
                    .resolve_private_method_for_vm(&cn, method, &args);
                if let Some((owner_class, method_def)) = resolved {
                    let caller_allowed = self
                        .interpreter
                        .can_fast_dispatch_private_method_vm(&owner_class);
                    if caller_allowed && let Some(ref cc) = method_def.compiled_code {
                        let cc = cc.clone();
                        let target_id = match &target {
                            Value::Instance { id, .. } => Some(*id),
                            _ => None,
                        };
                        let attributes = match &target {
                            Value::Instance { attributes, .. } => (**attributes).clone(),
                            _ => std::collections::HashMap::new(),
                        };
                        let invocant_for_dispatch = if attributes.is_empty() {
                            Value::Package(crate::symbol::Symbol::intern(&cn))
                        } else {
                            target.clone()
                        };
                        let pushed_dispatch = self.interpreter.push_method_dispatch_frame(
                            &cn,
                            method,
                            &args,
                            invocant_for_dispatch,
                        );
                        let invocant = Some(target);
                        let empty_fns = HashMap::new();
                        let method_result = self.call_compiled_method(
                            &cn,
                            &owner_class,
                            method,
                            &method_def,
                            &cc,
                            attributes,
                            args,
                            invocant,
                            &empty_fns,
                        );
                        if pushed_dispatch {
                            self.interpreter.pop_method_dispatch();
                        }
                        self.interpreter.pop_method_samewith_context();
                        let (result, new_attrs) = method_result?;
                        if let Some(id) = target_id {
                            self.interpreter.overwrite_instance_bindings_by_identity(
                                &cn,
                                id,
                                new_attrs.clone(),
                            );
                            if let Value::Proxy { ref fetcher, .. } = result {
                                return self
                                    .interpreter
                                    .proxy_fetch(fetcher, None, &cn, &new_attrs, id);
                            }
                        }
                        return Ok(result);
                    }
                }
            }
        }
        let class_name = match &target {
            Value::Instance { class_name, .. } => Some(class_name.resolve()),
            Value::Package(name) => Some(name.resolve()),
            _ => None,
        };
        if let Some(cn) = class_name
            && let Some((owner_class, method_def)) = self
                .interpreter
                .resolve_method_with_owner(&cn, method, &args)
            && let Some(ref cc) = method_def.compiled_code
        {
            let cc = cc.clone();
            let target_id = match &target {
                Value::Instance { id, .. } => Some(*id),
                _ => None,
            };
            let attributes = match &target {
                Value::Instance { attributes, .. } => (**attributes).clone(),
                _ => std::collections::HashMap::new(),
            };
            let invocant_for_dispatch = if attributes.is_empty() {
                Value::Package(crate::symbol::Symbol::intern(&cn))
            } else {
                target.clone()
            };
            let pushed_dispatch = self.interpreter.push_method_dispatch_frame(
                &cn,
                method,
                &args,
                invocant_for_dispatch,
            );
            let invocant = Some(target);
            let empty_fns = HashMap::new();
            let method_result = self.call_compiled_method(
                &cn,
                &owner_class,
                method,
                &method_def,
                &cc,
                attributes,
                args,
                invocant,
                &empty_fns,
            );
            if pushed_dispatch {
                self.interpreter.pop_method_dispatch();
            }
            self.interpreter.pop_method_samewith_context();
            let (result, new_attrs) = method_result?;
            if let Some(id) = target_id {
                self.interpreter.overwrite_instance_bindings_by_identity(
                    &cn,
                    id,
                    new_attrs.clone(),
                );
                if let Value::Proxy { ref fetcher, .. } = result {
                    return self
                        .interpreter
                        .proxy_fetch(fetcher, None, &cn, &new_attrs, id);
                }
            }
            return Ok(result);
        }
        self.interpreter
            .call_method_mut_with_values(target_name, target, method, args)
    }

    pub(super) fn exec_call_method_dynamic_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
    ) -> Result<(), RuntimeError> {
        self.ensure_env_synced(code);
        let arity = arity as usize;
        if self.stack.len() < arity + 2 {
            return Err(RuntimeError::new("VM stack underflow in CallMethodDynamic"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, false);
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
                self.try_compiled_method_or_interpret(target, &method, args)
            }
        };
        self.stack.push(call_result?);
        self.env_dirty = true;
        Ok(())
    }

    pub(super) fn exec_call_method_dynamic_mut_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        target_name_idx: u32,
    ) -> Result<(), RuntimeError> {
        self.ensure_env_synced(code);
        let target_name = Self::const_str(code, target_name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity + 2 {
            return Err(RuntimeError::new(
                "VM stack underflow in CallMethodDynamicMut",
            ));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, false);
        }
        let name_val = self
            .stack
            .pop()
            .ok_or_else(|| RuntimeError::new("VM stack underflow in CallMethodDynamicMut"))?;
        let target = self
            .stack
            .pop()
            .ok_or_else(|| RuntimeError::new("VM stack underflow in CallMethodDynamicMut"))?;
        let call_result = if matches!(
            &name_val,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
        ) {
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(target);
            call_args.extend(args);
            self.interpreter
                .call_sub_value(name_val, call_args, false)?
        } else {
            let method = name_val.to_string_value();
            self.interpreter
                .call_method_mut_with_values(&target_name, target, &method, args)?
        };
        self.stack.push(call_result);
        self.env_dirty = true;
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
        self.ensure_env_synced(code);
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
        let preserve_empty_slip = Self::preserve_empty_slip_arg(&method);
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, preserve_empty_slip);
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
                    self.try_compiled_method_or_interpret(v.clone(), &method, args.clone())?
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

        // Fast path for Lock::Async.protect — execute block inline in current VM
        if method == "protect"
            && args.len() == 1
            && let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
            && (class_name.resolve() == "Lock::Async" || class_name.resolve() == "Lock")
        {
            let lock_id = match attributes.get("lock-id") {
                Some(Value::Int(id)) if *id > 0 => *id as u64,
                _ => {
                    return Err(RuntimeError::new(
                        "Lock.protect called on Lock without lock-id",
                    ));
                }
            };
            let lock = crate::runtime::native_methods::lock_runtime_by_id(lock_id)
                .ok_or_else(|| RuntimeError::new("Lock.protect could not find lock state"))?;
            let me = crate::runtime::native_methods::current_thread_id();
            crate::runtime::native_methods::acquire_lock(&lock, me)?;
            let code_val = args.into_iter().next().unwrap_or(Value::Nil);
            let result = self.exec_protect_block_inline(code, &code_val);
            let _ = crate::runtime::native_methods::release_lock(&lock, me);
            self.stack.push(result?);
            self.env_dirty = true;
            return Ok(());
        }

        let mut skip_native = quoted
            && matches!(
                method.as_str(),
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            );
        let is_junction_target = match &target {
            Value::Junction { .. } => true,
            Value::Scalar(inner) => matches!(inner.as_ref(), Value::Junction { .. }),
            _ => false,
        };
        if matches!(method.as_str(), "gist" | "raku" | "perl") && is_junction_target {
            skip_native = true;
        }
        // Also skip native if the target has a user-defined method with this name,
        // but NOT for pseudo-methods like DEFINITE, WHAT, etc. which are macros.
        if !skip_native
            && !matches!(
                method.as_str(),
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            )
        {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name
                && self.interpreter.has_user_method(&cn, &method)
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
        if !skip_native
            && method == "keys"
            && target_name.starts_with('%')
            && self
                .interpreter
                .var_hash_key_constraint(&target_name)
                .is_some()
        {
            skip_native = true;
        }
        if !skip_native
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "Proc::Async")
            && matches!(
                method.as_str(),
                "start"
                    | "kill"
                    | "write"
                    | "close-stdin"
                    | "ready"
                    | "print"
                    | "say"
                    | "command"
                    | "started"
                    | "w"
                    | "pid"
                    | "stdout"
                    | "stderr"
                    | "Supply"
            )
        {
            skip_native = true;
        }
        if !skip_native
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "IterationBuffer")
            && matches!(
                method.as_str(),
                "elems"
                    | "AT-POS"
                    | "BIND-POS"
                    | "push"
                    | "unshift"
                    | "List"
                    | "Slip"
                    | "Seq"
                    | "append"
                    | "prepend"
                    | "clear"
            )
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
                self.try_compiled_method_mut_or_interpret(&target_name, target, &method, args)
            }
        } else {
            self.try_compiled_method_mut_or_interpret(&target_name, target, &method, args)
        };
        match modifier.as_deref() {
            Some("?") => {
                self.stack.push(call_result.unwrap_or(Value::Nil));
            }
            Some("+") => {
                let val = call_result?;
                self.stack.push(Value::array(vec![val]));
                self.env_dirty = true;
            }
            Some("*") => match call_result {
                Ok(val) => self.stack.push(Value::array(vec![val])),
                Err(_) => self.stack.push(Value::array(vec![])),
            },
            _ => {
                self.stack.push(call_result?);
                self.env_dirty = true;
            }
        }
        Ok(())
    }

    pub(super) fn exec_call_on_value_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        self.ensure_env_synced(code);
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new("VM stack underflow in CallOnValue"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, false);
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

        // Upgrade WeakSub (e.g., &?BLOCK) to strong Sub before dispatch
        let target = if let Value::WeakSub(ref weak) = target {
            match weak.upgrade() {
                Some(strong) => Value::Sub(strong),
                None => Value::Nil,
            }
        } else {
            target
        };

        // Fast path: if target is a Sub with compiled_code, dispatch to compiled closure
        if let Value::Sub(ref data) = target
            && let Some(ref cc) = data.compiled_code
        {
            let cc = cc.clone();
            let sub_is_rw = data.is_rw;
            let data = data.clone();
            self.interpreter.set_pending_call_arg_sources(arg_sources);
            let result = self.call_compiled_closure(&data, &cc, args, compiled_fns);
            self.interpreter.set_pending_call_arg_sources(None);
            let result = self.interpreter.maybe_fetch_rw_proxy(result?, sub_is_rw)?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }

        let sub_is_rw = if let Value::Sub(ref data) = target {
            data.is_rw
        } else {
            false
        };
        self.interpreter.set_pending_call_arg_sources(arg_sources);
        let result = self.interpreter.eval_call_on_value(target, args);
        self.interpreter.set_pending_call_arg_sources(None);
        let result = self.interpreter.maybe_fetch_rw_proxy(result?, sub_is_rw)?;
        self.stack.push(result);
        self.env_dirty = true;
        Ok(())
    }

    pub(super) fn exec_call_on_code_var_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        arg_sources_idx: Option<u32>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<(), RuntimeError> {
        self.ensure_env_synced(code);
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
            // Fast path: compiled closure dispatch
            if let Value::Sub(ref data) = target
                && let Some(ref cc) = data.compiled_code
            {
                let cc = cc.clone();
                let sub_is_rw = data.is_rw;
                let data = data.clone();
                self.interpreter.set_pending_call_arg_sources(arg_sources);
                let result = self.call_compiled_closure(&data, &cc, args, compiled_fns);
                self.interpreter.set_pending_call_arg_sources(None);
                let result = self.interpreter.maybe_fetch_rw_proxy(result?, sub_is_rw)?;
                self.stack.push(result);
                self.env_dirty = true;
                return Ok(());
            }
            let sub_is_rw = if let Value::Sub(ref data) = target {
                data.is_rw
            } else {
                false
            };
            self.interpreter
                .set_pending_call_arg_sources(arg_sources.clone());
            let result = self.interpreter.eval_call_on_value(target, args);
            self.interpreter.set_pending_call_arg_sources(None);
            self.interpreter.maybe_fetch_rw_proxy(result?, sub_is_rw)?
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?
        } else {
            // Sync VM locals to env before spawning threads so closures capture them
            if name == "start" {
                self.sync_env_from_locals(code);
            }
            self.interpreter.set_pending_call_arg_sources(arg_sources);
            let result = self.interpreter.call_function(&name, args);
            self.interpreter.set_pending_call_arg_sources(None);
            result?
        };
        let result = self.interpreter.maybe_fetch_rw_proxy(result, true)?;
        self.stack.push(result);
        self.env_dirty = true;
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
        self.ensure_env_synced(code);
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
                    Value::Instance { class_name, .. } => Some(class_name.resolve()),
                    Value::Package(name) => Some(name.resolve()),
                    _ => None,
                };
                if let Some(cn) = class_name
                    && self.interpreter.has_user_method(&cn, &method)
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
                            native_result.unwrap_or(Value::Package(Symbol::intern("Any")))
                        } else {
                            match self
                                .call_method_mut_with_temp_target(item, &method, item_args, idx)
                            {
                                Ok((v, updated)) => {
                                    *item = updated;
                                    v
                                }
                                Err(_) => Value::Package(Symbol::intern("Any")),
                            }
                        }
                    } else {
                        match self.call_method_mut_with_temp_target(item, &method, item_args, idx) {
                            Ok((v, updated)) => {
                                *item = updated;
                                v
                            }
                            Err(_) => Value::Package(Symbol::intern("Any")),
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
        if let Value::Array(existing, kind) = &target {
            self.interpreter.overwrite_array_items_by_identity_for_vm(
                existing,
                items.clone(),
                *kind,
            );
            if let Some((source, indices, source_kind)) =
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
                    source_kind,
                );
            }
            self.env_dirty = true;
        }
        // Preserve the container type of the target: Array→Array, List→List
        let result_kind = match &target {
            Value::Array(_, kind) if kind.is_real_array() => ArrayKind::Array,
            _ => ArrayKind::List,
        };
        self.stack
            .push(Value::Array(std::sync::Arc::new(results), result_kind));
        Ok(())
    }

    pub(super) fn exec_hyper_method_call_dynamic_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        modifier_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        self.ensure_env_synced(code);
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
                                .unwrap_or(Value::Package(Symbol::intern("Any"))),
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
                        native_result.unwrap_or(Value::Package(Symbol::intern("Any")))
                    } else {
                        match self.call_method_mut_with_temp_target(item, method, item_args, idx) {
                            Ok((v, updated)) => {
                                *item = updated;
                                v
                            }
                            Err(_) => Value::Package(Symbol::intern("Any")),
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
        if let Value::Array(existing, kind) = &target {
            self.interpreter.overwrite_array_items_by_identity_for_vm(
                existing,
                items.clone(),
                *kind,
            );
            self.env_dirty = true;
        }
        // Preserve the container type of the target
        let result_kind = match &target {
            Value::Array(_, kind) if kind.is_real_array() => ArrayKind::Array,
            _ => ArrayKind::List,
        };
        self.stack
            .push(Value::Array(std::sync::Arc::new(results), result_kind));
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
        self.ensure_env_synced(code);
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
        // Auto-FETCH Proxy args for statement-level calls (same as CallFunc)
        let args = if self.interpreter.in_lvalue_assignment {
            args
        } else {
            self.auto_fetch_proxy_args(args)?
        };
        self.interpreter.set_pending_callsite_line(callsite_line);
        // Check wrap chain for named function calls
        if let Some(sub_id) = self.interpreter.wrap_sub_id_for_name(&name)
            && !self.interpreter.is_wrap_dispatching(sub_id)
            && let Some(sub_val) = self.interpreter.get_wrapped_sub(&name)
        {
            let result = self.interpreter.call_sub_value(sub_val, args, false)?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }
        if let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args) {
            self.interpreter
                .set_pending_call_arg_sources(arg_sources.clone());
            let pkg = self.interpreter.current_package().to_string();
            let call_result =
                self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name);
            self.interpreter.set_pending_call_arg_sources(None);
            call_result?;
            self.env_dirty = true;
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?;
        } else {
            self.interpreter.set_pending_call_arg_sources(arg_sources);
            let exec_result = self.interpreter.exec_call_values(&name, args);
            self.interpreter.set_pending_call_arg_sources(None);
            exec_result?;
            self.env_dirty = true;
        }
        Ok(())
    }

    pub(super) fn exec_exec_call_pairs_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
    ) -> Result<(), RuntimeError> {
        self.ensure_env_synced(code);
        let name = Self::const_str(code, name_idx).to_string();
        let arity = arity as usize;
        if self.stack.len() < arity {
            return Err(RuntimeError::new("VM stack underflow in ExecCallPairs"));
        }
        let start = self.stack.len() - arity;
        let args: Vec<Value> = self.stack.drain(start..).collect();
        // Auto-FETCH Proxy args
        let args = if self.interpreter.in_lvalue_assignment {
            args
        } else {
            self.auto_fetch_proxy_args(args)?
        };
        self.interpreter.exec_call_pairs_values(&name, args)?;
        self.env_dirty = true;
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
        self.ensure_env_synced(code);
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
            self.env_dirty = true;
            return Ok(());
        }
        self.interpreter.exec_call_pairs_values(&name, args)?;
        self.env_dirty = true;
        Ok(())
    }

    /// Execute a protect block inline in the current VM, avoiding the overhead
    /// of creating a new VM.
    fn exec_protect_block_inline(
        &mut self,
        _outer_code: &CompiledCode,
        code_val: &Value,
    ) -> Result<Value, RuntimeError> {
        let (block_cc, block_fns, captured_env, captured_slots) = match code_val {
            Value::Sub(data) => {
                let (block_cc, block_fns, captured_slots) = self
                    .interpreter
                    .get_or_compile_protect_block_with_slots(data);
                self.interpreter
                    .sync_shared_vars_for_names(data.env.keys().map(|name| name.as_str()));
                (block_cc, block_fns, Some(&data.env), captured_slots)
            }
            _ => {
                return self.interpreter.call_protect_block(code_val);
            }
        };

        // Save/swap stack and locals for the block
        let saved_locals = std::mem::take(&mut self.locals);
        let saved_stack = std::mem::take(&mut self.stack);

        // Initialize locals for the block
        self.locals = vec![Value::Nil; block_cc.locals.len()];
        if let Some(captured) = captured_env {
            for i in captured_slots.iter().copied() {
                if let Some(name) = block_cc.locals.get(i)
                    && captured.contains_key(name)
                    && let Some(val) = self.interpreter.env().get(name)
                {
                    self.locals[i] = val.clone();
                }
            }
        }

        // Execute the block's opcodes inline
        let mut sub_ip = 0;
        let mut exec_err = None;
        while sub_ip < block_cc.ops.len() {
            if let Err(e) = self.exec_one(&block_cc, &mut sub_ip, &block_fns) {
                exec_err = Some(e);
                break;
            }
        }

        // Sync locals back to env
        if let Some(captured) = captured_env {
            for i in captured_slots.iter().copied() {
                if let Some(name) = block_cc.locals.get(i)
                    && captured.contains_key(name)
                {
                    self.interpreter
                        .env_mut()
                        .insert(name.clone(), self.locals[i].clone());
                }
            }
        }

        // Get return value before restoring state
        let ret_val = self.stack.pop().unwrap_or(Value::Nil);

        // Restore outer state
        self.locals = saved_locals;
        self.stack = saved_stack;
        self.env_dirty = true;

        match exec_err {
            Some(e) => Err(e),
            None => Ok(ret_val),
        }
    }
}
