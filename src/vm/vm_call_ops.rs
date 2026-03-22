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
            Value::Array(elements, ..) | Value::Seq(elements) => {
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
            // When a Pair or ValuePair is slipped via |, it becomes a named
            // argument (regular Pair).  ValuePair is the "positional pair"
            // wrapper produced by (:key(val)), but |$pair always flattens it
            // back to a named argument in Raku.
            Value::ValuePair(key, val) => {
                if let Value::Str(name) = key.as_ref() {
                    args.push(Value::Pair(name.to_string(), val));
                } else {
                    args.push(Value::ValuePair(key, val));
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
            "return-rw"
                | "return"
                | "die"
                | "fail"
                | "leave"
                | "__mutsu_assign_method_lvalue"
                | "__mutsu_index_assign_method_lvalue"
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
        // Junction auto-threading for function call arguments:
        // If any positional arg is a Junction and the function parameter doesn't accept
        // Junction (i.e., not typed as Mu or Junction), auto-thread over the junction.
        if let Some(autothread_result) =
            self.maybe_autothread_func_call(code, &name, &args, &arg_sources, compiled_fns)?
        {
            self.stack.push(autothread_result);
            self.env_dirty = true;
            return Ok(());
        }

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
        let result = self.dispatch_func_call_inner(
            code,
            &name,
            args,
            arg_sources,
            call_me_override,
            compiled_fns,
        )?;
        self.stack.push(result);
        self.env_dirty = true;
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
        // .return method: triggers a return from the enclosing sub with the invocant
        // as the return value. Does NOT auto-thread over junctions.
        if method == "return" && args.is_empty() {
            let mut err = RuntimeError::new("return");
            err.return_value = Some(target);
            return Err(err);
        }
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
            self.env_dirty = true;
            return Ok(());
        }

        // Junction auto-threading for method arguments:
        // If any method arg is a Junction, auto-thread over it.
        if let Some(result) = self.maybe_autothread_method_args(&target, &method, &args)? {
            self.stack.push(result);
            self.env_dirty = true;
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
                    | "bind-stdin"
                    | "bind-stdout"
                    | "bind-stderr"
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
        // Force gather-sourced LazyList before method dispatch for methods that need
        // element access. This must happen before the native method fast path, because
        // builtins don't have access to the interpreter for forcing.
        // Non-gather LazyLists (e.g. from infinite ranges) are NOT forced here — they
        // go through builtins which may return Failure for methods like .elems.
        let target = if let Value::LazyList(ref ll) = target
            && matches!(
                ll.env.get("__mutsu_lazylist_from_gather"),
                Some(crate::value::Value::Bool(true))
            )
            && Self::lazy_list_needs_forcing(&method)
        {
            let saved_env = self.interpreter.env().clone();
            let items = self.force_lazy_list_vm(ll)?;
            if !matches!(method.as_str(), "elems" | "hyper" | "race") {
                *self.interpreter.env_mut() = saved_env;
            }
            Value::Seq(std::sync::Arc::new(items))
        } else {
            target
        };
        // Regex.Bool / Regex.so: smartmatch against $_ (needs runtime context)
        if matches!(method.as_str(), "Bool" | "so")
            && args.is_empty()
            && matches!(
                &target,
                Value::Regex(_)
                    | Value::RegexWithAdverbs { .. }
                    | Value::Routine { is_regex: true, .. }
            )
        {
            let topic = self
                .interpreter
                .env()
                .get("_")
                .cloned()
                .unwrap_or(Value::Nil);
            let matched = self.vm_smart_match(&topic, &target);
            self.stack.push(Value::Bool(matched));
            self.env_dirty = true;
            return Ok(());
        }
        // Unhandled Failure explosion: calling a non-Failure method on an unhandled
        // Failure should throw the stored exception (Raku behavior).
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && class_name.resolve() == "Failure"
            && !attributes.get("handled").is_some_and(Value::truthy)
            && !matches!(
                method.as_str(),
                "exception"
                    | "handled"
                    | "self"
                    | "defined"
                    | "Bool"
                    | "so"
                    | "gist"
                    | "Str"
                    | "raku"
                    | "perl"
                    | "WHICH"
                    | "backtrace"
                    | "is-handling"
                    | "WHAT"
                    | "^name"
                    | "isa"
                    | "does"
                    | "ACCEPTS"
                    | "Failure"
                    | "sink"
            )
            && let Some(err) = self
                .interpreter
                .failure_to_runtime_error_if_unhandled(&target)
        {
            return Err(err);
        }
        // For .* and .+ modifiers, skip the single-dispatch call and go
        // directly to the all-methods-in-MRO path to avoid double execution.
        match modifier.as_deref() {
            Some("+") => {
                let vals =
                    self.call_method_all_with_fallback(&target, &method, &args, skip_native)?;
                self.stack.push(Value::array(vals));
                self.env_dirty = true;
            }
            Some("*") => {
                match self.call_method_all_with_fallback(&target, &method, &args, skip_native) {
                    Ok(vals) => self.stack.push(Value::array(vals)),
                    Err(_) => self.stack.push(Value::array(vec![])),
                }
                self.env_dirty = true;
            }
            _ => {
                // Nil method fallback: in Raku, calling most methods on Nil returns Nil.
                // Certain mutating methods throw exceptions.
                // This must be in the VM path (not the interpreter's call_method_with_values)
                // to avoid affecting internal dispatch (e.g. max :by comparators).
                if matches!(&target, Value::Nil) {
                    match method.as_str() {
                        "BIND-POS" | "BIND-KEY" | "ASSIGN-POS" | "ASSIGN-KEY" | "STORE"
                        | "push" | "append" | "unshift" | "prepend" => {
                            return Err(RuntimeError::new(format!(
                                "Invocant of method '{}' must be an object instance of type \
                                 'Any', not a type object of type 'Nil'.  Did you forget a \
                                 '.new'?",
                                method
                            )));
                        }
                        "defined" | "Bool" | "so" | "not" | "gist" | "Str" | "raku" | "perl"
                        | "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "VAR" | "DEFINITE"
                        | "isa" | "does" | "can" | "^name" | "^mro" | "^pun" | "new" | "bless"
                        | "clone" | "item" | "self" | "sink" => {
                            // Fall through to normal dispatch
                        }
                        _ => {
                            self.stack.push(Value::Nil);
                            self.env_dirty = true;
                            return Ok(());
                        }
                    }
                }
                // If we have a pending Proxy subclass attribute reference and this
                // is a mutating array method, delegate to the shared storage mutator
                // so the mutation is visible through the Proxy subclass.
                if matches!(
                    method.as_str(),
                    "push" | "pop" | "shift" | "unshift" | "append" | "prepend"
                ) && matches!(&target, Value::Array(..))
                    && let Some((attrs_ref, attr_name)) =
                        self.interpreter.pending_proxy_subclass_attr.take()
                {
                    let result = self
                        .interpreter
                        .proxy_subclass_array_mutate(&attrs_ref, &attr_name, &method, &args)?;
                    self.stack.push(result);
                    self.env_dirty = true;
                    return Ok(());
                }
                // Fast path for shift/pop on array values in the non-mutating
                // (CallMethod) path. Returns the removed element without modifying
                // any variable. This handles cases like [1,2,3].shift where there
                // is no variable to mutate. The CallMethodMut path handles variable
                // targets separately.
                let call_result = if matches!(method.as_str(), "shift" | "pop")
                    && args.is_empty()
                    && matches!(&target, Value::Array(_, kind) if kind.is_real_array())
                {
                    if let Value::Array(items, _) = &target {
                        Ok(if items.is_empty() {
                            crate::runtime::make_empty_array_failure(&method)
                        } else if method == "shift" {
                            items[0].clone()
                        } else {
                            items[items.len() - 1].clone()
                        })
                    } else {
                        unreachable!()
                    }
                } else if !skip_native {
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
                        self.env_dirty = true;
                    }
                    _ => {
                        self.stack.push(call_result?);
                        self.env_dirty = true;
                    }
                }
            }
        }
        Ok(())
    }

    /// Try compiled method fast path; fall back to interpreter.
    pub(super) fn try_compiled_method_or_interpret(
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
            self.vm_call_on_value(name_val, call_args, None)
        } else {
            let method = name_val.to_string_value();
            // .return method: triggers a return from the enclosing sub
            if method == "return" && args.is_empty() {
                let mut err = RuntimeError::new("return");
                err.return_value = Some(target);
                return Err(err);
            }
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
            self.vm_call_on_value(name_val, call_args, None)?
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
        // Detect calls on undeclared type names: when a BareWord resolved to a Str
        // (because the name wasn't a known type/class), and .new() is called on it,
        // this means the user tried to instantiate a nonexistent class.
        if method == "new"
            && let Value::Str(s) = &target
            && **s == target_name
            && target_name
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase())
            && !self.interpreter.has_type(&target_name)
            && !Self::is_builtin_type(&target_name)
            && !self.interpreter.has_class(&target_name)
        {
            return Err(RuntimeError::undeclared_symbols(format!(
                "Undeclared name:\n    {} used at line 1",
                target_name,
            )));
        }
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
            self.env_dirty = true;
            return Ok(());
        }

        // Junction auto-threading for method arguments (mut variant)
        if let Some(result) = self.maybe_autothread_method_args(&target, &method, &args)? {
            self.stack.push(result);
            self.env_dirty = true;
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
                    | "bind-stdin"
                    | "bind-stdout"
                    | "bind-stderr"
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
        // Handle Match.make — must mutate the Match instance's `ast` attribute
        // and write the modified Match back to the variable.
        if method == "make"
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "Match")
        {
            let value = args.into_iter().next().unwrap_or(Value::Nil);
            if let Value::Instance {
                class_name,
                attributes,
                id,
            } = target
            {
                let mut attrs = crate::value::InstanceAttrs::clone(&attributes);
                attrs.insert("ast".to_string(), value.clone());
                let updated = Value::Instance {
                    class_name,
                    attributes: Arc::new(crate::value::InstanceAttrs::new(
                        class_name, attrs, id, false,
                    )),
                    id,
                };
                self.interpreter
                    .env_mut()
                    .insert(target_name.to_string(), updated);
                self.interpreter
                    .env_mut()
                    .insert("made".to_string(), value.clone());
                self.interpreter.action_made = Some(value.clone());
            }
            self.stack.push(value);
            self.env_dirty = true;
            return Ok(());
        }
        // Auto-vivify undefined values (Nil, Any, Mu type objects) to empty Arrays
        // for mutating list methods. In Raku, calling push/unshift/append/prepend on
        // an undefined variable auto-vivifies it to an Array.
        let target = if matches!(method.as_str(), "push" | "unshift" | "append" | "prepend")
            && (matches!(&target, Value::Nil)
                || matches!(
                    &target,
                    Value::Package(name) if matches!(name.resolve().as_str(), "Any" | "Mu")
                )) {
            let empty_array = Value::real_array(vec![]);
            self.interpreter
                .env_mut()
                .insert(target_name.to_string(), empty_array.clone());
            self.env_dirty = true;
            empty_array
        } else {
            target
        };
        // For .* and .+ modifiers, skip the single-dispatch call and go
        // directly to the all-methods-in-MRO path to avoid double execution.
        match modifier.as_deref() {
            Some("+") => {
                let vals =
                    self.call_method_all_with_fallback(&target, &method, &args, skip_native)?;
                self.stack.push(Value::array(vals));
                self.env_dirty = true;
            }
            Some("*") => {
                match self.call_method_all_with_fallback(&target, &method, &args, skip_native) {
                    Ok(vals) => self.stack.push(Value::array(vals)),
                    Err(_) => self.stack.push(Value::array(vec![])),
                }
                self.env_dirty = true;
            }
            _ => {
                let call_result = if !skip_native {
                    if let Some(native_result) =
                        self.try_native_method(&target, Symbol::intern(&method), &args)
                    {
                        native_result
                    } else {
                        self.try_compiled_method_mut_or_interpret(
                            &target_name,
                            target,
                            &method,
                            args,
                        )
                    }
                } else {
                    self.try_compiled_method_mut_or_interpret(&target_name, target, &method, args)
                };
                match modifier.as_deref() {
                    Some("?") => {
                        self.stack.push(call_result.unwrap_or(Value::Nil));
                        self.env_dirty = true;
                    }
                    _ => {
                        self.stack.push(call_result?);
                        self.env_dirty = true;
                    }
                }
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

        let sub_is_rw = if let Value::Sub(ref data) = target {
            data.is_rw
        } else {
            false
        };
        self.interpreter.set_pending_call_arg_sources(arg_sources);
        let result = self.vm_call_on_value(target, args, Some(compiled_fns));
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
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, false);
        }
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
            let sub_is_rw = if let Value::Sub(ref data) = target {
                data.is_rw
            } else {
                false
            };
            self.interpreter
                .set_pending_call_arg_sources(arg_sources.clone());
            let result = self.vm_call_on_value(target, args, Some(compiled_fns));
            self.interpreter.set_pending_call_arg_sources(None);
            self.interpreter.maybe_fetch_rw_proxy(result?, sub_is_rw)?
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?
        } else if !self.interpreter.has_proto(&name)
            && let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args)
        {
            let cf_auto_fetch = !cf.is_raw;
            let pkg = self.interpreter.current_package().to_string();
            self.interpreter
                .set_pending_call_arg_sources(arg_sources.clone());
            let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name);
            self.interpreter.set_pending_call_arg_sources(None);
            self.interpreter
                .maybe_fetch_rw_proxy(result?, cf_auto_fetch)?
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
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // Flatten any Slip values in the argument list (from |capture slipping)
        let mut args = Vec::new();
        for arg in raw_args {
            Self::append_flattened_call_arg(&mut args, arg, false);
        }
        let target = self
            .stack
            .pop()
            .ok_or_else(|| RuntimeError::new("VM stack underflow in HyperMethodCall target"))?;
        let mut items = crate::runtime::value_to_list(&target);
        let mut results = Vec::with_capacity(items.len());
        for (idx, item) in items.iter_mut().enumerate() {
            let method = Self::rewrite_method_name(&method_raw, modifier.as_deref());
            // Special case: CALL-ME on callable items (from >>.(args) syntax).
            // Instead of method dispatch, invoke the item directly as a callable.
            if method == "CALL-ME"
                && matches!(
                    item,
                    Value::Sub(..) | Value::WeakSub(..) | Value::Routine { .. } | Value::Mixin(..)
                )
            {
                let val = self.vm_call_on_value(item.clone(), args.clone(), None)?;
                results.push(val);
                continue;
            }
            // Special case: user-defined postfix:<...> operators applied via hyper (»op / >>op).
            // These are function calls, not method calls.
            // Exclude built-in postfix operators (++, --) which are handled by method dispatch.
            if method.starts_with("postfix:<")
                && !matches!(method.as_str(), "postfix:<++>" | "postfix:<-->")
            {
                let mut call_args = vec![item.clone()];
                call_args.extend(args.clone());
                let val = self.interpreter.call_function(&method, call_args)?;
                results.push(val);
                continue;
            }
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
                    // Hyper method dispatch on nested list/array/seq items.
                    // Raku's >> descends into Iterable structures, but stops
                    // if the method is natively defined on the list type
                    // (e.g., .join, .elems, .sort, .reverse, .unique, .squish).
                    let is_iterable_item =
                        matches!(item, Value::Array(..) | Value::Seq(..) | Value::Slip(..));
                    let is_list_native_method = matches!(
                        method.as_str(),
                        "join"
                            | "elems"
                            | "end"
                            | "sort"
                            | "reverse"
                            | "unique"
                            | "squish"
                            | "pick"
                            | "roll"
                            | "head"
                            | "tail"
                            | "first"
                            | "min"
                            | "max"
                            | "minmax"
                            | "sum"
                            | "flat"
                            | "eager"
                            | "lazy"
                            | "sink"
                            | "cache"
                            | "List"
                            | "Array"
                            | "Seq"
                            | "Slip"
                            | "Supply"
                            | "Set"
                            | "SetHash"
                            | "Bag"
                            | "BagHash"
                            | "Mix"
                            | "MixHash"
                            | "Str"
                            | "gist"
                            | "raku"
                            | "perl"
                            | "WHAT"
                            | "WHO"
                            | "HOW"
                            | "so"
                            | "Bool"
                            | "Numeric"
                            | "Int"
                            | "Rat"
                            | "Real"
                            | "hash"
                            | "Hash"
                            | "kv"
                            | "keys"
                            | "values"
                            | "pairs"
                            | "antipairs"
                            | "classify"
                            | "categorize"
                            | "map"
                            | "grep"
                            | "reduce"
                            | "produce"
                            | "combinations"
                            | "permutations"
                            | "rotate"
                            | "batch"
                            | "rotor"
                            | "repeated"
                            | "snip"
                            | "defined"
                            | "DEFINITE"
                            | "item"
                            | "list"
                            | "AT-POS"
                            | "AT-KEY"
                            | "EXISTS-POS"
                            | "EXISTS-KEY"
                            | "DELETE-POS"
                            | "DELETE-KEY"
                            | "ASSIGN-POS"
                            | "ASSIGN-KEY"
                            | "BIND-POS"
                            | "BIND-KEY"
                            | "push"
                            | "pop"
                            | "shift"
                            | "unshift"
                            | "append"
                            | "prepend"
                            | "splice"
                    );
                    if is_iterable_item && !is_list_native_method {
                        let sub_items = crate::runtime::value_to_list(item);
                        let mut sub_results = Vec::with_capacity(sub_items.len());
                        for sub_item in sub_items {
                            let sub_val = if !skip_native {
                                if let Some(native_result) = self.try_native_method(
                                    &sub_item,
                                    Symbol::intern(&method),
                                    &item_args,
                                ) {
                                    native_result?
                                } else {
                                    let (v, _updated) = self.call_method_mut_with_temp_target(
                                        &sub_item,
                                        &method,
                                        item_args.clone(),
                                        idx,
                                    )?;
                                    v
                                }
                            } else {
                                let (v, _updated) = self.call_method_mut_with_temp_target(
                                    &sub_item,
                                    &method,
                                    item_args.clone(),
                                    idx,
                                )?;
                                v
                            };
                            sub_results.push(sub_val);
                        }
                        let sub_kind = match item {
                            Value::Array(_, kind) => *kind,
                            _ => ArrayKind::List,
                        };
                        results.push(Value::Array(std::sync::Arc::new(sub_results), sub_kind));
                    } else {
                        let val = if !skip_native {
                            if let Some(native_result) =
                                self.try_native_method(item, Symbol::intern(&method), &item_args)
                            {
                                native_result?
                            } else {
                                let (v, updated) = self.call_method_mut_with_temp_target(
                                    item, &method, item_args, idx,
                                )?;
                                *item = updated;
                                v
                            }
                        } else {
                            let (v, updated) = self
                                .call_method_mut_with_temp_target(item, &method, item_args, idx)?;
                            *item = updated;
                            v
                        };
                        results.push(val);
                    }
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
                            self.vm_call_on_value(name_val.clone(), call_args, None)
                                .unwrap_or(Value::Package(Symbol::intern("Any"))),
                        );
                    }
                    Some("+") => {
                        let val = self.vm_call_on_value(name_val.clone(), call_args, None)?;
                        results.push(Value::array(vec![val]));
                    }
                    Some("*") => match self.vm_call_on_value(name_val.clone(), call_args, None) {
                        Ok(v) => results.push(Value::array(vec![v])),
                        Err(_) => results.push(Value::array(vec![])),
                    },
                    _ => {
                        results.push(self.vm_call_on_value(name_val.clone(), call_args, None)?);
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
        compiled_fns: &HashMap<String, CompiledFunction>,
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
        // Try compiled function dispatch first
        if let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args) {
            let pkg = self.interpreter.current_package().to_string();
            self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
            self.env_dirty = true;
            return Ok(());
        }
        // Try native function
        if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            native_result?;
            self.env_dirty = true;
            return Ok(());
        }
        // Fall back to interpreter
        self.interpreter.exec_call_pairs_values(&name, args)?;
        self.env_dirty = true;
        Ok(())
    }

    /// Execute a call with capture slip: regular args + 1 slip arg on stack.
    /// The slip arg is flattened into the argument list.
    pub(super) fn exec_exec_call_slip_op(
        &mut self,
        code: &CompiledCode,
        compiled_fns: &HashMap<String, CompiledFunction>,
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
        // Try compiled function dispatch first
        if let Some(cf) = self.find_compiled_function(compiled_fns, &name, &args) {
            let pkg = self.interpreter.current_package().to_string();
            self.call_compiled_function_named(cf, args, compiled_fns, &pkg, &name)?;
            self.env_dirty = true;
            return Ok(());
        }
        // Try native function (same as non-slip call path)
        if let Some(native_result) = self.try_native_function(Symbol::intern(&name), &args) {
            self.stack.push(native_result?);
            self.env_dirty = true;
            return Ok(());
        }
        // Fall back to interpreter
        self.interpreter.exec_call_pairs_values(&name, args)?;
        self.env_dirty = true;
        Ok(())
    }

    /// Execute a protect block inline in the current VM, avoiding the overhead
    /// of creating a new VM.
    fn exec_protect_block_inline(
        &mut self,
        outer_code: &CompiledCode,
        code_val: &Value,
    ) -> Result<Value, RuntimeError> {
        let outer_local_slots: std::collections::HashMap<&str, usize> = outer_code
            .locals
            .iter()
            .enumerate()
            .map(|(idx, name)| (name.as_str(), idx))
            .collect();
        let (block_cc, block_fns, captured_env, captured_bindings, writeback_bindings) =
            match code_val {
                Value::Sub(data) => {
                    let (
                        block_cc,
                        block_fns,
                        captured_bindings,
                        writeback_bindings,
                        captured_names,
                    ) = self
                        .interpreter
                        .get_or_compile_protect_block_with_slots(data);
                    self.interpreter.sync_shared_vars_for_names(
                        captured_names.iter().map(|name| name.as_str()),
                    );
                    (
                        block_cc,
                        block_fns,
                        Some(&data.env),
                        captured_bindings,
                        writeback_bindings,
                    )
                }
                _ => {
                    // TODO: Handle non-Sub protect blocks (e.g. WeakSub, Routine)
                    // in the VM. Currently these are rare and delegate to interpreter.
                    return self.interpreter.call_protect_block(code_val);
                }
            };

        // Save/swap stack and locals for the block
        let mut saved_locals = std::mem::take(&mut self.locals);
        let saved_stack = std::mem::take(&mut self.stack);
        let saved_env_dirty = self.env_dirty;
        let saved_locals_dirty = self.locals_dirty;

        // Initialize locals for the block
        self.locals = vec![Value::Nil; block_cc.locals.len()];
        self.env_dirty = false;
        self.locals_dirty = false;
        if captured_env.is_some() {
            for (slot, name) in captured_bindings.iter() {
                if (name.starts_with('@') || name.starts_with('%'))
                    && self.interpreter.get_shared_var(name).is_some()
                {
                    // Leave shared collections unmaterialized in locals.
                    // GetLocal will read the shared value on demand.
                    continue;
                }
                if let Some(outer_slot) = outer_local_slots.get(name.as_str())
                    && let Some(val) = saved_locals.get(*outer_slot)
                {
                    self.locals[*slot] = val.clone();
                    continue;
                }
                if let Some(val) = self.interpreter.env().get(name) {
                    self.locals[*slot] = val.clone();
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
            for (slot, name) in writeback_bindings.iter() {
                if matches!(
                    self.interpreter.get_shared_var(name),
                    Some(Value::Array(..) | Value::Hash(..))
                ) {
                    continue;
                }
                if let Some(outer_slot) = outer_local_slots.get(name.as_str())
                    && let Some(target) = saved_locals.get_mut(*outer_slot)
                {
                    *target = self.locals[*slot].clone();
                }
                if captured.contains_key(name)
                    && !matches!(
                        self.interpreter.get_shared_var(name),
                        Some(Value::Array(..) | Value::Hash(..))
                    )
                {
                    self.interpreter
                        .env_mut()
                        .insert(name.clone(), self.locals[*slot].clone());
                }
            }
        }

        // Get return value before restoring state
        let ret_val = self.stack.pop().unwrap_or(Value::Nil);

        // Restore outer state
        self.locals = saved_locals;
        self.stack = saved_stack;
        self.env_dirty = saved_env_dirty;
        self.locals_dirty = saved_locals_dirty;

        match exec_err {
            Some(e) => Err(e),
            None => Ok(ret_val),
        }
    }

    /// Inner dispatch for function calls. Handles CALL-ME override, compiled functions,
    /// native functions, and interpreter fallback. Returns the result value.
    fn dispatch_func_call_inner(
        &mut self,
        code: &CompiledCode,
        name: &str,
        args: Vec<Value>,
        arg_sources: Option<Vec<Option<String>>>,
        call_me_override: Option<Value>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Value, RuntimeError> {
        if let Some(callable) = call_me_override {
            let result = self.try_compiled_method_or_interpret(callable, "CALL-ME", args);
            self.interpreter.maybe_fetch_rw_proxy(result?, true)
        } else if !self.interpreter.has_proto(name)
            && let Some(cf) = self.find_compiled_function(compiled_fns, name, &args)
        {
            self.interpreter
                .set_pending_call_arg_sources(arg_sources.clone());
            let pushed_dispatch = self.interpreter.push_multi_dispatch_frame(name, &args);
            self.interpreter.push_samewith_context(name, None);
            let pkg = self.interpreter.current_package().to_string();
            let cf_auto_fetch = !cf.is_raw;
            let result = self.call_compiled_function_named(cf, args, compiled_fns, &pkg, name);
            self.interpreter.set_pending_call_arg_sources(None);
            self.interpreter.pop_samewith_context();
            if pushed_dispatch {
                self.interpreter.pop_multi_dispatch();
            }
            self.interpreter
                .maybe_fetch_rw_proxy(result?, cf_auto_fetch)
        } else if self.interpreter.has_multi_candidates(name) && !self.interpreter.has_proto(name) {
            // User-defined multi candidates take priority over builtins.
            // Call call_function_fallback directly to bypass the builtin match
            // in call_function, which would shadow user-defined multi subs.
            self.interpreter.set_pending_call_arg_sources(arg_sources);
            let result = self.interpreter.call_function_fallback(name, &args);
            self.interpreter.set_pending_call_arg_sources(None);
            self.interpreter.maybe_fetch_rw_proxy(result?, true)
        } else if let Some(native_result) = self.try_native_function(Symbol::intern(name), &args) {
            native_result
        } else {
            // Sync VM locals to env before spawning threads so closures capture them
            if name == "start" {
                self.sync_env_from_locals(code);
            }
            self.interpreter.set_pending_call_arg_sources(arg_sources);
            let result = self.interpreter.call_function(name, args);
            self.interpreter.set_pending_call_arg_sources(None);
            self.interpreter.maybe_fetch_rw_proxy(result?, true)
        }
    }

    /// Check if any function call arguments are Junctions that need auto-threading.
    /// Returns Some(result) if auto-threading was performed, None if no auto-threading needed.
    fn maybe_autothread_func_call(
        &mut self,
        code: &CompiledCode,
        name: &str,
        args: &[Value],
        arg_sources: &Option<Vec<Option<String>>>,
        compiled_fns: &HashMap<String, CompiledFunction>,
    ) -> Result<Option<Value>, RuntimeError> {
        // Skip auto-threading for internal functions and junction constructors
        if name.starts_with("__mutsu_")
            || matches!(
                name,
                "any"
                    | "all"
                    | "one"
                    | "none"
                    | "so"
                    | "not"
                    | "defined"
                    | "return"
                    | "return-rw"
                    | "die"
                    | "fail"
                    | "exit"
                    | "leave"
                    | "succeed"
                    | "infix:<,>"
                    | "infix:<=>>"
                    | "say"
                    | "print"
                    | "put"
                    | "note"
                    | "dd"
                    | "warn"
                    // Test functions accept Mu parameters
                    | "ok"
                    | "nok"
                    | "is"
                    | "isnt"
                    | "is-deeply"
                    | "is-approx"
                    | "is_approx"
                    | "isa-ok"
                    | "does-ok"
                    | "can-ok"
                    | "like"
                    | "unlike"
                    | "cmp-ok"
                    | "dies-ok"
                    | "lives-ok"
                    | "eval-dies-ok"
                    | "eval-lives-ok"
                    | "throws-like"
                    | "subtest"
                    | "skip"
                    | "todo"
                    | "pass"
                    | "flunk"
                    | "bail-out"
                    | "done-testing"
                    | "diag"
                    | "plan"
                    | "is-deeply-junction"
                    // Collection/container functions
                    | "push"
                    | "pop"
                    | "shift"
                    | "unshift"
                    | "append"
                    | "prepend"
                    | "elems"
                    | "join"
                    | "grep"
                    | "map"
                    | "sort"
                    | "reverse"
                    | "flat"
                    | "eager"
                    | "lazy"
                    | "sink"
            )
        {
            return Ok(None);
        }

        // Find arg indices that contain Junctions (positional or named)
        let junction_indices: Vec<usize> = args
            .iter()
            .enumerate()
            .filter(|(_, v)| {
                // Check for junction in named arg (Pair value)
                if let Value::Pair(_, val) = v {
                    return Self::unwrap_junction_value(val).is_some();
                }
                Self::unwrap_junction_value(v).is_some()
            })
            .map(|(i, _)| i)
            .collect();

        if junction_indices.is_empty() {
            return Ok(None);
        }

        // Get param_defs if available to check which params accept Junction
        let param_defs = self.get_func_param_defs_for_autothread(name, args);

        // Without param_defs, we can't safely auto-thread
        if param_defs.is_none() {
            return Ok(None);
        }
        let pds = param_defs.unwrap();

        // Filter to only junction args whose parameter does NOT accept Junction
        let autothread_indices: Vec<usize> = junction_indices
            .into_iter()
            .filter(|&idx| {
                // Check if the arg is a named arg (Pair) — find matching named param
                if let Value::Pair(key, _) = &args[idx] {
                    if let Some(pd) = pds.iter().find(|pd| pd.named && pd.name == *key) {
                        if let Some(tc) = &pd.type_constraint {
                            return !matches!(tc.as_str(), "Mu" | "Junction");
                        }
                        return true; // No type constraint = default Any
                    }
                    return true; // No matching named param found, auto-thread
                }

                // Positional arg — find corresponding positional param
                let positional_pds: Vec<&crate::ast::ParamDef> =
                    pds.iter().filter(|pd| !pd.named).collect();
                if let Some(pd) = positional_pds.get(idx) {
                    // Don't auto-thread if param accepts Mu or Junction
                    if let Some(tc) = &pd.type_constraint {
                        return !matches!(tc.as_str(), "Mu" | "Junction");
                    }
                    // No type constraint means default Any — needs auto-threading
                    return true;
                }
                // If param is slurpy or we're past the defined params, don't auto-thread
                false
            })
            .collect();

        if autothread_indices.is_empty() {
            return Ok(None);
        }

        // Pick the junction to thread over based on priority:
        // all/none junctions first (leftmost), then any/one (leftmost)
        let thread_idx = self.pick_autothread_junction_index(args, &autothread_indices);

        let (kind, values, is_pair) = match Self::extract_junction_from_arg(&args[thread_idx]) {
            Some((k, v, p)) => (k, v, p),
            std::option::Option::None => return Ok(None),
        };

        // Thread over the chosen junction: call the function for each eigenstate
        let mut results = Vec::with_capacity(values.len());
        for eigenstate in values.iter() {
            let mut threaded_args = args.to_vec();
            if let Some(ref pair_key) = is_pair {
                // Replace the Pair value while keeping the key
                threaded_args[thread_idx] =
                    Value::Pair(pair_key.clone(), Box::new(eigenstate.clone()));
            } else {
                threaded_args[thread_idx] = eigenstate.clone();
            }

            // Recursively check for more junctions that need auto-threading
            if let Some(recursive_result) = self.maybe_autothread_func_call(
                code,
                name,
                &threaded_args,
                arg_sources,
                compiled_fns,
            )? {
                results.push(recursive_result);
            } else {
                // No more junctions to thread — actually call the function
                let call_me_override = self.get_call_me_override(name);
                let result = self.dispatch_func_call_inner(
                    code,
                    name,
                    threaded_args,
                    arg_sources.clone(),
                    call_me_override,
                    compiled_fns,
                )?;
                results.push(result);
            }
        }

        Ok(Some(Value::junction(kind, results)))
    }

    /// Unwrap a Junction value (possibly through Scalar wrapper).
    fn unwrap_junction_value(val: &Value) -> Option<(crate::value::JunctionKind, Arc<Vec<Value>>)> {
        match val {
            Value::Junction { kind, values } => Some((kind.clone(), values.clone())),
            Value::Scalar(inner) => {
                if let Value::Junction { kind, values } = inner.as_ref() {
                    Some((kind.clone(), values.clone()))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Extract junction from an argument, handling both positional and named (Pair) args.
    /// Returns (kind, values, pair_key) where pair_key is Some(key) for Pair args.
    fn extract_junction_from_arg(
        val: &Value,
    ) -> Option<(crate::value::JunctionKind, Arc<Vec<Value>>, Option<String>)> {
        if let Value::Pair(key, inner) = val {
            Self::unwrap_junction_value(inner).map(|(k, v)| (k, v, Some(key.clone())))
        } else {
            Self::unwrap_junction_value(val).map(|(k, v)| (k, v, None))
        }
    }

    /// Pick the junction argument to thread over, based on Raku's priority:
    /// all/none junctions are threaded first (leftmost), then any/one (leftmost).
    fn pick_autothread_junction_index(&self, args: &[Value], indices: &[usize]) -> usize {
        use crate::value::JunctionKind::{All, None as JNone};
        // First, look for leftmost all/none junction
        for &idx in indices {
            if let Some((kind, _, _)) = Self::extract_junction_from_arg(&args[idx])
                && matches!(kind, All | JNone)
            {
                return idx;
            }
        }
        // Then, leftmost any/one junction
        indices[0]
    }

    /// Get CALL-ME override for a function name (extracted from exec_call_func_op).
    fn get_call_me_override(&self, name: &str) -> Option<Value> {
        self.interpreter
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
            })
    }

    /// Check if any method call arguments are Junctions that need auto-threading.
    /// Returns Some(result) if auto-threading was performed, None otherwise.
    fn maybe_autothread_method_args(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        // Don't auto-thread args for methods that natively handle junctions
        // or that accept Mu/Junction arguments (matchers, comparators, etc.)
        if matches!(
            method,
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
                | "return"
                // Methods that accept matchers/code blocks where junctions are used as values
                | "grep"
                | "first"
                | "map"
                | "sort"
                | "min"
                | "max"
                | "minmax"
                | "classify"
                | "categorize"
                | "reduce"
                | "produce"
                | "supply"
                | "unique"
                | "repeated"
                | "squish"
                | "race"
                | "hyper"
                // Collection methods that accept any value
                | "push"
                | "unshift"
                | "append"
                | "prepend"
                | "ACCEPTS"
                | "cmp"
                | "STORE"
                | "AT-POS"
                | "AT-KEY"
                | "ASSIGN-POS"
                | "ASSIGN-KEY"
                | "BIND-POS"
                | "BIND-KEY"
                | "EXISTS-POS"
                | "EXISTS-KEY"
                | "DELETE-POS"
                | "DELETE-KEY"
                // Smartmatch and type checking
                | "isa"
                | "does"
                | "can"
                | "FALLBACK"
                // IO and output
                | "say"
                | "print"
                | "put"
                | "note"
        ) {
            return Ok(None);
        }

        // Find junction arg indices (including junctions inside named Pair args)
        let junction_indices: Vec<usize> = args
            .iter()
            .enumerate()
            .filter(|(_, v)| {
                if let Value::Pair(_, val) = v {
                    return Self::unwrap_junction_value(val).is_some();
                }
                Self::unwrap_junction_value(v).is_some()
            })
            .map(|(i, _)| i)
            .collect();

        if junction_indices.is_empty() {
            return Ok(None);
        }

        // Pick the junction to thread over based on priority
        let thread_idx = self.pick_autothread_junction_index(args, &junction_indices);

        let (kind, values, is_pair) = match Self::extract_junction_from_arg(&args[thread_idx]) {
            Some((k, v, p)) => (k, v, p),
            std::option::Option::None => return Ok(None),
        };

        // Get instance identity for refreshing target after each call
        let target_identity = match target {
            Value::Instance { class_name, id, .. } => Some((class_name.resolve(), *id)),
            _ => None,
        };

        // Thread over the chosen junction
        let mut results = Vec::with_capacity(values.len());
        let mut current_target = target.clone();
        for eigenstate in values.iter() {
            let mut threaded_args = args.to_vec();
            if let Some(ref pair_key) = is_pair {
                threaded_args[thread_idx] =
                    Value::Pair(pair_key.clone(), Box::new(eigenstate.clone()));
            } else {
                threaded_args[thread_idx] = eigenstate.clone();
            }

            // Recursively check for more junctions
            if let Some(recursive_result) =
                self.maybe_autothread_method_args(&current_target, method, &threaded_args)?
            {
                results.push(recursive_result);
            } else {
                // No more junctions — actually call the method
                let r = if let Some(nr) =
                    self.try_native_method(&current_target, Symbol::intern(method), &threaded_args)
                {
                    nr?
                } else {
                    self.try_compiled_method_or_interpret(
                        current_target.clone(),
                        method,
                        threaded_args,
                    )?
                };
                results.push(r);
            }

            // Mark env as dirty so subsequent reads see updated values
            self.env_dirty = true;

            // Refresh the target from the environment to pick up attribute mutations
            if let Some((ref cn, id)) = target_identity
                && let Some(refreshed) = self.find_instance_in_env(cn, id)
            {
                current_target = refreshed;
            }
        }

        Ok(Some(Value::junction(kind, results)))
    }

    /// Find an instance in the environment by class name and id.
    /// Used to refresh a target after mutation during auto-threading.
    fn find_instance_in_env(&self, class_name: &str, id: u64) -> Option<Value> {
        for v in self.interpreter.env().values() {
            if let Value::Instance {
                class_name: cn,
                id: vid,
                ..
            } = v
                && cn.resolve() == class_name
                && *vid == id
            {
                return Some(v.clone());
            }
        }
        None
    }

    /// Recursively unwrap a Junction to get a non-junction eigenstate.
    fn unwrap_junction_deep(val: &Value) -> Value {
        match val {
            Value::Junction { values, .. } => {
                if let Some(first) = values.first() {
                    Self::unwrap_junction_deep(first)
                } else {
                    Value::Nil
                }
            }
            Value::Scalar(inner) if matches!(inner.as_ref(), Value::Junction { .. }) => {
                Self::unwrap_junction_deep(inner)
            }
            Value::Pair(key, val) => {
                let inner = Self::unwrap_junction_deep(val);
                if std::ptr::eq(val.as_ref(), &inner) {
                    return val.as_ref().clone();
                }
                Value::Pair(key.clone(), Box::new(inner))
            }
            other => other.clone(),
        }
    }

    /// Try to get param_defs for a function to determine which params accept Junction.
    fn get_func_param_defs_for_autothread(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Option<Vec<crate::ast::ParamDef>> {
        // Replace junction args with non-junction placeholder values for type resolution
        let resolved_args: Vec<Value> = args.iter().map(Self::unwrap_junction_deep).collect();
        self.interpreter
            .resolve_function_with_types(name, &resolved_args)
            .map(|def| def.param_defs)
    }
}
