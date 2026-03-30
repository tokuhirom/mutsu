use super::*;
use crate::symbol::Symbol;
use std::sync::Arc;

impl VM {
    pub(super) fn exec_call_method_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        self.ensure_env_synced(code);
        // Set pending arg sources for `is rw` dispatch matching
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        self.interpreter
            .set_pending_call_arg_sources(arg_sources.clone());
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
        // Force LazyIoLines into an eager array when calling methods on it,
        // unless the method preserves laziness (e.g., .kv).
        let target = if matches!(&target, Value::LazyIoLines { .. })
            && !matches!(method.as_str(), "kv" | "iterator" | "lazy")
        {
            self.force_if_lazy_io_lines(target)?
        } else {
            target
        };
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
                let r = if let Some(threaded) =
                    self.maybe_autothread_method_args(v, &method, &args)?
                {
                    threaded
                } else if let Some(nr) = self.try_native_method(v, Symbol::intern(&method), &args) {
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

        // Deprecation.report — return the accumulated deprecation report
        if method == "report"
            && matches!(&target, Value::Package(name) if name.resolve() == "Deprecation")
        {
            let result = match crate::runtime::deprecation::take_report() {
                Some(report) => Value::str(report),
                None => Value::Nil,
            };
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
            let result = match self.try_exec_simple_shared_protect_block(code, &code_val)? {
                Some(value) => Ok(value),
                None => self.exec_protect_block_inline(code, &code_val),
            };
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
        if let Value::Instance { class_name, .. } = &target
            && class_name.resolve() == "Failure"
            && !target.is_failure_handled()
            && !matches!(
                method.as_str(),
                "exception"
                    | "handled"
                    | "self"
                    | "defined"
                    | "Bool"
                    | "so"
                    | "not"
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
                    // Resolve hash sentinel entries (bound variable refs, self-refs)
                    // before passing to native methods that iterate hash values.
                    if let Value::Hash(ref items) = target
                        && Self::hash_has_sentinels(items)
                    {
                        let resolved = self.resolve_hash_for_iteration(items);
                        if let Some(native_result) =
                            self.try_native_method(&resolved, Symbol::intern(&method), &args)
                        {
                            let result = native_result;
                            match modifier.as_deref() {
                                Some("?") => {
                                    self.stack.push(result.unwrap_or(Value::Nil));
                                    self.env_dirty = true;
                                }
                                _ => {
                                    self.stack.push(result?);
                                    self.env_dirty = true;
                                }
                            }
                            return Ok(());
                        }
                    }
                    // .Slip on arrays with `is default(X)`: fill holes with
                    // the default value instead of leaving Package("Any").
                    if method == "Slip" && args.is_empty() && matches!(&target, Value::Array(..)) {
                        if let Some(def) = self.interpreter.container_default(&target).cloned() {
                            let Value::Array(items, ..) = &target else {
                                unreachable!()
                            };
                            let converted: Vec<Value> = items
                                .iter()
                                .map(|v| {
                                    if matches!(v, Value::Package(name) if name == "Any") {
                                        def.clone()
                                    } else {
                                        v.clone()
                                    }
                                })
                                .collect();
                            Ok(Value::Slip(std::sync::Arc::new(converted)))
                        } else if let Some(native_result) =
                            self.try_native_method(&target, Symbol::intern(&method), &args)
                        {
                            native_result
                        } else {
                            self.try_compiled_method_or_interpret(target, &method, args)
                        }
                    } else if let Some(native_result) =
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
}
