use super::*;
use crate::symbol::Symbol;
use std::sync::Arc;

impl VM {
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
        // Force lazy IO lines for non-lazy-preserving methods
        let method_name_str = name_val.to_string_value();
        let target = if matches!(&target, Value::LazyIoLines { .. })
            && !matches!(method_name_str.as_str(), "kv" | "iterator" | "lazy")
        {
            self.force_if_lazy_io_lines(target)?
        } else {
            target
        };
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
            // .hyper/.race with named arguments: validate, then create HyperSeq/RaceSeq
            if matches!(method.as_str(), "hyper" | "race") {
                // Extract batch/degree for validation
                let mut batch: Option<i64> = None;
                let mut degree: Option<i64> = None;
                for arg in &args {
                    let (key, val) = match arg {
                        Value::Pair(k, v) => (k.clone(), crate::runtime::to_int(v)),
                        Value::ValuePair(k, v) => (k.to_string_value(), crate::runtime::to_int(v)),
                        _ => continue,
                    };
                    match key.as_str() {
                        "batch" => batch = Some(val),
                        "degree" => degree = Some(val),
                        _ => {}
                    }
                }
                if let Some(b) = batch
                    && b <= 0
                {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("method".to_string(), Value::str(method.clone()));
                    attrs.insert("name".to_string(), Value::str("batch".to_string()));
                    attrs.insert("value".to_string(), Value::Int(b));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!("Invalid value '{}' for 'batch' on '{}'", b, method)),
                    );
                    return Err(RuntimeError::typed("X::Invalid::Value", attrs));
                }
                if let Some(d) = degree
                    && d <= 0
                {
                    let mut attrs = std::collections::HashMap::new();
                    attrs.insert("method".to_string(), Value::str(method.clone()));
                    attrs.insert("name".to_string(), Value::str("degree".to_string()));
                    attrs.insert("value".to_string(), Value::Int(d));
                    attrs.insert(
                        "message".to_string(),
                        Value::str(format!(
                            "Invalid value '{}' for 'degree' on '{}'",
                            d, method
                        )),
                    );
                    return Err(RuntimeError::typed("X::Invalid::Value", attrs));
                }
                // Create HyperSeq/RaceSeq
                let items = crate::runtime::value_to_list(&target);
                let arc = std::sync::Arc::new(items);
                let result = if method == "hyper" {
                    Value::HyperSeq(arc)
                } else {
                    Value::RaceSeq(arc)
                };
                self.stack.push(result);
                self.env_dirty = true;
                return Ok(());
            }
            // HyperSeq/RaceSeq: delegate methods
            if matches!(&target, Value::HyperSeq(_) | Value::RaceSeq(_)) {
                let is_hyper = matches!(&target, Value::HyperSeq(_));
                let items_arc = match &target {
                    Value::HyperSeq(items) | Value::RaceSeq(items) => items.clone(),
                    _ => unreachable!(),
                };
                match method.as_str() {
                    "hyper" => {
                        self.stack.push(Value::HyperSeq(items_arc));
                        self.env_dirty = true;
                        return Ok(());
                    }
                    "race" => {
                        self.stack.push(Value::RaceSeq(items_arc));
                        self.env_dirty = true;
                        return Ok(());
                    }
                    "is-lazy" => {
                        self.stack.push(Value::Bool(false));
                        self.env_dirty = true;
                        return Ok(());
                    }
                    "^name" => {
                        self.stack.push(Value::str(
                            if is_hyper { "HyperSeq" } else { "RaceSeq" }.to_string(),
                        ));
                        self.env_dirty = true;
                        return Ok(());
                    }
                    "WHAT" => {
                        self.stack.push(Value::Package(Symbol::intern(if is_hyper {
                            "HyperSeq"
                        } else {
                            "RaceSeq"
                        })));
                        self.env_dirty = true;
                        return Ok(());
                    }
                    "defined" => {
                        self.stack.push(Value::Bool(true));
                        self.env_dirty = true;
                        return Ok(());
                    }
                    "map" | "grep" => {
                        let array_target = Value::Array(items_arc, crate::value::ArrayKind::List);
                        let call_result = if let Some(nr) =
                            self.try_native_method(&array_target, Symbol::intern(&method), &args)
                        {
                            nr
                        } else {
                            self.try_compiled_method_or_interpret(array_target, &method, args)
                        };
                        let result_val = call_result?;
                        let result_items = crate::runtime::value_to_list(&result_val);
                        let wrapped = if is_hyper {
                            Value::HyperSeq(Arc::new(result_items))
                        } else {
                            Value::RaceSeq(Arc::new(result_items))
                        };
                        self.stack.push(wrapped);
                        self.env_dirty = true;
                        return Ok(());
                    }
                    _ => {
                        // Convert to array and delegate
                        let array_target = Value::Array(items_arc, crate::value::ArrayKind::List);
                        let call_result = if let Some(nr) =
                            self.try_native_method(&array_target, Symbol::intern(&method), &args)
                        {
                            nr
                        } else {
                            self.try_compiled_method_or_interpret(array_target, &method, args)
                        };
                        self.stack.push(call_result?);
                        self.env_dirty = true;
                        return Ok(());
                    }
                }
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

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_call_method_mut_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        target_name_idx: u32,
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
        // Force lazy IO lines for non-lazy-preserving methods
        let target = if matches!(&target, Value::LazyIoLines { .. })
            && !matches!(method.as_str(), "kv" | "iterator" | "lazy")
        {
            self.force_if_lazy_io_lines(target)?
        } else {
            target
        };
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
                    | "WHICH"
                    | "^name"
                    | "gist"
                    | "Str"
                    | "defined"
                    | "THREAD"
                    | "raku"
                    | "perl"
                    | "say"
                    | "note"
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
            let result = match self.try_exec_simple_shared_protect_block(code, &code_val)? {
                Some(value) => Ok(value),
                None => self.exec_protect_block_inline(code, &code_val),
            };
            let _ = crate::runtime::native_methods::release_lock(&lock, me);
            self.stack.push(result?);
            self.env_dirty = true;
            return Ok(());
        }

        // Fast path for push/unshift on shared @-arrays.
        // Bypasses the full method dispatch chain (try_native_method →
        // call_method_mut_with_values → push_to_shared_var) for the common case
        // of pushing simple values to a shared array inside a tight loop
        // (e.g. Lock::Async.protect { push @target, $i }).
        if matches!(method.as_str(), "push" | "unshift")
            && !args.is_empty()
            && target_name.starts_with('@')
            && matches!(&target, Value::Array(..))
            && self.interpreter.shared_vars_active
        {
            let result = self
                .interpreter
                .push_to_existing_shared_array(&target_name, args.clone())
                .unwrap_or_else(|| {
                    self.interpreter
                        .push_to_shared_var(&target_name, args, &target)
                });
            self.stack.push(result);
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
        // .hyper/.race with named arguments in mut path
        if matches!(method.as_str(), "hyper" | "race") && !args.is_empty() {
            let mut batch: Option<i64> = None;
            let mut degree: Option<i64> = None;
            for arg in &args {
                let (key, val) = match arg {
                    Value::Pair(k, v) => (k.clone(), crate::runtime::to_int(v)),
                    Value::ValuePair(k, v) => (k.to_string_value(), crate::runtime::to_int(v)),
                    _ => continue,
                };
                match key.as_str() {
                    "batch" => batch = Some(val),
                    "degree" => degree = Some(val),
                    _ => {}
                }
            }
            if let Some(b) = batch
                && b <= 0
            {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "method".to_string(),
                    Value::str(method.as_str().to_string()),
                );
                attrs.insert("name".to_string(), Value::str("batch".to_string()));
                attrs.insert("value".to_string(), Value::Int(b));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Invalid value '{}' for 'batch' on '{}'",
                        b,
                        method.as_str()
                    )),
                );
                return Err(RuntimeError::typed("X::Invalid::Value", attrs));
            }
            if let Some(d) = degree
                && d <= 0
            {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert(
                    "method".to_string(),
                    Value::str(method.as_str().to_string()),
                );
                attrs.insert("name".to_string(), Value::str("degree".to_string()));
                attrs.insert("value".to_string(), Value::Int(d));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Invalid value '{}' for 'degree' on '{}'",
                        d,
                        method.as_str()
                    )),
                );
                return Err(RuntimeError::typed("X::Invalid::Value", attrs));
            }
            let items = crate::runtime::value_to_list(&target);
            let arc = std::sync::Arc::new(items);
            let result = if method == "hyper" {
                Value::HyperSeq(arc)
            } else {
                Value::RaceSeq(arc)
            };
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }
        // HyperSeq/RaceSeq delegation in mut path
        if matches!(&target, Value::HyperSeq(_) | Value::RaceSeq(_)) {
            let is_hyper = matches!(&target, Value::HyperSeq(_));
            match method.as_str() {
                "hyper" | "race" | "is-lazy" | "^name" | "WHAT" | "defined" => {
                    let items_arc = match &target {
                        Value::HyperSeq(items) | Value::RaceSeq(items) => items.clone(),
                        _ => unreachable!(),
                    };
                    let result = match method.as_str() {
                        "hyper" => Value::HyperSeq(items_arc),
                        "race" => Value::RaceSeq(items_arc),
                        "is-lazy" => Value::Bool(false),
                        "defined" => Value::Bool(true),
                        "^name" => {
                            let name = if is_hyper { "HyperSeq" } else { "RaceSeq" };
                            Value::str(name.to_string())
                        }
                        "WHAT" => {
                            let name = if is_hyper { "HyperSeq" } else { "RaceSeq" };
                            Value::Package(Symbol::intern(name))
                        }
                        _ => unreachable!(),
                    };
                    self.stack.push(result);
                    self.env_dirty = true;
                    return Ok(());
                }
                "map" | "grep" => {
                    // Delegate to array, then wrap result
                    let items_arc = match &target {
                        Value::HyperSeq(items) | Value::RaceSeq(items) => items.clone(),
                        _ => unreachable!(),
                    };
                    let array_target = Value::Array(items_arc, crate::value::ArrayKind::List);
                    let call_result = if let Some(native_result) =
                        self.try_native_method(&array_target, Symbol::intern(&method), &args)
                    {
                        native_result
                    } else {
                        self.try_compiled_method_mut_or_interpret(
                            &target_name,
                            array_target,
                            &method,
                            args,
                        )
                    };
                    let result_val = call_result?;
                    let result_items = crate::runtime::value_to_list(&result_val);
                    let wrapped = if is_hyper {
                        Value::HyperSeq(std::sync::Arc::new(result_items))
                    } else {
                        Value::RaceSeq(std::sync::Arc::new(result_items))
                    };
                    self.stack.push(wrapped);
                    self.env_dirty = true;
                    return Ok(());
                }
                _ => {
                    // For all other methods, convert to List and delegate
                }
            }
        }
        // Convert HyperSeq/RaceSeq to List for remaining method dispatch
        let target = match target {
            Value::HyperSeq(items) | Value::RaceSeq(items) => {
                Value::Array(items, crate::value::ArrayKind::List)
            }
            other => other,
        };
        // Auto-vivify undefined values (Nil, Any, Mu type objects) to empty Arrays
        // for mutating list methods. In Raku, calling push/unshift/append/prepend on
        // an undefined variable auto-vivifies it to an Array.
        let target = if matches!(method.as_str(), "push" | "unshift" | "append" | "prepend")
            && (matches!(&target, Value::Nil)
                || matches!(
                    &target,
                    Value::Package(name) if matches!(name.resolve().as_str(), "Any" | "Mu" | "Array")
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
                // NOTE: No Nil absorber here for CallMethodMut. Unlike CallMethod
                // (which handles direct Nil.method calls), CallMethodMut targets
                // are from variables. Uninitialized variables in mutsu are Nil
                // (should be Any), so absorbing here would break methods like
                // .end, .elems, etc. on uninitialized containers.
                // The CallMethod path has the Nil absorber for direct Nil.method calls.
                let call_result = if !skip_native {
                    // Resolve hash sentinel entries (bound variable refs, self-refs)
                    // before passing to native methods that iterate hash values.
                    let effective_target = if let Value::Hash(ref items) = target {
                        if Self::hash_has_sentinels(items) {
                            Some(self.resolve_hash_for_iteration(items))
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    let dispatch_target = effective_target.as_ref().unwrap_or(&target);
                    if let Some(native_result) =
                        self.try_native_method(dispatch_target, Symbol::intern(&method), &args)
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
}
