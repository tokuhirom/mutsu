use super::*;
use crate::symbol::Symbol;
use std::sync::Arc;

impl VM {
    pub(super) fn exec_call_method_dynamic_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        modifier_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_method_dispatch();
        self.flatten_scoped_env();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx));
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
        let method = Self::rewrite_method_name(&method_name_str, modifier);
        let target = if matches!(&target, Value::LazyIoLines { .. })
            && !matches!(method.as_str(), "kv" | "iterator" | "lazy")
        {
            self.force_if_lazy_io_lines(target)?
        } else {
            target
        };
        // Handle .* and .+ modifiers
        match modifier {
            Some("+") => {
                let vals = self.call_method_all_with_fallback(&target, &method, &args, false)?;
                self.stack.push(Value::array(vals));
                self.env_dirty = true;
                return Ok(());
            }
            Some("*") => {
                match self.call_method_all_with_fallback(&target, &method, &args, false) {
                    Ok(vals) => self.stack.push(Value::array(vals)),
                    Err(e) if Self::is_method_not_found_error(&e) => {
                        self.stack.push(Value::array(vec![]))
                    }
                    Err(e) => return Err(e),
                }
                self.env_dirty = true;
                return Ok(());
            }
            _ => {}
        }
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
        match modifier {
            Some("?") => match call_result {
                Ok(val) => self.stack.push(val),
                Err(e) if Self::is_method_not_found_error(&e) => self.stack.push(Value::Nil),
                Err(e) => return Err(e),
            },
            _ => {
                self.stack.push(call_result?);
            }
        }
        self.env_dirty = true;
        Ok(())
    }

    pub(super) fn exec_call_method_dynamic_mut_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_method_dispatch();
        self.flatten_scoped_env();
        let target_name = Self::const_str(code, target_name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx));
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
        let method_name_str = name_val.to_string_value();
        let method = Self::rewrite_method_name(&method_name_str, modifier);
        // Handle .* and .+ modifiers
        match modifier {
            Some("+") => {
                let vals = self.call_method_all_with_fallback(&target, &method, &args, false)?;
                self.stack.push(Value::array(vals));
                self.env_dirty = true;
                return Ok(());
            }
            Some("*") => {
                match self.call_method_all_with_fallback(&target, &method, &args, false) {
                    Ok(vals) => self.stack.push(Value::array(vals)),
                    Err(e) if Self::is_method_not_found_error(&e) => {
                        self.stack.push(Value::array(vec![]))
                    }
                    Err(e) => return Err(e),
                }
                self.env_dirty = true;
                return Ok(());
            }
            _ => {}
        }
        let call_result = if matches!(
            &name_val,
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. }
        ) {
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(target);
            call_args.extend(args);
            self.vm_call_on_value(name_val, call_args, None)?
        } else {
            // TODO: compile to bytecode — generic mut method fork (ledger §1).
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
        crate::vm::vm_stats::record_method_dispatch();
        // Set pending arg sources for `is rw` dispatch matching
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        self.interpreter
            .set_pending_call_arg_sources(arg_sources.clone());
        let method_raw = Self::const_str(code, name_idx);
        let target_name = Self::const_str(code, target_name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx));
        let method = Self::rewrite_method_name(method_raw, modifier);
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new("VM stack underflow in CallMethodMut"));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        let args = if raw_args.iter().any(|a| matches!(a, Value::Slip(_))) {
            let preserve_empty_slip = Self::preserve_empty_slip_arg(&method);
            let mut args = Vec::new();
            for arg in raw_args {
                Self::append_flattened_call_arg(&mut args, arg, preserve_empty_slip);
            }
            args
        } else {
            raw_args
        };
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
        // Lazy `.first` over a gather coroutine: pull incrementally instead of
        // forcing the (possibly infinite) list to completion.
        if let Value::LazyList(ref ll) = target
            && matches!(
                ll.env.get("__mutsu_lazylist_from_gather"),
                Some(crate::value::Value::Bool(true))
            )
            && method == "first"
            && let Some(result) = self.try_lazy_gather_first(ll, &args)
        {
            self.stack.push(result?);
            return Ok(());
        }
        let target = if let Value::LazyList(ref ll) = target
            && matches!(
                ll.env.get("__mutsu_lazylist_from_gather"),
                Some(crate::value::Value::Bool(true))
            )
            && Self::lazy_list_needs_forcing(&method)
            // A chained `.map`/`.grep` on a lazy pipeline appends another stage
            // (interpreter dispatch); laziness-preserving coercions return the
            // pipeline unchanged (native dispatch) — neither forces.
            && !(ll.lazy_pipe.is_some()
                && (matches!(method.as_str(), "map" | "grep")
                    || Self::lazy_pipe_preserving_coercion(&method)))
        {
            let saved_env = self.interpreter.env().clone();
            // `.head(n)` only needs the first `n` elements: pull them lazily so
            // an infinite gather does not hang.
            let items = match Self::gather_head_bound(&method, &args) {
                Some(n) => self.force_lazy_list_vm_n(ll, n)?,
                // A strict force of an infinite lazy pipeline cannot terminate:
                // raise X::Cannot::Lazy with this method's name.
                None if ll.lazy_pipe.is_some() => {
                    return Err(RuntimeError::cannot_lazy(&method));
                }
                None => self.force_lazy_list_vm(ll)?,
            };
            // A lazy map/grep pipeline runs its callback via `vm_call_on_value`
            // in this VM, so its side effects on enclosing variables are
            // legitimate and must persist (unlike gather coroutine corruption,
            // which the env restore undoes).
            if !matches!(method.as_str(), "elems" | "hyper" | "race") && ll.lazy_pipe.is_none() {
                *self.interpreter.env_mut() = saved_env;
            }
            Value::Seq(std::sync::Arc::new(items))
        } else {
            target
        };
        // Fast path: 0-arg attribute accessor read on an Instance (e.g.
        // `$obj.x`). A method call on a *variable* compiles to CallMethodMut for
        // potential invocant write-back, so accessor reads land here -- without
        // this they all fell back to the interpreter. The read does not mutate
        // the invocant, so no write-back to `target_name` is needed.
        if let Some(val) =
            self.try_fast_accessor_read(&target, &method, &args, modifier.is_some(), quoted)
        {
            // Pure attribute read: does not mutate the invocant (see comment
            // above), so it does not dirty the caller's locals (Slice 6.3).
            self.stack.push(val);
            return Ok(());
        }
        // `.so` / `.not` on a value whose type defines a user `Bool` method must
        // dispatch through that method (Mu.so / Mu.not are defined in terms of
        // .Bool) rather than the native truthiness fast path.
        if matches!(method.as_str(), "so" | "not") && args.is_empty() {
            let user_bool_owner = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = user_bool_owner
                && self
                    .interpreter
                    .resolve_method_with_owner(&cn, "Bool", &[])
                    .is_some()
            {
                let t = self.eval_truthy(&target);
                self.stack
                    .push(Value::Bool(if method == "not" { !t } else { t }));
                self.env_dirty = true;
                return Ok(());
            }
        }
        // Beyond the pure-read accessor fast path above, full method dispatch may
        // capture/iterate the env; collapse a transient scoped overlay env to a
        // flat env so the full lexical view is seen. Placed after the accessor
        // read so a `$.attr` read inside a scoped method body does not collapse
        // the overlay (defeating the per-method-call deep-copy elimination).
        self.flatten_scoped_env();
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
            let suggestions = self.interpreter.suggest_type_names(&target_name);
            return Err(RuntimeError::undeclared_type_symbols(
                &target_name,
                format!("Undeclared name:\n    {} used at line 1", target_name),
                suggestions,
            ));
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

        // .WHO on pseudo-package Package values: build the stash in the VM
        // where we have access to locals (which the interpreter doesn't have).
        if method == "WHO"
            && args.is_empty()
            && matches!(&target, Value::Package(name) if Self::is_pseudo_package_bare(&name.resolve()))
        {
            if let Value::Package(pkg_name) = &target {
                let stash = self.build_pseudo_stash(code, &pkg_name.resolve());
                self.stack.push(stash);
            }
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
                        class_name,
                        (attrs).to_map(),
                        id,
                        false,
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

        // Fast paths for xxKEY methods on Hash/Set/Bag/Mix types
        match method.as_str() {
            "AT-KEY" if args.len() == 1 => {
                let inner_target = match &target {
                    Value::Scalar(inner) => inner.as_ref(),
                    other => other,
                };
                if let Value::Hash(map) = inner_target {
                    let key = args[0].to_string_value();
                    let result = self.resolve_hash_entry(map, &key);
                    self.stack.push(result);
                    self.env_dirty = true;
                    return Ok(());
                }
            }
            "ASSIGN-KEY" if args.len() == 2 => {
                let key = args[0].to_string_value();
                let value = args[1].clone();
                let inner_target = match &target {
                    Value::Scalar(inner) => inner.as_ref(),
                    other => other,
                };
                match inner_target {
                    Value::Hash(_) => {
                        let old_meta = self
                            .interpreter
                            .container_type_metadata(inner_target)
                            .clone();
                        let mut hash = match inner_target {
                            Value::Hash(map) => (**map).clone(),
                            _ => std::collections::HashMap::new(),
                        };
                        hash.insert(key, value.clone());
                        let new_hash = Value::Hash(Arc::new(hash));
                        let meta = old_meta.unwrap_or(crate::runtime::ContainerTypeInfo {
                            value_type: "Any".to_string(),
                            key_type: None,
                            declared_type: None,
                        });
                        self.interpreter
                            .register_container_type_metadata(&new_hash, meta);
                        self.interpreter
                            .env_mut()
                            .insert(target_name.to_string(), new_hash);
                        self.stack.push(value);
                        self.env_dirty = true;
                        return Ok(());
                    }
                    Value::Set(_, false) => {
                        let repr = crate::runtime::gist_value(inner_target);
                        return Err(RuntimeError::assignment_ro_typename("Set", &repr));
                    }
                    Value::Set(data, true) => {
                        let mut new_set = data.elements.clone();
                        if value.truthy() {
                            new_set.insert(key);
                        } else {
                            new_set.remove(&key);
                        }
                        let new_val = Value::set_hash(new_set);
                        self.interpreter
                            .env_mut()
                            .insert(target_name.to_string(), new_val);
                        self.stack.push(value);
                        self.env_dirty = true;
                        return Ok(());
                    }
                    Value::Bag(_, false) => {
                        return Err(RuntimeError::assignment_ro_typename("Int", "1"));
                    }
                    Value::Bag(data, true) => {
                        let count = crate::runtime::to_int(&value);
                        let mut new_counts = data.counts.clone();
                        if count > 0 {
                            new_counts.insert(key, count);
                        } else {
                            new_counts.remove(&key);
                        }
                        let new_val = Value::bag_hash(new_counts);
                        self.interpreter
                            .env_mut()
                            .insert(target_name.to_string(), new_val);
                        self.stack.push(value);
                        self.env_dirty = true;
                        return Ok(());
                    }
                    Value::Mix(_, false) => {
                        return Err(RuntimeError::assignment_ro_typename("Int", "1"));
                    }
                    Value::Mix(data, true) => {
                        let weight = crate::runtime::to_float_value(&value).unwrap_or(0.0);
                        let mut new_weights = data.weights.clone();
                        if weight != 0.0 {
                            new_weights.insert(key, weight);
                        } else {
                            new_weights.remove(&key);
                        }
                        let new_val = Value::mix_hash(new_weights);
                        self.interpreter
                            .env_mut()
                            .insert(target_name.to_string(), new_val);
                        self.stack.push(value);
                        self.env_dirty = true;
                        return Ok(());
                    }
                    Value::Nil | Value::Package(_) => {
                        let mut hash = std::collections::HashMap::new();
                        hash.insert(key, value.clone());
                        self.interpreter
                            .env_mut()
                            .insert(target_name.to_string(), Value::Hash(Arc::new(hash)));
                        self.stack.push(value);
                        self.env_dirty = true;
                        return Ok(());
                    }
                    _ => {}
                }
            }
            "DELETE-KEY" if args.len() == 1 => {
                let key = args[0].to_string_value();
                let inner_target = match &target {
                    Value::Scalar(inner) => inner.as_ref(),
                    other => other,
                };
                match inner_target {
                    Value::Hash(map) => {
                        let old_meta = self
                            .interpreter
                            .container_type_metadata(inner_target)
                            .clone();
                        let old_value = if map.contains_key(&key) {
                            self.resolve_hash_entry(map, &key)
                        } else {
                            let type_name = old_meta
                                .as_ref()
                                .map(|info| info.value_type.clone())
                                .unwrap_or_else(|| "Any".to_string());
                            Value::Package(Symbol::intern(&type_name))
                        };
                        let mut new_map = (**map).clone();
                        new_map.remove(&key);
                        let new_hash = Value::Hash(Arc::new(new_map));
                        let meta = old_meta.unwrap_or(crate::runtime::ContainerTypeInfo {
                            value_type: "Any".to_string(),
                            key_type: None,
                            declared_type: None,
                        });
                        self.interpreter
                            .register_container_type_metadata(&new_hash, meta);
                        self.interpreter
                            .env_mut()
                            .insert(target_name.to_string(), new_hash);
                        self.stack.push(old_value);
                        self.env_dirty = true;
                        return Ok(());
                    }
                    Value::Set(_, false) => {
                        return Err(RuntimeError::immutable("Set", "DELETE-KEY"));
                    }
                    Value::Set(data, true) => {
                        let existed = data.elements.contains(&key);
                        let mut new_set = data.elements.clone();
                        new_set.remove(&key);
                        let new_val = Value::set_hash(new_set);
                        self.interpreter
                            .env_mut()
                            .insert(target_name.to_string(), new_val);
                        self.stack.push(Value::Bool(existed));
                        self.env_dirty = true;
                        return Ok(());
                    }
                    Value::Bag(_, false) => {
                        return Err(RuntimeError::immutable("Bag", "DELETE-KEY"));
                    }
                    Value::Bag(data, true) => {
                        let old_count = data.counts.get(&key).copied().unwrap_or(0);
                        let mut new_counts = data.counts.clone();
                        new_counts.remove(&key);
                        let new_val = Value::bag_hash(new_counts);
                        self.interpreter
                            .env_mut()
                            .insert(target_name.to_string(), new_val);
                        self.stack.push(Value::Int(old_count));
                        self.env_dirty = true;
                        return Ok(());
                    }
                    Value::Mix(_, false) => {
                        return Err(RuntimeError::immutable("Mix", "DELETE-KEY"));
                    }
                    Value::Mix(data, true) => {
                        let old_weight = data.weights.get(&key).copied().unwrap_or(0.0);
                        let mut new_weights = data.weights.clone();
                        new_weights.remove(&key);
                        let new_val = Value::mix_hash(new_weights);
                        self.interpreter
                            .env_mut()
                            .insert(target_name.to_string(), new_val);
                        let result = crate::value::mix_weight_to_value(old_weight);
                        self.stack.push(result);
                        self.env_dirty = true;
                        return Ok(());
                    }
                    Value::Nil | Value::Package(_) => {
                        self.stack.push(Value::Nil);
                        self.env_dirty = true;
                        return Ok(());
                    }
                    _ => {}
                }
            }
            "BIND-KEY" if args.len() == 2 => {
                let inner_target = match &target {
                    Value::Scalar(inner) => inner.as_ref(),
                    other => other,
                };
                match inner_target {
                    Value::Hash(map) => {
                        let old_meta = self
                            .interpreter
                            .container_type_metadata(inner_target)
                            .clone();
                        let key = args[0].to_string_value();
                        let value = args[1].clone();
                        let source_var = arg_sources
                            .as_ref()
                            .and_then(|s| s.get(1))
                            .and_then(|s| s.clone());
                        let mut new_map = (**map).clone();
                        if let Some(var_name) = source_var {
                            new_map.insert(
                                key,
                                Value::Pair(
                                    super::vm_var_ops::BOUND_HASH_REF_SENTINEL.to_string(),
                                    Box::new(Value::str(var_name)),
                                ),
                            );
                        } else {
                            new_map.insert(key, value.clone());
                        }
                        let new_hash = Value::Hash(Arc::new(new_map));
                        let meta = old_meta.unwrap_or(crate::runtime::ContainerTypeInfo {
                            value_type: "Any".to_string(),
                            key_type: None,
                            declared_type: None,
                        });
                        self.interpreter
                            .register_container_type_metadata(&new_hash, meta);
                        self.interpreter
                            .env_mut()
                            .insert(target_name.to_string(), new_hash);
                        self.stack.push(value);
                        self.env_dirty = true;
                        return Ok(());
                    }
                    Value::Nil | Value::Package(_) => {
                        let key = args[0].to_string_value();
                        let value = args[1].clone();
                        let source_var = arg_sources
                            .as_ref()
                            .and_then(|s| s.get(1))
                            .and_then(|s| s.clone());
                        let mut new_map = std::collections::HashMap::new();
                        if let Some(var_name) = source_var {
                            new_map.insert(
                                key,
                                Value::Pair(
                                    super::vm_var_ops::BOUND_HASH_REF_SENTINEL.to_string(),
                                    Box::new(Value::str(var_name)),
                                ),
                            );
                        } else {
                            new_map.insert(key, value.clone());
                        }
                        self.interpreter
                            .env_mut()
                            .insert(target_name.to_string(), Value::Hash(Arc::new(new_map)));
                        self.stack.push(value);
                        self.env_dirty = true;
                        return Ok(());
                    }
                    Value::Set(_, mutable) => {
                        let name = if *mutable { "SetHash" } else { "Set" };
                        return Err(RuntimeError::bind(name));
                    }
                    Value::Bag(_, mutable) => {
                        let name = if *mutable { "BagHash" } else { "Bag" };
                        return Err(RuntimeError::bind(name));
                    }
                    Value::Mix(_, mutable) => {
                        let name = if *mutable { "MixHash" } else { "Mix" };
                        return Err(RuntimeError::bind(name));
                    }
                    _ => {}
                }
            }
            _ => {}
        }

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
        match modifier {
            Some("+") => {
                let vals =
                    self.call_method_all_with_fallback(&target, &method, &args, skip_native)?;
                self.stack.push(Value::array(vals));
                self.env_dirty = true;
            }
            Some("*") => {
                match self.call_method_all_with_fallback(&target, &method, &args, skip_native) {
                    Ok(vals) => self.stack.push(Value::array(vals)),
                    Err(e) if Self::is_method_not_found_error(&e) => {
                        self.stack.push(Value::array(vec![]))
                    }
                    Err(e) => return Err(e),
                }
                self.env_dirty = true;
            }
            _ => {
                // Native fast path for mutating list methods on a plain, untyped
                // `@`-array (ledger §1: native receiver dispatch -> VM-native).
                // Handles the common hot-loop case directly in the VM, writing the
                // mutated array back to env, instead of routing through the
                // tree-walking interpreter bridge. Falls through (returns None) for
                // typed/shaped/lazy/shared/constrained arrays so the interpreter
                // keeps owning those richer semantics.
                if modifier.is_none()
                    && let Some(result) =
                        self.try_native_array_mut(&target_name, &target, &method, &args)
                {
                    self.stack.push(result?);
                    self.env_dirty = true;
                    return Ok(());
                }
                // Native fast path for the simple (non-erroring) forms of `splice`
                // on a plain, untyped `@`-array (ledger §1: native receiver
                // dispatch -> VM-native).
                if modifier.is_none()
                    && let Some(result) =
                        self.try_native_array_splice(&target_name, &target, &method, &args)
                {
                    self.stack.push(result?);
                    self.env_dirty = true;
                    return Ok(());
                }
                // Native fast path for mutating Buf write methods on a mutable Buf
                // instance (ledger §1: native receiver dispatch -> VM-native).
                if modifier.is_none()
                    && let Some(result) =
                        self.try_native_buf_mut(&target_name, &target, &method, &args)
                {
                    self.stack.push(result?);
                    self.env_dirty = true;
                    return Ok(());
                }
                // Native fast path for the Iterator protocol on a self-contained
                // array-backed iterator (ledger §1: native receiver dispatch ->
                // VM-native). `$it.pull-one` etc. compile to CallMethodMut, so the
                // index-advancing dispatch lands here.
                if modifier.is_none()
                    && let Some(result) = self.try_native_iterator(&target, &method, &args)
                {
                    self.stack.push(result?);
                    self.env_dirty = true;
                    return Ok(());
                }
                // Array-subclass instance delegation (mut path): when the Instance's
                // class inherits from Array, delegate mutating Array methods to the
                // backing __mutsu_array_storage attribute and write back.
                if let Value::Instance {
                    class_name: ref inst_class,
                    ref attributes,
                    id: inst_id,
                } = target
                {
                    let cn = inst_class.resolve();
                    let is_array_method = matches!(
                        method.as_str(),
                        "push"
                            | "pop"
                            | "shift"
                            | "unshift"
                            | "append"
                            | "prepend"
                            | "splice"
                            | "join"
                            | "elems"
                            | "end"
                            | "List"
                            | "Array"
                            | "Seq"
                            | "Slip"
                            | "sort"
                            | "reverse"
                            | "rotate"
                            | "unique"
                            | "squish"
                            | "flat"
                            | "map"
                            | "grep"
                            | "first"
                            | "head"
                            | "tail"
                            | "AT-POS"
                            | "ASSIGN-POS"
                            | "EXISTS-POS"
                            | "DELETE-POS"
                            | "BIND-POS"
                    );
                    if is_array_method
                        && !self.interpreter.has_user_method(&cn, &method)
                        && attributes.contains_key("__mutsu_array_storage")
                        && self
                            .interpreter
                            .mro_readonly(&cn)
                            .iter()
                            .any(|n| n == "Array")
                    {
                        let mut storage = attributes
                            .get("__mutsu_array_storage")
                            .cloned()
                            .unwrap_or(Value::real_array(Vec::new()));
                        // Perform the operation on the backing array
                        // TODO: compile to bytecode — Array-backed instance method
                        // (push/pop/shift on `is Array` storage). See ledger §1.
                        crate::vm::vm_stats::record_method_fallback(&method);
                        let result = self
                            .interpreter
                            .call_method_mut_with_values(
                                "__mutsu_array_tmp",
                                storage.clone(),
                                &method,
                                args,
                            )
                            .or_else(|_| {
                                // Try non-mut dispatch for read-only methods
                                self.interpreter.call_method_with_values(
                                    storage.clone(),
                                    &method,
                                    vec![],
                                )
                            })?;
                        // Read back the (potentially mutated) storage
                        if let Some(updated_storage) =
                            self.interpreter.env().get("__mutsu_array_tmp").cloned()
                        {
                            storage = updated_storage;
                        }
                        self.interpreter.env_mut().remove("__mutsu_array_tmp");
                        // Update the instance with the new storage
                        let mut new_attrs = crate::value::InstanceAttrs::clone(attributes);
                        new_attrs.insert("__mutsu_array_storage".to_string(), storage);
                        let updated_instance = Value::Instance {
                            class_name: *inst_class,
                            attributes: Arc::new(crate::value::InstanceAttrs::new(
                                *inst_class,
                                (new_attrs).to_map(),
                                inst_id,
                                true,
                            )),
                            id: inst_id,
                        };
                        self.interpreter
                            .env_mut()
                            .insert(target_name.to_string(), updated_instance);
                        self.env_dirty = true;
                        self.stack.push(result);
                        self.env_dirty = true;
                        return Ok(());
                    }
                }
                // NOTE: No Nil absorber here for CallMethodMut. Unlike CallMethod
                // (which handles direct Nil.method calls), CallMethodMut targets
                // are from variables. Uninitialized variables in mutsu are Nil
                // (should be Any), so absorbing here would break methods like
                // .end, .elems, etc. on uninitialized containers.
                // The CallMethod path has the Nil absorber for direct Nil.method calls.
                // Slice 6.3: assume the dispatch dirties the caller env; only a
                // proven-pure compiled method path clears this.
                self.method_dispatch_pure = false;
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
                        // A native method reaching this tail returns a value and
                        // does not write the receiver back into env (mutating
                        // array/hash natives are handled by the dedicated
                        // writeback branches above and return early). So it is
                        // env-pure w.r.t. the caller -> no per-call locals pull.
                        self.method_dispatch_pure = true;
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
                // Slice 6.3: mark env dirty only when the dispatch was not a
                // proven-pure compiled method call.
                let mark_dirty = !self.method_dispatch_pure;
                match modifier {
                    Some("?") => match call_result {
                        Ok(val) => {
                            self.stack.push(val);
                            if mark_dirty {
                                self.env_dirty = true;
                            }
                        }
                        Err(e) if Self::is_method_not_found_error(&e) => {
                            self.stack.push(Value::Nil);
                            if mark_dirty {
                                self.env_dirty = true;
                            }
                        }
                        Err(e) => return Err(e),
                    },
                    _ => {
                        self.stack.push(call_result?);
                        if mark_dirty {
                            self.env_dirty = true;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// VM-native mutating list methods (`append`/`prepend`/`unshift`/`pop`/`shift`)
    /// on a plain, untyped `@`-array stored in env. Mirrors the interpreter's
    /// primary (`env.get_mut` + `Arc::make_mut`) branch in `methods_mut.rs`
    /// exactly for this narrow case, so the result is behavior-invariant.
    ///
    /// Returns:
    /// - `Some(Ok(v))` — handled natively (env already mutated); `v` is the
    ///   method's return value (the array for append/prepend/unshift, the removed
    ///   element for pop/shift).
    /// - `Some(Err(_))` — handled natively but errored.
    /// - `None` — not eligible; the caller must fall through to the interpreter.
    ///
    /// Intentionally conservative: bails out (returns `None`) for typed/shaped/
    /// lazy arrays, type-constrained or metadata-bearing containers, shared
    /// arrays, and any receiver that is not the exact array currently bound to
    /// `target_name` in env. Those richer semantics stay owned by the interpreter.
    fn try_native_array_mut(
        &mut self,
        target_name: &str,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "append" | "prepend" | "unshift" | "pop" | "shift") {
            return None;
        }
        // Only a plain `@`-sigiled variable whose value is a real `[...]` array
        // (ArrayKind::Array). This excludes List/Item/Shaped/Lazy kinds.
        if !target_name.starts_with('@')
            || !matches!(target, Value::Array(_, crate::value::ArrayKind::Array))
        {
            return None;
        }
        // Shared arrays keep their interior-mutation (Arc>1) semantics in the
        // interpreter so bound aliases observe the change; type-constrained or
        // metadata-bearing containers need element checks / typed empty Failures.
        if self.interpreter.shared_vars_active
            || self.interpreter.var_type_constraint(target_name).is_some()
            || self.interpreter.container_type_metadata(target).is_some()
        {
            return None;
        }
        // pop/shift take no positionals; let the interpreter raise the arity error.
        if matches!(method, "pop" | "shift") && !args.is_empty() {
            return None;
        }
        // The receiver must be exactly the array currently bound to this name, so
        // an in-place `Arc::make_mut` writeback is correct.
        let Some(Value::Array(arc_items, crate::value::ArrayKind::Array)) =
            self.interpreter.env_mut().get_mut(target_name)
        else {
            return None;
        };
        let result = match method {
            "append" | "prepend" => {
                let flat = crate::runtime::flatten_append_args(args.to_vec());
                let items = Arc::make_mut(arc_items);
                if method == "append" {
                    items.extend(flat);
                } else {
                    for (i, v) in flat.into_iter().enumerate() {
                        items.insert(i, v);
                    }
                }
                Value::Array(Arc::clone(arc_items), crate::value::ArrayKind::Array)
            }
            "unshift" => {
                let norm = crate::runtime::Interpreter::normalize_push_unshift_args(args.to_vec());
                let items = Arc::make_mut(arc_items);
                for (i, v) in norm.into_iter().enumerate() {
                    items.insert(i, v);
                }
                Value::Array(Arc::clone(arc_items), crate::value::ArrayKind::Array)
            }
            "pop" => {
                if arc_items.is_empty() {
                    crate::runtime::utils::make_empty_array_failure_what("pop", "Array")
                } else {
                    Arc::make_mut(arc_items).pop().unwrap_or(Value::Nil)
                }
            }
            "shift" => {
                if arc_items.is_empty() {
                    crate::runtime::utils::make_empty_array_failure_what("shift", "Array")
                } else {
                    Arc::make_mut(arc_items).remove(0)
                }
            }
            _ => unreachable!(),
        };
        // `Arc::make_mut` may reallocate the backing buffer, giving the array a
        // fresh heap pointer. Container type metadata is keyed by that pointer
        // (`array_type_metadata`), and the map is not cleaned when an Arc is
        // freed, so the new pointer can collide with a stale entry left by a
        // freed typed array and spuriously type this (guaranteed-untyped) array.
        // Defensively drop any such aliased entry for the post-mutation array.
        if let Some(stored @ Value::Array(..)) = self.interpreter.env().get(target_name).cloned() {
            self.interpreter.unregister_container_type_metadata(&stored);
        }
        Some(Ok(result))
    }

    /// VM-native `splice` on a plain, untyped `@`-array bound to `target_name`
    /// (ledger §1: native receiver dispatch -> VM-native). Mirrors the
    /// interpreter's `splice` branch in `methods_mut.rs` exactly (`drain` +
    /// `insert`, returning the removed elements as a real array), so the result
    /// is behavior-invariant.
    ///
    /// Conservatively handles only the simple, non-erroring forms: the offset
    /// and count arguments must be plain non-negative `Int`s (or absent) and any
    /// replacement values must be non-lazy. Returns `None` (fall through to the
    /// interpreter) for every richer case the interpreter owns: a
    /// WhateverCode/`Whatever`/`Str`/`Num` offset or count, an out-of-range
    /// offset (`X::OutOfRange`), a lazy replacement (`X::Cannot::Lazy`), and
    /// typed/shaped/shared/metadata-bearing arrays.
    fn try_native_array_splice(
        &mut self,
        target_name: &str,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if method != "splice" {
            return None;
        }
        // Only a plain `@`-sigiled variable whose value is a real `[...]` array
        // (ArrayKind::Array). This excludes List/Item/Shaped/Lazy kinds.
        if !target_name.starts_with('@')
            || !matches!(target, Value::Array(_, crate::value::ArrayKind::Array))
        {
            return None;
        }
        // Shared / type-constrained / metadata-bearing containers need the
        // interpreter's element checks, native-array semantics, and identity
        // sharing; let it own those.
        if self.interpreter.shared_vars_active
            || self.interpreter.var_type_constraint(target_name).is_some()
            || self.interpreter.container_type_metadata(target).is_some()
        {
            return None;
        }
        // Offset (arg 0) and count (arg 1): plain non-negative `Int`, or absent.
        // Anything else (Whatever/Str/Num/Callable) goes to the interpreter,
        // which also owns the `X::OutOfRange` error for a negative offset/count.
        let raw_start = match args.first() {
            None => None,
            Some(Value::Int(i)) if *i >= 0 => Some(*i as usize),
            _ => return None,
        };
        let raw_count = match args.get(1) {
            None => None,
            Some(Value::Int(i)) if *i >= 0 => Some(*i as usize),
            _ => return None,
        };
        // Replacement values (args[2..]): reject lazy values (the interpreter
        // raises `X::Cannot::Lazy`); flatten `Array` args exactly as the
        // interpreter's `do_splice` does.
        let mut replacement: Vec<Value> = Vec::new();
        for arg in args.iter().skip(2) {
            match arg {
                Value::Array(arr, ..) => {
                    if arr.iter().any(crate::builtins::methods_0arg::is_value_lazy) {
                        return None;
                    }
                    replacement.extend(arr.iter().cloned());
                }
                other => {
                    if crate::builtins::methods_0arg::is_value_lazy(other) {
                        return None;
                    }
                    replacement.push(other.clone());
                }
            }
        }
        // The receiver must be exactly the array currently bound to this name, so
        // an in-place `Arc::make_mut` writeback is correct. Compute the splice
        // bounds from the live binding's length (not `target`).
        let Some(Value::Array(arc_items, crate::value::ArrayKind::Array)) =
            self.interpreter.env_mut().get_mut(target_name)
        else {
            return None;
        };
        let len = arc_items.len();
        let start = raw_start.unwrap_or(0);
        // An offset past the end is `X::OutOfRange` in the interpreter.
        if start > len {
            return None;
        }
        let count = raw_count.unwrap_or(len - start);
        let end = (start + count).min(len);
        let items = Arc::make_mut(arc_items);
        let removed: Vec<Value> = items.drain(start..end).collect();
        for (i, item) in replacement.into_iter().enumerate() {
            items.insert(start + i, item);
        }
        // `Arc::make_mut` may reallocate the backing buffer; drop any stale
        // pointer-keyed type metadata that could alias the fresh pointer (same
        // hazard handled in `try_native_array_mut`).
        if let Some(stored @ Value::Array(..)) = self.interpreter.env().get(target_name).cloned() {
            self.interpreter.unregister_container_type_metadata(&stored);
        }
        Some(Ok(Value::real_array(removed)))
    }

    /// VM-native mutating Buf write methods (`write-bits`/`write-ubits`/
    /// `write-num*`/`write-int*`/`write-uint*`) on a mutable `Buf` instance bound
    /// to `target_name` (ledger §1: native receiver dispatch -> VM-native). Mirrors
    /// the interpreter's instance-mutate branches in `methods_mut.rs` exactly: the
    /// byte transforms are the single shared pure implementations in `builtins/`
    /// (`buf_bits`/`buf_write_num`/`buf_write_int`), and the writeback uses the
    /// same identity-based `overwrite_instance_bindings_by_identity` so aliases of
    /// the same buf observe the mutation — so the result is behavior-invariant.
    ///
    /// Returns `None` (fall through to the interpreter) for type-object receivers
    /// (`buf8.write-...` on the type returns a fresh buf), immutable `Blob`, and
    /// malformed arity/arguments, leaving the interpreter to own those
    /// error/construction semantics.
    fn try_native_buf_mut(
        &mut self,
        target_name: &str,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let is_write_bits = matches!(method, "write-ubits" | "write-bits");
        let is_write_num = crate::builtins::buf_write_num::write_num_size(method).is_some();
        let is_write_int = crate::builtins::buf_write_int::write_int_info(method).is_some();
        if !(is_write_bits || is_write_num || is_write_int) {
            return None;
        }
        let Value::Instance {
            class_name,
            attributes,
            id,
        } = target
        else {
            return None;
        };
        let cn = class_name.resolve();
        if !crate::runtime::utils::is_buf_or_blob_class(&cn) {
            return None;
        }
        // Immutable Blob: let the interpreter raise "Cannot modify immutable Blob".
        if cn == "Blob" || cn.starts_with("Blob[") || cn.starts_with("blob") {
            return None;
        }
        // Extract the current bytes (same clamping the interpreter uses).
        let mut bytes: Vec<u8> = Vec::new();
        if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
            bytes.reserve(items.len());
            for it in items.iter() {
                bytes.push(match it {
                    Value::Int(i) => (*i).clamp(0, 255) as u8,
                    Value::Num(f) => (*f as i64).clamp(0, 255) as u8,
                    Value::BigInt(bi) => num_traits::ToPrimitive::to_i64(bi.as_ref())
                        .unwrap_or(0)
                        .clamp(0, 255) as u8,
                    _ => 0,
                });
            }
        }
        // Compute the new bytes via the shared pure transform.
        let new_bytes: Vec<u8> = if is_write_bits {
            if args.len() != 3 {
                return None; // interpreter handles non-3-arg forms
            }
            let (Some(from), Some(bits)) = (
                crate::runtime::Interpreter::value_to_non_negative_i64(&args[0]),
                crate::runtime::Interpreter::value_to_non_negative_i64(&args[1]),
            ) else {
                return None; // let the interpreter raise the offset/bits parse error
            };
            match crate::builtins::buf_bits::write_bits(&bytes, from, bits, &args[2]) {
                Ok(b) => b,
                Err(e) => return Some(Err(e)),
            }
        } else {
            // write-num* / write-int*: 2 or 3 args (interpreter raises on others).
            if args.len() < 2 || args.len() > 3 {
                return None;
            }
            let offset_i64 = match &args[0] {
                Value::Int(i) => *i,
                Value::Num(f) => *f as i64,
                _ => 0,
            };
            let endian_val = if args.len() == 3 {
                crate::builtins::buf_write_num::decode_endian(&args[2])
            } else {
                0
            };
            let res = if is_write_num {
                crate::builtins::buf_write_num::apply_write_num(
                    &mut bytes, method, offset_i64, &args[1], endian_val,
                )
            } else {
                crate::builtins::buf_write_int::apply_write_int(
                    &mut bytes, method, offset_i64, &args[1], endian_val,
                )
            };
            if let Err(e) = res {
                return Some(Err(e));
            }
            bytes
        };
        // Build updated attributes and write back by instance identity (so aliases
        // observing the same buf see the mutation), then refresh the receiver
        // binding to match the interpreter's `env.insert(target_var, ...)`.
        let mut updated_attrs = attributes.as_ref().clone();
        updated_attrs.insert(
            "bytes".to_string(),
            Value::array(
                new_bytes
                    .into_iter()
                    .map(|b| Value::Int(b as i64))
                    .collect(),
            ),
        );
        self.interpreter.overwrite_instance_bindings_by_identity(
            &cn,
            *id,
            (updated_attrs.clone()).to_map(),
        );
        let updated = Value::make_instance_with_id(*class_name, (updated_attrs).to_map(), *id);
        self.interpreter
            .env_mut()
            .insert(target_name.to_string(), updated.clone());
        Some(Ok(updated))
    }
}
