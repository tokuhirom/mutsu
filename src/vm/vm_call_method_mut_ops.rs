use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// The invocant of a `.&sub(...)` call is always bound positionally to the
    /// sub's first parameter, even when it is a literal colonpair (`:42foo.&f`).
    /// A bare `Pair` in an argument list is otherwise splatted into a
    /// named argument, so containerize a Pair invocant into a positional
    /// `ValuePair` (the same conversion `OpCode::ContainerizePair` performs).
    fn invocant_as_positional(target: Value) -> Value {
        match target.view() {
            ValueView::Pair(k, v) => Value::value_pair(Value::str(k.clone()), v.clone()),
            _ => target,
        }
    }

    /// Derive the method name for an indirect call `$obj.$name`. A **type
    /// object** used as the name specifier (`$string.$type` with `$type = Int`)
    /// dispatches the method named by its short name (`.Int`), so use that name
    /// rather than the type object's gist (`(Int)`). Any other value falls back
    /// to its string form (mutsu treats a plain `Str` as a method name).
    fn dynamic_method_name(name_val: &Value) -> String {
        match name_val.view() {
            ValueView::Package(name) => name.resolve(),
            _ => name_val.to_string_value(),
        }
    }

    pub(super) fn exec_call_method_dynamic_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_method_dispatch();
        self.flatten_scoped_env();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx));
        let arity = arity as usize;
        if self.stack.len() < arity + 2 {
            return Err(RuntimeError::new(
                "Interpreter stack underflow in CallMethodDynamic",
            ));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // ADR-0054 S3: spread only the `|EXPR` positions -- this opcode has
        // never tracked rw-arg sources, so the decoded name list is
        // discarded (it exists solely to keep the slip-position decoder
        // in the shared helper).
        let (args, _arg_sources) =
            Self::spread_call_args_by_syntax(code, raw_args, arg_sources_idx, None);
        let name_val = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("Interpreter stack underflow in CallMethodDynamic name")
        })?;
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("Interpreter stack underflow in CallMethodDynamic target")
        })?;
        if !quoted && !matches!(name_val.view(), ValueView::Package(_)) {
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(Self::invocant_as_positional(target));
            call_args.extend(args);
            let result = self.vm_call_on_value(name_val, call_args, None);
            match modifier {
                Some("+") | Some("*") => self.stack.push(Value::array(vec![result?])),
                _ => self.stack.push(result?),
            }
            return Ok(());
        }
        // Reify/consume a deferred Seq (ADR-0034 §2.3) for non-lazy-preserving
        // methods before dispatch.
        let method_name_str = Self::dynamic_method_name(&name_val);
        let method = Self::rewrite_method_name(&method_name_str, modifier);
        let target = self.reify_or_consume_seq_target(target, &method)?;
        // Handle .* and .+ modifiers
        match modifier {
            Some("+") => {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethoddynamic",
                    "modifier-plus",
                );
                let vals = self.call_method_all_with_fallback(&target, &method, &args, false)?;
                self.stack.push(Value::array(vals));
                return Ok(());
            }
            Some("*") => {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethoddynamic",
                    "modifier-star",
                );
                match self.call_method_all_with_fallback(&target, &method, &args, false) {
                    Ok(vals) => self.stack.push(Value::array(vals)),
                    Err(e) if Self::is_method_not_found_error(&e) => {
                        self.stack.push(Value::array(vec![]))
                    }
                    Err(e) => return Err(e),
                }
                return Ok(());
            }
            _ => {}
        }
        let call_result = if matches!(
            name_val.view(),
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
        ) {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethoddynamic",
                "call-sub-value",
            );
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(Self::invocant_as_positional(target));
            call_args.extend(args);
            self.vm_call_on_value(name_val, call_args, None)
        } else {
            let method = Self::dynamic_method_name(&name_val);
            // .return method: triggers a return from the enclosing sub
            if method == "return" && args.is_empty() {
                crate::vm::vm_stats::record_dispatch_entry_intercept("callmethoddynamic", "return");
                let mut err = RuntimeError::new("return");
                err.return_value = Some(target);
                return Err(err);
            }
            // .hyper/.race with named arguments: validate, then create HyperSeq/RaceSeq
            if matches!(method.as_str(), "hyper" | "race") {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethoddynamic",
                    "hyper-race-config",
                );
                // Extract batch/degree for validation
                let mut batch: Option<i64> = None;
                let mut degree: Option<i64> = None;
                for arg in &args {
                    let (key, val) = match arg.view() {
                        ValueView::Pair(k, v) => (k.clone(), crate::runtime::to_int(v)),
                        ValueView::ValuePair(k, v) => {
                            (k.to_string_value(), crate::runtime::to_int(v))
                        }
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
                    attrs.insert("value".to_string(), Value::int(b));
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
                    attrs.insert("value".to_string(), Value::int(d));
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
                let body = crate::value::SeqBody::reified(items);
                // Remember the requested batch/degree so `.configuration` can
                // report them (the HyperSeq/RaceSeq does not carry the config).
                body.set_hyper_config(batch, degree);
                let result = if method == "hyper" {
                    Value::hyper_seq_body(body)
                } else {
                    Value::race_seq_body(body)
                };
                self.stack.push(result);
                return Ok(());
            }
            // HyperSeq/RaceSeq: delegate methods
            if matches!(
                target.view(),
                ValueView::HyperSeq(_) | ValueView::RaceSeq(_)
            ) {
                let is_hyper = matches!(target.view(), ValueView::HyperSeq(_));
                let items_arc = match target.view() {
                    ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => items.clone(),
                    _ => unreachable!(),
                };
                match method.as_str() {
                    "hyper" => {
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethoddynamic",
                            "hyperseq-hyper",
                        );
                        self.stack.push(Value::hyper_seq_body(items_arc));
                        return Ok(());
                    }
                    "race" => {
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethoddynamic",
                            "hyperseq-race",
                        );
                        self.stack.push(Value::race_seq_body(items_arc));
                        return Ok(());
                    }
                    "is-lazy" => {
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethoddynamic",
                            "hyperseq-is-lazy",
                        );
                        self.stack.push(Value::FALSE);
                        return Ok(());
                    }
                    "configuration" if args.is_empty() => {
                        // `HyperSeq.configuration` — expose the `.batch`/`.degree`
                        // the sequence was hyperized with (defaults otherwise).
                        // Used by the `hyperize` dist.
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethoddynamic",
                            "hyperseq-configuration",
                        );
                        let (batch, degree) = items_arc.hyper_config().unwrap_or((None, None));
                        self.stack
                            .push(Interpreter::make_hyper_configuration(batch, degree));
                        return Ok(());
                    }
                    "^name" => {
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethoddynamic",
                            "hyperseq-name",
                        );
                        self.stack.push(Value::str(
                            if is_hyper { "HyperSeq" } else { "RaceSeq" }.to_string(),
                        ));
                        return Ok(());
                    }
                    "WHAT" => {
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethoddynamic",
                            "hyperseq-what",
                        );
                        self.stack.push(Value::package(Symbol::intern(if is_hyper {
                            "HyperSeq"
                        } else {
                            "RaceSeq"
                        })));
                        return Ok(());
                    }
                    "defined" => {
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethoddynamic",
                            "hyperseq-defined",
                        );
                        self.stack.push(Value::TRUE);
                        return Ok(());
                    }
                    "map" | "grep" => {
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethoddynamic",
                            "hyperseq-map-grep",
                        );
                        let array_target = Value::array_with_kind(
                            crate::value::Value::array_arc(items_arc.to_vec()),
                            crate::value::ArrayKind::List,
                        );
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
                            Value::hyper_seq(result_items)
                        } else {
                            Value::race_seq(result_items)
                        };
                        self.stack.push(wrapped);
                        return Ok(());
                    }
                    _ => {
                        // Convert to array and delegate
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethoddynamic",
                            "hyperseq-delegate",
                        );
                        let array_target = Value::array_with_kind(
                            crate::value::Value::array_arc(items_arc.to_vec()),
                            crate::value::ArrayKind::List,
                        );
                        let call_result = if let Some(nr) =
                            self.try_native_method(&array_target, Symbol::intern(&method), &args)
                        {
                            nr
                        } else {
                            self.try_compiled_method_or_interpret(array_target, &method, args)
                        };
                        self.stack.push(call_result?);
                        return Ok(());
                    }
                }
            }
            // An `is Array`/`is List` subclass instance answers through its
            // backing storage, so the native probe must not answer FOR the
            // instance first (`.elems` on the Instance is 1, not its element
            // count). The `CallMethod` opcode takes its delegation before its
            // own native probe for the same reason; falling through here reaches
            // the shared one in `call_method_with_values`.
            if !self.delegates_to_array_storage(&target, &method)
                && let Some(native_result) =
                    self.try_native_method(&target, Symbol::intern(&method), &args)
            {
                crate::vm::vm_stats::record_dispatch_entry_outcome("callmethoddynamic", "native");
                native_result
            } else {
                crate::vm::vm_stats::record_dispatch_entry_outcome("callmethoddynamic", "user");
                self.try_compiled_method_or_interpret(target, &method, args)
            }
        };
        match modifier {
            Some("?") => match call_result {
                Ok(val) => self.stack.push(val),
                Err(e) if Self::is_method_not_found_error(&e) => {
                    crate::vm::vm_stats::record_dispatch_entry_outcome(
                        "callmethoddynamic",
                        "notfound",
                    );
                    self.stack.push(Value::NIL)
                }
                Err(e) => return Err(e),
            },
            _ => {
                if let Err(e) = &call_result
                    && Self::is_method_not_found_error(e)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome(
                        "callmethoddynamic",
                        "notfound",
                    );
                }
                self.stack.push(call_result?);
            }
        }
        Ok(())
    }

    pub(super) fn exec_call_method_dynamic_mut_op(
        &mut self,
        code: &CompiledCode,
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_method_dispatch();
        self.flatten_scoped_env();
        let target_name = Self::const_str(code, target_name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx));
        let arity = arity as usize;
        if self.stack.len() < arity + 2 {
            return Err(RuntimeError::new(
                "Interpreter stack underflow in CallMethodDynamicMut",
            ));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        // ADR-0054 S3: spread only the `|EXPR` positions (see the matching
        // comment in `exec_call_method_dynamic_op`).
        let (args, _arg_sources) =
            Self::spread_call_args_by_syntax(code, raw_args, arg_sources_idx, None);
        let name_val = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("Interpreter stack underflow in CallMethodDynamicMut")
        })?;
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("Interpreter stack underflow in CallMethodDynamicMut")
        })?;
        if !quoted && !matches!(name_val.view(), ValueView::Package(_)) {
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(Self::invocant_as_positional(target));
            call_args.extend(args);
            let result = self.vm_call_on_value(name_val, call_args, None);
            match modifier {
                Some("+") | Some("*") => self.stack.push(Value::array(vec![result?])),
                _ => self.stack.push(result?),
            }
            return Ok(());
        }
        let method_name_str = Self::dynamic_method_name(&name_val);
        let method = Self::rewrite_method_name(&method_name_str, modifier);
        // ADR-0040's store boundary, Proxy half — the same hook the statically
        // named mutator dispatch applies (`@a."$name"($p)` stores the FETCHed
        // value too).
        let args = self.fetch_proxy_mutator_args(&method, args)?;
        // Handle .* and .+ modifiers
        match modifier {
            Some("+") => {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethoddynamicmut",
                    "modifier-plus",
                );
                let vals = self.call_method_all_with_fallback(&target, &method, &args, false)?;
                self.stack.push(Value::array(vals));
                return Ok(());
            }
            Some("*") => {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethoddynamicmut",
                    "modifier-star",
                );
                match self.call_method_all_with_fallback(&target, &method, &args, false) {
                    Ok(vals) => self.stack.push(Value::array(vals)),
                    Err(e) if Self::is_method_not_found_error(&e) => {
                        self.stack.push(Value::array(vec![]))
                    }
                    Err(e) => return Err(e),
                }
                return Ok(());
            }
            _ => {}
        }
        // Preserve the caller's env `self` across the dispatch: a dynamic method
        // call (`$obj."$name"()`) binds `self` to `$obj` for the callee, and the
        // mut dispatch path does not restore it. Without this, a later `self` read
        // in an enclosing nested sub (resolved from env via `GetSelfOrNoSelf`)
        // would see `$obj` leaked in. See try_compiled_method_or_interpret.
        let saved_self = self.get_env_with_main_alias("self");
        let saved_topic = self.get_env_with_main_alias("_");
        let call_result = if matches!(
            name_val.view(),
            ValueView::Sub(_) | ValueView::WeakSub(_) | ValueView::Routine { .. }
        ) {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethoddynamicmut",
                "call-sub-value",
            );
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(Self::invocant_as_positional(target));
            call_args.extend(args);
            self.vm_call_on_value(name_val, call_args, None)
        } else if modifier.is_none()
            && let Some(result) = self.try_native_buf_mut(&target_name, &target, &method, &args)
        {
            // Native fast path for mutating Buf write methods (`write-int*`/`write-uint*`/
            // `write-num*`/`write-bits`) reached via a *dynamic* method name
            // (`$buf."$write"(...)`) on a mutable Buf instance — mirror the static
            // CallMethodMut path (ledger §D(b)). Type-object / non-Buf receivers and
            // bad arity fall through to the generic fork unchanged.
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethoddynamicmut", "native");
            result
        } else {
            // TODO: compile to bytecode — generic mut method fork (ledger §1).
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethoddynamicmut", "user");
            self.vm_call_method_mut_with_values(&target_name, target, &method, args)
        };
        match saved_self {
            Some(s) => self.set_env_with_main_alias("self", s),
            None => {
                self.env_mut().remove("self");
            }
        }
        match saved_topic {
            Some(t) => self.set_env_with_main_alias("_", t),
            None => {
                self.env_mut().remove("_");
            }
        }
        let call_result = call_result?;
        self.stack.push(call_result);
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
        // Whether the receiver is `Nil`, read before the impl consumes the
        // operands (the stack is `[.., target, args...]` here, so the target is
        // `arity` slots below the top). Used for the Nil-absorb fallback below.
        let receiver_is_nil = self
            .stack
            .len()
            .checked_sub(arity as usize + 1)
            .and_then(|i| self.stack.get(i))
            .is_some_and(Value::is_nil);
        let result = self.exec_call_method_mut_op_impl(
            code,
            name_idx,
            arity,
            target_name_idx,
            modifier_idx,
            quoted,
            arg_sources_idx,
        );
        // Nil absorbs a method it does not define (raku's `Nil.FALLBACK`), the
        // same verdict the scalar `CallMethod` opcode and the hyper path reach.
        // This opcode -- a method call on a *named* receiver -- never applied
        // it, so `$?DISTRIBUTION.meta<ver>` outside a distribution died with
        // "No such method 'meta'" where raku answers Nil.
        //
        // Applied only *after* normal dispatch fails to find the method, not as
        // a pre-dispatch shortcut: `Nil` really does define control-flow and
        // introspection methods (`&?BLOCK.leave` on a Nil block, the exception
        // accessors), and short-circuiting those to Nil silently skipped them
        // (S04-statements/leave.t, S32-exceptions/misc.t). Falling back on the
        // not-found error is what `FALLBACK` means. `is_nil` is strictly `Nil`,
        // so an uninitialised `Any` receiver still errors as before.
        let result = match result {
            Err(e) if receiver_is_nil && Self::is_method_not_found_error(&e) => {
                self.stack.push(Value::NIL);
                Ok(())
            }
            other => other,
        };
        // The pending arg-source names/slots are scoped to THIS dispatch: a
        // callee signature bind consumes them, but a native/builtin dispatch
        // never binds and would leave them behind. A later bind with no
        // interleaving call opcode (e.g. the next chunk call of a Rust-driven
        // `.map` loop) would then re-resolve its sigilless params from the
        // leftover names against stale env keys (S32-hash/multislice-6e.t:
        // `-> \k, \v { Pair.new(k,v) }` repeated the first chunk). Clear on
        // every exit, mirroring the CallFunc/CallOnCodeVar set-then-clear pair.
        self.set_pending_call_arg_sources(None);
        self.pending_call_arg_source_slots.clear();
        result
    }

    #[allow(clippy::too_many_arguments)]
    fn exec_call_method_mut_op_impl(
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
        // Consume (and unconditionally clear) the accessor-ref marker: it is
        // emitted immediately before this opcode and scoped to this one dispatch.
        let want_ref = std::mem::take(&mut self.accessor_ref_pending);
        let decoded_sources = self.decode_arg_sources(code, arg_sources_idx);
        let method_raw = Self::const_str(code, name_idx);
        let target_name = Self::const_str(code, target_name_idx).to_string();
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx));
        let method = Self::rewrite_method_name(method_raw, modifier);
        // Interned once per call: the unrewritten name comes from the per-chunk
        // constant-symbol table, so the hot path pays no re-intern.
        let method_sym = match modifier {
            Some("^") | Some("!") => crate::symbol::Symbol::intern(&method),
            _ => code.const_sym(name_idx),
        };
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new(
                "Interpreter stack underflow in CallMethodMut",
            ));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        let has_varref = raw_args
            .iter()
            .any(|a| matches!(a.view(), ValueView::VarRef { .. }));
        // ADR-0054 S3: spread only the positions the caller wrote as `|EXPR`
        // -- decided by call-site syntax, not by a value merely evaluating to
        // a Slip (`.method(@a.Slip)` stays one argument).
        let (args, arg_sources) =
            Self::spread_call_args_by_syntax(code, raw_args, arg_sources_idx, decoded_sources);
        self.set_pending_call_arg_sources(arg_sources.clone());
        // Elements appended to a NATIVE integer array store through the native
        // slot, so each one wraps to the element width exactly as an assignment
        // does (`my uint8 @e; @e.push(1, 300, 2)` stores 1, 44, 2). Done here,
        // before the several push/append dispatch branches below, so every one
        // of them sees already-wrapped values.
        let args = if matches!(method.as_str(), "push" | "unshift" | "append" | "prepend")
            && !args.is_empty()
        {
            // Dual store: a scalar-held container (`my $a := array[uint8].new`)
            // keeps its live value — including the `array[uint8]` element-type
            // metadata `wrap_native_int_items` below reads via
            // `element_constraint_for` — in the local slot only, leaving the env
            // mirror at the `my`-declaration seed until some later sync point
            // (an I/O op, a frame boundary, ...) republishes it. Without this,
            // `native_int_element_constraint`'s `self.env().get(target_name)`
            // read the STALE, untagged env copy and silently skipped the wrap
            // (`$a.push(-1)` stored `-1` instead of wrapping to `255`), even
            // though `$a[0]`/`.of` — which read the authoritative slot — already
            // reported the array as `uint8`. Same fix as the sibling
            // element-assignment/`:delete` handlers (`seed_env_from_scalar_slot`).
            self.seed_env_from_scalar_slot(code, None, &target_name);
            self.wrap_native_int_items(&target_name, args)
        } else {
            args
        };
        // ADR-0040's store boundary, Proxy half — see
        // `Interpreter::fetch_proxy_mutator_args`.
        let args = self.fetch_proxy_mutator_args(&method, args)?;
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("Interpreter stack underflow in CallMethodMut target".to_string())
        })?;
        // `Pair.new($k, $v)` compiles its value argument tagged with `WrapVarRef`
        // (see `compile_expr_method_on_var`): when the receiver is the native
        // Pair type, box the source local into a shared `ContainerRef` cell so
        // the built Pair's value aliases `$v` (write-through, the same capture
        // the fat-arrow `MakePair` path performs). Any other receiver (a
        // shadowing user class, a rebound name) unwraps to the plain value —
        // identical to an untagged compile.
        let args = if has_varref {
            let native_pair_new = method == "new"
                && matches!(target.view(), ValueView::Package(cn) if cn == "Pair")
                && !self.has_user_method("Pair", "new");
            args.into_iter()
                .map(|a| match a.view() {
                    ValueView::VarRef { name, value, .. } => {
                        let inner = value.clone();
                        if native_pair_new {
                            let slot_hint = a.varref_slot();
                            // `box_type_objects`, exactly as the fat-arrow
                            // `MakePair` path does: an UNINITIALIZED declared
                            // scalar holds a bare type object but is still a
                            // container, and raku aliases it --
                            // `my Int $x; my $p = Pair.new("k", $x);
                            // $p.value = 5` leaves `$x` at 5.
                            self.capture_var_cell_boxing_type_objects(
                                code,
                                &name.resolve(),
                                inner,
                                slot_hint,
                            )
                        } else {
                            inner
                        }
                    }
                    _ => a,
                })
                .collect()
        } else {
            args
        };
        // `X::Foo.throw`/`.fail`/... on an Exception type object (compiled here
        // because the bareword target routes through CallMethodMut) requires a
        // concrete invocant: X::Parameter::InvalidConcreteness.
        if let Some(err) = self.exception_concreteness_error(&method, &args, &target) {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "exception-concreteness",
            );
            return Err(err);
        }
        // Reify/consume a deferred Seq (ADR-0034 §2.3) — including an
        // `IO::Handle.lines`/`.words` source (formerly the separate
        // `LazyIoLines` special case, forced here with a name-keyed env
        // writeback band-aid for `.cache` — ADR-0034 §1.3/§2.4). Reification
        // now fills the SAME `Arc<SeqBody>` every alias of the receiver
        // shares, so no writeback is needed: every alias (this frame's
        // variable, a second alias, a value passed to a sub one call frame
        // away) observes it for free.
        let target = self.reify_or_consume_seq_target(target, method.as_str())?;
        // Mutating methods reached through `.VAR` must retain the underlying
        // cell so the established container writeback paths can update it.
        let target = if !matches!(method.as_str(), "WHAT" | "^name" | "VAR")
            && let ValueView::ContainerView(cell) = target.view()
        {
            Value::container_ref(cell.clone())
        } else {
            target
        };
        // A lexical receiver uses CallMethodMut even for a read-only method.
        // Read through scalar itemization for Range methods, while retaining the
        // wrapper for the renderers that expose itemization.
        let target = if matches!(method.as_str(), "ACCEPTS" | "combinations" | "int-bounds")
            && target.descalarize().is_range()
        {
            target.descalarize().clone()
        } else {
            target
        };
        // ADR-0040 §9.2: a renderer resolves its receiver's `Proxy` elements
        // first — the third entry that needs this guard, alongside the
        // `CallMethod` opcode and `call_method_with_values_inner`. A method call
        // on a *variable* compiles to `CallMethodMut` (see
        // `compile_expr_method_on_var`), so `@a.gist` and `$l.raku` arrive here
        // and nowhere else. Placed with the other receiver-deciding steps above,
        // and after them, so it sees the receiver they settled on.
        let target = if Self::renders_receiver_elements(method.as_str())
            && Self::holds_nested_proxy(&target)
        {
            loan_env!(self, resolve_proxies_in_value(&target))?
        } else {
            target
        };
        if method == "value"
            && args.is_empty()
            && let Some(weight) = self.quanthash_weight_pair_value(target.unwrap_varref())
        {
            self.stack.push(weight);
            return Ok(());
        }
        // An `is native(...)` method: the call belongs to NativeCall, not to the
        // `{ * }` stub the declaration gives it. Both method-call opcodes need
        // this — a class's methods are compiled to bytecode and dispatched
        // without reaching the resolver, and `$obj.meth` on a variable compiles
        // to the *mut* opcode.
        if let Some(result) = loan_env!(
            self,
            try_native_method_on_receiver(&target, method.as_str(), &args)
        ) {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "nativecall");
            self.stack.push(result?);
            return Ok(());
        }
        // Mutating a lazy `@`-array (infinite source). raku rejects operations
        // that touch the (non-existent) end — push/pop/append — with
        // `X::Cannot::Lazy`, but allows front operations (unshift/prepend/shift/
        // splice), which reify the cached prefix to a real Array first
        // (no worse than the pre-L2 capped Array). Restricted to cache-backed
        // specs so the reify never runs user code or hangs. (L2)
        if let ValueView::LazyList(ll) = target.view()
            && ll.in_array_context()
            && ll.is_genuinely_lazy()
            && let Some(action) = match method.as_str() {
                "push" => Some("push to"),
                "pop" => Some("pop from"),
                "append" => Some("append to"),
                _ => None,
            }
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "lazy-array-mutate-reject",
            );
            return Err(RuntimeError::cannot_lazy_with_action(action, "Array"));
        }
        let target = if let ValueView::LazyList(ll) = target.view()
            && ll.in_array_context()
            && (ll.sequence_spec.is_some() || ll.closure_seq.is_some() || ll.scan_spec.is_some())
            && matches!(method.as_str(), "shift" | "unshift" | "prepend" | "splice")
        {
            let items = self.force_lazy_list_vm(&ll)?;
            let reified = Value::real_array(items);
            self.env_mut().insert(target_name.clone(), reified.clone());
            reified
        } else {
            target
        };
        // `Pair.freeze`: decontainerize the value (severing any Scalar-container
        // alias), make it read-only, and return the value (Pair.rakudoc).
        if method == "freeze"
            && args.is_empty()
            && matches!(
                target.view(),
                ValueView::Pair(..) | ValueView::ValuePair(..)
            )
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "pair-freeze");
            let frozen = self.pair_freeze(&target, &target_name);
            self.stack.push(frozen);
            return Ok(());
        }
        // `proto method` body dispatch (see try_proto_method_body).
        if let Some(result) = self.try_proto_method_body(&target, &method, &args) {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "proto");
            let v = result?;
            // Drain captured-outer writeback recorded by the dispatched multi
            // candidate (see exec_call_method_op). No-op in default builds.
            self.apply_pending_rw_writeback(code);
            self.stack.push(v);
            return Ok(());
        }
        // `Exception.Str`/`.gist` delegate to a user `message` *method* (e.g. from a
        // parameterized role). `$e.Str` on a variable compiles to CallMethodMut, so
        // the mut path needs the same interception as CallMethod.
        if let Some(out) = self.try_exception_str_via_user_message(&target, &method, &args)? {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "exception-str-message",
            );
            self.stack.push(out);
            return Ok(());
        }
        // gist/Str/raku/perl of a genuinely-lazy list renders raku's placeholder
        // (`[...]` in `@` array context, `(...)` for a bare Seq, `...` for Str)
        // rather than forcing the (possibly infinite) sequence. Must run before
        // the gather-coroutine force below, which would hang on an infinite list.
        if let ValueView::LazyList(ll) = target.view()
            && matches!(method.as_str(), "gist" | "Str" | "raku" | "perl")
            && ll.renders_lazy_placeholder()
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "lazy-placeholder",
            );
            self.stack
                .push(Value::str(crate::value::lazy_list_placeholder(
                    method.as_str(),
                    ll.in_array_context(),
                )));
            return Ok(());
        }
        // Lazy `.first` over a gather coroutine: pull incrementally instead of
        // forcing the (possibly infinite) list to completion.
        if let ValueView::LazyList(ll) = target.view()
            && ll.needs_vm_lazy_dispatch()
            && method == "first"
            && let Some(result) = self.try_lazy_gather_first(&ll, &args)
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "lazy-first");
            self.stack.push(result?);
            return Ok(());
        }
        // Lazy `.pairs`/`.antipairs`/`.kv` over a genuinely-lazy source: build a
        // lazy index-pipe stage instead of forcing the (possibly infinite)
        // source. Matches Rakudo where these are `.is-lazy` over a lazy list.
        if let ValueView::LazyList(ll) = target.view()
            && ll.needs_vm_lazy_dispatch()
            && ll.is_genuinely_lazy()
            && args.is_empty()
            && matches!(method.as_str(), "kv" | "pairs" | "antipairs")
        {
            let transform = match method.as_str() {
                "pairs" => crate::value::IndexTransform::Pairs,
                "antipairs" => crate::value::IndexTransform::AntiPairs,
                _ => crate::value::IndexTransform::Kv,
            };
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "lazy-index-pipe",
            );
            let pipe = Value::lazy_list(crate::gc::Gc::new(
                crate::value::LazyList::new_index_pipe(target.clone(), transform),
            ));
            self.stack.push(pipe);
            return Ok(());
        }
        // `.cache` on a genuinely-lazy list stays lazy (caches on demand); see
        // the matching note in the non-mut dispatch path.
        if let ValueView::LazyList(ll) = target.view()
            && method == "cache"
            && let Some(result) = ll.cache_lazy_view()
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "lazy-cache");
            self.stack.push(result);
            return Ok(());
        }
        let target = if let ValueView::LazyList(ll) = target.view()
            && ll.needs_vm_lazy_dispatch()
            && Self::lazy_list_needs_forcing(&method)
            // A `.map`/`.grep` on a lazy pipeline, an infinite sequence/closure
            // spec, OR a gather coroutine appends another lazy stage (interpreter
            // dispatch via `is_lazy_pipe_source`) — it must not force the source
            // here, or a `gather { … }.grep(…)[^3]` would run the whole gather body
            // (and its trailing side effects) instead of pulling on demand.
            // Laziness-preserving coercions return the list unchanged (native
            // dispatch) — neither forces.
            && !(matches!(method.as_str(), "map" | "grep")
                && (ll.lazy_pipe.is_some() || ll.is_infinite_spec() || ll.is_from_gather() || ll.cat_pull.is_some()))
            && !((ll.lazy_pipe.is_some() || ll.is_infinite_spec())
                && Self::lazy_pipe_preserving_coercion(&method))
            // On an infinite sequence/closure spec — OR an explicitly `lazy`-marked
            // (`lazy gather {…}`) list — the count/numeric coercions produce a
            // *soft* X::Cannot::Lazy Failure (recoverable with `//`), emitted by
            // the 0-arg native dispatch — they must not be hard-forced/reified.
            // A plain (non-`lazy`) finite gather stays forceable and reifies.
            && !((ll.is_infinite_spec() || ll.is_lazy_marked())
                && matches!(method.as_str(), "elems" | "Int" | "Numeric"))
        {
            let saved_env = self.env().clone();
            // `.head(n)` only needs the first `n` elements: pull them lazily so
            // an infinite gather does not hang.
            let items = match Self::gather_head_bound(&method, &args) {
                Some(n) => self.force_lazy_list_vm_n(&ll, n)?,
                // A strict force of an infinite list (lazy pipeline / infinite
                // sequence / closure spec) cannot terminate: raise
                // X::Cannot::Lazy with this method's name. A pipe whose source
                // chain provably bottoms out finite (`gather {...}.map(*+1)`)
                // DOES terminate, so it forces like any other finite list --
                // raku answers `.elems` there rather than throwing.
                None if (ll.lazy_pipe.is_some() && !ll.pipe_bottoms_out_finite())
                    || ll.is_infinite_spec() =>
                {
                    return Err(RuntimeError::cannot_lazy(&method));
                }
                None => self.force_lazy_list_vm(&ll)?,
            };
            // A lazy map/grep pipeline runs its callback via `vm_call_on_value`
            // in this Interpreter, so its side effects on enclosing variables are
            // legitimate and must persist (unlike gather coroutine corruption,
            // which the env restore undoes).
            if !matches!(method.as_str(), "elems" | "hyper" | "race") && ll.lazy_pipe.is_none() {
                *self.env_mut() = saved_env;
            }
            // A list-context view (`(gather {...}).List`, `.cache`) records
            // that the finite result must render as a List, not a Seq; the
            // non-mut dispatch path already honours it (`vm_call_method_ops.rs`)
            // and this one silently did not, so whether `.raku`/`eqv` saw a
            // List or a Seq depended purely on which of the two opcodes the
            // call compiled to (`CallMethod` for an inline receiver,
            // `CallMethodMut` for a named-variable one). That made
            // `my $a = (gather {...}).List; $a.raku` render `(1, 2).Seq` while
            // the two-statement spelling rendered `$(1, 2)`.
            if ll.in_list_context() {
                Value::array(items)
            } else {
                Value::seq(items)
            }
        } else {
            target
        };
        // Fast path: 0-arg attribute accessor read on an Instance (e.g.
        // `$obj.x`). A method call on a *variable* compiles to CallMethodMut for
        // potential invocant write-back, so accessor reads land here -- without
        // this they all fell back to the interpreter. The read does not mutate
        // the invocant, so no write-back to `target_name` is needed.
        if let Some(val) = self.try_fast_accessor_read(
            &target,
            &method,
            &args,
            modifier.is_some(),
            quoted,
            want_ref,
        ) {
            // Pure attribute read: does not mutate the invocant (see comment
            // above), so it does not dirty the caller's locals (Slice 6.3).
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "accessor");
            self.stack.push(val);
            return Ok(());
        }
        // `.so` / `.not` on a value whose type defines a user `Bool` method must
        // dispatch through that method (Mu.so / Mu.not are defined in terms of
        // .Bool) rather than the native truthiness fast path.
        if matches!(method.as_str(), "so" | "not") && args.is_empty() {
            let user_bool_owner = match target.view() {
                ValueView::Instance { class_name, .. } => Some(class_name.resolve()),
                ValueView::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = user_bool_owner
                && loan_env!(self, resolve_method_with_owner(&cn, "Bool", &[])).is_some()
            {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmut",
                    "so-not-user-bool",
                );
                let t = self.eval_truthy(&target);
                self.stack
                    .push(Value::truth(if method == "not" { !t } else { t }));
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
            && let ValueView::Str(s) = target.view()
            && **s == target_name
            && target_name
                .chars()
                .next()
                .is_some_and(|c| c.is_ascii_uppercase())
            && !self.has_type(&target_name)
            && !Self::is_builtin_type(&target_name)
            && !self.has_class(&target_name)
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "undeclared-type-new",
            );
            let suggestions = self.suggest_type_names(&target_name);
            return Err(RuntimeError::undeclared_type_symbols(
                &target_name,
                format!("Undeclared name:\n    {} used at line 1", target_name),
                suggestions,
            ));
        }
        // Junction auto-threading: thread method calls over junction values
        if let ValueView::Junction { kind, values } = target.view()
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
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "junction-invocant",
            );
            let mut results = Vec::new();
            // env_dirty substrate (docs/captured-outer-cell-sharing.md §10):
            // accumulate EVERY eigenstate's by-name caller write. Each eigenstate's
            // method dispatch records its captured-outer / `our` write into
            // `pending_rw_writeback_sources`, but the NEXT eigenstate's dispatch
            // overwrites that buffer, so only the last eigenstate's source survived
            // to the post-loop drain — a var written only by an EARLIER eigenstate
            // (`$cnt1` while the last writes `$cnt2`) was lost (double-OFF). Drain
            // each eigenstate's sources into the retain-on-miss
            // `pending_caller_var_writeback` so all of them persist; the post-loop
            // drain then writes every owning caller slot precisely from env (which
            // already holds the accumulated value).
            for v in values.iter() {
                let r = if let Some(threaded) =
                    self.maybe_autothread_method_args(v, &method, &args)?
                {
                    threaded
                } else if let Some(nr) = self.try_native_method(v, method_sym, &args) {
                    nr?
                } else {
                    self.try_compiled_method_or_interpret_sym(v.clone(), method_sym, args.clone())?
                };
                results.push(r);
                let pending: Vec<String> = self
                    .pending_rw_writeback_sources
                    .drain(..)
                    .chain(self.pending_caller_var_writeback.drain(..))
                    .collect();
                for name in pending {
                    self.record_caller_var_writeback(&name);
                }
            }
            let junction_result = Value::junction(kind, results);
            self.stack.push(junction_result);
            // Slice F (env<->locals coherence): an invocant junction that threads
            // a *user* method mutating a captured outer / `our` variable (e.g.
            // `$junc.a` with `method a { $cnt++ }`) accumulates each eigenstate's
            // write into env correctly, but the per-call pending writeback only
            // carries the *last* eigenstate's source — so a different variable
            // written by an earlier eigenstate (`$cnt1` vs the last `$cnt2`) never
            // reaches the caller's local slot. This junction path returns before
            // the normal post-dispatch reconcile, so drain the accumulated
            // per-eigenstate writebacks into the caller's slots here; env already
            // holds every eigenstate's accumulated value.
            self.apply_pending_caller_var_writeback(code);
            return Ok(());
        }

        // Junction auto-threading for method arguments (mut variant)
        if let Some(result) = self.maybe_autothread_method_args(&target, &method, &args)? {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "junction-args");
            self.stack.push(result);
            return Ok(());
        }

        // .WHO on pseudo-package Package values: build the stash in the Interpreter
        // where we have access to locals (which the interpreter doesn't have).
        if method == "WHO"
            && args.is_empty()
            && matches!(target.view(), ValueView::Package(name) if Self::is_pseudo_package_bare(&name.resolve()))
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "who-pseudo-package",
            );
            if let ValueView::Package(pkg_name) = target.view() {
                let stash = self.build_pseudo_stash(code, &pkg_name.resolve());
                self.stack.push(stash);
            }
            return Ok(());
        }

        // `Lock.protect` / `Lock::Async.protect` require a defined invocant and a
        // single Callable block. The type object (`Lock.protect: …`) or a
        // non-Callable arg (`.protect: %()`) matches no candidate and must throw
        // X::Multi::NoMatch (roast .../multi-no-match.t).
        if method == "protect" {
            let is_lock_type_object = matches!(target.view(), ValueView::Package(name)
                if matches!(name.resolve().as_str(),
                    "Lock" | "Lock::Async" | "Lock::Soft"));
            let is_lock_instance_bad_arg = matches!(target.view(),
                ValueView::Instance { class_name, .. }
                if matches!(class_name.resolve().as_str(),
                    "Lock" | "Lock::Async" | "Lock::Soft"))
                && (args.len() != 1
                    || !matches!(args[0].view(), ValueView::Sub(..) | ValueView::WeakSub(..)));
            if is_lock_type_object || is_lock_instance_bad_arg {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmut",
                    "lock-protect-nomatch",
                );
                return Err(
                    crate::runtime::methods_signature_errors::make_multi_no_match_error("protect"),
                );
            }
        }
        // Fast path for Lock::Async.protect — execute block inline in current Interpreter
        if method == "protect"
            && args.len() == 1
            && let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
            && (class_name.resolve() == "Lock::Async" || class_name.resolve() == "Lock")
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "lock-protect");
            let lock_id = match attributes.as_map().get("lock-id").map(Value::view) {
                Some(ValueView::Int(id)) if id > 0 => id as u64,
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
            // Entering the critical section: pull the latest value of any
            // shared scalar a previous holder committed inside its own
            // critical section (mirrors Semaphore.acquire).
            self.enter_critical_section();
            let code_val = args.into_iter().next().unwrap_or(Value::NIL);
            let result = match self.try_exec_simple_shared_protect_block(code, &code_val)? {
                Some(value) => Ok(value),
                None => self.exec_protect_block_inline(code, &code_val),
            };
            self.leave_critical_section();
            let _ = crate::runtime::native_methods::release_lock(&lock, me);
            self.stack.push(result?);
            return Ok(());
        }

        // `Lock::Async.protect-or-queue-on-recursion` /
        // `.with-lock-hidden-from-recursion-check`: the recursion-aware
        // siblings of `.protect`. See `runtime::lock_async_recursion`.
        if matches!(
            method.as_str(),
            "protect-or-queue-on-recursion" | "with-lock-hidden-from-recursion-check"
        ) && args.len() == 1
            && let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = target.view()
            && class_name.resolve() == "Lock::Async"
        {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "lock-async-recursion",
            );
            let lock_id = match attributes.as_map().get("lock-id").map(Value::view) {
                Some(ValueView::Int(id)) if id > 0 => id as u64,
                _ => {
                    return Err(RuntimeError::new(format!(
                        "Lock::Async.{method} called on a Lock without lock-id"
                    )));
                }
            };
            let code_val = args.into_iter().next().unwrap_or(Value::NIL);
            let result = if method == "protect-or-queue-on-recursion" {
                self.exec_lock_protect_or_queue_on_recursion(lock_id, code_val)?
            } else {
                self.exec_lock_with_lock_hidden_from_recursion_check(lock_id, code_val)?
            };
            self.stack.push(result);
            return Ok(());
        }

        // Fast path for mutating array methods on shared @-arrays.
        // Bypasses the full method dispatch chain (try_native_method →
        // call_method_mut_with_values → push_to_shared_var) for the common case
        // of pushing simple values to a shared array inside a tight loop
        // (e.g. Lock::Async.protect { push @target, $i }).
        // ...but not for a name this lineage RE-DECLARED: the store's entry under
        // it belongs to the shadowed outer binding, so funnelling a routine's own
        // `my @a` through it makes the two the same array (a nested sub's `push
        // @components` landing in the caller's `@components` broke every
        // multi-server Cro::HTTP test — see
        // `news/2026-08/threaded-array-mutation-escapes-to-the-caller.md`).
        if target_name.starts_with('@')
            && matches!(target.view(), ValueView::Array(..))
            && self.shared_vars_active
            && !self.container_name_is_redeclared(&target_name)
        {
            // Only a plain *lexical* `@name` is a single variable shared across
            // threads. Instance-attribute arrays (`@!order` / `@.order`) and
            // other twigil'd forms (`@*dyn`) have per-instance / per-context
            // identity, so they must NOT funnel into the global atomic store
            // keyed by name — that would accumulate pushes across every object
            // (roles-6e.t DESTROY: each `C1` instance's `@!order` doubled). They
            // keep the original base-key / interior-mutation path.
            let plain = Self::is_plain_lexical_array_name(&target_name);
            match method.as_str() {
                // Route through the atomic shared store. The base-key
                // `push_to_existing_shared_array`/`push_to_shared_var` write the
                // plain `@a` shared entry, which `set_shared_var` can clobber with
                // a stale empty snapshot during env sync — losing concurrent
                // `start { @a.push(...) }` updates from sibling threads. (The
                // base-key path also `extend`ed for `unshift`, appending instead
                // of prepending.) The `__mutsu_atomic_arr::` store is exempt from
                // that clobber, so concurrent push/unshift serialize and all land.
                //
                // append/prepend MUST funnel here too: once a push created the
                // atomic entry, reads prefer it, so an append applied to the
                // stale base/env copy is silently invisible — the zef
                // `populate-distributions` bug (`push @idx, ...; append @idx,
                // ...` on a hyper worker lost every appended element).
                "push" | "unshift" | "append" | "prepend" if plain && !args.is_empty() => {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "shared-array-push-atomic",
                    );
                    let items = if matches!(method.as_str(), "push" | "unshift") {
                        crate::runtime::Interpreter::normalize_push_unshift_args(args.clone())
                    } else {
                        crate::runtime::flatten_append_args(args.clone())
                    };
                    // Stored through a native slot, so each element wraps to the
                    // element width (`my uint8 @e; @e.push(1, 300, 2)` -> 1, 44, 2).
                    let items = self.wrap_native_int_items(&target_name, items);
                    let front = matches!(method.as_str(), "unshift" | "prepend");
                    let result = self.shared_array_extend(&target_name, items, front);
                    self.stack.push(result);
                    return Ok(());
                }
                "push" | "unshift" if !args.is_empty() => {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "shared-array-push-legacy",
                    );
                    let result = loan_env!(
                        self,
                        push_to_existing_shared_array(&target_name, args.clone())
                    )
                    .unwrap_or_else(|| {
                        loan_env!(self, push_to_shared_var(&target_name, args, &target))
                    });
                    self.stack.push(result);
                    return Ok(());
                }
                // Removal ops only lose updates once the atomic entry shadows
                // the base copy, so gate on its existence and keep the richer
                // slow path (arity/lazy/immutable errors, callable splice
                // args) for the unshadowed case.
                "pop" | "shift"
                    if plain
                        && args.is_empty()
                        && matches!(
                            target.view(),
                            ValueView::Array(_, crate::value::ArrayKind::Array)
                        )
                        && self.atomic_array_entry_exists(&target_name) =>
                {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "shared-array-pop-shift",
                    );
                    let (result, _) = self.shared_array_mutate(&target_name, |data, _| {
                        if data.items().is_empty() {
                            crate::runtime::utils::make_empty_array_failure_what(&method, "Array")
                        } else if method == "shift" {
                            data.items_mut().remove(0)
                        } else {
                            data.items_mut().pop().unwrap_or(Value::NIL)
                        }
                    });
                    self.stack.push(result);
                    return Ok(());
                }
                "splice"
                    if plain
                        && matches!(
                            target.view(),
                            ValueView::Array(_, crate::value::ArrayKind::Array)
                        )
                        && self.atomic_array_entry_exists(&target_name) =>
                {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "shared-array-splice",
                    );
                    let (removed, _) = self.shared_array_mutate(&target_name, |data, _| {
                        crate::runtime::Interpreter::splice_array_data(data, &args)
                    });
                    self.stack.push(Value::real_array(removed));
                    return Ok(());
                }
                _ => {}
            }
        }

        let mut skip_native = quoted
            && matches!(
                method.as_str(),
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            );
        let is_junction_target = match target.view() {
            ValueView::Junction { .. } => true,
            ValueView::Scalar(inner) => matches!(inner.view(), ValueView::Junction { .. }),
            _ => false,
        };
        if matches!(method.as_str(), "gist" | "raku" | "perl") && is_junction_target {
            skip_native = true;
        }
        // Also skip native if the target has a user-defined method with this name,
        // but NOT for pseudo-methods like DEFINITE, WHAT, etc. which are macros.
        // WHICH/WHY are exceptions: unlike the other six, raku treats them as
        // ordinary, overridable methods in every call form (not just quoted),
        // so a user override must win here too.
        if !skip_native
            && !matches!(
                method.as_str(),
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHERE" | "VAR"
            )
        {
            let class_name = match target.view() {
                ValueView::Instance { class_name, .. } => Some(class_name.resolve()),
                ValueView::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name
                && self.has_user_method(&cn, &method)
            {
                skip_native = true;
            }
        }
        if !skip_native
            && matches!(method.as_str(), "AT-KEY" | "keys" | "values")
            && matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Stash")
        {
            skip_native = true;
        }
        if !skip_native
            && method == "keys"
            && target_name.starts_with('%')
            && loan_env!(self, var_hash_key_constraint(&target_name)).is_some()
        {
            skip_native = true;
        }
        if !skip_native
            && matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "Proc::Async")
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
            && matches!(target.view(), ValueView::Instance { class_name, .. } if class_name == "IterationBuffer")
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
        // `skip_pseudo_method_native` exists for exactly one purpose: a *quoted*
        // MOP pseudo-method call (`$obj."WHAT"()`) must dispatch a user-defined
        // method of that name instead of the reflection macro
        // (`dispatch_method_by_name_1` consumes it). It is NOT a general
        // "this receiver skips native dispatch" signal, so it must be gated the
        // same way its `CallMethod` twin gates it (`vm_call_method_ops.rs`).
        // Setting it for every `skip_native` leaked the flag into the *first*
        // nested dispatch of the same method name: `my $r = any("5","6");
        // $r.raku` set it to `"raku"` (junction receiver), so the junction
        // renderer's first `"5".raku` bypassed the native repr and fell to the
        // stringifying catch-all, printing `any(5, "6")`.
        if quoted
            && skip_native
            && matches!(
                method.as_str(),
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            )
        {
            self.skip_pseudo_method_native = Some(method.clone());
        }
        // Handle Match.make — must mutate the Match instance's `ast` attribute
        // and write the modified Match back to the variable.
        if method == "make" && target.is_match_instance() {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "match-make");
            let value = args.into_iter().next().unwrap_or(Value::NIL);
            if let Some(updated) = target.match_with_ast_keeping_id(value.clone()) {
                self.env_mut().insert(target_name.to_string(), updated);
                self.env_mut().insert("made".to_string(), value.clone());
                self.action_made = Some(value.clone());
            }
            self.stack.push(value);
            return Ok(());
        }
        // `$s.subst-mutate(pattern, replacement, ...)` substitutes in place (like
        // `s///`) and returns the value `s///` would set in `$/`: a Match for a
        // single hit, the `Any` type object when nothing matched, or a List of
        // Matches under `:g`. Reuses the `.subst` machinery for the new string
        // and the `.match` machinery for the return, then writes the new string
        // back to the variable -- mirroring the `Match.make` pattern above.
        if method == "subst-mutate" && matches!(target.view(), ValueView::Str(_)) {
            crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "subst-mutate");
            let new_str = self.dispatch_subst(target.clone(), &args)?;
            // `.match` takes the pattern + adverbs but not the replacement (the
            // 2nd positional), so drop the replacement when building its args.
            let mut match_args: Vec<Value> = Vec::new();
            let mut positional_seen = 0;
            for arg in &args {
                if arg.is_string_pair_value() {
                    match_args.push(arg.clone());
                } else {
                    positional_seen += 1;
                    if positional_seen != 2 {
                        match_args.push(arg.clone());
                    }
                }
            }
            let global = args.iter().any(
                |a| matches!(a.view(), ValueView::Pair(k, v) if (k == "g" || k == "global") && v.truthy()),
            );
            let match_result = self.dispatch_match_method(target.clone(), &match_args)?;
            // A single failed match yields the `Any` type object (matching `$/`
            // after a failed `s///`), where `.match` alone would yield `Nil`.
            let ret = if !global && match_result.is_nil() {
                Value::package(crate::symbol::Symbol::intern("Any"))
            } else {
                match_result
            };
            self.env_mut()
                .insert(target_name.to_string(), new_str.clone());
            self.locals_set_by_name(code, &target_name, new_str);
            self.stack.push(ret);
            return Ok(());
        }
        // .hyper/.race with named arguments in mut path
        if matches!(method.as_str(), "hyper" | "race") && !args.is_empty() {
            crate::vm::vm_stats::record_dispatch_entry_intercept(
                "callmethodmut",
                "hyper-race-config",
            );
            let mut batch: Option<i64> = None;
            let mut degree: Option<i64> = None;
            for arg in &args {
                let (key, val) = match arg.view() {
                    ValueView::Pair(k, v) => (k.clone(), crate::runtime::to_int(v)),
                    ValueView::ValuePair(k, v) => (k.to_string_value(), crate::runtime::to_int(v)),
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
                attrs.insert("value".to_string(), Value::int(b));
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
                attrs.insert("value".to_string(), Value::int(d));
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
            let body = crate::value::SeqBody::reified(items);
            // Remember the requested batch/degree so `.configuration` can report
            // them (the HyperSeq/RaceSeq does not carry the config).
            body.set_hyper_config(batch, degree);
            let result = if method == "hyper" {
                Value::hyper_seq_body(body)
            } else {
                Value::race_seq_body(body)
            };
            self.stack.push(result);
            return Ok(());
        }
        // HyperSeq/RaceSeq delegation in mut path
        if matches!(
            target.view(),
            ValueView::HyperSeq(_) | ValueView::RaceSeq(_)
        ) {
            let is_hyper = matches!(target.view(), ValueView::HyperSeq(_));
            match method.as_str() {
                "hyper" | "race" | "is-lazy" | "^name" | "WHAT" | "defined" => {
                    let items_arc = match target.view() {
                        ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => items.clone(),
                        _ => unreachable!(),
                    };
                    let result = match method.as_str() {
                        "hyper" => Value::hyper_seq_body(items_arc),
                        "race" => Value::race_seq_body(items_arc),
                        "is-lazy" => Value::FALSE,
                        "defined" => Value::TRUE,
                        "^name" => {
                            let name = if is_hyper { "HyperSeq" } else { "RaceSeq" };
                            Value::str(name.to_string())
                        }
                        "WHAT" => {
                            let name = if is_hyper { "HyperSeq" } else { "RaceSeq" };
                            Value::package(Symbol::intern(name))
                        }
                        _ => unreachable!(),
                    };
                    let arm = match method.as_str() {
                        "hyper" => "hyperseq-hyper",
                        "race" => "hyperseq-race",
                        "is-lazy" => "hyperseq-is-lazy",
                        "defined" => "hyperseq-defined",
                        "^name" => "hyperseq-name",
                        "WHAT" => "hyperseq-what",
                        _ => unreachable!(),
                    };
                    crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", arm);
                    self.stack.push(result);
                    return Ok(());
                }
                "map" | "grep" => {
                    // Delegate to array, then wrap result
                    let items_arc = match target.view() {
                        ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => items.clone(),
                        _ => unreachable!(),
                    };
                    let array_target = Value::array_with_kind(
                        crate::value::Value::array_arc(items_arc.to_vec()),
                        crate::value::ArrayKind::List,
                    );
                    let call_result = if let Some(native_result) =
                        self.try_native_method(&array_target, method_sym, &args)
                    {
                        native_result
                    } else {
                        self.try_compiled_method_mut_or_interpret_sym(
                            &target_name,
                            array_target,
                            method_sym,
                            args,
                        )
                    };
                    let result_val = call_result?;
                    let result_items = crate::runtime::value_to_list(&result_val);
                    let wrapped = if is_hyper {
                        Value::hyper_seq(result_items)
                    } else {
                        Value::race_seq(result_items)
                    };
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "hyperseq-map-grep",
                    );
                    self.stack.push(wrapped);
                    return Ok(());
                }
                "iterator" if args.is_empty() => {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "hyperseq-iterator",
                    );
                    // A HyperSeq/RaceSeq allows only a single iterator (rakudo #4413):
                    // a second `.iterator` throws X::Seq::Consumed. The consumed-state
                    // is tracked on the inner Arc via the shared Seq registry.
                    let items_arc = match target.view() {
                        ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => items.clone(),
                        _ => unreachable!(),
                    };
                    let type_name = if is_hyper { "HyperSeq" } else { "RaceSeq" };
                    // Atomic check-and-mark under one lock, so concurrent workers
                    // racing for the single iterator resolve to exactly one winner
                    // (rakudo #4413 concurrency contract). Orthogonal to
                    // ADR-0034's reify/consume split — see `claim_hyper_iterator_once`.
                    if items_arc.claim_hyper_iterator_once().is_err() {
                        return Err(crate::value::seq_consumed_error_for(type_name));
                    }
                    let array_target = Value::array_with_kind(
                        crate::value::Value::array_arc(items_arc.to_vec()),
                        crate::value::ArrayKind::List,
                    );
                    let iter =
                        crate::builtins::iterator_construct::build_iterator_instance(&array_target);
                    self.stack.push(iter);
                    return Ok(());
                }
                _ => {
                    // For all other methods, convert to List and delegate
                }
            }
        }
        // Convert HyperSeq/RaceSeq to List for remaining method dispatch
        let target = match target.view() {
            ValueView::HyperSeq(items) | ValueView::RaceSeq(items) => Value::array_with_kind(
                crate::value::Value::array_arc(items.to_vec()),
                crate::value::ArrayKind::List,
            ),
            _ => target,
        };

        // Fast paths for xxKEY methods on Hash/Set/Bag/Mix types
        match method.as_str() {
            "AT-KEY" if args.len() == 1 => {
                let inner_target = match target.view() {
                    ValueView::Scalar(inner) => inner,
                    _ => &target,
                };
                if let ValueView::Hash(map) = inner_target.view() {
                    // An object hash stores `.WHICH` keys.
                    let key = if map.key_type.is_some() {
                        crate::runtime::utils::value_which_key(&args[0])
                    } else {
                        args[0].to_string_value()
                    };
                    let raw = self.resolve_hash_entry(&map, &key);
                    // ADR-0049 slice 5 (row 25): `resolve_hash_entry` returns
                    // the raw `Value::NIL` absent-key sentinel with no
                    // compensation of its own -- every OTHER hash-key reader
                    // (`vm_var_index_ops.rs`) substitutes the container's own
                    // default (`is default(...)` -> typed element type object
                    // -> `Any`) when the key is missing; `AT-KEY` had none at
                    // all, so `%h.AT-KEY("missing")` answered a bare `Nil`
                    // instead of `(Any)`/`(Int)`/the declared default.
                    let result = if raw.is_nil() {
                        self.typed_container_default(inner_target)
                    } else {
                        raw
                    };
                    crate::vm::vm_stats::record_dispatch_entry_intercept("callmethodmut", "at-key");
                    self.stack.push(result);
                    return Ok(());
                }
            }
            "ASSIGN-KEY" if args.len() == 2 => {
                let key = args[0].to_string_value();
                let value = args[1].clone();
                let inner_target = match target.view() {
                    ValueView::Scalar(inner) => inner,
                    _ => &target,
                };
                match inner_target.view() {
                    ValueView::Hash(map) => {
                        let old_meta = self.container_type_metadata(inner_target).clone();
                        // Clone the whole HashData (not just the map) so the
                        // object-hash `original_keys` survive; an object hash
                        // stores the key under its `.WHICH` and records the
                        // key object.
                        let mut data = (**map).clone();
                        if data.key_type.is_some() {
                            let which = crate::runtime::utils::value_which_key(&args[0]);
                            data.original_keys
                                .get_or_insert_with(std::collections::HashMap::new)
                                .insert(which.clone(), args[0].clone());
                            data.map.insert(which, value.clone());
                        } else {
                            data.map.insert(key, value.clone());
                        }
                        let new_hash = Value::hash_with_data(crate::gc::Gc::new(data));
                        let meta = old_meta.unwrap_or(crate::runtime::ContainerTypeInfo {
                            value_type: "Any".to_string(),
                            key_type: None,
                            declared_type: None,
                        });
                        let new_hash = self.tag_container_metadata(new_hash, meta);
                        self.env_mut().insert(target_name.to_string(), new_hash);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "assign-key",
                        );
                        self.stack.push(value);
                        return Ok(());
                    }
                    ValueView::Set(_, false) => {
                        let repr = crate::runtime::gist_value(inner_target);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "assign-key",
                        );
                        return Err(RuntimeError::assignment_ro_typename("Set", &repr));
                    }
                    ValueView::Set(data, true) => {
                        let (qkey, elem) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
                        let mut new_data = (**data).clone();
                        if value.truthy() {
                            crate::runtime::utils::record_quanthash_original(
                                new_data.original_keys.get_or_insert_with(Default::default),
                                &qkey,
                                &elem,
                            );
                            new_data.elements.insert(qkey);
                        } else {
                            new_data.elements.remove(&qkey);
                            if let Some(ok) = new_data.original_keys.as_mut() {
                                ok.remove(&qkey);
                            }
                        }
                        let new_val = Value::set_parts(crate::gc::Gc::new(new_data), true);
                        self.env_mut().insert(target_name.to_string(), new_val);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "assign-key",
                        );
                        self.stack.push(value);
                        return Ok(());
                    }
                    ValueView::Bag(_, false) => {
                        let repr = crate::runtime::gist_value(inner_target);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "assign-key",
                        );
                        return Err(RuntimeError::assignment_ro_typename("Bag", &repr));
                    }
                    ValueView::Bag(data, true) => {
                        let (qkey, elem) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
                        let count = value.to_bigint();
                        let mut new_data = (**data).clone();
                        if num_traits::Signed::is_positive(&count) {
                            crate::runtime::utils::record_quanthash_original(
                                new_data.original_keys.get_or_insert_with(Default::default),
                                &qkey,
                                &elem,
                            );
                            new_data.counts.insert(qkey, count);
                        } else {
                            new_data.counts.remove(&qkey);
                            if let Some(ok) = new_data.original_keys.as_mut() {
                                ok.remove(&qkey);
                            }
                        }
                        let new_val = Value::bag_parts(crate::gc::Gc::new(new_data), true);
                        self.env_mut().insert(target_name.to_string(), new_val);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "assign-key",
                        );
                        self.stack.push(value);
                        return Ok(());
                    }
                    ValueView::Mix(_, false) => {
                        let repr = crate::runtime::gist_value(inner_target);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "assign-key",
                        );
                        return Err(RuntimeError::assignment_ro_typename("Mix", &repr));
                    }
                    ValueView::Mix(data, true) => {
                        let (qkey, elem) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
                        let weight = crate::runtime::to_float_value(&value).unwrap_or(0.0);
                        let mut new_data = (**data).clone();
                        if weight != 0.0 {
                            crate::runtime::utils::record_quanthash_original(
                                new_data.original_keys.get_or_insert_with(Default::default),
                                &qkey,
                                &elem,
                            );
                            new_data.weights.insert(qkey, weight);
                        } else {
                            new_data.weights.remove(&qkey);
                            if let Some(ok) = new_data.original_keys.as_mut() {
                                ok.remove(&qkey);
                            }
                        }
                        let new_val = Value::mix_parts(crate::gc::Gc::new(new_data), true);
                        self.env_mut().insert(target_name.to_string(), new_val);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "assign-key",
                        );
                        self.stack.push(value);
                        return Ok(());
                    }
                    ValueView::Nil | ValueView::Package(_) => {
                        let mut hash = std::collections::HashMap::new();
                        hash.insert(key, value.clone());
                        self.env_mut().insert(
                            target_name.to_string(),
                            Value::hash_with_data(Value::hash_arc(hash)),
                        );
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "assign-key",
                        );
                        self.stack.push(value);
                        return Ok(());
                    }
                    _ => {}
                }
            }
            "DELETE-KEY" if args.len() == 1 => {
                if let Err(e) = crate::runtime::refuse_map_removal(&target) {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "delete-key",
                    );
                    return Err(e);
                }
                let key = args[0].to_string_value();
                let inner_target = match target.view() {
                    ValueView::Scalar(inner) => inner,
                    _ => &target,
                };
                match inner_target.view() {
                    ValueView::Hash(map) => {
                        // An object hash stores `.WHICH` keys.
                        let key = if map.key_type.is_some() {
                            crate::runtime::utils::value_which_key(&args[0])
                        } else {
                            key
                        };
                        let old_meta = self.container_type_metadata(inner_target).clone();
                        let old_value = if map.contains_key(&key) {
                            self.resolve_hash_entry(&map, &key)
                        } else {
                            let type_name = old_meta
                                .as_ref()
                                .map(|info| info.value_type.clone())
                                .unwrap_or_else(|| "Any".to_string());
                            Value::package(Symbol::intern(&type_name))
                        };
                        // Prefer an aliased in-place removal so the deletion is
                        // visible through every holder of the same hash Arc (a
                        // `\SELF` raw param / `:=` bind), exactly like array
                        // splice/DELETE-POS. `HashMap::remove` does not
                        // reallocate, so the pointer-keyed container metadata
                        // stays attached and needs no re-tag. Rebuilding a fresh
                        // hash (the fallback below) severs the alias, so a
                        // `postcircumfix:<{ }>(\SELF, \k, :$eject){ SELF.DELETE-KEY(k) }`
                        // would not reach the caller's `%h`.
                        // `env_root_descended_mut` rather than a raw
                        // `env.get_mut`: the name may hold a shared
                        // `ContainerRef` cell (a `:=` rebind, an rw capture, or
                        // simply having been passed to a Raku-level routine),
                        // which `with_hash_mut` does not match, dropping us into
                        // the alias-severing rebuild below.
                        let removed_in_place = self
                            .env_root_descended_mut(target_name.as_ref())
                            .and_then(|v| {
                                v.with_hash_mut(|gc| {
                                    crate::value::gc_data_mut(gc).remove(&key);
                                })
                            })
                            .is_some();
                        if !removed_in_place {
                            let mut new_map = (**map).clone();
                            new_map.remove(&key);
                            let new_hash = Value::hash_with_data(Value::hash_arc(new_map));
                            let meta = old_meta.unwrap_or(crate::runtime::ContainerTypeInfo {
                                value_type: "Any".to_string(),
                                key_type: None,
                                declared_type: None,
                            });
                            let new_hash = self.tag_container_metadata(new_hash, meta);
                            self.env_mut().insert(target_name.to_string(), new_hash);
                        }
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "delete-key",
                        );
                        self.stack.push(old_value);
                        return Ok(());
                    }
                    ValueView::Set(_, false) => {
                        let repr = crate::runtime::utils::gist_value(inner_target);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "delete-key",
                        );
                        return Err(RuntimeError::assignment_ro_typename("Set", &repr));
                    }
                    ValueView::Set(data, true) => {
                        let (qkey, _) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
                        let existed = data.elements.contains(&qkey);
                        let mut new_data = (**data).clone();
                        new_data.elements.remove(&qkey);
                        if let Some(ok) = new_data.original_keys.as_mut() {
                            ok.remove(&qkey);
                        }
                        let new_val = Value::set_parts(crate::gc::Gc::new(new_data), true);
                        self.env_mut().insert(target_name.to_string(), new_val);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "delete-key",
                        );
                        self.stack.push(Value::truth(existed));
                        return Ok(());
                    }
                    ValueView::Bag(_, false) => {
                        let repr = crate::runtime::utils::gist_value(inner_target);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "delete-key",
                        );
                        return Err(RuntimeError::assignment_ro_typename("Bag", &repr));
                    }
                    ValueView::Bag(data, true) => {
                        let (qkey, _) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
                        let old_count = data.counts.get(&qkey).cloned().unwrap_or_default();
                        let mut new_data = (**data).clone();
                        new_data.counts.remove(&qkey);
                        if let Some(ok) = new_data.original_keys.as_mut() {
                            ok.remove(&qkey);
                        }
                        let new_val = Value::bag_parts(crate::gc::Gc::new(new_data), true);
                        self.env_mut().insert(target_name.to_string(), new_val);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "delete-key",
                        );
                        self.stack.push(Value::from_bigint(old_count));
                        return Ok(());
                    }
                    ValueView::Mix(_, false) => {
                        let repr = crate::runtime::utils::gist_value(inner_target);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "delete-key",
                        );
                        return Err(RuntimeError::assignment_ro_typename("Mix", &repr));
                    }
                    ValueView::Mix(data, true) => {
                        let (qkey, _) = crate::runtime::utils::quanthash_elem_entry(&args[0]);
                        let old_weight = data.weights.get(&qkey).copied().unwrap_or(0.0);
                        let mut new_data = (**data).clone();
                        new_data.weights.remove(&qkey);
                        if let Some(ok) = new_data.original_keys.as_mut() {
                            ok.remove(&qkey);
                        }
                        let new_val = Value::mix_parts(crate::gc::Gc::new(new_data), true);
                        self.env_mut().insert(target_name.to_string(), new_val);
                        let result = crate::value::mix_weight_to_value(old_weight);
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "delete-key",
                        );
                        self.stack.push(result);
                        return Ok(());
                    }
                    ValueView::Nil | ValueView::Package(_) => {
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "delete-key",
                        );
                        self.stack.push(Value::NIL);
                        return Ok(());
                    }
                    _ => {}
                }
            }
            "BIND-KEY" if args.len() == 2 => {
                let inner_target = match target.view() {
                    ValueView::Scalar(inner) => inner,
                    _ => &target,
                };
                match inner_target.view() {
                    ValueView::Hash(map) => {
                        let old_meta = self.container_type_metadata(inner_target).clone();
                        let value = args[1].clone();
                        let source_var = arg_sources
                            .as_ref()
                            .and_then(|s| s.get(1))
                            .and_then(|s| s.clone());
                        let mut new_map = (**map).clone();
                        // An object hash stores `.WHICH` keys and records the
                        // key object.
                        let key = if new_map.key_type.is_some() {
                            let which = crate::runtime::utils::value_which_key(&args[0]);
                            new_map
                                .original_keys
                                .get_or_insert_with(std::collections::HashMap::new)
                                .insert(which.clone(), args[0].clone());
                            which
                        } else {
                            args[0].to_string_value()
                        };
                        // Phase 2 Stage 2: BIND-KEY installs a shared
                        // `ContainerRef` cell (reusing the source variable's
                        // existing cell binding when present) instead of a
                        // BOUND_HASH_REF_SENTINEL back-reference.
                        let mut bind_source_install: Option<(String, Value)> = None;
                        if let Some(var_name) = source_var {
                            let cell = match self.env().get(&var_name).map(Value::view) {
                                Some(ValueView::ContainerRef(cell)) => cell.clone(),
                                _ => {
                                    let cell = crate::gc::Gc::new(
                                        crate::value::ContainerCell::new(value.clone()),
                                    );
                                    bind_source_install =
                                        Some((var_name, Value::container_ref(cell.clone())));
                                    cell
                                }
                            };
                            new_map.insert(key, Value::container_ref(cell));
                        } else {
                            new_map.insert(key, value.clone());
                        }
                        let new_hash = Value::hash_with_data(Value::hash_arc(new_map));
                        let meta = old_meta.unwrap_or(crate::runtime::ContainerTypeInfo {
                            value_type: "Any".to_string(),
                            key_type: None,
                            declared_type: None,
                        });
                        let new_hash = self.tag_container_metadata(new_hash, meta);
                        self.env_mut().insert(target_name.to_string(), new_hash);
                        if let Some((source_name, cell_val)) = bind_source_install {
                            self.set_env_with_main_alias(&source_name, cell_val.clone());
                            self.update_local_if_exists(code, &source_name, &cell_val);
                        }
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "bind-key",
                        );
                        self.stack.push(value);
                        return Ok(());
                    }
                    ValueView::Nil | ValueView::Package(_) => {
                        let key = args[0].to_string_value();
                        let value = args[1].clone();
                        let source_var = arg_sources
                            .as_ref()
                            .and_then(|s| s.get(1))
                            .and_then(|s| s.clone());
                        let mut new_map = std::collections::HashMap::new();
                        let mut bind_source_install: Option<(String, Value)> = None;
                        if let Some(var_name) = source_var {
                            let cell = match self.env().get(&var_name).map(Value::view) {
                                Some(ValueView::ContainerRef(cell)) => cell.clone(),
                                _ => {
                                    let cell = crate::gc::Gc::new(
                                        crate::value::ContainerCell::new(value.clone()),
                                    );
                                    bind_source_install =
                                        Some((var_name, Value::container_ref(cell.clone())));
                                    cell
                                }
                            };
                            new_map.insert(key, Value::container_ref(cell));
                        } else {
                            new_map.insert(key, value.clone());
                        }
                        self.env_mut().insert(
                            target_name.to_string(),
                            Value::hash_with_data(Value::hash_arc(new_map)),
                        );
                        if let Some((source_name, cell_val)) = bind_source_install {
                            self.set_env_with_main_alias(&source_name, cell_val.clone());
                            self.update_local_if_exists(code, &source_name, &cell_val);
                        }
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "bind-key",
                        );
                        self.stack.push(value);
                        return Ok(());
                    }
                    ValueView::Set(_, mutable) => {
                        let name = if mutable { "SetHash" } else { "Set" };
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "bind-key",
                        );
                        return Err(RuntimeError::bind(name));
                    }
                    ValueView::Bag(_, mutable) => {
                        let name = if mutable { "BagHash" } else { "Bag" };
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "bind-key",
                        );
                        return Err(RuntimeError::bind(name));
                    }
                    ValueView::Mix(_, mutable) => {
                        let name = if mutable { "MixHash" } else { "Mix" };
                        crate::vm::vm_stats::record_dispatch_entry_intercept(
                            "callmethodmut",
                            "bind-key",
                        );
                        return Err(RuntimeError::bind(name));
                    }
                    _ => {}
                }
            }
            // `$b.add(x)` / `$b.remove(x)`: the BagHash-only per-key count
            // mutators (semantics, and the rationale for mutating in place
            // through the shared node, live in `vm_baghash_mutators`). The
            // counts are already adjusted through the bag's own `Gc` node, so
            // the writeback below is NOT what makes the mutation visible -- it
            // re-seats the SAME (mutated) value in both halves of the dual
            // store so a later locals<->env sync cannot resurrect a stale
            // snapshot of the bag (`my %b is BagHash` reproduced exactly that).
            "add" | "remove" => {
                if let Some(receiver) =
                    crate::vm::vm_baghash_mutators::baghash_mutator_receiver(&target, &method)
                {
                    let result = crate::vm::vm_baghash_mutators::apply_baghash_mutator(
                        receiver, &method, &args,
                    )?;
                    if !target_name.is_empty() {
                        self.env_mut()
                            .insert(target_name.to_string(), target.clone());
                        self.update_local_if_exists(code, &target_name, &target);
                    }
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "baghash-add-remove",
                    );
                    self.stack.push(result);
                    return Ok(());
                }
            }
            // `@a.BIND-POS($i, $x)` binds element `$i` to the caller variable
            // `$x` as a shared `ContainerRef` cell — the array analog of
            // BIND-KEY above. A later `$x = ...` writes through to `@a[$i]` (and
            // vice versa). Only the single-index plain-`Array` case with a scalar
            // *variable* source is handled here; a literal source (no var) and
            // multi-dimensional BIND-POS fall through to the slow path, which
            // stores the immutable `Scalar` bind marker.
            "BIND-POS"
                if args.len() == 2
                    && matches!(target.view(), ValueView::Array(..))
                    && arg_sources
                        .as_ref()
                        .and_then(|s| s.get(1))
                        .and_then(|s| s.as_ref())
                        .is_some_and(|n| !n.contains('\0')) =>
            {
                // A natively typed array (`array[int]`) cannot hold a boxed
                // `ContainerRef` cell — BIND-POS on it must throw "Cannot bind to
                // a natively typed array". Detect it (a var bound to this same
                // backing Arc whose element type is native) and fall through to
                // the slow path, which raises that error.
                let is_native_array = if let ValueView::Array(items, ..) = target.view() {
                    let native_var =
                        self.env()
                            .iter()
                            .find_map(|(name, bound)| match bound.view() {
                                ValueView::Array(existing, ..)
                                    if crate::gc::Gc::ptr_eq(&existing, &items) =>
                                {
                                    Some(*name)
                                }
                                _ => None,
                            });
                    native_var.is_some_and(|name| {
                        self.var_type_constraint(&name.resolve())
                            .as_deref()
                            .is_some_and(crate::runtime::native_types::is_native_array_element_type)
                    })
                } else {
                    false
                };
                if !is_native_array
                    && let ValueView::Array(items, arr_kind) = target.view()
                    && let Some(i) = match args[0].view() {
                        ValueView::Int(n) if n >= 0 => Some(n as usize),
                        ValueView::Num(f) if f >= 0.0 => Some(f as usize),
                        _ => None,
                    }
                {
                    let source_var = arg_sources
                        .as_ref()
                        .and_then(|s| s.get(1))
                        .and_then(|s| s.clone())
                        .expect("arg_sources[1] present per match guard");
                    let value = args[1].clone();
                    // Reuse the source variable's existing cell when it is already
                    // cell-bound (so all aliases stay shared); otherwise install a
                    // fresh cell back into the source var.
                    let mut bind_source_install: Option<(String, Value)> = None;
                    let cell = match self.env().get(&source_var).map(Value::view) {
                        Some(ValueView::ContainerRef(cell)) => cell.clone(),
                        _ => {
                            let cell =
                                crate::gc::Gc::new(crate::value::ContainerCell::new(value.clone()));
                            bind_source_install =
                                Some((source_var, Value::container_ref(cell.clone())));
                            cell
                        }
                    };
                    let mut updated = items.to_vec();
                    if i >= updated.len() {
                        updated.resize(i + 1, Value::package(Symbol::intern("Any")));
                    }
                    updated[i] = Value::container_ref(cell);
                    let new_array = Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(updated)),
                        arr_kind,
                    );
                    self.env_mut().insert(target_name.to_string(), new_array);
                    if let Some((source_name, cell_val)) = bind_source_install {
                        self.set_env_with_main_alias(&source_name, cell_val.clone());
                        self.update_local_if_exists(code, &source_name, &cell_val);
                    }
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "bind-pos",
                    );
                    self.stack.push(value);
                    return Ok(());
                }
            }
            _ => {}
        }

        // Pre-dispatch Nil special cases — the same verdicts the scalar
        // `MethodCall` opcode reaches: warn-and-resume coercions (`$v.Int` /
        // `$v.Str` on a variable *bound* to Nil warn like `Nil.Int`) and
        // element-mutator errors. Everything else falls through to normal
        // dispatch, and the post-dispatch FALLBACK absorb in
        // `exec_call_method_mut_op` keeps handling genuinely-unknown methods.
        if modifier.is_none() && target.is_nil() {
            match crate::vm::vm_call_method_ops::nil_predispatch_verdict(&method, args.is_empty()) {
                Some(crate::vm::vm_call_method_ops::NilPredispatchVerdict::Error(err)) => {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "nil-predispatch",
                    );
                    return Err(err);
                }
                Some(crate::vm::vm_call_method_ops::NilPredispatchVerdict::Warn {
                    message,
                    resume,
                }) => {
                    crate::vm::vm_stats::record_dispatch_entry_intercept(
                        "callmethodmut",
                        "nil-predispatch",
                    );
                    let resumed = self.raise_resumable_warning(message, resume)?;
                    self.stack.push(resumed);
                    return Ok(());
                }
                None => {}
            }
        }
        // Auto-vivify undefined values (Nil, Any, Mu type objects) to empty Arrays
        // for mutating list methods. In Raku, calling push/unshift/append/prepend on
        // an undefined variable auto-vivifies it to an Array.
        let target = if matches!(method.as_str(), "push" | "unshift" | "append" | "prepend")
            && (target.is_nil()
                || matches!(
                    target.view(),
                    ValueView::Package(name) if matches!(name.resolve().as_str(), "Any" | "Mu" | "Array")
                )) {
            let empty_array = Value::real_array(vec![]);
            self.env_mut()
                .insert(target_name.to_string(), empty_array.clone());
            empty_array
        } else {
            target
        };
        // For .* and .+ modifiers, skip the single-dispatch call and go
        // directly to the all-methods-in-MRO path to avoid double execution.
        match modifier {
            Some("+") => {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmut",
                    "modifier-plus",
                );
                let vals =
                    self.call_method_all_with_fallback(&target, &method, &args, skip_native)?;
                self.stack.push(Value::array(vals));
            }
            Some("*") => {
                crate::vm::vm_stats::record_dispatch_entry_intercept(
                    "callmethodmut",
                    "modifier-star",
                );
                match self.call_method_all_with_fallback(&target, &method, &args, skip_native) {
                    Ok(vals) => self.stack.push(Value::array(vals)),
                    Err(e) if Self::is_method_not_found_error(&e) => {
                        self.stack.push(Value::array(vec![]))
                    }
                    Err(e) => return Err(e),
                }
            }
            _ => {
                // Native fast path for mutating list methods on a plain, untyped
                // `@`-array (ledger §1: native receiver dispatch -> Interpreter-native).
                // Handles the common hot-loop case directly in the Interpreter, writing the
                // mutated array back to env, instead of routing through the
                // tree-walking interpreter bridge. Falls through (returns None) for
                // typed/shaped/lazy/shared/constrained arrays so the interpreter
                // keeps owning those richer semantics.
                if modifier.is_none()
                    && let Some(result) =
                        self.try_native_array_mut(&target_name, &target, &method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
                    // ADR-0019 E6b step 1: shadow-verify the `Native` candidate
                    // (E5b step 1's template) at each of CallMethodMut's own
                    // native-probe completion shapes, observational only.
                    self.shadow_check_native_row_candidate(
                        &target,
                        &method,
                        method_sym,
                        args.len(),
                        true,
                    );
                    self.stack.push(result?);
                    return Ok(());
                }
                // Symmetric bound-cell fast path for hash mutators (`%h.push` /
                // `.append`) where `%h := %g` holds a shared `ContainerRef` cell.
                // The array mutator above descends the cell via
                // `env_root_descended_mut`; hashes need the same so the mutation
                // propagates to the bind source instead of detaching into the
                // receiver's own slot.
                if modifier.is_none()
                    && let Some(result) =
                        self.try_native_hash_mut_bound(&target_name, &method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
                    self.shadow_check_native_row_candidate(
                        &target,
                        &method,
                        method_sym,
                        args.len(),
                        true,
                    );
                    self.stack.push(result?);
                    return Ok(());
                }
                // Native fast path for the simple (non-erroring) forms of `splice`
                // on a plain, untyped `@`-array (ledger §1: native receiver
                // dispatch -> Interpreter-native).
                if modifier.is_none()
                    && let Some(result) =
                        self.try_native_array_splice(&target_name, &target, &method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
                    self.shadow_check_native_row_candidate(
                        &target,
                        &method,
                        method_sym,
                        args.len(),
                        true,
                    );
                    self.stack.push(result?);
                    return Ok(());
                }
                // Native fast path for mutating Buf write methods on a mutable Buf
                // instance (ledger §1: native receiver dispatch -> Interpreter-native).
                if modifier.is_none()
                    && let Some(result) =
                        self.try_native_buf_mut(&target_name, &target, &method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
                    self.shadow_check_native_row_candidate(
                        &target,
                        &method,
                        method_sym,
                        args.len(),
                        true,
                    );
                    self.stack.push(result?);
                    return Ok(());
                }
                // Native fast path for the Iterator protocol on a self-contained
                // array-backed iterator (ledger §1: native receiver dispatch ->
                // Interpreter-native). `$it.pull-one` etc. compile to CallMethodMut, so the
                // index-advancing dispatch lands here.
                if modifier.is_none()
                    && let Some(result) = self.try_native_iterator(&target, &method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
                    self.shadow_check_native_row_candidate(
                        &target,
                        &method,
                        method_sym,
                        args.len(),
                        true,
                    );
                    self.stack.push(result?);
                    return Ok(());
                }
                // Array-subclass instance delegation (mut path): when the Instance's
                // class inherits from Array, delegate mutating Array methods to the
                // backing __mutsu_array_storage attribute and write back.
                if let ValueView::Instance {
                    class_name: inst_class,
                    attributes,
                    id: inst_id,
                } = target.view()
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
                            // `.list` is `.List`'s lower-case sibling and was
                            // missing here, so `$v.list` on an `is Array`
                            // subclass wrapped the instance in a one-element
                            // list while the non-mut `CallMethod` path (which
                            // delegates unconditionally) returned the elements.
                            | "list"
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
                            // Numeric reductions / list folds over the elements.
                            | "min"
                            | "max"
                            | "minmax"
                            | "sum"
                            | "reduce"
                            | "produce"
                            // Index/element views.
                            | "kv"
                            | "pairs"
                            | "antipairs"
                            | "keys"
                            | "values"
                            // Grouping and combinatorics.
                            | "classify"
                            | "categorize"
                            | "combinations"
                            | "permutations"
                            | "rotor"
                            | "batch"
                            // Element selection (non-mutating).
                            | "pick"
                            | "roll"
                            // Junction constructors over the elements.
                            | "all"
                            | "any"
                            | "none"
                            | "one"
                    );
                    if is_array_method
                        && !self.has_user_method(&cn, &method)
                        && attributes.contains_key("__mutsu_array_storage")
                        && self
                            .mro_readonly(&cn)
                            .iter()
                            .any(|n| Self::is_positional_base(n))
                    {
                        let mut storage = attributes
                            .as_map()
                            .get("__mutsu_array_storage")
                            .cloned()
                            .unwrap_or(Value::real_array(Vec::new()));
                        // Interpreter-native fast path: simple mutators on the plain
                        // untyped backing array are performed in Rust and the
                        // updated storage written back, with no interpreter
                        // dispatch. Richer methods fall through below.
                        if let Some(result) =
                            Self::native_array_storage_mut(&mut storage, &method, &args)
                        {
                            let result = result?;
                            let updated_instance = self.write_back_array_storage_instance(
                                &target_name,
                                &inst_class,
                                &attributes,
                                inst_id,
                                storage,
                            );
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "native",
                            );
                            // ADR-0019 E6b step 1: shadow-check against the actual
                            // opcode receiver `target` (the Instance), not the
                            // backing `storage` value the Tier-A helper above was
                            // fed — `target` is what a real E6b cutover would
                            // query resolve_sequence with.
                            self.shadow_check_native_row_candidate(
                                &target,
                                &method,
                                method_sym,
                                args.len(),
                                true,
                            );
                            self.stack.push(
                                if matches!(
                                    method.as_str(),
                                    "push" | "append" | "prepend" | "unshift"
                                ) {
                                    updated_instance
                                } else {
                                    result
                                },
                            );
                            return Ok(());
                        }
                        // Non-mutating block list methods (`.map`/`.first`/
                        // `.minmax`) dispatch through the same native helpers a
                        // *plain* array uses on the backing storage, so an
                        // `is Array` instance gets the same VM-native coverage
                        // instead of bouncing to the tree-walk interpreter (ledger
                        // §D / §C Phase-3). They borrow `&storage` and return a
                        // fresh value, so they never mutate the instance. `.grep`
                        // returns rw views into the source (a `for @s.grep { $_++ }`
                        // writes back), and `.splice`/`ASSIGN-POS`/… mutate, so
                        // those keep the fallback — they need the first-class
                        // element-cell write-back the interpreter owns.
                        if let Some(r) = self.try_native_array_map(None, &storage, &method, &args) {
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "native",
                            );
                            self.shadow_check_native_row_candidate(
                                &target,
                                &method,
                                method_sym,
                                args.len(),
                                true,
                            );
                            self.stack.push(r?);
                            return Ok(());
                        }
                        if let Some(r) = self.try_native_first(&storage, &method, &args) {
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "native",
                            );
                            self.shadow_check_native_row_candidate(
                                &target,
                                &method,
                                method_sym,
                                args.len(),
                                true,
                            );
                            self.stack.push(r?);
                            return Ok(());
                        }
                        if let Some(r) = self.try_native_minmax(&storage, &method, &args) {
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "native",
                            );
                            self.shadow_check_native_row_candidate(
                                &target,
                                &method,
                                method_sym,
                                args.len(),
                                true,
                            );
                            self.stack.push(r?);
                            return Ok(());
                        }
                        // Other non-mutating, non-rw-view list methods
                        // (`.sort`/`.reverse`/`.unique`/`.elems`/…) go through the
                        // umbrella native dispatch on the backing storage. Gated to
                        // a whitelist of methods that return fresh values (never an
                        // rw view into, nor a mutation of, the source), so the
                        // by-value `&storage` borrow is correct for the instance.
                        if Self::is_array_storage_native_safe(&method)
                            && let Some(r) = self.try_native_method(&storage, method_sym, &args)
                        {
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "native",
                            );
                            self.shadow_check_native_row_candidate(
                                &target,
                                &method,
                                method_sym,
                                args.len(),
                                true,
                            );
                            self.stack.push(r?);
                            return Ok(());
                        }
                        // Perform the operation on the backing array
                        // TODO: compile to bytecode — Array-backed instance method
                        // (non-simple methods on `is Array` storage). See ledger §1.
                        crate::vm::vm_stats::record_method_fallback(&method);
                        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "user");
                        self.shadow_check_native_row_candidate(
                            &target,
                            &method,
                            method_sym,
                            args.len(),
                            false,
                        );
                        // Seed the synthetic binding with the current storage
                        // BEFORE dispatching: methods like `ASSIGN-POS`/`BIND-POS`/
                        // `DELETE-POS` mutate by scanning `self.env` for a binding
                        // whose Array Arc pointer identity matches the receiver
                        // (`overwrite_array_bindings_by_identity`) rather than
                        // returning an updated value through `target_var`. A real
                        // named `@a.ASSIGN-POS(...)` call works because `@a` is
                        // already bound in `self.env` with that same Arc; without
                        // this seed, `"__mutsu_array_tmp"` was never in `self.env`
                        // at call time, so the identity scan found nothing and the
                        // mutation silently no-op'd.
                        self.env_mut()
                            .insert("__mutsu_array_tmp".to_string(), storage.clone());
                        let result = loan_env!(
                            self,
                            call_method_mut_with_values(
                                "__mutsu_array_tmp",
                                storage.clone(),
                                &method,
                                args,
                            )
                        )
                        .or_else(|_| {
                            // Try non-mut dispatch for read-only methods
                            self.vm_call_method_with_values(storage.clone(), &method, vec![])
                        })?;
                        // Read back the (potentially mutated) storage
                        if let Some(updated_storage) = self.env().get("__mutsu_array_tmp").cloned()
                        {
                            storage = updated_storage;
                        }
                        self.env_mut().remove("__mutsu_array_tmp");
                        // Update the instance with the new storage
                        self.write_back_array_storage_instance(
                            &target_name,
                            &inst_class,
                            &attributes,
                            inst_id,
                            storage,
                        );
                        self.stack.push(result);
                        return Ok(());
                    }
                }
                // Hash-subclass instance delegation (mut path): the Associative
                // twin of the Array-subclass delegation block just above. See
                // `vm_hash_subclass_delegate.rs` for why this reuses the
                // existing native Hash dispatch (via a synthetic env binding)
                // instead of hand-written Rust mutators.
                if let Some(result) =
                    self.try_hash_storage_delegate_mut(&target_name, &target, &method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
                    self.shadow_check_native_row_candidate(
                        &target,
                        &method,
                        method_sym,
                        args.len(),
                        true,
                    );
                    self.stack.push(result?);
                    return Ok(());
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
                if !skip_native
                    && !self.native_lever_a_user_override(&target, &method)
                    && let Some(produced) = self.try_quanthash_weight_pair_producer(
                        &target,
                        &target_name,
                        &method,
                        &args,
                    )
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome(
                        "callmethodmut",
                        "quanthash-weight-pair-producer",
                    );
                    self.method_dispatch_pure = true;
                    self.stack.push(produced);
                    return Ok(());
                }
                // ADR-0036 slice 3 / ADR-0045 slice 4: `.pairs`/`.kv`/
                // `.antipairs`/`.values`/`.reverse`/`.sort` on a real mutable
                // container hand out the elements' own `Scalar` containers, not
                // clones. This must run BEFORE the sentinel-resolved copy below,
                // which is a fresh Hash and therefore has no identity to
                // promote into. `try_element_container_producer` declines every
                // receiver that must keep the snapshot producer.
                if !skip_native
                    // An `augment`ed native type's own `.sort`/`.pairs`/... must
                    // still win: this routing changes how a *native* producer
                    // builds its result, and there is no native producer to
                    // change when the user has replaced the method.
                    && !self.native_lever_a_user_override(&target, &method)
                    && let Some(produced) = self.try_element_container_producer(&target, &method, &args)
                {
                    crate::vm::vm_stats::record_dispatch_entry_outcome(
                        "callmethodmut",
                        "element-container-producer",
                    );
                    self.method_dispatch_pure = true;
                    self.stack.push(produced);
                    return Ok(());
                }
                let call_result = if !skip_native {
                    // Resolve hash sentinel entries (bound variable refs, self-refs)
                    // before passing to native methods that iterate hash values.
                    let effective_target = if let ValueView::Hash(items) = target.view() {
                        if Self::hash_has_sentinels(&items) {
                            Some(self.resolve_hash_for_iteration(&items))
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    let dispatch_target = effective_target.as_ref().unwrap_or(&target);
                    if let Some(native_result) =
                        self.try_native_method(dispatch_target, method_sym, &args)
                    {
                        // A native method reaching this tail returns a value and
                        // does not write the receiver back into env (mutating
                        // array/hash natives are handled by the dedicated
                        // writeback branches above and return early). So it is
                        // env-pure w.r.t. the caller -> no per-call locals pull.
                        self.method_dispatch_pure = true;
                        crate::vm::vm_stats::record_dispatch_entry_outcome(
                            "callmethodmut",
                            "native",
                        );
                        self.shadow_check_native_row_candidate(
                            dispatch_target,
                            &method,
                            method_sym,
                            args.len(),
                            true,
                        );
                        native_result
                    } else {
                        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "user");
                        self.shadow_check_native_row_candidate(
                            dispatch_target,
                            &method,
                            method_sym,
                            args.len(),
                            false,
                        );
                        self.try_compiled_method_mut_or_interpret_sym(
                            &target_name,
                            target,
                            method_sym,
                            args,
                        )
                    }
                } else {
                    // ADR-0019 E6b step 1: NOT shadow-checked here. `skip_native`
                    // means the arity cascade was deliberately never consulted for
                    // this call (a user-defined method override, a pseudo-method
                    // like WHAT/HOW, junction .gist, Stash AT-KEY, ...) -- there is
                    // no "did the cascade serve this call" outcome to compare the
                    // resolver's Native candidate against, unlike the genuine
                    // native/user completions above.
                    crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "user");
                    self.try_compiled_method_mut_or_interpret_sym(
                        &target_name,
                        target,
                        method_sym,
                        args,
                    )
                };
                match modifier {
                    Some("?") => match call_result {
                        Ok(val) => {
                            self.stack.push(val);
                        }
                        Err(e) if Self::is_method_not_found_error(&e) => {
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "notfound",
                            );
                            self.stack.push(Value::NIL);
                        }
                        Err(e) => return Err(e),
                    },
                    _ => {
                        if let Err(e) = &call_result
                            && Self::is_method_not_found_error(e)
                        {
                            crate::vm::vm_stats::record_dispatch_entry_outcome(
                                "callmethodmut",
                                "notfound",
                            );
                        }
                        self.stack.push(call_result?);
                    }
                }
            }
        }
        Ok(())
    }

    /// Interpreter-native mutating list methods (`append`/`prepend`/`unshift`/`pop`/`shift`)
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
    /// Is `name` a plain lexical `@array` variable (sigil `@` immediately
    /// followed by an identifier char), as opposed to an attribute (`@!x` /
    /// `@.x`), a dynamic (`@*x`), or other twigil'd form? Only plain lexicals
    /// have a single shared identity across threads, so only they may be routed
    /// through the name-keyed atomic shared store.
    pub(crate) fn is_plain_lexical_array_name(name: &str) -> bool {
        let mut bytes = name.bytes();
        bytes.next() == Some(b'@')
            && matches!(bytes.next(), Some(c) if c.is_ascii_alphabetic() || c == b'_')
    }

    /// Sigil-agnostic form of `is_plain_lexical_array_name`: a plain lexical
    /// container variable (`@name` / `%name`) whose second character is an
    /// identifier start — i.e. not a twigil'd attribute (`@!`, `%.`), dynamic
    /// (`@*`), or other special form. Used to gate atomic-shared-store routing
    /// (those non-plain forms share a name across instances and must not funnel
    /// into the global name-keyed store).
    pub(crate) fn is_plain_lexical_name(name: &str) -> bool {
        let mut bytes = name.bytes();
        matches!(bytes.next(), Some(b'@') | Some(b'%'))
            && matches!(bytes.next(), Some(c) if c.is_ascii_alphabetic() || c == b'_')
    }

    /// Bound-hash twin of `try_native_array_mut`: `$r.push((k => v))` /
    /// `$r.append(...)` where `my $r := %g` holds a shared `ContainerRef` cell.
    /// Only the bound case is intercepted — a plain hash has no cell to detach,
    /// so its existing interpreter writeback (into the receiver's slot) is
    /// already correct. Hash push/append semantics (existing-key value becomes a
    /// list) are non-trivial, so we delegate to the interpreter on the *inner*
    /// hash and write the result back through the cell, keeping every alias
    /// coherent.
    ///
    /// A `%`-SIGILED name is deliberately NOT intercepted: the interpreter's own
    /// `%`-arm (`runtime/methods_mut_dispatch.rs`) descends the cell itself now,
    /// and it is the only path carrying the richer semantics an intercept here
    /// would skip — the object-hash `.WHICH` key encoding with its
    /// `original_keys` record, the typed-hash key/value type checks, and the
    /// duplicate-key array-conflict check. Routing `%h` through the by-value
    /// implementation instead silently stringified an object hash's key and
    /// dropped the type check.
    fn try_native_hash_mut_bound(
        &mut self,
        target_name: &str,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(method, "push" | "append") || target_name.starts_with('%') {
            return None;
        }
        let cell = match self.env().get(target_name).map(Value::view) {
            Some(ValueView::ContainerRef(cell)) => cell.clone(),
            _ => return None,
        };
        let inner = cell.lock().unwrap().clone();
        if !matches!(inner.view(), ValueView::Hash(_)) {
            return None;
        }
        let result = match loan_env!(self, call_method_with_values(inner, method, args.to_vec())) {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };
        *cell.lock().unwrap() = result.clone();
        Some(Ok(result))
    }

    fn try_native_array_mut(
        &mut self,
        target_name: &str,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        if !matches!(
            method,
            "push" | "append" | "prepend" | "unshift" | "pop" | "shift"
        ) {
            return None;
        }
        // A plain `@`-sigiled variable whose value is a real `[...]` array
        // (ArrayKind::Array), excluding List/Item/Shaped/Lazy kinds — OR a scalar
        // variable bound to a whole array container (`my $r := @a`), which holds
        // a shared `ContainerRef` cell that `env_root_descended_mut` below
        // unwraps so the mutation still writes through the shared array.
        let is_bound_cell = matches!(
            self.env().get(target_name).map(Value::view),
            Some(ValueView::ContainerRef(_))
        );
        if (!target_name.starts_with('@') && !is_bound_cell)
            || !matches!(
                target.view(),
                ValueView::Array(_, crate::value::ArrayKind::Array)
            )
        {
            return None;
        }
        // Shared arrays keep their interior-mutation (Arc>1) semantics in the
        // interpreter so bound aliases observe the change; type-constrained or
        // metadata-bearing containers need element checks / typed empty Failures.
        // (Shared `push`/`unshift` are intercepted earlier by the shared-array
        // fast path in `exec_call_method_mut_op`.)
        //
        // A name this lineage RE-DECLARED is exempt: it is a frame-local
        // container that deliberately gets no shared-store lane, so the
        // shared-array fast path does not intercept it and the interpreter
        // fallback cannot reach it either — `box_decl_local_container_cell` put
        // a `ContainerRef` cell in the env entry and the fallback's plain
        // `env.get_mut(..).with_array_mut(..)` does not descend cells, so it
        // silently rebuilt a detached array (`my @a; @a.push("n");
        // @a.append(...)` inside a `start` block lost the append). This path
        // does descend, via `env_root_descended_mut`.
        //
        // ADR-0049 slice 4: also bail on a container carrying its own
        // `is default(...)` value. `decay_nil_vec_elements` below is
        // deliberately untyped-only (always decays a Nil arg to plain `Any`)
        // -- correct ONLY because this guard already routes every typed/
        // metadata-tagged target to the richer interpreter path. Without this
        // check an `is default(...)` array (which carries neither a type
        // constraint nor `container_type_metadata`, a separate side channel)
        // stayed on this fast path and silently stored a bare `Any` element
        // instead of the container's own default: `my @a is default(42) =
        // 1,2,3; @a.append(Nil)` stored `Any`, where both push (which has its
        // own dedicated opcode/fast path, already routed through
        // `assign_store_nil_default`) and real raku store `42`.
        if (self.shared_vars_active && !self.container_name_is_redeclared(target_name))
            || loan_env!(self, var_type_constraint(target_name)).is_some()
            || self.container_type_metadata(target).is_some()
            || self.container_default(target).is_some()
        {
            return None;
        }
        // pop/shift take no positionals; let the interpreter raise the arity error.
        if matches!(method, "pop" | "shift") && !args.is_empty() {
            return None;
        }
        // The receiver must be exactly the array currently bound to this name.
        // Container identity (§3): mutate through the SHARED backing node
        // (`gc_contents_mut`, no COW) so every by-value holder of the same
        // array — a `(0, @a)` capture, an element holding the array — observes
        // the mutation. Descend through a whole-container `:=` bound cell
        // (`my @x := @a`) so the mutation writes through the shared cell.
        // `env_root_descended_mut` itself prefers a compunit unit-lexical
        // container (ADR-0039 slice 1) over the raw `env` entry when
        // `target_name` names one, so a module's/mainline-sub's own `@items`
        // is never confused with the loading scope's same-named `env` entry.
        // ADR-0049 slice 3: precompute the (possibly Nil-decaying) argument
        // list BEFORE taking the mutable borrow into `self`'s env below --
        // `decay_nil_vec_elements` needs `&mut self`, which the
        // `with_array_mut` closure below cannot borrow (it is already
        // running inside `self.env_root_descended_mut(..)`'s mutable
        // borrow). This whole function bailed out above for any typed or
        // metadata-tagged target, so the result is always the untyped `Any`
        // default -- exactly what the old hardcoded `nil_elems_to_any` call
        // produced here, now sourced from the one shared decay helper.
        // ADR-0040 slice 1: itemize per element, after the one-arg-rule
        // flattening decision (and after Nil-decay), so a single pushed
        // aggregate becomes one itemized element.
        let mut precomputed_args = match method {
            // `@a.push`/`.unshift` compile to `ArrayPush`/(unshift opcode)
            // only for a single-arg call on a *local* array; the
            // captured-closure and multi-arg forms reach here as
            // `CallMethodMut`. Mirror the opcode's env-bound branch
            // (`normalize_push_unshift_args` then extend/insert).
            "push" | "unshift" => Some(
                self.decay_nil_vec_elements(
                    crate::runtime::Interpreter::normalize_push_unshift_args(args.to_vec()),
                )
                .into_iter()
                .map(Self::itemize_value)
                .collect::<Vec<_>>(),
            ),
            "append" | "prepend" => Some(
                self.decay_nil_vec_elements(crate::runtime::flatten_append_args(args.to_vec()))
                    .into_iter()
                    .map(Self::itemize_value)
                    .collect::<Vec<_>>(),
            ),
            _ => None,
        };
        let result = self.env_root_descended_mut(target_name)?.with_array_mut(
            move |arc_items, kind| {
                if !matches!(*kind, crate::value::ArrayKind::Array) {
                    return None;
                }
                // SAFETY: audited aliased in-place container write (see
                // value::aliased_mut); no other borrow into this node is
                // live across the mutation below.
                let items = unsafe { crate::value::gc_contents_mut(arc_items) };
                Some(match method {
                    "push" => {
                        let norm = precomputed_args.take().expect("precomputed for push above");
                        items.extend(norm);
                        Value::array_with_kind(
                            crate::gc::Gc::clone(arc_items),
                            crate::value::ArrayKind::Array,
                        )
                    }
                    "append" | "prepend" => {
                        let flat = precomputed_args
                            .take()
                            .expect("precomputed for append/prepend above");
                        if method == "append" {
                            items.extend(flat);
                        } else {
                            for (i, v) in flat.into_iter().enumerate() {
                                items.insert(i, v);
                            }
                        }
                        Value::array_with_kind(
                            crate::gc::Gc::clone(arc_items),
                            crate::value::ArrayKind::Array,
                        )
                    }
                    "unshift" => {
                        let norm = precomputed_args
                            .take()
                            .expect("precomputed for unshift above");
                        for (i, v) in norm.into_iter().enumerate() {
                            items.insert(i, v);
                        }
                        Value::array_with_kind(
                            crate::gc::Gc::clone(arc_items),
                            crate::value::ArrayKind::Array,
                        )
                    }
                    "pop" => {
                        if items.is_empty() {
                            crate::runtime::utils::make_empty_array_failure_what("pop", "Array")
                        } else {
                            items.pop().unwrap_or(Value::NIL)
                        }
                    }
                    "shift" => {
                        if items.is_empty() {
                            crate::runtime::utils::make_empty_array_failure_what("shift", "Array")
                        } else {
                            items.remove(0)
                        }
                    }
                    _ => unreachable!(),
                })
            },
        )??;
        Some(Ok(result))
    }

    /// Interpreter-native simple array mutators (push/pop/shift/unshift/append/prepend)
    /// applied directly to an `is Array`-backed instance's backing storage
    /// `Value` (ledger §1: array-backed instance dispatch -> Interpreter-native).
    ///
    /// Mirrors the interpreter's plain, non-shared env-keyed mutator branch
    /// (`methods_mut.rs`): the `__mutsu_array_storage` value is a plain untyped
    /// `real_array`, so `push`/`append`/`unshift`/`prepend` extend/insert the
    /// normalized arguments and `pop`/`shift` remove an element (returning a
    /// typed empty Failure when empty). `storage` is mutated in place and the
    /// method's result value is returned. Returns `None` (fall through to the
    /// interpreter) for any other method, a non-plain `ArrayKind`, or an
    /// arity-erroring `pop`/`shift` so the interpreter owns the richer cases.
    /// Non-mutating, non-rw-view list methods that are safe to dispatch on an
    /// `is Array` instance's backing storage via `try_native_method` (which
    /// borrows the storage immutably and returns a fresh value). Excludes:
    /// `map`/`first`/`minmax` (handled by their own helpers above), `grep` (its
    /// result shares rw element cells with the source — `for @s.grep { $_++ }`
    /// must write back), and the mutating methods (`splice`/`ASSIGN-POS`/
    /// `DELETE-POS`/`BIND-POS`), which must update the instance via the fallback.
    fn is_array_storage_native_safe(method: &str) -> bool {
        matches!(
            method,
            "sort"
                | "reverse"
                | "rotate"
                | "unique"
                | "squish"
                | "flat"
                | "join"
                | "elems"
                | "end"
                | "List"
                | "Array"
                | "Seq"
                | "Slip"
                | "AT-POS"
                | "EXISTS-POS"
                | "head"
                | "tail"
        )
    }

    /// `pub(crate)`: also reused by the `nextsame`/`callsame` synthesized native
    /// fallback (`native_array_storage_next_candidate` in
    /// `runtime/builtins_dispatch_next.rs`) so a deferred call from a user
    /// override reaches the same mutation as the direct `$a.push(...)` path,
    /// instead of silently no-op'ing through the non-mutating
    /// `try_native_method` dispatch.
    pub(crate) fn native_array_storage_mut(
        storage: &mut Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let result = storage.with_array_mut(|arc_items, kind| {
            if !matches!(*kind, crate::value::ArrayKind::Array) {
                return None;
            }
            Some(match method {
                // ADR-0040 slice 1: itemize per element, after the
                // one-arg-rule flattening decision.
                "push" => {
                    let norm =
                        crate::runtime::Interpreter::normalize_push_unshift_args(args.to_vec())
                            .into_iter()
                            .map(crate::runtime::Interpreter::itemize_value)
                            .collect::<Vec<_>>();
                    crate::gc::Gc::make_mut(arc_items).extend(norm);
                    Value::array_with_kind(
                        crate::gc::Gc::clone(arc_items),
                        crate::value::ArrayKind::Array,
                    )
                }
                "append" => {
                    let flat = crate::runtime::flatten_append_args(args.to_vec())
                        .into_iter()
                        .map(crate::runtime::Interpreter::itemize_value)
                        .collect::<Vec<_>>();
                    crate::gc::Gc::make_mut(arc_items).extend(flat);
                    Value::array_with_kind(
                        crate::gc::Gc::clone(arc_items),
                        crate::value::ArrayKind::Array,
                    )
                }
                "unshift" => {
                    let norm =
                        crate::runtime::Interpreter::normalize_push_unshift_args(args.to_vec())
                            .into_iter()
                            .map(crate::runtime::Interpreter::itemize_value)
                            .collect::<Vec<_>>();
                    let items = crate::gc::Gc::make_mut(arc_items);
                    for (i, v) in norm.into_iter().enumerate() {
                        items.insert(i, v);
                    }
                    Value::array_with_kind(
                        crate::gc::Gc::clone(arc_items),
                        crate::value::ArrayKind::Array,
                    )
                }
                "prepend" => {
                    let flat = crate::runtime::flatten_append_args(args.to_vec())
                        .into_iter()
                        .map(crate::runtime::Interpreter::itemize_value)
                        .collect::<Vec<_>>();
                    let items = crate::gc::Gc::make_mut(arc_items);
                    for (i, v) in flat.into_iter().enumerate() {
                        items.insert(i, v);
                    }
                    Value::array_with_kind(
                        crate::gc::Gc::clone(arc_items),
                        crate::value::ArrayKind::Array,
                    )
                }
                "pop" => {
                    if !args.is_empty() {
                        return None;
                    }
                    if arc_items.is_empty() {
                        crate::runtime::utils::make_empty_array_failure_what("pop", "Array")
                    } else {
                        crate::gc::Gc::make_mut(arc_items)
                            .pop()
                            .unwrap_or(Value::NIL)
                    }
                }
                "shift" => {
                    if !args.is_empty() {
                        return None;
                    }
                    if arc_items.is_empty() {
                        crate::runtime::utils::make_empty_array_failure_what("shift", "Array")
                    } else {
                        crate::gc::Gc::make_mut(arc_items).remove(0)
                    }
                }
                _ => return None,
            })
        })??;
        Some(Ok(result))
    }

    /// Rebuild an `is Array`-backed instance with its `__mutsu_array_storage`
    /// attribute replaced by `storage` and write it back into `target_name`.
    /// Shared by the Interpreter-native mutator fast path and the interpreter fallback.
    fn write_back_array_storage_instance(
        &mut self,
        target_name: &str,
        inst_class: &Symbol,
        attributes: &crate::gc::Gc<crate::value::InstanceAttrs>,
        inst_id: u64,
        storage: Value,
    ) -> Value {
        let new_attrs = crate::value::InstanceAttrs::clone(attributes);
        new_attrs.insert("__mutsu_array_storage".to_string(), storage);
        let updated_instance = Value::instance_parts(
            *inst_class,
            crate::gc::Gc::new(crate::value::InstanceAttrs::new(
                *inst_class,
                new_attrs.to_map(),
                inst_id,
                true,
            )),
            inst_id,
        );
        self.env_mut()
            .insert(target_name.to_string(), updated_instance.clone());
        updated_instance
    }

    /// Interpreter-native `splice` on a plain, untyped `@`-array bound to `target_name`
    /// (ledger §1: native receiver dispatch -> Interpreter-native). Mirrors the
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
        // A plain `@`-sigiled variable whose value is a real `[...]` array
        // (ArrayKind::Array), excluding List/Item/Shaped/Lazy — OR a scalar bound
        // to a whole array container (`my $r := @a`), unwrapped via
        // `env_root_descended_mut` below.
        let is_bound_cell = matches!(
            self.env().get(target_name).map(Value::view),
            Some(ValueView::ContainerRef(_))
        );
        if (!target_name.starts_with('@') && !is_bound_cell)
            || !matches!(
                target.view(),
                ValueView::Array(_, crate::value::ArrayKind::Array)
            )
        {
            return None;
        }
        // Shared / type-constrained / metadata-bearing containers need the
        // interpreter's element checks, native-array semantics, and identity
        // sharing; let it own those.
        if self.shared_vars_active
            || loan_env!(self, var_type_constraint(target_name)).is_some()
            || self.container_type_metadata(target).is_some()
        {
            return None;
        }
        // Offset (arg 0) and count (arg 1): plain non-negative `Int`, or absent.
        // Anything else (Whatever/Str/Num/Callable) goes to the interpreter,
        // which also owns the `X::OutOfRange` error for a negative offset/count.
        let raw_start = match args.first().map(Value::view) {
            None => None,
            Some(ValueView::Int(i)) if i >= 0 => Some(i as usize),
            _ => return None,
        };
        let raw_count = match args.get(1).map(Value::view) {
            None => None,
            Some(ValueView::Int(i)) if i >= 0 => Some(i as usize),
            _ => return None,
        };
        // Replacement values (args[2..]): reject lazy values (the interpreter
        // raises `X::Cannot::Lazy`), then apply splice's one-arg rule /
        // itemization / Nil decay through the single shared helper the
        // interpreter's `do_splice` uses, so the two paths cannot diverge.
        let post = args.get(2..).unwrap_or(&[]);
        for arg in post {
            let lazy = match arg.view() {
                ValueView::Array(arr, ..) => {
                    arr.iter().any(crate::builtins::methods_0arg::is_value_lazy)
                }
                _ => crate::builtins::methods_0arg::is_value_lazy(arg),
            };
            if lazy {
                return None;
            }
        }
        let replacement = crate::runtime::flatten_splice_replacement_args(post);
        // The receiver must be exactly the array currently bound to this name.
        // Container identity (§3): splice through the SHARED backing node (no
        // COW) so by-value holders of the same array observe it. Compute the
        // splice bounds from the live binding's length (not `target`). Descend
        // through a whole-container `:=` bound cell so the splice writes
        // through the cell.
        let removed =
            self.env_root_descended_mut(target_name)?
                .with_array_mut(|arc_items, kind| {
                    if !matches!(*kind, crate::value::ArrayKind::Array) {
                        return None;
                    }
                    let len = arc_items.len();
                    let start = raw_start.unwrap_or(0);
                    // An offset past the end is `X::OutOfRange` in the interpreter.
                    if start > len {
                        return None;
                    }
                    let count = raw_count.unwrap_or(len - start);
                    let end = (start + count).min(len);
                    // SAFETY: audited aliased in-place container write (see
                    // value::aliased_mut); no other borrow into this node is
                    // live across the mutation below.
                    let items = unsafe { crate::value::gc_contents_mut(arc_items) };
                    let removed: Vec<Value> = items.drain(start..end).collect();
                    for (i, item) in replacement.into_iter().enumerate() {
                        items.insert(start + i, item);
                    }
                    Some(removed)
                })??;
        Some(Ok(Value::real_array(removed)))
    }

    /// Interpreter-native mutating Buf write methods (`write-bits`/`write-ubits`/
    /// `write-num*`/`write-int*`/`write-uint*`) on a mutable `Buf` instance bound
    /// to `target_name` (ledger §1: native receiver dispatch -> Interpreter-native). Mirrors
    /// the interpreter's instance-mutate branches in `methods_mut.rs` exactly: the
    /// byte transforms are the single shared pure implementations in `builtins/`
    /// (`buf_bits`/`buf_write_num`/`buf_write_int`), and the writeback goes
    /// straight into the receiver's shared cell (`Value::write_back_sharing`) so
    /// aliases of the same buf observe the mutation — so the result is
    /// behavior-invariant.
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
        let ValueView::Instance {
            class_name,
            attributes,
            id,
        } = target.view()
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
        let mut bytes = crate::value::value_buf::buf_raw_bytes_or_empty(&attributes);
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
            let offset_i64 = match args[0].view() {
                ValueView::Int(i) => i,
                ValueView::Num(f) => f as i64,
                _ => 0,
            };
            let endian_val = if args.len() == 3 {
                crate::builtins::buf_write_num::decode_endian(&args[2])
            } else {
                0
            };
            let width = crate::value::value_buf::buf_elem_width(&cn);
            let res = if is_write_num {
                crate::builtins::buf_write_num::apply_write_num(
                    &mut bytes, method, offset_i64, &args[1], endian_val, width,
                )
            } else {
                crate::builtins::buf_write_int::apply_write_int(
                    &mut bytes, method, offset_i64, &args[1], endian_val, width,
                )
            };
            if let Err(e) = res {
                return Some(Err(e));
            }
            bytes
        };
        // Write the updated bytes straight into the receiver's live shared cell
        // (so aliases observing the same buf see the mutation), then refresh the
        // receiver binding to match the interpreter's `env.insert(target_var, ...)`.
        let mut updated_attrs = attributes.to_map();
        crate::value::value_buf::set_buf_raw_bytes(&mut updated_attrs, class_name, new_bytes);
        let updated = Value::write_back_sharing(&attributes, class_name, updated_attrs, id);
        self.env_mut()
            .insert(target_name.to_string(), updated.clone());
        Some(Ok(updated))
    }
}
