use super::*;
use crate::symbol::Symbol;
use std::sync::Arc;

impl Interpreter {
    /// Build an X::Multi::NoMatch error for a `print`/`say`/`put`/`note` call
    /// made with a positional argument on an invocant whose only candidate is
    /// the default `(Mu: *%_)` one. The `capture` attribute carries the full
    /// call capture (invocant + arguments) so a `capture => { ... }` matcher
    /// can inspect it (e.g. `$_[0] ~~ Failure`).
    pub(super) fn print_routine_no_match_error(
        method: &str,
        target: &Value,
        args: &[Value],
    ) -> RuntimeError {
        let invocant_type = crate::value::types::what_type_name(target);
        let arg_profile: Vec<String> = args
            .iter()
            .filter(|a| !matches!(a, Value::Pair(..) | Value::ValuePair(..)))
            .map(|a| {
                let tn = crate::value::types::what_type_name(a);
                if matches!(a, Value::Nil) {
                    tn
                } else {
                    format!("{}:D", tn)
                }
            })
            .collect();
        let message = format!(
            "Cannot resolve caller {}({}:D: {}); none of these signatures matches:\n    (Mu: *%_)",
            method,
            invocant_type,
            arg_profile.join(", ")
        );
        let mut positional = vec![target.clone()];
        for a in args {
            if !matches!(a, Value::Pair(..) | Value::ValuePair(..)) {
                positional.push(a.clone());
            }
        }
        let capture = Value::capture(positional, std::collections::HashMap::new());
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message.clone()));
        attrs.insert("capture".to_string(), capture);
        let mut err = RuntimeError::new(message);
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Multi::NoMatch"),
            attrs,
        )));
        err
    }

    /// Fast path for a 0-arg public attribute-accessor read on an Instance
    /// (`$obj.x`). Returns `Some(value)` to push when it handled the call, or
    /// `None` to fall through to full method dispatch.
    ///
    /// This is a *pure read*: it never mutates the invocant and needs no
    /// invocant write-back, so it is valid from both the non-mut (`CallMethod`)
    /// and mut (`CallMethodMut`) opcodes. `$obj.x` on a *variable* compiles to
    /// `CallMethodMut` (for potential invocant write-back), so without this
    /// shared helper every accessor read on a lexical fell back to the
    /// interpreter -- the dominant method fallback in `method-call.raku`
    /// (`x`/`y` = 20000 fallbacks). See docs/vm-decoupling.md.
    ///
    /// Restricted to scalar attribute values: an `@.x` / `%.x` accessor must
    /// register container type metadata on the returned value (so later typed
    /// push/insert is enforced), which only the interpreter accessor path does,
    /// so Array/Hash values fall through.
    pub(super) fn try_fast_accessor_read(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
        has_modifier: bool,
        quoted: bool,
    ) -> Option<Value> {
        if !args.is_empty() || has_modifier || quoted {
            return None;
        }
        let Value::Instance {
            attributes,
            class_name,
            ..
        } = target
        else {
            return None;
        };
        if matches!(
            method,
            "new"
                | "BUILD"
                | "TWEAK"
                | "BUILDALL"
                | "DESTROY"
                | "Bool"
                | "so"
                | "not"
                | "defined"
                | "DEFINITE"
                | "WHAT"
                | "WHO"
                | "HOW"
                | "WHY"
                | "WHICH"
                | "WHERE"
                | "VAR"
                | "Str"
                | "gist"
                | "raku"
                | "perl"
                | "ACCEPTS"
                | "isa"
                | "does"
                | "can"
                | "^name"
                | "^mro"
                | "^methods"
                | "^attributes"
                | "sink"
                | "self"
                | "clone"
                | "return"
                | "handled"
                | "bytes"
        ) {
            return None;
        }
        let cn = class_name.resolve();
        if self.has_user_method(&cn, method) {
            return None;
        }
        // Only a *public* accessor reads through the fast path. Gating on this
        // first is essential: a private attribute (`has $!secret`) is stored
        // under the `secret!` key, so reading it by the bare name must fall
        // through to the interpreter (which denies the access) rather than
        // leaking the private value.
        if !self.has_public_accessor(&cn, method) {
            return None;
        }
        // Public accessor confirmed. Read its backing value, stored under the
        // public name or the `!`-suffixed private storage name. This mirrors the
        // long-standing non-mut fast path (which returned the value regardless of
        // type); extending it to the mut path means an accessor read on a
        // *variable* (`$obj.x`, compiled as CallMethodMut) no longer falls back
        // to the interpreter.
        let map = attributes.as_map();
        let priv_key = format!("{}!", method);
        let val = map.get(method).or_else(|| map.get(&priv_key));
        match val {
            Some(v) => {
                let out = v.clone();
                if let Some(msg) = self.class_attribute_deprecated(&cn, method) {
                    loan_env!(self, check_deprecation_for_method(method, &cn, &msg));
                }
                Some(out)
            }
            // Public accessor exists but the attribute is unset.
            None => Some(Value::Nil),
        }
    }

    pub(super) fn exec_call_method_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        arity: u32,
        modifier_idx: Option<u32>,
        quoted: bool,
        arg_sources_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_method_dispatch();
        let arg_sources = self.decode_arg_sources(code, arg_sources_idx);
        self.set_pending_call_arg_sources(arg_sources.clone());
        let method_raw = Self::const_str(code, name_idx);
        let modifier = modifier_idx.map(|idx| Self::const_str(code, idx));
        let method_cow = Self::rewrite_method_name_cow(method_raw, modifier);
        let method = &*method_cow;
        let arity = arity as usize;
        if self.stack.len() < arity + 1 {
            return Err(RuntimeError::new(
                "Interpreter stack underflow in CallMethod",
            ));
        }
        let start = self.stack.len() - arity;
        let raw_args: Vec<Value> = self.stack.drain(start..).collect();
        let args = if raw_args.iter().any(|a| matches!(a, Value::Slip(_))) {
            let preserve_empty_slip = Self::preserve_empty_slip_arg(method);
            let mut args = Vec::new();
            for arg in raw_args {
                Self::append_flattened_call_arg(&mut args, arg, preserve_empty_slip);
            }
            args
        } else {
            raw_args
        };
        let target = self.stack.pop().ok_or_else(|| {
            RuntimeError::new("Interpreter stack underflow in CallMethod target".to_string())
        })?;
        // Force LazyIoLines into an eager array when calling methods on it,
        // unless the method preserves laziness (e.g., .kv).
        let target = if matches!(&target, Value::LazyIoLines { .. })
            && !matches!(method, "kv" | "iterator" | "lazy")
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
        // `.throw`/`.rethrow` on an exception instance: attach a backtrace built
        // from the current call stack. The `die`/`fail` opcodes do this, but an
        // explicit `ExceptionObject.throw` goes through method dispatch and would
        // otherwise carry no frames (so `.backtrace.list` would be empty).
        let target = if matches!(method, "throw" | "rethrow")
            && args.is_empty()
            && matches!(
                &target,
                Value::Instance { class_name, attributes, .. }
                    if {
                        let cn = class_name.resolve();
                        (cn == "Exception" || cn.starts_with("X::") || cn.starts_with("CX::"))
                            && !attributes.as_map().contains_key("backtrace")
                    }
            ) {
            if let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
            {
                let backtrace_val = self.build_backtrace_value();
                let mut new_attrs = attributes.as_map().clone();
                new_attrs.insert("backtrace".to_string(), backtrace_val);
                if let Some(line) = self.current_source_line() {
                    new_attrs
                        .entry("line".to_string())
                        .or_insert_with(|| Value::Int(line as i64));
                }
                if let Some(file) = self.current_source_file() {
                    new_attrs
                        .entry("file".to_string())
                        .or_insert_with(|| Value::str_from(&file));
                }
                Value::make_instance(*class_name, new_attrs)
            } else {
                target
            }
        } else {
            target
        };
        // Autovivify a typed array element when a mutating array method is called
        // on its (undefined) type object, e.g. `my Array of Int @x; @x[0].push(3)`
        // reads `@x[0]` as the `Array[Int]` type object — pushing builds a fresh
        // typed `Array[Int]` whose elements are type-checked (so a later
        // `@x[0].push('foo')` still dies), and the result is written back to the slot.
        if let Value::Package(type_name) = &target
            && matches!(method, "push" | "append" | "unshift" | "prepend")
            && let Some(result) = self.autoviv_typed_array_push(&type_name.resolve(), &args)?
        {
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }
        // .emit on any value: push to supply emit buffer if inside a supply
        // block, otherwise raise CX::Emit. Skip for Supplier instances (they
        // have their own emit method).
        if method == "emit"
            && args.is_empty()
            && !matches!(
                &target,
                Value::Instance { class_name, .. } if class_name == "Supplier"
            )
        {
            if let Some(buf) = self.supply_emit_buffer.last_mut() {
                buf.push(target);
                self.stack.push(Value::Nil);
                // Buffering into the supply emit buffer touches no env: no mark.
                return Ok(());
            }
            return Err(RuntimeError::emit_signal(target));
        }
        // Fast path: 0-arg attribute accessor on Instance (e.g. $obj.x). This is
        // a pure read that touches no env, so it runs safely under a scoped
        // overlay env (the common `$.attr` read inside a scoped method body) --
        // hence the `flatten_scoped_env` below is placed *after* it, so an
        // accessor read does not collapse the caller's scoped overlay.
        if let Some(val) =
            self.try_fast_accessor_read(&target, method, &args, modifier_idx.is_some(), quoted)
        {
            // Pure attribute read: touches no env (see comment above), so it does
            // not dirty the caller's locals (Slice 6.3 — removes the per-accessor
            // env->locals pull that dominated method-heavy code like bench-class).
            self.stack.push(val);
            return Ok(());
        }
        // `.so` / `.not` on a value whose type defines a user `Bool` method must
        // dispatch through that method (Mu.so / Mu.not are defined in terms of
        // .Bool) rather than the native truthiness fast path, which is unaware of
        // user-defined Bool.
        if matches!(method, "so" | "not") && args.is_empty() {
            let user_bool_owner = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = user_bool_owner
                && loan_env!(self, resolve_method_with_owner(&cn, "Bool", &[])).is_some()
            {
                let t = self.eval_truthy(&target);
                self.stack
                    .push(Value::Bool(if method == "not" { !t } else { t }));
                self.env_dirty = true;
                return Ok(());
            }
        }
        // Full method dispatch from here on may capture the env into a Sub /
        // closure or run an interpreter fallback that iterates it; collapse a
        // transient scoped overlay env to a flat env first so the full lexical
        // view is seen.
        self.flatten_scoped_env();
        // Junction auto-threading: thread method calls over junction values
        if let Value::Junction { kind, values } = &target
            && !matches!(
                method,
                "Bool"
                    | "so"
                    | "WHAT"
                    | "WHICH"
                    | "^name"
                    | "gist"
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
                    self.maybe_autothread_method_args(v, method, &args)?
                {
                    threaded
                } else if let Some(nr) = self.try_native_method(v, Symbol::intern(method), &args) {
                    nr?
                } else {
                    self.try_compiled_method_or_interpret(v.clone(), method, args.clone())?
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
        if let Some(result) = self.maybe_autothread_method_args(&target, method, &args)? {
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
            // Reads/clears global deprecation state, not env: no mark.
            return Ok(());
        }

        // Fast path for Lock::Async.protect — execute block inline in current Interpreter
        if method == "protect"
            && args.len() == 1
            && let Value::Instance {
                class_name,
                attributes,
                ..
            } = &target
            && (class_name.resolve() == "Lock::Async" || class_name.resolve() == "Lock")
        {
            let lock_id = match attributes.as_map().get("lock-id") {
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
                    method,
                    "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
                ));
        let is_junction_target = match &target {
            Value::Junction { .. } => true,
            Value::Scalar(inner) => matches!(inner.as_ref(), Value::Junction { .. }),
            _ => false,
        };
        if matches!(method, "gist" | "raku" | "perl") && is_junction_target {
            skip_native = true;
        }
        // Also skip native if the target has a user-defined method with this name,
        // but NOT for pseudo-methods like DEFINITE, WHAT, etc. which are macros.
        if !skip_native
            && !matches!(
                method,
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            )
        {
            let class_name = match &target {
                Value::Instance { class_name, .. } => Some(class_name.resolve()),
                Value::Package(name) => Some(name.resolve()),
                _ => None,
            };
            if let Some(cn) = class_name
                && self.has_user_method(&cn, method)
            {
                skip_native = true;
            }
        }
        if !skip_native
            && matches!(method, "AT-KEY" | "keys")
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "Stash")
        {
            skip_native = true;
        }
        if !skip_native
            && matches!(&target, Value::Instance { class_name, .. } if class_name == "Proc::Async")
            && matches!(
                method,
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
                method,
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
                method,
                "DEFINITE" | "WHAT" | "WHO" | "HOW" | "WHY" | "WHICH" | "WHERE" | "VAR"
            )
        {
            self.skip_pseudo_method_native = Some(method.to_string());
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
                method,
                "VAR" | "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "REPR" | "DEFINITE"
            ) {
            let has_subclass_attr = if let Some((_, attrs)) = subclass {
                attrs.lock().unwrap().contains_key(method)
            } else {
                false
            };
            if has_subclass_attr {
                target
            } else {
                self.auto_fetch_proxy(&target)?
            }
        } else {
            target
        };
        // gist/Str/raku of a *genuinely* lazy list (an infinite sequence or a
        // lazy map/grep pipeline) renders as raku's placeholder rather than
        // reifying the (possibly infinite) sequence: `(...)` for gist/raku,
        // `...` for Str. An eager gather has neither `sequence_spec` nor
        // `lazy_pipe` and is forced & rendered normally below.
        if let Value::LazyList(ref ll) = target
            && matches!(method, "gist" | "Str" | "raku" | "perl")
            && (ll.lazy_pipe.is_some() || ll.sequence_spec.is_some())
        {
            self.stack.push(Value::str(
                if method == "Str" { "..." } else { "(...)" }.to_string(),
            ));
            return Ok(());
        }
        // Force gather-sourced LazyList before method dispatch for methods that need
        // element access. This must happen before the native method fast path, because
        // builtins don't have access to the interpreter for forcing.
        // Non-gather LazyLists (e.g. from infinite ranges) are NOT forced here — they
        // go through builtins which may return Failure for methods like .elems.
        // Exception: .List and .values on coroutine-equipped gathers preserve laziness.
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
            && Self::lazy_list_needs_forcing(method)
            && !(ll.coroutine.is_some() && matches!(method, "List" | "values"))
            // A chained `.map`/`.grep` on a lazy pipeline appends another stage
            // (interpreter dispatch); laziness-preserving coercions return the
            // pipeline unchanged (native dispatch) — neither forces.
            && !(ll.lazy_pipe.is_some()
                && (matches!(method, "map" | "grep") || Self::lazy_pipe_preserving_coercion(method)))
        {
            let saved_env = self.env().clone();
            // `.head(n)` only needs the first `n` elements: pull them lazily so
            // an infinite gather does not hang.
            let items = match Self::gather_head_bound(method, &args) {
                Some(n) => self.force_lazy_list_vm_n(ll, n)?,
                // A strict force of an infinite lazy pipeline cannot terminate:
                // raise X::Cannot::Lazy with this method's name.
                None if ll.lazy_pipe.is_some() => {
                    return Err(RuntimeError::cannot_lazy(method));
                }
                None => self.force_lazy_list_vm(ll)?,
            };
            // Restoring the pre-force env undoes captured-variable corruption
            // from gather coroutine forcing. A lazy map/grep pipeline runs its
            // callback via `vm_call_on_value` in this Interpreter, so its side effects on
            // enclosing variables are legitimate and must persist.
            if !matches!(method, "elems" | "hyper" | "race") && ll.lazy_pipe.is_none() {
                *self.env_mut() = saved_env;
            }
            Value::Seq(std::sync::Arc::new(items))
        } else {
            target
        };
        // .hyper/.race with named arguments (batch, degree): validate and wrap
        if matches!(method, "hyper" | "race") && !args.is_empty() {
            // Extract named args (batch, degree) and validate
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
            // Validate batch
            if let Some(b) = batch
                && b <= 0
            {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("method".to_string(), Value::str(method.to_string()));
                attrs.insert("name".to_string(), Value::str("batch".to_string()));
                attrs.insert("value".to_string(), Value::Int(b));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!("Invalid value '{}' for 'batch' on '{}'", b, method)),
                );
                return Err(RuntimeError::typed("X::Invalid::Value", attrs));
            }
            // Validate degree
            if let Some(d) = degree
                && d <= 0
            {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("method".to_string(), Value::str(method.to_string()));
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
            // Materialize and wrap
            let items = crate::runtime::value_to_list(&target);
            let arc = std::sync::Arc::new(items);
            let result = if method == "hyper" {
                Value::HyperSeq(arc)
            } else {
                Value::RaceSeq(arc)
            };
            self.stack.push(result);
            // Pure value wrap (no env write): no env_dirty mark needed.
            return Ok(());
        }
        // HyperSeq/RaceSeq delegation: unwrap, dispatch on inner list, wrap back for map/grep
        let hyper_race_wrap = if matches!(&target, Value::HyperSeq(_) | Value::RaceSeq(_)) {
            match method {
                "hyper" => {
                    let items_arc = match &target {
                        Value::HyperSeq(items) | Value::RaceSeq(items) => items.clone(),
                        _ => unreachable!(),
                    };
                    self.stack.push(Value::HyperSeq(items_arc));
                    // Pure rewrap (no env write): no env_dirty mark needed.
                    return Ok(());
                }
                "race" => {
                    let items_arc = match &target {
                        Value::HyperSeq(items) | Value::RaceSeq(items) => items.clone(),
                        _ => unreachable!(),
                    };
                    self.stack.push(Value::RaceSeq(items_arc));
                    // Pure rewrap (no env write): no env_dirty mark needed.
                    return Ok(());
                }
                "is-lazy" => {
                    self.stack.push(Value::Bool(false));
                    // Pure reflection (no env write): no env_dirty mark needed.
                    return Ok(());
                }
                "^name" => {
                    let name = if matches!(&target, Value::HyperSeq(_)) {
                        "HyperSeq"
                    } else {
                        "RaceSeq"
                    };
                    self.stack.push(Value::str(name.to_string()));
                    // Pure reflection (no env write): no env_dirty mark needed.
                    return Ok(());
                }
                "WHAT" => {
                    let name = if matches!(&target, Value::HyperSeq(_)) {
                        "HyperSeq"
                    } else {
                        "RaceSeq"
                    };
                    self.stack.push(Value::Package(Symbol::intern(name)));
                    // Pure reflection (no env write): no env_dirty mark needed.
                    return Ok(());
                }
                "isa" | "does" => {
                    let type_name = if !args.is_empty() {
                        match &args[0] {
                            Value::Package(name) => name.resolve(),
                            Value::Str(s) => (**s).clone(),
                            _ => args[0].to_string_value(),
                        }
                    } else {
                        String::new()
                    };
                    // Delegate to isa_check which handles the full type hierarchy
                    let result = target.isa_check(&type_name);
                    self.stack.push(Value::Bool(result));
                    // Pure reflection (no env write): no env_dirty mark needed.
                    return Ok(());
                }
                "defined" => {
                    self.stack.push(Value::Bool(true));
                    // Pure reflection (no env write): no env_dirty mark needed.
                    return Ok(());
                }
                "map" | "grep" => {
                    let items_arc = match &target {
                        Value::HyperSeq(items) | Value::RaceSeq(items) => items.clone(),
                        _ => unreachable!(),
                    };
                    // For small lists, use parallel execution so inter-item
                    // synchronization (e.g. Promise await chains) works.
                    // For large lists, fall through to the sequential array
                    // map/grep path which is more efficient.
                    if items_arc.len() < 1000 {
                        let is_hyper = matches!(&target, Value::HyperSeq(_));
                        let block = if !args.is_empty() {
                            args[0].clone()
                        } else {
                            Value::Nil
                        };
                        let is_map = method == "map";
                        let result =
                            self.exec_hyper_race_map_grep(&items_arc, block, is_map, is_hyper)?;
                        let wrapped = if is_hyper {
                            Value::HyperSeq(Arc::new(result))
                        } else {
                            Value::RaceSeq(Arc::new(result))
                        };
                        self.stack.push(wrapped);
                        self.env_dirty = true;
                        return Ok(());
                    }
                    // Large list: fall through to array-based dispatch
                    Some(matches!(&target, Value::HyperSeq(_)))
                }
                _ => None,
            }
        } else {
            None
        };
        // Convert HyperSeq/RaceSeq to List for method dispatch
        let target = match target {
            Value::HyperSeq(items) | Value::RaceSeq(items) => Value::Array(
                crate::value::Value::array_arc(items.to_vec()),
                crate::value::ArrayKind::List,
            ),
            other => other,
        };
        // Regex.Bool / Regex.so: smartmatch against $_ (needs runtime context)
        if matches!(method, "Bool" | "so")
            && args.is_empty()
            && matches!(
                &target,
                Value::Regex(_)
                    | Value::RegexWithAdverbs { .. }
                    | Value::Routine { is_regex: true, .. }
            )
        {
            let topic = self.env().get("_").cloned().unwrap_or(Value::Nil);
            let matched = self.vm_smart_match(&topic, &target);
            self.stack.push(Value::Bool(matched));
            self.env_dirty = true;
            return Ok(());
        }
        // .WHO on pseudo-package Package values: build the stash in the Interpreter
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
        // `.print(arg)` / `.say(arg)` / `.put(arg)` / `.note(arg)` on a Failure
        // invocant: the default Mu candidates for these routines take only a
        // slurpy named hash (`(Mu: *%_)`), so a positional argument fails to
        // resolve and Raku throws X::Multi::NoMatch (rather than exploding the
        // Failure). IO::Handle / user classes override these with a positional
        // signature and are dispatched normally before reaching this point.
        if matches!(method, "print" | "say" | "put" | "note")
            && args
                .iter()
                .any(|a| !matches!(a, Value::Pair(..) | Value::ValuePair(..)))
            && let Value::Instance { class_name, .. } = &target
            && class_name.resolve() == "Failure"
            && !target.is_failure_handled()
        {
            return Err(Self::print_routine_no_match_error(method, &target, &args));
        }
        // Unhandled Failure explosion: calling a non-Failure method on an unhandled
        // Failure should throw the stored exception (Raku behavior).
        if let Value::Instance { class_name, .. } = &target
            && class_name.resolve() == "Failure"
            && !target.is_failure_handled()
            && !matches!(
                method,
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
            && let Some(err) = self.failure_to_runtime_error_if_unhandled(&target)
        {
            return Err(err);
        }
        // Pseudo-methods (WHAT, WHICH, etc.) cannot be used with .* or .+
        if matches!(modifier, Some("*") | Some("+"))
            && matches!(
                method,
                "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "WHO" | "DEFINITE" | "VAR"
            )
        {
            return Err(RuntimeError::new(format!(
                "Cannot use .{} on a non-identifier method call",
                modifier.unwrap()
            )));
        }
        // For .* and .+ modifiers, skip the single-dispatch call and go
        // directly to the all-methods-in-MRO path to avoid double execution.
        match modifier {
            Some("+") => {
                let vals =
                    self.call_method_all_with_fallback(&target, method, &args, skip_native)?;
                self.stack.push(Value::array(vals));
                self.env_dirty = true;
            }
            Some("*") => {
                match self.call_method_all_with_fallback(&target, method, &args, skip_native) {
                    Ok(vals) => self.stack.push(Value::array(vals)),
                    Err(e) if Self::is_method_not_found_error(&e) => {
                        self.stack.push(Value::array(vec![]))
                    }
                    Err(e) => return Err(e),
                }
                self.env_dirty = true;
            }
            _ => {
                // Array-subclass instance delegation: when the Instance's class
                // inherits from Array, delegate non-mutating Array methods to the
                // backing __mutsu_array_storage attribute.
                if let Value::Instance {
                    class_name,
                    attributes,
                    ..
                } = &target
                {
                    let cn = class_name.resolve();
                    if !self.has_user_method(&cn, method)
                        && attributes.contains_key("__mutsu_array_storage")
                        && self.mro_readonly(&cn).iter().any(|n| n == "Array")
                    {
                        let storage = attributes
                            .as_map()
                            .get("__mutsu_array_storage")
                            .cloned()
                            .unwrap_or(Value::real_array(Vec::new()));
                        // For mutating methods, delegate and return the result
                        // (the actual mutation is handled in the mut path)
                        if !skip_native
                            && let Some(native_result) =
                                self.try_native_method(&storage, Symbol::intern(method), &args)
                        {
                            // Native method on the by-value backing storage is
                            // env-pure: no env_dirty mark needed.
                            self.stack.push(native_result?);
                            return Ok(());
                        }
                        let result =
                            self.try_compiled_method_or_interpret(storage, method, args.clone());
                        if let Ok(val) = result {
                            self.stack.push(val);
                            self.env_dirty = true;
                            return Ok(());
                        }
                        // Fall through to normal dispatch if delegation failed
                    }
                }
                // Nil method fallback: in Raku, calling most methods on Nil returns Nil.
                // Certain mutating methods throw exceptions.
                // This must be in the Interpreter path (not the interpreter's call_method_with_values)
                // to avoid affecting internal dispatch (e.g. max :by comparators).
                if matches!(&target, Value::Nil) {
                    match method {
                        "BIND-POS" | "BIND-KEY" | "ASSIGN-POS" | "ASSIGN-KEY" | "STORE" => {
                            return Err(RuntimeError::new(format!(
                                "Invocant of method '{}' must be an object instance of type \
                                 'Any', not a type object of type 'Nil'.  Did you forget a \
                                 '.new'?",
                                method
                            )));
                        }
                        // Any:U autovivification: push/append/unshift/prepend on
                        // a runtime-undefined value (e.g. an out-of-range array
                        // element `@a[5]` or an uninitialised variable) creates a
                        // new Array, which the indexed-method writeback path then
                        // stores back. A *literal* `Nil.push` is rejected at
                        // compile time (see compile_expr in the compiler), so this
                        // path only sees autovivifiable Nils.
                        "push" | "append" | "unshift" | "prepend" => {
                            let arr: Vec<Value> = match method {
                                "append" | "prepend" => {
                                    // Flatten list arguments for append/prepend
                                    let mut flat = Vec::new();
                                    for arg in &args {
                                        match arg {
                                            Value::Array(items, _) => {
                                                flat.extend(items.iter().cloned());
                                            }
                                            Value::Seq(items) | Value::Slip(items) => {
                                                flat.extend(items.iter().cloned());
                                            }
                                            other => flat.push(other.clone()),
                                        }
                                    }
                                    flat
                                }
                                "unshift" => args.to_vec(),
                                _ => args,
                            };
                            self.stack.push(Value::real_array(arr));
                            self.env_dirty = true;
                            return Ok(());
                        }
                        "defined" | "Bool" | "so" | "not" | "gist" | "Str" | "raku" | "perl"
                        | "WHAT" | "WHICH" | "WHERE" | "HOW" | "WHY" | "VAR" | "DEFINITE"
                        | "isa" | "does" | "can" | "^name" | "^mro" | "^pun" | "new" | "bless"
                        | "clone" | "item" | "self" | "sink" | "pending" => {
                            // Fall through to normal dispatch
                        }
                        // List/iteration methods inherited from Any: Nil behaves
                        // like a single undefined item (e.g. `Nil.grep(*.defined)`
                        // is an empty Seq, `Nil.map(*.so)` is `(False)`), NOT a
                        // Nil-absorbing no-op. Falling through routes these to the
                        // normal dispatch, which treats Nil as a 1-element list.
                        // This matters now that `for Nil { }` runs one iteration:
                        // a Nil-returning `.grep` would otherwise iterate once over
                        // Nil instead of zero times over an empty Seq.
                        "grep" | "map" | "first" | "sort" | "reverse" | "list" | "List"
                        | "Slip" | "flat" | "Seq" | "cache" | "head" | "tail" | "elems" => {
                            // Fall through to normal dispatch
                        }
                        // Numeric coercion on Nil (an undefined value) warns and
                        // yields the corresponding numeric type object, e.g.
                        // `Nil.Rat` is `(Rat)` which numifies to 0.
                        "Rat" | "FatRat" | "Int" | "Num" | "Complex" if args.is_empty() => {
                            let msg = "Use of Nil in numeric context".to_string();
                            return Err(RuntimeError::warn_signal_with_resume(
                                msg,
                                Value::Package(Symbol::intern(method)),
                            ));
                        }
                        // `Nil.ords` warns ("Use of Nil in string context") and
                        // resumes to an empty Seq; `Nil.chrs` warns and resumes
                        // to a single null byte.
                        "ords" if args.is_empty() => {
                            return Err(RuntimeError::warn_signal_with_resume(
                                "Use of Nil in string context".to_string(),
                                Value::Seq(Arc::new(vec![])),
                            ));
                        }
                        "chrs" if args.is_empty() => {
                            return Err(RuntimeError::warn_signal_with_resume(
                                "Use of Nil in string context".to_string(),
                                Value::str("\0".to_string()),
                            ));
                        }
                        _ => {
                            // Nil-absorbing method returns Nil and touches no env.
                            self.stack.push(Value::Nil);
                            return Ok(());
                        }
                    }
                }
                // Any:U autovivification: push/append/unshift/prepend on a type
                // object (e.g. Any from hash miss) creates a new Array.
                if matches!(method, "push" | "append" | "unshift" | "prepend")
                    && matches!(&target, Value::Package(name) if name.resolve() == "Any" || name.resolve() == "Mu")
                {
                    let arr: Vec<Value> = match method {
                        "append" | "prepend" => {
                            let mut flat = Vec::new();
                            for arg in &args {
                                match arg {
                                    Value::Array(items, _) => {
                                        flat.extend(items.iter().cloned());
                                    }
                                    Value::Seq(items) | Value::Slip(items) => {
                                        flat.extend(items.iter().cloned());
                                    }
                                    other => flat.push(other.clone()),
                                }
                            }
                            flat
                        }
                        "unshift" => args.to_vec(),
                        _ => args,
                    };
                    self.stack.push(Value::real_array(arr));
                    self.env_dirty = true;
                    return Ok(());
                }
                // If we have a pending Proxy subclass attribute reference and this
                // is a mutating array method, delegate to the shared storage mutator
                // so the mutation is visible through the Proxy subclass.
                if matches!(
                    method,
                    "push" | "pop" | "shift" | "unshift" | "append" | "prepend"
                ) && matches!(&target, Value::Array(..))
                    && let Some((attrs_ref, attr_name)) = self.pending_proxy_subclass_attr.take()
                {
                    let result =
                        self.proxy_subclass_array_mutate(&attrs_ref, &attr_name, method, &args)?;
                    self.stack.push(result);
                    self.env_dirty = true;
                    return Ok(());
                }
                // Fast path for shift/pop on array values in the non-mutating
                // (CallMethod) path. Returns the removed element without modifying
                // any variable. This handles cases like [1,2,3].shift where there
                // is no variable to mutate. The CallMethodMut path handles variable
                // targets separately.
                // Slice 6.3: assume the dispatch dirties the caller env; only a
                // proven-pure compiled method path clears this (sets it true),
                // letting the opcode tail skip the env_dirty mark + per-call pull.
                self.method_dispatch_pure = false;
                let call_result = if matches!(method, "shift" | "pop")
                    && args.is_empty()
                    && matches!(&target, Value::Array(_, kind) if kind.is_real_array())
                {
                    // Native array read on a by-value target: env-pure.
                    self.method_dispatch_pure = true;
                    if let Value::Array(_, kind) = &target
                        && kind.is_lazy()
                    {
                        return Err(RuntimeError::cannot_lazy(method));
                    }
                    if let Value::Array(items, _) = &target {
                        Ok(if items.is_empty() {
                            crate::runtime::make_empty_array_failure(method)
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
                            self.try_native_method(&resolved, Symbol::intern(method), &args)
                        {
                            let result = native_result;
                            // Native method on a by-value resolved hash is env-pure
                            // (see the sibling native-method branches below that set
                            // method_dispatch_pure): no env_dirty mark needed.
                            match modifier {
                                Some("?") => {
                                    self.stack.push(result.unwrap_or(Value::Nil));
                                }
                                _ => {
                                    self.stack.push(result?);
                                }
                            }
                            return Ok(());
                        }
                    }
                    // .Slip on arrays with `is default(X)`: fill holes with
                    // the default value instead of leaving Package("Any").
                    if method == "Slip" && args.is_empty() && matches!(&target, Value::Array(..)) {
                        if let Some(def) = self.container_default(&target).cloned() {
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
                            // Pure array transform: env-pure.
                            self.method_dispatch_pure = true;
                            Ok(Value::Slip(std::sync::Arc::new(converted)))
                        } else if let Some(native_result) =
                            self.try_native_method(&target, Symbol::intern(method), &args)
                        {
                            // Native method on a by-value (read) target: env-pure.
                            self.method_dispatch_pure = true;
                            native_result
                        } else {
                            self.try_compiled_method_or_interpret(target, method, args)
                        }
                    } else if let Some(native_result) =
                        self.try_native_method(&target, Symbol::intern(method), &args)
                    {
                        // Native method on a by-value (read) target: env-pure.
                        self.method_dispatch_pure = true;
                        native_result
                    } else {
                        self.try_compiled_method_or_interpret(target, method, args)
                    }
                } else {
                    self.try_compiled_method_or_interpret(target, method, args)
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
                // Wrap map/grep results back into HyperSeq/RaceSeq
                if let Some(is_hyper) = hyper_race_wrap
                    && let Some(result) = self.stack.pop()
                {
                    let result_items = crate::runtime::value_to_list(&result);
                    let wrapped = if is_hyper {
                        Value::HyperSeq(std::sync::Arc::new(result_items))
                    } else {
                        Value::RaceSeq(std::sync::Arc::new(result_items))
                    };
                    self.stack.push(wrapped);
                }
            }
        }
        Ok(())
    }
}
