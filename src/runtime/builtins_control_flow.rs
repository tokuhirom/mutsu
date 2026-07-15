use super::*;
use crate::symbol::Symbol;
use std::sync::atomic::{AtomicBool, AtomicI64, Ordering};

/// Global flag set by `exit()` from any thread.  `sleep()` polls this
/// so the main thread can wake up when a spawned thread calls `exit`.
static GLOBAL_EXIT_REQUESTED: AtomicBool = AtomicBool::new(false);
static GLOBAL_EXIT_CODE: AtomicI64 = AtomicI64::new(0);

/// Set the global exit flag (called by `exit()` from any thread).
pub(crate) fn set_global_exit_flag(code: i64) {
    GLOBAL_EXIT_CODE.store(code, Ordering::SeqCst);
    GLOBAL_EXIT_REQUESTED.store(true, Ordering::SeqCst);
}

/// Check whether a global exit has been requested and return the exit code.
pub(crate) fn global_exit_requested() -> Option<i64> {
    if GLOBAL_EXIT_REQUESTED.load(Ordering::SeqCst) {
        Some(GLOBAL_EXIT_CODE.load(Ordering::SeqCst))
    } else {
        None
    }
}

impl Interpreter {
    pub(super) fn runtime_error_from_die_value(
        &mut self,
        value: &Value,
        default_message: &str,
        is_fail: bool,
    ) -> RuntimeError {
        if value.is_nil() {
            let mut err = RuntimeError::new(default_message);
            if is_fail {
                err.control = Some(crate::value::Control::Fail);
            }
            return err;
        }

        let msg = if let ValueView::Instance { attributes, .. } = value.view() {
            attributes
                .as_map()
                .get("message")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| {
                    // Try calling the user-defined .Str method
                    self.call_method_with_values(value.clone(), "Str", vec![])
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|_| value.to_string_value())
                })
        } else if let ValueView::Array(items, _) = value.view() {
            // Multi-arg die: concatenate .Str of each element
            let mut parts = Vec::new();
            for item in items.iter() {
                let s = self
                    .call_method_with_values(item.clone(), "Str", vec![])
                    .map(|v| v.to_string_value())
                    .unwrap_or_else(|_| item.to_string_value());
                parts.push(s);
            }
            parts.join("")
        } else {
            value.to_string_value()
        };

        let mut err = RuntimeError::new(&msg);
        if is_fail {
            err.control = Some(crate::value::Control::Fail);
        }
        if let ValueView::Instance { class_name, .. } = value.view() {
            let cn = class_name.resolve();
            let is_exception = cn == "Exception"
                || cn.starts_with("X::")
                || cn.starts_with("CX::")
                || self
                    .mro_readonly(&cn)
                    .iter()
                    .any(|p| p == "Exception" || p.starts_with("X::") || p.starts_with("CX::"));
            if is_exception {
                err.exception = Some(Box::new(value.clone()));
            } else {
                // Non-exception instance: wrap in X::AdHoc with payload
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("payload".to_string(), value.clone());
                attrs.insert("message".to_string(), Value::str(msg.clone()));
                err.exception = Some(Box::new(Value::make_instance(
                    Symbol::intern("X::AdHoc"),
                    attrs,
                )));
            }
        } else {
            // Non-instance value (Str, Int, etc.): wrap in X::AdHoc with payload
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("payload".to_string(), value.clone());
            attrs.insert("message".to_string(), Value::str(msg.clone()));
            err.exception = Some(Box::new(Value::make_instance(
                Symbol::intern("X::AdHoc"),
                attrs,
            )));
        }
        err
    }

    pub(super) fn builtin_die(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        // Check if we have actual arguments (not just an empty array from die())
        let has_real_args = match args.first().map(Value::view) {
            Some(ValueView::Array(items, _)) if items.is_empty() => false,
            Some(_) => true,
            None => false,
        };
        if has_real_args {
            return Err(self.runtime_error_from_die_value(args.first().unwrap(), "Died", false));
        }
        if let Some(current) = self.env.get("!").cloned()
            && !current.is_nil()
        {
            return Err(self.runtime_error_from_die_value(&current, "Died", false));
        }
        // die() with no args and $! not set: create X::AdHoc with "Died" message
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("payload".to_string(), Value::str("Died".to_string()));
        attrs.insert("message".to_string(), Value::str("Died".to_string()));
        let exception = Value::make_instance(Symbol::intern("X::AdHoc"), attrs);
        let mut err = RuntimeError::new("Died");
        err.exception = Some(Box::new(exception));
        Err(err)
    }

    pub(super) fn builtin_fail(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if let Some(v) = args.first().cloned() {
            // When fail() receives a Failure:D, extract the inner exception
            // and re-arm it (Raku behavior: fail(Failure:D) re-arms)
            if let ValueView::Instance {
                class_name,
                attributes,
                ..
            } = v.view()
                && class_name.resolve() == "Failure"
                && let Some(exc) = attributes.as_map().get("exception").cloned()
            {
                return Err(self.runtime_error_from_die_value(&exc, "Failed", true));
            }
            return Err(self.runtime_error_from_die_value(&v, "Failed", true));
        }
        if let Some(current) = self.env.get("!").cloned()
            && !current.is_nil()
        {
            return Err(self.runtime_error_from_die_value(&current, "Failed", true));
        }
        let mut err = RuntimeError::new("Failed");
        err.control = Some(crate::value::Control::Fail);
        Err(err)
    }

    pub(super) fn builtin_succeed(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut sig = RuntimeError::succeed_signal();
        if let Some(v) = args.first() {
            sig.return_value = Some(v.clone());
        }
        Err(sig)
    }

    pub(super) fn builtin_return_rw(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let value = args.first().cloned().unwrap_or(Value::NIL);
        Err(RuntimeError {
            return_value: Some(value),
            ..RuntimeError::new("")
        })
    }

    pub(super) fn leave_return_value(args: &[Value]) -> Option<Value> {
        match args {
            [] => None,
            [single] => Some(single.clone()),
            _ => Some(Value::slip_arc(std::sync::Arc::new(args.to_vec()))),
        }
    }

    pub(crate) fn builtin_leave(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        self.builtin_leave_with_target(None, args)
    }

    pub(crate) fn builtin_leave_method(
        &mut self,
        target: Value,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.builtin_leave_with_target(Some(target), args)
    }

    pub(super) fn builtin_leave_with_target(
        &mut self,
        target: Option<Value>,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let mut sig = RuntimeError::last_signal();
        sig.is_leave = true;
        sig.return_value = Self::leave_return_value(args);

        let current_callable_id =
            self.env
                .get("__mutsu_callable_id")
                .and_then(|v| match v.view() {
                    ValueView::Int(i) if i > 0 => Some(i as u64),
                    _ => None,
                });
        let current_block_id = self.env.get("&?BLOCK").and_then(|v| match v.view() {
            ValueView::WeakSub(weak) => weak.upgrade().map(|sub| sub.id),
            ValueView::Sub(sub) => Some(sub.id),
            _ => None,
        });

        match target.as_ref().map(Value::view) {
            None => {}
            Some(ValueView::WeakSub(weak)) => {
                if let Some(sub) = weak.upgrade() {
                    if Some(sub.id) != current_callable_id && Some(sub.id) != current_block_id {
                        sig.set_leave_callable_id(Some(sub.id));
                    }
                } else {
                    return Err(RuntimeError::new("Callable has been freed"));
                }
            }
            Some(ValueView::Sub(data)) => {
                if Some(data.id) != current_callable_id && Some(data.id) != current_block_id {
                    sig.set_leave_callable_id(Some(data.id));
                }
            }
            Some(ValueView::Routine { package, name, .. }) => {
                sig.set_leave_routine(Some(format!("{package}::{name}")));
            }
            Some(ValueView::Nil) => {}
            Some(ValueView::Package(name)) if name == "Any" => {}
            Some(ValueView::Package(name)) if name == "Sub" => {
                let caller_callable_id = self
                    .caller_env_stack
                    .last()
                    .and_then(|env| env.get("__mutsu_callable_id"))
                    .and_then(|v| match v.view() {
                        ValueView::Int(i) if i > 0 => Some(i as u64),
                        _ => None,
                    });
                if let Some(id) = caller_callable_id {
                    sig.set_leave_callable_id(Some(id));
                } else if let Some(frame) = self.routine_stack_top() {
                    sig.set_leave_routine(Some(format!("{}::{}", frame.package, frame.name)));
                }
            }
            Some(ValueView::Package(name)) if name == "Block" => {}
            Some(ValueView::Str(label)) => {
                sig.label = Some(label.to_string());
            }
            Some(_) => {
                sig.label = Some(target.as_ref().unwrap().to_string_value());
            }
        }

        Err(sig)
    }

    pub(super) fn builtin_exit(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code = match args.first().map(Value::view) {
            Some(ValueView::Int(i)) => i,
            Some(ValueView::Bool(b)) => {
                if b {
                    1
                } else {
                    0
                }
            }
            Some(ValueView::Num(f)) => f as i64,
            Some(_) => args.first().unwrap().to_f64() as i64,
            _ => 0,
        };
        self.halted = true;
        self.exit_code = code;
        // Signal any sleeping threads that the process should exit.
        // This is used when exit() is called from a `start` block or
        // signal handler -- the main thread may be blocked in sleep()
        // and needs to wake up.
        if !self.nested_mode {
            set_global_exit_flag(code);
            // `exit` terminates the PROCESS in raku, wherever it is called.
            // The flag above only works if some thread polls it; when `exit`
            // runs on a worker thread while the main thread is stuck inside a
            // long non-polling computation (`start { sleep 2; exit }; EVAL
            // 'say 1.0000001 ** 10**8'`, A01-limits/overflow.t), the process
            // would hang forever. Flush this thread's buffered output, then
            // exit for real.
            if self.output_sink().is_thread_clone {
                use std::io::Write;
                let (out, err, shared_out, shared_err) = {
                    let sink = self.output_sink();
                    (
                        sink.output.clone(),
                        sink.stderr_output.clone(),
                        sink.shared_thread_output.clone(),
                        sink.shared_thread_stderr.clone(),
                    )
                };
                let mut stdout = std::io::stdout();
                if let Some(so) = shared_out {
                    let _ = stdout.write_all(so.lock().unwrap().as_bytes());
                }
                let _ = stdout.write_all(out.as_bytes());
                let _ = stdout.flush();
                let mut stderr_h = std::io::stderr();
                if let Some(se) = shared_err {
                    let _ = stderr_h.write_all(se.lock().unwrap().as_bytes());
                }
                let _ = stderr_h.write_all(err.as_bytes());
                let _ = stderr_h.flush();
                std::process::exit(code as i32);
            }
        }
        Ok(Value::NIL)
    }

    pub(super) fn builtin_warn(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut message = String::new();
        for arg in args {
            message.push_str(&arg.to_string_value());
        }
        if message.is_empty() {
            message = "Warning: something's wrong".to_string();
        }
        // Append a Raku-style source location annotation so that the
        // stderr output includes a file/line reference. The callsite line
        // was set by the VM right before dispatching this builtin.
        let file = self
            .program_path
            .clone()
            .unwrap_or_else(|| "-e".to_string());
        let line = self
            .test_pending_callsite_line
            .unwrap_or(self.cur_source_line);
        message.push_str(&format!("\n  in block <unit> at {} line {}", file, line));
        // When no CONTROL handler is active (`control_handler_depth == 0`), this
        // warn's only possible fate is the default handler: print to stderr and
        // resume. Handle it inline here, at the raise site, rather than returning
        // an `Err` that unwinds the Rust call stack. Unwinding loses every frame
        // between here and the top-level loop, so a `warn` raised deep inside a
        // method/sub chain (e.g. `render -> log -> &warn.()`) would abandon the
        // whole computation instead of resuming it (cross-frame warn). Because
        // `control_handler_depth == 0` guarantees there is no `CONTROL`/`when
        // CX::Warn` block on the stack, there is no `succeed`/unwind target to
        // honour, so resolving the warn locally is complete and correct here.
        if self.control_handler_depth == 0 {
            if !self.warning_suppressed() {
                self.write_warn_to_stderr(&message);
            }
            return Ok(Value::NIL);
        }
        // A CONTROL handler is active somewhere up the dynamic call stack. If the
        // *innermost* one unconditionally `.resume`s (`resume_safe`), run it
        // INLINE here, at the raise site, then return `Ok(Nil)` so the deep
        // computation continues exactly where the `warn` was raised. Returning an
        // `Err` instead would unwind the Rust call stack to the CONTROL block's
        // frame, destroying every frame between here and there — so the rest of
        // the deep computation (the code after the `warn`) would be lost. This is
        // the cross-frame resumable-warn mechanism. A non-resume-safe handler
        // (e.g. `when CX::Warn { }`, which `succeed`s/exits the block) must take
        // the unwinding path so the `succeed` has a block boundary to unwind to.
        if let Some(result) = self.try_resume_safe_control_inline(&message) {
            return result;
        }
        Err(RuntimeError::warn_signal(message))
    }

    /// Raise a resumable warning from an arbitrary raise site: print-and-resume
    /// inline when no CONTROL handler is active (returning `resume` as the
    /// expression's value), run a resume-safe CONTROL handler inline, or fall
    /// back to the unwinding `CX::Warn` signal. Op-level warn sites (e.g. the
    /// `+Any` numeric coercion) must use this instead of returning a bare
    /// `warn_signal_with_resume` error: the unwinding signal carries its resume
    /// value in `return_value`, which function-call boundaries treat as an
    /// explicit `return` — silently swallowing the warning and abandoning the
    /// rest of the callee body.
    pub(crate) fn raise_resumable_warning(
        &mut self,
        message: &str,
        resume: Value,
    ) -> Result<Value, RuntimeError> {
        if self.control_handler_depth == 0 {
            if !self.warning_suppressed() {
                self.write_warn_to_stderr(message);
            }
            return Ok(resume);
        }
        if let Some(result) = self.try_resume_safe_control_inline(message) {
            return result.map(|_| resume);
        }
        Err(RuntimeError::warn_signal_with_resume(
            message.to_string(),
            resume,
        ))
    }

    /// If the innermost active CONTROL handler is `resume_safe`, run it inline
    /// against the current (deep) environment and return the value the suspended
    /// `warn` should evaluate to (`Nil` for a bare `.resume`). Returns `None`
    /// when there is no inline-eligible handler, so the caller falls back to the
    /// unwinding path. See `builtin_warn` for why this is necessary.
    fn try_resume_safe_control_inline(
        &mut self,
        message: &str,
    ) -> Option<Result<Value, RuntimeError>> {
        let top = self.control_handlers.last()?;
        if !top.resume_safe {
            return None;
        }
        let handler = top.handler.as_ref()?;
        // Clone what we need so we can take `&mut self` while running.
        let code = handler.code.clone();
        let control_begin = handler.control_begin;
        let end = handler.end;
        let fns = handler.compiled_fns.clone();

        // Present the warning to the handler as a `CX::Warn` topic in `$_`,
        // mirroring the unwinding path in `exec_try_catch_op_inner`.
        let warn_signal = RuntimeError::warn_signal(message.to_string());
        let topic = Self::control_signal_topic_value(&warn_signal);
        let saved_topic = self.env().get("_").cloned();
        if let Some(t) = topic {
            self.env_mut().insert("_".to_string(), t);
        }
        let saved_when = self.when_matched();
        self.set_when_matched(false);

        // The handler's bytecode (`code`) belongs to the CONTROL-installing
        // frame and addresses *that* frame's local slots, but right now
        // `self.locals` is the deep frame's (the `warn` raise site). Reconstruct
        // the installing frame's locals from `env` — the cross-frame-visible
        // store, where `code.locals[i]` names slot `i` (this mirrors
        // `reconcile_locals_from_env_at_site`). After running, flush any slot the
        // handler mutated back to `env` and mark `env_dirty`, so the installing
        // frame reconciles its slots from `env` when the deep call chain returns
        // (the same path a normal cross-frame by-name write takes). This is what
        // makes the handler's `$out ~= .Str` survive to after `render()`.
        let handler_locals: Vec<Value> = code
            .locals
            .iter()
            .map(|name| {
                self.env().get(name).cloned().unwrap_or_else(|| {
                    name.strip_prefix('$')
                        .or_else(|| name.strip_prefix('@'))
                        .or_else(|| name.strip_prefix('%'))
                        .or_else(|| name.strip_prefix('&'))
                        .and_then(|bare| self.env().get(bare).cloned())
                        .unwrap_or(Value::NIL)
                })
            })
            .collect();
        let seeded = handler_locals.clone();
        let saved_locals = std::mem::replace(&mut self.locals, handler_locals);
        // The handler addresses the installing frame's lexicals by name through
        // env (handler_locals above), NOT this deep frame's upvalue array. Clear
        // the upvalue array so any `GetUpvalue` in the handler range falls back to
        // the env read (correct), instead of indexing the wrong frame's array.
        let saved_upvalues = std::mem::take(&mut self.upvalues);

        // The handler runs in the deep frame's stack; isolate its operand-stack
        // effects so the suspended computation's stack is left untouched.
        let saved_stack = self.stack.len();
        let result = self.run_range(&code, control_begin, end, &fns);
        self.stack.truncate(saved_stack);

        // Flush slots the handler changed back to env so the installing frame
        // (and any intervening by-name reader, e.g. `render`'s own `$out ~=`)
        // observes them. Only changed slots are written, to keep blast radius
        // minimal.
        let handler_locals = std::mem::replace(&mut self.locals, saved_locals);
        self.upvalues = saved_upvalues;
        for (i, name) in code.locals.iter().enumerate() {
            if name.is_empty() {
                continue;
            }
            if handler_locals[i] != seeded[i] {
                self.env_mut()
                    .insert(name.clone(), handler_locals[i].clone());
                // The handler body mutated the installing frame's lexical `name`
                // by writing `env` here (double-OFF: a *resumed* indirect `warn`
                // left the caller's `$out` slot stale even though env held the
                // handler's write). Record the name for the precise drain
                // (`apply_pending_rw_writeback`) that every call site runs:
                // drop-on-miss for the same frame, plus retain-on-miss so a deeper
                // raise site carries it up to the installing frame.
                self.pending_rw_writeback_sources.push(name.clone());
                self.record_caller_var_writeback(name);
            }
        }

        self.set_when_matched(saved_when);
        if let Some(v) = saved_topic {
            self.env_mut().insert("_".to_string(), v);
        } else {
            self.env_mut().remove("_");
        }

        match result {
            // Handler fell through (no explicit `.resume`) — for a resume_safe
            // handler this is equivalent to resuming with Nil.
            Ok(()) => Some(Ok(Value::NIL)),
            // `.resume` — the warn resumes; a bare `.resume` yields Nil.
            Err(ce) if ce.is_resume() => Some(Ok(Value::NIL)),
            // The handler re-threw the warn (`warn`/`.rethrow` of CX::Warn):
            // act like the default handler — print and resume.
            Err(ce) if ce.is_warn() => {
                if !self.warning_suppressed() {
                    self.write_warn_to_stderr(&ce.message);
                }
                Some(Ok(Value::NIL))
            }
            // Anything else (shouldn't happen for a resume_safe handler) →
            // propagate so it is not silently swallowed.
            Err(ce) => Some(Err(ce)),
        }
    }

    pub(super) fn make_stub_exception(message: String) -> Value {
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(message));
        Value::make_instance(Symbol::intern("X::StubCode"), attrs)
    }

    pub(super) fn builtin_stub_die(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut message = String::new();
        for arg in args {
            message.push_str(&arg.to_string_value());
        }
        let ex = Self::make_stub_exception(message);
        Err(self.runtime_error_from_die_value(&ex, "Stub code executed", false))
    }

    /// Compile-target for constructs the parser recognizes as an undeclared
    /// bare variable (e.g. `%::{''}` — rakudo's "Variable '%' is not
    /// declared"). Raises X::Undeclared with symbol/what/line attributes.
    pub(super) fn builtin_undeclared_var_die(
        &mut self,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        let symbol = args.first().map(Value::to_string_value).unwrap_or_default();
        let msg = format!(
            "Variable '{symbol}' is not declared. Perhaps you forgot a 'sub' if this was\nintended to be part of a signature?"
        );
        let mut attrs = HashMap::new();
        attrs.insert("symbol".to_string(), Value::str(symbol));
        attrs.insert("what".to_string(), Value::str_from("Variable"));
        attrs.insert("line".to_string(), Value::int(self.cur_source_line));
        attrs.insert("message".to_string(), Value::str(msg));
        Err(RuntimeError::typed("X::Undeclared", attrs))
    }

    pub(super) fn builtin_stub_warn(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let mut message = String::new();
        for arg in args {
            message.push_str(&arg.to_string_value());
        }
        if message.is_empty() {
            message = "Warning: something's wrong".to_string();
        }
        Err(RuntimeError::warn_signal(message))
    }

    pub(super) fn builtin_incdec_nomatch(&self, args: &[Value]) -> Result<Value, RuntimeError> {
        let caller = args
            .first()
            .map(Value::to_string_value)
            .unwrap_or_else(|| "postfix:<++>".to_string());
        let msg = format!(
            "Cannot resolve caller {}(...); the parameter requires mutable arguments",
            caller
        );
        let mut err = RuntimeError::new(msg.clone());
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("message".to_string(), Value::str(msg));
        err.exception = Some(Box::new(Value::make_instance(
            Symbol::intern("X::Multi::NoMatch"),
            attrs,
        )));
        Err(err)
    }

    pub(super) fn builtin_hyper_prefix(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() < 2 {
            return Ok(Value::array(vec![]));
        }
        let op = args[0].to_string_value();
        let routine = format!("prefix:<{}>", op);
        fn apply_hyper_prefix(
            interp: &mut Interpreter,
            routine: &str,
            value: Value,
        ) -> Result<Value, RuntimeError> {
            match value.view() {
                ValueView::Array(items, kind) => {
                    let mut mapped = Vec::with_capacity(items.len());
                    for item in items.iter() {
                        mapped.push(apply_hyper_prefix(interp, routine, item.clone())?);
                    }
                    Ok(Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(mapped)),
                        kind,
                    ))
                }
                ValueView::Seq(items) => {
                    let mut mapped = Vec::with_capacity(items.len());
                    for item in items.iter() {
                        mapped.push(apply_hyper_prefix(interp, routine, item.clone())?);
                    }
                    Ok(Value::seq(mapped))
                }
                ValueView::Slip(items) => {
                    let mut mapped = Vec::with_capacity(items.len());
                    for item in items.iter() {
                        mapped.push(apply_hyper_prefix(interp, routine, item.clone())?);
                    }
                    Ok(Value::slip_arc(std::sync::Arc::new(mapped)))
                }
                _ => interp.call_function(routine, vec![value.clone()]),
            }
        }
        // Auto-increment/decrement prefix hyper ops mutate the operand in place,
        // so the original container variable must be written back (e.g.
        // `--<<%h` and `--<<@a` both decrement the stored elements). Negation
        // (`-<<`) and other non-mutating prefixes only return a new container.
        let mutating = op == "++" || op == "--";
        // Handle hashes: apply the operation to values, preserving hash structure
        if let ValueView::Hash(map) = args[1].view() {
            let mut result_map = std::collections::HashMap::new();
            for (k, v) in map.iter() {
                let new_val = apply_hyper_prefix(self, &routine, v.clone())?;
                result_map.insert(k.clone(), new_val);
            }
            let result = Value::hash_with_data(Value::hash_arc(result_map));
            if mutating {
                self.overwrite_hash_bindings_by_identity(&map, result.clone());
            }
            return Ok(result);
        }
        // Handle arrays: preserve the array kind and write the mutation back to
        // the original variable for auto-increment/decrement.
        if let ValueView::Array(items, kind) = args[1].view() {
            let mut results = Vec::with_capacity(items.len());
            for item in items.iter() {
                results.push(apply_hyper_prefix(self, &routine, item.clone())?);
            }
            let result = Value::array_with_kind(
                crate::gc::Gc::new(crate::value::ArrayData::new(results)),
                kind,
            );
            if mutating {
                self.overwrite_array_bindings_by_identity(&items, result.clone());
            }
            return Ok(result);
        }
        let items = crate::runtime::value_to_list(&args[1]);
        let mut results = Vec::with_capacity(items.len());
        for item in items {
            results.push(apply_hyper_prefix(self, &routine, item)?);
        }
        Ok(Value::array(results))
    }
}
