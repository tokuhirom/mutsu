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
        if matches!(value, Value::Nil) {
            let mut err = RuntimeError::new(default_message);
            err.is_fail = is_fail;
            return err;
        }

        let msg = if let Value::Instance { attributes, .. } = value {
            attributes
                .get("message")
                .map(|v| v.to_string_value())
                .unwrap_or_else(|| {
                    // Try calling the user-defined .Str method
                    self.call_method_with_values(value.clone(), "Str", vec![])
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|_| value.to_string_value())
                })
        } else if let Value::Array(items, _) = value {
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
        err.is_fail = is_fail;
        if let Value::Instance { class_name, .. } = value {
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
        let has_real_args = match args.first() {
            Some(Value::Array(items, _)) if items.is_empty() => false,
            Some(_) => true,
            None => false,
        };
        if has_real_args {
            return Err(self.runtime_error_from_die_value(args.first().unwrap(), "Died", false));
        }
        if let Some(current) = self.env.get("!").cloned()
            && !matches!(current, Value::Nil)
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
            if let Value::Instance {
                class_name,
                attributes,
                ..
            } = &v
                && class_name.resolve() == "Failure"
                && let Some(exc) = attributes.get("exception").cloned()
            {
                return Err(self.runtime_error_from_die_value(&exc, "Failed", true));
            }
            return Err(self.runtime_error_from_die_value(&v, "Failed", true));
        }
        if let Some(current) = self.env.get("!").cloned()
            && !matches!(current, Value::Nil)
        {
            return Err(self.runtime_error_from_die_value(&current, "Failed", true));
        }
        let mut err = RuntimeError::new("Failed");
        err.is_fail = true;
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
        let value = args.first().cloned().unwrap_or(Value::Nil);
        Err(RuntimeError {
            return_value: Some(value),
            ..RuntimeError::new("")
        })
    }

    pub(super) fn leave_return_value(args: &[Value]) -> Option<Value> {
        match args {
            [] => None,
            [single] => Some(single.clone()),
            _ => Some(Value::Slip(std::sync::Arc::new(args.to_vec()))),
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

        let current_callable_id = self.env.get("__mutsu_callable_id").and_then(|v| match v {
            Value::Int(i) if *i > 0 => Some(*i as u64),
            _ => None,
        });
        let current_block_id = self.env.get("&?BLOCK").and_then(|v| match v {
            Value::WeakSub(weak) => weak.upgrade().map(|sub| sub.id),
            Value::Sub(sub) => Some(sub.id),
            _ => None,
        });

        match target {
            None => {}
            Some(Value::WeakSub(weak)) => {
                if let Some(sub) = weak.upgrade() {
                    if Some(sub.id) != current_callable_id && Some(sub.id) != current_block_id {
                        sig.leave_callable_id = Some(sub.id);
                    }
                } else {
                    return Err(RuntimeError::new("Callable has been freed"));
                }
            }
            Some(Value::Sub(data)) => {
                if Some(data.id) != current_callable_id && Some(data.id) != current_block_id {
                    sig.leave_callable_id = Some(data.id);
                }
            }
            Some(Value::Routine { package, name, .. }) => {
                sig.leave_routine = Some(format!("{package}::{name}"));
            }
            Some(Value::Nil) => {}
            Some(Value::Package(name)) if name == "Any" => {}
            Some(Value::Package(name)) if name == "Sub" => {
                let caller_callable_id = self
                    .caller_env_stack
                    .last()
                    .and_then(|env| env.get("__mutsu_callable_id"))
                    .and_then(|v| match v {
                        Value::Int(i) if *i > 0 => Some(*i as u64),
                        _ => None,
                    });
                if let Some(id) = caller_callable_id {
                    sig.leave_callable_id = Some(id);
                } else if let Some((package, routine)) = self.routine_stack_top() {
                    sig.leave_routine = Some(format!("{package}::{routine}"));
                }
            }
            Some(Value::Package(name)) if name == "Block" => {}
            Some(Value::Str(label)) => {
                sig.label = Some(label.to_string());
            }
            Some(other) => {
                sig.label = Some(other.to_string_value());
            }
        }

        Err(sig)
    }

    pub(super) fn builtin_exit(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        let code = match args.first() {
            Some(Value::Int(i)) => *i,
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
        }
        Ok(Value::Nil)
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
        let line = self.test_pending_callsite_line.unwrap_or(1);
        message.push_str(&format!("\n  in block <unit> at {} line {}", file, line));
        Err(RuntimeError::warn_signal(message))
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
            match value {
                Value::Array(items, kind) => {
                    let mut mapped = Vec::with_capacity(items.len());
                    for item in items.iter() {
                        mapped.push(apply_hyper_prefix(interp, routine, item.clone())?);
                    }
                    Ok(Value::Array(std::sync::Arc::new(mapped), kind))
                }
                Value::Seq(items) => {
                    let mut mapped = Vec::with_capacity(items.len());
                    for item in items.iter() {
                        mapped.push(apply_hyper_prefix(interp, routine, item.clone())?);
                    }
                    Ok(Value::Seq(std::sync::Arc::new(mapped)))
                }
                Value::Slip(items) => {
                    let mut mapped = Vec::with_capacity(items.len());
                    for item in items.iter() {
                        mapped.push(apply_hyper_prefix(interp, routine, item.clone())?);
                    }
                    Ok(Value::Slip(std::sync::Arc::new(mapped)))
                }
                other => interp.call_function(routine, vec![other]),
            }
        }
        let items = crate::runtime::value_to_list(&args[1]);
        let mut results = Vec::with_capacity(items.len());
        for item in items {
            results.push(apply_hyper_prefix(self, &routine, item)?);
        }
        Ok(Value::array(results))
    }
}
