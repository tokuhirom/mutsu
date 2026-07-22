use crate::runtime::*;
use crate::symbol::Symbol;

use super::state::proc_stdin_map;
use crate::value::AttrMap;

impl Interpreter {
    /// Mutating Proc methods (`.spawn`, `.run`, `.shell`) re-run a command and
    /// update the Proc's state in place. They return Bool (success) — never the
    /// Proc itself — so using them in sink context does not throw
    /// X::Proc::Unsuccessful. Per rakudo, a failed *spawn* (could not start the
    /// program at all) leaves the previous `.pid` untouched, whereas `shell`
    /// always starts a shell, so its `.pid` updates even for a missing command.
    pub(in crate::runtime) fn native_proc_mut(
        &mut self,
        attributes: AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        let new_proc = match method {
            "spawn" | "run" => self.builtin_run(&args)?,
            "shell" => self.builtin_shell(&args)?,
            _ => {
                return Err(RuntimeError::new(format!(
                    "No native mutable method '{}' on 'Proc'",
                    method
                )));
            }
        };
        let ValueView::Instance {
            attributes: new_attrs,
            ..
        } = new_proc.view()
        else {
            return Ok((Value::FALSE, attributes));
        };
        let new_pid = match new_attrs.as_map().get("pid").map(Value::view) {
            Some(ValueView::Int(p)) => p,
            _ => 0,
        };
        let exitcode = match new_attrs.as_map().get("exitcode").map(Value::view) {
            Some(ValueView::Int(c)) => c,
            _ => -1,
        };
        let mut updated = attributes;
        // A pid of 0 means the program could not be spawned at all; keep the
        // previous pid in that case (matches rakudo's `.spawn` semantics).
        if new_pid != 0 {
            for key in ["pid", "command", "out", "err"] {
                if let Some(v) = new_attrs.as_map().get(key) {
                    updated.insert(key.to_string(), v.clone());
                }
            }
        }
        if let Some(v) = new_attrs.as_map().get("exitcode") {
            updated.insert("exitcode".to_string(), v.clone());
        }
        if let Some(v) = new_attrs.as_map().get("signal") {
            updated.insert("signal".to_string(), v.clone());
        }
        Ok((Value::truth(exitcode == 0), updated))
    }

    // --- Proc::Async immutable ---

    pub(in crate::runtime) fn native_proc_async(
        &self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let proc_async_error = |class_name: &str, attrs: &[(&str, Value)]| {
            let mut ex_attrs = HashMap::new();
            for (k, v) in attrs {
                ex_attrs.insert((*k).to_string(), v.clone());
            }
            let message = class_name.to_string();
            ex_attrs.insert("message".to_string(), Value::str(message.clone()));
            let ex = Value::make_instance(Symbol::intern(class_name), ex_attrs);
            RuntimeError {
                exception: Some(Box::new(ex)),
                ..RuntimeError::new(message)
            }
        };
        match method {
            "command" => {
                let mut cmd = attributes
                    .get("cmd")
                    .cloned()
                    .unwrap_or(Value::array(Vec::new()));
                let replacement = if let ValueView::Array(items, ..) = cmd.view()
                    && items.len() == 1
                {
                    match items[0].view() {
                        ValueView::Array(inner, kind) => {
                            Some(Value::array_with_kind(inner.clone(), kind))
                        }
                        ValueView::Seq(inner) => Some(Value::real_array(inner.to_vec())),
                        ValueView::Slip(inner) => Some(Value::real_array(inner.to_vec())),
                        _ => None,
                    }
                } else {
                    None
                };
                if let Some(r) = replacement {
                    cmd = r;
                }
                // .command returns a List in Raku
                let list_items = if let ValueView::Array(items, _) = cmd.view() {
                    Some(items.clone())
                } else {
                    None
                };
                if let Some(items) = list_items {
                    Ok(Value::array_with_kind(items, ArrayKind::List))
                } else {
                    Ok(cmd)
                }
            }
            "started" => Ok(attributes.get("started").cloned().unwrap_or(Value::FALSE)),
            "w" => Ok(attributes.get("w").cloned().unwrap_or(Value::FALSE)),
            "pid" => {
                if let Some(ValueView::Int(pid)) = attributes.get("pid").map(Value::view) {
                    let promise = SharedPromise::new();
                    promise.keep(Value::int(pid), String::new(), String::new());
                    Ok(Value::promise(promise))
                } else if let Some(p) = attributes.get("ready_promise")
                    && matches!(p.view(), ValueView::Promise(_))
                {
                    Ok(p.clone())
                } else {
                    Ok(Value::promise(SharedPromise::new()))
                }
            }
            "stdout" | "stderr" => {
                if attributes
                    .get("supply_selected")
                    .is_some_and(|v| v.truthy())
                {
                    return Err(proc_async_error("X::Proc::Async::SupplyOrStd", &[]));
                }
                if attributes.get("started").is_some_and(|v| v.truthy()) {
                    return Err(proc_async_error(
                        "X::Proc::Async::TapBeforeSpawn",
                        &[("handle", Value::str_from(method))],
                    ));
                }
                if !args.is_empty() {
                    return Err(proc_async_error(
                        "X::Proc::Async::CharsOrBytes",
                        &[("handle", Value::str_from(method))],
                    ));
                }
                Ok(attributes.get(method).cloned().unwrap_or(Value::NIL))
            }
            "Supply" => {
                if attributes
                    .get("stdout_selected")
                    .is_some_and(|v| v.truthy())
                    || attributes
                        .get("stderr_selected")
                        .is_some_and(|v| v.truthy())
                {
                    return Err(proc_async_error("X::Proc::Async::SupplyOrStd", &[]));
                }
                Ok(attributes.get("supply").cloned().unwrap_or(Value::NIL))
            }
            _ => Ok(Value::NIL),
        }
    }

    // --- Proc immutable ---

    pub(in crate::runtime) fn native_proc(&self, attributes: &AttrMap, method: &str) -> Value {
        // For live procs (with :in), finalize the child on exitcode/out/err/signal access
        let is_live = matches!(
            attributes.get("live").map(Value::view),
            Some(ValueView::Bool(true))
        );
        if is_live && matches!(method, "exitcode" | "out" | "err" | "signal" | "in") {
            let pid = match attributes.get("pid").map(Value::view) {
                Some(ValueView::Int(p)) => p,
                _ => -1,
            };

            // .in returns the stored IO::Pipe for stdin
            if method == "in" {
                return attributes.get("in").cloned().unwrap_or(Value::NIL);
            }

            // .out / .err on a live proc: return a "live" IO::Pipe
            if method == "out" || method == "err" {
                let mut pipe_attrs = HashMap::new();
                pipe_attrs.insert("live-pid".to_string(), Value::int(pid));
                pipe_attrs.insert("pipe-type".to_string(), Value::str(method.to_string()));
                if matches!(
                    attributes.get("bin").map(Value::view),
                    Some(ValueView::Bool(true))
                ) {
                    pipe_attrs.insert("bin".to_string(), Value::TRUE);
                }
                return Value::make_instance(crate::symbol::Symbol::intern("IO::Pipe"), pipe_attrs);
            }

            // Finalize the child if not yet done
            if let Ok(mut map) = super::super::builtins_system::live_proc_map().lock()
                && let Some(mut state) = map.remove(&pid)
            {
                // Close stdin if still open
                if let Ok(mut stdin_map) = proc_stdin_map().lock() {
                    stdin_map.remove(&(pid as u32));
                }
                // Read stdout/stderr
                let captured_out: Option<String> = if state.capture_out {
                    state
                        .child
                        .stdout
                        .take()
                        .map(|mut s: std::process::ChildStdout| {
                            let mut buf = String::new();
                            use std::io::Read;
                            let _ = s.read_to_string(&mut buf);
                            buf
                        })
                } else {
                    None
                };
                let captured_err: Option<String> = if state.capture_err {
                    state
                        .child
                        .stderr
                        .take()
                        .map(|mut s: std::process::ChildStderr| {
                            let mut buf = String::new();
                            use std::io::Read;
                            let _ = s.read_to_string(&mut buf);
                            buf
                        })
                } else {
                    None
                };
                let (exitcode, signal): (i64, i64) = match state.child.wait() {
                    Ok(status) => {
                        let exitcode = status.code().unwrap_or(-1) as i64;
                        #[cfg(unix)]
                        let signal = {
                            use std::os::unix::process::ExitStatusExt;
                            status.signal().unwrap_or(0) as i64
                        };
                        #[cfg(not(unix))]
                        let signal = 0i64;
                        (exitcode, signal)
                    }
                    Err(_) => (-1i64, 0i64),
                };
                // Cache the finalized results
                if let Ok(mut fmap) = super::super::builtins_system::finalized_proc_map().lock() {
                    fmap.insert(
                        pid,
                        super::super::builtins_system::FinalizedProc {
                            exitcode,
                            signal,
                            captured_out,
                            captured_err,
                        },
                    );
                }
            }

            // Look up from finalized cache
            if let Ok(fmap) = super::super::builtins_system::finalized_proc_map().lock()
                && let Some(finalized) = fmap.get(&pid)
            {
                return match method {
                    "exitcode" => Value::int(finalized.exitcode),
                    "signal" => Value::int(finalized.signal),
                    "out" => {
                        if let Some(ref content) = finalized.captured_out {
                            Self::make_io_pipe(content.clone())
                        } else {
                            Value::NIL
                        }
                    }
                    "err" => {
                        if let Some(ref content) = finalized.captured_err {
                            Self::make_io_pipe(content.clone())
                        } else {
                            Value::NIL
                        }
                    }
                    _ => Value::NIL,
                };
            }
        }
        match method {
            "exitcode" => attributes.get("exitcode").cloned().unwrap_or(Value::NIL),
            "signal" => attributes.get("signal").cloned().unwrap_or(Value::int(0)),
            "command" => {
                let cmd = attributes
                    .get("command")
                    .cloned()
                    .unwrap_or(Value::array(Vec::new()));
                // .command returns a List in Raku
                let list_items = if let ValueView::Array(items, _) = cmd.view() {
                    Some(items.clone())
                } else {
                    None
                };
                if let Some(items) = list_items {
                    Value::array_with_kind(items, ArrayKind::List)
                } else {
                    cmd
                }
            }
            "pid" => attributes.get("pid").cloned().unwrap_or(Value::NIL),
            "err" => attributes.get("err").cloned().unwrap_or(Value::NIL),
            "out" => attributes.get("out").cloned().unwrap_or(Value::NIL),
            "in" => attributes.get("in").cloned().unwrap_or(Value::NIL),
            "Numeric" | "Int" => attributes
                .get("exitcode")
                .cloned()
                .unwrap_or(Value::int(-1)),
            "Bool" => {
                let exitcode = match attributes.get("exitcode").map(Value::view) {
                    Some(ValueView::Int(c)) => c,
                    _ => -1,
                };
                Value::truth(exitcode == 0)
            }
            _ => Value::NIL,
        }
    }
}
