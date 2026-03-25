use crate::runtime::*;
use crate::symbol::Symbol;

use super::state::proc_stdin_map;

impl Interpreter {
    // --- Proc::Async immutable ---

    pub(in crate::runtime) fn native_proc_async(
        &self,
        attributes: &HashMap<String, Value>,
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
                if let Value::Array(items, ..) = &cmd
                    && items.len() == 1
                {
                    cmd = match &items[0] {
                        Value::Array(inner, kind) => Value::Array(inner.clone(), *kind),
                        Value::Seq(inner) => Value::real_array(inner.to_vec()),
                        Value::Slip(inner) => Value::real_array(inner.to_vec()),
                        _ => cmd,
                    };
                }
                // .command returns a List in Raku
                if let Value::Array(items, _) = cmd {
                    Ok(Value::Array(items, ArrayKind::List))
                } else {
                    Ok(cmd)
                }
            }
            "started" => Ok(attributes
                .get("started")
                .cloned()
                .unwrap_or(Value::Bool(false))),
            "w" => Ok(attributes.get("w").cloned().unwrap_or(Value::Bool(false))),
            "pid" => {
                if let Some(Value::Int(pid)) = attributes.get("pid") {
                    let promise = SharedPromise::new();
                    promise.keep(Value::Int(*pid), String::new(), String::new());
                    Ok(Value::Promise(promise))
                } else if let Some(promise @ Value::Promise(_)) = attributes.get("ready_promise") {
                    Ok(promise.clone())
                } else {
                    Ok(Value::Promise(SharedPromise::new()))
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
                Ok(attributes.get(method).cloned().unwrap_or(Value::Nil))
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
                Ok(attributes.get("supply").cloned().unwrap_or(Value::Nil))
            }
            _ => Ok(Value::Nil),
        }
    }

    // --- Proc immutable ---

    pub(in crate::runtime) fn native_proc(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Value {
        // For live procs (with :in), finalize the child on exitcode/out/err/signal access
        let is_live = matches!(attributes.get("live"), Some(Value::Bool(true)));
        if is_live && matches!(method, "exitcode" | "out" | "err" | "signal" | "in") {
            let pid = match attributes.get("pid") {
                Some(Value::Int(p)) => *p,
                _ => -1,
            };

            // .in returns the stored IO::Pipe for stdin
            if method == "in" {
                return attributes.get("in").cloned().unwrap_or(Value::Nil);
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
                    "exitcode" => Value::Int(finalized.exitcode),
                    "signal" => Value::Int(finalized.signal),
                    "out" => {
                        if let Some(ref content) = finalized.captured_out {
                            Self::make_io_pipe(content.clone())
                        } else {
                            Value::Nil
                        }
                    }
                    "err" => {
                        if let Some(ref content) = finalized.captured_err {
                            Self::make_io_pipe(content.clone())
                        } else {
                            Value::Nil
                        }
                    }
                    _ => Value::Nil,
                };
            }
        }
        match method {
            "exitcode" => attributes.get("exitcode").cloned().unwrap_or(Value::Nil),
            "signal" => attributes.get("signal").cloned().unwrap_or(Value::Int(0)),
            "command" => {
                let cmd = attributes
                    .get("command")
                    .cloned()
                    .unwrap_or(Value::array(Vec::new()));
                // .command returns a List in Raku
                if let Value::Array(items, _) = cmd {
                    Value::Array(items, ArrayKind::List)
                } else {
                    cmd
                }
            }
            "pid" => attributes.get("pid").cloned().unwrap_or(Value::Nil),
            "err" => attributes.get("err").cloned().unwrap_or(Value::Nil),
            "out" => attributes.get("out").cloned().unwrap_or(Value::Nil),
            "in" => attributes.get("in").cloned().unwrap_or(Value::Nil),
            "Numeric" | "Int" => attributes
                .get("exitcode")
                .cloned()
                .unwrap_or(Value::Int(-1)),
            "Bool" => {
                let exitcode = match attributes.get("exitcode") {
                    Some(Value::Int(c)) => *c,
                    _ => -1,
                };
                Value::Bool(exitcode == 0)
            }
            "Str" | "gist" => {
                let exitcode = match attributes.get("exitcode") {
                    Some(Value::Int(c)) => *c,
                    _ => -1,
                };
                Value::str(exitcode.to_string())
            }
            _ => Value::Nil,
        }
    }
}
