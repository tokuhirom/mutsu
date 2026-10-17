use crate::runtime::*;

use super::state::proc_stdin_map;
use crate::value::AttrMap;

impl Interpreter {
    /// `Proc.spawn` / `Proc.shell`: re-run a command and answer the
    /// attributes of the receiver it updates, with whether the child
    /// succeeded. They answer Bool (success), never the Proc itself, so using
    /// them in sink context does not throw X::Proc::Unsuccessful. Per rakudo,
    /// a failed *spawn* (could not start the program at all) leaves the
    /// previous `.pid` untouched, whereas `shell` always starts a shell, so its
    /// `.pid` updates even for a missing command.
    // Cost: the child's run, plus O(a) for a = the attributes copied.
    pub(crate) fn proc_respawn(
        &mut self,
        args: &[Value],
        shell: bool,
    ) -> Result<(Value, Vec<(Symbol, Option<Value>)>), RuntimeError> {
        let new_proc = if shell {
            self.builtin_shell(args)?
        } else {
            self.builtin_run(args)?
        };
        let ValueView::Instance {
            attributes: new_attrs,
            ..
        } = new_proc.view()
        else {
            return Ok((Value::FALSE, Vec::new()));
        };
        let new_attrs = new_attrs.as_map();
        let int_of = |key: &str, default: i64| match new_attrs.get(key).map(Value::view) {
            Some(ValueView::Int(n)) => n,
            _ => default,
        };
        let (new_pid, exitcode, signal) = (int_of("pid", 0), int_of("exitcode", -1), int_of("signal", 0));
        let mut updates = Vec::new();
        // A pid of 0 means the program could not be spawned at all; keep the
        // previous pid in that case (matches rakudo's `.spawn` semantics).
        let keys: &[&str] = if new_pid != 0 {
            &["pid", "command", "out", "err", "exitcode", "signal"]
        } else {
            &["exitcode", "signal"]
        };
        for &key in keys {
            if let Some(v) = new_attrs.get(key) {
                updates.push((Symbol::intern(key), Some(v.clone())));
            }
        }
        // `.spawn` answers whether the child succeeded; a signal-killed child
        // reports `exitcode = 0`, so the signal decides there.
        Ok((Value::truth(exitcode == 0 && signal == 0), updates))
    }

    // --- Proc::Async immutable ---

    pub(in crate::runtime) fn native_proc_async_read(
        &self,
        attributes: &AttrMap,
        op: crate::runtime::native_proc_async::ProcAsyncOp,
    ) -> Result<Value, RuntimeError> {
        use crate::runtime::native_proc_async::ProcAsyncOp;
        match op {
            ProcAsyncOp::Command => {
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
            ProcAsyncOp::Started => Ok(attributes.get("started").cloned().unwrap_or(Value::FALSE)),
            ProcAsyncOp::W => Ok(attributes.get("w").cloned().unwrap_or(Value::FALSE)),
            ProcAsyncOp::Pid => {
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
            _ => Ok(Value::NIL),
        }
    }

    // --- Proc immutable ---

    /// The finalized `exitcode`, `signal`, `out` or `err` of a live proc (one
    /// run with `:in`), whose child is reaped on the first such read; `in`
    /// answers its stdin pipe. `None` for a proc that is not live.
    // Cost: O(o), o = bytes of output collected when the child is reaped.
    pub(crate) fn proc_live_stream(attributes: &AttrMap, field: ProcStream) -> Option<Value> {
        let is_live = matches!(
            attributes.get("live").map(Value::view),
            Some(ValueView::Bool(true))
        );
        if !is_live {
            return None;
        }
        let pid = match attributes.get("pid").map(Value::view) {
            Some(ValueView::Int(p)) => p,
            _ => -1,
        };
        match field {
            // .in returns the stored IO::Pipe for stdin
            ProcStream::In => return Some(attributes.get("in").cloned().unwrap_or(Value::NIL)),
            // .out / .err on a live proc: return a "live" IO::Pipe
            ProcStream::Out | ProcStream::Err => {
                let mut pipe_attrs = HashMap::new();
                pipe_attrs.insert("live-pid".to_string(), Value::int(pid));
                let kind = if field == ProcStream::Out { "out" } else { "err" };
                pipe_attrs.insert("pipe-type".to_string(), Value::str_from(kind));
                if matches!(
                    attributes.get("bin").map(Value::view),
                    Some(ValueView::Bool(true))
                ) {
                    pipe_attrs.insert("bin".to_string(), Value::TRUE);
                }
                return Some(Value::make_instance(
                    crate::symbol::Symbol::intern("IO::Pipe"),
                    pipe_attrs,
                ));
            }
            ProcStream::Exitcode | ProcStream::Signal => {}
        }
        Self::finalize_live_proc(pid);
        // Look up from finalized cache
        let fmap = super::super::builtins_system::finalized_proc_map().lock().ok()?;
        let finalized = fmap.get(&pid)?;
        Some(match field {
            ProcStream::Signal => Value::int(finalized.signal),
            _ => Value::int(finalized.exitcode),
        })
    }

    /// Reap the live child `pid` once: close its stdin, collect its output and
    /// cache its status in the finalized-proc map.
    // Cost: O(o), o = bytes of output collected.
    fn finalize_live_proc(pid: i64) {
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
                .and_then(|s| crate::runtime::exit_aware_read::slurp_pipe(s).ok())
        } else {
            None
        };
        let captured_err: Option<String> = if state.capture_err {
            state
                .child
                .stderr
                .take()
                .and_then(|s| crate::runtime::exit_aware_read::slurp_pipe(s).ok())
        } else {
            None
        };
        let (exitcode, signal): (i64, i64) = match crate::runtime::builtins_system_proc_wait::wait_child(&mut state.child)
            // A pending worker `exit` (#13092): the interpreter is
            // halted and unwinds; the status read here is discarded.
            .unwrap_or_else(|_| Err(std::io::Error::other("exit pending")))
        {
            Ok(status) => super::super::builtins_system::exit_status_parts(&status),
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
    }
}

/// The streams and status of a live proc that its child's reaping decides.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum ProcStream {
    Exitcode,
    Signal,
    Out,
    Err,
    In,
}
