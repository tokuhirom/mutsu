//! The process ops of the Asynchronous `nqp::` family (#11502, ADR-11502
//! §2.4.3): `nqp::spawnprocasync`, `nqp::permit` and `nqp::killprocasync`.
//!
//! `nqp::spawnprocasync($queue, $program, $argv, $cwd, $env, $callbacks)`
//! spawns `$program` with `$argv` (whose first element is the program's own
//! name) and answers a `BOOTIO` handle. The child's events go onto `$queue`
//! as `[callback, args...]`, each callback taken from the `$callbacks` hash:
//!
//! - `ready`: `[ready, BOOTIntArray[stdout fd, stderr fd], BOOTInt pid]`, a fd
//!   being -1 for a stream nobody reads;
//! - `stdout_bytes` / `stderr_bytes`: `[cb, BOOTInt seq, Buf, BOOTStr]` per
//!   chunk (a `Buf` of `buf_type`'s class), then `[cb, seq, BOOTStr, BOOTStr]`
//!   at the end of the stream, or with the read error in the last slot;
//! - `done`: `[done, BOOTInt status]`, the raw wait status (`exit << 8`, or
//!   the signal number);
//! - `error`: `[error, BOOTStr message, BOOTInt -errno]` when the spawn fails.
//!
//! A stream is piped only when its `*_bytes` callback is present, and read
//! only once `nqp::permit($handle, 1 | 2, $n)` allows it: `$n` chunks, or
//! all of them for -1. stdin is a pipe when `write` is present, else closed.
//! The spawn and the read loop are `Proc::Async.start`'s
//! (`runtime::proc_spawn`); only what is done with a chunk differs.

use crate::runtime::proc_spawn::{ProcSpawn, ProcStdin, pump_pipe};
use crate::runtime::{Interpreter, RuntimeError};
use crate::symbol::Symbol;
use crate::value::{NqpElemKind, SharedChannel, Value, ValueView};
use std::collections::HashMap;
use std::io::Read;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::{Arc, Condvar, LazyLock, Mutex, PoisonError};

/// The handle class `nqp::spawnprocasync` answers, as in MoarVM.
pub(crate) const PROC_HANDLE_CLASS: &str = "BOOTIO";
/// The attribute of a handle holding its process task id.
const PROC_ATTR: &str = "proc";

/// The reader side of one output stream.
#[derive(Default)]
struct StreamState {
    /// How many more chunks the stream may read: -1 for any number.
    permits: i64,
    /// The reader waits for a permit.
    parked: bool,
    /// The reader reached the end of the stream.
    finished: bool,
}

/// A stream's [`StreamState`] and the condition its changes signal.
#[derive(Default)]
struct StreamGate {
    state: Mutex<StreamState>,
    changed: Condvar,
}

impl StreamGate {
    /// Wait until a chunk may be read, and take one permit.
    // Cost: O(1), plus the wait.
    fn take(&self) {
        let mut state = self.state.lock().unwrap_or_else(PoisonError::into_inner);
        while state.permits == 0 {
            state.parked = true;
            self.changed.notify_all();
            state = crate::gc::block_quiescent(|| {
                self.changed
                    .wait(state)
                    .unwrap_or_else(PoisonError::into_inner)
            });
        }
        state.parked = false;
        if state.permits > 0 {
            state.permits -= 1;
        }
    }

    // Cost: O(1).
    fn set_permits(&self, n: i64) {
        self.state
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .permits = n;
        self.changed.notify_all();
    }

    // Cost: O(1).
    fn finish(&self) {
        self.state
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .finished = true;
        self.changed.notify_all();
    }

    /// Wait until the reader has nothing more to push for now: it reached
    /// the end of the stream, or it waits for a permit.
    // Cost: O(1), plus the wait.
    fn settle(&self) {
        let mut state = self.state.lock().unwrap_or_else(PoisonError::into_inner);
        while !state.finished && !state.parked {
            state = crate::gc::block_quiescent(|| {
                self.changed
                    .wait(state)
                    .unwrap_or_else(PoisonError::into_inner)
            });
        }
    }
}

/// One output stream of a spawned child: its pipe until a reader takes it.
struct ProcStream {
    /// `None` once a reader took it.
    pipe: Mutex<Option<Box<dyn Read + Send>>>,
    callback: Value,
    gate: StreamGate,
}

/// A child spawned by `nqp::spawnprocasync`.
struct ProcTask {
    pid: u32,
    queue: SharedChannel,
    buf_class: Symbol,
    /// stdout, stderr.
    streams: [Option<ProcStream>; 2],
    /// Which readers were started; written under the stream's pipe lock.
    started: [AtomicBool; 2],
    /// The child's stdin pipe (`write`), held open for the child.
    // TODO: read by `nqp::asyncwritebytes` once the write ops land (#11502).
    #[allow(dead_code)]
    stdin: Mutex<Option<std::process::ChildStdin>>,
}

static NEXT_TASK: AtomicU64 = AtomicU64::new(1);
static TASKS: LazyLock<Mutex<HashMap<u64, Arc<ProcTask>>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

/// An operand with any argument wrapper and container stripped.
fn operand(v: &Value) -> Value {
    crate::runtime::types::unwrap_varref_value(v.clone()).deref_container()
}

/// The task of a `BOOTIO` handle; `None` once its child is reaped. Any
/// other operand dies with `refusal`, MoarVM's message for the op.
// Cost: O(1).
fn task_of(handle: &Value, refusal: &'static str) -> Result<Option<Arc<ProcTask>>, RuntimeError> {
    let handle = operand(handle);
    let id = match handle.view() {
        ValueView::Instance {
            class_name,
            attributes,
            ..
        } if class_name.as_str() == PROC_HANDLE_CLASS => attributes
            .as_map()
            .get(PROC_ATTR)
            .and_then(Value::as_int),
        _ => None,
    };
    let Some(id) = id else {
        return Err(RuntimeError::new(refusal));
    };
    let tasks = TASKS.lock().unwrap_or_else(PoisonError::into_inner);
    Ok(tasks.get(&(id as u64)).cloned())
}

/// The string entries of an nqp hash operand.
// Cost: O(k), k = entries.
fn string_entries(v: &Value) -> Vec<(String, String)> {
    match operand(v).view() {
        ValueView::Hash(map) => map
            .iter()
            .map(|(k, v)| (k.to_string(), operand(v).to_string_value()))
            .collect(),
        _ => Vec::new(),
    }
}

/// MoarVM's (libuv's) spelling of an OS error: `no such file or directory`.
// Cost: O(m), m = message length.
fn uv_error_text(e: &std::io::Error) -> String {
    let text = e.to_string();
    let text = text.split(" (os error").next().unwrap_or(&text);
    text.to_lowercase()
}

/// The raw wait status MoarVM reports: `exit << 8`, or the signal number.
// Cost: O(1).
fn wait_status(status: &std::process::ExitStatus) -> i64 {
    let (code, signal) = crate::runtime::builtins_system::exit_status_parts(status);
    if signal != 0 { signal } else { code << 8 }
}

/// The `BOOTStr` type object, the null string of a pushed event.
// Cost: O(1).
fn boot_str_type() -> Value {
    Value::package(Symbol::intern("BOOTStr"))
}

#[cfg(unix)]
fn pipe_fd<P: std::os::fd::AsRawFd>(pipe: Option<&P>) -> i64 {
    pipe.map_or(-1, |p| i64::from(p.as_raw_fd()))
}

#[cfg(not(unix))]
fn pipe_fd<P>(pipe: Option<&P>) -> i64 {
    if pipe.is_some() { 0 } else { -1 }
}

impl ProcTask {
    /// Push `[callback, args...]`.
    // Cost: O(a), a = args.len().
    fn push(&self, callback: &Value, args: Vec<Value>) {
        let mut items = Vec::with_capacity(args.len() + 1);
        items.push(callback.clone());
        items.extend(args);
        self.queue.send(Value::vm_array(items));
    }

    /// Start the reader of stream `idx` (0 = stdout, 1 = stderr) unless it
    /// already runs. The reader pushes each chunk, then the end of the stream.
    // Cost: O(1) here; the reader pays O(n), n = bytes read.
    fn start_reader(self: &Arc<Self>, idx: usize) -> Result<(), RuntimeError> {
        let Some(stream) = &self.streams[idx] else {
            return Ok(());
        };
        let mut slot = stream.pipe.lock().unwrap_or_else(PoisonError::into_inner);
        let Some(pipe) = slot.take() else {
            return Ok(());
        };
        self.started[idx].store(true, Ordering::Release);
        drop(slot);
        let task = Arc::clone(self);
        // Pushes `Value`s (Gc nodes): a registered GC mutator, its pipe reads
        // and permit waits quiescent. Runs no user VM code. Detached: the
        // waiter settles on the stream's gate instead of joining it.
        crate::runtime::builtins_system::try_spawn_gc_helper_thread("nqp-proc-read", move || {
            let Some(stream) = &task.streams[idx] else {
                return;
            };
            let mut seq = 0i64;
            stream.gate.take();
            let error = pump_pipe(pipe, |chunk| {
                let buf = crate::value::value_buf::make_buf_from_bytes(task.buf_class, chunk);
                task.push(
                    &stream.callback,
                    vec![Interpreter::boot_int(seq), buf, boot_str_type()],
                );
                seq += 1;
                stream.gate.take();
                true
            });
            let last = match error {
                Some(e) => Interpreter::boot_str(&uv_error_text(&e)),
                None => boot_str_type(),
            };
            task.push(
                &stream.callback,
                vec![Interpreter::boot_int(seq), boot_str_type(), last],
            );
            stream.gate.finish();
        })
        .map(drop)
        .map_err(crate::runtime::builtins_system::refused_thread_error)
    }
}

impl Interpreter {
    /// `nqp::spawnprocasync` / `nqp::permit` / `nqp::killprocasync`.
    pub(crate) fn call_nqp_op_proc(
        &mut self,
        op: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        Some(match op {
            // Cost: O(a + e), a = arguments, e = environment entries, plus
            // the fork/exec.
            "spawnprocasync" => self.nqp_spawnprocasync(args),
            // nqp::permit($handle, $channel, $permits): let stream `$channel`
            // (1 stdout, 2 stderr) read `$permits` more chunks (-1: any
            // number, 0: pause); answer the handle.
            // Cost: O(1).
            "permit" => nqp_permit(args),
            // nqp::killprocasync($handle, $signal): send `$signal` to the
            // child; answer 0.
            // Cost: O(1).
            "killprocasync" => nqp_killprocasync(args),
            _ => return None,
        })
    }

    fn nqp_spawnprocasync(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 6 {
            return Err(RuntimeError::new(format!(
                "Arg count {} doesn't equal required operand count 6 for op 'spawnprocasync'",
                args.len()
            )));
        }
        let queue = crate::runtime::nqp_ops_async::queue_operand(
            "spawnprocasync",
            "target queue",
            &args[0],
        )?;
        let program = operand(&args[1]).to_string_value();
        let argv: Vec<String> = match operand(&args[2]).view() {
            ValueView::Array(items, _) => items
                .iter()
                .skip(1)
                .map(|v| operand(v).to_string_value())
                .collect(),
            _ => Vec::new(),
        };
        let cwd = operand(&args[3]).to_string_value();
        let env = string_entries(&args[4]);
        let callbacks: HashMap<String, Value> = match operand(&args[5]).view() {
            ValueView::Hash(map) => map.iter().map(|(k, v)| (k.to_string(), v.clone())).collect(),
            _ => HashMap::new(),
        };
        for unsupported in ["stdin_fd", "stdout_fd", "stderr_fd", "merge_bytes", "pty"] {
            if callbacks.contains_key(unsupported) {
                // TODO: binding a child's stdio to a given fd, merging its
                // output, and a pty are not modelled yet (#11502).
                return Err(RuntimeError::new(format!(
                    "spawnprocasync: the '{unsupported}' option is not supported"
                )));
            }
        }
        let callback = |key: &str| callbacks.get(key).cloned();
        let buf_class = match callback("buf_type").map(|v| operand(&v)) {
            Some(v) => match v.view() {
                ValueView::Instance { class_name, .. } | ValueView::Package(class_name) => {
                    class_name
                }
                _ => Symbol::intern("Buf"),
            },
            None => Symbol::intern("Buf"),
        };
        let out_cb = callback("stdout_bytes");
        let err_cb = callback("stderr_bytes");
        let spawn = ProcSpawn {
            program: program.clone(),
            args: argv,
            cwd: Some(cwd),
            env: Some(env),
            stdin: if callbacks.contains_key("write") {
                ProcStdin::Piped
            } else {
                ProcStdin::Null
            },
            pipe_stdout: out_cb.is_some(),
            pipe_stderr: err_cb.is_some(),
        };
        let id = NEXT_TASK.fetch_add(1, Ordering::Relaxed);
        let mut attrs = HashMap::new();
        attrs.insert(PROC_ATTR.to_string(), Value::int(id as i64));
        let handle = Value::make_instance(Symbol::intern(PROC_HANDLE_CLASS), attrs);
        let mut child = match spawn.spawn() {
            Ok(child) => child,
            Err(e) => {
                if let Some(cb) = callback("error") {
                    let errno = e.raw_os_error().unwrap_or(0);
                    let message = format!(
                        "Failed to spawn process {program}: {} (error code {})",
                        uv_error_text(&e),
                        -errno
                    );
                    queue.send(Value::vm_array(vec![
                        cb,
                        Interpreter::boot_str(&message),
                        Interpreter::boot_int(-i64::from(errno)),
                    ]));
                }
                return Ok(handle);
            }
        };
        let fds = vec![
            Value::int(pipe_fd(child.stdout.as_ref())),
            Value::int(pipe_fd(child.stderr.as_ref())),
        ];
        let stream = |pipe: Option<Box<dyn Read + Send>>, cb: Option<Value>| {
            Some(ProcStream {
                pipe: Mutex::new(pipe),
                callback: cb?,
                gate: StreamGate::default(),
            })
        };
        let task = Arc::new(ProcTask {
            pid: child.id(),
            queue: queue.clone(),
            buf_class,
            streams: [
                stream(
                    child.stdout.take().map(|p| Box::new(p) as Box<dyn Read + Send>),
                    out_cb,
                ),
                stream(
                    child.stderr.take().map(|p| Box::new(p) as Box<dyn Read + Send>),
                    err_cb,
                ),
            ],
            started: [AtomicBool::new(false), AtomicBool::new(false)],
            stdin: Mutex::new(child.stdin.take()),
        });
        TASKS
            .lock()
            .unwrap_or_else(PoisonError::into_inner)
            .insert(id, Arc::clone(&task));
        if let Some(cb) = callback("ready") {
            task.push(
                &cb,
                vec![
                    Value::nqp_typed_list(fds, NqpElemKind::Int),
                    Interpreter::boot_int(i64::from(task.pid)),
                ],
            );
        }
        let done = callback("done");
        let waiter_task = Arc::clone(&task);
        // Pushes `Value`s: a registered GC mutator, the child wait and the
        // reader joins quiescent. Runs no user VM code.
        let waiter = crate::runtime::builtins_system::try_spawn_gc_helper_thread(
            "nqp-proc-wait",
            move || {
                let status = crate::gc::block_quiescent(|| child.wait());
                // The output a permitted reader can still push comes before
                // `done`.
                for (idx, stream) in waiter_task.streams.iter().enumerate() {
                    if let Some(stream) = stream
                        && waiter_task.started[idx].load(Ordering::Acquire)
                    {
                        stream.gate.settle();
                    }
                }
                TASKS
                    .lock()
                    .unwrap_or_else(PoisonError::into_inner)
                    .remove(&id);
                if let Some(done) = done {
                    let status = status.as_ref().map_or(-1, wait_status);
                    waiter_task.push(&done, vec![Interpreter::boot_int(status)]);
                }
            },
        );
        if let Err(e) = waiter {
            TASKS
                .lock()
                .unwrap_or_else(PoisonError::into_inner)
                .remove(&id);
            return Err(crate::runtime::builtins_system::refused_thread_error(e));
        }
        Ok(handle)
    }
}

fn nqp_permit(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 3 {
        return Err(RuntimeError::new(format!(
            "Arg count {} doesn't equal required operand count 3 for op 'permit'",
            args.len()
        )));
    }
    let handle = args[0].clone();
    // A reaped child's streams are gone: there is nothing left to permit.
    let Some(task) = task_of(&handle, "Can only permit an AsyncTask handle")? else {
        return Ok(handle);
    };
    let channel = crate::runtime::to_int(&operand(&args[1]));
    let permits = crate::runtime::to_int(&operand(&args[2]));
    let idx = match channel {
        1 => 0,
        2 => 1,
        // TODO: channel 0 is the merged output, which needs `merge_bytes`
        // (not modelled yet, #11502).
        _ => {
            return Err(RuntimeError::new(format!(
                "permit: channel {channel} is not supported"
            )));
        }
    };
    if let Some(stream) = &task.streams[idx] {
        stream.gate.set_permits(permits.max(-1));
        if permits != 0 {
            task.start_reader(idx)?;
        }
    }
    Ok(handle)
}

fn nqp_killprocasync(args: &[Value]) -> Result<Value, RuntimeError> {
    if args.len() != 2 {
        return Err(RuntimeError::new(format!(
            "Arg count {} doesn't equal required operand count 2 for op 'killprocasync'",
            args.len()
        )));
    }
    let signal = crate::runtime::to_int(&operand(&args[1]));
    // Killing a reaped child is not an error.
    if let Some(task) = task_of(&args[0], "killprocasync requires a process handle")? {
        Interpreter::send_signal(i64::from(task.pid), signal);
    }
    Ok(Value::int(0))
}
