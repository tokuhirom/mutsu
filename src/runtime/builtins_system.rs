use super::*;
use crate::symbol::Symbol;
use std::sync::OnceLock;

/// Stack size for worker threads that execute user code (Promise / `start` /
/// Supply callbacks). Matches the main thread's stack (see `main.rs`): the
/// default ~2 MiB thread stack overflows on deep VM recursion, which shows up
/// as debug-build-only crashes (release frames are smaller and fit in 2 MiB).
pub(crate) const USER_THREAD_STACK_SIZE: usize = 256 * 1024 * 1024;

/// Spawn a worker thread with a large stack for running user code, so deep VM
/// recursion does not overflow the default thread stack.
pub(crate) fn spawn_user_thread<F, T>(f: F) -> std::thread::JoinHandle<T>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    std::thread::Builder::new()
        .stack_size(USER_THREAD_STACK_SIZE)
        .spawn(f)
        .expect("failed to spawn worker thread")
}

/// State for a live child process (when `:in` is used with `run`).
pub(super) struct LiveProcState {
    pub(super) child: std::process::Child,
    pub(super) capture_out: bool,
    pub(super) capture_err: bool,
}

type LiveProcMap = std::sync::Mutex<HashMap<i64, LiveProcState>>;

pub(super) fn live_proc_map() -> &'static LiveProcMap {
    static MAP: OnceLock<LiveProcMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Cached results from finalized live procs.
pub(super) struct FinalizedProc {
    pub exitcode: i64,
    pub signal: i64,
    pub captured_out: Option<String>,
    pub captured_err: Option<String>,
}

type FinalizedProcMap = std::sync::Mutex<HashMap<i64, FinalizedProc>>;

pub(super) fn finalized_proc_map() -> &'static FinalizedProcMap {
    static MAP: OnceLock<FinalizedProcMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Per-pipe buffered-read state for IO::Pipe instances returned by
/// `shell(..., :out)` / `run(..., :out)`. Keyed by the `pipe-id` attribute
/// on the IO::Pipe instance so that repeated method calls on the same
/// logical pipe share cursor state even after the instance value is cloned.
pub(crate) struct IoPipeState {
    pub(crate) content: String,
    pub(crate) cursor: usize,
    pub(crate) closed: bool,
}

type IoPipeStateMap = std::sync::Mutex<HashMap<i64, IoPipeState>>;

pub(crate) fn io_pipe_state_map() -> &'static IoPipeStateMap {
    static MAP: OnceLock<IoPipeStateMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Maps pipe-id to the parent Proc instance so that IO::Pipe.close and
/// IO::Pipe.proc can return the owning Proc.
type PipeProcMap = std::sync::Mutex<HashMap<i64, Value>>;

pub(crate) fn pipe_proc_map() -> &'static PipeProcMap {
    static MAP: OnceLock<PipeProcMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Maps a child PID to the owning Proc instance, so that `.in`/`.out`/`.err`
/// pipes (which carry the PID, not a pipe-id) can return the exact parent Proc
/// from `.close` — preserving object identity for `$p.out.close === $p`.
type ProcByPidMap = std::sync::Mutex<HashMap<i64, Value>>;

pub(crate) fn proc_by_pid_map() -> &'static ProcByPidMap {
    static MAP: OnceLock<ProcByPidMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Options extracted from named arguments for run/shell.
pub(super) struct ProcOptions {
    pub(super) cwd: Option<String>,
    pub(super) env: HashMap<String, String>,
    pub(super) capture_err: bool,
    pub(super) capture_out: bool,
    pub(super) capture_in: bool,
    pub(super) in_pipe_pid: Option<i64>,
    pub(super) in_pipe_content: Option<String>,
    pub(super) bin: bool,
    pub(super) win_verbatim_args: bool,
    /// When :out is given an IO::Handle, store the handle ID to dup the fd
    pub(super) out_handle_id: Option<usize>,
    /// When :merge is True, redirect stderr to stdout
    pub(super) merge: bool,
}

impl Interpreter {
    pub(crate) fn spawn_callable_promise(&mut self, block: Value, class_name: Symbol) -> Value {
        use crate::value::SharedPromise;

        let promise = SharedPromise::new_with_class(class_name);
        let ret = Value::Promise(promise.clone());
        let thread_interp = self.clone_for_thread();
        let parent_handles_snapshot: std::collections::HashSet<usize> =
            self.io_handles().map.keys().copied().collect();

        // Raku gives each start block fresh $/ and $!.
        // Strip these from the closure's captured env so they don't override
        // the fresh Nil values set in clone_for_thread.
        let block = if let Value::Sub(ref data) = block {
            let mut new_data = (**data).clone();
            new_data.env.remove("/");
            new_data.env.remove("!");
            new_data.env.remove("$/");
            new_data.env.remove("$!");
            Value::Sub(std::sync::Arc::new(new_data))
        } else {
            block
        };

        spawn_user_thread(move || {
            // CP-3 collapse: the thread's cloned Interpreter *is* the VM — run the
            // block on it directly instead of wrapping it in a sub-VM.
            let mut thread_interp = thread_interp;
            // Worker bodies run via `call_value` without the main thread's
            // `run_top` panic boundary, so guard them here: a Rust panic in
            // user code becomes a catchable broken-Promise error (X::AdHoc)
            // instead of silently killing the thread (hanging `await`) or
            // aborting the process.
            let result = crate::vm::guard_worker_panic(|| thread_interp.call_value(block, vec![]));
            // Transfer any handles opened by this thread back to the awaiter.
            let mut new_handles: Vec<(usize, IoHandleState)> = Vec::new();
            let new_ids: Vec<usize> = thread_interp
                .io_handles()
                .map
                .keys()
                .copied()
                .filter(|id| !parent_handles_snapshot.contains(id))
                .collect();
            for id in new_ids {
                if let Some(state) = thread_interp.io_handles_mut().map.remove(&id) {
                    new_handles.push((id, state));
                }
            }
            let next_id = thread_interp.io_handles().next_id;
            if !new_handles.is_empty() {
                promise.set_thread_payload(Box::new(ThreadPromisePayload {
                    new_handles,
                    next_handle_id: next_id,
                }));
            }
            match result {
                Ok(result) => {
                    let output = std::mem::take(&mut thread_interp.output_sink_mut().output);
                    let stderr = std::mem::take(&mut thread_interp.output_sink_mut().stderr_output);
                    promise.keep(result, output, stderr);
                }
                Err(e) => {
                    let output = std::mem::take(&mut thread_interp.output_sink_mut().output);
                    let stderr = std::mem::take(&mut thread_interp.output_sink_mut().stderr_output);
                    let error_val = if let Some(ex) = e.exception {
                        *ex
                    } else {
                        Value::str(e.message)
                    };
                    promise.break_with(error_val.clone(), output, stderr);
                    // Call uncaught_handler if set, running in a helper thread
                    // so we don't block the promise thread.
                    if let Some(handler) =
                        crate::runtime::native_methods::state_scheduler::get_uncaught_handler()
                    {
                        let handler_interp = thread_interp.clone_for_thread();
                        let ex_val = error_val;
                        spawn_user_thread(move || {
                            let mut handler_interp = handler_interp;
                            let _ = handler_interp.call_value(handler, vec![ex_val]);
                        });
                    }
                }
            }
        });

        ret
    }
}
