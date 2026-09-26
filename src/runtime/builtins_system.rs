use super::*;
use crate::symbol::Symbol;
use std::sync::OnceLock;

/// How hard a user-code spawn may try to get its thread (ADR-0123).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum StackPolicy {
    /// Optional growth (the worker pool adding capacity while other workers
    /// are still making progress): only a full-size stack, and only if it fits
    /// the address-space budget. It does not -> [`SpawnError::OverBudget`].
    /// (wasm32 has no pool, so nothing grows optionally there.)
    #[cfg_attr(target_arch = "wasm32", allow(dead_code))]
    Budgeted,
    /// A thread that has to exist -- nothing else can run the work, or user
    /// code asked for a `Thread` explicitly. Steps down through the stack
    /// tiers, past the budget if it has to.
    Required,
}

/// Why a user-code thread could not be started.
#[derive(Debug)]
pub(crate) enum SpawnError {
    /// [`StackPolicy::Budgeted`] and no stack tier fits the budget.
    OverBudget,
    /// The OS refused every stack size tried.
    Os(std::io::Error),
}

impl SpawnError {
    /// The text of the Raku exception the caller raises (rakudo's MoarVM
    /// reports a refused thread as a plain `X::AdHoc` as well).
    pub(crate) fn message(&self) -> String {
        match self {
            SpawnError::OverBudget => {
                "Could not create a new Thread: stack address-space budget exhausted".to_string()
            }
            SpawnError::Os(e) => format!("Could not create a new Thread: {e}"),
        }
    }

    pub(crate) fn to_runtime_error(&self) -> RuntimeError {
        RuntimeError::typed_msg("X::AdHoc", self.message())
    }
}

/// The catchable X::AdHoc for an OS-refused service thread (#9401): the
/// [`try_spawn_gc_helper_thread`] error in the same words a refused user-code
/// thread reports.
pub(crate) fn refused_thread_error(e: std::io::Error) -> RuntimeError {
    SpawnError::Os(e).to_runtime_error()
}

/// Spawn a worker thread with a large stack for running user code, so deep VM
/// recursion does not overflow the default thread stack. `name` becomes the
/// OS thread name (see `thread_compat::spawn_thread`), so pick something that
/// identifies the call site in a crash report's `thread:` field.
///
/// Panics when no thread can be created at all; code that runs on behalf of a
/// Raku program uses [`try_spawn_user_thread`] and raises instead.
pub(crate) fn spawn_user_thread<F, T>(
    name: &str,
    f: F,
) -> crate::runtime::thread_compat::JoinHandle<T>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    try_spawn_user_thread(name, StackPolicy::Required, f)
        .unwrap_or_else(|e| panic!("failed to spawn worker thread: {e:?}"))
}

/// Spawn a user-code thread (see [`spawn_user_thread`]) under `policy`,
/// stepping down through `stack_budget::STACK_TIERS` when the budget or the
/// OS cannot afford the full stack, and reporting failure instead of
/// panicking (ADR-0123).
pub(crate) fn try_spawn_user_thread<F, T>(
    name: &str,
    policy: StackPolicy,
    f: F,
) -> Result<crate::runtime::thread_compat::JoinHandle<T>, SpawnError>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    use crate::runtime::stack_budget::{STACK_TIERS, reserve_over_budget, try_reserve};
    // `Builder::spawn` consumes its closure even when it fails, so the body
    // lives in a slot each attempt borrows from; a refused attempt drops only
    // its handle on the slot.
    let slot = std::sync::Arc::new(std::sync::Mutex::new(Some(f)));
    let mut last_os_error = None;
    // Optional growth only ever takes a full-size stack, so which worker a
    // task lands on never changes how deep it may recurse.
    if let Some(reservation) = try_reserve(STACK_TIERS[0]) {
        match spawn_registered_thread(name, Some(reservation), slot.clone()) {
            Ok(handle) => return Ok(handle),
            Err(e) => last_os_error = Some(e),
        }
    }
    if policy == StackPolicy::Required {
        // A thread that has to exist steps down through the tiers, past the
        // budget if it must; the full size is retried only if the budget,
        // not the OS, refused it above.
        let first = usize::from(last_os_error.is_some());
        for &size in &STACK_TIERS[first..] {
            let reservation = try_reserve(size).unwrap_or_else(|| reserve_over_budget(size));
            match spawn_registered_thread(name, Some(reservation), slot.clone()) {
                Ok(handle) => return Ok(handle),
                Err(e) => last_os_error = Some(e),
            }
        }
    }
    Err(match last_os_error {
        Some(e) => SpawnError::Os(e),
        None => SpawnError::OverBudget,
    })
}

/// Spawn a runtime service thread (timer, promise combinator, socket
/// accept/read pump, supply emitter) as a REGISTERED GC mutator, with the
/// default thread stack (these never run user VM code, so they need no deep
/// recursion headroom).
///
/// Every thread that clones, drops, or creates `Gc` values MUST go through a
/// registered spawn: the cycle collector's stop-the-world counts only
/// registered threads toward quiescence, so an unregistered thread's
/// `Gc::drop` can land mid-scan and corrupt trial deletion (seen as
/// `MUTSU_GC_VERIFY` "survivor left inconsistent", strong N -> N-1 color
/// Purple, when a `Promise.in` timer dropped its promise handle during a
/// collect). Long blocking waits inside the closure (sleeps, blocking reads)
/// must be wrapped in `gc::block_quiescent` so the thread does not starve the
/// stop-the-world rendezvous.
///
/// A thread the OS refuses is reported, never a panic: every caller turns it
/// into the catchable `X::AdHoc` its API uses for other failures (a raised
/// exception, a broken promise, a quit supply -- ADR-0123, #9401).
pub(crate) fn try_spawn_gc_helper_thread<F, T>(
    name: &str,
    f: F,
) -> std::io::Result<crate::runtime::thread_compat::JoinHandle<T>>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    if service_thread_refusal_injected(name) {
        return Err(std::io::Error::from(std::io::ErrorKind::WouldBlock));
    }
    spawn_registered_thread(
        name,
        None,
        std::sync::Arc::new(std::sync::Mutex::new(Some(f))),
    )
}

/// Fault injection for the refused-service-thread paths (#9401), which an OS
/// limit cannot drive deterministically: `MUTSU_REFUSE_SERVICE_THREADS` is a
/// comma-separated list of thread names (`timer`, `proc-out`, ...) whose
/// spawn is refused as if the OS returned `EAGAIN`, or `all`. Read once.
fn service_thread_refusal_injected(name: &str) -> bool {
    static REFUSED: std::sync::OnceLock<Vec<String>> = std::sync::OnceLock::new();
    let refused = REFUSED.get_or_init(|| {
        std::env::var("MUTSU_REFUSE_SERVICE_THREADS")
            .map(|v| v.split(',').map(|n| n.trim().to_string()).collect())
            .unwrap_or_default()
    });
    refused.iter().any(|n| n == name || n == "all")
}

type BodySlot<F> = std::sync::Arc<std::sync::Mutex<Option<F>>>;

fn spawn_registered_thread<F, T>(
    name: &str,
    stack: Option<crate::runtime::stack_budget::StackReservation>,
    body: BodySlot<F>,
) -> std::io::Result<crate::runtime::thread_compat::JoinHandle<T>>
where
    F: FnOnce() -> T + Send + 'static,
    T: Send + 'static,
{
    // ADR-0068 §4 step 1: from here on, an aliased container can be structurally
    // mutated by more than one thread, so the store-side exclusion in
    // `value::container_lock` stops being a no-op. Sticky and process-global: a
    // container this worker wrote can still be aliased after the worker exits.
    crate::value::container_lock::note_mutator_thread_spawned();
    // Register a mutator worker with the GC: the worker count is the
    // cooperative stop-the-world's quiescence target (trial deletion must not
    // race this thread's `Gc` mutations; see `gc::stw`). Raised here on the
    // parent — before the thread exists — so the count is up the instant this
    // returns, then dropped by the worker's RAII guard (panic-safe).
    crate::gc::enter_mutator_worker();
    // Count the unborn worker quiescent until it actually starts: it touches
    // no `Gc` state before `worker_started` leaves quiescence (checked), and
    // without this a spawn burst starves every stop-the-world attempt — see
    // `gc::stw::preregister_worker_quiescent`.
    crate::gc::preregister_worker_quiescent();
    let stack_size = stack.as_ref().map(|r| r.size());
    let spawned = crate::runtime::thread_compat::spawn_thread(name, stack_size, move || {
        // The reservation is this thread's for as long as it lives.
        let _stack = stack;
        // ADR-0100: arm the deep-recursion guard, from the top of this
        // thread's stack. Only a worker that was given an explicit stack size
        // can be guarded -- a default-stack service thread runs no user VM
        // code, so it has no Raku recursion to bound and no size to measure
        // against.
        //
        // On wasm there is no new thread to arm: `spawn_thread` queues the
        // closure on the single browser thread, whose stack is neither this
        // size nor mutsu's to measure. That thread stays unguarded, as
        // `vm_stack_guard`'s module docs describe.
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(size) = stack_size {
            crate::vm::vm_stack_guard::init_thread_stack_floor(size);
        }
        struct WorkerGuard;
        impl Drop for WorkerGuard {
            fn drop(&mut self) {
                // Drop this thread's `Gc`-bearing thread-local state (pending
                // DESTROY queue, failure registry) BEFORE unregistering. Rust
                // runs TLS destructors after the closure returns — i.e. after
                // `exit_mutator_worker` below — so without this an unregistered
                // thread's TLS teardown would mutate the `Gc` graph while a
                // collector, believing this thread gone, runs trial deletion.
                // See `value::drop_thread_local_gc_state`.
                crate::value::drop_thread_local_gc_state();
                crate::gc::mark_thread_registered(false);
                crate::gc::exit_mutator_worker();
            }
        }
        let _guard = WorkerGuard;
        // This thread's own alternate signal stack. `std` already gave it an
        // 8 KiB one, which the crash handler overflows — so without this a
        // fatal signal on a worker faults a second time inside the handler and
        // the process dies leaving no crash report at all
        // (`todo/deep/procasync-stress-segv.md` §8.2). Dropped with the thread.
        let _alt_stack = crate::crash_report::install_thread_alt_stack();
        // Registered-mutator flag + leave the parent-granted quiescent
        // state (parking first if a scan is in progress): this thread's
        // quiescence now counts toward (and is required by) the STW
        // rendezvous (gc::stw).
        crate::gc::worker_started();
        let f = body
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
            .take()
            .expect("thread body taken twice");
        f()
    });
    if spawned.is_err() {
        // The thread never existed: hand back the GC registration taken on its
        // behalf above, or every later stop-the-world waits for it forever.
        crate::gc::abort_unborn_worker();
    }
    spawned
}

/// Split a finished child's `ExitStatus` into rakudo's `(exitcode, signal)`
/// pair.
///
/// A process killed by a signal has no exit status at all — `waitpid` reports
/// it as signalled, not exited, and Rust's `ExitStatus::code()` is `None`
/// there. Rakudo derives both numbers from the raw wait status (`exitcode` is
/// the high byte, `signal` the low one), so a signal death reports
/// `exitcode = 0` and carries the information in `.signal`; only a genuine
/// non-zero exit reports a non-zero `exitcode`. Reporting `-1` for a signal
/// death instead (what `code().unwrap_or(-1)` gives) made `.exitcode` wrong
/// for every killed process (#7924).
pub(crate) fn exit_status_parts(status: &std::process::ExitStatus) -> (i64, i64) {
    #[cfg(unix)]
    {
        use std::os::unix::process::ExitStatusExt;
        let signal = status.signal().unwrap_or(0) as i64;
        // `code()` is `None` exactly when the child was signalled; rakudo
        // reports 0 there rather than a synthetic failure code.
        let exitcode = status.code().unwrap_or(if signal != 0 { 0 } else { -1 }) as i64;
        (exitcode, signal)
    }
    #[cfg(not(unix))]
    {
        (status.code().unwrap_or(-1) as i64, 0i64)
    }
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
    /// Whether `:env` was explicitly given (even as an empty hash) — distinct
    /// from `env.is_empty()`, so callers can tell "use the default `%*ENV`"
    /// apart from "explicitly clear the child's environment".
    pub(super) env_explicit: bool,
    pub(super) capture_err: bool,
    pub(super) err_explicit: bool,
    pub(super) capture_out: bool,
    pub(super) out_explicit: bool,
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
        // `start { ... }` / `Promise.start` hand back a promise whose vow the
        // runtime keeps: the worker thread's outcome resolves it, so a user
        // `.keep`/`.break`/`.vow` on it is X::Promise::Vowed.
        promise.mark_vowed();
        let ret = Value::promise(promise.clone());
        let thread_interp = self.clone_for_thread_for_block(&block);
        let parent_handles_snapshot: std::collections::HashSet<usize> =
            self.io_handles().map.keys().copied().collect();

        let block = Self::strip_start_block_topics(block);

        // Pooled (ADR-0020 slice 1): `start` is the hottest spawner, and its
        // body may block (`await`) — the elastic pool reuses a warm worker or
        // grows instead of deadlocking. If no thread can be had for it at all
        // (ADR-0123), the promise breaks with a catchable X::AdHoc.
        let reject_promise = promise.clone();
        crate::runtime::worker_pool::submit_or_reject(
            move || {
                // CP-3 collapse: the thread's cloned Interpreter *is* the VM — run the
                // block on it directly instead of wrapping it in a sub-VM.
                let mut thread_interp = thread_interp;
                promise.set_thread_id(crate::runtime::current_mutsu_thread_id());
                // Worker bodies run via `call_value` without the main thread's
                // `run_top` panic boundary, so guard them here: a Rust panic in
                // user code becomes a catchable broken-Promise error (X::AdHoc)
                // instead of silently killing the thread (hanging `await`) or
                // aborting the process.
                let result =
                    crate::vm::guard_worker_panic(|| thread_interp.call_value(block, vec![]));
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
                        let stderr =
                            std::mem::take(&mut thread_interp.output_sink_mut().stderr_output);
                        promise.keep(result, output, stderr);
                    }
                    Err(e) => {
                        let output = std::mem::take(&mut thread_interp.output_sink_mut().output);
                        let stderr =
                            std::mem::take(&mut thread_interp.output_sink_mut().stderr_output);
                        let error_val = if let Some(ex) = e.exception {
                            *ex
                        } else {
                            Value::str(e.message.into_owned())
                        };
                        promise.break_with(error_val.clone(), output, stderr);
                        // Call uncaught_handler if set, running in a helper thread
                        // so we don't block the promise thread.
                        if let Some(handler) =
                            crate::runtime::native_methods::state_scheduler::get_uncaught_handler()
                        {
                            let handler_interp = thread_interp.clone_for_thread();
                            let ex_val = error_val;
                            // Pooled (ADR-0020 slice 3): short one-shot callback.
                            crate::runtime::worker_pool::submit(move || {
                                let mut handler_interp = handler_interp;
                                let _ = handler_interp.call_value(handler, vec![ex_val]);
                            });
                        }
                    }
                }
            },
            move |err| {
                let error_val = err
                    .to_runtime_error()
                    .exception
                    .map(|ex| *ex)
                    .unwrap_or_else(|| Value::str(err.message()));
                reject_promise.break_with(error_val, String::new(), String::new());
            },
        );

        ret
    }
}
