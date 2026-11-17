//! A pooled task its spawner joins: [`submit_joinable`] and its [`TaskHandle`].

#[cfg(not(target_arch = "wasm32"))]
use super::submit_or_reject;

/// Handle on a pooled task whose completion (and result) the spawner waits
/// for. Natively this is a channel the task sends its result on — a worker
/// that panics drops the sender during unwind, so `join` reports the panic as
/// `Err` exactly like a dedicated thread's `join` would. On wasm32 it wraps
/// the cooperative scheduler's `JoinHandle`, whose `join` *runs* the queued
/// task — a channel wait would spin forever there, which is why the cfg fork
/// lives here and not at the call sites.
pub(crate) struct TaskHandle<T> {
    #[cfg(not(target_arch = "wasm32"))]
    rx: std::sync::mpsc::Receiver<T>,
    #[cfg(target_arch = "wasm32")]
    inner: crate::thread_compat::JoinHandle<T>,
}

impl<T> TaskHandle<T> {
    /// Wait for the task to finish and take its result. Callers wrap this in
    /// `gc::block_quiescent` like any thread join — the wait itself touches no
    /// `Gc` state.
    pub(crate) fn join(self) -> std::thread::Result<T> {
        #[cfg(not(target_arch = "wasm32"))]
        {
            self.rx
                .recv()
                .map_err(|e| Box::new(e) as Box<dyn std::any::Any + Send>)
        }
        #[cfg(target_arch = "wasm32")]
        {
            self.inner.join()
        }
    }
}

/// Run `task` on a pooled worker and return a handle its spawner can `join`.
/// For the joined fan-out sites (hyper/race batches, throttle workers). The
/// tasks run concurrently up to the pool's growth rule (ADR-0123); a spawner
/// that blocks joining them frees its own slot, and if every worker ends up
/// blocked the pool grows regardless, so a join never waits on a task that
/// nobody will run. A task the pool has to reject drops its sender, so its
/// `join` reports `Err` like a panicked thread's would.
#[cfg(not(target_arch = "wasm32"))]
pub(crate) fn submit_joinable<T: Send + 'static>(
    task: impl FnOnce() -> T + Send + 'static,
) -> TaskHandle<T> {
    let (tx, rx) = std::sync::mpsc::channel();
    submit_or_reject(
        move || {
            let _ = tx.send(task());
        },
        |_| {},
    );
    TaskHandle { rx }
}

#[cfg(target_arch = "wasm32")]
pub(crate) fn submit_joinable<T: Send + 'static>(
    task: impl FnOnce() -> T + Send + 'static,
) -> TaskHandle<T> {
    TaskHandle {
        inner: crate::runtime::builtins_system::spawn_user_thread("pool", task),
    }
}
