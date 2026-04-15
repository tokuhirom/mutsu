use crate::runtime::*;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

use super::state::cancellation_map;

#[derive(Debug, Default)]
pub(super) struct LockState {
    pub(super) owner: Option<std::thread::ThreadId>,
    pub(super) recursion: u64,
    /// FIFO queue of async Promises waiting to acquire the lock
    /// (Lock::Async.lock() returns a Promise that becomes Kept when the
    /// waiter becomes the lock owner).
    pub(super) async_waiters: std::collections::VecDeque<crate::value::SharedPromise>,
}

#[derive(Debug, Default)]
pub(crate) struct LockRuntime {
    pub(super) state: std::sync::Mutex<LockState>,
    pub(super) lock_cv: std::sync::Condvar,
    condvars: std::sync::Mutex<HashMap<u64, Arc<std::sync::Condvar>>>,
}

type LockStateMap = std::sync::RwLock<HashMap<u64, Arc<LockRuntime>>>;

fn lock_state_map() -> &'static LockStateMap {
    static MAP: OnceLock<LockStateMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::RwLock::new(HashMap::new()))
}

pub(in crate::runtime) fn next_lock_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    let id = COUNTER.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut map) = lock_state_map().write() {
        map.entry(id)
            .or_insert_with(|| Arc::new(LockRuntime::default()));
    }
    id
}

pub(super) fn next_cancellation_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    let id = COUNTER.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut map) = cancellation_map().lock() {
        map.insert(id, Arc::new(AtomicBool::new(false)));
    }
    id
}

pub(super) fn cancellation_state(id: u64) -> Option<Arc<AtomicBool>> {
    cancellation_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&id).cloned())
}

pub(crate) fn lock_runtime_by_id(id: u64) -> Option<Arc<LockRuntime>> {
    lock_state_map()
        .read()
        .ok()
        .and_then(|map| map.get(&id).cloned())
}

pub(crate) fn current_thread_id() -> std::thread::ThreadId {
    std::thread::current().id()
}

pub(crate) fn acquire_lock(
    runtime: &LockRuntime,
    me: std::thread::ThreadId,
) -> Result<(), RuntimeError> {
    let mut state = runtime
        .state
        .lock()
        .map_err(|_| RuntimeError::new("Lock state is poisoned"))?;
    loop {
        match state.owner {
            None => {
                state.owner = Some(me);
                state.recursion = 1;
                return Ok(());
            }
            Some(owner) if owner == me => {
                state.recursion += 1;
                return Ok(());
            }
            Some(_) => {
                state = runtime
                    .lock_cv
                    .wait(state)
                    .map_err(|_| RuntimeError::new("Lock wait failed"))?;
            }
        }
    }
}

pub(crate) fn release_lock(
    runtime: &LockRuntime,
    me: std::thread::ThreadId,
) -> Result<(), RuntimeError> {
    let mut state = runtime
        .state
        .lock()
        .map_err(|_| RuntimeError::new("Lock state is poisoned"))?;
    match state.owner {
        Some(owner) if owner == me => {
            if state.recursion > 1 {
                state.recursion -= 1;
            } else {
                state.recursion = 0;
                state.owner = None;
                // Drop the MutexGuard before notifying to reduce contention:
                // waiters wake up and can immediately try to acquire the mutex.
                drop(state);
                runtime.lock_cv.notify_one();
                return Ok(());
            }
            Ok(())
        }
        _ => Err(RuntimeError::new(
            "Cannot unlock a Lock not owned by current thread",
        )),
    }
}

/// Async-flavored lock acquisition used by Lock::Async.lock().
/// Returns a Promise that is Kept immediately if the lock is free, or
/// Planned and enqueued if another waiter holds it. The caller holding
/// the lock must call `async_release_lock` to pass ownership to the
/// next waiter (keeping their Promise).
pub(crate) fn async_acquire_lock(
    runtime: &LockRuntime,
    me: std::thread::ThreadId,
) -> Result<crate::value::SharedPromise, RuntimeError> {
    let mut state = runtime
        .state
        .lock()
        .map_err(|_| RuntimeError::new("Lock state is poisoned"))?;
    let promise = crate::value::SharedPromise::new();
    if state.owner.is_none() && state.async_waiters.is_empty() {
        state.owner = Some(me);
        state.recursion = 1;
        // Promise resolves immediately.
        let _ = promise.try_keep(crate::value::Value::Nil);
    } else {
        state.async_waiters.push_back(promise.clone());
    }
    Ok(promise)
}

/// Async-flavored unlock: if a waiter is queued, hand ownership to it
/// and keep its Promise. Throws X::Lock::Async::NotLocked when the lock
/// is not held.
pub(crate) fn async_release_lock(runtime: &LockRuntime) -> Result<(), RuntimeError> {
    let mut state = runtime
        .state
        .lock()
        .map_err(|_| RuntimeError::new("Lock state is poisoned"))?;
    if state.owner.is_none() {
        let mut err = RuntimeError::new("Cannot unlock an unlocked lock");
        let mut attrs = HashMap::new();
        attrs.insert(
            "message".to_string(),
            crate::value::Value::str_from("Cannot unlock an unlocked lock"),
        );
        let ex = crate::value::Value::make_instance(
            crate::symbol::Symbol::intern("X::Lock::Async::NotLocked"),
            attrs,
        );
        err.exception = Some(Box::new(ex));
        return Err(err);
    }
    if let Some(next) = state.async_waiters.pop_front() {
        // Transfer ownership to the next waiter and keep their Promise.
        // We use the current thread's id as the owner because async locks
        // do not track per-thread ownership in a meaningful way in our
        // single-threaded unit test model; cross-thread behavior still
        // works because the queue ordering is preserved.
        state.owner = Some(current_thread_id());
        state.recursion = 1;
        drop(state);
        let _ = next.try_keep(crate::value::Value::Nil);
    } else {
        state.owner = None;
        state.recursion = 0;
        drop(state);
        runtime.lock_cv.notify_one();
    }
    Ok(())
}

pub(super) fn ensure_condition(
    runtime: &LockRuntime,
    cond_id: u64,
) -> Option<Arc<std::sync::Condvar>> {
    runtime.condvars.lock().ok().map(|mut map| {
        map.entry(cond_id)
            .or_insert_with(|| Arc::new(std::sync::Condvar::new()))
            .clone()
    })
}

pub(super) fn next_condition_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

// --- Counting semaphore registry ---

#[derive(Debug)]
pub(crate) struct SemaphoreRuntime {
    pub(super) state: std::sync::Mutex<i64>,
    pub(super) cv: std::sync::Condvar,
}

type SemaphoreMap = std::sync::RwLock<HashMap<u64, Arc<SemaphoreRuntime>>>;

fn semaphore_map() -> &'static SemaphoreMap {
    static MAP: OnceLock<SemaphoreMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::RwLock::new(HashMap::new()))
}

pub(in crate::runtime) fn next_semaphore_id(permits: i64) -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    let id = COUNTER.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut map) = semaphore_map().write() {
        map.entry(id).or_insert_with(|| {
            Arc::new(SemaphoreRuntime {
                state: std::sync::Mutex::new(permits),
                cv: std::sync::Condvar::new(),
            })
        });
    }
    id
}

pub(crate) fn semaphore_runtime_by_id(id: u64) -> Option<Arc<SemaphoreRuntime>> {
    semaphore_map()
        .read()
        .ok()
        .and_then(|map| map.get(&id).cloned())
}

pub(crate) fn semaphore_acquire(rt: &SemaphoreRuntime) -> Result<(), RuntimeError> {
    let mut state = rt
        .state
        .lock()
        .map_err(|_| RuntimeError::new("Semaphore state poisoned"))?;
    while *state <= 0 {
        state = rt
            .cv
            .wait(state)
            .map_err(|_| RuntimeError::new("Semaphore wait failed"))?;
    }
    *state -= 1;
    Ok(())
}

pub(crate) fn semaphore_try_acquire(rt: &SemaphoreRuntime) -> Result<bool, RuntimeError> {
    let mut state = rt
        .state
        .lock()
        .map_err(|_| RuntimeError::new("Semaphore state poisoned"))?;
    if *state > 0 {
        *state -= 1;
        Ok(true)
    } else {
        Ok(false)
    }
}

pub(crate) fn semaphore_release(rt: &SemaphoreRuntime) -> Result<(), RuntimeError> {
    let mut state = rt
        .state
        .lock()
        .map_err(|_| RuntimeError::new("Semaphore state poisoned"))?;
    *state += 1;
    drop(state);
    rt.cv.notify_one();
    Ok(())
}
