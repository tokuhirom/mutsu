use super::*;
use crate::symbol::Symbol;
use std::process::ChildStdin;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc;
use std::time::Duration;

type StdinMap = std::sync::Mutex<HashMap<u32, Arc<std::sync::Mutex<Option<ChildStdin>>>>>;

pub(super) fn proc_stdin_map() -> &'static StdinMap {
    static MAP: OnceLock<StdinMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

type SupplyTapsMap = std::sync::Mutex<HashMap<u64, Vec<Value>>>;

fn supply_taps_map() -> &'static SupplyTapsMap {
    static MAP: OnceLock<SupplyTapsMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

type SupplyCollectedMap = std::sync::Mutex<HashMap<u64, String>>;

fn supply_collected_map() -> &'static SupplyCollectedMap {
    static MAP: OnceLock<SupplyCollectedMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

type CancellationMap = std::sync::Mutex<HashMap<u64, Arc<AtomicBool>>>;

fn cancellation_map() -> &'static CancellationMap {
    static MAP: OnceLock<CancellationMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

#[derive(Debug, Clone)]
pub(crate) struct AsyncSocketConnState {
    pub(crate) peer_id: Option<u64>,
    pub(crate) encoding: String,
    pub(crate) closed: bool,
    pub(crate) peer_closed: bool,
    pub(crate) supply_ids: Vec<u64>,
    pub(crate) pending_bytes: Vec<u8>,
    pub(crate) deferred_accept_callback: Option<Value>,
    pub(crate) deferred_accept_socket: Option<Value>,
}

#[derive(Debug, Clone)]
pub(crate) struct AsyncSocketSupplyState {
    pub(crate) is_bin: bool,
    pub(crate) encoding: String,
    pub(crate) text_buffer: String,
    pub(crate) byte_buffer: Vec<u8>,
}

#[derive(Debug, Clone)]
pub(crate) struct AsyncSocketListenerState {
    pub(crate) host: String,
    pub(crate) port: u16,
    pub(crate) callback: Value,
    pub(crate) closed: bool,
    pub(crate) encoding: String,
}

type AsyncSocketConnMap = std::sync::Mutex<HashMap<u64, AsyncSocketConnState>>;
type AsyncSocketSupplyMap = std::sync::Mutex<HashMap<u64, AsyncSocketSupplyState>>;
type AsyncSocketListenerMap = std::sync::Mutex<HashMap<u64, AsyncSocketListenerState>>;

fn async_socket_conn_map() -> &'static AsyncSocketConnMap {
    static MAP: OnceLock<AsyncSocketConnMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn async_socket_supply_map() -> &'static AsyncSocketSupplyMap {
    static MAP: OnceLock<AsyncSocketSupplyMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn async_socket_listener_map() -> &'static AsyncSocketListenerMap {
    static MAP: OnceLock<AsyncSocketListenerMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Supply channel registry: supply_id -> Receiver for streaming data from
/// Proc::Async stdout/stderr reader threads.
type SupplyChannelMap = std::sync::Mutex<HashMap<u64, mpsc::Receiver<SupplyEvent>>>;

pub(super) fn supply_channel_map() -> &'static SupplyChannelMap {
    static MAP: OnceLock<SupplyChannelMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Events sent through supply channels
#[derive(Debug, Clone)]
pub(crate) enum SupplyEvent {
    Emit(Value),
    Done,
    Quit(Value),
}

/// Take a receiver from the supply channel registry (can only be consumed once)
pub(crate) fn take_supply_channel(supply_id: u64) -> Option<mpsc::Receiver<SupplyEvent>> {
    if let Ok(mut map) = supply_channel_map().lock() {
        map.remove(&supply_id)
    } else {
        None
    }
}

/// Public access to the supply channel map for signal registration
pub(super) fn supply_channel_map_pub()
-> &'static std::sync::Mutex<HashMap<u64, mpsc::Receiver<SupplyEvent>>> {
    supply_channel_map()
}

#[derive(Debug, Default)]
struct SupplierRuntimeState {
    emitted: Vec<Value>,
    done: bool,
    quit_reason: Option<Value>,
    pending_promises: Vec<SharedPromise>,
}

type SupplierStateMap = std::sync::Mutex<HashMap<u64, SupplierRuntimeState>>;

fn supplier_state_map() -> &'static SupplierStateMap {
    static MAP: OnceLock<SupplierStateMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

pub(super) fn supplier_id_from_attrs(attributes: &HashMap<String, Value>) -> Option<u64> {
    match attributes.get("supplier_id") {
        Some(Value::Int(id)) if *id > 0 => Some(*id as u64),
        _ => None,
    }
}

pub(super) fn supplier_snapshot(supplier_id: u64) -> (Vec<Value>, bool, Option<Value>) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        (state.emitted.clone(), state.done, state.quit_reason.clone())
    } else {
        (Vec::new(), false, None)
    }
}

// --- FakeScheduler support (for Test::Tap) ---

/// A scheduled item in a FakeScheduler.
#[derive(Debug, Clone)]
struct FakeScheduled {
    deadline: f64,
    /// Regular callback (from user code) or None for counter-mode entries.
    callback: Option<Value>,
    /// Counter index for counter-mode scheduled items.
    counter_value: i64,
}

/// Per-instance state for FakeScheduler.
#[derive(Debug, Clone)]
struct FakeSchedulerState {
    time: f64,
    upcoming: Vec<FakeScheduled>,
}

type FakeSchedulerMap = std::sync::Mutex<HashMap<u64, FakeSchedulerState>>;

fn fake_scheduler_map() -> &'static FakeSchedulerMap {
    static MAP: OnceLock<FakeSchedulerMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

static FAKE_SCHEDULER_ID_COUNTER: AtomicU64 = AtomicU64::new(1);

pub(super) fn next_fake_scheduler_id() -> u64 {
    FAKE_SCHEDULER_ID_COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(super) fn fake_scheduler_cue(
    scheduler_id: u64,
    callback: Value,
    every: Option<f64>,
    delay: f64,
) {
    if let Ok(mut map) = fake_scheduler_map().lock() {
        let state = map
            .entry(scheduler_id)
            .or_insert_with(|| FakeSchedulerState {
                time: 0.0,
                upcoming: Vec::new(),
            });
        if let Some(every) = every {
            // Schedule 100 future callbacks (matching FakeScheduler behavior)
            let mut deadline = state.time + delay;
            for _ in 0..100 {
                state.upcoming.push(FakeScheduled {
                    deadline,
                    callback: Some(callback.clone()),
                    counter_value: -1,
                });
                deadline += every;
            }
        } else {
            // No :every — execute immediately (but we store it for progress-by)
            state.upcoming.push(FakeScheduled {
                deadline: state.time + delay,
                callback: Some(callback),
                counter_value: -1,
            });
        }
    }
}

/// Register a counter-mode cue: each scheduled item produces an incrementing
/// integer value instead of calling a callback.
pub(super) fn fake_scheduler_cue_counter(scheduler_id: u64, every: f64, delay: f64) {
    if let Ok(mut map) = fake_scheduler_map().lock() {
        let state = map
            .entry(scheduler_id)
            .or_insert_with(|| FakeSchedulerState {
                time: 0.0,
                upcoming: Vec::new(),
            });
        let mut deadline = state.time + delay;
        for i in 0..100i64 {
            state.upcoming.push(FakeScheduled {
                deadline,
                callback: None,
                counter_value: i,
            });
            deadline += every;
        }
    }
}

/// Advance time, run callbacks whose deadline <= new time, and return counter
/// values from counter-mode entries.  Regular callbacks are returned for the
/// caller to execute.
pub(super) fn fake_scheduler_progress_by(
    scheduler_id: u64,
    duration: f64,
) -> (Vec<Value>, Vec<i64>) {
    if let Ok(mut map) = fake_scheduler_map().lock() {
        if let Some(state) = map.get_mut(&scheduler_id) {
            state.time += duration;
            let new_time = state.time;
            let mut to_run = Vec::new();
            let mut remaining = Vec::new();
            for item in state.upcoming.drain(..) {
                if item.deadline <= new_time {
                    to_run.push(item);
                } else {
                    remaining.push(item);
                }
            }
            to_run.sort_by(|a, b| {
                a.deadline
                    .partial_cmp(&b.deadline)
                    .unwrap_or(std::cmp::Ordering::Equal)
            });
            state.upcoming = remaining;
            let mut callbacks = Vec::new();
            let mut counter_values = Vec::new();
            for item in to_run {
                if let Some(cb) = item.callback {
                    callbacks.push(cb);
                } else {
                    counter_values.push(item.counter_value);
                }
            }
            (callbacks, counter_values)
        } else {
            (Vec::new(), Vec::new())
        }
    } else {
        (Vec::new(), Vec::new())
    }
}

pub(super) fn fake_scheduler_init(scheduler_id: u64, time: f64) {
    if let Ok(mut map) = fake_scheduler_map().lock() {
        map.insert(
            scheduler_id,
            FakeSchedulerState {
                time,
                upcoming: Vec::new(),
            },
        );
    }
}

pub(crate) fn split_supply_chunks_into_lines(chunks: &[Value], chomp: bool) -> Vec<Value> {
    let mut combined = String::new();
    for chunk in chunks {
        combined.push_str(&chunk.to_string_value());
    }
    crate::builtins::split_lines_with_chomp(&combined, chomp)
        .into_iter()
        .map(Value::str)
        .collect()
}

fn take_complete_lines_from_buffer(buffer: &mut String, chomp: bool, flush: bool) -> Vec<String> {
    let bytes = buffer.as_bytes();
    let mut out = Vec::new();
    let mut start = 0usize;
    let mut i = 0usize;
    while i < bytes.len() {
        let sep_len = if bytes[i] == b'\n' {
            1
        } else if bytes[i] == b'\r' {
            if i + 1 < bytes.len() {
                if bytes[i + 1] == b'\n' { 2 } else { 1 }
            } else if flush {
                1
            } else {
                break;
            }
        } else {
            i += 1;
            continue;
        };
        let end = if chomp { i } else { i + sep_len };
        out.push(buffer[start..end].to_string());
        i += sep_len;
        start = i;
    }
    let remaining = buffer[start..].to_string();
    *buffer = remaining;
    if flush && !buffer.is_empty() {
        out.push(std::mem::take(buffer));
    }
    out
}

pub(super) fn supplier_register_promise(supplier_id: u64, promise: SharedPromise) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        if let Some(reason) = state.quit_reason.clone() {
            promise.break_with(reason, String::new(), String::new());
        } else if state.done {
            let result = state.emitted.last().cloned().unwrap_or(Value::Nil);
            promise.keep(result, String::new(), String::new());
        } else {
            state.pending_promises.push(promise);
        }
    }
}

pub(super) fn supplier_emit(supplier_id: u64, value: Value) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        if state.done || state.quit_reason.is_some() {
            return;
        }
        state.emitted.push(value);
    }
}

pub(super) fn supplier_done(supplier_id: u64) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        if state.done || state.quit_reason.is_some() {
            return;
        }
        state.done = true;
        let result = state.emitted.last().cloned().unwrap_or(Value::Nil);
        let pending = std::mem::take(&mut state.pending_promises);
        for promise in pending {
            promise.keep(result.clone(), String::new(), String::new());
        }
    }
}

pub(super) fn supplier_quit(supplier_id: u64, reason: Value) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        if state.done || state.quit_reason.is_some() {
            return;
        }
        state.quit_reason = Some(reason.clone());
        let pending = std::mem::take(&mut state.pending_promises);
        for promise in pending {
            promise.break_with(reason.clone(), String::new(), String::new());
        }
    }
}

#[derive(Clone)]
struct UniqueFilterState {
    as_fn: Option<Value>,
    with_fn: Option<Value>,
    expires_seconds: Option<f64>,
    seen: Vec<(Value, std::time::Instant)>,
}

#[derive(Clone)]
struct SupplierTapSubscription {
    callback: Value,
    line_mode: bool,
    line_chomp: bool,
    line_buffer: String,
    delay_seconds: f64,
    unique_filter: Option<UniqueFilterState>,
}

#[derive(Clone, Default)]
struct SupplierSubscriptions {
    taps: Vec<SupplierTapSubscription>,
    done_callbacks: Vec<Value>,
    quit_callbacks: Vec<Value>,
}

type SupplierSubscriptionsMap = std::sync::Mutex<HashMap<u64, SupplierSubscriptions>>;

fn supplier_subscriptions_map() -> &'static SupplierSubscriptionsMap {
    static MAP: OnceLock<SupplierSubscriptionsMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

#[derive(Debug, Default)]
struct LockState {
    owner: Option<std::thread::ThreadId>,
    recursion: u64,
}

#[derive(Debug, Default)]
pub(crate) struct LockRuntime {
    state: std::sync::Mutex<LockState>,
    lock_cv: std::sync::Condvar,
    condvars: std::sync::Mutex<HashMap<u64, Arc<std::sync::Condvar>>>,
}

type LockStateMap = std::sync::RwLock<HashMap<u64, Arc<LockRuntime>>>;

fn lock_state_map() -> &'static LockStateMap {
    static MAP: OnceLock<LockStateMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::RwLock::new(HashMap::new()))
}

pub(super) fn next_supply_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(super) fn next_async_socket_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(super) fn next_async_listener_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(super) fn allocate_async_listen_port() -> u16 {
    static COUNTER: AtomicU64 = AtomicU64::new(43000);
    loop {
        let candidate = COUNTER.fetch_add(1, Ordering::Relaxed) as u16;
        let occupied = async_socket_listener_map()
            .lock()
            .ok()
            .is_some_and(|map| map.values().any(|l| !l.closed && l.port == candidate));
        if !occupied && candidate != 0 {
            return candidate;
        }
    }
}

pub(super) fn register_async_listener(listener_id: u64, state: AsyncSocketListenerState) {
    if let Ok(mut map) = async_socket_listener_map().lock() {
        map.insert(listener_id, state);
    }
}

pub(super) fn close_async_listener(listener_id: u64) {
    if let Ok(mut map) = async_socket_listener_map().lock()
        && let Some(listener) = map.get_mut(&listener_id)
    {
        listener.closed = true;
    }
}

pub(super) fn lookup_async_listener(
    host: &str,
    port: u16,
) -> Option<(u64, AsyncSocketListenerState)> {
    if let Ok(map) = async_socket_listener_map().lock() {
        for (id, listener) in map.iter() {
            if listener.closed || listener.port != port {
                continue;
            }
            if listener.host == host
                || listener.host == "0.0.0.0"
                || listener.host == "::"
                || (host == "localhost" && listener.host == "127.0.0.1")
            {
                return Some((*id, listener.clone()));
            }
        }
    }
    None
}

pub(super) fn async_port_in_use(host: &str, port: u16) -> bool {
    if let Ok(map) = async_socket_listener_map().lock() {
        return map.values().any(|listener| {
            !listener.closed
                && listener.port == port
                && (listener.host == host || listener.host == "0.0.0.0" || host == "0.0.0.0")
        });
    }
    false
}

pub(super) fn register_async_connection(conn_id: u64, state: AsyncSocketConnState) {
    if let Ok(mut map) = async_socket_conn_map().lock() {
        map.insert(conn_id, state);
    }
}

pub(super) fn get_async_connection(conn_id: u64) -> Option<AsyncSocketConnState> {
    async_socket_conn_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&conn_id).cloned())
}

pub(super) fn update_async_connection<F>(conn_id: u64, f: F)
where
    F: FnOnce(&mut AsyncSocketConnState),
{
    if let Ok(mut map) = async_socket_conn_map().lock()
        && let Some(state) = map.get_mut(&conn_id)
    {
        f(state);
    }
}

fn take_deferred_accept_callback(conn_id: u64) -> Option<(Value, Value)> {
    if let Ok(mut map) = async_socket_conn_map().lock()
        && let Some(state) = map.get_mut(&conn_id)
        && let (Some(callback), Some(socket)) = (
            state.deferred_accept_callback.take(),
            state.deferred_accept_socket.take(),
        )
    {
        Some((callback, socket))
    } else {
        None
    }
}

pub(super) fn register_async_supply(supply_id: u64, state: AsyncSocketSupplyState) {
    if let Ok(mut map) = async_socket_supply_map().lock() {
        map.insert(supply_id, state);
    }
}

pub(super) fn get_async_supply(supply_id: u64) -> Option<AsyncSocketSupplyState> {
    async_socket_supply_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&supply_id).cloned())
}

pub(super) fn update_async_supply<F>(supply_id: u64, f: F)
where
    F: FnOnce(&mut AsyncSocketSupplyState),
{
    if let Ok(mut map) = async_socket_supply_map().lock()
        && let Some(state) = map.get_mut(&supply_id)
    {
        f(state);
    }
}

pub(super) fn next_supplier_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(super) fn next_lock_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    let id = COUNTER.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut map) = lock_state_map().write() {
        map.entry(id)
            .or_insert_with(|| Arc::new(LockRuntime::default()));
    }
    id
}

fn next_cancellation_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    let id = COUNTER.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut map) = cancellation_map().lock() {
        map.insert(id, Arc::new(AtomicBool::new(false)));
    }
    id
}

fn cancellation_state(id: u64) -> Option<Arc<AtomicBool>> {
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
                runtime.lock_cv.notify_one();
            }
            Ok(())
        }
        _ => Err(RuntimeError::new(
            "Cannot unlock a Lock not owned by current thread",
        )),
    }
}

fn ensure_condition(runtime: &LockRuntime, cond_id: u64) -> Option<Arc<std::sync::Condvar>> {
    runtime.condvars.lock().ok().map(|mut map| {
        map.entry(cond_id)
            .or_insert_with(|| Arc::new(std::sync::Condvar::new()))
            .clone()
    })
}

fn next_condition_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(super) fn register_supply_tap(supply_id: u64, tap: Value) {
    if let Ok(mut map) = supply_taps_map().lock() {
        map.entry(supply_id).or_default().push(tap);
    }
}

pub(super) fn get_supply_taps(supply_id: u64) -> Vec<Value> {
    if let Ok(map) = supply_taps_map().lock() {
        map.get(&supply_id).cloned().unwrap_or_default()
    } else {
        Vec::new()
    }
}

pub(super) fn set_supply_collected_output(supply_id: u64, output: String) {
    if let Ok(mut map) = supply_collected_map().lock() {
        map.insert(supply_id, output);
    }
}

pub(super) fn get_supply_collected_output(supply_id: u64) -> Option<String> {
    supply_collected_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&supply_id).cloned())
}

pub(super) fn register_supplier_tap(supplier_id: u64, tap: Value, delay_seconds: f64) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: tap,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds,
                unique_filter: None,
            });
    }
}

pub(super) fn register_supplier_lines_tap(
    supplier_id: u64,
    tap: Value,
    chomp: bool,
    delay_seconds: f64,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: tap,
                line_mode: true,
                line_chomp: chomp,
                line_buffer: String::new(),
                delay_seconds,
                unique_filter: None,
            });
    }
}

/// Result of checking a supplier emit callback.
/// Normal: callback, value, delay
/// UniqueFiltered: callback, value, delay, as_fn, with_fn (need interpreter to check)
pub(super) enum SupplierEmitAction {
    Call(Value, Value, f64),
    UniqueCheck {
        callback: Value,
        value: Value,
        delay_seconds: f64,
        as_fn: Option<Value>,
        with_fn: Option<Value>,
        tap_index: usize,
    },
}

pub(super) fn supplier_emit_callbacks(
    supplier_id: u64,
    emitted_value: &Value,
) -> Vec<SupplierEmitAction> {
    let mut actions = Vec::new();
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
    {
        for (idx, tap) in subs.taps.iter_mut().enumerate() {
            if tap.line_mode {
                tap.line_buffer.push_str(&emitted_value.to_string_value());
                for line in
                    take_complete_lines_from_buffer(&mut tap.line_buffer, tap.line_chomp, false)
                {
                    actions.push(SupplierEmitAction::Call(
                        tap.callback.clone(),
                        Value::str(line),
                        tap.delay_seconds,
                    ));
                }
            } else if let Some(ref mut uf) = tap.unique_filter {
                // Expire old seen values if :expires is set
                if let Some(expire_secs) = uf.expires_seconds {
                    let now = std::time::Instant::now();
                    uf.seen
                        .retain(|(_, ts)| now.duration_since(*ts).as_secs_f64() < expire_secs);
                }

                if uf.as_fn.is_some() || uf.with_fn.is_some() {
                    // Need interpreter to evaluate :as / :with
                    actions.push(SupplierEmitAction::UniqueCheck {
                        callback: tap.callback.clone(),
                        value: emitted_value.clone(),
                        delay_seconds: tap.delay_seconds,
                        as_fn: uf.as_fn.clone(),
                        with_fn: uf.with_fn.clone(),
                        tap_index: idx,
                    });
                } else {
                    // Simple unique check (no :as, no :with) — use value equality
                    let already_seen = uf
                        .seen
                        .iter()
                        .any(|(s, _)| values_identical(s, emitted_value));
                    if !already_seen {
                        uf.seen
                            .push((emitted_value.clone(), std::time::Instant::now()));
                        actions.push(SupplierEmitAction::Call(
                            tap.callback.clone(),
                            emitted_value.clone(),
                            tap.delay_seconds,
                        ));
                    }
                }
            } else {
                actions.push(SupplierEmitAction::Call(
                    tap.callback.clone(),
                    emitted_value.clone(),
                    tap.delay_seconds,
                ));
            }
        }
    }
    actions
}

/// After the interpreter has evaluated :as/:with for a unique check, call this
/// to update the seen list if the value is unique.
pub(super) fn supplier_unique_mark_seen(supplier_id: u64, tap_index: usize, key: Value) {
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
        && let Some(tap) = subs.taps.get_mut(tap_index)
        && let Some(ref mut uf) = tap.unique_filter
    {
        uf.seen.push((key, std::time::Instant::now()));
    }
}

/// Get a copy of the seen keys from a unique filter tap.
/// Used by the interpreter to check :with comparisons.
pub(super) fn supplier_unique_get_seen(supplier_id: u64, tap_index: usize) -> Vec<Value> {
    if let Ok(map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get(&supplier_id)
        && let Some(tap) = subs.taps.get(tap_index)
        && let Some(ref uf) = tap.unique_filter
    {
        uf.seen.iter().map(|(v, _)| v.clone()).collect()
    } else {
        Vec::new()
    }
}

pub(super) fn register_supplier_unique_tap(
    supplier_id: u64,
    tap: Value,
    delay_seconds: f64,
    as_fn: Option<Value>,
    with_fn: Option<Value>,
    expires_seconds: Option<f64>,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: tap,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds,
                unique_filter: Some(UniqueFilterState {
                    as_fn,
                    with_fn,
                    expires_seconds,
                    seen: Vec::new(),
                }),
            });
    }
}

/// Returns the number of taps currently registered for a supplier.
pub(super) fn supplier_tap_count(supplier_id: u64) -> usize {
    if let Ok(map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get(&supplier_id)
    {
        subs.taps.len()
    } else {
        0
    }
}

pub(super) fn flush_supplier_line_taps(supplier_id: u64) -> Vec<(Value, Value)> {
    let mut callbacks = Vec::new();
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
    {
        for tap in &mut subs.taps {
            if tap.line_mode {
                for line in
                    take_complete_lines_from_buffer(&mut tap.line_buffer, tap.line_chomp, true)
                {
                    callbacks.push((tap.callback.clone(), Value::str(line)));
                }
            }
        }
    }
    callbacks
}

pub(super) fn take_supplier_done_callbacks(supplier_id: u64) -> Vec<Value> {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        if let Some(subs) = map.get_mut(&supplier_id) {
            std::mem::take(&mut subs.done_callbacks)
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    }
}

pub(super) fn register_supplier_done_callback(supplier_id: u64, done_cb: Value) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .done_callbacks
            .push(done_cb);
    }
}

pub(super) fn take_supplier_quit_callbacks(supplier_id: u64) -> Vec<Value> {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        if let Some(subs) = map.get_mut(&supplier_id) {
            std::mem::take(&mut subs.quit_callbacks)
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    }
}

pub(super) fn register_supplier_quit_callback(supplier_id: u64, quit_cb: Value) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .quit_callbacks
            .push(quit_cb);
    }
}

impl Interpreter {
    pub(super) fn resolve_supply_delay_seconds(
        &self,
        arg: Option<&Value>,
    ) -> Result<f64, RuntimeError> {
        let Some(value) = arg else {
            return Err(RuntimeError::new(
                "Supply.delayed requires a delay argument",
            ));
        };
        let delay = value.to_f64();
        if delay.is_nan() {
            return Err(RuntimeError::new("Supply.delayed requires a numeric delay"));
        }
        if delay.is_infinite() {
            return Err(RuntimeError::new("Supply.delayed requires a finite delay"));
        }
        Ok(delay.max(0.0))
    }

    pub(super) fn supply_delay_seconds(attributes: &HashMap<String, Value>) -> f64 {
        attributes
            .get("delay_seconds")
            .map(Value::to_f64)
            .filter(|value| value.is_finite() && *value > 0.0)
            .unwrap_or(0.0)
    }

    pub(super) fn sleep_for_supply_delay(delay_seconds: f64) {
        if delay_seconds > 0.0 {
            std::thread::sleep(Duration::from_secs_f64(delay_seconds));
        }
    }

    pub(super) fn resolve_supply_tail_count(
        &mut self,
        arg: Option<&Value>,
        total_len: usize,
    ) -> Result<usize, RuntimeError> {
        let Some(value) = arg else {
            return Ok(1);
        };
        let parsed = match value {
            Value::Int(i) => *i,
            Value::Whatever => return Ok(total_len),
            Value::Num(f) if f.is_infinite() && f.is_sign_positive() => return Ok(total_len),
            Value::Num(f) if f.is_infinite() && f.is_sign_negative() => return Ok(0),
            Value::Num(f) if f.is_nan() => {
                return Err(RuntimeError::new("Cannot use NaN as a tail count"));
            }
            Value::Num(f) => *f as i64,
            Value::Str(s) => {
                let trimmed = s.trim();
                if trimmed.eq_ignore_ascii_case("inf") {
                    return Ok(total_len);
                }
                if trimmed.eq_ignore_ascii_case("-inf") {
                    return Ok(0);
                }
                match trimmed.parse::<f64>() {
                    Ok(f) if f.is_infinite() && f.is_sign_positive() => return Ok(total_len),
                    Ok(f) if f.is_infinite() && f.is_sign_negative() => return Ok(0),
                    Ok(f) if f.is_nan() => {
                        return Err(RuntimeError::new("Cannot use NaN as a tail count"));
                    }
                    Ok(f) => f as i64,
                    Err(_) => {
                        return Err(RuntimeError::new(format!(
                            "Cannot use '{}' as a tail count",
                            s
                        )));
                    }
                }
            }
            Value::Sub(_) | Value::WeakSub(_) | Value::Routine { .. } => {
                let computed =
                    self.eval_call_on_value(value.clone(), vec![Value::Int(total_len as i64)])?;
                return self.resolve_supply_tail_count(Some(&computed), total_len);
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "Cannot use '{}' as a tail count",
                    value.to_string_value()
                )));
            }
        };
        Ok(parsed.clamp(0, total_len as i64) as usize)
    }

    pub(super) fn translate_newlines_for_decode_native(&self, input: &str) -> String {
        match self.newline_mode {
            NewlineMode::Lf => input.to_string(),
            NewlineMode::Cr => input.replace('\r', "\n"),
            NewlineMode::Crlf => input.replace("\r\n", "\n"),
        }
    }

    pub(super) fn decode_bytes_for_supply(
        &self,
        bytes: &[u8],
        encoding_name: &str,
    ) -> Result<String, RuntimeError> {
        let encoding = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();

        match encoding.as_str() {
            "ascii" => Ok(bytes
                .iter()
                .map(|b| if *b <= 0x7F { *b as char } else { '\u{FFFD}' })
                .collect()),
            "iso-8859-1" => Ok(bytes.iter().map(|b| *b as char).collect()),
            "utf-16" | "utf-16le" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16 byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|chunk| u16::from_le_bytes([chunk[0], chunk[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            "utf-16be" => {
                if !bytes.len().is_multiple_of(2) {
                    return Err(RuntimeError::new("Invalid utf-16be byte length"));
                }
                let units: Vec<u16> = bytes
                    .chunks_exact(2)
                    .map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]]))
                    .collect();
                Ok(String::from_utf16_lossy(&units))
            }
            _ => {
                if let Some(enc) = encoding_rs::Encoding::for_label(encoding.as_bytes()) {
                    let (decoded, _used_encoding, _had_errors) = enc.decode(bytes);
                    return Ok(decoded.into_owned());
                }
                Ok(String::from_utf8_lossy(bytes).into_owned())
            }
        }
    }

    pub(super) fn supply_chunk_to_bytes(&self, chunk: &Value, encoding_name: &str) -> Vec<u8> {
        let normalized = self
            .find_encoding(encoding_name)
            .map(|e| e.name.as_str())
            .unwrap_or(encoding_name)
            .to_lowercase();
        match chunk {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if {
                let cn = class_name.resolve();
                cn == "Buf"
                    || cn == "Blob"
                    || cn == "utf8"
                    || cn.starts_with("buf")
                    || cn.starts_with("blob")
                    || cn.starts_with("Buf[")
                    || cn.starts_with("Blob[")
            } =>
            {
                if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                    return items
                        .iter()
                        .map(|v| match v {
                            Value::Int(i) => *i as u8,
                            _ => 0,
                        })
                        .collect();
                }
                Vec::new()
            }
            Value::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "utf16" => {
                if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                    let use_be = normalized == "utf-16be";
                    let mut bytes = Vec::with_capacity(items.len() * 2);
                    for item in items.iter() {
                        let unit = match item {
                            Value::Int(i) => *i as u16,
                            _ => 0u16,
                        };
                        let pair = if use_be {
                            unit.to_be_bytes()
                        } else {
                            unit.to_le_bytes()
                        };
                        bytes.extend_from_slice(&pair);
                    }
                    return bytes;
                }
                Vec::new()
            }
            Value::Array(items, ..) => items
                .iter()
                .map(|v| match v {
                    Value::Int(i) => *i as u8,
                    _ => 0,
                })
                .collect(),
            Value::Int(i) => vec![*i as u8],
            Value::Str(s) => s.as_bytes().to_vec(),
            other => other.to_string_value().into_bytes(),
        }
    }

    /// Dispatch a mutable native instance method.
    /// Returns (result_value, updated_attributes).
    pub(super) fn call_native_instance_method_mut(
        &mut self,
        class_name: &str,
        attributes: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match class_name {
            "Promise" => self.native_promise_mut(attributes, method, args),
            "Channel" => self.native_channel_mut(attributes, method, args),
            "Supply" => self.native_supply_mut(attributes, method, args),
            "Supplier" => self.native_supplier_mut(attributes, method, args),
            "Proc::Async" => self.native_proc_async_mut(attributes, method, args),
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on '{}'",
                method, class_name
            ))),
        }
    }

    /// Dispatch an immutable native instance method.
    pub(super) fn call_native_instance_method(
        &mut self,
        class_name: &str,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match class_name {
            "IO::Path" => self.native_io_path(attributes, method, args),
            "IO::Handle" => self.native_io_handle(attributes, method, args),
            "IO::Socket::INET" => self.native_socket_inet(attributes, method, args),
            "IO::Socket::Async" => self.native_socket_async(attributes, method, args),
            "IO::Socket::Async::Listener" => {
                self.native_socket_async_listener(attributes, method, args)
            }
            "IO::Pipe" => self.native_io_pipe(attributes, method),
            "Lock" | "Lock::Async" => self.native_lock(attributes, method, args),
            "Lock::ConditionVariable" => self.native_condition_variable(attributes, method, args),
            "Distro" => self.native_distro(attributes, method),
            "Kernel" => self.native_kernel(attributes, method, args),
            "Perl" => Ok(self.native_perl(attributes, method)),
            "Compiler" => Ok(self.native_perl(attributes, method)),
            "Promise" => self.native_promise(attributes, method, args),
            "Promise::Vow" => self.native_promise_vow(attributes, method, args),
            "Channel" => Ok(self.native_channel(attributes, method)),
            "Thread" => self.native_thread(attributes, method),
            "Proc::Async" => self.native_proc_async(attributes, method, args),
            "Proc" => Ok(self.native_proc(attributes, method)),
            "Supply" => self.native_supply(attributes, method, args),
            "Supplier" => self.native_supplier(attributes, method, args),
            "Tap" => self.native_tap(attributes, method),
            "ThreadPoolScheduler" | "CurrentThreadScheduler" => {
                self.native_scheduler(attributes, method, args)
            }
            "FakeScheduler" => self.native_fake_scheduler(attributes, method, args),
            "Cancellation" => self.native_cancellation(attributes, method),
            "Encoding::Builtin" => Ok(Self::native_encoding_builtin(attributes, method)),
            "Encoding::Encoder" => Ok(Self::native_encoding_encoder(attributes, method, &args)),
            "Encoding::Decoder" => Ok(Self::native_encoding_decoder(attributes, method, &args)),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on '{}'",
                method, class_name
            ))),
        }
    }

    fn cancellation_instance() -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "cancellation-id".to_string(),
            Value::Int(next_cancellation_id() as i64),
        );
        Value::make_instance(Symbol::intern("Cancellation"), attrs)
    }

    fn scheduler_times_arg(args: &[Value]) -> Result<Option<usize>, RuntimeError> {
        let Some(value) = Self::named_value(args, "times") else {
            return Ok(None);
        };
        let count = match value {
            Value::Int(i) => i,
            Value::Num(f) if f.is_finite() => f as i64,
            Value::Bool(b) => i64::from(b),
            Value::Str(s) => s.trim().parse::<i64>().map_err(|_| {
                RuntimeError::new(format!(
                    "Scheduler.cue: :times must be numeric, got '{}'",
                    s
                ))
            })?,
            other => {
                return Err(RuntimeError::new(format!(
                    "Scheduler.cue: :times must be numeric, got '{}'",
                    other.to_string_value()
                )));
            }
        };
        Ok(Some(count.max(0) as usize))
    }

    fn native_tap(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "cancel" | "close" => {
                if let Some(Value::Int(listener_id)) = attributes.get("listener-id") {
                    close_async_listener(*listener_id as u64);
                }
                Ok(Value::Nil)
            }
            "socket-port" => Ok(attributes
                .get("socket-port")
                .cloned()
                .unwrap_or(Value::Promise(SharedPromise::new()))),
            "socket-host" => Ok(attributes
                .get("socket-host")
                .cloned()
                .unwrap_or(Value::Promise(SharedPromise::new()))),
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Tap",
                method
            ))),
        }
    }

    fn native_cancellation(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "cancel" => {
                if let Some(Value::Int(id)) = attributes.get("cancellation-id")
                    && *id > 0
                    && let Some(flag) = cancellation_state(*id as u64)
                {
                    flag.store(true, Ordering::Relaxed);
                }
                Ok(Value::Nil)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Cancellation",
                method
            ))),
        }
    }

    fn native_scheduler(
        &mut self,
        _attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "cue" => {
                let callback = args.first().cloned().unwrap_or(Value::Nil);
                let times = Self::scheduler_times_arg(&args)?.unwrap_or(1);
                let cancellation = Self::cancellation_instance();
                let cancellation_id = match &cancellation {
                    Value::Instance { attributes, .. } => {
                        match attributes.get("cancellation-id").cloned() {
                            Some(Value::Int(id)) if id > 0 => id as u64,
                            _ => 0,
                        }
                    }
                    _ => 0,
                };
                let cancel_flag = cancellation_state(cancellation_id);
                for _ in 0..times {
                    if cancel_flag
                        .as_ref()
                        .is_some_and(|flag| flag.load(Ordering::Relaxed))
                    {
                        break;
                    }
                    self.call_sub_value(callback.clone(), Vec::new(), true)?;
                }
                Ok(cancellation)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Scheduler",
                method
            ))),
        }
    }

    fn native_fake_scheduler(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let scheduler_id = match attributes.get("scheduler_id") {
            Some(Value::Int(id)) => *id as u64,
            _ => 0,
        };
        match method {
            "cue" => {
                let callback = args.first().cloned().unwrap_or(Value::Nil);
                let every = Self::named_value(&args, "every").map(|v| match v {
                    Value::Int(i) => i as f64,
                    Value::Num(f) => f,
                    other => other.to_string_value().parse::<f64>().unwrap_or(0.0),
                });
                let delay = Self::named_value(&args, "in")
                    .map(|v| match v {
                        Value::Int(i) => i as f64,
                        Value::Num(f) => f,
                        Value::Instance {
                            class_name,
                            attributes,
                            ..
                        } if class_name == "Duration" => {
                            attributes.get("value").map(|v| v.to_f64()).unwrap_or(0.0)
                        }
                        other => other.to_string_value().parse::<f64>().unwrap_or(0.0),
                    })
                    .unwrap_or(0.0);

                if every.is_some() {
                    fake_scheduler_cue(scheduler_id, callback, every, delay);
                } else {
                    // No :every — execute immediately
                    self.call_sub_value(callback, Vec::new(), true)?;
                }
                Ok(Value::Nil)
            }
            "progress-by" => {
                let duration = args.first().map_or(0.0, |v| match v {
                    Value::Int(i) => *i as f64,
                    Value::Num(f) => *f,
                    Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } if class_name == "Duration" => {
                        attributes.get("value").map(|v| v.to_f64()).unwrap_or(0.0)
                    }
                    other => other.to_string_value().parse::<f64>().unwrap_or(0.0),
                });
                let (callbacks, counter_values) =
                    fake_scheduler_progress_by(scheduler_id, duration);
                // Execute regular callbacks
                for cb in callbacks {
                    self.call_sub_value(cb, Vec::new(), true)?;
                }
                // Push counter values to supply_emit_buffer
                if !counter_values.is_empty()
                    && let Some(buf) = self.supply_emit_buffer.last_mut()
                {
                    for v in counter_values {
                        buf.push(Value::Int(v));
                    }
                }
                Ok(Value::Nil)
            }
            "time" => {
                if let Ok(map) = fake_scheduler_map().lock()
                    && let Some(state) = map.get(&scheduler_id)
                {
                    return Ok(Value::Num(state.time));
                }
                Ok(Value::Num(0.0))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on FakeScheduler",
                method
            ))),
        }
    }

    fn native_lock(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "protect" => {
                let lock_id = match attributes.get("lock-id") {
                    Some(Value::Int(id)) if *id > 0 => *id as u64,
                    _ => {
                        return Err(RuntimeError::new(
                            "Lock.protect called on Lock without lock-id",
                        ));
                    }
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.protect could not find lock state"))?;
                let me = current_thread_id();
                acquire_lock(&lock, me)?;
                let code = args.first().cloned().unwrap_or(Value::Nil);
                let result = self.call_protect_block(&code);
                let _ = release_lock(&lock, me);
                result
            }
            "lock" => {
                let lock_id = match attributes.get("lock-id") {
                    Some(Value::Int(id)) if *id > 0 => *id as u64,
                    _ => {
                        return Err(RuntimeError::new("Lock.lock called on invalid Lock"));
                    }
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.lock could not find lock state"))?;
                let me = current_thread_id();
                acquire_lock(&lock, me)?;
                Ok(Value::Nil)
            }
            "unlock" => {
                let lock_id = match attributes.get("lock-id") {
                    Some(Value::Int(id)) if *id > 0 => *id as u64,
                    _ => {
                        return Err(RuntimeError::new("Lock.unlock called on invalid Lock"));
                    }
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.unlock could not find lock state"))?;
                let me = current_thread_id();
                release_lock(&lock, me)?;
                Ok(Value::Nil)
            }
            "condition" => {
                let lock_id = match attributes.get("lock-id") {
                    Some(Value::Int(id)) if *id > 0 => *id as u64,
                    _ => return Err(RuntimeError::new("Lock.condition called on invalid Lock")),
                };
                let lock = lock_runtime_by_id(lock_id)
                    .ok_or_else(|| RuntimeError::new("Lock.condition could not find lock state"))?;
                let cond_id = next_condition_id();
                let _ = ensure_condition(&lock, cond_id).ok_or_else(|| {
                    RuntimeError::new("Lock.condition failed to create condition")
                })?;
                let mut attrs = HashMap::new();
                attrs.insert("lock-id".to_string(), Value::Int(lock_id as i64));
                attrs.insert("cond-id".to_string(), Value::Int(cond_id as i64));
                Ok(Value::make_instance(
                    Symbol::intern("Lock::ConditionVariable"),
                    attrs,
                ))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Lock",
                method
            ))),
        }
    }

    fn native_condition_variable(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let lock_id = match attributes.get("lock-id") {
            Some(Value::Int(id)) if *id > 0 => *id as u64,
            _ => return Err(RuntimeError::new("Condition variable has invalid lock-id")),
        };
        let cond_id = match attributes.get("cond-id") {
            Some(Value::Int(id)) if *id > 0 => *id as u64,
            _ => return Err(RuntimeError::new("Condition variable has invalid cond-id")),
        };
        let lock = lock_runtime_by_id(lock_id)
            .ok_or_else(|| RuntimeError::new("Condition variable lock state not found"))?;
        let cond = ensure_condition(&lock, cond_id)
            .ok_or_else(|| RuntimeError::new("Condition variable state not found"))?;
        match method {
            "signal" => {
                cond.notify_one();
                Ok(Value::Nil)
            }
            "signal_all" => {
                cond.notify_all();
                Ok(Value::Nil)
            }
            "wait" => {
                let maybe_test = args.first().cloned();
                let me = current_thread_id();
                let mut state = lock
                    .state
                    .lock()
                    .map_err(|_| RuntimeError::new("Lock state is poisoned"))?;
                match state.owner {
                    Some(owner) if owner == me => {}
                    _ => {
                        return Err(RuntimeError::new(
                            "Condition.wait requires the current thread to hold the lock",
                        ));
                    }
                }
                if let Some(test) = maybe_test.clone()
                    && self.call_sub_value(test, Vec::new(), false)?.truthy()
                {
                    return Ok(Value::Nil);
                }
                let held_recursion = state.recursion;
                state.owner = None;
                state.recursion = 0;
                lock.lock_cv.notify_one();
                loop {
                    state = cond
                        .wait(state)
                        .map_err(|_| RuntimeError::new("Condition wait failed"))?;
                    while state.owner.is_some() && state.owner != Some(me) {
                        state = lock
                            .lock_cv
                            .wait(state)
                            .map_err(|_| RuntimeError::new("Lock reacquire wait failed"))?;
                    }
                    state.owner = Some(me);
                    state.recursion = held_recursion.max(1);
                    drop(state);

                    let predicate_ok = if let Some(test) = maybe_test.clone() {
                        self.call_sub_value(test, Vec::new(), false)?.truthy()
                    } else {
                        true
                    };
                    if predicate_ok {
                        return Ok(Value::Nil);
                    }
                    state = lock
                        .state
                        .lock()
                        .map_err(|_| RuntimeError::new("Lock state is poisoned"))?;
                    state.owner = None;
                    state.recursion = 0;
                    lock.lock_cv.notify_one();
                }
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Lock::ConditionVariable",
                method
            ))),
        }
    }

    // --- Promise mutable ---

    fn native_promise_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "keep" => {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                attrs.insert("result".to_string(), value);
                attrs.insert("status".to_string(), Value::str_from("Kept"));
                Ok((Value::Nil, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Promise",
                method
            ))),
        }
    }

    // --- Channel mutable ---

    fn native_channel_mut(
        &mut self,
        mut attrs: HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        match method {
            "send" => {
                let value = args.first().cloned().unwrap_or(Value::Nil);
                match attrs.get_mut("queue") {
                    Some(Value::Array(items, ..)) => Arc::make_mut(items).push(value),
                    _ => {
                        attrs.insert("queue".to_string(), Value::array(vec![value]));
                    }
                }
                Ok((Value::Nil, attrs))
            }
            "receive" => {
                let mut value = Value::Nil;
                if let Some(Value::Array(items, ..)) = attrs.get_mut("queue")
                    && !items.is_empty()
                {
                    value = Arc::make_mut(items).remove(0);
                }
                Ok((value, attrs))
            }
            "close" => {
                attrs.insert("closed".to_string(), Value::Bool(true));
                Ok((Value::Nil, attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No native mutable method '{}' on Channel",
                method
            ))),
        }
    }

    // --- Supply immutable ---

    // --- Proc::Async mutable ---

    // --- Promise immutable ---

    fn native_promise(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "result" => Ok(attributes.get("result").cloned().unwrap_or(Value::Nil)),
            "status" => Ok(attributes
                .get("status")
                .cloned()
                .unwrap_or(Value::str_from("Planned"))),
            "then" => {
                let block = args.first().cloned().unwrap_or(Value::Nil);
                let status = attributes
                    .get("status")
                    .cloned()
                    .unwrap_or(Value::str_from("Planned"));
                if matches!(status, Value::Str(ref s) if s.as_str() == "Kept") {
                    let value = attributes.get("result").cloned().unwrap_or(Value::Nil);
                    let result = self.call_sub_value(block, vec![value], true)?;
                    Ok(self.make_promise_instance("Kept", result))
                } else {
                    Ok(self.make_promise_instance("Planned", Value::Nil))
                }
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Promise",
                method
            ))),
        }
    }

    fn native_promise_vow(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let promise = attributes
            .get("promise")
            .ok_or_else(|| RuntimeError::new("Promise::Vow missing promise"))?;
        let Value::Promise(shared) = promise else {
            return Err(RuntimeError::new("Promise::Vow promise is not a Promise"));
        };
        match method {
            "keep" => {
                let value = args.into_iter().next().unwrap_or(Value::Bool(true));
                if let Err(_status) = shared.try_keep(value) {
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "message".to_string(),
                        Value::str(
                            "Access denied to keep/break this Promise; already vowed".to_string(),
                        ),
                    );
                    let ex = Value::make_instance(Symbol::intern("X::Promise::Vowed"), attrs);
                    let mut err = RuntimeError::new(
                        "Access denied to keep/break this Promise; already vowed".to_string(),
                    );
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                Ok(Value::Nil)
            }
            "break" => {
                let reason = args
                    .into_iter()
                    .next()
                    .unwrap_or_else(|| Value::str_from("Died"));
                if let Err(_status) = shared.try_break(reason) {
                    let mut attrs = HashMap::new();
                    attrs.insert(
                        "message".to_string(),
                        Value::str(
                            "Access denied to keep/break this Promise; already vowed".to_string(),
                        ),
                    );
                    let ex = Value::make_instance(Symbol::intern("X::Promise::Vowed"), attrs);
                    let mut err = RuntimeError::new(
                        "Access denied to keep/break this Promise; already vowed".to_string(),
                    );
                    err.exception = Some(Box::new(ex));
                    return Err(err);
                }
                Ok(Value::Nil)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Promise::Vow",
                method
            ))),
        }
    }

    // --- Channel immutable ---

    fn native_channel(&self, attributes: &HashMap<String, Value>, method: &str) -> Value {
        match method {
            "closed" => attributes
                .get("closed")
                .cloned()
                .unwrap_or(Value::Bool(false)),
            _ => Value::Nil,
        }
    }

    // --- Proc::Async immutable ---

    fn native_proc_async(
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
                Ok(cmd)
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

    fn native_proc(&self, attributes: &HashMap<String, Value>, method: &str) -> Value {
        match method {
            "exitcode" => attributes.get("exitcode").cloned().unwrap_or(Value::Nil),
            "signal" => attributes.get("signal").cloned().unwrap_or(Value::Int(0)),
            "command" => attributes
                .get("command")
                .cloned()
                .unwrap_or(Value::array(Vec::new())),
            "pid" => attributes.get("pid").cloned().unwrap_or(Value::Nil),
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

    // --- Distro ---

    fn native_distro(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "name" | "auth" | "desc" | "release" | "path-sep" | "is-win" | "version"
            | "signature" => Ok(attributes.get(method).cloned().unwrap_or(Value::Nil)),
            "gist" => {
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let v = attributes
                    .get("version")
                    .map(|v| {
                        if let Value::Version { parts, .. } = v {
                            Value::version_parts_to_string(parts)
                        } else {
                            v.to_string_value()
                        }
                    })
                    .unwrap_or_default();
                Ok(Value::str(format!("{} ({})", n, v)))
            }
            "Str" => {
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::str(n))
            }
            "raku" | "perl" => {
                let release = attributes
                    .get("release")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let path_sep = attributes
                    .get("path-sep")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let auth = attributes
                    .get("auth")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let ver = attributes
                    .get("version")
                    .map(|v| {
                        if let Value::Version { parts, .. } = v {
                            format!("v{}", Value::version_parts_to_string(parts))
                        } else {
                            v.to_string_value()
                        }
                    })
                    .unwrap_or_default();
                let desc = attributes
                    .get("desc")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::str(format!(
                    "Distro.new(release => \"{}\", path-sep => \"{}\", name => \"{}\", auth => \"{}\", version => {}, signature => Blob, desc => \"{}\")",
                    release, path_sep, n, auth, ver, desc
                )))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Distro",
                method
            ))),
        }
    }

    // --- Kernel ---

    fn native_kernel(
        &self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "name" | "auth" | "desc" | "release" | "hardware" | "arch" | "bits" | "hostname"
            | "version" | "signature" | "signals" => {
                Ok(attributes.get(method).cloned().unwrap_or(Value::Nil))
            }
            "signal" => {
                // .signal(SIGHUP), .signal("SIGHUP"), .signal("HUP"), .signal(Int)
                let arg = args.first().cloned().unwrap_or(Value::Nil);
                let signal_names = [
                    "", "HUP", "INT", "QUIT", "ILL", "TRAP", "ABRT", "BUS", "FPE", "KILL", "USR1",
                    "SEGV", "USR2", "PIPE", "ALRM", "TERM", "STKFLT", "CHLD", "CONT", "STOP",
                    "TSTP", "TTIN", "TTOU", "URG", "XCPU", "XFSZ", "VTALRM", "PROF", "WINCH", "IO",
                    "PWR", "SYS",
                ];
                match &arg {
                    Value::Int(n) => Ok(Value::Int(*n)),
                    Value::Str(s) => {
                        let name = s
                            .strip_prefix("SIG")
                            .map(|s| s.to_string())
                            .unwrap_or_else(|| s.to_string());
                        for (i, sn) in signal_names.iter().enumerate() {
                            if *sn == name {
                                return Ok(Value::Int(i as i64));
                            }
                        }
                        Ok(Value::Int(0))
                    }
                    Value::Enum { key, .. } => {
                        let key_str = key.resolve();
                        let name = key_str.strip_prefix("SIG").unwrap_or(&key_str);
                        for (i, sn) in signal_names.iter().enumerate() {
                            if *sn == name {
                                return Ok(Value::Int(i as i64));
                            }
                        }
                        Ok(Value::Int(0))
                    }
                    _ => Ok(Value::Int(0)),
                }
            }
            "gist" | "Str" => {
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::str(n))
            }
            "raku" | "perl" => {
                let n = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let auth = attributes
                    .get("auth")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Ok(Value::str(format!(
                    "Kernel.new(release => Str, hardware => Str, arch => Str, bits => Int, name => \"{}\", auth => \"{}\", version => Version, signature => Blob, desc => Str)",
                    n, auth
                )))
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on Kernel",
                method
            ))),
        }
    }

    // --- Perl ---

    fn native_perl(&self, attributes: &HashMap<String, Value>, method: &str) -> Value {
        match method {
            "compiler" => {
                let mut compiler_attrs = HashMap::new();
                compiler_attrs.insert("name".to_string(), Value::str_from("mutsu"));
                compiler_attrs.insert("auth".to_string(), Value::str_from("github.com/tokuhirom"));
                compiler_attrs.insert(
                    "version".to_string(),
                    Value::Version {
                        parts: vec![
                            crate::value::VersionPart::Num(0),
                            crate::value::VersionPart::Num(1),
                            crate::value::VersionPart::Num(0),
                        ],
                        plus: false,
                        minus: false,
                    },
                );
                compiler_attrs.insert(
                    "signature".to_string(),
                    Value::make_instance(Symbol::intern("Blob"), {
                        let mut a = HashMap::new();
                        a.insert("values".to_string(), Value::array(vec![Value::Int(0)]));
                        a
                    }),
                );
                compiler_attrs.insert(
                    "desc".to_string(),
                    Value::str_from("mutsu Raku interpreter"),
                );
                compiler_attrs.insert("release".to_string(), Value::str_from("0.1.0"));
                compiler_attrs.insert("codename".to_string(), Value::str_from("mutsu"));
                compiler_attrs.insert("id".to_string(), Value::str(String::new()));
                Value::make_instance(Symbol::intern("Compiler"), compiler_attrs)
            }
            "backend" => Value::str_from("mutsu"),
            "gist" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let version = attributes
                    .get("version")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(format!("{} ({})", name, version))
            }
            "Str" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(name)
            }
            "raku" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(format!("{}.new(...)", name))
            }
            _ => attributes.get(method).cloned().unwrap_or(Value::Nil),
        }
    }

    // --- Thread ---

    fn native_thread(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
    ) -> Result<Value, RuntimeError> {
        match method {
            "finish" => self.dispatch_thread_finish(attributes),
            "id" => Ok(attributes
                .get("id")
                .or_else(|| attributes.get("thread_id"))
                .cloned()
                .unwrap_or(Value::Int(0))),
            "WHAT" => Ok(Value::Package(crate::symbol::Symbol::intern("Thread"))),
            "Str" | "gist" => Ok(Value::str_from("Thread")),
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on Thread",
                method
            ))),
        }
    }

    // --- IO::Socket::INET ---

    fn native_socket_inet(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let handle_id = attributes.get("handle").and_then(|v| {
            if let Value::Int(i) = v {
                Some(*i as usize)
            } else {
                None
            }
        });
        match method {
            "getpeername" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let state = self
                    .handles
                    .get(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                if state.closed {
                    return Err(RuntimeError::new("Socket is closed"));
                }
                if let Some(ref stream) = state.socket {
                    let addr = stream
                        .peer_addr()
                        .map_err(|e| RuntimeError::new(format!("getpeername failed: {}", e)))?;
                    Ok(Value::str(addr.to_string()))
                } else {
                    Err(RuntimeError::new("Socket not connected"))
                }
            }
            "close" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let state = self
                    .handles
                    .get_mut(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                state.closed = true;
                state.socket = None;
                state.listener = None;
                Ok(Value::Bool(true))
            }
            "localport" => {
                // Return localport from attributes
                Ok(attributes
                    .get("localport")
                    .cloned()
                    .unwrap_or(Value::Int(0)))
            }
            "accept" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                // Take listener out temporarily to avoid borrow issues
                let listener = {
                    let state = self
                        .handles
                        .get_mut(&id)
                        .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                    state.listener.as_ref().and_then(|l| l.try_clone().ok())
                };
                let listener =
                    listener.ok_or_else(|| RuntimeError::new("Socket is not listening"))?;
                let (stream, _addr) = listener
                    .accept()
                    .map_err(|e| RuntimeError::new(format!("accept failed: {}", e)))?;
                // Create a new handle for the accepted connection
                let new_id = self.next_handle_id;
                self.next_handle_id += 1;
                let state = super::IoHandleState {
                    target: super::IoHandleTarget::Socket,
                    mode: super::IoHandleMode::ReadWrite,
                    path: None,
                    line_separators: self.default_line_separators(),
                    line_chomp: true,
                    encoding: "utf-8".to_string(),
                    file: None,
                    socket: Some(stream),
                    listener: None,
                    closed: false,
                    out_buffer_capacity: None,
                    out_buffer_pending: Vec::new(),
                    bin: false,
                };
                self.handles.insert(new_id, state);
                let mut attrs = HashMap::new();
                attrs.insert("handle".to_string(), Value::Int(new_id as i64));
                Ok(Value::make_instance(
                    crate::symbol::Symbol::intern("IO::Socket::INET"),
                    attrs,
                ))
            }
            "print" | "say" | "put" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let mut data = String::new();
                for arg in &args {
                    data.push_str(&arg.to_string_value());
                }
                if method == "say" || method == "put" {
                    data.push('\n');
                }
                let state = self
                    .handles
                    .get_mut(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                if let Some(ref mut stream) = state.socket {
                    use std::io::Write;
                    stream
                        .write_all(data.as_bytes())
                        .map_err(|e| RuntimeError::new(format!("print failed: {}", e)))?;
                    stream
                        .flush()
                        .map_err(|e| RuntimeError::new(format!("flush failed: {}", e)))?;
                    Ok(Value::Bool(true))
                } else {
                    Err(RuntimeError::new("Socket not connected"))
                }
            }
            "write" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let buf_data = if let Some(arg) = args.first() {
                    Self::extract_bytes(arg)
                        .ok_or_else(|| RuntimeError::new("write expects a Buf argument"))?
                } else {
                    return Err(RuntimeError::new("write expects a Buf argument"));
                };
                let state = self
                    .handles
                    .get_mut(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                if let Some(ref mut stream) = state.socket {
                    use std::io::Write;
                    stream
                        .write_all(&buf_data)
                        .map_err(|e| RuntimeError::new(format!("write failed: {}", e)))?;
                    stream
                        .flush()
                        .map_err(|e| RuntimeError::new(format!("flush failed: {}", e)))?;
                    Ok(Value::Bool(true))
                } else {
                    Err(RuntimeError::new("Socket not connected"))
                }
            }
            "recv" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                // Check for :bin named arg
                let mut bin_mode = false;
                let mut max_chars: Option<usize> = None;
                for arg in &args {
                    match arg {
                        Value::Int(n) => max_chars = Some(*n as usize),
                        Value::Pair(key, value) => {
                            if key.as_str() == "bin" {
                                bin_mode = value.as_ref().truthy();
                            }
                        }
                        _ => {}
                    }
                }
                self.socket_recv(id, max_chars, bin_mode)
            }
            "read" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let nbytes = args
                    .first()
                    .map(|v| match v {
                        Value::Int(i) => *i as usize,
                        _ => 0,
                    })
                    .unwrap_or(0);
                self.socket_read_bytes(id, nbytes)
            }
            "get" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                self.socket_get_line(id)
            }
            "lines" => {
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                self.socket_lines(id)
            }
            "nl-in" => {
                // Getter for nl-in
                let id =
                    handle_id.ok_or_else(|| RuntimeError::new("IO::Socket::INET has no handle"))?;
                let state = self
                    .handles
                    .get(&id)
                    .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
                let seps: Vec<Value> = state
                    .line_separators
                    .iter()
                    .map(|s| Value::str(String::from_utf8_lossy(s).to_string()))
                    .collect();
                if seps.len() == 1 {
                    Ok(seps.into_iter().next().unwrap_or(Value::str_from("\n")))
                } else {
                    Ok(Value::array(seps))
                }
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::INET",
                method
            ))),
        }
    }

    fn async_socket_status_result(status: &str, result: Value, cause: Value) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("status".to_string(), Value::str_from(status));
        attrs.insert("result".to_string(), result);
        attrs.insert("cause".to_string(), cause);
        Value::make_instance(Symbol::intern("IO::Socket::Async::StatusResult"), attrs)
    }

    fn async_socket_kept(result: Value) -> Value {
        Self::async_socket_status_result("Kept", result, Value::Nil)
    }

    fn async_socket_broken(cause: Value) -> Value {
        Self::async_socket_status_result("Broken", Value::Nil, cause)
    }

    fn async_supplier_emit_value(
        &mut self,
        supplier_id: u64,
        value: Value,
    ) -> Result<(), RuntimeError> {
        supplier_emit(supplier_id, value.clone());
        let actions = supplier_emit_callbacks(supplier_id, &value);
        for action in actions {
            match action {
                SupplierEmitAction::Call(tap, emitted, delay_seconds) => {
                    Self::sleep_for_supply_delay(delay_seconds);
                    let _ = self.call_sub_value(tap, vec![emitted], true);
                }
                SupplierEmitAction::UniqueCheck {
                    callback,
                    value: val,
                    delay_seconds,
                    ..
                } => {
                    Self::sleep_for_supply_delay(delay_seconds);
                    let _ = self.call_sub_value(callback, vec![val], true);
                }
            }
        }
        Ok(())
    }

    fn async_supplier_done_value(&mut self, supplier_id: u64) {
        supplier_done(supplier_id);
        for (tap, emitted) in flush_supplier_line_taps(supplier_id) {
            let _ = self.call_sub_value(tap, vec![emitted], true);
        }
        for done_cb in take_supplier_done_callbacks(supplier_id) {
            let _ = self.call_sub_value(done_cb, Vec::new(), true);
        }
    }

    fn async_supplier_quit_value(&mut self, supplier_id: u64, reason: Value) {
        supplier_quit(supplier_id, reason.clone());
        for (tap, emitted) in flush_supplier_line_taps(supplier_id) {
            let _ = self.call_sub_value(tap, vec![emitted], true);
        }
        for quit_cb in take_supplier_quit_callbacks(supplier_id) {
            let _ = self.call_sub_value(quit_cb, vec![reason.clone()], true);
        }
        for done_cb in take_supplier_done_callbacks(supplier_id) {
            let _ = self.call_sub_value(done_cb, Vec::new(), true);
        }
    }

    fn native_socket_async_listener(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        match method {
            "tap" | "act" => {
                let callback = args.first().cloned().unwrap_or(Value::Nil);
                let quit_cb = Self::named_value(&args, "quit");
                let host = attributes
                    .get("host")
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| "0.0.0.0".to_string());
                let requested_port = attributes
                    .get("port")
                    .and_then(|v| match v {
                        Value::Int(i) => Some(*i as u16),
                        Value::Num(n) if n.is_finite() => Some(*n as u16),
                        _ => None,
                    })
                    .unwrap_or(0);
                let enc = attributes
                    .get("enc")
                    .map(Value::to_string_value)
                    .unwrap_or_else(|| "utf-8".to_string());
                let port = if requested_port == 0 {
                    allocate_async_listen_port()
                } else {
                    requested_port
                };

                let host_ok = host == "0.0.0.0"
                    || host == "::"
                    || host == "127.0.0.1"
                    || host == "::1"
                    || host == "localhost"
                    || host.parse::<std::net::IpAddr>().is_ok();
                if !host_ok {
                    if let Some(q) = quit_cb.clone() {
                        let mut attrs = HashMap::new();
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!("Failed to resolve '{}'", host)),
                        );
                        let ex = Value::make_instance(Symbol::intern("Exception"), attrs);
                        let _ = self.call_sub_value(q, vec![ex], true);
                    }
                    return Ok(Value::make_instance(Symbol::intern("Tap"), HashMap::new()));
                }

                if async_port_in_use(&host, port) {
                    if let Some(q) = quit_cb.clone() {
                        let mut attrs = HashMap::new();
                        attrs.insert(
                            "message".to_string(),
                            Value::str_from("Address already in use"),
                        );
                        let ex = Value::make_instance(Symbol::intern("Exception"), attrs);
                        let _ = self.call_sub_value(q, vec![ex], true);
                    }
                    return Ok(Value::make_instance(Symbol::intern("Tap"), HashMap::new()));
                }

                let listener_id = next_async_listener_id();
                register_async_listener(
                    listener_id,
                    AsyncSocketListenerState {
                        host: host.clone(),
                        port,
                        callback,
                        closed: false,
                        encoding: enc,
                    },
                );

                let socket_port = SharedPromise::new();
                socket_port.keep(Value::Int(port as i64), String::new(), String::new());
                let socket_host = SharedPromise::new();
                socket_host.keep(Value::str(host), String::new(), String::new());

                let mut tap_attrs = HashMap::new();
                tap_attrs.insert("listener-id".to_string(), Value::Int(listener_id as i64));
                tap_attrs.insert(
                    "socket-port".to_string(),
                    Value::Promise(socket_port.clone()),
                );
                tap_attrs.insert("socket-host".to_string(), Value::Promise(socket_host));
                Ok(Value::make_instance(Symbol::intern("Tap"), tap_attrs))
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::Async::Listener",
                method
            ))),
        }
    }

    fn native_socket_async(
        &mut self,
        attributes: &HashMap<String, Value>,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let conn_id = attributes.get("conn-id").and_then(|v| match v {
            Value::Int(i) => Some(*i as u64),
            _ => None,
        });

        match method {
            "socket-port" => Ok(attributes
                .get("socket-port")
                .cloned()
                .unwrap_or(Value::Int(0))),
            "peer-port" => Ok(attributes
                .get("peer-port")
                .cloned()
                .unwrap_or(Value::Int(0))),
            "socket-host" => Ok(attributes
                .get("socket-host")
                .cloned()
                .unwrap_or_else(|| Value::str_from("0.0.0.0"))),
            "peer-host" => Ok(attributes
                .get("peer-host")
                .cloned()
                .unwrap_or_else(|| Value::str_from("0.0.0.0"))),
            "close" => {
                if let Some(id) = conn_id
                    && let Some(state) = get_async_connection(id)
                {
                    update_async_connection(id, |conn| {
                        conn.closed = true;
                    });
                    for sid in &state.supply_ids {
                        self.async_supplier_done_value(*sid);
                    }
                    if let Some(peer_id) = state.peer_id
                        && let Some(peer) = get_async_connection(peer_id)
                    {
                        update_async_connection(peer_id, |conn| {
                            conn.peer_closed = true;
                        });
                        if let Some((callback, socket)) = take_deferred_accept_callback(peer_id) {
                            let _ = self.call_sub_value(callback, vec![socket], true);
                        }
                        for sid in &peer.supply_ids {
                            if let Some(supply) = get_async_supply(*sid)
                                && !supply.is_bin
                                && !supply.text_buffer.is_empty()
                            {
                                let _ = self.async_supplier_emit_value(
                                    *sid,
                                    Value::str(supply.text_buffer.clone()),
                                );
                                update_async_supply(*sid, |st| st.text_buffer.clear());
                            }
                            self.async_supplier_done_value(*sid);
                        }
                    }
                }
                Ok(Value::Nil)
            }
            "Supply" => {
                let id = conn_id.ok_or_else(|| RuntimeError::new("Missing async conn-id"))?;
                let is_bin = Self::named_bool(&args, "bin");
                let supply_enc = Self::named_value(&args, "enc")
                    .map(|v| v.to_string_value())
                    .or_else(|| attributes.get("enc").map(Value::to_string_value))
                    .unwrap_or_else(|| "utf-8".to_string());
                let supply_id = next_supply_id();
                register_async_supply(
                    supply_id,
                    AsyncSocketSupplyState {
                        is_bin,
                        encoding: supply_enc,
                        text_buffer: String::new(),
                        byte_buffer: Vec::new(),
                    },
                );
                update_async_connection(id, |conn| conn.supply_ids.push(supply_id));

                let mut initial_values: Vec<Value> = Vec::new();
                let mut is_supplier_done = false;
                let mut attrs = HashMap::new();
                attrs.insert("values".to_string(), Value::array(Vec::new()));
                attrs.insert("taps".to_string(), Value::array(Vec::new()));
                attrs.insert("live".to_string(), Value::Bool(true));
                attrs.insert("supplier_id".to_string(), Value::Int(supply_id as i64));
                if let Some(conn_state) = get_async_connection(id) {
                    is_supplier_done = conn_state.closed || conn_state.peer_closed;
                    if !conn_state.pending_bytes.is_empty() {
                        if is_bin {
                            let mut battrs = HashMap::new();
                            battrs.insert(
                                "bytes".to_string(),
                                Value::array(
                                    conn_state
                                        .pending_bytes
                                        .iter()
                                        .map(|b| Value::Int(*b as i64))
                                        .collect(),
                                ),
                            );
                            initial_values
                                .push(Value::make_instance(Symbol::intern("Buf"), battrs));
                        } else if let Some(supply) = get_async_supply(supply_id) {
                            let decoded = if supply.encoding.eq_ignore_ascii_case("utf-8") {
                                String::from_utf8_lossy(&conn_state.pending_bytes).to_string()
                            } else if supply.encoding.eq_ignore_ascii_case("latin-1")
                                || supply.encoding.eq_ignore_ascii_case("iso-8859-1")
                            {
                                conn_state
                                    .pending_bytes
                                    .iter()
                                    .map(|b| char::from_u32(*b as u32).unwrap_or('\u{FFFD}'))
                                    .collect()
                            } else {
                                self.decode_with_encoding(
                                    &conn_state.pending_bytes,
                                    &supply.encoding,
                                )
                                .unwrap_or_default()
                            };
                            if !decoded.is_empty() {
                                let mut start = 0usize;
                                for (idx, ch) in decoded.char_indices() {
                                    if ch == '\n' {
                                        initial_values
                                            .push(Value::str(decoded[start..=idx].to_string()));
                                        start = idx + ch.len_utf8();
                                    }
                                }
                                let tail = decoded[start..].to_string();
                                update_async_supply(supply_id, |st| st.text_buffer = tail);
                            }
                        }
                        update_async_connection(id, |conn| conn.pending_bytes.clear());
                    }
                }
                if is_supplier_done
                    && !is_bin
                    && let Some(supply) = get_async_supply(supply_id)
                    && !supply.text_buffer.is_empty()
                {
                    initial_values.push(Value::str(supply.text_buffer.clone()));
                    update_async_supply(supply_id, |st| st.text_buffer.clear());
                }
                attrs.insert("values".to_string(), Value::array(initial_values));
                if is_supplier_done {
                    supplier_done(supply_id);
                    attrs.insert("supplier_done".to_string(), Value::Bool(true));
                    attrs.insert("live".to_string(), Value::Bool(false));
                }
                Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
            }
            "write" | "print" => {
                let promise = SharedPromise::new();
                let result = (|| -> Result<Value, RuntimeError> {
                    let id = conn_id.ok_or_else(|| RuntimeError::new("Missing async conn-id"))?;
                    let state = get_async_connection(id)
                        .ok_or_else(|| RuntimeError::new("Unknown async socket connection"))?;
                    if state.closed {
                        return Ok(Self::async_socket_broken(Value::str_from(
                            "Socket is closed",
                        )));
                    }
                    let peer_id = state
                        .peer_id
                        .ok_or_else(|| RuntimeError::new("Peer missing"))?;
                    let peer = get_async_connection(peer_id)
                        .ok_or_else(|| RuntimeError::new("Unknown peer connection"))?;

                    let bytes = if method == "write" {
                        args.last()
                            .and_then(Self::extract_bytes)
                            .unwrap_or_else(|| {
                                args.last()
                                    .map(Value::to_string_value)
                                    .unwrap_or_default()
                                    .into_bytes()
                            })
                    } else {
                        let text = args
                            .last()
                            .map(|v| self.render_str_value(v))
                            .unwrap_or_default();
                        self.encode_with_encoding(&text, &state.encoding)?
                    };
                    if bytes.is_empty() {
                        return Ok(Self::async_socket_kept(Value::Bool(true)));
                    }
                    if peer.closed {
                        return Ok(Self::async_socket_broken(Value::str_from(
                            "Socket is closed",
                        )));
                    }

                    if peer.supply_ids.is_empty() {
                        update_async_connection(peer_id, |conn| {
                            conn.pending_bytes.extend_from_slice(&bytes);
                        });
                        return Ok(Self::async_socket_kept(Value::Bool(true)));
                    }

                    for sid in &peer.supply_ids {
                        if let Some(supply) = get_async_supply(*sid) {
                            if supply.is_bin {
                                let mut battrs = HashMap::new();
                                battrs.insert(
                                    "bytes".to_string(),
                                    Value::array(
                                        bytes.iter().map(|b| Value::Int(*b as i64)).collect(),
                                    ),
                                );
                                let buf = Value::make_instance(Symbol::intern("Buf"), battrs);
                                let _ = self.async_supplier_emit_value(*sid, buf);
                            } else {
                                let mut pending = supply.byte_buffer.clone();
                                pending.extend_from_slice(&bytes);
                                let (decoded, remainder) = if supply
                                    .encoding
                                    .eq_ignore_ascii_case("utf-8")
                                {
                                    match std::str::from_utf8(&pending) {
                                        Ok(s) => (s.to_string(), Vec::new()),
                                        Err(e) => {
                                            if e.error_len().is_none() {
                                                let valid = &pending[..e.valid_up_to()];
                                                let valid_decoded = std::str::from_utf8(valid)
                                                    .unwrap_or("")
                                                    .to_string();
                                                (valid_decoded, pending[e.valid_up_to()..].to_vec())
                                            } else {
                                                self.async_supplier_quit_value(
                                                    *sid,
                                                    Value::str_from(
                                                        "Malformed UTF-8 on socket Supply",
                                                    ),
                                                );
                                                continue;
                                            }
                                        }
                                    }
                                } else if supply.encoding.eq_ignore_ascii_case("latin-1")
                                    || supply.encoding.eq_ignore_ascii_case("iso-8859-1")
                                {
                                    (
                                        pending
                                            .iter()
                                            .map(|b| {
                                                char::from_u32(*b as u32).unwrap_or('\u{FFFD}')
                                            })
                                            .collect(),
                                        Vec::new(),
                                    )
                                } else {
                                    match self.decode_with_encoding(&pending, &supply.encoding) {
                                        Ok(s) => (s, Vec::new()),
                                        Err(e) => {
                                            self.async_supplier_quit_value(
                                                *sid,
                                                Value::str(e.message),
                                            );
                                            continue;
                                        }
                                    }
                                };
                                let mut merged = supply.text_buffer.clone();
                                merged.push_str(&decoded);
                                let mut start = 0usize;
                                for (idx, ch) in merged.char_indices() {
                                    if ch == '\n' {
                                        let chunk = merged[start..=idx].to_string();
                                        let _ =
                                            self.async_supplier_emit_value(*sid, Value::str(chunk));
                                        start = idx + ch.len_utf8();
                                    }
                                }
                                let tail = merged[start..].to_string();
                                update_async_supply(*sid, |st| {
                                    st.text_buffer = tail;
                                    st.byte_buffer = remainder;
                                });
                            }
                        }
                    }
                    Ok(Self::async_socket_kept(Value::Bool(true)))
                })();

                match result {
                    Ok(v) => promise.keep(v, String::new(), String::new()),
                    Err(e) => promise.keep(
                        Self::async_socket_broken(Value::str(e.message)),
                        String::new(),
                        String::new(),
                    ),
                }
                Ok(Value::Promise(promise))
            }
            _ => Err(RuntimeError::new(format!(
                "No method '{}' on IO::Socket::Async",
                method
            ))),
        }
    }

    /// Create a Buf instance from raw bytes
    fn make_buf(bytes: Vec<u8>) -> Value {
        let byte_vals: Vec<Value> = bytes.into_iter().map(|b| Value::Int(b as i64)).collect();
        let mut attrs = HashMap::new();
        attrs.insert("bytes".to_string(), Value::array(byte_vals));
        Value::make_instance(crate::symbol::Symbol::intern("Buf[uint8]"), attrs)
    }

    /// Extract bytes from a Buf/Blob instance or array
    fn extract_bytes(val: &Value) -> Option<Vec<u8>> {
        match val {
            Value::Instance {
                class_name,
                attributes,
                ..
            } if {
                let cn = class_name.resolve();
                cn == "Buf"
                    || cn == "Blob"
                    || cn == "utf8"
                    || cn == "utf16"
                    || cn.starts_with("buf")
                    || cn.starts_with("blob")
                    || cn.starts_with("Buf[")
                    || cn.starts_with("Blob[")
            } =>
            {
                if let Some(Value::Array(items, ..)) = attributes.get("bytes") {
                    Some(
                        items
                            .iter()
                            .map(|v| match v {
                                Value::Int(i) => *i as u8,
                                _ => 0,
                            })
                            .collect(),
                    )
                } else {
                    Some(Vec::new())
                }
            }
            Value::Array(elems, ..) => Some(
                elems
                    .iter()
                    .map(|v| match v {
                        Value::Int(i) => *i as u8,
                        _ => 0,
                    })
                    .collect(),
            ),
            _ => None,
        }
    }

    /// Socket recv: read N chars (or all available) in text or binary mode
    fn socket_recv(
        &mut self,
        handle_id: usize,
        max_chars: Option<usize>,
        bin_mode: bool,
    ) -> Result<Value, RuntimeError> {
        use std::io::Read;
        let state = self
            .handles
            .get_mut(&handle_id)
            .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
        let stream = state
            .socket
            .as_mut()
            .ok_or_else(|| RuntimeError::new("Socket not connected"))?;

        if bin_mode {
            let max = max_chars.unwrap_or(65536);
            let mut buf = vec![0u8; max];
            let n = stream
                .read(&mut buf)
                .map_err(|e| RuntimeError::new(format!("recv failed: {}", e)))?;
            buf.truncate(n);
            return Ok(Self::make_buf(buf));
        }

        // Text mode
        match max_chars {
            Some(n) => {
                // Read N characters (UTF-8 aware)
                let mut result = String::new();
                let mut byte_buf = [0u8; 4];
                let mut remaining = n;
                while remaining > 0 {
                    let bytes_read = stream
                        .read(&mut byte_buf[..1])
                        .map_err(|e| RuntimeError::new(format!("recv failed: {}", e)))?;
                    if bytes_read == 0 {
                        break;
                    }
                    // Determine UTF-8 char length from first byte
                    let first = byte_buf[0];
                    let char_len = if first < 0x80 {
                        1
                    } else if first < 0xE0 {
                        2
                    } else if first < 0xF0 {
                        3
                    } else {
                        4
                    };
                    // Read remaining bytes for this character
                    if char_len > 1 {
                        let more = stream
                            .read(&mut byte_buf[1..char_len])
                            .map_err(|e| RuntimeError::new(format!("recv failed: {}", e)))?;
                        if more < char_len - 1 {
                            break;
                        }
                    }
                    if let Ok(s) = std::str::from_utf8(&byte_buf[..char_len]) {
                        result.push_str(s);
                    }
                    remaining -= 1;
                }
                Ok(Value::str(result))
            }
            None => {
                // Read all available data
                let mut buf = vec![0u8; 65536];
                let n = stream
                    .read(&mut buf)
                    .map_err(|e| RuntimeError::new(format!("recv failed: {}", e)))?;
                buf.truncate(n);
                Ok(Value::str(String::from_utf8_lossy(&buf).to_string()))
            }
        }
    }

    /// Socket read: read N bytes, return Buf
    fn socket_read_bytes(
        &mut self,
        handle_id: usize,
        nbytes: usize,
    ) -> Result<Value, RuntimeError> {
        use std::io::Read;
        let state = self
            .handles
            .get_mut(&handle_id)
            .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
        let stream = state
            .socket
            .as_mut()
            .ok_or_else(|| RuntimeError::new("Socket not connected"))?;

        let mut buf = vec![0u8; nbytes];
        let mut total = 0;
        while total < nbytes {
            let n = stream
                .read(&mut buf[total..])
                .map_err(|e| RuntimeError::new(format!("read failed: {}", e)))?;
            if n == 0 {
                break;
            }
            total += n;
        }
        buf.truncate(total);
        Ok(Self::make_buf(buf))
    }

    /// Socket get: read one line using the handle's line separators
    fn socket_get_line(&mut self, handle_id: usize) -> Result<Value, RuntimeError> {
        use std::io::Read;
        let separators = {
            let state = self
                .handles
                .get(&handle_id)
                .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
            state.line_separators.clone()
        };

        let state = self
            .handles
            .get_mut(&handle_id)
            .ok_or_else(|| RuntimeError::new("Invalid socket handle"))?;
        let stream = state
            .socket
            .as_mut()
            .ok_or_else(|| RuntimeError::new("Socket not connected"))?;

        let mut line_bytes = Vec::new();
        let mut byte_buf = [0u8; 1];
        loop {
            let n = stream.read(&mut byte_buf).unwrap_or(0);
            if n == 0 {
                break;
            }
            line_bytes.push(byte_buf[0]);

            // Check if line ends with any separator
            for sep in &separators {
                if line_bytes.len() >= sep.len() && line_bytes.ends_with(sep) {
                    // Remove separator
                    line_bytes.truncate(line_bytes.len() - sep.len());
                    return Ok(Value::str(String::from_utf8_lossy(&line_bytes).to_string()));
                }
            }
        }
        if line_bytes.is_empty() {
            Ok(Value::Nil)
        } else {
            Ok(Value::str(String::from_utf8_lossy(&line_bytes).to_string()))
        }
    }

    /// Socket lines: read all remaining lines, return as a Seq/Array
    fn socket_lines(&mut self, handle_id: usize) -> Result<Value, RuntimeError> {
        let mut lines = Vec::new();
        loop {
            let line = self.socket_get_line(handle_id)?;
            if matches!(line, Value::Nil) {
                break;
            }
            lines.push(line);
        }
        Ok(Value::Seq(Arc::new(lines)))
    }

    fn native_encoding_builtin(attributes: &HashMap<String, Value>, method: &str) -> Value {
        match method {
            "name" => attributes
                .get("name")
                .cloned()
                .unwrap_or(Value::str(String::new())),
            "alternative-names" => attributes
                .get("alternative-names")
                .cloned()
                .unwrap_or_else(|| Value::array(Vec::new())),
            "encoder" => {
                // Return a stub encoder object that supports encode-chars
                let enc_name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let mut attrs = HashMap::new();
                attrs.insert("encoding".to_string(), Value::str(enc_name));
                Value::make_instance(Symbol::intern("Encoding::Encoder"), attrs)
            }
            "decoder" => {
                let enc_name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let mut attrs = HashMap::new();
                attrs.insert("encoding".to_string(), Value::str(enc_name));
                Value::make_instance(Symbol::intern("Encoding::Decoder"), attrs)
            }
            "gist" | "Str" => {
                let name = attributes
                    .get("name")
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(format!("Encoding::Builtin<{}>", name))
            }
            "WHAT" => Value::Package(Symbol::intern("Encoding::Builtin")),
            _ => Value::Nil,
        }
    }

    fn native_encoding_encoder(
        _attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Value {
        match method {
            "encode-chars" => {
                // Stub: encode the string as UTF-8 bytes and return a Buf
                let input = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                let bytes: Vec<Value> = input.bytes().map(|b| Value::Int(b as i64)).collect();
                Value::array(bytes)
            }
            "WHAT" => Value::Package(Symbol::intern("Encoding::Encoder")),
            _ => Value::Nil,
        }
    }

    fn native_encoding_decoder(
        _attributes: &HashMap<String, Value>,
        method: &str,
        args: &[Value],
    ) -> Value {
        match method {
            "decode-chars" => {
                let input = args
                    .first()
                    .map(|v| v.to_string_value())
                    .unwrap_or_default();
                Value::str(input)
            }
            "WHAT" => Value::Package(Symbol::intern("Encoding::Decoder")),
            _ => Value::Nil,
        }
    }

    /// Background event loop for Supply.act on live supplies (e.g., signal).
    /// Receives events from the channel and calls the callback.
    /// If the callback calls `exit`, terminates the entire process.
    pub(super) fn run_supply_act_loop(
        interp: &mut Interpreter,
        rx: &std::sync::mpsc::Receiver<SupplyEvent>,
        cb: &Value,
        delay_seconds: f64,
    ) {
        use std::io::Write;
        while let Ok(SupplyEvent::Emit(value)) = rx.recv() {
            Self::sleep_for_supply_delay(delay_seconds);
            let result = interp.call_sub_value(cb.clone(), vec![value], true);
            // Flush stdout
            if !interp.output.is_empty() {
                print!("{}", interp.output);
                let _ = std::io::stdout().flush();
                interp.output.clear();
            }
            // Flush stderr
            if !interp.stderr_output.is_empty() {
                eprint!("{}", interp.stderr_output);
                let _ = std::io::stderr().flush();
                interp.stderr_output.clear();
            }
            // If the callback called exit, terminate the process
            if interp.halted {
                std::process::exit(interp.exit_code as i32);
            }
            // If the callback threw an unhandled exception, terminate
            if let Err(err) = result {
                eprintln!(
                    "Unhandled exception in code scheduled on thread\n{}",
                    err.message
                );
                let _ = std::io::stderr().flush();
                std::process::exit(1);
            }
        }
    }
}
