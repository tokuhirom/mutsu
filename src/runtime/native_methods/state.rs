use crate::runtime::*;
use std::process::ChildStdin;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::sync::mpsc;

type StdinMap = std::sync::Mutex<HashMap<u32, Arc<std::sync::Mutex<Option<ChildStdin>>>>>;

pub(in crate::runtime) fn proc_stdin_map() -> &'static StdinMap {
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

type PromiseCombinatorMap = std::sync::Mutex<HashMap<usize, Vec<SharedPromise>>>;

fn promise_combinator_map() -> &'static PromiseCombinatorMap {
    static MAP: OnceLock<PromiseCombinatorMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

pub(super) type CancellationMap = std::sync::Mutex<HashMap<u64, Arc<AtomicBool>>>;

pub(super) fn cancellation_map() -> &'static CancellationMap {
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

pub(in crate::runtime) fn supply_channel_map() -> &'static SupplyChannelMap {
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
pub(in crate::runtime) fn supply_channel_map_pub()
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

pub(in crate::runtime) fn supplier_id_from_attrs(
    attributes: &HashMap<String, Value>,
) -> Option<u64> {
    match attributes.get("supplier_id") {
        Some(Value::Int(id)) if *id > 0 => Some(*id as u64),
        _ => None,
    }
}

pub(in crate::runtime) fn supplier_snapshot(supplier_id: u64) -> (Vec<Value>, bool, Option<Value>) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        (state.emitted.clone(), state.done, state.quit_reason.clone())
    } else {
        (Vec::new(), false, None)
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

/// Split supply chunks into words, buffering across chunk boundaries.
pub(crate) fn split_supply_chunks_into_words(chunks: &[Value]) -> Vec<Value> {
    let mut words = Vec::new();
    let mut buffer = String::new();
    for chunk in chunks {
        buffer.push_str(&chunk.to_string_value());
        loop {
            let trimmed = buffer.trim_start();
            if trimmed.is_empty() {
                buffer.clear();
                break;
            }
            if let Some(ws_pos) = trimmed.find(char::is_whitespace) {
                let word = trimmed[..ws_pos].to_string();
                let consumed = buffer.len() - trimmed.len() + ws_pos;
                buffer = buffer[consumed..].to_string();
                words.push(Value::str(word));
            } else {
                let leading_ws = buffer.len() - trimmed.len();
                buffer = buffer[leading_ws..].to_string();
                break;
            }
        }
    }
    // Flush any remaining buffered word
    let remaining = buffer.trim();
    if !remaining.is_empty() {
        words.push(Value::str(remaining.to_string()));
    }
    words
}

pub(super) fn take_complete_lines_from_buffer(
    buffer: &mut String,
    chomp: bool,
    flush: bool,
) -> Vec<String> {
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

pub(in crate::runtime) fn supplier_register_promise(supplier_id: u64, promise: SharedPromise) {
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

pub(in crate::runtime) fn supplier_emit(supplier_id: u64, value: Value) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        if state.done || state.quit_reason.is_some() {
            return;
        }
        state.emitted.push(value);
    }
}

pub(in crate::runtime) fn supplier_done(supplier_id: u64) {
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

/// Mark the supplier as done but return pending promises WITHOUT resolving them.
/// This allows callers to fire done callbacks before resolving promises, avoiding
/// a race where `await` on the promise returns before done callbacks run.
pub(in crate::runtime) fn supplier_done_deferred(
    supplier_id: u64,
) -> Vec<(crate::value::SharedPromise, Value)> {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        if state.done || state.quit_reason.is_some() {
            return Vec::new();
        }
        state.done = true;
        let result = state.emitted.last().cloned().unwrap_or(Value::Nil);
        let pending = std::mem::take(&mut state.pending_promises);
        pending.into_iter().map(|p| (p, result.clone())).collect()
    } else {
        Vec::new()
    }
}

pub(in crate::runtime) fn supplier_quit(supplier_id: u64, reason: Value) {
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

pub(in crate::runtime) fn next_supply_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(in crate::runtime) fn next_async_socket_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(in crate::runtime) fn next_async_listener_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(in crate::runtime) fn allocate_async_listen_port() -> u16 {
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

pub(in crate::runtime) fn register_async_listener(
    listener_id: u64,
    state: AsyncSocketListenerState,
) {
    if let Ok(mut map) = async_socket_listener_map().lock() {
        map.insert(listener_id, state);
    }
}

pub(in crate::runtime) fn close_async_listener(listener_id: u64) {
    if let Ok(mut map) = async_socket_listener_map().lock()
        && let Some(listener) = map.get_mut(&listener_id)
    {
        listener.closed = true;
    }
}

pub(in crate::runtime) fn lookup_async_listener(
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

pub(in crate::runtime) fn async_port_in_use(host: &str, port: u16) -> bool {
    if let Ok(map) = async_socket_listener_map().lock() {
        return map.values().any(|listener| {
            !listener.closed
                && listener.port == port
                && (listener.host == host || listener.host == "0.0.0.0" || host == "0.0.0.0")
        });
    }
    false
}

pub(in crate::runtime) fn register_async_connection(conn_id: u64, state: AsyncSocketConnState) {
    if let Ok(mut map) = async_socket_conn_map().lock() {
        map.insert(conn_id, state);
    }
}

pub(in crate::runtime) fn get_async_connection(conn_id: u64) -> Option<AsyncSocketConnState> {
    async_socket_conn_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&conn_id).cloned())
}

pub(in crate::runtime) fn update_async_connection<F>(conn_id: u64, f: F)
where
    F: FnOnce(&mut AsyncSocketConnState),
{
    if let Ok(mut map) = async_socket_conn_map().lock()
        && let Some(state) = map.get_mut(&conn_id)
    {
        f(state);
    }
}

pub(super) fn take_deferred_accept_callback(conn_id: u64) -> Option<(Value, Value)> {
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

pub(in crate::runtime) fn register_async_supply(supply_id: u64, state: AsyncSocketSupplyState) {
    if let Ok(mut map) = async_socket_supply_map().lock() {
        map.insert(supply_id, state);
    }
}

pub(in crate::runtime) fn get_async_supply(supply_id: u64) -> Option<AsyncSocketSupplyState> {
    async_socket_supply_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&supply_id).cloned())
}

pub(in crate::runtime) fn update_async_supply<F>(supply_id: u64, f: F)
where
    F: FnOnce(&mut AsyncSocketSupplyState),
{
    if let Ok(mut map) = async_socket_supply_map().lock()
        && let Some(state) = map.get_mut(&supply_id)
    {
        f(state);
    }
}

pub(in crate::runtime) fn next_supplier_id() -> u64 {
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(in crate::runtime) fn register_supply_tap(supply_id: u64, tap: Value) {
    if let Ok(mut map) = supply_taps_map().lock() {
        map.entry(supply_id).or_default().push(tap);
    }
}

pub(in crate::runtime) fn get_supply_taps(supply_id: u64) -> Vec<Value> {
    if let Ok(map) = supply_taps_map().lock() {
        map.get(&supply_id).cloned().unwrap_or_default()
    } else {
        Vec::new()
    }
}

pub(in crate::runtime) fn set_supply_collected_output(supply_id: u64, output: String) {
    if let Ok(mut map) = supply_collected_map().lock() {
        map.insert(supply_id, output);
    }
}

pub(in crate::runtime) fn get_supply_collected_output(supply_id: u64) -> Option<String> {
    supply_collected_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&supply_id).cloned())
}

pub(in crate::runtime) fn register_promise_combinator_sources(
    promise: &SharedPromise,
    sources: Vec<SharedPromise>,
) {
    if let Ok(mut map) = promise_combinator_map().lock() {
        map.insert(promise.id(), sources);
    }
}

pub(in crate::runtime) fn take_promise_combinator_sources(
    promise: &SharedPromise,
) -> Option<Vec<SharedPromise>> {
    if let Ok(mut map) = promise_combinator_map().lock() {
        map.remove(&promise.id())
    } else {
        None
    }
}
