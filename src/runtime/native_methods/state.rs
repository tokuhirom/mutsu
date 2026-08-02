use crate::runtime::*;
use crate::value::AttrMap;
use std::net::TcpStream;
use std::process::ChildStdin;
use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};

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

type SupplyCollectedBytesMap = std::sync::Mutex<HashMap<u64, Vec<u8>>>;

fn supply_collected_bytes_map() -> &'static SupplyCollectedBytesMap {
    static MAP: OnceLock<SupplyCollectedBytesMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

type SupplyQuitTapsMap = std::sync::Mutex<HashMap<u64, Vec<Value>>>;

fn supply_quit_taps_map() -> &'static SupplyQuitTapsMap {
    static MAP: OnceLock<SupplyQuitTapsMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

type SupplyEncMap = std::sync::Mutex<HashMap<u64, String>>;

fn supply_enc_map() -> &'static SupplyEncMap {
    static MAP: OnceLock<SupplyEncMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Which combinator produced a composite promise. `Promise.allof` settles once
/// every source has settled, `Promise.anyof` on the first one — the react driver
/// may only block on the whole source list for `allof`, while deferred
/// `Proc::Async` tap replay applies to both.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum PromiseCombinator {
    Allof,
    Anyof,
}

type PromiseCombinatorMap =
    std::sync::Mutex<HashMap<usize, (PromiseCombinator, Vec<SharedPromise>)>>;

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

/// UDP bound socket state
#[derive(Debug, Clone)]
pub(crate) struct UdpBoundSocketState {
    pub(crate) host: String,
    pub(crate) port: u16,
    pub(crate) closed: bool,
    pub(crate) supply_ids: Vec<u64>,
}

type UdpBoundSocketMap = std::sync::Mutex<HashMap<u64, UdpBoundSocketState>>;
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

fn udp_bound_socket_map() -> &'static UdpBoundSocketMap {
    static MAP: OnceLock<UdpBoundSocketMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Supply channel registry: supply_id -> Receiver for streaming data from
/// Proc::Async stdout/stderr reader threads.
type SupplyChannelMap = std::sync::Mutex<HashMap<u64, super::supply_channel::SupplyReceiver>>;

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
pub(crate) fn take_supply_channel(supply_id: u64) -> Option<super::supply_channel::SupplyReceiver> {
    if let Ok(mut map) = supply_channel_map().lock() {
        map.remove(&supply_id)
    } else {
        None
    }
}

/// Public access to the supply channel map for signal registration
pub(in crate::runtime) fn supply_channel_map_pub() -> &'static SupplyChannelMap {
    supply_channel_map()
}

/// Global monotonic emit sequence, stamped on every `supplier_emit` /
/// `supplier_done` / `supplier_quit`. Buffered values across *different*
/// suppliers (e.g. two `whenever $s.grep(...)` derived supplies) carry
/// comparable sequence numbers, so a batch sink registration can replay them
/// merged in true emit order rather than one whole supplier's buffer at a time
/// (see `supplier_sinks_register_batch`).
fn next_emit_seq() -> u64 {
    static EMIT_SEQ: AtomicU64 = AtomicU64::new(1);
    EMIT_SEQ.fetch_add(1, Ordering::Relaxed)
}

fn next_sink_id() -> u64 {
    static SINK_IDS: AtomicU64 = AtomicU64::new(1);
    SINK_IDS.fetch_add(1, Ordering::Relaxed)
}

#[derive(Debug, Default)]
struct SupplierRuntimeState {
    emitted: Vec<Value>,
    /// Global emit sequence for each entry in `emitted` (parallel vector), used
    /// to merge buffered values across suppliers in true emit order at sink
    /// registration.
    emitted_seq: Vec<u64>,
    done: bool,
    quit_reason: Option<Value>,
    /// Global emit sequence of the terminal (done/quit) event, so a replay can
    /// order it relative to buffered emits of sibling suppliers.
    terminal_seq: Option<u64>,
    pending_promises: Vec<SharedPromise>,
    /// Watermark into `emitted` for `Supplier::Preserving`: values below it
    /// were already delivered to a tap (live dispatch or backlog replay);
    /// values at/above it are the buffered backlog the next tap must replay.
    preserved_consumed: usize,
    /// Push sinks registered by consuming drive loops (react / `await
    /// $supply` / control waits). Every emit/done/quit is pushed to each
    /// registered sink under this registry's lock, so a later
    /// `supplier_reset` cannot un-publish an event — the old snapshot-polling
    /// scheme both busy-spun consumers and lost events when `Supplier.done`
    /// reset the state before the consumer's next poll.
    sinks: Vec<SupplierSink>,
}

#[derive(Debug)]
struct SupplierSink {
    sink_id: u64,
    /// The consumer-side subscription index this sink feeds.
    key: usize,
    waker: crate::value::waker::ReactWaker,
}

type SupplierStateMap = std::sync::Mutex<HashMap<u64, SupplierRuntimeState>>;

fn supplier_state_map() -> &'static SupplierStateMap {
    static MAP: OnceLock<SupplierStateMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

pub(in crate::runtime) fn supplier_id_from_attrs(attributes: &AttrMap) -> Option<u64> {
    match attributes.get("supplier_id").and_then(|v| v.as_int()) {
        Some(id) if id > 0 => Some(id as u64),
        _ => None,
    }
}

/// Register a push sink on a supplier: replay everything a fresh polling
/// subscription would have observed (the buffered values, then a pending
/// done/quit), then subscribe the waker to all future emit/done/quit events.
/// Replay and subscription happen under one lock acquisition, so no event can
/// fall between them. Returns a sink id for `supplier_sink_unregister`.
pub(crate) fn supplier_sink_register(
    supplier_id: u64,
    key: usize,
    waker: &crate::value::waker::ReactWaker,
) -> u64 {
    let sink_id = next_sink_id();
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        for v in &state.emitted {
            waker.push(key, crate::value::waker::SinkEvent::Emit(v.clone()));
        }
        if let Some(reason) = &state.quit_reason {
            waker.push(key, crate::value::waker::SinkEvent::Quit(reason.clone()));
        } else if state.done {
            waker.push(key, crate::value::waker::SinkEvent::Done);
        }
        state.sinks.push(SupplierSink {
            sink_id,
            key,
            waker: waker.clone(),
        });
    }
    sink_id
}

/// Register push sinks on several suppliers at once, replaying their buffered
/// events **merged in global emit order** rather than one supplier's whole
/// buffer at a time.
///
/// A single [`supplier_sink_register`] replays exactly one supplier's buffer,
/// so registering N sibling derived supplies (e.g. two `whenever $s.grep(...)`)
/// one after another emits `p1,p2,p3` then `n1,n2,n3` — losing the interleaved
/// order the values were actually emitted in. That is only observable when a
/// producer (a `whenever start { emit … }` thread) races ahead of the react
/// drive loop's sink registration and buffers values before the sinks exist
/// (PLAN.md 8.19). By holding the registry lock across every subscribe, reading
/// each buffer with its per-value [`next_emit_seq`] stamp, then replaying the
/// combined set sorted by that stamp, sibling supplies interleave in true emit
/// order. Future live emits (pushed after this returns) are naturally later.
///
/// `regs` is `(supplier_id, consumer key)`; returns `(supplier_id, sink_id)`
/// pairs for [`supplier_sink_unregister`].
pub(crate) fn supplier_sinks_register_batch(
    regs: &[(u64, usize)],
    waker: &crate::value::waker::ReactWaker,
) -> Vec<(u64, u64)> {
    let mut sink_ids = Vec::with_capacity(regs.len());
    let mut replay: Vec<(u64, usize, crate::value::waker::SinkEvent)> = Vec::new();
    if let Ok(mut map) = supplier_state_map().lock() {
        for &(supplier_id, key) in regs {
            let sink_id = next_sink_id();
            let state = map.entry(supplier_id).or_default();
            for (i, v) in state.emitted.iter().enumerate() {
                let seq = state.emitted_seq.get(i).copied().unwrap_or(0);
                replay.push((seq, key, crate::value::waker::SinkEvent::Emit(v.clone())));
            }
            // A terminal event follows all of this supplier's own emits; give it
            // the recorded terminal sequence (or, defensively, one past the last
            // buffered emit) so it sorts after them but relative to siblings.
            if state.quit_reason.is_some() || state.done {
                let seq = state
                    .terminal_seq
                    .unwrap_or_else(|| state.emitted_seq.last().map(|s| s + 1).unwrap_or(0));
                let event = if let Some(reason) = &state.quit_reason {
                    crate::value::waker::SinkEvent::Quit(reason.clone())
                } else {
                    crate::value::waker::SinkEvent::Done
                };
                replay.push((seq, key, event));
            }
            state.sinks.push(SupplierSink {
                sink_id,
                key,
                waker: waker.clone(),
            });
            sink_ids.push((supplier_id, sink_id));
        }
        // Stable sort keeps same-sequence ties in registration order.
        replay.sort_by_key(|(seq, _, _)| *seq);
        for (_, key, event) in replay {
            waker.push(key, event);
        }
    }
    sink_ids
}

pub(crate) fn supplier_sink_unregister(supplier_id: u64, sink_id: u64) {
    if let Ok(mut map) = supplier_state_map().lock()
        && let Some(state) = map.get_mut(&supplier_id)
    {
        state.sinks.retain(|s| s.sink_id != sink_id);
    }
}

pub(crate) fn supplier_snapshot(supplier_id: u64) -> (Vec<Value>, bool, Option<Value>) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        (state.emitted.clone(), state.done, state.quit_reason.clone())
    } else {
        (Vec::new(), false, None)
    }
}

/// Enumerate every `Value` (and `Value`-wrapped async node) held live by the
/// process-global supply/promise registries defined in *this* module, for GC
/// root enumeration (`Interpreter::visit_roots`; design doc §3.4 / §11 step 7).
///
/// These registries are GC **root containers**, not GC-managed nodes: the
/// collector never frees them, it only needs to see the `Value`s / async nodes
/// they keep reachable so a supply-held `Promise`/`Channel`/callback closure is
/// not misjudged as garbage. `Value::promise` wrapping a `SharedPromise` (etc.)
/// is created transiently just to feed the visitor — it is not retained.
///
/// The blocking `.lock()` matches `visit_roots`' `shared_vars` read: a
/// root-enumeration pass runs at a re-entry boundary (design doc §1.2) where no
/// thread holds these locks, and skipping a contended lock would be *unsound*
/// (a live root could be missed), so we must not `try_lock`-and-skip.
#[allow(dead_code)] // live once a collector consumes `visit_roots` (step 8)
pub(in crate::runtime) fn visit_supply_state_roots(visitor: &mut dyn crate::gc::RootVisitor) {
    if let Ok(map) = supply_taps_map().lock() {
        for taps in map.values() {
            for v in taps {
                visitor.visit_value(v);
            }
        }
    }
    if let Ok(map) = promise_combinator_map().lock() {
        for (_, promises) in map.values() {
            for p in promises {
                visitor.visit_value(&Value::promise(p.clone()));
            }
        }
    }
    if let Ok(map) = supplier_state_map().lock() {
        for state in map.values() {
            for v in &state.emitted {
                visitor.visit_value(v);
            }
            if let Some(reason) = &state.quit_reason {
                visitor.visit_value(reason);
            }
            for p in &state.pending_promises {
                visitor.visit_value(&Value::promise(p.clone()));
            }
            for s in &state.sinks {
                s.waker.visit_roots(visitor);
            }
        }
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

pub(crate) fn supplier_register_promise(supplier_id: u64, promise: SharedPromise) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        if let Some(reason) = state.quit_reason.clone() {
            promise.break_with(reason, String::new(), String::new());
        } else if state.done {
            let result = state.emitted.last().cloned().unwrap_or(Value::NIL);
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
        for s in &state.sinks {
            s.waker
                .push(s.key, crate::value::waker::SinkEvent::Emit(value.clone()));
        }
        state.emitted.push(value);
        state.emitted_seq.push(next_emit_seq());
    }
}

/// `Supplier::Preserving`: take the buffered backlog (values emitted while no
/// tap was listening) and mark it consumed, so exactly one tap replays it.
pub(in crate::runtime) fn supplier_take_preserved_backlog(supplier_id: u64) -> Vec<Value> {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        let backlog = state.emitted[state.preserved_consumed.min(state.emitted.len())..].to_vec();
        state.preserved_consumed = state.emitted.len();
        backlog
    } else {
        Vec::new()
    }
}

/// `Supplier::Preserving`: a live tap just received the current emission, so
/// everything emitted so far no longer belongs to the buffered backlog.
pub(in crate::runtime) fn supplier_mark_preserved_consumed(supplier_id: u64) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        state.preserved_consumed = state.emitted.len();
    }
}

pub(in crate::runtime) fn supplier_done(supplier_id: u64) {
    if let Ok(mut map) = supplier_state_map().lock() {
        let state = map.entry(supplier_id).or_default();
        if state.done || state.quit_reason.is_some() {
            return;
        }
        state.done = true;
        state.terminal_seq = Some(next_emit_seq());
        for s in &state.sinks {
            s.waker.push(s.key, crate::value::waker::SinkEvent::Done);
        }
        let result = state.emitted.last().cloned().unwrap_or(Value::NIL);
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
        state.terminal_seq = Some(next_emit_seq());
        for s in &state.sinks {
            s.waker.push(s.key, crate::value::waker::SinkEvent::Done);
        }
        let result = state.emitted.last().cloned().unwrap_or(Value::NIL);
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
        state.terminal_seq = Some(next_emit_seq());
        for s in &state.sinks {
            s.waker
                .push(s.key, crate::value::waker::SinkEvent::Quit(reason.clone()));
        }
        let pending = std::mem::take(&mut state.pending_promises);
        for promise in pending {
            promise.break_with(reason.clone(), String::new(), String::new());
        }
    }
}

/// Reset the supplier state after done/quit so it can be reused.
pub(in crate::runtime) fn supplier_reset(supplier_id: u64) {
    if let Ok(mut map) = supplier_state_map().lock()
        && let Some(state) = map.get_mut(&supplier_id)
    {
        state.done = false;
        state.quit_reason = None;
        state.terminal_seq = None;
        state.emitted.clear();
        state.emitted_seq.clear();
    }
}

/// Reset supplier state but preserve quit_reason and emitted values
/// so react can observe them.
pub(in crate::runtime) fn supplier_reset_keep_quit(supplier_id: u64) {
    if let Ok(mut map) = supplier_state_map().lock()
        && let Some(state) = map.get_mut(&supplier_id)
    {
        state.done = false;
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

#[allow(dead_code)]
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

pub(crate) fn next_supplier_id() -> u64 {
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

/// Store the raw (undecoded) bytes read from a Proc::Async output stream, so the
/// `await`-time replay can decode them with the stream's effective encoding
/// (which may be `latin-1`/`utf-8` set on the constructor or per-`stdout`/`stderr`
/// tap). Keyed by the output Supply's `supply_id`.
pub(in crate::runtime) fn set_supply_collected_bytes(supply_id: u64, bytes: Vec<u8>) {
    if let Ok(mut map) = supply_collected_bytes_map().lock() {
        map.insert(supply_id, bytes);
    }
}

pub(in crate::runtime) fn take_supply_collected_bytes(supply_id: u64) -> Option<Vec<u8>> {
    supply_collected_bytes_map()
        .lock()
        .ok()
        .and_then(|mut map| map.remove(&supply_id))
}

/// Once-guard for the await/result-time Proc::Async tap replay. Returns `true`
/// only the first time it is called for a given output-supply id: `await
/// $p.start` followed by `.result` (or a second `await`) must not deliver the
/// same collected output to the registered taps again.
pub(in crate::runtime) fn mark_supply_replayed(supply_id: u64) -> bool {
    type ReplayedSet = std::sync::Mutex<std::collections::HashSet<u64>>;
    fn replayed_set() -> &'static ReplayedSet {
        static SET: OnceLock<ReplayedSet> = OnceLock::new();
        SET.get_or_init(|| std::sync::Mutex::new(std::collections::HashSet::new()))
    }
    replayed_set()
        .lock()
        .map(|mut set| set.insert(supply_id))
        .unwrap_or(false)
}

/// Register a `quit =>` handler on a Proc::Async output Supply. Unlike ordinary
/// value taps these fire only when the stream ends in an encoding error.
pub(in crate::runtime) fn register_supply_quit_tap(supply_id: u64, tap: Value) {
    if let Ok(mut map) = supply_quit_taps_map().lock() {
        map.entry(supply_id).or_default().push(tap);
    }
}

pub(in crate::runtime) fn get_supply_quit_taps(supply_id: u64) -> Vec<Value> {
    if let Ok(map) = supply_quit_taps_map().lock() {
        map.get(&supply_id).cloned().unwrap_or_default()
    } else {
        Vec::new()
    }
}

/// Record the effective decode encoding for a Proc::Async output Supply, as seen
/// at tap time (per-tap `:enc` overrides the constructor `:enc`).
pub(in crate::runtime) fn set_supply_enc(supply_id: u64, enc: String) {
    if let Ok(mut map) = supply_enc_map().lock() {
        map.insert(supply_id, enc);
    }
}

pub(in crate::runtime) fn get_supply_enc(supply_id: u64) -> Option<String> {
    supply_enc_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&supply_id).cloned())
}

pub(in crate::runtime) fn register_promise_combinator_sources(
    promise: &SharedPromise,
    kind: PromiseCombinator,
    sources: Vec<SharedPromise>,
) {
    if let Ok(mut map) = promise_combinator_map().lock() {
        map.insert(promise.id(), (kind, sources));
    }
}

pub(crate) fn take_promise_combinator_sources(
    promise: &SharedPromise,
) -> Option<(PromiseCombinator, Vec<SharedPromise>)> {
    if let Ok(mut map) = promise_combinator_map().lock() {
        map.remove(&promise.id())
    } else {
        None
    }
}

pub(in crate::runtime) fn register_udp_bound_socket(id: u64, state: UdpBoundSocketState) {
    if let Ok(mut map) = udp_bound_socket_map().lock() {
        map.insert(id, state);
    }
}

pub(in crate::runtime) fn get_udp_bound_socket(id: u64) -> Option<UdpBoundSocketState> {
    udp_bound_socket_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&id).cloned())
}

pub(in crate::runtime) fn update_udp_bound_socket<F>(id: u64, f: F)
where
    F: FnOnce(&mut UdpBoundSocketState),
{
    if let Ok(mut map) = udp_bound_socket_map().lock()
        && let Some(state) = map.get_mut(&id)
    {
        f(state);
    }
}

pub(in crate::runtime) fn udp_port_in_use(host: &str, port: u16) -> bool {
    if let Ok(map) = udp_bound_socket_map().lock() {
        return map.values().any(|s| {
            !s.closed
                && s.port == port
                && (s.host == host || s.host == "0.0.0.0" || host == "0.0.0.0")
        });
    }
    false
}

pub(in crate::runtime) fn lookup_udp_bound_socket(
    host: &str,
    port: u16,
) -> Option<(u64, UdpBoundSocketState)> {
    if let Ok(map) = udp_bound_socket_map().lock() {
        for (id, state) in map.iter() {
            if state.closed || state.port != port {
                continue;
            }
            if state.host == host
                || state.host == "0.0.0.0"
                || state.host == "::"
                || (host == "localhost" && state.host == "127.0.0.1")
            {
                return Some((*id, state.clone()));
            }
        }
    }
    None
}

/// Global map of conn-id -> TcpStream for real async TCP connections
type TcpStreamMap = std::sync::Mutex<HashMap<u64, Arc<std::sync::Mutex<TcpStream>>>>;

fn tcp_stream_map() -> &'static TcpStreamMap {
    static MAP: OnceLock<TcpStreamMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

pub(in crate::runtime) fn register_tcp_stream(conn_id: u64, stream: TcpStream) {
    if let Ok(mut map) = tcp_stream_map().lock() {
        map.insert(conn_id, std::sync::Arc::new(std::sync::Mutex::new(stream)));
    }
}

pub(in crate::runtime) fn get_tcp_stream(conn_id: u64) -> Option<Arc<std::sync::Mutex<TcpStream>>> {
    tcp_stream_map()
        .lock()
        .ok()
        .and_then(|map| map.get(&conn_id).cloned())
}

pub(in crate::runtime) fn remove_tcp_stream(conn_id: u64) {
    if let Ok(mut map) = tcp_stream_map().lock() {
        map.remove(&conn_id);
    }
}

/// Map of listener_id -> Arc<AtomicBool> for signaling listener threads to stop
type ListenerClosedMap = std::sync::Mutex<HashMap<u64, Arc<AtomicBool>>>;

fn listener_closed_map() -> &'static ListenerClosedMap {
    static MAP: OnceLock<ListenerClosedMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

pub(in crate::runtime) fn register_listener_closed_flag(listener_id: u64, flag: Arc<AtomicBool>) {
    if let Ok(mut map) = listener_closed_map().lock() {
        map.insert(listener_id, flag);
    }
}

#[allow(dead_code)]
pub(in crate::runtime) fn set_listener_closed(listener_id: u64) {
    if let Ok(map) = listener_closed_map().lock()
        && let Some(flag) = map.get(&listener_id)
    {
        flag.store(true, Ordering::SeqCst);
    }
}

#[cfg(test)]
mod gc_root_tests {
    use super::*;
    use crate::value::ValueView;

    struct Collector {
        seen: Vec<String>,
    }

    impl crate::gc::RootVisitor for Collector {
        fn visit_value(&mut self, value: &Value) {
            if let ValueView::Str(s) = value.view() {
                self.seen.push(s.to_string());
            }
        }
    }

    #[test]
    fn visit_supply_state_roots_sees_emitted_values() {
        // A high, test-unique id avoids colliding with any other test that
        // shares this process-global registry (tests run in parallel).
        let supplier_id = 0xF00D_0001;
        let sentinel = "__gc_supply_state_root_sentinel__";
        supplier_emit(supplier_id, Value::str(sentinel.to_string()));

        let mut collector = Collector { seen: Vec::new() };
        visit_supply_state_roots(&mut collector);

        assert!(
            collector.seen.iter().any(|s| s == sentinel),
            "visit_supply_state_roots should enumerate the emitted sentinel"
        );

        // Leave the registry as we found it (empty for this id).
        supplier_reset(supplier_id);
        if let Ok(mut map) = supplier_state_map().lock() {
            map.remove(&supplier_id);
        }
    }
}
