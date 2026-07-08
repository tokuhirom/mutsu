use crate::runtime::*;
use crate::value::ValueView;
use std::sync::OnceLock;

use super::state::take_complete_lines_from_buffer;

#[derive(Clone)]
struct UniqueFilterState {
    as_fn: Option<Value>,
    with_fn: Option<Value>,
    expires_seconds: Option<f64>,
    seen: Vec<(Value, std::time::Instant)>,
}

#[derive(Clone)]
struct ClassifyState {
    mapper: Value,
    classify_supplier_id: u64,
    seen_keys: Vec<Value>,
    key_supplier_ids: Vec<(Value, u64)>,
    /// `true` for `.categorize` (mapper returns a list of keys; a value may go
    /// to several buckets), `false` for `.classify` (single key per value).
    categorize: bool,
}

#[derive(Clone)]
struct ElemsTraceState {
    interval_seconds: f64,
    last_emit_at: Option<std::time::Instant>,
    emitted_count: i64,
    last_reported_count: i64,
}

#[derive(Clone)]
struct SupplierTapSubscription {
    callback: Value,
    line_mode: bool,
    line_chomp: bool,
    line_buffer: String,
    delay_seconds: f64,
    unique_filter: Option<UniqueFilterState>,
    classify_state: Option<ClassifyState>,
    elems_trace: Option<ElemsTraceState>,
    /// Optional limit on how many values to emit (from .head(N))
    head_limit: Option<usize>,
    /// Count of values emitted so far (used with head_limit)
    head_count: usize,
    /// Produce (scan/fold) state: callable and running accumulator
    produce_state: Option<ProduceState>,
    /// Start transform state: callable and output supplier_id
    start_state: Option<StartState>,
    /// Batch state: buffer values and emit as batched lists
    batch_state: Option<BatchState>,
    /// Words mode: split incoming text into words across chunk boundaries
    words_mode: bool,
    /// Buffer for partial words across chunk boundaries
    words_buffer: String,
    /// Flat transform: re-emit flattened sub-elements to this downstream supplier
    flat_downstream: Option<u64>,
    /// Channel sink: when set, emitted values are pushed directly into this
    /// SharedChannel instead of being delivered to a callback. Used by
    /// `Supply.Channel` to bridge a live supplier into a Channel.
    channel_sink: Option<crate::value::SharedChannel>,
    /// Zip tap state: buffer values for multi-supply zip
    zip_tap: Option<ZipTapState>,
    /// Zip-latest tap state
    zip_latest_tap: Option<ZipLatestTapState>,
    /// Migrate tap state: this tap is the outer migrate subscription on the
    /// master supply-of-supplies; each emitted value must itself be a Supply.
    migrate_state: Option<MigrateState>,
    /// Forward tap: emitted values are re-emitted verbatim to this downstream
    /// supplier (used by `migrate` to pipe the currently-active inner supply
    /// into the migrate output supplier).
    forward_downstream: Option<u64>,
    /// Transform tap: each emitted value is passed through a `grep`/`map`
    /// callable and the (filtered or mapped) result is re-emitted to a
    /// downstream supplier. Used by `Supply.grep`/`Supply.map` on a live
    /// (Supplier-backed) supply so the transform stays live rather than
    /// snapshotting the source.
    transform_state: Option<TransformState>,
    /// Stable identifier so taps can be closed individually.
    tap_id: u64,
    /// When set, this tap is closed and should no longer receive emits.
    closed: bool,
}

#[derive(Clone)]
struct TransformState {
    /// The `grep`/`map` callable applied to each emitted value.
    callable: Value,
    /// `true` for `grep` (filter: forward the original value when the callable
    /// is truthy), `false` for `map` (forward the callable's return value).
    is_grep: bool,
    /// The downstream supplier that receives the forwarded/transformed values.
    downstream_supplier_id: u64,
}

#[derive(Clone)]
struct ZipTapState {
    zip_state_id: u64,
    source_index: usize,
}

#[derive(Clone)]
struct ZipLatestTapState {
    zip_latest_state_id: u64,
    source_index: usize,
}

#[derive(Clone)]
struct MigrateState {
    /// The migrate output supplier that downstream taps subscribe to.
    downstream_supplier_id: u64,
    /// The inner supply currently being forwarded, and the tap id of the
    /// forward subscription registered on it (so a switch can close it).
    current_inner_supplier_id: Option<u64>,
    current_inner_tap_id: Option<u64>,
}

#[derive(Clone)]
struct ProduceState {
    callable: Value,
    accumulator: Option<Value>,
}

#[derive(Clone)]
struct StartState {
    /// The user's block to run for each emitted value
    callable: Value,
    /// The output supplier_id where wrapped Supply values are emitted
    output_supplier_id: u64,
}

#[derive(Clone)]
struct BatchState {
    /// Maximum number of elements per batch (from :elems)
    elems: Option<usize>,
    /// Time window in seconds (from :seconds)
    seconds: Option<f64>,
    /// Buffer of values accumulated so far
    buffer: Vec<Value>,
    /// The downstream supplier_id to emit batched lists into
    downstream_supplier_id: u64,
    /// Absolute wall-clock period (`time div :seconds`, rakudo semantics) of
    /// the most recent emit. A value arriving in a *different* period flushes
    /// the pending buffer first. Anchoring to absolute periods (not "elapsed
    /// since last flush / tap registration") matters: the old
    /// registration-anchored `Instant` fired a spurious 1-element time-flush
    /// on the second emit whenever the tap was registered just after a period
    /// boundary (S17-supply/batch.t "batch by time and elems" flake —
    /// deterministic repro in that shape, raku emits 7,3,7,3, mutsu emitted
    /// 1,7,2,7,3).
    last_period: i64,
}

/// Absolute wall-clock period index for `batch(:seconds)`: `floor(epoch /
/// seconds)`, matching rakudo's `time div $seconds` bucketing for integer
/// seconds.
fn batch_time_period(seconds: f64) -> i64 {
    let epoch = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs_f64())
        .unwrap_or(0.0);
    if seconds > 0.0 {
        (epoch / seconds).floor() as i64
    } else {
        0
    }
}

#[derive(Clone, Default)]
struct SupplierSubscriptions {
    taps: Vec<SupplierTapSubscription>,
    done_callbacks: Vec<Value>,
    quit_callbacks: Vec<Value>,
    /// QUIT phaser bodies from `whenever` blocks. Unlike `quit_callbacks`
    /// (the downstream tap's `quit =>` handler), these run *first* when the
    /// source quits: if a QUIT phaser handles the exception the supply
    /// completes with `done` and the downstream quit is suppressed.
    whenever_quit_callbacks: Vec<Value>,
    /// CLOSE phaser bodies from a `supply { ... }` block. They run once when
    /// the tap is closed or the supply terminates normally — whichever
    /// happens first (taking them empties the list, giving run-once).
    close_callbacks: Vec<Value>,
}

type SupplierSubscriptionsMap = std::sync::Mutex<HashMap<u64, SupplierSubscriptions>>;

fn supplier_subscriptions_map() -> &'static SupplierSubscriptionsMap {
    static MAP: OnceLock<SupplierSubscriptionsMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

// ---------------------------------------------------------------------------
// Supply-block `whenever` serialization
//
// Raku guarantees "you can only be in one `whenever` block at a time" for a
// given `supply {}` / `react {}` instance: while one `whenever` handler runs
// (even while it is blocked inside `await`), no sibling `whenever` of the same
// block may run. mutsu delivers a live `whenever` callback synchronously on the
// thread that emitted into the source Supplier (e.g. `$trigger.emit(...)`
// running inside a `start { }`), so two sibling `whenever`s fed by two
// different triggers on two worker threads would otherwise run concurrently and
// interleave their emits (roast S17-supply/syntax.t test 53).
//
// We serialize them with a per-supply-block lock. Every `whenever` registered
// during one on-demand supply-block body shares the block's emitter
// `supplier_id` as its "serialize group". A side map records, for each source
// trigger `supplier_id`, which group its `whenever` tap belongs to. At emit
// time the group lock is held across the callback invocation. The lock is
// re-entrant per thread (a `whenever` body that synchronously re-triggers a
// same-group source on its own thread must not self-deadlock) and blocks across
// threads via a condvar.
type SupplierSerializeGroupsMap = std::sync::Mutex<HashMap<u64, u64>>;

fn supplier_serialize_groups() -> &'static SupplierSerializeGroupsMap {
    static MAP: OnceLock<SupplierSerializeGroupsMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Record that `whenever` taps on `trigger_supplier_id` belong to the supply
/// block identified by `group` (the block's emitter supplier id).
pub(in crate::runtime) fn set_supplier_serialize_group(trigger_supplier_id: u64, group: u64) {
    if let Ok(mut map) = supplier_serialize_groups().lock() {
        map.insert(trigger_supplier_id, group);
    }
}

/// Look up the serialize group for `whenever` taps on `trigger_supplier_id`.
pub(in crate::runtime) fn supplier_serialize_group(trigger_supplier_id: u64) -> Option<u64> {
    supplier_serialize_groups()
        .lock()
        .ok()
        .and_then(|map| map.get(&trigger_supplier_id).copied())
}

struct GroupLock {
    state: std::sync::Mutex<GroupLockState>,
    cv: std::sync::Condvar,
}

struct GroupLockState {
    owner: Option<std::thread::ThreadId>,
    depth: u32,
}

fn group_locks() -> &'static std::sync::Mutex<HashMap<u64, std::sync::Arc<GroupLock>>> {
    static MAP: OnceLock<std::sync::Mutex<HashMap<u64, std::sync::Arc<GroupLock>>>> =
        OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// RAII guard for a held supply-block serialize group. Dropping it releases the
/// group (decrements the re-entrancy depth; when it reaches zero, clears the
/// owner and wakes a waiting thread).
pub(in crate::runtime) struct SupplySerializeGuard {
    lock: std::sync::Arc<GroupLock>,
}

impl Drop for SupplySerializeGuard {
    fn drop(&mut self) {
        if let Ok(mut st) = self.lock.state.lock() {
            st.depth = st.depth.saturating_sub(1);
            if st.depth == 0 {
                st.owner = None;
                self.lock.cv.notify_one();
            }
        }
    }
}

/// Acquire the serialize group `group`, blocking the current thread until no
/// other thread holds it. Re-entrant on the same thread.
pub(in crate::runtime) fn acquire_supply_serialize(group: u64) -> SupplySerializeGuard {
    let lock = {
        let mut map = group_locks().lock().unwrap();
        map.entry(group)
            .or_insert_with(|| {
                std::sync::Arc::new(GroupLock {
                    state: std::sync::Mutex::new(GroupLockState {
                        owner: None,
                        depth: 0,
                    }),
                    cv: std::sync::Condvar::new(),
                })
            })
            .clone()
    };
    let me = std::thread::current().id();
    let mut st = lock.state.lock().unwrap();
    loop {
        match st.owner {
            None => {
                st.owner = Some(me);
                st.depth = 1;
                break;
            }
            Some(owner) if owner == me => {
                st.depth += 1;
                break;
            }
            Some(_) => {
                st = lock.cv.wait(st).unwrap();
            }
        }
    }
    drop(st);
    SupplySerializeGuard { lock }
}

/// Drop the serialize-group registration for a torn-down trigger supplier so
/// the side map does not grow without bound across many supply blocks.
pub(in crate::runtime) fn clear_supplier_serialize_group(trigger_supplier_id: u64) {
    if let Ok(mut map) = supplier_serialize_groups().lock() {
        map.remove(&trigger_supplier_id);
    }
}

/// Monotonic count of `Supplier.done` invocations. Used to detect whether a
/// QUIT phaser called `done` (which completes the supply via the emitter):
/// snapshot before running the phaser, compare after.
static SUPPLIER_DONE_CALLS: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);

pub(in crate::runtime) fn bump_supplier_done_count() {
    SUPPLIER_DONE_CALLS.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
}

pub(in crate::runtime) fn supplier_done_count() -> u64 {
    SUPPLIER_DONE_CALLS.load(std::sync::atomic::Ordering::Relaxed)
}

fn next_tap_id() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

/// Close the tap with the given id on the given supplier so it no longer
/// receives emitted values.
pub(in crate::runtime) fn close_supplier_tap(supplier_id: u64, tap_id: u64) {
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
    {
        for tap in subs.taps.iter_mut() {
            if tap.tap_id == tap_id {
                tap.closed = true;
            }
        }
    }
}

/// Register the outer `migrate` tap on the master supply-of-supplies. Each
/// value it receives must itself be a Supply; the interpreter switches the
/// forwarded inner supply via `migrate_switch_inner`.
pub(in crate::runtime) fn register_supplier_migrate_tap(master_sid: u64, downstream_sid: u64) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(master_sid)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: Value::NIL,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds: 0.0,
                unique_filter: None,
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: Some(MigrateState {
                    downstream_supplier_id: downstream_sid,
                    current_inner_supplier_id: None,
                    current_inner_tap_id: None,
                }),
                forward_downstream: None,
                transform_state: None,
            });
    }
}

/// Register a verbatim-forward tap on an inner supply that re-emits each value
/// to the given downstream supplier. Returns the new tap id.
fn register_supplier_forward_tap(inner_sid: u64, downstream_sid: u64) -> u64 {
    let tap_id = next_tap_id();
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(inner_sid)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: Value::NIL,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds: 0.0,
                unique_filter: None,
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id,
                closed: false,
                migrate_state: None,
                forward_downstream: Some(downstream_sid),
                transform_state: None,
            });
    }
    tap_id
}

/// Switch the migrate tap to a new inner supply: close the forward tap on the
/// previously-active inner supply (if any), register a fresh forward tap on the
/// new inner supply, and remember it on the migrate state.
pub(in crate::runtime) fn migrate_switch_inner(
    master_sid: u64,
    tap_index: usize,
    new_inner_sid: u64,
    downstream_sid: u64,
) {
    let old = {
        let mut result = None;
        if let Ok(mut map) = supplier_subscriptions_map().lock()
            && let Some(subs) = map.get_mut(&master_sid)
            && let Some(tap) = subs.taps.get_mut(tap_index)
            && let Some(ref ms) = tap.migrate_state
            && let (Some(sid), Some(tid)) = (ms.current_inner_supplier_id, ms.current_inner_tap_id)
        {
            result = Some((sid, tid));
        }
        result
    };
    if let Some((sid, tid)) = old {
        close_supplier_tap(sid, tid);
    }
    let new_tap_id = register_supplier_forward_tap(new_inner_sid, downstream_sid);
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&master_sid)
        && let Some(tap) = subs.taps.get_mut(tap_index)
        && let Some(ref mut ms) = tap.migrate_state
    {
        ms.current_inner_supplier_id = Some(new_inner_sid);
        ms.current_inner_tap_id = Some(new_tap_id);
    }
}

/// Close all taps on the given supplier so they stop receiving emits.
pub(in crate::runtime) fn close_all_supplier_taps(supplier_id: u64) {
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
    {
        for tap in subs.taps.iter_mut() {
            tap.closed = true;
        }
        subs.done_callbacks.clear();
        subs.quit_callbacks.clear();
        subs.whenever_quit_callbacks.clear();
    }
    // The trigger's `whenever` taps are gone; drop its serialize-group mapping
    // so the side map does not grow across many supply blocks.
    clear_supplier_serialize_group(supplier_id);
}

/// Return the id of the most recently registered tap on the given supplier,
/// if any. Used by `tap` to build a Tap handle that can be closed.
pub(in crate::runtime) fn last_supplier_tap_id(supplier_id: u64) -> Option<u64> {
    if let Ok(map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get(&supplier_id)
    {
        subs.taps.last().map(|t| t.tap_id)
    } else {
        None
    }
}

/// Register a channel sink tap on a supplier. Each emitted value is pushed
/// into the channel; on done/quit the channel is closed (or failed).
pub(in crate::runtime) fn register_supplier_channel_tap(
    supplier_id: u64,
    channel: crate::value::SharedChannel,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: Value::NIL,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds: 0.0,
                unique_filter: None,
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: Some(channel),
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

/// Close (or fail) all channel-sink taps registered on the given supplier.
/// If `failure` is Some, the channel is failed with that value, otherwise
/// it is closed cleanly.
pub(in crate::runtime) fn close_supplier_channel_taps(supplier_id: u64, failure: Option<Value>) {
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
    {
        for tap in subs.taps.iter_mut() {
            if let Some(ch) = tap.channel_sink.take() {
                if let Some(ref err) = failure {
                    ch.fail(err.clone());
                } else {
                    ch.close();
                }
                tap.closed = true;
            }
        }
    }
}

pub(in crate::runtime) fn register_supplier_tap(supplier_id: u64, tap: Value, delay_seconds: f64) {
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
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

pub(in crate::runtime) fn register_supplier_tap_with_head_limit(
    supplier_id: u64,
    tap: Value,
    delay_seconds: f64,
    limit: usize,
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
                unique_filter: None,
                classify_state: None,
                elems_trace: None,
                head_limit: Some(limit),
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

pub(in crate::runtime) fn register_supplier_lines_tap(
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
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

pub(in crate::runtime) fn register_supplier_words_tap(
    supplier_id: u64,
    tap: Value,
    delay_seconds: f64,
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
                unique_filter: None,
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: true,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

pub(in crate::runtime) fn register_supplier_elems_tap(
    supplier_id: u64,
    tap: Value,
    delay_seconds: f64,
    interval_seconds: f64,
    initial_count: i64,
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
                unique_filter: None,
                classify_state: None,
                elems_trace: Some(ElemsTraceState {
                    interval_seconds: interval_seconds.max(0.0),
                    last_emit_at: None,
                    emitted_count: initial_count.max(0),
                    last_reported_count: initial_count.max(0),
                }),
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

/// Result of checking a supplier emit callback.
/// Normal: callback, value, delay
/// UniqueFiltered: callback, value, delay, as_fn, with_fn (need interpreter to check)
pub(in crate::runtime) enum SupplierEmitAction {
    Call(Value, Value, f64),
    UniqueCheck {
        callback: Value,
        value: Value,
        delay_seconds: f64,
        as_fn: Option<Value>,
        with_fn: Option<Value>,
        tap_index: usize,
    },
    ClassifyCheck {
        value: Value,
        tap_index: usize,
    },
    /// Signal that a head-limited tap has reached its limit; fire done callbacks.
    HeadLimitReached {
        supplier_id: u64,
    },
    /// Produce (scan) transform: needs interpreter to call the callable
    ProduceCall {
        callback: Value,
        callable: Value,
        value: Value,
        accumulator: Option<Value>,
        delay_seconds: f64,
        tap_index: usize,
    },
    /// Start transform: run callable, wrap result in Supply, emit to output supplier
    StartCall {
        callable: Value,
        value: Value,
        output_supplier_id: u64,
    },
    /// Batch emit: a full batch is ready to be emitted to the downstream supplier
    BatchEmit {
        downstream_supplier_id: u64,
        batch: Vec<Value>,
    },
    /// Flat emit: emit each flattened sub-element individually to the downstream supplier
    FlatEmit {
        downstream_supplier_id: u64,
        items: Vec<Value>,
    },
    /// Zip buffer: a value needs to be buffered for zip
    ZipBuffer {
        zip_state_id: u64,
        source_index: usize,
        value: Value,
    },
    /// Zip-latest buffer: update latest value for zip-latest
    ZipLatestBuffer {
        zip_latest_state_id: u64,
        source_index: usize,
        value: Value,
    },
    /// Migrate: the master supply emitted a value; it must be a Supply, and the
    /// migrate tap should switch its forwarded inner supply to it (or throw
    /// X::Supply::Migrate::Needs if it is not a Supply).
    Migrate {
        value: Value,
        master_supplier_id: u64,
        downstream_supplier_id: u64,
        tap_index: usize,
    },
    /// Forward: re-emit a value verbatim to the given downstream supplier.
    ForwardEmit {
        downstream_supplier_id: u64,
        value: Value,
    },
    /// Transform: run the `grep`/`map` callable on the value, then forward the
    /// (filtered or mapped) result to the downstream supplier. Needs the
    /// interpreter to invoke the callable.
    TransformCall {
        downstream_supplier_id: u64,
        callable: Value,
        is_grep: bool,
        value: Value,
    },
}

pub(in crate::runtime) fn supplier_emit_callbacks(
    supplier_id: u64,
    emitted_value: &Value,
) -> Vec<SupplierEmitAction> {
    let mut actions = Vec::new();
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
    {
        for (idx, tap) in subs.taps.iter_mut().enumerate() {
            if tap.closed {
                continue;
            }
            // Check head_limit: skip emissions once the limit is reached
            if let Some(limit) = tap.head_limit
                && tap.head_count >= limit
            {
                continue;
            }
            if let Some(ref ch) = tap.channel_sink {
                ch.send(emitted_value.clone());
                continue;
            }
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
            } else if tap.words_mode {
                tap.words_buffer.push_str(&emitted_value.to_string_value());
                // Extract complete words (followed by whitespace) from the buffer
                loop {
                    let trimmed = tap.words_buffer.trim_start();
                    if trimmed.is_empty() {
                        tap.words_buffer.clear();
                        break;
                    }
                    if let Some(ws_pos) = trimmed.find(char::is_whitespace) {
                        let word = trimmed[..ws_pos].to_string();
                        let consumed = tap.words_buffer.len() - trimmed.len() + ws_pos;
                        tap.words_buffer = tap.words_buffer[consumed..].to_string();
                        actions.push(SupplierEmitAction::Call(
                            tap.callback.clone(),
                            Value::str(word),
                            tap.delay_seconds,
                        ));
                    } else {
                        // No whitespace after word -- may be partial, keep in buffer
                        let leading_ws = tap.words_buffer.len() - trimmed.len();
                        tap.words_buffer = tap.words_buffer[leading_ws..].to_string();
                        break;
                    }
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
            } else if tap.classify_state.is_some() {
                actions.push(SupplierEmitAction::ClassifyCheck {
                    value: emitted_value.clone(),
                    tap_index: idx,
                });
            } else if let Some(ref mut elems) = tap.elems_trace {
                elems.emitted_count += 1;
                let now = std::time::Instant::now();
                let should_emit = if elems.interval_seconds <= 0.0 {
                    true
                } else if let Some(last_emit) = elems.last_emit_at {
                    now.duration_since(last_emit).as_secs_f64() >= elems.interval_seconds
                } else {
                    true
                };
                if should_emit && elems.emitted_count > elems.last_reported_count {
                    elems.last_emit_at = Some(now);
                    elems.last_reported_count = elems.emitted_count;
                    actions.push(SupplierEmitAction::Call(
                        tap.callback.clone(),
                        Value::int(elems.emitted_count),
                        tap.delay_seconds,
                    ));
                }
            } else if let Some(ref mut bs) = tap.batch_state {
                // A value arriving in a new absolute time period flushes the
                // previous period's pending buffer first (rakudo `time div
                // $seconds` semantics; see BatchState::last_period).
                if let Some(seconds) = bs.seconds {
                    let this_period = batch_time_period(seconds);
                    if this_period != bs.last_period {
                        if !bs.buffer.is_empty() {
                            let batch = std::mem::take(&mut bs.buffer);
                            actions.push(SupplierEmitAction::BatchEmit {
                                downstream_supplier_id: bs.downstream_supplier_id,
                                batch,
                            });
                        }
                        bs.last_period = this_period;
                    }
                }
                bs.buffer.push(emitted_value.clone());
                // Check if we should flush based on elems count
                if let Some(elems) = bs.elems
                    && bs.buffer.len() >= elems
                {
                    let batch = std::mem::take(&mut bs.buffer);
                    let dsid = bs.downstream_supplier_id;
                    actions.push(SupplierEmitAction::BatchEmit {
                        downstream_supplier_id: dsid,
                        batch,
                    });
                }
            } else if let Some(ref ps) = tap.produce_state {
                actions.push(SupplierEmitAction::ProduceCall {
                    callback: tap.callback.clone(),
                    callable: ps.callable.clone(),
                    value: emitted_value.clone(),
                    accumulator: ps.accumulator.clone(),
                    delay_seconds: tap.delay_seconds,
                    tap_index: idx,
                });
            } else if let Some(downstream_sid) = tap.flat_downstream {
                // Flatten the emitted value and emit each element individually
                // to the downstream supplier.
                let items: Vec<Value> = match emitted_value.view() {
                    ValueView::Array(arr, kind) if !kind.is_itemized() => {
                        arr.iter().cloned().collect()
                    }
                    ValueView::Slip(arr) | ValueView::Seq(arr) => arr.iter().cloned().collect(),
                    _ => vec![emitted_value.clone()],
                };
                actions.push(SupplierEmitAction::FlatEmit {
                    downstream_supplier_id: downstream_sid,
                    items,
                });
            } else if let Some(ref ss) = tap.start_state {
                actions.push(SupplierEmitAction::StartCall {
                    callable: ss.callable.clone(),
                    value: emitted_value.clone(),
                    output_supplier_id: ss.output_supplier_id,
                });
            } else if let Some(ref zt) = tap.zip_tap {
                actions.push(SupplierEmitAction::ZipBuffer {
                    zip_state_id: zt.zip_state_id,
                    source_index: zt.source_index,
                    value: emitted_value.clone(),
                });
            } else if let Some(ref zlt) = tap.zip_latest_tap {
                actions.push(SupplierEmitAction::ZipLatestBuffer {
                    zip_latest_state_id: zlt.zip_latest_state_id,
                    source_index: zlt.source_index,
                    value: emitted_value.clone(),
                });
            } else if let Some(ref ms) = tap.migrate_state {
                actions.push(SupplierEmitAction::Migrate {
                    value: emitted_value.clone(),
                    master_supplier_id: supplier_id,
                    downstream_supplier_id: ms.downstream_supplier_id,
                    tap_index: idx,
                });
            } else if let Some(downstream_sid) = tap.forward_downstream {
                actions.push(SupplierEmitAction::ForwardEmit {
                    downstream_supplier_id: downstream_sid,
                    value: emitted_value.clone(),
                });
            } else if let Some(ref ts) = tap.transform_state {
                actions.push(SupplierEmitAction::TransformCall {
                    downstream_supplier_id: ts.downstream_supplier_id,
                    callable: ts.callable.clone(),
                    is_grep: ts.is_grep,
                    value: emitted_value.clone(),
                });
            } else {
                actions.push(SupplierEmitAction::Call(
                    tap.callback.clone(),
                    emitted_value.clone(),
                    tap.delay_seconds,
                ));
            }
            // Track head_limit emissions
            if let Some(limit) = tap.head_limit {
                tap.head_count += 1;
                if tap.head_count >= limit {
                    actions.push(SupplierEmitAction::HeadLimitReached { supplier_id });
                }
            }
        }
    }
    actions
}

/// After the interpreter has evaluated :as/:with for a unique check, call this
/// to update the seen list if the value is unique.
pub(in crate::runtime) fn supplier_unique_mark_seen(
    supplier_id: u64,
    tap_index: usize,
    key: Value,
) {
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
pub(in crate::runtime) fn supplier_unique_get_seen(
    supplier_id: u64,
    tap_index: usize,
) -> Vec<Value> {
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

pub(in crate::runtime) fn register_supplier_unique_tap(
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
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

pub(in crate::runtime) fn register_supplier_produce_tap(
    supplier_id: u64,
    tap: Value,
    delay_seconds: f64,
    callable: Value,
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
                unique_filter: None,
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: Some(ProduceState {
                    callable,
                    accumulator: None,
                }),
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

/// Register a start-transform tap on a supplier.
/// When the source emits a value, the callable runs and the result is wrapped
/// in a single-value Supply and emitted to the output supplier.
pub(in crate::runtime) fn register_supplier_start_tap(
    supplier_id: u64,
    callable: Value,
    output_supplier_id: u64,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: Value::NIL,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds: 0.0,
                unique_filter: None,
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: Some(StartState {
                    callable,
                    output_supplier_id,
                }),
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

/// Get output supplier IDs from start-transform taps, for done propagation.
pub(in crate::runtime) fn get_start_output_supplier_ids(supplier_id: u64) -> Vec<u64> {
    let mut ids = Vec::new();
    if let Ok(map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get(&supplier_id)
    {
        for tap in &subs.taps {
            if let Some(ref ss) = tap.start_state {
                ids.push(ss.output_supplier_id);
            }
        }
    }
    ids
}

/// Update the accumulator for a produce tap after the interpreter has computed the new value.
pub(in crate::runtime) fn supplier_produce_update_acc(
    supplier_id: u64,
    tap_index: usize,
    new_acc: Value,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
        && let Some(tap) = subs.taps.get_mut(tap_index)
        && let Some(ref mut ps) = tap.produce_state
    {
        ps.accumulator = Some(new_acc);
    }
}

/// Returns the number of taps currently registered for a supplier.
pub(in crate::runtime) fn supplier_tap_count(supplier_id: u64) -> usize {
    if let Ok(map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get(&supplier_id)
    {
        subs.taps.len()
    } else {
        0
    }
}

pub(in crate::runtime) fn register_supplier_classify_tap(
    supplier_id: u64,
    mapper: Value,
    classify_supplier_id: u64,
    categorize: bool,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: Value::NIL,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds: 0.0,
                unique_filter: None,
                classify_state: Some(ClassifyState {
                    mapper,
                    classify_supplier_id,
                    seen_keys: Vec::new(),
                    key_supplier_ids: Vec::new(),
                    categorize,
                }),
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

/// Get the classify state for a tap. Returns
/// (mapper, classify_supplier_id, seen_keys, key_supplier_ids, categorize).
#[allow(clippy::type_complexity)]
pub(in crate::runtime) fn get_classify_state(
    supplier_id: u64,
    tap_index: usize,
) -> Option<(Value, u64, Vec<Value>, Vec<(Value, u64)>, bool)> {
    if let Ok(map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get(&supplier_id)
        && let Some(tap) = subs.taps.get(tap_index)
        && let Some(ref cs) = tap.classify_state
    {
        Some((
            cs.mapper.clone(),
            cs.classify_supplier_id,
            cs.seen_keys.clone(),
            cs.key_supplier_ids.clone(),
            cs.categorize,
        ))
    } else {
        None
    }
}

/// Update classify state after processing a value.
pub(in crate::runtime) fn update_classify_state(
    supplier_id: u64,
    tap_index: usize,
    seen_keys: Vec<Value>,
    key_supplier_ids: Vec<(Value, u64)>,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
        && let Some(tap) = subs.taps.get_mut(tap_index)
        && let Some(ref mut cs) = tap.classify_state
    {
        cs.seen_keys = seen_keys;
        cs.key_supplier_ids = key_supplier_ids;
    }
}

/// Get all sub-supplier IDs from classify taps on a given supplier.
/// Used to propagate done/quit to classify sub-suppliers.
pub(in crate::runtime) fn get_classify_sub_supplier_ids(supplier_id: u64) -> Vec<u64> {
    let mut ids = Vec::new();
    if let Ok(map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get(&supplier_id)
    {
        for tap in &subs.taps {
            if let Some(ref cs) = tap.classify_state {
                ids.push(cs.classify_supplier_id);
                for (_, sid) in &cs.key_supplier_ids {
                    ids.push(*sid);
                }
            }
        }
    }
    ids
}

pub(in crate::runtime) fn flush_supplier_line_taps(supplier_id: u64) -> Vec<(Value, Value)> {
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

pub(in crate::runtime) fn flush_supplier_words_taps(supplier_id: u64) -> Vec<(Value, Value)> {
    let mut callbacks = Vec::new();
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
    {
        for tap in &mut subs.taps {
            if tap.words_mode {
                let remaining = tap.words_buffer.trim();
                if !remaining.is_empty() {
                    callbacks.push((tap.callback.clone(), Value::str(remaining.to_string())));
                }
                tap.words_buffer.clear();
            }
        }
    }
    callbacks
}

pub(in crate::runtime) fn take_supplier_done_callbacks(supplier_id: u64) -> Vec<Value> {
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

pub(in crate::runtime) fn register_supplier_done_callback(supplier_id: u64, done_cb: Value) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .done_callbacks
            .push(done_cb);
    }
}

pub(in crate::runtime) fn take_supplier_quit_callbacks(supplier_id: u64) -> Vec<Value> {
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

pub(in crate::runtime) fn register_supplier_quit_callback(supplier_id: u64, quit_cb: Value) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .quit_callbacks
            .push(quit_cb);
    }
}

pub(in crate::runtime) fn take_supplier_whenever_quit_callbacks(supplier_id: u64) -> Vec<Value> {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        if let Some(subs) = map.get_mut(&supplier_id) {
            std::mem::take(&mut subs.whenever_quit_callbacks)
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    }
}

pub(in crate::runtime) fn register_supplier_whenever_quit_callback(
    supplier_id: u64,
    quit_cb: Value,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .whenever_quit_callbacks
            .push(quit_cb);
    }
}

pub(in crate::runtime) fn take_supplier_close_callbacks(supplier_id: u64) -> Vec<Value> {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        if let Some(subs) = map.get_mut(&supplier_id) {
            std::mem::take(&mut subs.close_callbacks)
        } else {
            Vec::new()
        }
    } else {
        Vec::new()
    }
}

pub(in crate::runtime) fn register_supplier_close_callback(supplier_id: u64, close_cb: Value) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .close_callbacks
            .push(close_cb);
    }
}

// --- Whenever done group ---
// Tracks a group of inner suppliers that must all complete before the
// supply block's done callback fires.

struct WheneverDoneGroup {
    remaining: std::sync::atomic::AtomicUsize,
    done_cb: Value,
}

type WheneverDoneGroupMap = std::sync::Mutex<HashMap<u64, std::sync::Arc<WheneverDoneGroup>>>;

fn whenever_done_group_map() -> &'static WheneverDoneGroupMap {
    static MAP: OnceLock<WheneverDoneGroupMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn next_whenever_done_group_id() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

/// Create a new whenever-done group with the given number of active
/// whenevers and a done callback. Returns the group ID.
pub(in crate::runtime) fn create_whenever_done_group(count: usize, done_cb: Value) -> u64 {
    let id = next_whenever_done_group_id();
    let group = std::sync::Arc::new(WheneverDoneGroup {
        remaining: std::sync::atomic::AtomicUsize::new(count),
        done_cb,
    });
    if let Ok(mut map) = whenever_done_group_map().lock() {
        map.insert(id, group);
    }
    id
}

/// Decrement the active count for a whenever-done group.
/// Returns Some(done_cb) if the count reached zero (all whenevers done).
pub(in crate::runtime) fn whenever_done_group_decrement(group_id: u64) -> Option<Value> {
    if let Ok(mut map) = whenever_done_group_map().lock()
        && let Some(group) = map.get(&group_id)
    {
        let prev = group
            .remaining
            .fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
        if prev <= 1 {
            // All whenevers done — remove the group and return the callback
            let group = map.remove(&group_id).unwrap();
            return Some(group.done_cb.clone());
        }
    }
    None
}

/// Register a batch tap on a supplier. Values are buffered and emitted as
/// batched lists to the downstream supplier when `:elems` count is reached.
pub(in crate::runtime) fn register_supplier_batch_tap(
    supplier_id: u64,
    downstream_supplier_id: u64,
    elems: Option<usize>,
    seconds: Option<f64>,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: Value::NIL,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds: 0.0,
                unique_filter: None,
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: Some(BatchState {
                    elems,
                    seconds,
                    buffer: Vec::new(),
                    downstream_supplier_id,
                    last_period: seconds.map(batch_time_period).unwrap_or(0),
                }),
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

/// Register a flat tap on a supplier. Incoming values are flattened
/// (arrays/lists expanded) and each element is re-emitted to the downstream
/// supplier so downstream taps see the individual elements.
pub(in crate::runtime) fn register_supplier_flat_tap(
    supplier_id: u64,
    downstream_supplier_id: u64,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: Value::NIL,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds: 0.0,
                unique_filter: None,
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: Some(downstream_supplier_id),
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

/// Register a `grep`/`map` transform tap on a live supplier: each emitted value
/// is passed through `callable` and the result forwarded to
/// `downstream_supplier_id` (for `grep`, the original value is forwarded when
/// the callable is truthy; for `map`, the callable's return value is forwarded).
pub(in crate::runtime) fn register_supplier_transform_tap(
    supplier_id: u64,
    downstream_supplier_id: u64,
    callable: Value,
    is_grep: bool,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: Value::NIL,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds: 0.0,
                unique_filter: None,
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: Some(TransformState {
                    callable,
                    is_grep,
                    downstream_supplier_id,
                }),
            });
    }
}

/// Get the downstream supplier ids of all `grep`/`map` transform taps on this
/// supplier, so a source `done` can be propagated to the derived supplies.
pub(in crate::runtime) fn get_transform_output_supplier_ids(supplier_id: u64) -> Vec<u64> {
    let mut result = Vec::new();
    if let Ok(map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get(&supplier_id)
    {
        for tap in &subs.taps {
            if let Some(ref ts) = tap.transform_state {
                result.push(ts.downstream_supplier_id);
            }
        }
    }
    result
}

/// Flush any remaining values in batch tap buffers when the supplier is done.
/// Returns (downstream_supplier_id, batched_values) pairs.
pub(in crate::runtime) fn flush_supplier_batch_taps(supplier_id: u64) -> Vec<(u64, Vec<Value>)> {
    let mut result = Vec::new();
    if let Ok(mut map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get_mut(&supplier_id)
    {
        for tap in subs.taps.iter_mut() {
            if let Some(ref mut bs) = tap.batch_state
                && !bs.buffer.is_empty()
            {
                let batch = std::mem::take(&mut bs.buffer);
                result.push((bs.downstream_supplier_id, batch));
            }
        }
    }
    result
}

/// Get zip state ids associated with a supplier's taps.
pub(in crate::runtime) fn get_supplier_zip_state_ids(supplier_id: u64) -> Vec<u64> {
    let mut result = Vec::new();
    if let Ok(map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get(&supplier_id)
    {
        for tap in &subs.taps {
            if let Some(ref zt) = tap.zip_tap {
                result.push(zt.zip_state_id);
            }
        }
    }
    result
}

pub(in crate::runtime) fn get_supplier_zip_latest_state_ids(supplier_id: u64) -> Vec<u64> {
    let mut result = Vec::new();
    if let Ok(map) = supplier_subscriptions_map().lock()
        && let Some(subs) = map.get(&supplier_id)
    {
        for tap in &subs.taps {
            if let Some(ref zlt) = tap.zip_latest_tap {
                result.push(zlt.zip_latest_state_id);
            }
        }
    }
    result
}

/// Register a zip tap on a supplier.
pub(in crate::runtime) fn register_supplier_zip_tap(
    supplier_id: u64,
    zip_state_id: u64,
    source_index: usize,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: Value::NIL,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds: 0.0,
                unique_filter: None,
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: Some(ZipTapState {
                    zip_state_id,
                    source_index,
                }),
                zip_latest_tap: None,
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

pub(in crate::runtime) fn register_supplier_zip_latest_tap(
    supplier_id: u64,
    zip_latest_state_id: u64,
    source_index: usize,
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: Value::NIL,
                line_mode: false,
                line_chomp: true,
                line_buffer: String::new(),
                delay_seconds: 0.0,
                unique_filter: None,
                classify_state: None,
                elems_trace: None,
                head_limit: None,
                head_count: 0,
                produce_state: None,
                start_state: None,
                batch_state: None,
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                zip_tap: None,
                zip_latest_tap: Some(ZipLatestTapState {
                    zip_latest_state_id,
                    source_index,
                }),
                tap_id: next_tap_id(),
                closed: false,
                migrate_state: None,
                forward_downstream: None,
                transform_state: None,
            });
    }
}

// ── Zip state ────────────────────────────────────────────────────────

#[derive(Clone)]
struct ZipState {
    buffers: Vec<Vec<Value>>,
    output_supplier_id: u64,
    done_count: usize,
    source_count: usize,
    with_fn: Option<Value>,
}

type ZipStateMap = std::sync::Mutex<HashMap<u64, ZipState>>;

fn zip_state_map() -> &'static ZipStateMap {
    static MAP: OnceLock<ZipStateMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn next_zip_state_id() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(in crate::runtime) fn register_zip_state(
    source_count: usize,
    output_supplier_id: u64,
    with_fn: Option<Value>,
) -> u64 {
    let id = next_zip_state_id();
    let state = ZipState {
        buffers: (0..source_count).map(|_| Vec::new()).collect(),
        output_supplier_id,
        done_count: 0,
        source_count,
        with_fn,
    };
    if let Ok(mut map) = zip_state_map().lock() {
        map.insert(id, state);
    }
    id
}

pub(in crate::runtime) enum ZipAction {
    Emit(Value),
    AllDone,
    #[allow(dead_code)]
    None,
}

pub(in crate::runtime) fn zip_buffer_value(
    zip_state_id: u64,
    source_index: usize,
    value: Value,
) -> ZipAction {
    if let Ok(mut map) = zip_state_map().lock()
        && let Some(state) = map.get_mut(&zip_state_id)
    {
        if source_index < state.buffers.len() {
            state.buffers[source_index].push(value);
        }
        if state.buffers.iter().all(|b| !b.is_empty()) {
            let mut tuple: Vec<Value> = Vec::with_capacity(state.source_count);
            for buf in state.buffers.iter_mut() {
                tuple.push(buf.remove(0));
            }
            return ZipAction::Emit(Value::array(tuple));
        }
    }
    ZipAction::None
}

pub(in crate::runtime) fn zip_source_done(zip_state_id: u64) -> (ZipAction, u64) {
    if let Ok(mut map) = zip_state_map().lock()
        && let Some(state) = map.get_mut(&zip_state_id)
    {
        state.done_count += 1;
        let output_id = state.output_supplier_id;
        if state.done_count >= state.source_count {
            return (ZipAction::AllDone, output_id);
        }
        return (ZipAction::None, output_id);
    }
    (ZipAction::None, 0)
}

pub(in crate::runtime) fn zip_state_info(zip_state_id: u64) -> (u64, Option<Value>) {
    if let Ok(map) = zip_state_map().lock()
        && let Some(state) = map.get(&zip_state_id)
    {
        return (state.output_supplier_id, state.with_fn.clone());
    }
    (0, None)
}

// ── Zip-latest state ─────────────────────────────────────────────────

#[derive(Clone)]
struct ZipLatestState {
    latest: Vec<Option<Value>>,
    output_supplier_id: u64,
    done_count: usize,
    source_count: usize,
    with_fn: Option<Value>,
}

type ZipLatestStateMap = std::sync::Mutex<HashMap<u64, ZipLatestState>>;

fn zip_latest_state_map() -> &'static ZipLatestStateMap {
    static MAP: OnceLock<ZipLatestStateMap> = OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn next_zip_latest_state_id() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(1);
    COUNTER.fetch_add(1, Ordering::Relaxed)
}

pub(in crate::runtime) fn register_zip_latest_state(
    source_count: usize,
    output_supplier_id: u64,
    with_fn: Option<Value>,
    initial: Option<Vec<Value>>,
) -> u64 {
    let id = next_zip_latest_state_id();
    let latest = if let Some(init_vals) = initial {
        init_vals.into_iter().map(Some).collect()
    } else {
        vec![None; source_count]
    };
    let state = ZipLatestState {
        latest,
        output_supplier_id,
        done_count: 0,
        source_count,
        with_fn,
    };
    if let Ok(mut map) = zip_latest_state_map().lock() {
        map.insert(id, state);
    }
    id
}

pub(in crate::runtime) fn zip_latest_buffer_value(
    zip_latest_state_id: u64,
    source_index: usize,
    value: Value,
) -> ZipAction {
    if let Ok(mut map) = zip_latest_state_map().lock()
        && let Some(state) = map.get_mut(&zip_latest_state_id)
    {
        if source_index < state.latest.len() {
            state.latest[source_index] = Some(value);
        }
        if state.latest.iter().all(|v| v.is_some()) {
            let tuple: Vec<Value> = state.latest.iter().map(|v| v.clone().unwrap()).collect();
            return ZipAction::Emit(Value::array(tuple));
        }
    }
    ZipAction::None
}

pub(in crate::runtime) fn zip_latest_source_done(zip_latest_state_id: u64) -> (ZipAction, u64) {
    if let Ok(mut map) = zip_latest_state_map().lock()
        && let Some(state) = map.get_mut(&zip_latest_state_id)
    {
        state.done_count += 1;
        let output_id = state.output_supplier_id;
        if state.done_count >= state.source_count {
            return (ZipAction::AllDone, output_id);
        }
        return (ZipAction::None, output_id);
    }
    (ZipAction::None, 0)
}

pub(in crate::runtime) fn zip_latest_state_info(zip_latest_state_id: u64) -> (u64, Option<Value>) {
    if let Ok(map) = zip_latest_state_map().lock()
        && let Some(state) = map.get(&zip_latest_state_id)
    {
        return (state.output_supplier_id, state.with_fn.clone());
    }
    (0, None)
}

// ── GC root enumeration ──────────────────────────────────────────────
//
// The subscription/whenever/zip registries above are the supply-side half of
// the process-global supply graph (design doc §3.4). They are GC **root
// containers**, not GC-managed nodes: `Interpreter::visit_roots` enumerates the
// `Value`s (tap callbacks, done/quit/close phasers, channel sinks, unique/
// classify/produce/start/batch/zip state) they keep reachable so a supply-held
// closure/`Channel` is never misjudged garbage. See `visit_supply_state_roots`
// in the sibling `state` module for the promise/emit half and the rationale for
// the blocking `.lock()`.

/// Visit every `Value` held live by one tap subscription's transform state.
fn visit_tap_roots(tap: &SupplierTapSubscription, visitor: &mut dyn crate::gc::RootVisitor) {
    visitor.visit_value(&tap.callback);
    if let Some(channel) = &tap.channel_sink {
        visitor.visit_value(&Value::channel(channel.clone()));
    }
    if let Some(uf) = &tap.unique_filter {
        if let Some(f) = &uf.as_fn {
            visitor.visit_value(f);
        }
        if let Some(f) = &uf.with_fn {
            visitor.visit_value(f);
        }
        for (v, _) in &uf.seen {
            visitor.visit_value(v);
        }
    }
    if let Some(cs) = &tap.classify_state {
        visitor.visit_value(&cs.mapper);
        for v in &cs.seen_keys {
            visitor.visit_value(v);
        }
        for (v, _) in &cs.key_supplier_ids {
            visitor.visit_value(v);
        }
    }
    if let Some(ps) = &tap.produce_state {
        visitor.visit_value(&ps.callable);
        if let Some(acc) = &ps.accumulator {
            visitor.visit_value(acc);
        }
    }
    if let Some(ss) = &tap.start_state {
        visitor.visit_value(&ss.callable);
    }
    if let Some(bs) = &tap.batch_state {
        for v in &bs.buffer {
            visitor.visit_value(v);
        }
    }
}

/// Enumerate every `Value` held live by the process-global supply subscription
/// registries defined in *this* module (design doc §3.4 / §11 step 7).
#[allow(dead_code)] // live once a collector consumes `visit_roots` (step 8)
pub(in crate::runtime) fn visit_supplier_subscription_roots(
    visitor: &mut dyn crate::gc::RootVisitor,
) {
    if let Ok(map) = supplier_subscriptions_map().lock() {
        for subs in map.values() {
            for tap in &subs.taps {
                visit_tap_roots(tap, visitor);
            }
            for v in &subs.done_callbacks {
                visitor.visit_value(v);
            }
            for v in &subs.quit_callbacks {
                visitor.visit_value(v);
            }
            for v in &subs.whenever_quit_callbacks {
                visitor.visit_value(v);
            }
            for v in &subs.close_callbacks {
                visitor.visit_value(v);
            }
        }
    }
    if let Ok(map) = whenever_done_group_map().lock() {
        for group in map.values() {
            visitor.visit_value(&group.done_cb);
        }
    }
    if let Ok(map) = zip_state_map().lock() {
        for state in map.values() {
            for buf in &state.buffers {
                for v in buf {
                    visitor.visit_value(v);
                }
            }
            if let Some(f) = &state.with_fn {
                visitor.visit_value(f);
            }
        }
    }
    if let Ok(map) = zip_latest_state_map().lock() {
        for state in map.values() {
            for v in state.latest.iter().flatten() {
                visitor.visit_value(v);
            }
            if let Some(f) = &state.with_fn {
                visitor.visit_value(f);
            }
        }
    }
}

#[cfg(test)]
mod gc_root_tests {
    use super::*;

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
    fn visit_supplier_subscription_roots_sees_tap_callback() {
        // A high, test-unique id avoids colliding with any other test that
        // shares this process-global registry (tests run in parallel).
        let supplier_id = 0xF00D_0002;
        let sentinel = "__gc_supplier_subscription_root_sentinel__";
        register_supplier_tap(supplier_id, Value::str(sentinel.to_string()), 0.0);

        let mut collector = Collector { seen: Vec::new() };
        visit_supplier_subscription_roots(&mut collector);

        assert!(
            collector.seen.iter().any(|s| s == sentinel),
            "visit_supplier_subscription_roots should enumerate the tap callback"
        );

        // Leave the registry as we found it (no entry for this id).
        if let Ok(mut map) = supplier_subscriptions_map().lock() {
            map.remove(&supplier_id);
        }
    }
}
