use crate::runtime::*;
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
    /// Stable identifier so taps can be closed individually.
    tap_id: u64,
    /// When set, this tap is closed and should no longer receive emits.
    closed: bool,
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
    /// Timestamp of last flush (for timer-based batching)
    last_flush: std::time::Instant,
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
                callback: Value::Nil,
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
                tap_id: next_tap_id(),
                closed: false,
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
                tap_id: next_tap_id(),
                closed: false,
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
                tap_id: next_tap_id(),
                closed: false,
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
                tap_id: next_tap_id(),
                closed: false,
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
                tap_id: next_tap_id(),
                closed: false,
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
                tap_id: next_tap_id(),
                closed: false,
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
                        Value::Int(elems.emitted_count),
                        tap.delay_seconds,
                    ));
                }
            } else if let Some(ref mut bs) = tap.batch_state {
                // Check if we should flush the existing buffer based on time
                if let Some(seconds) = bs.seconds
                    && bs.last_flush.elapsed().as_secs_f64() >= seconds
                    && !bs.buffer.is_empty()
                {
                    let batch = std::mem::take(&mut bs.buffer);
                    let dsid = bs.downstream_supplier_id;
                    bs.last_flush = std::time::Instant::now();
                    actions.push(SupplierEmitAction::BatchEmit {
                        downstream_supplier_id: dsid,
                        batch,
                    });
                }
                bs.buffer.push(emitted_value.clone());
                // Check if we should flush based on elems count
                if let Some(elems) = bs.elems
                    && bs.buffer.len() >= elems
                {
                    let batch = std::mem::take(&mut bs.buffer);
                    let dsid = bs.downstream_supplier_id;
                    bs.last_flush = std::time::Instant::now();
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
                let items: Vec<Value> = match emitted_value {
                    Value::Array(arr, kind) if !kind.is_itemized() => arr.iter().cloned().collect(),
                    Value::Slip(arr) | Value::Seq(arr) => arr.iter().cloned().collect(),
                    other => vec![other.clone()],
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
                tap_id: next_tap_id(),
                closed: false,
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
                tap_id: next_tap_id(),
                closed: false,
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
                callback: Value::Nil,
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
                tap_id: next_tap_id(),
                closed: false,
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
) {
    if let Ok(mut map) = supplier_subscriptions_map().lock() {
        map.entry(supplier_id)
            .or_default()
            .taps
            .push(SupplierTapSubscription {
                callback: Value::Nil,
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
                tap_id: next_tap_id(),
                closed: false,
            });
    }
}

/// Get the classify state for a tap. Returns (mapper, classify_supplier_id, seen_keys, key_supplier_ids).
#[allow(clippy::type_complexity)]
pub(in crate::runtime) fn get_classify_state(
    supplier_id: u64,
    tap_index: usize,
) -> Option<(Value, u64, Vec<Value>, Vec<(Value, u64)>)> {
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
                callback: Value::Nil,
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
                    last_flush: std::time::Instant::now(),
                }),
                words_mode: false,
                words_buffer: String::new(),
                flat_downstream: None,
                channel_sink: None,
                tap_id: next_tap_id(),
                closed: false,
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
                callback: Value::Nil,
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
                tap_id: next_tap_id(),
                closed: false,
            });
    }
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
