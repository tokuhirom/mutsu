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
}

#[derive(Clone)]
struct ProduceState {
    callable: Value,
    accumulator: Option<Value>,
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
            // Check head_limit: skip emissions once the limit is reached
            if let Some(limit) = tap.head_limit
                && tap.head_count >= limit
            {
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
            } else if let Some(ref ps) = tap.produce_state {
                actions.push(SupplierEmitAction::ProduceCall {
                    callback: tap.callback.clone(),
                    callable: ps.callable.clone(),
                    value: emitted_value.clone(),
                    accumulator: ps.accumulator.clone(),
                    delay_seconds: tap.delay_seconds,
                    tap_index: idx,
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
            });
    }
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
