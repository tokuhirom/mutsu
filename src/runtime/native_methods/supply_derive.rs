//! `Supply.grep` / `Supply.map` over an on-demand source.
//!
//! Rakudo defines both as a new supply that taps its source:
//! `supply { whenever self -> \value { emit value if test.ACCEPTS(value) } }`.
//! The source's producer therefore runs once per tap of the *derived* supply,
//! and a value the producer emits long after the tap call returned (from a
//! `Supply.interval` tap, a `start` block, ...) still flows through the
//! transform.
//!
//! mutsu used to run the source's producer once, at `.grep`/`.map` time, and
//! turn whatever it emitted synchronously into a static snapshot. A producer
//! that only emits later (`Supply.on-demand(-> $p { Supply.interval(1).tap({
//! $p.emit(...) }) })`) produced an empty, already-finished supply, so a
//! `.grep(...).tap(...)` on it never saw a value (the Chronic `t/040-at.t`
//! hang, #9493).
//!
//! The derived supply is itself on-demand, with a native producer: an
//! `__SupplyDerive` shim that, when the derived supply is tapped, taps the
//! source with emit/done/quit forwarders bound to the derived supply's own
//! emitter. The forwarders apply the transform and re-emit, so the derived
//! supply follows the source exactly — synchronously for a finite source,
//! live for one that keeps emitting — and every consumer that already handles
//! an on-demand supply (tap, react `whenever`, `.list`, `.Promise`) handles it
//! with no special case.

use super::native_shim::native_method_shim;
use super::state_supplier::{TransformMode, register_supplier_close_callback};
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;
use crate::value::ValueView;

const CLASS: &str = "__SupplyDerive";

impl Interpreter {
    /// The derived on-demand Supply for `source.grep(callable)` /
    /// `source.map(callable)`, where `source` is an on-demand supply.
    // Cost: O(1).
    pub(in crate::runtime) fn make_on_demand_derived_supply(
        source: Value,
        mode: TransformMode,
        callable: Value,
    ) -> Value {
        Self::make_on_demand_derived_supply_named(source, mode_name(mode), callable)
    }

    /// The derived on-demand Supply for `source.head(count)`, where `source`
    /// is an on-demand supply: it taps the source per tap of its own, passes
    /// the first `count` values on, then signals done.
    // Cost: O(1).
    pub(in crate::runtime) fn make_on_demand_head_supply(source: Value, count: usize) -> Value {
        Self::make_on_demand_derived_supply_named(source, "head", Value::int(count as i64))
    }

    /// The derived on-demand Supply for `source.lines(:chomp)`, where
    /// `source` is an on-demand supply (`supply { emit $text }`): per tap of
    /// its own it taps the source, buffers the emitted chunks and passes on
    /// each complete line, flushing a trailing partial line when the source
    /// is done. It used to read the source's (empty) materialized values and
    /// produced nothing (TAP's `parse-stream` over a file's contents).
    // Cost: O(1).
    pub(in crate::runtime) fn make_on_demand_lines_supply(source: Value, chomp: bool) -> Value {
        Self::make_on_demand_derived_supply_named(source, "lines", Value::truth(chomp))
    }

    fn make_on_demand_derived_supply_named(source: Value, mode: &str, callable: Value) -> Value {
        let mut producer_attrs = HashMap::new();
        producer_attrs.insert("source".to_string(), source);
        producer_attrs.insert("mode".to_string(), Value::str(mode.to_string()));
        producer_attrs.insert("callable".to_string(), callable);
        let producer = native_method_shim(
            Value::make_instance(Symbol::intern(CLASS), producer_attrs),
            "__mutsu_derive_start",
            true,
        );
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::FALSE);
        attrs.insert("on_demand_callback".to_string(), producer);
        attrs.insert("derived_on_demand".to_string(), Value::TRUE);
        Value::make_instance(Symbol::intern("Supply"), attrs)
    }

    /// Whether an `on_demand_callback` is a `supply { }` block body (which the
    /// parser lowers to `Supply.on-demand(-> $__mutsu_supply_emitter_N { … })`)
    /// rather than an explicit `Supply.on-demand(&producer)` producer. The two
    /// complete differently: the block when its body and `whenever`s finish,
    /// the producer only when it calls `done`.
    // Cost: O(p), p = the callback's parameter count.
    pub(in crate::runtime) fn is_supply_block_producer(callback: &Value) -> bool {
        callback.as_sub().is_some_and(|data| {
            data.compiled_code
                .as_ref()
                .is_some_and(|cc| cc.is_supply_block_body)
                || data
                    .params
                    .iter()
                    .any(|p| p.starts_with(crate::parser::SUPPLY_EMITTER_PREFIX))
        })
    }

    /// `.Promise` of a derived on-demand supply: tap it and keep the promise
    /// with the last emitted value when it is done (break it on quit). The
    /// promise stays planned until then, like rakudo's.
    // Cost: O(1) plus the tap.
    pub(in crate::runtime) fn derived_supply_promise(
        &mut self,
        attributes: &AttrMap,
        promise: &crate::value::SharedPromise,
    ) -> Result<(), RuntimeError> {
        let mut fwd_attrs = HashMap::new();
        fwd_attrs.insert("promise".to_string(), Value::promise(promise.clone()));
        fwd_attrs.insert(
            "collect_id".to_string(),
            Value::int(next_collect_id() as i64),
        );
        let fwd = Value::make_instance(Symbol::intern(CLASS), fwd_attrs);
        let tap_args = vec![
            native_method_shim(fwd.clone(), "__mutsu_promise_emit", true),
            Value::pair(
                "done".to_string(),
                native_method_shim(fwd.clone(), "__mutsu_promise_done", false),
            ),
            Value::pair(
                "quit".to_string(),
                native_method_shim(fwd, "__mutsu_promise_quit", true),
            ),
        ];
        let attrs_map: ValueMap = attributes.into();
        let supply = Value::make_instance(Symbol::intern("Supply"), attrs_map);
        self.call_method_with_values(supply, "tap", tap_args)?;
        Ok(())
    }

    pub(in crate::runtime) fn native_supply_derive(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let arg = args.into_iter().next().unwrap_or(Value::NIL);
        match method {
            // Cost: O(1).
            "__mutsu_promise_emit" => {
                if let Some(id) = attributes.get("collect_id").and_then(|v| v.as_int())
                    && let Ok(mut map) = collected_last().lock()
                {
                    map.insert(id as u64, arg);
                }
                Ok(Value::NIL)
            }
            // Cost: O(1).
            "__mutsu_promise_done" => {
                let last = attributes
                    .get("collect_id")
                    .and_then(|v| v.as_int())
                    .and_then(|id| collected_last().lock().ok()?.remove(&(id as u64)))
                    .unwrap_or(Value::NIL);
                if let Some(ValueView::Promise(p)) = attributes.get("promise").map(Value::view) {
                    p.keep(last, String::new(), String::new());
                }
                Ok(Value::NIL)
            }
            // Cost: O(1).
            "__mutsu_promise_quit" => {
                if let Some(id) = attributes.get("collect_id").and_then(|v| v.as_int())
                    && let Ok(mut map) = collected_last().lock()
                {
                    map.remove(&(id as u64));
                }
                if let Some(ValueView::Promise(p)) = attributes.get("promise").map(Value::view) {
                    p.break_with(arg, String::new(), String::new());
                }
                Ok(Value::NIL)
            }
            // Cost: O(1) plus the source's own tap.
            "__mutsu_derive_start" => self.derive_start(attributes, arg),
            // Cost: O(1) plus one call of the transform callable.
            "__mutsu_derive_emit" => {
                let emitter = attributes.get("emitter").cloned().unwrap_or(Value::NIL);
                let callable = attributes.get("callable").cloned().unwrap_or(Value::NIL);
                if let Some(id) = lines_id(attributes) {
                    let chomp = attributes.get("callable").is_some_and(Value::truthy);
                    for line in lines_take(id, &arg.to_string_value(), chomp, false) {
                        self.call_method_with_values(emitter.clone(), "emit", vec![line])?;
                    }
                    return Ok(Value::NIL);
                }
                if let Some(id) = head_id(attributes) {
                    // `head`: pass the value on while the counter lasts, and
                    // finish the derived supply with the last one.
                    let Some(last) = head_take(id) else {
                        return Ok(Value::NIL);
                    };
                    self.call_method_with_values(emitter.clone(), "emit", vec![arg])?;
                    if last {
                        self.call_method_with_values(emitter, "done", vec![])?;
                    }
                    return Ok(Value::NIL);
                }
                let out = match attributes
                    .get("mode")
                    .map(Value::to_string_value)
                    .as_deref()
                {
                    Some("map") => Some(self.call_supply_callback(callable, vec![arg], true)?),
                    Some("do") => {
                        self.call_supply_callback(callable, vec![arg.clone()], true)?;
                        Some(arg)
                    }
                    _ => self.smart_match_values(&arg, &callable).then_some(arg),
                };
                if let Some(value) = out {
                    self.call_method_with_values(emitter, "emit", vec![value])?;
                }
                Ok(Value::NIL)
            }
            // Cost: O(1).
            "__mutsu_derive_done" => {
                let emitter = attributes.get("emitter").cloned().unwrap_or(Value::NIL);
                if let Some(id) = lines_id(attributes) {
                    // A trailing line with no terminator is still a line.
                    let chomp = attributes.get("callable").is_some_and(Value::truthy);
                    for line in lines_take(id, "", chomp, true) {
                        self.call_method_with_values(emitter.clone(), "emit", vec![line])?;
                    }
                }
                if let Some(id) = head_id(attributes)
                    && !head_finish(id)
                {
                    // The count already ran out and `done` was sent.
                    return Ok(Value::NIL);
                }
                self.call_method_with_values(emitter, "done", vec![])?;
                Ok(Value::NIL)
            }
            // Cost: O(1).
            "__mutsu_derive_quit" => {
                let emitter = attributes.get("emitter").cloned().unwrap_or(Value::NIL);
                if let Some(id) = lines_id(attributes) {
                    lines_take(id, "", false, true);
                }
                if let Some(id) = head_id(attributes)
                    && !head_finish(id)
                {
                    return Ok(Value::NIL);
                }
                self.call_method_with_values(emitter, "quit", vec![arg])?;
                Ok(Value::NIL)
            }
            // Cost: O(1) plus closing the source tap.
            "__mutsu_derive_close" => {
                if let Some(tap) = attributes.get("source_tap").cloned() {
                    self.call_method_with_values(tap, "close", vec![])?;
                }
                Ok(Value::NIL)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on '{}'",
                method, CLASS
            ))),
        }
    }

    /// The derived supply was tapped: tap the source with forwarders bound to
    /// `emitter` (the derived supply's own emitter), and make closing the
    /// derived tap close the source tap too.
    fn derive_start(
        &mut self,
        attributes: &AttrMap,
        emitter: Value,
    ) -> Result<Value, RuntimeError> {
        let source = attributes.get("source").cloned().unwrap_or(Value::NIL);
        let mut fwd_attrs = HashMap::new();
        for key in ["mode", "callable"] {
            if let Some(v) = attributes.get(key) {
                fwd_attrs.insert(key.to_string(), v.clone());
            }
        }
        fwd_attrs.insert("emitter".to_string(), emitter.clone());
        let is_head = attributes
            .get("mode")
            .is_some_and(|m| m.to_string_value() == "head");
        if is_head {
            let count = attributes
                .get("callable")
                .and_then(|c| c.as_int())
                .unwrap_or(0)
                .max(0) as usize;
            if count == 0 {
                self.call_method_with_values(emitter, "done", vec![])?;
                return Ok(Value::NIL);
            }
            fwd_attrs.insert(
                "head_id".to_string(),
                Value::int(head_register(count) as i64),
            );
        }
        if attributes
            .get("mode")
            .is_some_and(|m| m.to_string_value() == "lines")
        {
            fwd_attrs.insert("lines_id".to_string(), Value::int(lines_register() as i64));
        }
        let forwarder = Value::make_instance(Symbol::intern(CLASS), fwd_attrs);
        let tap_args = vec![
            native_method_shim(forwarder.clone(), "__mutsu_derive_emit", true),
            Value::pair(
                "done".to_string(),
                native_method_shim(forwarder.clone(), "__mutsu_derive_done", false),
            ),
            Value::pair(
                "quit".to_string(),
                native_method_shim(forwarder, "__mutsu_derive_quit", true),
            ),
        ];
        let source_tap = self.call_method_with_values(source, "tap", tap_args)?;
        if matches!(source_tap.view(), ValueView::Instance { .. })
            && let ValueView::Instance { attributes, .. } = emitter.view()
            && let Some(ValueView::Int(sid)) =
                attributes.as_map().get("supplier_id").map(Value::view)
        {
            let mut close_attrs = HashMap::new();
            close_attrs.insert("source_tap".to_string(), source_tap);
            let closer = native_method_shim(
                Value::make_instance(Symbol::intern(CLASS), close_attrs),
                "__mutsu_derive_close",
                false,
            );
            register_supplier_close_callback(sid as u64, closer);
        }
        Ok(Value::NIL)
    }
}

fn mode_name(mode: TransformMode) -> &'static str {
    match mode {
        TransformMode::Map => "map",
        TransformMode::Grep => "grep",
        TransformMode::Do => "do",
    }
}

/// Remaining counts of the live `head` taps, keyed by a per-tap id: the
/// forwarder instances are rebuilt per delivery, so the count cannot live in
/// their attributes.
fn head_counters() -> &'static std::sync::Mutex<HashMap<u64, usize>> {
    static MAP: std::sync::OnceLock<std::sync::Mutex<HashMap<u64, usize>>> =
        std::sync::OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn head_register(count: usize) -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT: AtomicU64 = AtomicU64::new(1);
    let id = NEXT.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut map) = head_counters().lock() {
        map.insert(id, count);
    }
    id
}

fn head_id(attributes: &AttrMap) -> Option<u64> {
    attributes
        .get("head_id")
        .and_then(|v| v.as_int())
        .map(|n| n as u64)
}

/// Take one slot of a `head` tap: `None` when its count already ran out,
/// otherwise whether this value is the last one.
fn head_take(id: u64) -> Option<bool> {
    let mut map = head_counters().lock().ok()?;
    let left = map.get_mut(&id)?;
    *left -= 1;
    if *left == 0 {
        map.remove(&id);
        Some(true)
    } else {
        Some(false)
    }
}

/// The source finished or quit: `true` when the derived supply must still be
/// finished (the count had not run out), and retires the counter.
fn head_finish(id: u64) -> bool {
    head_counters()
        .lock()
        .is_ok_and(|mut map| map.remove(&id).is_some())
}

/// The last value each `.Promise` tap has seen, keyed by a per-call id.
fn collected_last() -> &'static std::sync::Mutex<HashMap<u64, Value>> {
    static MAP: std::sync::OnceLock<std::sync::Mutex<HashMap<u64, Value>>> =
        std::sync::OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn next_collect_id() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT: AtomicU64 = AtomicU64::new(1);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

/// The partial-line buffers of the live `lines` taps, keyed by a per-tap id
/// (the forwarder instances are rebuilt per delivery, as for `head`).
fn line_buffers() -> &'static std::sync::Mutex<HashMap<u64, String>> {
    static MAP: std::sync::OnceLock<std::sync::Mutex<HashMap<u64, String>>> =
        std::sync::OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn lines_register() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT: AtomicU64 = AtomicU64::new(1);
    let id = NEXT.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut map) = line_buffers().lock() {
        map.insert(id, String::new());
    }
    id
}

fn lines_id(attributes: &AttrMap) -> Option<u64> {
    attributes
        .get("lines_id")
        .and_then(|v| v.as_int())
        .map(|n| n as u64)
}

/// Append `chunk` to the tap's buffer and take every complete line; with
/// `finish`, also the trailing partial line, and retire the buffer.
// Cost: O(c + b), c = chars of the chunk, b = chars buffered.
fn lines_take(id: u64, chunk: &str, chomp: bool, finish: bool) -> Vec<Value> {
    let Ok(mut map) = line_buffers().lock() else {
        return Vec::new();
    };
    let Some(buffer) = map.get_mut(&id) else {
        return Vec::new();
    };
    buffer.push_str(chunk);
    let lines = super::state::take_complete_lines_from_buffer(buffer, chomp, finish);
    if finish {
        map.remove(&id);
    }
    lines.into_iter().map(Value::str).collect()
}
