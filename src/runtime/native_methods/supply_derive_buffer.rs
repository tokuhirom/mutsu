//! The buffering stages `batch`, `tail`, `produce`, `elems` and `words`, and
//! the stateless `flat` and `start`, as per-tap recipes (ADR-18443 slices 2b
//! and 3).
//!
//! Rakudo defines each as `supply { whenever self { ... } }` with its state
//! (the pending batch, the last N values, the accumulator, the count, the
//! partial word) local to the supply block, so every tap of the derived
//! supply has its own. The derived supply rides the [`super::supply_derive`]
//! machinery (modes `batch` / `tail` / `produce` / `elems` / `words`); this
//! module holds the per-tap state and the parts of each stage that are not
//! a plain emit. The splitting and batching primitives here are also what the
//! eager supplier stages (`state_supplier`) use, so the two cannot drift.

use crate::runtime::*;
use crate::thread_compat::Instant;
use crate::value::{AttrMap, ValueView};

/// One tap's state.
enum BufferState {
    /// `batch(:elems, :seconds)`: the pending batch and the wall-clock
    /// period of the last value (see [`batch_time_period`]).
    Batch {
        elems: Option<usize>,
        seconds: Option<f64>,
        buffer: Vec<Value>,
        last_period: i64,
    },
    /// `tail(N)`: the last `count` values, emitted when the source is done.
    Tail {
        count: usize,
        buffer: std::collections::VecDeque<Value>,
    },
    /// `produce(&f)`: the running accumulator; `None` before the first value
    /// and while the callable runs.
    Produce { acc: Option<Value> },
    /// `elems($seconds?)`: the running count, reported at most once per
    /// `interval` seconds.
    Elems {
        count: i64,
        last_reported: i64,
        last_emit_at: Option<Instant>,
        interval: f64,
    },
    /// `words`: the text after the last complete word.
    Words { buffer: String },
    /// `flat`: no state.
    Flat,
    /// `start(&block)`: no state; each value starts the block.
    Start,
    /// `classify` / `categorize`: the groups live in
    /// [`super::supply_derive_classify`].
    Classify { categorize: bool },
    /// A combinator that needs every value before it emits anything
    /// (`sort`, `reverse`, ...): the values so far.
    Collect { buffer: Vec<Value> },
    /// `skip` / `min` / `max` / `minmax` / `rotor` / `encode` / `decode`
    /// ([`super::supply_derive_step`]); `None` while a value runs through it.
    PerValue(Option<Box<super::supply_derive_step::StepState>>),
}

fn states() -> &'static std::sync::Mutex<HashMap<u64, BufferState>> {
    static MAP: std::sync::OnceLock<std::sync::Mutex<HashMap<u64, BufferState>>> =
        std::sync::OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Start the state of a new tap of a `mode` stage whose arguments are
/// `params` (as [`Interpreter::make_on_demand_buffer_supply`] packed them).
/// `None` for a mode this module does not own.
// Cost: O(1).
pub(super) fn buffer_register(mode: &str, params: &Value) -> Option<u64> {
    let state = match mode {
        "batch" => {
            let (elems, seconds) = batch_params(params);
            BufferState::Batch {
                elems,
                seconds,
                buffer: Vec::new(),
                last_period: seconds.map(batch_time_period).unwrap_or(0),
            }
        }
        "tail" => BufferState::Tail {
            count: params.as_int().unwrap_or(0).max(0) as usize,
            buffer: std::collections::VecDeque::new(),
        },
        "produce" => BufferState::Produce { acc: None },
        "elems" => BufferState::Elems {
            count: 0,
            last_reported: 0,
            last_emit_at: None,
            interval: params.to_f64().max(0.0),
        },
        "words" => BufferState::Words {
            buffer: String::new(),
        },
        "flat" => BufferState::Flat,
        "start" => BufferState::Start,
        "collect" => BufferState::Collect { buffer: Vec::new() },
        other => match super::supply_derive_step::step_state(other, params) {
            Some(state) => BufferState::PerValue(Some(Box::new(state))),
            None => BufferState::Classify {
                categorize: super::supply_derive_classify::classify_mode(other)?,
            },
        },
    };
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT: AtomicU64 = AtomicU64::new(1);
    let id = NEXT.fetch_add(1, Ordering::Relaxed);
    if matches!(state, BufferState::Classify { .. }) {
        super::supply_derive_classify::classify_register(id);
    }
    if let Ok(mut map) = states().lock() {
        map.insert(id, state);
    }
    Some(id)
}

pub(super) fn buffer_id(attributes: &AttrMap) -> Option<u64> {
    attributes
        .get("buffer_id")
        .and_then(|v| v.as_int())
        .map(|n| n as u64)
}

/// The source finished: retire the tap's state and return what it still
/// holds back -- the partial batch, the tail, the trailing word.
// Cost: O(b), b = values held back.
pub(super) fn buffer_finish(id: u64) -> Vec<Value> {
    let Some(state) = states().lock().ok().and_then(|mut map| map.remove(&id)) else {
        return Vec::new();
    };
    match state {
        BufferState::Batch { buffer, .. } if !buffer.is_empty() => vec![Value::array(buffer)],
        BufferState::Tail { count, buffer } => {
            let mut values: Vec<Value> = buffer.into();
            // `tail(1)` of a source that emitted nothing still emits one
            // undefined value (rakudo's `$last` was never assigned).
            if count == 1 && values.is_empty() {
                values.push(Value::package(crate::symbol::Symbol::intern("Any")));
            }
            values
        }
        BufferState::Words { mut buffer } => take_complete_words_from_buffer(&mut buffer, true)
            .into_iter()
            .map(Value::str)
            .collect(),
        BufferState::PerValue(Some(state)) => super::supply_derive_step::step_finish(*state),
        _ => Vec::new(),
    }
}

/// The source quit or the tap closed: drop the tap's state.
// Cost: O(1).
pub(super) fn buffer_retire(id: u64) {
    if let Ok(mut map) = states().lock() {
        map.remove(&id);
    }
    super::supply_derive_classify::classify_take_groups(id);
}

/// Keep every value the GC must see: the values the live taps hold back.
// Cost: O(v), v = values held.
pub(in crate::runtime) fn visit_buffer_roots(visitor: &mut dyn crate::gc::RootVisitor) {
    if let Ok(map) = states().lock() {
        for state in map.values() {
            match state {
                BufferState::Batch { buffer, .. } => buffer.iter().for_each(|v| visitor.visit_value(v)),
                BufferState::Tail { buffer, .. } => buffer.iter().for_each(|v| visitor.visit_value(v)),
                BufferState::Collect { buffer } => buffer.iter().for_each(|v| visitor.visit_value(v)),
                BufferState::Produce { acc: Some(acc) } => visitor.visit_value(acc),
                BufferState::PerValue(Some(state)) => {
                    super::supply_derive_step::visit_step_roots(state, visitor)
                }
                _ => {}
            }
        }
    }
}

/// `batch`'s `(:elems, :seconds)` as a stage parameter.
// Cost: O(1).
pub(in crate::runtime) fn batch_stage_params(elems: Option<usize>, seconds: Option<f64>) -> Value {
    Value::array(vec![
        elems.map_or(Value::NIL, |n| Value::int(n as i64)),
        seconds.map_or(Value::NIL, Value::num),
    ])
}

fn batch_params(params: &Value) -> (Option<usize>, Option<f64>) {
    let ValueView::Array(items, ..) = params.view() else {
        return (None, None);
    };
    let elems = items.first().and_then(Value::as_int).map(|n| n.max(0) as usize);
    let seconds = items.get(1).filter(|v| !v.is_nil()).map(Value::to_f64);
    (elems, seconds)
}

/// Absolute wall-clock period index for `batch(:seconds)`: `floor(epoch /
/// seconds)`, matching rakudo's `time div $seconds` bucketing for integer
/// seconds. Anchoring to absolute periods (not "elapsed since the tap") is
/// what keeps a batch from flushing early when the tap was made just after a
/// period boundary.
// Cost: O(1).
pub(super) fn batch_time_period(seconds: f64) -> i64 {
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

/// Add `value` to a pending batch and take the batches that are complete: a
/// value in a new `:seconds` period flushes the previous period's values
/// first, and reaching `:elems` flushes the batch.
// Cost: O(1) amortized.
pub(super) fn batch_admit(
    buffer: &mut Vec<Value>,
    last_period: &mut i64,
    elems: Option<usize>,
    seconds: Option<f64>,
    value: Value,
) -> Vec<Vec<Value>> {
    let mut out = Vec::new();
    if let Some(seconds) = seconds {
        let this_period = batch_time_period(seconds);
        if this_period != *last_period {
            if !buffer.is_empty() {
                out.push(std::mem::take(buffer));
            }
            *last_period = this_period;
        }
    }
    buffer.push(value);
    if elems.is_some_and(|n| buffer.len() >= n) {
        out.push(std::mem::take(buffer));
    }
    out
}

/// Take every complete word (one followed by whitespace) from `buffer`,
/// leaving the partial word after it; with `finish`, the trailing word too.
// Cost: O(b), b = chars buffered.
pub(super) fn take_complete_words_from_buffer(buffer: &mut String, finish: bool) -> Vec<String> {
    let mut words = Vec::new();
    let mut rest = buffer.as_str();
    loop {
        rest = rest.trim_start();
        match rest.find(char::is_whitespace) {
            Some(end) => {
                words.push(rest[..end].to_string());
                rest = &rest[end..];
            }
            None => break,
        }
    }
    let rest = rest.to_string();
    if finish {
        if !rest.is_empty() {
            words.push(rest);
        }
        buffer.clear();
    } else {
        *buffer = rest;
    }
    words
}

/// The elements `flat` passes on for one value: a list's elements, or the
/// value itself.
// Cost: O(k), k = elements of the value.
pub(super) fn flat_items(value: &Value) -> Vec<Value> {
    match value.view() {
        ValueView::Array(items, kind) if !kind.is_itemized() => items.to_vec(),
        ValueView::Seq(items) => items.to_vec(),
        ValueView::Slip(items) => items.to_vec(),
        _ => vec![value.clone()],
    }
}

/// One `elems` tick: count the value, and answer the count to report when
/// the reporting interval allows one.
// Cost: O(1).
pub(super) fn elems_tick(
    count: &mut i64,
    last_reported: &mut i64,
    last_emit_at: &mut Option<Instant>,
    interval: f64,
) -> Option<i64> {
    *count += 1;
    let now = Instant::now();
    let due = interval <= 0.0
        || last_emit_at.is_none_or(|last| now.duration_since(last).as_secs_f64() >= interval);
    if due && *count > *last_reported {
        *last_emit_at = Some(now);
        *last_reported = *count;
        Some(*count)
    } else {
        None
    }
}

/// What one value does to a tap's state.
enum Step {
    Emit(Vec<Value>),
    /// `produce`: fold with the callable (outside the lock).
    Fold(Option<Value>, Value),
    /// `start`: run the block on the value in a thread of its own.
    Start(Value),
    /// `classify` / `categorize`: map the value to its group(s).
    Classify(bool, Value),
    /// A per-value combinator: run the state taken out of the map.
    PerValue(Option<Box<super::supply_derive_step::StepState>>, Value),
}

impl Interpreter {
    /// The derived on-demand Supply for one of the buffering stages over
    /// `source` (a live recipe or a plain live source): per tap of its own it
    /// taps the source with fresh state (ADR-18443).
    // Cost: O(1).
    pub(in crate::runtime) fn make_on_demand_buffer_supply(
        source: Value,
        mode: &str,
        params: Value,
    ) -> Value {
        Self::mark_live_recipe(Self::make_on_demand_derived_supply_named(source, mode, params))
    }

    /// The source of a `collect` tap `id` is done: run the combinator named
    /// in `params` (`[name, args...]`) over everything it emitted, as over a
    /// finite supply, and return what that emits.
    // Cost: the combinator's own over v values, v = values collected.
    pub(super) fn collect_finish(&mut self, id: u64, params: &Value) -> Result<Vec<Value>, RuntimeError> {
        let values = {
            let Ok(mut map) = states().lock() else {
                return Ok(Vec::new());
            };
            match map.get_mut(&id) {
                Some(BufferState::Collect { buffer }) => std::mem::take(buffer),
                _ => return Ok(Vec::new()),
            }
        };
        let ValueView::Array(params, ..) = params.view() else {
            return Ok(Vec::new());
        };
        let name = params.first().map(Value::to_string_value).unwrap_or_default();
        let args = params.iter().skip(1).cloned().collect();
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(values));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::FALSE);
        let finite = Value::make_instance(crate::symbol::Symbol::intern("Supply"), attrs);
        let result = self.call_method_with_values(finite, &name, args)?;
        let list = self.call_method_with_values(result, "list", Vec::new())?;
        Ok(Self::value_to_list(&list))
    }

    /// The `Supply` whose attributes are `attributes`, as a stage's source.
    // Cost: O(a), a = the attributes.
    pub(in crate::runtime) fn supply_from_attrs(attributes: &AttrMap) -> Value {
        let attrs: crate::value::ValueMap = attributes.into();
        Value::make_instance(crate::symbol::Symbol::intern("Supply"), attrs)
    }

    /// One value through a buffering stage's tap `id`; `emitter` is the
    /// derived supply's own emitter, `callable` the stage's parameter.
    /// `true` when the stage quit (its `produce` callable died), so the
    /// caller releases the source.
    // Cost: O(1) amortized plus one call of the `produce` callable.
    pub(super) fn buffer_stage_emit(
        &mut self,
        id: u64,
        callable: &Value,
        arg: Value,
        emitter: &Value,
    ) -> Result<bool, RuntimeError> {
        let step = {
            let Ok(mut map) = states().lock() else {
                return Ok(false);
            };
            let Some(state) = map.get_mut(&id) else {
                return Ok(false);
            };
            match state {
                BufferState::Batch {
                    elems,
                    seconds,
                    buffer,
                    last_period,
                } => Step::Emit(
                    batch_admit(buffer, last_period, *elems, *seconds, arg)
                        .into_iter()
                        .map(Value::array)
                        .collect(),
                ),
                BufferState::Tail { count, buffer } => {
                    if *count > 0 {
                        if buffer.len() >= *count {
                            buffer.pop_front();
                        }
                        buffer.push_back(arg);
                    }
                    Step::Emit(Vec::new())
                }
                BufferState::Produce { acc } => Step::Fold(acc.take(), arg),
                BufferState::Elems {
                    count,
                    last_reported,
                    last_emit_at,
                    interval,
                } => Step::Emit(
                    elems_tick(count, last_reported, last_emit_at, *interval)
                        .map(Value::int)
                        .into_iter()
                        .collect(),
                ),
                BufferState::Flat => Step::Emit(flat_items(&arg)),
                BufferState::Start => Step::Start(arg),
                BufferState::Classify { categorize } => Step::Classify(*categorize, arg),
                BufferState::PerValue(state) => Step::PerValue(state.take(), arg),
                BufferState::Collect { buffer } => {
                    buffer.push(arg);
                    Step::Emit(Vec::new())
                }
                BufferState::Words { buffer } => {
                    buffer.push_str(&arg.to_string_value());
                    Step::Emit(
                        take_complete_words_from_buffer(buffer, false)
                            .into_iter()
                            .map(Value::str)
                            .collect(),
                    )
                }
            }
        };
        let out = match step {
            Step::Emit(values) => values,
            Step::PerValue(None, _) => Vec::new(),
            Step::PerValue(Some(mut state), value) => {
                let out = self.step_emit(&mut state, value);
                if let Ok(mut map) = states().lock()
                    && let Some(BufferState::PerValue(slot)) = map.get_mut(&id)
                {
                    *slot = Some(state);
                }
                match out {
                    Ok(out) => out,
                    Err(err) => return self.buffer_stage_quit(id, err, emitter),
                }
            }
            Step::Classify(categorize, value) => {
                return self.classify_stage_emit(id, categorize, callable, value, emitter);
            }
            Step::Start(value) => {
                // The Supply of the block's result goes out on the stage's
                // own supplier, as the eager `start` stage does.
                if let ValueView::Instance { attributes, .. } = emitter.view()
                    && let Some(sid) = attributes.as_map().get("supplier_id").and_then(Value::as_int)
                {
                    self.run_start_call_in_thread(callable.clone(), value, sid as u64);
                }
                Vec::new()
            }
            Step::Fold(acc, arg) => {
                let next = match acc {
                    Some(acc) => {
                        match self.call_supply_callback(callable.clone(), vec![acc, arg]) {
                            Ok(next) => next,
                            // A dying callable quits the stage, as `map`'s
                            // does (#18473).
                            Err(err) => return self.buffer_stage_quit(id, err, emitter),
                        }
                    }
                    None => arg,
                };
                if let Ok(mut map) = states().lock()
                    && let Some(BufferState::Produce { acc }) = map.get_mut(&id)
                {
                    *acc = Some(next.clone());
                }
                vec![next]
            }
        };
        for value in out {
            self.call_method_with_values(emitter.clone(), "emit", vec![value])?;
        }
        Ok(false)
    }

    /// A stage's callable (or its own check) died: quit the stage, as
    /// `map`'s does (#18473), and report it stopped.
    // Cost: O(1) plus the downstream quit.
    fn buffer_stage_quit(&mut self, id: u64, err: RuntimeError, emitter: &Value) -> Result<bool, RuntimeError> {
        buffer_retire(id);
        let reason = Self::supply_stage_quit_reason(err)?;
        self.call_method_with_values(emitter.clone(), "quit", vec![reason])?;
        Ok(true)
    }
}
