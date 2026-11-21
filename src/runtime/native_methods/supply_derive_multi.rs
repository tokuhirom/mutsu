//! `merge`, `zip` and `zip-latest` over live sources as per-tap recipes
//! (ADR-18443 slice 3).
//!
//! Rakudo defines each as `supply { whenever $a { ... }; whenever $b { ... } }`
//! with its buffers local to the supply block: every tap of the combined
//! supply taps every source for itself, with its own buffers, and closing it
//! closes those source taps. The combined supply here is a derived on-demand
//! Supply whose native producer (`__SupplyDeriveMulti`) does that when the
//! tap path ([`Interpreter::tap_live_recipe`]) runs it with the tap's own
//! emitter.
//!
//! Completion follows rakudo: `merge` and `zip-latest` are done when every
//! source is done; `zip` is done as soon as a source is done and its buffer
//! is empty, since no further tuple can be formed. A quit from any source
//! quits the combined supply. Finishing closes the source taps still open.
//!
//! The tuple building (`zip_take_tuple`, `zip_latest_tuple`) is the one
//! implementation the eager supplier zip stages use too.

use super::native_shim::native_method_shim;
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::{AttrMap, ValueView};

const CLASS: &str = "__SupplyDeriveMulti";

/// Take one tuple off the per-source buffers when every buffer holds a
/// value (`zip`).
// Cost: O(n), n = sources.
pub(in crate::runtime) fn zip_take_tuple(
    buffers: &mut [std::collections::VecDeque<Value>],
) -> Option<Vec<Value>> {
    if buffers.iter().any(|b| b.is_empty()) {
        return None;
    }
    Some(buffers.iter_mut().filter_map(|b| b.pop_front()).collect())
}

/// The tuple of every source's latest value, once each has one
/// (`zip-latest`).
// Cost: O(n), n = sources.
pub(in crate::runtime) fn zip_latest_tuple(latest: &[Option<Value>]) -> Option<Vec<Value>> {
    latest.iter().cloned().collect()
}

/// One tap's state.
struct MultiState {
    kind: Kind,
    buffers: Vec<std::collections::VecDeque<Value>>,
    latest: Vec<Option<Value>>,
    done: Vec<bool>,
    finished: bool,
}

#[derive(Clone, Copy, PartialEq)]
enum Kind {
    Merge,
    Zip,
    ZipLatest,
}

impl Kind {
    fn from_name(name: &str) -> Kind {
        match name {
            "zip" => Kind::Zip,
            "zip-latest" => Kind::ZipLatest,
            _ => Kind::Merge,
        }
    }
}

fn states() -> &'static std::sync::Mutex<HashMap<u64, MultiState>> {
    static MAP: std::sync::OnceLock<std::sync::Mutex<HashMap<u64, MultiState>>> =
        std::sync::OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

fn state_id(attributes: &AttrMap) -> Option<u64> {
    attributes.get("state_id").and_then(Value::as_int).map(|n| n as u64)
}

/// Keep every value the GC must see: the buffered and latest values.
// Cost: O(v), v = values held.
pub(in crate::runtime) fn visit_multi_roots(visitor: &mut dyn crate::gc::RootVisitor) {
    if let Ok(map) = states().lock() {
        for state in map.values() {
            for buffer in &state.buffers {
                buffer.iter().for_each(|v| visitor.visit_value(v));
            }
            state.latest.iter().flatten().for_each(|v| visitor.visit_value(v));
        }
    }
}

/// What a source event asks of the combined supply.
enum Outcome {
    Nothing,
    Emit(Vec<Value>),
    EmitThenFinish(Vec<Value>),
}

impl Interpreter {
    /// The combined per-tap recipe for `kind` (`merge` / `zip` /
    /// `zip-latest`) over `sources`, with `zip`'s `:with` and `zip-latest`'s
    /// `:with` / `:initial`.
    // Cost: O(n), n = sources.
    pub(in crate::runtime) fn make_multi_source_recipe(
        kind: &str,
        sources: Vec<Value>,
        with_fn: Option<Value>,
        initial: Option<Vec<Value>>,
    ) -> Value {
        let first = sources.first().cloned().unwrap_or(Value::NIL);
        let mut producer_attrs = HashMap::new();
        producer_attrs.insert("sources".to_string(), Value::array(sources));
        producer_attrs.insert("kind".to_string(), Value::str(kind.to_string()));
        producer_attrs.insert("with".to_string(), with_fn.unwrap_or(Value::NIL));
        producer_attrs.insert(
            "initial".to_string(),
            initial.map_or(Value::NIL, Value::array),
        );
        let producer = native_method_shim(
            Value::make_instance(Symbol::intern(CLASS), producer_attrs),
            "__mutsu_multi_start",
            true,
        );
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::FALSE);
        attrs.insert("on_demand_callback".to_string(), producer);
        attrs.insert("derived_on_demand".to_string(), Value::TRUE);
        attrs.insert("derive_source".to_string(), first);
        attrs.insert("derive_mode".to_string(), Value::str(kind.to_string()));
        attrs.insert(Self::LIVE_RECIPE_KEY.to_string(), Value::TRUE);
        Value::make_instance(Symbol::intern("Supply"), attrs)
    }

    pub(in crate::runtime) fn native_supply_derive_multi(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let arg = args.into_iter().next().unwrap_or(Value::NIL);
        match method {
            // Cost: O(n) plus each source's own tap, n = sources.
            "__mutsu_multi_start" => self.multi_start(attributes, arg),
            // Cost: O(n) plus the `:with` call, n = sources.
            "__mutsu_multi_emit" => {
                let index = attributes.get("index").and_then(Value::as_int).unwrap_or(0) as usize;
                let outcome = match state_id(attributes) {
                    Some(id) => multi_on_emit(id, index, arg),
                    None => Outcome::Nothing,
                };
                self.multi_apply(attributes, outcome)
            }
            // Cost: O(n), n = sources.
            "__mutsu_multi_done" => {
                let index = attributes.get("index").and_then(Value::as_int).unwrap_or(0) as usize;
                let outcome = match state_id(attributes) {
                    Some(id) => multi_on_done(id, index),
                    None => Outcome::Nothing,
                };
                self.multi_apply(attributes, outcome)
            }
            // Cost: O(1) plus closing the source taps.
            "__mutsu_multi_quit" => {
                if state_id(attributes).is_some_and(multi_take_finish) {
                    let emitter = attributes.get("emitter").cloned().unwrap_or(Value::NIL);
                    self.call_method_with_values(emitter.clone(), "quit", vec![arg])?;
                    self.multi_release(&emitter)?;
                }
                Ok(Value::NIL)
            }
            // Cost: O(n), n = source taps.
            "__mutsu_multi_close" => {
                if let Some(id) = state_id(attributes)
                    && let Ok(mut map) = states().lock()
                {
                    map.remove(&id);
                }
                if let Some(ValueView::Array(taps, ..)) = attributes.get("source_taps").map(Value::view) {
                    for tap in taps.iter() {
                        self.call_method_with_values(tap.clone(), "close", vec![])?;
                    }
                }
                Ok(Value::NIL)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{}' on '{}'",
                method, CLASS
            ))),
        }
    }

    /// The combined supply was tapped: tap every source with forwarders bound
    /// to `emitter`, and make closing the tap close those source taps.
    fn multi_start(&mut self, attributes: &AttrMap, emitter: Value) -> Result<Value, RuntimeError> {
        let sources = match attributes.get("sources").map(Value::view) {
            Some(ValueView::Array(items, ..)) => items.to_vec(),
            _ => Vec::new(),
        };
        let kind = Kind::from_name(
            &attributes.get("kind").map(Value::to_string_value).unwrap_or_default(),
        );
        let n = sources.len();
        let latest = match attributes.get("initial").map(Value::view) {
            Some(ValueView::Array(items, ..)) if items.len() == n => {
                items.iter().cloned().map(Some).collect()
            }
            _ => vec![None; n],
        };
        use std::sync::atomic::{AtomicU64, Ordering};
        static NEXT: AtomicU64 = AtomicU64::new(1);
        let id = NEXT.fetch_add(1, Ordering::Relaxed);
        if let Ok(mut map) = states().lock() {
            map.insert(
                id,
                MultiState {
                    kind,
                    buffers: vec![std::collections::VecDeque::new(); n],
                    latest,
                    done: vec![false; n],
                    finished: false,
                },
            );
        }
        if n == 0 {
            if multi_take_finish(id) {
                self.call_method_with_values(emitter, "done", vec![])?;
            }
            return Ok(Value::NIL);
        }
        let mut source_taps = Vec::with_capacity(n);
        for (index, source) in sources.into_iter().enumerate() {
            if state_finished(id) {
                break;
            }
            let mut fwd_attrs = HashMap::new();
            fwd_attrs.insert("state_id".to_string(), Value::int(id as i64));
            fwd_attrs.insert("index".to_string(), Value::int(index as i64));
            fwd_attrs.insert("emitter".to_string(), emitter.clone());
            if let Some(with) = attributes.get("with") {
                fwd_attrs.insert("with".to_string(), with.clone());
            }
            let fwd = Value::make_instance(Symbol::intern(CLASS), fwd_attrs);
            source_taps.push(self.call_method_with_values(
                source,
                "tap",
                vec![
                    native_method_shim(fwd.clone(), "__mutsu_multi_emit", true),
                    Value::pair(
                        "done".to_string(),
                        native_method_shim(fwd.clone(), "__mutsu_multi_done", false),
                    ),
                    Value::pair(
                        "quit".to_string(),
                        native_method_shim(fwd, "__mutsu_multi_quit", true),
                    ),
                ],
            )?);
        }
        let mut close_attrs = HashMap::new();
        close_attrs.insert("state_id".to_string(), Value::int(id as i64));
        close_attrs.insert("source_taps".to_string(), Value::array(source_taps));
        let closer = native_method_shim(
            Value::make_instance(Symbol::intern(CLASS), close_attrs),
            "__mutsu_multi_close",
            false,
        );
        // A source that finished the combined supply while the sources were
        // being tapped found no close callback yet: release them now.
        if state_finished(id) {
            self.call_sub_value(closer, vec![])?;
        } else if let ValueView::Instance { attributes: em, .. } = emitter.view()
            && let Some(sid) = em.as_map().get("supplier_id").and_then(Value::as_int)
        {
            super::state_supplier::register_supplier_close_callback(sid as u64, closer);
        }
        Ok(Value::NIL)
    }

    /// Deliver what a source event produced, applying `:with` to each tuple.
    fn multi_apply(&mut self, attributes: &AttrMap, outcome: Outcome) -> Result<Value, RuntimeError> {
        let emitter = attributes.get("emitter").cloned().unwrap_or(Value::NIL);
        let with = attributes.get("with").cloned().filter(|w| !w.is_nil());
        let (values, finish) = match outcome {
            Outcome::Nothing => return Ok(Value::NIL),
            Outcome::Emit(values) => (values, false),
            Outcome::EmitThenFinish(values) => (values, true),
        };
        for value in values {
            let value = match (&with, value.view()) {
                (Some(f), ValueView::Array(items, ..)) => {
                    self.call_supply_callback(f.clone(), items.to_vec())?
                }
                _ => value,
            };
            self.call_method_with_values(emitter.clone(), "emit", vec![value])?;
        }
        if finish {
            self.call_method_with_values(emitter.clone(), "done", vec![])?;
            self.multi_release(&emitter)?;
        }
        Ok(Value::NIL)
    }

    /// The combined supply finished on its own: close the source taps it
    /// still holds (the close callbacks `multi_start` registered).
    fn multi_release(&mut self, emitter: &Value) -> Result<(), RuntimeError> {
        if let ValueView::Instance { attributes, .. } = emitter.view()
            && let Some(sid) = attributes.as_map().get("supplier_id").and_then(Value::as_int)
        {
            for cb in super::state_supplier::take_supplier_close_callbacks(sid as u64) {
                self.call_sub_value(cb, vec![])?;
            }
        }
        Ok(())
    }
}

fn state_finished(id: u64) -> bool {
    states().lock().map_or(true, |map| map.get(&id).is_none_or(|s| s.finished))
}

/// Mark the tap finished; `true` if it was not already.
fn multi_take_finish(id: u64) -> bool {
    let Ok(mut map) = states().lock() else {
        return false;
    };
    match map.get_mut(&id) {
        Some(state) if !state.finished => {
            state.finished = true;
            true
        }
        _ => false,
    }
}

/// A value from source `index`.
fn multi_on_emit(id: u64, index: usize, value: Value) -> Outcome {
    let Ok(mut map) = states().lock() else {
        return Outcome::Nothing;
    };
    let Some(state) = map.get_mut(&id).filter(|s| !s.finished) else {
        return Outcome::Nothing;
    };
    match state.kind {
        Kind::Merge => Outcome::Emit(vec![value]),
        Kind::ZipLatest => {
            if let Some(slot) = state.latest.get_mut(index) {
                *slot = Some(value);
            }
            Outcome::Emit(zip_latest_tuple(&state.latest).map(Value::array).into_iter().collect())
        }
        Kind::Zip => {
            if let Some(buffer) = state.buffers.get_mut(index) {
                buffer.push_back(value);
            }
            let tuple = zip_take_tuple(&mut state.buffers).map(Value::array);
            if zip_exhausted(state) {
                state.finished = true;
                Outcome::EmitThenFinish(tuple.into_iter().collect())
            } else {
                Outcome::Emit(tuple.into_iter().collect())
            }
        }
    }
}

/// Source `index` is done.
fn multi_on_done(id: u64, index: usize) -> Outcome {
    let Ok(mut map) = states().lock() else {
        return Outcome::Nothing;
    };
    let Some(state) = map.get_mut(&id).filter(|s| !s.finished) else {
        return Outcome::Nothing;
    };
    if let Some(flag) = state.done.get_mut(index) {
        *flag = true;
    }
    let finished = match state.kind {
        Kind::Zip => zip_exhausted(state),
        Kind::Merge | Kind::ZipLatest => state.done.iter().all(|d| *d),
    };
    if finished {
        state.finished = true;
        Outcome::EmitThenFinish(Vec::new())
    } else {
        Outcome::Nothing
    }
}

/// `zip` can form no further tuple: a source is done with nothing buffered.
fn zip_exhausted(state: &MultiState) -> bool {
    state
        .done
        .iter()
        .zip(&state.buffers)
        .any(|(done, buffer)| *done && buffer.is_empty())
}
