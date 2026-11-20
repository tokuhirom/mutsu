//! `Supply.unique` / `Supply.squish` over an on-demand source.
//!
//! Rakudo defines both as a new supply that taps its source
//! (`supply { whenever self -> \val { ... emit val } }`), keeping the keys it
//! has seen per tap. They used to read the source's materialized values
//! instead: an on-demand source with a `whenever` over a live supply never
//! finishes, so `.squish` hung, and `.unique` saw no values at all (#18400).
//!
//! The derived supply rides the [`super::supply_derive`] machinery (modes
//! `unique` / `squish`); this module holds what is specific to them: the
//! per-tap seen keys and the duplicate test.

use crate::runtime::*;
use crate::value::AttrMap;

/// The keys each live `unique` / `squish` tap has seen, keyed by a per-tap
/// id (the forwarder instances are rebuilt per delivery, as for `head`).
/// A `squish` tap keeps only the last key.
fn seen_keys() -> &'static std::sync::Mutex<HashMap<u64, Vec<Value>>> {
    static MAP: std::sync::OnceLock<std::sync::Mutex<HashMap<u64, Vec<Value>>>> =
        std::sync::OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Start the seen-key list of a new tap.
// Cost: O(1).
pub(super) fn dedup_register() -> u64 {
    use std::sync::atomic::{AtomicU64, Ordering};
    static NEXT: AtomicU64 = AtomicU64::new(1);
    let id = NEXT.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut map) = seen_keys().lock() {
        map.insert(id, Vec::new());
    }
    id
}

pub(super) fn dedup_id(attributes: &AttrMap) -> Option<u64> {
    attributes
        .get("dedup_id")
        .and_then(|v| v.as_int())
        .map(|n| n as u64)
}

/// The source finished or quit: retire the tap's keys.
// Cost: O(1).
pub(super) fn dedup_finish(id: u64) {
    if let Ok(mut map) = seen_keys().lock() {
        map.remove(&id);
    }
}

/// The `:as` / `:with` pair a dedup stage carries as its `callable`.
pub(super) fn dedup_callable(as_fn: Option<Value>, with_fn: Option<Value>) -> Value {
    Value::array(vec![
        as_fn.unwrap_or(Value::NIL),
        with_fn.unwrap_or(Value::NIL),
    ])
}

/// Keep every value the GC must see: the seen keys of the live taps.
// Cost: O(k), k = keys held.
pub(in crate::runtime) fn visit_dedup_roots(visitor: &mut dyn crate::gc::RootVisitor) {
    if let Ok(map) = seen_keys().lock() {
        for keys in map.values() {
            for key in keys {
                visitor.visit_value(key);
            }
        }
    }
}

impl Interpreter {
    /// Whether `value` passes the tap's filter, recording its key: not a
    /// duplicate of any earlier key (`unique`) or of the previous one
    /// (`squish`). The lock is not held while `:as` / `:with` run.
    // Cost: O(s) comparisons, s = keys seen (`unique`) or 1 (`squish`).
    pub(super) fn dedup_admit(
        &mut self,
        id: u64,
        squish: bool,
        callable: &Value,
        value: &Value,
    ) -> Result<bool, RuntimeError> {
        let (as_fn, with_fn) = match callable.view() {
            ValueView::Array(items, ..) => (
                items.first().cloned().filter(|f| !f.is_nil()),
                items.get(1).cloned().filter(|f| !f.is_nil()),
            ),
            _ => (None, None),
        };
        let key = match as_fn {
            Some(f) => self.call_supply_callback(f, vec![value.clone()])?,
            None => value.clone(),
        };
        let seen = seen_keys()
            .lock()
            .ok()
            .and_then(|map| map.get(&id).cloned())
            .unwrap_or_default();
        let mut duplicate = false;
        for earlier in &seen {
            let same = match &with_fn {
                Some(f) => self
                    .call_supply_callback(f.clone(), vec![earlier.clone(), key.clone()])?
                    .truthy(),
                None => crate::runtime::values_identical(earlier, &key),
            };
            if same {
                duplicate = true;
                break;
            }
        }
        if let Ok(mut map) = seen_keys().lock()
            && let Some(keys) = map.get_mut(&id)
        {
            if squish {
                keys.clear();
                keys.push(key);
            } else if !duplicate {
                keys.push(key);
            }
        }
        Ok(!duplicate)
    }

    /// The derived on-demand Supply for `source.unique` / `source.squish`
    /// (with their `:as` / `:with`), where `source` is an on-demand supply.
    // Cost: O(1).
    pub(in crate::runtime) fn make_on_demand_dedup_supply(
        source: Value,
        squish: bool,
        as_fn: Option<Value>,
        with_fn: Option<Value>,
    ) -> Value {
        Self::make_on_demand_derived_supply_named(
            source,
            if squish { "squish" } else { "unique" },
            dedup_callable(as_fn, with_fn),
        )
    }
}
