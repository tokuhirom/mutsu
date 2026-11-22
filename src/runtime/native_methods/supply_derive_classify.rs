//! `Supply.classify` / `Supply.categorize` over a live source as a per-tap
//! recipe (ADR-18443 slice 3).
//!
//! Rakudo keeps the key => group map inside the `supply { whenever self ...
//! }` that `classify` returns, so every tap of the classified supply gets its
//! own groups. Each group is backed by a `Supplier::Preserving`: the
//! `key => Supply` pair reaches the outer tap before the value that opened
//! the group, so a consumer that taps the group from that callback (or
//! later) still sees it. The groups are done or quit with the source.
//!
//! The derived supply rides the [`super::supply_derive`] machinery (modes
//! `classify` / `categorize`, state in [`super::supply_derive_buffer`]); this
//! module holds the per-tap groups and their delivery.

use crate::runtime::*;
use crate::symbol::Symbol;

/// The groups of each live `classify` / `categorize` tap, keyed by a per-tap
/// id: the key's string form, the key and the group's preserving supplier.
type Groups = Vec<(String, Value)>;

fn groups() -> &'static std::sync::Mutex<HashMap<u64, Groups>> {
    static MAP: std::sync::OnceLock<std::sync::Mutex<HashMap<u64, Groups>>> =
        std::sync::OnceLock::new();
    MAP.get_or_init(|| std::sync::Mutex::new(HashMap::new()))
}

/// Start the groups of a new tap.
// Cost: O(1).
pub(super) fn classify_register(id: u64) {
    if let Ok(mut map) = groups().lock() {
        map.insert(id, Vec::new());
    }
}

/// The source finished or quit: retire the tap's groups and return their
/// suppliers, to be finished the same way.
// Cost: O(1).
pub(super) fn classify_take_groups(id: u64) -> Vec<Value> {
    groups()
        .lock()
        .ok()
        .and_then(|mut map| map.remove(&id))
        .map(|groups| groups.into_iter().map(|(_, supplier)| supplier).collect())
        .unwrap_or_default()
}

/// Keep every value the GC must see: the group suppliers of the live taps.
// Cost: O(g), g = groups held.
pub(in crate::runtime) fn visit_classify_roots(visitor: &mut dyn crate::gc::RootVisitor) {
    if let Ok(map) = groups().lock() {
        for groups in map.values() {
            groups.iter().for_each(|(_, supplier)| visitor.visit_value(supplier));
        }
    }
}

impl Interpreter {
    /// One value through a `classify` / `categorize` tap `id`: map it to its
    /// key(s), open a group (and emit its `key => Supply` pair on `emitter`)
    /// for each key not seen yet, and emit the value into each group.
    /// `true` when the mapper died and the stage quit.
    // Cost: O(g + k) plus one mapper call, g = groups of the tap, k = keys.
    pub(super) fn classify_stage_emit(
        &mut self,
        id: u64,
        categorize: bool,
        mapper: &Value,
        value: Value,
        emitter: &Value,
    ) -> Result<bool, RuntimeError> {
        let mapped = match self.apply_classify_mapper(mapper, &value) {
            Ok(mapped) => mapped,
            Err(err) => {
                let reason = Self::supply_stage_quit_reason(err)?;
                for supplier in classify_take_groups(id) {
                    self.call_method_with_values(supplier, "quit", vec![reason.clone()])?;
                }
                self.call_method_with_values(emitter.clone(), "quit", vec![reason])?;
                return Ok(true);
            }
        };
        let keys = if categorize {
            Self::flatten_categorize_keys(mapped)
        } else {
            vec![mapped]
        };
        for key in keys {
            let key_str = key.to_string_value();
            let found = groups().lock().ok().and_then(|map| {
                map.get(&id)?
                    .iter()
                    .find(|(k, _)| *k == key_str)
                    .map(|(_, supplier)| supplier.clone())
            });
            let supplier = match found {
                Some(supplier) => supplier,
                None => {
                    let (supplier, supply) = Self::classify_group();
                    if let Ok(mut map) = groups().lock()
                        && let Some(groups) = map.get_mut(&id)
                    {
                        groups.push((key_str, supplier.clone()));
                    }
                    self.call_method_with_values(
                        emitter.clone(),
                        "emit",
                        vec![Value::value_pair(key, supply)],
                    )?;
                    supplier
                }
            };
            self.call_method_with_values(supplier, "emit", vec![value.clone()])?;
        }
        Ok(false)
    }

    /// A new group: its preserving supplier and the Supply over it.
    // Cost: O(1).
    fn classify_group() -> (Value, Value) {
        let sid = super::next_supplier_id();
        let mut supplier_attrs = HashMap::new();
        supplier_attrs.insert("done".to_string(), Value::FALSE);
        supplier_attrs.insert("supplier_id".to_string(), Value::int(sid as i64));
        supplier_attrs.insert("preserving".to_string(), Value::TRUE);
        let supplier = Value::make_instance(Symbol::intern("Supplier::Preserving"), supplier_attrs);
        let mut supply_attrs = HashMap::new();
        supply_attrs.insert("values".to_string(), Value::array(Vec::new()));
        supply_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        supply_attrs.insert("live".to_string(), Value::TRUE);
        supply_attrs.insert("supplier_id".to_string(), Value::int(sid as i64));
        supply_attrs.insert("preserving".to_string(), Value::TRUE);
        (supplier, Value::make_instance(Symbol::intern("Supply"), supply_attrs))
    }

    /// The source of a `classify` / `categorize` tap `id` finished (`quit`
    /// is the reason it quit with, or `None` for done): finish every group
    /// the same way.
    // Cost: O(g), g = groups of the tap.
    pub(super) fn classify_finish(&mut self, id: u64, quit: Option<&Value>) -> Result<(), RuntimeError> {
        for supplier in classify_take_groups(id) {
            match quit {
                Some(reason) => self.call_method_with_values(supplier, "quit", vec![reason.clone()])?,
                None => self.call_method_with_values(supplier, "done", vec![])?,
            };
        }
        Ok(())
    }
}

/// Whether `mode` is one this module owns, and whether it categorizes.
// Cost: O(1).
pub(super) fn classify_mode(mode: &str) -> Option<bool> {
    match mode {
        "classify" => Some(false),
        "categorize" => Some(true),
        _ => None,
    }
}
