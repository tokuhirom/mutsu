//! Tapping a live per-tap recipe (ADR-18443).
//!
//! A combinator over a live source returns a recipe: a derived on-demand
//! Supply (`__SupplyDerive`, marked `live_recipe`) whose producer taps its
//! source. Tapping it allocates the recipe's own output supplier, registers
//! the subscriber's tap on it, and only then runs the producer -- which taps
//! the stage below, recursively down to the live source. A value the source
//! emits on another thread while the chain is being built therefore reaches
//! the subscriber exactly once: there is no setup window in which the stage
//! has a source tap but no subscriber. Closing the subscriber's tap runs the
//! close callbacks the producer registered on the output supplier, which
//! close the tap it made on the stage below.

use super::*;
use crate::symbol::Symbol;
use crate::value::{AttrMap, ValueView};

impl Interpreter {
    /// `tap` / `act` of a live per-tap recipe with `attributes`.
    // Cost: O(d), d = stages in the recipe, plus each stage's source tap.
    pub(crate) fn tap_live_recipe(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        use crate::runtime::native_methods::next_supplier_id;
        let out_sid = next_supplier_id();
        let mut out_attrs = HashMap::new();
        out_attrs.insert("values".to_string(), Value::array(Vec::new()));
        out_attrs.insert("taps".to_string(), Value::array(Vec::new()));
        out_attrs.insert("supplier_id".to_string(), Value::int(out_sid as i64));
        out_attrs.insert("live".to_string(), Value::TRUE);
        // `delayed` / `schedule-on` over a recipe mark the recipe itself;
        // the subscriber's tap on this tap's own output honors the mark, so
        // each tap delays (or schedules) its own instance of the chain.
        for key in ["delay_seconds", "scheduler"] {
            if let Some(mark) = attributes.get(key) {
                out_attrs.insert(key.to_string(), mark.clone());
            }
        }
        let out = Value::make_instance(Symbol::intern("Supply"), out_attrs);
        let tap = self.call_method_with_values(out, method, args)?;
        let mut emitter_attrs = HashMap::new();
        emitter_attrs.insert("done".to_string(), Value::FALSE);
        emitter_attrs.insert("supplier_id".to_string(), Value::int(out_sid as i64));
        let emitter = Value::make_instance(Symbol::intern("Supplier"), emitter_attrs);
        let producer = attributes.get("on_demand_callback").cloned().unwrap_or(Value::NIL);
        self.call_sub_value(producer, vec![emitter])?;
        // `Tap.close` runs the output supplier's close callbacks: the
        // producer's release of the stage below.
        let ValueView::Instance { class_name, attributes: tap_attrs, .. } = tap.view() else {
            return Ok(tap);
        };
        let mut handle: HashMap<String, Value> = tap_attrs
            .as_map()
            .iter()
            .map(|(k, v)| (k.to_string(), v.clone()))
            .collect();
        handle.insert("close_supplier_id".to_string(), Value::int(out_sid as i64));
        Ok(Value::make_instance(class_name, handle))
    }

    /// Instantiate a live per-tap recipe (ADR-18443) for one consumer that
    /// only knows a live source by its supplier (a react `whenever`, a
    /// combinator not converted to a recipe yet): tap the recipe -- the one
    /// instantiation path every tap takes -- with forwarders into a fresh
    /// supplier, and answer a Supply over that supplier. Its
    /// `on_close_callbacks` close the tap, which releases every stage below;
    /// a react runs them when the `whenever` or the react ends.
    /// `for_react`: the instance keeps what a preserving root replayed into
    /// it at tap time until the react's drive loop subscribes.
    // Cost: O(d), d = stages in the recipe, plus the recipe's own tap.
    pub(crate) fn instantiate_live_recipe(
        &mut self,
        recipe: &Value,
        for_react: bool,
    ) -> Result<Value, RuntimeError> {
        use crate::runtime::native_methods::{native_method_shim, next_supplier_id};
        let out_sid = next_supplier_id();
        let tap = self.tap_supply_into(recipe, out_sid)?;
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("supplier_id".to_string(), Value::int(out_sid as i64));
        attrs.insert("live".to_string(), Value::truth(Self::live_recipe_is_live(recipe)));
        // What a preserving root replays to the recipe's tap is the backlog
        // of this instance too, until the react subscribes.
        if for_react
            && Self::live_recipe_root(recipe).is_some_and(|root| root.contains_key("preserving"))
        {
            attrs.insert("preserving".to_string(), Value::TRUE);
        }
        attrs.insert(
            "on_close_callbacks".to_string(),
            Value::array(vec![native_method_shim(tap, "close", false)]),
        );
        Ok(Value::make_instance(Symbol::intern("Supply"), attrs))
    }

    /// Tap `supply` with forwarders that emit, finish and quit the supplier
    /// `out_sid`; answer the tap.
    // Cost: O(1) plus the supply's own tap.
    pub(crate) fn tap_supply_into(&mut self, supply: &Value, out_sid: u64) -> Result<Value, RuntimeError> {
        use crate::runtime::native_methods::native_method_shim;
        let mut supplier_attrs = HashMap::new();
        supplier_attrs.insert("done".to_string(), Value::FALSE);
        supplier_attrs.insert("supplier_id".to_string(), Value::int(out_sid as i64));
        let supplier = Value::make_instance(Symbol::intern("Supplier"), supplier_attrs);
        self.call_method_with_values(
            supply.clone(),
            "tap",
            vec![
                native_method_shim(supplier.clone(), "emit", true),
                Value::pair(
                    "done".to_string(),
                    native_method_shim(supplier.clone(), "done", false),
                ),
                Value::pair("quit".to_string(), native_method_shim(supplier, "quit", true)),
            ],
        )
    }

    /// The attributes of the source a live per-tap recipe ultimately taps.
    // Cost: O(d), d = stages in the recipe.
    fn live_recipe_root(supply: &Value) -> Option<crate::value::AttrMap> {
        let mut cur = supply.clone();
        loop {
            let ValueView::Instance { attributes, .. } = cur.view() else {
                return None;
            };
            let attrs = attributes.as_map();
            if !attrs.contains_key(Self::LIVE_RECIPE_KEY) {
                return Some((*attrs).clone());
            }
            let next = attrs.get("derive_source").cloned()?;
            drop(attrs);
            cur = next;
        }
    }

    /// `.live` of a live per-tap recipe: walk its stages down to the live
    /// source; a `head` stage on the way makes it not live.
    // Cost: O(d), d = stages in the recipe.
    pub(crate) fn live_recipe_is_live(supply: &Value) -> bool {
        let mut cur = supply.clone();
        loop {
            let ValueView::Instance { attributes, .. } = cur.view() else {
                return false;
            };
            let attrs = attributes.as_map();
            if !attrs.contains_key(Self::LIVE_RECIPE_KEY) {
                return attrs.get("live").is_some_and(Value::truthy);
            }
            if attrs
                .get("derive_mode")
                .is_some_and(|m| !matches!(m.to_string_value().as_str(), "map" | "grep" | "do" | "start"))
            {
                return false;
            }
            let Some(next) = attrs.get("derive_source").cloned() else {
                return false;
            };
            drop(attrs);
            cur = next;
        }
    }

    /// `merge` / `zip` / `zip-latest` (`kind`) over `sources` plus the
    /// named `:with` / `:initial` in `args`, as a per-tap recipe
    /// (ADR-18443), when a source is live or a recipe and none is backed by a
    /// native event channel (those keep their pump). `None` otherwise, and
    /// for anything but Supply instances, which the eager paths report.
    // Cost: O(n), n = the arguments.
    pub(crate) fn multi_source_recipe(kind: &str, args: &[Value]) -> Option<Value> {
        let mut flat = Vec::with_capacity(args.len());
        super::types::flatten_into_slurpy(args, &mut flat);
        let mut with_fn = None;
        let mut initial = None;
        let mut sources = Vec::with_capacity(flat.len());
        for arg in flat {
            match arg.view() {
                ValueView::Pair(key, value) if key == "with" => with_fn = Some(value.clone()),
                ValueView::Pair(key, value) if key == "initial" => {
                    if let ValueView::Array(items, ..) = value.view() {
                        initial = Some(items.to_vec());
                    }
                }
                ValueView::Instance { class_name, .. }
                    if crate::value::supply_class::is_supply_class(class_name) =>
                {
                    sources.push(arg.clone())
                }
                _ => return None,
            }
        }
        let mut any_live = false;
        for source in &sources {
            let ValueView::Instance { attributes, .. } = source.view() else {
                return None;
            };
            let attrs = attributes.as_map();
            if Self::is_channel_backed_source(&attrs) || attrs.contains_key("supply_id") {
                return None;
            }
            any_live |= Self::stage_is_live_recipe(&attrs);
        }
        any_live.then(|| Self::make_multi_source_recipe(kind, sources, with_fn, initial))
    }

    /// Record `sid`, a supplier a `migrate` tapped an inner supply into
    /// ([`Self::tap_supply_into`]), with that `tap`, so switching away from
    /// it releases the inner supply.
    // Cost: O(c), c = the close callbacks.
    pub(crate) fn migrate_own_inner(sid: u64, tap: Value) {
        crate::runtime::native_methods::register_supplier_close_callback(
            sid,
            crate::runtime::native_methods::native_method_shim(tap, "close", false),
        );
        if let Ok(mut owned) = migrate_owned().lock() {
            owned.insert(sid);
        }
    }

    /// A `migrate` switched away from the inner supplier `sid`: if the
    /// migration tapped it itself, close that tap. A supplier the program
    /// emitted (`$supplier.Supply`) is left alone.
    // Cost: O(c), c = the close callbacks.
    pub(crate) fn migrate_release_inner(&mut self, sid: u64) -> Result<(), RuntimeError> {
        let owned = migrate_owned().lock().is_ok_and(|mut owned| owned.remove(&sid));
        if owned {
            for closer in crate::runtime::native_methods::take_supplier_close_callbacks(sid) {
                self.call_sub_value(closer, vec![])?;
            }
        }
        Ok(())
    }
}

/// The inner suppliers a `migrate` tapped for itself.
fn migrate_owned() -> &'static std::sync::Mutex<std::collections::HashSet<u64>> {
    static SET: std::sync::OnceLock<std::sync::Mutex<std::collections::HashSet<u64>>> =
        std::sync::OnceLock::new();
    SET.get_or_init(|| std::sync::Mutex::new(std::collections::HashSet::new()))
}
