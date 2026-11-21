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
        let mut supplier_attrs = HashMap::new();
        supplier_attrs.insert("done".to_string(), Value::FALSE);
        supplier_attrs.insert("supplier_id".to_string(), Value::int(out_sid as i64));
        let supplier = Value::make_instance(Symbol::intern("Supplier"), supplier_attrs);
        let tap = self.call_method_with_values(
            recipe.clone(),
            "tap",
            vec![
                native_method_shim(supplier.clone(), "emit", true),
                Value::pair(
                    "done".to_string(),
                    native_method_shim(supplier.clone(), "done", false),
                ),
                Value::pair("quit".to_string(), native_method_shim(supplier, "quit", true)),
            ],
        )?;
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
                .is_some_and(|m| !matches!(m.to_string_value().as_str(), "map" | "grep" | "do"))
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
}
