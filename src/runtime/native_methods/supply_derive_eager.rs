//! `stable`, `delayed`, `throttle` and `migrate` over a live source, one
//! instance per tap (ADR-18443, #18579); `stable` and `delayed` over a finite
//! source too (#18696, #18615).
//!
//! Each stage already has a live implementation that runs over a
//! Supplier-backed supply: `stable` debounces and `delayed` delays on a
//! worker thread, `migrate` switches between inner supplies. Rakudo builds each per tap
//! (`supply { whenever self { ... } }`), so the derived Supply here is a
//! recipe (mode `eager-stage`) whose tap instantiates its source for that tap
//! alone, builds the stage over that instance, and forwards what the stage
//! emits. Closing the tap closes the instance and finishes it, which ends the
//! stage's worker.

use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

/// The derive mode of a per-tap eager stage.
pub(super) const EAGER_STAGE_MODE: &str = "eager-stage";

impl Interpreter {
    /// The per-tap recipe of `stable` / `migrate` (`name`) with `args` over
    /// the live `source`.
    // Cost: O(a), a = the arguments.
    pub(in crate::runtime) fn make_eager_stage_supply(source: Value, name: &str, args: Vec<Value>) -> Value {
        let mut params = Vec::with_capacity(args.len() + 1);
        params.push(Value::str(name.to_string()));
        params.extend(args);
        Self::mark_live_recipe(Self::make_on_demand_derived_supply_named(
            source,
            EAGER_STAGE_MODE,
            Value::array(params),
        ))
    }

    /// One tap's stage: instantiate `source` for this tap, build the stage
    /// named in `params` over it, and answer the stage's Supply together
    /// with the callbacks that release the instance when the tap closes.
    // Cost: O(d), d = stages below, plus the instance's own tap.
    pub(super) fn eager_stage_for_tap(
        &mut self,
        source: Value,
        params: &Value,
    ) -> Result<(Value, Vec<Value>, Option<StageFeed>), RuntimeError> {
        let (name, args) = match params.view() {
            ValueView::Array(items, ..) => (
                items.first().map(Value::to_string_value).unwrap_or_default(),
                items.iter().skip(1).cloned().collect::<Vec<_>>(),
            ),
            _ => (String::new(), Vec::new()),
        };
        // `delayed` / `stable` listen on a fresh supplier first, and the source is fed
        // into it only once the stage's own tap is in place: a finite source
        // emits everything (and `done`) while it is tapped (#18696).
        if matches!(name.as_str(), "delayed" | "stable") {
            let sid = super::next_supplier_id();
            let seconds = args.first().map(Value::to_f64).unwrap_or(0.0);
            let stage = if name == "delayed" {
                self.delayed_live_supply(sid, seconds)
            } else {
                self.stable_live_supply(sid, seconds)
            };
            return Ok((stage, Vec::new(), Some(StageFeed { source, sid })));
        }
        let instance = self.instantiate_live_recipe(&source, false)?;
        let ValueView::Instance { attributes, .. } = instance.view() else {
            return Ok((instance, Vec::new(), None));
        };
        let attrs = attributes.as_map();
        let Some(sid) = super::supplier_id_from_attrs(&attrs) else {
            return Ok((instance.clone(), Vec::new(), None));
        };
        let stage = match name.as_str() {
            "throttle" => self.throttle_live_supply(sid, &args),
            _ => self.native_supply(&attrs, &name, args)?,
        };
        // Close the instance's tap on the source, then finish the instance,
        // so the stage's worker sees `done` and stops.
        let closers = match attrs.get("on_close_callbacks").map(Value::view) {
            Some(ValueView::Array(items, ..)) => items.to_vec(),
            _ => Vec::new(),
        };
        Ok((stage, with_finish(closers, sid), None))
    }

    /// Feed a stage that listens on `feed.sid`: tap its source into that
    /// supplier, and answer the callbacks that release the source when the
    /// stage's tap closes.
    // Cost: O(1) plus the source's own tap (a finite source emits every
    // value here).
    pub(super) fn feed_eager_stage(&mut self, feed: StageFeed) -> Result<Vec<Value>, RuntimeError> {
        let tap = self.tap_supply_into(&feed.source, feed.sid)?;
        Ok(with_finish(vec![super::native_method_shim(tap, "close", false)], feed.sid))
    }
}

/// The source a stage still has to be fed from, once the stage is tapped:
/// `source` emits into the supplier `sid` the stage listens on.
pub(super) struct StageFeed {
    source: Value,
    sid: u64,
}

/// `closers` plus finishing the supplier `sid`, so the stage's worker sees
/// `done` and stops.
// Cost: O(1).
fn with_finish(mut closers: Vec<Value>, sid: u64) -> Vec<Value> {
    let mut supplier_attrs = HashMap::new();
    supplier_attrs.insert("done".to_string(), Value::FALSE);
    supplier_attrs.insert("supplier_id".to_string(), Value::int(sid as i64));
    let supplier = Value::make_instance(Symbol::intern("Supplier"), supplier_attrs);
    closers.push(super::native_method_shim(supplier, "done", false));
    closers
}
