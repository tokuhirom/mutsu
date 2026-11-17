//! `Promise.Supply`: an on-demand Supply that emits the promise's result and
//! is done once it is kept, or quits with its cause once it is broken.
//!
//! rakudo defines it as
//!
//! ```raku
//! method Supply(Promise:D:) {
//!     Supply.on-demand: -> $s {
//!         self.then: {
//!             if .status == Kept { $s.emit(.result); $s.done }
//!             else               { $s.quit(.cause) }
//!         }
//!     }
//! }
//! ```
//!
//! Each tap runs the native producer (`__PromiseSupply`) with its own emitter.
//! A promise already resolved settles that emitter at once; a planned one
//! queues a `.then` callback (the same `Promise.then` every other subscriber
//! uses), which settles it when the promise resolves.

use super::native_shim::native_method_shim;
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::{AttrMap, SharedPromise, ValueView};

const CLASS: &str = "__PromiseSupply";
/// The Supply attribute naming the promise a `Promise.Supply` was made from.
const PROMISE_SOURCE: &str = "promise_source";

fn promise_of(attributes: &AttrMap) -> Option<SharedPromise> {
    match attributes.get("promise").map(Value::view) {
        Some(ValueView::Promise(shared)) => Some(shared.clone()),
        _ => None,
    }
}

impl Interpreter {
    /// `$promise.Supply`.
    // Cost: O(1).
    pub(crate) fn make_promise_supply(promise: &SharedPromise) -> Value {
        let mut producer_attrs = HashMap::new();
        producer_attrs.insert("promise".to_string(), Value::promise(promise.clone()));
        let producer = native_method_shim(
            Value::make_instance(Symbol::intern(CLASS), producer_attrs),
            "__mutsu_promise_supply_start",
            true,
        );
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::FALSE);
        attrs.insert("on_demand_callback".to_string(), producer);
        // A react/supply `whenever` on it subscribes to the promise itself
        // (`run_whenever_with_value`), which is the same one-shot source.
        attrs.insert(PROMISE_SOURCE.to_string(), Value::promise(promise.clone()));
        Value::make_instance(Symbol::intern("Supply"), attrs)
    }

    /// The promise a `Promise.Supply` was made from, if `supply` is one.
    // Cost: O(1).
    pub(crate) fn promise_supply_source(supply: &Value) -> Option<Value> {
        match supply.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if class_name == "Supply" => attributes.as_map().get(PROMISE_SOURCE).cloned(),
            _ => None,
        }
    }

    pub(in crate::runtime) fn native_promise_supply(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let Some(promise) = promise_of(attributes) else {
            return Err(RuntimeError::new(format!("{CLASS} has no promise")));
        };
        match method {
            // The body of the on-demand supply, run once per tap.
            // Cost: O(1), plus queueing one `.then` callback.
            "__mutsu_promise_supply_start" => {
                let emitter = args.into_iter().next().unwrap_or(Value::NIL);
                if promise.is_resolved() {
                    return self.settle_promise_supply(&promise, emitter);
                }
                let mut settle_attrs = HashMap::new();
                settle_attrs.insert("promise".to_string(), Value::promise(promise.clone()));
                settle_attrs.insert("emitter".to_string(), emitter);
                let settle = native_method_shim(
                    Value::make_instance(Symbol::intern(CLASS), settle_attrs),
                    "__mutsu_promise_supply_settle",
                    true,
                );
                self.promise_chain(&promise, "then", settle)?;
                Ok(Value::NIL)
            }
            // The `.then` callback of a tap made while the promise was planned.
            // Cost: O(1) plus the emitter's delivery.
            "__mutsu_promise_supply_settle" => {
                let emitter = attributes.get("emitter").cloned().unwrap_or(Value::NIL);
                self.settle_promise_supply(&promise, emitter)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{method}' on '{CLASS}'"
            ))),
        }
    }

    /// Emit a kept promise's result and complete `emitter`, or quit it with a
    /// broken promise's cause.
    // Cost: O(1) plus the emitter's delivery.
    fn settle_promise_supply(
        &mut self,
        promise: &SharedPromise,
        emitter: Value,
    ) -> Result<Value, RuntimeError> {
        if promise.status() == "Kept" {
            let (result, _, _) = promise.wait();
            self.call_method_with_values(emitter.clone(), "emit", vec![result])?;
            self.call_method_with_values(emitter, "done", vec![])?;
        } else {
            let cause = Self::promise_cause(promise)?;
            self.call_method_with_values(emitter, "quit", vec![cause])?;
        }
        Ok(Value::NIL)
    }
}
