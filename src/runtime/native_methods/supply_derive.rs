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
        let mut producer_attrs = HashMap::new();
        producer_attrs.insert("source".to_string(), source);
        producer_attrs.insert("mode".to_string(), Value::str(mode_name(mode).to_string()));
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

    pub(in crate::runtime) fn native_supply_derive(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let arg = args.into_iter().next().unwrap_or(Value::NIL);
        match method {
            // Cost: O(1) plus the source's own tap.
            "__mutsu_derive_start" => self.derive_start(attributes, arg),
            // Cost: O(1) plus one call of the transform callable.
            "__mutsu_derive_emit" => {
                let emitter = attributes.get("emitter").cloned().unwrap_or(Value::NIL);
                let callable = attributes.get("callable").cloned().unwrap_or(Value::NIL);
                let out = match attributes
                    .get("mode")
                    .map(Value::to_string_value)
                    .as_deref()
                {
                    Some("map") => Some(self.call_sub_value(callable, vec![arg], true)?),
                    Some("do") => {
                        self.call_sub_value(callable, vec![arg.clone()], true)?;
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
                self.call_method_with_values(emitter, "done", vec![])?;
                Ok(Value::NIL)
            }
            // Cost: O(1).
            "__mutsu_derive_quit" => {
                let emitter = attributes.get("emitter").cloned().unwrap_or(Value::NIL);
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
