//! Native dispatch for the internal `__SupplyCollector` class — the
//! emit/done/quit shim `Interpreter::supply_collect_values` (ADR-0031
//! Decision B / Slice 2) taps a `Supply` with, instead of the old
//! synchronous-replay helpers (`replay_cold_whenever_capture` /
//! `replay_static_whenever_promise`) that could not see a value emitted
//! after the tap call itself returned.
//!
//! An instance carries a `collector_id` looked up in
//! `state_supply_collector`'s process-global map to find the `ReactWaker`
//! the drain loop is reading from. Invoking the shim just pushes an event
//! and returns, so the emitting thread (which may be a completely different
//! thread than the one draining) is never blocked on real user code.

use super::state_supply_collector::{
    CollectorSink, supply_collector_sink, unregister_supply_collector,
};
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;
use crate::value::ValueView;
use crate::value::waker::SinkEvent;

impl Interpreter {
    pub(in crate::runtime) fn native_supply_collector(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let Some(ValueView::Int(collector_id)) = attributes.get("collector_id").map(Value::view)
        else {
            return Ok(Value::NIL);
        };
        let Some(sink) = supply_collector_sink(collector_id as u64) else {
            // The drain already gave up (deadline/disconnect) and unregistered
            // this collector; a late event has nowhere left to go.
            return Ok(Value::NIL);
        };
        let arg = || args.first().cloned().unwrap_or(Value::NIL);
        match (method, sink) {
            ("__mutsu_collector_emit", CollectorSink::Waker(waker)) => {
                waker.push(0, SinkEvent::Emit(arg()));
            }
            ("__mutsu_collector_done", CollectorSink::Waker(waker)) => {
                waker.push(0, SinkEvent::Done);
            }
            ("__mutsu_collector_quit", CollectorSink::Waker(waker)) => {
                waker.push(0, SinkEvent::Quit(arg()));
            }
            // Through `Channel.send`/`.close`/`.fail`'s own delivery, so a
            // tap of the channel's Supplies (`$s.Channel.Supply`) sees it.
            ("__mutsu_collector_emit", CollectorSink::Channel(ch)) => {
                self.channel_send_value(&ch, arg());
            }
            ("__mutsu_collector_done", CollectorSink::Channel(ch)) => {
                unregister_supply_collector(collector_id as u64);
                self.channel_close(&ch, None)?;
            }
            ("__mutsu_collector_quit", CollectorSink::Channel(ch)) => {
                unregister_supply_collector(collector_id as u64);
                self.channel_close(&ch, Some(arg()))?;
            }
            _ => {
                return Err(RuntimeError::new(format!(
                    "No native method '{}' on '__SupplyCollector'",
                    method
                )));
            }
        }
        Ok(Value::NIL)
    }

    /// A zero-or-one-param synthesized callable whose body is a single
    /// `MethodCall` on a literal `__SupplyCollector` instance carrying
    /// `collector_id` — the same synthesized-callable idiom
    /// `build_scheduled_shim_sub` uses for the `__ScheduledTapPump` shims.
    /// `"done"` takes no parameter (Raku's `done()` is zero-arg); `"emit"`/
    /// `"quit"` take one (the emitted value / quit exception) and forward it
    /// verbatim.
    pub(in crate::runtime) fn build_supply_collector_shim(collector_id: u64, kind: &str) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("collector_id".to_string(), Value::int(collector_id as i64));
        let instance = Value::make_instance(Symbol::intern("__SupplyCollector"), attrs);
        let method_name = match kind {
            "emit" => "__mutsu_collector_emit",
            "done" => "__mutsu_collector_done",
            "quit" => "__mutsu_collector_quit",
            other => unreachable!("unknown supply collector shim kind {other:?}"),
        };
        super::native_shim::native_method_shim(instance, method_name, kind != "done")
    }
}
