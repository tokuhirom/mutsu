//! `Channel.Supply`: an on-demand Supply whose every tap is one more consumer
//! of the channel queue.
//!
//! rakudo defines it as
//!
//! ```raku
//! method Supply(Channel:D:) {
//!     supply {
//!         whenever $!async-notify.unsanitized-supply.schedule-on($*SCHEDULER) {
//!             my \got = self.poll;
//!             if nqp::eqaddr(got, Nil) { done/die once closed }
//!             else { emit got }
//!         }
//!         loop { my \got = self.poll; last if nqp::eqaddr(got, Nil); emit got }
//!     }
//! }
//! ```
//!
//! so each tap drains the backlog when it starts, then polls the queue once per
//! send. Values are never broadcast and never emitted at send time behind the
//! queue's back: a value leaves the queue for exactly one consumer -- a tap, a
//! `receive`/`poll`, or a react `whenever` draining the channel -- and a value
//! sent before any tap existed stays queued for the first one (issue #9900).
//!
//! The Supply here is the same: an on-demand Supply with a native producer
//! (`__ChannelSupply`). Each tap runs the producer with its own emitter; the
//! producer emits the backlog and attaches the emitter to the channel
//! (`SharedChannel::attach_tap`). From then on [`Interpreter::pump_channel_taps`]
//! -- run after every `send`, `close` and `fail`, and once more when the tap
//! has finished registering -- moves queued values to the attached taps round
//! robin and completes them once the channel is closed and drained. Closing the
//! `Tap` detaches the emitter. Because the Supply is an ordinary on-demand one,
//! `map`/`grep`/`head`, `.list`, `await`, `.Promise` and a `whenever` inside a
//! `supply { }` all tap it per use, with no channel-specific code of their own.
//! A react `whenever` on it reads the `channel` attribute and drains the queue
//! from its drive loop instead (`vm_react_loop`), which is the same competing
//! consumption.

use super::native_shim::native_method_shim;
use super::state::{supplier_has_sinks, supplier_id_from_attrs};
use super::state_supplier::{register_supplier_close_callback, supplier_live_tap_indices};
use crate::runtime::*;
use crate::symbol::Symbol;
use crate::value::{AttrMap, ChannelEnd, SharedChannel, ValueView};

const CLASS: &str = "__ChannelSupply";

impl Interpreter {
    /// `$channel.Supply`.
    // Cost: O(1).
    pub(in crate::runtime) fn make_channel_supply(ch: &SharedChannel) -> Value {
        let mut producer_attrs = HashMap::new();
        producer_attrs.insert("channel".to_string(), Value::channel(ch.clone()));
        let producer = native_method_shim(
            Value::make_instance(Symbol::intern(CLASS), producer_attrs),
            "__mutsu_channel_supply_start",
            true,
        );
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::array(Vec::new()));
        attrs.insert("taps".to_string(), Value::array(Vec::new()));
        attrs.insert("live".to_string(), Value::FALSE);
        attrs.insert("on_demand_callback".to_string(), producer);
        // A react `whenever` drains the channel directly (see the module doc).
        attrs.insert("channel".to_string(), Value::channel(ch.clone()));
        Value::make_instance(Symbol::intern("Supply"), attrs)
    }

    pub(in crate::runtime) fn native_channel_supply(
        &mut self,
        attributes: &AttrMap,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let Some(ch) = channel_of(attributes) else {
            return Err(RuntimeError::new(format!("{CLASS} has no channel")));
        };
        match method {
            // Cost: O(b), b = values queued before the tap.
            "__mutsu_channel_supply_start" => {
                let emitter = args.into_iter().next().unwrap_or(Value::NIL);
                self.channel_supply_start(&ch, emitter)?;
                Ok(Value::NIL)
            }
            // Cost: O(t), t = the channel's attached taps.
            "__mutsu_channel_supply_close" => {
                if let Some(id) = attributes.get("tap_id").and_then(Value::as_int) {
                    ch.detach_tap(id as u64);
                }
                Ok(Value::NIL)
            }
            _ => Err(RuntimeError::new(format!(
                "No native method '{method}' on '{CLASS}'"
            ))),
        }
    }

    /// A tap of the channel's Supply started: emit the backlog, then attach
    /// `emitter` as a consumer of the queue -- or, on a channel that already
    /// ended, complete it.
    fn channel_supply_start(
        &mut self,
        ch: &SharedChannel,
        emitter: Value,
    ) -> Result<(), RuntimeError> {
        // The body of an on-demand supply: these emits are collected and
        // replayed to the tap that is being set up.
        while let Ok(Some(value)) = ch.poll_result() {
            self.call_method_with_values(emitter.clone(), "emit", vec![value])?;
        }
        match ch.end_state() {
            Some(ChannelEnd::Done) => {
                self.call_method_with_values(emitter, "done", vec![])?;
                return Ok(());
            }
            // rakudo's block `die`s with the failure, which quits the supply
            // after the backlog it already emitted.
            Some(ChannelEnd::Quit(reason)) => {
                return Err(Self::runtime_error_from_supply_reason(reason));
            }
            None => {}
        }
        let tap_id = ch.attach_tap(emitter.clone());
        if let ValueView::Instance { attributes, .. } = emitter.view()
            && let Some(sid) = supplier_id_from_attrs(&attributes.as_map())
        {
            let mut close_attrs = HashMap::new();
            close_attrs.insert("channel".to_string(), Value::channel(ch.clone()));
            close_attrs.insert("tap_id".to_string(), Value::int(tap_id as i64));
            let closer = native_method_shim(
                Value::make_instance(Symbol::intern(CLASS), close_attrs),
                "__mutsu_channel_supply_close",
                false,
            );
            register_supplier_close_callback(sid, closer);
        }
        Ok(())
    }

    /// Hand every queued value a ready tap can take to one such tap, round
    /// robin, then complete the ready taps once the channel is closed and
    /// drained. A tap is ready once its `.tap` call has registered it (or,
    /// for a consumer that runs the producer some other way, once something
    /// listens on its emitter): before that an emitted value would reach
    /// nobody, so it stays queued for the pump that runs right after.
    // Cost: O(v * t), v = values moved, t = the channel's attached taps.
    pub(in crate::runtime) fn pump_channel_taps(
        &mut self,
        ch: &SharedChannel,
    ) -> Result<(), RuntimeError> {
        let taps = ch.tap_emitters();
        if taps.is_empty() {
            return Ok(());
        }
        let ready: Vec<u64> = taps
            .iter()
            .filter(|(_, emitter, ready)| *ready || emitter_has_listener(emitter))
            .map(|(id, _, _)| *id)
            .collect();
        if ready.is_empty() {
            return Ok(());
        }
        while let Some((emitter, value)) = ch.take_for_tap(&ready) {
            // A tap callback that dies does not fail the `send` that fed it:
            // in rakudo the callback runs on the scheduler, not in the sender.
            let _ = self.call_method_with_values(emitter, "emit", vec![value]);
        }
        if let Some((ended, end)) = ch.take_ended_taps(&ready) {
            for emitter in ended {
                self.complete_channel_tap(emitter, &end)?;
            }
        }
        Ok(())
    }

    fn complete_channel_tap(
        &mut self,
        emitter: Value,
        end: &ChannelEnd,
    ) -> Result<(), RuntimeError> {
        match end {
            ChannelEnd::Done => self.call_method_with_values(emitter, "done", vec![])?,
            ChannelEnd::Quit(reason) => {
                self.call_method_with_values(emitter, "quit", vec![reason.clone()])?
            }
        };
        Ok(())
    }

    /// `Supply.tap`/`.act`, then -- on a `Channel.Supply` -- mark the taps
    /// this call attached ready and pump: values sent while it was being set
    /// up (from another thread) are still queued, and a close in that window
    /// still has to complete it. (A derived supply such as `.map` taps the
    /// channel's Supply through this same method, so its tap is covered too.)
    // Cost: O(1) plus the tap, plus `pump_channel_taps` for a channel's Supply.
    pub(in crate::runtime) fn native_supply_mut(
        &mut self,
        attrs: AttrMap,
        method: &str,
        args: Vec<Value>,
        publish: &mut crate::runtime::native_methods::AttrPublisher<'_>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        let channel = if matches!(method, "tap" | "act") {
            channel_of(&attrs)
        } else {
            None
        };
        let Some(ch) = channel else {
            return self.native_supply_mut_unpumped(attrs, method, args, publish);
        };
        let first_id = ch.next_tap_id();
        let out = self.native_supply_mut_unpumped(attrs, method, args, publish)?;
        ch.mark_taps_ready_since(first_id);
        self.pump_channel_taps(&ch)?;
        Ok(out)
    }
}

fn channel_of(attrs: &AttrMap) -> Option<SharedChannel> {
    match attrs.get("channel").map(Value::view) {
        Some(ValueView::Channel(ch)) => Some(ch.clone()),
        _ => None,
    }
}

/// Whether a tap or a react sink listens on an on-demand emitter.
// Cost: O(k), k = taps on the emitter.
fn emitter_has_listener(emitter: &Value) -> bool {
    let ValueView::Instance { attributes, .. } = emitter.view() else {
        return false;
    };
    let Some(sid) = supplier_id_from_attrs(&attributes.as_map()) else {
        return false;
    };
    !supplier_live_tap_indices(sid).is_empty() || supplier_has_sinks(sid)
}
