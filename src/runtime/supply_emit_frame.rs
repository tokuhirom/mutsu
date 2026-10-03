//! A frame of `Interpreter::supply_emit_buffer`, and which frame a
//! `Supplier.emit` lands in.
//!
//! `run_on_demand_body` runs a `supply { }` body (or an explicit
//! `Supply.on-demand` producer) inside a fresh emit-buffer frame and replays
//! what the frame collected to the tap being set up. That frame stands for the
//! body's *own* emitter, and records it as its `owner`. An emission on some
//! other supplier made while the body runs -- `supply { $other.emit(1); emit 2 }`,
//! or a `Channel.send` from the body handing a value to a tap of that channel's
//! `Supply` -- is delivered to that supplier's taps by the emit itself, and must
//! not also be collected as if this body had emitted it (rakudo prints only `2`
//! for the outer tap there). A frame without an owner (a react block's
//! subscription storage, a coercion collecting emits) takes every emission.
use super::*;

/// One frame of the supply emit buffer: the values collected while it is the
/// innermost frame, and the supplier id of the on-demand emitter it collects
/// for, if any.
#[derive(Debug, Default)]
pub(crate) struct EmitFrame {
    pub(crate) values: Vec<Value>,
    pub(crate) owner: Option<u64>,
    /// A `react` block's subscription storage (`Interpreter::enter_react`).
    pub(crate) is_react: bool,
    /// Its setup hold, once a `whenever` taps a live supplier (#11268).
    pub(crate) react_setup: Option<super::react_setup::ReactSetup>,
    /// The tap a tapped on-demand body's plain emits stream to (#11434).
    pub(crate) tap_stream: Option<Box<super::supply_tap_stream::TapStream>>,
    /// A `quit` the on-demand body called on its own emitter while it was
    /// still running, held back to be delivered after the values collected
    /// before it, exactly like a `die` out of the body (#11237).
    pub(crate) quit: Option<Value>,
    /// The body quit its emitter (held back in `quit`, or delivered at once
    /// when the frame streams to a tap): later emits and quits are dropped.
    pub(crate) quit_seen: bool,
}

impl EmitFrame {
    /// A frame that collects only emissions on the supplier `owner`.
    // Cost: O(1).
    pub(crate) fn owned_by(owner: u64) -> Self {
        Self {
            values: Vec::new(),
            owner: Some(owner),
            ..Self::default()
        }
    }

    /// A `react` block's subscription storage.
    // Cost: O(1).
    pub(crate) fn react() -> Self {
        Self {
            is_react: true,
            ..Self::default()
        }
    }
}

impl std::ops::Deref for EmitFrame {
    type Target = Vec<Value>;
    fn deref(&self) -> &Vec<Value> {
        &self.values
    }
}

impl std::ops::DerefMut for EmitFrame {
    fn deref_mut(&mut self) -> &mut Vec<Value> {
        &mut self.values
    }
}

impl Interpreter {
    /// The emit-buffer frame an emission on the supplier `sid` belongs to:
    /// the innermost frame, unless that frame is owned by an on-demand body
    /// whose emitter is a different supplier.
    // Cost: O(1).
    pub(super) fn supply_emit_frame_for(&mut self, sid: Option<u64>) -> Option<&mut Vec<Value>> {
        let frame = self.async_state.supply_emit_buffer.last_mut()?;
        match frame.owner {
            Some(owner) if sid != Some(owner) => None,
            _ => Some(&mut frame.values),
        }
    }

    /// Handle `$emitter.quit(reason)` made while the on-demand body owning
    /// the supplier `sid` is still running. When the body's values are
    /// collected (replayed only once it returns), the quit is held back on
    /// the frame so it reaches the consumer after them. Returns true when the
    /// caller must not deliver the quit now: it was held back, or the body
    /// already quit. Returns false when no such body is running, or when its
    /// frame streams to a tap (the values before it are delivered already),
    /// so the quit is delivered immediately.
    // Cost: O(d), d = depth of the emit-buffer stack (nested supply bodies).
    pub(super) fn defer_on_demand_quit(&mut self, sid: u64, reason: &Value) -> bool {
        let Some(frame) = self
            .async_state
            .supply_emit_buffer
            .iter_mut()
            .rev()
            .find(|f| f.owner == Some(sid))
        else {
            return false;
        };
        if frame.quit_seen {
            return true;
        }
        frame.quit_seen = true;
        if frame.tap_stream.is_some() {
            return false;
        }
        frame.quit = Some(reason.clone());
        true
    }

    /// Whether the running on-demand body owning `sid` already quit, so a
    /// later `emit` on that emitter is dropped.
    // Cost: O(d), d = depth of the emit-buffer stack (nested supply bodies).
    pub(super) fn on_demand_quit_pending(&self, sid: Option<u64>) -> bool {
        let Some(sid) = sid else { return false };
        self.async_state
            .supply_emit_buffer
            .iter()
            .rev()
            .find(|f| f.owner == Some(sid))
            .is_some_and(|f| f.quit_seen)
    }
}
