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
        let frame = self.supply_emit_buffer.last_mut()?;
        match frame.owner {
            Some(owner) if sid != Some(owner) => None,
            _ => Some(&mut frame.values),
        }
    }
}
