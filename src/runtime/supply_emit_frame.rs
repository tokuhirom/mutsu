//! Which `supply_emit_buffer` frame a `Supplier.emit` lands in.
//!
//! `run_on_demand_body` runs a `supply { }` body (or an explicit
//! `Supply.on-demand` producer) inside a fresh emit-buffer frame and replays
//! what the frame collected to the tap being set up. That frame stands for the
//! body's *own* emitter. An emission on some other supplier made while the body
//! runs -- `supply { $other.emit(1); emit 2 }`, or a `Channel.send` from the body
//! handing a value to a tap of that channel's `Supply` -- is delivered to that
//! supplier's taps by the emit itself, and must not also be collected as if
//! this body had emitted it (rakudo prints only `2` for the outer tap there).
use super::*;

impl Interpreter {
    /// The emit-buffer frame an emission on the supplier `sid` belongs to:
    /// the innermost frame, unless that frame is owned by an on-demand body
    /// whose emitter is a different supplier.
    // Cost: O(1).
    pub(super) fn supply_emit_frame_for(&mut self, sid: Option<u64>) -> Option<&mut Vec<Value>> {
        let depth = self.supply_emit_buffer.len();
        if let Some(&(owner_depth, owner)) = self.supply_emit_owners.last()
            && owner_depth == depth
            && sid != Some(owner)
        {
            return None;
        }
        self.supply_emit_buffer.last_mut()
    }
}
