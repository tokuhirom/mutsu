//! What a compiled body needs from its call frame (ADR-18393 D2).
//!
//! A frame today is an `Env` overlay, a readonly scope and a record that
//! saves every side register. A body whose ops reach none of those can run
//! in a *slot frame* (`vm_method_slot_frame`): its slot window and a slim
//! record. [`FrameNeeds`] is the per-chunk answer, computed once from the
//! emitted ops and the chunk's existing flags.
//!
//! The classifier is a **closed list**: an op it does not name sets
//! [`FrameNeeds::NAME_VIEW`], so a new op can only make slot frames rarer,
//! never wrong. The stages of ADR-18393 lift bits one at a time by teaching
//! the slot frame to answer them without a name view.

use crate::opcode::{CompiledCode, OpCode};

/// A set of the frame services a body may need (ADR-18393 D2).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct FrameNeeds(u8);

impl FrameNeeds {
    pub(crate) const NONE: FrameNeeds = FrameNeeds(0);
    /// A by-name read or write that must start at this frame: the env overlay.
    pub(crate) const NAME_VIEW: FrameNeeds = FrameNeeds(1 << 0);
    /// `$_` / `$/` / `$!` answered by the frame root.
    pub(crate) const ROUTINE_STATE: FrameNeeds = FrameNeeds(1 << 1);
    /// A declaration whose name bookkeeping is observable (dynamic, `our`,
    /// `state`, a bind declaration, a typed `my`).
    pub(crate) const DECL_NAMES: FrameNeeds = FrameNeeds(1 << 2);
    /// A parameter slot written, bound or passed as a call argument: the
    /// name-keyed readonly set refuses those today.
    // Set by the classifier once stage 2 admits call and bind ops.
    #[allow(dead_code)]
    pub(crate) const READONLY_OBS: FrameNeeds = FrameNeeds(1 << 3);
    /// Closure creation, which captures the frame's env.
    pub(crate) const CLOSURES: FrameNeeds = FrameNeeds(1 << 4);
    /// Phasers and `let`/`temp`, which restore by name.
    // Phaser ops fall to `NAME_VIEW` until a stage classifies them.
    #[allow(dead_code)]
    pub(crate) const PHASERS: FrameNeeds = FrameNeeds(1 << 5);
    /// The deferral context and the call capture (`callsame`, `samewith`,
    /// `nqp::usecapture`, an observed `*%_`).
    pub(crate) const DISPATCH_CTX: FrameNeeds = FrameNeeds(1 << 6);

    // Cost: O(1).
    pub(crate) const fn union(self, other: FrameNeeds) -> FrameNeeds {
        FrameNeeds(self.0 | other.0)
    }

    // Cost: O(1).
    pub(crate) const fn is_empty(self) -> bool {
        self.0 == 0
    }

    // Cost: O(1).
    #[allow(dead_code)]
    pub(crate) const fn contains(self, other: FrameNeeds) -> bool {
        self.0 & other.0 == other.0
    }
}

impl CompiledCode {
    /// What this body needs from its frame (module docs). Computed on first
    /// use and kept for the life of the chunk.
    // Cost: O(n + l) on first call, n = ops, l = local slots; O(1) after.
    pub(crate) fn frame_needs(&self) -> FrameNeeds {
        *self.frame_needs.get_or_init(|| self.compute_frame_needs())
    }

    // Cost: O(n + l), n = ops, l = local slots.
    fn compute_frame_needs(&self) -> FrameNeeds {
        let mut needs = FrameNeeds::NONE;
        if self.static_name_lookup
            || self.needs_reflective_capture
            || self.uses_callframe
            || !self.free_var_writes.is_empty()
            || !self.free_var_container_writes.is_empty()
            || !self.nested_sub_written_free.is_empty()
        {
            needs = needs.union(FrameNeeds::NAME_VIEW);
        }
        if self.reads_topic || self.writes_topic || self.uses_routine_state || self.sets_routine_state
        {
            needs = needs.union(FrameNeeds::ROUTINE_STATE);
        }
        if !self.state_locals.is_empty() || !self.class_decl_plans.is_empty() {
            needs = needs.union(FrameNeeds::DECL_NAMES);
        }
        if !self.closure_compiled_codes.is_empty() || self.declares_inner_routines() {
            needs = needs.union(FrameNeeds::CLOSURES);
        }
        if self.uses_dispatcher || self.uses_capture || self.may_observe_named_slurpy {
            needs = needs.union(FrameNeeds::DISPATCH_CTX);
        }
        for op in self.ops.iter() {
            needs = needs.union(self.op_frame_needs(op));
        }
        needs
    }

    /// The closed op list (module docs): what executing `op` in this chunk
    /// needs from the frame.
    // Cost: O(1).
    fn op_frame_needs(&self, op: &OpCode) -> FrameNeeds {
        match op {
            // Constants and arithmetic: no name is read or written. An
            // arithmetic op on an object may call user code (an operator
            // candidate, a coercion); that callee builds its own frame over
            // the unchanged caller env, and `CALLER::` from it reads this
            // frame's record and slots (ADR-18393 D3).
            OpCode::LoadConst(_)
            | OpCode::LoadNil
            | OpCode::LoadTrue
            | OpCode::LoadFalse
            | OpCode::Add
            | OpCode::Sub
            | OpCode::Mul => FrameNeeds::NONE,
            // A plain local's read takes the slot-only fast path; any other
            // local (an attribute slot, a sigiled aggregate, a `.`-twigil)
            // has name-shaped guard work.
            OpCode::GetLocal(idx) if self.local_read_plain(*idx as usize) => FrameNeeds::NONE,
            _ => FrameNeeds::NAME_VIEW,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::FrameNeeds;

    #[test]
    fn union_and_contains() {
        let n = FrameNeeds::NAME_VIEW.union(FrameNeeds::CLOSURES);
        assert!(n.contains(FrameNeeds::NAME_VIEW));
        assert!(n.contains(FrameNeeds::CLOSURES));
        assert!(!n.contains(FrameNeeds::PHASERS));
        assert!(!n.is_empty());
        assert!(FrameNeeds::NONE.is_empty());
    }
}
