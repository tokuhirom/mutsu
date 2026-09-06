//! The VM half of ADR-0067 slice 3b: the invocant *arrives* as a container.
//!
//! `class C { method m(\S:) { S = 7 } }; my $c = C.new; $c.m` leaves `$c`
//! holding `7` in raku, because a raw invocant parameter binds the caller's
//! Scalar container rather than a copy of its contents. Slice 3a made an
//! lvalue *call* hand a container back; this is the mirror image, and it shares
//! no code with it: `$c.m` is an ordinary `CallMethodMut`, whose invocant
//! travels from the opcode to the parameter binder as a bare `Value`.
//!
//! The producer is the same one slice 3a uses
//! (`capture_lvalue_invocant_cell`), which is the whole point of reusing it:
//! its route order — an existing frame cell, then an existing env container,
//! then a direct slot box, and only then a freshly minted cell — is what keeps
//! a loop parameter's already-promoted element cell from being shadowed by a
//! second, disconnected one.
//!
//! Transport is a single-slot channel rather than a signature change across
//! `dispatch_compiled_method` / `call_method_with_values`: the invocant reaches
//! the two binders through two disjoint chains (an `Instance` receiver goes
//! straight to `call_compiled_method`, while an `augment`ed native receiver
//! detours through `call_method_mut_with_values` -> `call_method_with_values`
//! -> `try_dispatch_compiled_method_direct_as`), and only the *first* of those
//! carries a receiver name at all. The channel is armed immediately before the
//! dispatch, disarmed immediately after it, and is only consumed by a binder
//! that both agrees on the method name and is looking at a parameter that
//! really is a raw invocant — so a nested dispatch that happens in between
//! (a `where` clause, a multi tie-break) cannot mis-bind it.

use super::*;

/// The container staged for the invocant parameter of the method call the VM is
/// about to dispatch, and the method name that may consume it.
pub(crate) struct PendingRawInvocant {
    pub(crate) method: String,
    pub(crate) cell: Value,
}

impl Interpreter {
    /// Arm the arrival channel for `target.method(args)` when the callee binds
    /// its invocant raw. Returns whether anything was armed, which the caller
    /// passes back to [`Self::disarm_raw_invocant_arrival`].
    ///
    /// The first test is the registry's set-only pre-filter, against a
    /// *borrowed* method name and before any allocation: this runs on every
    /// `$var.method(...)` in the program, and slice 3a measured that the
    /// argument marshalling a full oracle call needs — not the MRO walk — is
    /// where such a gate's cost actually lives. A program that declares no
    /// raw-invocant method anywhere pays one bool load.
    #[inline]
    pub(super) fn arm_raw_invocant_arrival(
        &mut self,
        code: &CompiledCode,
        target_name: &str,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> bool {
        if !crate::runtime::raw_invocant::any_raw_invocant_method_possible()
            || target_name.is_empty()
        {
            return false;
        }
        self.arm_raw_invocant_arrival_slow(code, target_name, target, method, args)
    }

    /// The part of [`Self::arm_raw_invocant_arrival`] past the pre-gate, kept
    /// out of line so the gate itself is a bool load at the call site.
    fn arm_raw_invocant_arrival_slow(
        &mut self,
        code: &CompiledCode,
        target_name: &str,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> bool {
        if !self.method_binds_raw_invocant(target, method, args) {
            return false;
        }
        let cell = if target.is_container_ref() {
            // Already a location: whatever produced it (a `:=`-bound alias, a
            // shared state cell) owns the identity, so hand out that cell
            // rather than asking for another one — the same
            // "reuse before minting" rule the producer below encodes.
            target.clone()
        } else {
            // No storage location to hand out (an aggregate variable, an rvalue
            // receiver) leaves the channel disarmed, so the call behaves exactly
            // as it does today instead of binding a disconnected cell whose
            // writes would be silently dropped.
            let Some(cell) =
                self.capture_lvalue_invocant_cell(code, target_name, target.clone(), None)
            else {
                return false;
            };
            cell
        };
        self.pending_raw_invocant = Some(Box::new(PendingRawInvocant {
            method: method.to_string(),
            cell,
        }));
        true
    }

    /// `try_compiled_method_mut_or_interpret_sym` with the ADR-0067 slice 3b
    /// arrival channel armed around it. Both of `CallMethodMut`'s user-method
    /// dispatch sites go through here so the arm/disarm pair can never be
    /// split; the receiver name the opcode carries is the location the callee
    /// may write through.
    pub(super) fn dispatch_compiled_method_mut_with_raw_invocant(
        &mut self,
        code: &CompiledCode,
        target_name: &str,
        target: Value,
        method: &str,
        method_sym: crate::symbol::Symbol,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let armed = self.arm_raw_invocant_arrival(code, target_name, &target, method, &args);
        let result =
            self.try_compiled_method_mut_or_interpret_sym(target_name, target, method_sym, args);
        self.disarm_raw_invocant_arrival(armed);
        result
    }

    /// Disarm the channel after the dispatch returns. Unconditional on the
    /// `armed` path so a callee that never reached a compiled binder (a native
    /// fallback, an error) cannot leave the cell visible to the next call.
    #[inline]
    pub(super) fn disarm_raw_invocant_arrival(&mut self, armed: bool) {
        if armed {
            self.pending_raw_invocant = None;
        }
    }

    /// The container to bind parameter zero to, for a binder that has reached
    /// the invocant parameter of `method_name`.
    ///
    /// `pd` is re-checked here rather than trusted from the arming side because
    /// multi-dispatch may land on a different candidate than the one the gate
    /// resolved: the parameter actually being bound is the authority on whether
    /// it is raw.
    pub(crate) fn take_raw_invocant_arrival(
        &mut self,
        method_name: &str,
        pd: Option<&crate::ast::ParamDef>,
    ) -> Option<Value> {
        let pending = self.pending_raw_invocant.as_ref()?;
        if pending.method != method_name {
            return None;
        }
        if !pd.is_some_and(crate::runtime::raw_invocant::param_is_raw_invocant) {
            return None;
        }
        self.pending_raw_invocant.take().map(|p| p.cell)
    }
}
