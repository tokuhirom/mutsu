//! The instance-receiver front of `CallMethodMut` (#19232).
//!
//! A method call on an ordinary object whose `(class, method, argument
//! types)` key the plain-method lane has already proven inert
//! (`note_plain_method_lane_reached`) is answered by
//! [`Interpreter::try_plain_method_early_lane`]. That lane used to run only
//! after `exec_call_method_mut_named_op` and `exec_call_method_mut_op_impl`
//! had tried a dozen other lanes (`.?` misses, `Proxy` fetches, pseudo-methods,
//! `Attribute` metamethods, `take`, the `.^` lanes, scalar and array
//! receivers, the constructor lane). On a planned call that probing cost
//! ~1.2k instructions per call, a sixth of the whole call.
//!
//! None of those lanes can answer an ordinary instance receiver under a key
//! the plain-method lane holds, except two:
//!
//! - the lanes keyed on a receiver *kind* an instance is not (a `Proxy`,
//!   `ParametricRole`, `Sub`, lazy list, scalar, array or type object);
//! - the lanes keyed on a method *name* this front refuses (`new`, `take`,
//!   `WHAT`, `HOW`, `VAR`) or on a class it refuses (`Attribute`, the BOOT
//!   object classes), and on a modifier or quoted name, which it also refuses.
//!
//! The two lanes with a memo of their own are the accessor lane and the
//! constructor lane. They could answer a key the plain-method lane also holds.
//! The front tries the accessor lane first, as the old order did, and refuses
//! `new` (the constructor lane's only name). Every other lane's verdict is a
//! function of the receiver kind, the name, the modifier and the argument
//! shape. Those are part of the refusal test above, or of the key (argument
//! types, `want_ref`, the `^find_method`/wrap guards inside the early lane), so
//! the full path reaches the same lane for the same call.
//!
//! When the front tried and missed, `exec_call_method_mut_op_impl` skips its
//! own accessor and early-lane probes, so a miss costs no more than before.

use super::vm_call_method_ops::MethodName;
use super::*;

/// What [`Interpreter::try_instance_method_front`] did.
pub(super) enum InstanceFront {
    /// The call was answered.
    Done(Result<(), RuntimeError>),
    /// The receiver qualified and both lanes refused: the later accessor and
    /// early-lane probes would refuse too.
    Missed,
    /// The receiver did not qualify; nothing was probed.
    NotTried,
}

impl Interpreter {
    /// Answer a planned or lane-proven method call on an ordinary instance
    /// before the rest of `CallMethodMut`'s probes (see the module docs).
    // Cost: O(1) for a refusal (a tag probe and a few symbol compares);
    // otherwise O(a), a = arguments, as `try_plain_method_early_lane`.
    pub(super) fn try_instance_method_front(
        &mut self,
        code: &CompiledCode,
        name: &MethodName<'_>,
        arity: u32,
        target_name_idx: u32,
        modifier_idx: Option<u32>,
        arg_sources_idx: Option<u32>,
    ) -> InstanceFront {
        // The receiver's tag first: every other receiver is refused by one
        // probe, so this front costs the calls it cannot serve next to nothing.
        let Some(target_idx) = self.stack.len().checked_sub(arity as usize + 1) else {
            return InstanceFront::NotTried;
        };
        if !self.stack[target_idx].is_instance_value()
            || modifier_idx.is_some()
            || name.quoted
            || crate::sym_in!(name.sym, "new" | "take" | "WHAT" | "HOW" | "VAR")
        {
            return InstanceFront::NotTried;
        }
        let ValueView::Instance { class_name, .. } = self.stack[target_idx].view() else {
            return InstanceFront::NotTried;
        };
        if class_name == crate::sym!("Attribute")
            || crate::runtime::boot_collection::is_boot_collection_invocant(&self.stack[target_idx])
        {
            return InstanceFront::NotTried;
        }
        // The accessor lane, under the same conditions as in
        // `exec_call_method_mut_op_impl`.
        if arity == 0
            && !self.accessor_ref_pending
            && !crate::runtime::find_method_intercept::any_user_find_method()
            && let Some(val) = self.try_accessor_lane(name.sym)
        {
            crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "accessor");
            self.stack.pop();
            self.stack.push(val);
            return InstanceFront::Done(Ok(()));
        }
        match self.try_plain_method_early_lane(
            code,
            Self::const_str(code, target_name_idx),
            name.raw,
            name.sym,
            arity as usize,
            arg_sources_idx,
        ) {
            Some(result) => InstanceFront::Done(result),
            None => InstanceFront::Missed,
        }
    }
}
