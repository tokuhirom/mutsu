//! A method-call site whose plan carries a method kernel runs the kernel
//! straight off the operand stack (#18267, O2 call plan).
//!
//! A site-cache plan hit (`run_method_site_ic_plan`) prepares a call for the
//! frame it may need: it decodes the call site's argument sources, moves the
//! arguments into a `Vec`, records the sources as pending for the binder,
//! enters the method-call depth and runs `run_method_call_plan`, which then
//! re-checks the plan's per-call gates before it tries the kernel. On
//! `method-call` that wrapping cost ~1.2k instructions around a ~2k kernel.
//!
//! A kernel needs none of it: it binds nothing, reads no argument source and
//! cannot call back into user code. So this lane checks the same gates the
//! plan replay checks before its kernel, in front of the preparation, and
//! runs the kernel on the arguments where they lie.
//!
//! # Why the lane answers exactly what the replay would
//!
//! The lane runs only where `run_method_call_plan` would have reached its
//! kernel with the same arguments:
//!
//! * the plan is lean and has neither a container parameter, a `*%h` nor a
//!   private method, so the replay's per-call checks before the kernel are
//!   `has_alias_meta`, `slot_frame_entry_ok` and `lean_args_ok`, all re-asked
//!   here;
//! * every argument is a plain instance of exactly the class its plan key
//!   names (the replay's own key comparison for such arguments), so no
//!   `VarRef`, no spread and no value that needs hllizing reaches it, and the
//!   argument-source decode the replay makes has nothing to say about them.
//!
//! A kernel bail leaves the stack untouched, and the call takes the ordinary
//! replay, which tries the kernel again before its frame: a bail costs one
//! more kernel attempt, never a different answer. On success the lane does
//! the replay's exit bookkeeping: the pending argument sources, the rw
//! writeback list and the constraint-recheck flag are cleared exactly as the
//! kernel arm and the site's exit clear them.

use super::*;

/// The most arguments the lane copies off the stack.
const KERNEL_LANE_MAX_ARGS: usize = 4;

impl Interpreter {
    /// Run `plan`'s kernel for the call at `code.ops[ip]` (whose arity is
    /// `arity` and whose receiver class matched the site entry). `None`:
    /// the lane does not apply or the kernel bailed, and nothing changed.
    // Cost: O(a + k), a = arguments, k = kernel ops.
    pub(super) fn try_method_site_ic_kernel(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        arity: usize,
        arg_keys: &[Symbol],
        plan: &super::vm_method_call_plan::MethodCallPlan,
    ) -> Option<()> {
        let (Some(kernel), Some(body)) = (&plan.kernel, plan.slot_body.as_deref()) else {
            return None;
        };
        if !plan.lean
            || plan.private
            || plan.container_params
            || plan.named_slurpy
            || arity > KERNEL_LANE_MAX_ARGS
            || arg_keys.len() != arity
        {
            return None;
        }
        let target_idx = self.stack.len().checked_sub(arity + 1)?;
        // The arguments where they lie: each a plain instance of exactly the
        // class its key names.
        let mut buf: [Value; KERNEL_LANE_MAX_ARGS] = std::array::from_fn(|_| Value::NIL);
        for ((arg, key), slot) in self.stack[target_idx + 1..]
            .iter()
            .zip(arg_keys)
            .zip(buf.iter_mut())
        {
            if !(arg.is_instance_value()
                && matches!(arg.view(), ValueView::Instance { class_name, .. } if class_name == *key))
            {
                return None;
            }
            *slot = arg.clone();
        }
        let args = &buf[..arity];
        let target = self.stack[target_idx].clone();
        let ValueView::Instance { attributes, .. } = target.view() else {
            return None;
        };
        if super::vm_method_call_attrs::CallAttrs::Cell(&attributes).has_alias_meta()
            || !self.slot_frame_entry_ok()
            || !self.lean_args_ok(&plan.def, args)
        {
            return None;
        }
        let value = self.try_method_kernel(kernel, body, &target, args)?;
        self.sync_source_line(code, ip);
        drop(buf);
        self.stack.truncate(target_idx);
        self.stack.push(value);
        // The site's entry effects (`run_method_site_ic_plan`), the kernel
        // arm's (`run_method_call_plan`) and the site's exit.
        self.caches.ctor_lane_candidate = None;
        self.caches.plain_method_lane_candidate = None;
        self.discard_pending_call_arg_sources();
        self.pending_call_arg_source_slots.clear();
        self.pending_skip_constraint_recheck = false;
        self.pending_rw_writeback_sources.clear();
        self.apply_pending_rw_writeback(code);
        self.drain_pending_local_updates_after_call(code);
        Some(())
    }
}
