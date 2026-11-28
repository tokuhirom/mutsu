//! A loop kernel over a materialized list of plain `Int`s (ADR-19288 D5).
//!
//! `for %h.values { $sum += $_ }` hands the general body driver
//! (`exec_for_loop_body_from`) a list it already holds. When the loop reads
//! its topic only through its own slot (`ForLoopSpec::topic_slot_only`) and
//! every item is a plain `Int`, the body is the same closed `Int`/`Num`
//! region a counted loop's kernel runs, with the loop variable read from the
//! list instead of the counter: `LoopShape::Range { list: true, .. }`.
//!
//! The kernel runs before the driver binds anything. On a bail it hands back
//! the index of the iteration it could not finish, and the driver starts
//! there, so the generic path reruns that iteration from its beginning with
//! the state the kernel left at the end of the one before it.

use super::vm_control_ops::ForLoopSpec;
use super::vm_loop_kernel::LoopOutcome;
use super::vm_loop_kernel_types::{LoopShape, LoopVar};
use super::*;

/// Lists shorter than this are not worth a kernel entry: the entry reads and
/// writes back every carried slot.
const MIN_LIST_ITEMS: usize = 16;

impl Interpreter {
    /// Run the loop at `body_start - 1` over `items` natively, when it is a
    /// plain implicit-topic loop with its own topic slot and every item is a
    /// plain `Int`. `Ok(None)`: declined, nothing ran. `Ok(Some(k))`: items
    /// before `k` are done (`k == items.len()` when the loop finished).
    // Cost: O(n) to check and copy the items, n = items, plus the native run.
    pub(super) fn try_list_loop_kernel(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        items: &[Value],
        body_start: usize,
        loop_end: usize,
        compiled_fns: &CompiledFns,
    ) -> Result<Option<usize>, RuntimeError> {
        let Some(slot) = spec.topic_local.filter(|_| spec.topic_slot_only) else {
            return Ok(None);
        };
        if items.len() < MIN_LIST_ITEMS
            || !list_kernel_spec(spec)
            || self.async_state.gather_for_loop_resume.is_some()
            || self.async_state.gather_resume_body_ip.is_some()
        {
            return Ok(None);
        }
        let mut words = Vec::with_capacity(items.len());
        for item in items {
            match item.view() {
                ValueView::Int(n) => words.push(n),
                _ => return Ok(None),
            }
        }
        let Ok(len) = i64::try_from(words.len()) else {
            return Ok(None);
        };
        let shape = LoopShape::Range {
            body: (body_start, loop_end),
            var: LoopVar::Slot(slot),
            list: true,
        };
        Ok(
            match self.try_loop_kernel(
                code,
                body_start - 1,
                shape,
                Some((0, len, false)),
                true,
                compiled_fns,
                Some(&words),
            )? {
                LoopOutcome::Declined => None,
                LoopOutcome::Done => Some(words.len()),
                LoopOutcome::Bailed(at) => Some(usize::try_from(at).unwrap_or(0)),
            },
        )
    }
}

/// A one-item-per-iteration implicit-topic loop with nothing the driver must
/// do per item beyond binding the topic: no collection, threads, writeback,
/// rw or typed binding, chunking, block callable or routine declarations.
// Cost: O(1).
fn list_kernel_spec(spec: &ForLoopSpec) -> bool {
    spec.param_idx.is_none()
        && spec.param_local.is_none()
        && !(spec.collect
            || spec.threaded
            || spec.is_rw
            || spec.kv_mode
            || spec.arity > 1
            || spec.chunk_local.is_some()
            || spec.block_callable_local.is_some()
            || spec.body_declares_routines
            || spec.param_type_constraint.is_some()
            || !spec.multi_param_names.is_empty()
            || spec.autothread_junctions
            || spec.param_sigilless
            || spec.param_raw
            || spec.do_writeback
            || spec.loop_var_wraps_element)
}
