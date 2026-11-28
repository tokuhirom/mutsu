//! A loop kernel over a materialized list (ADR-19288 D5).
//!
//! `for %h.values { $sum += $_ }` and `for $line.split(' ') -> $w { %c{$w}++ }`
//! hand the general body driver (`exec_for_loop_body_from`) a list it already
//! holds. When the loop reads its variable only through its own slot (the
//! topic's, `ForLoopSpec::topic_slot_only`, or one named parameter's,
//! `param_slot_only`), the body can run as the same closed region a counted
//! loop's kernel runs, with the counter indexing the list
//! (`LoopShape::Range { list, .. }`):
//!
//! - every item a plain `Int` (`ListKind::Int`): the items are copied out as
//!   machine words and the loop variable is the `Int` at the counter;
//! - every item a plain `Str` (`ListKind::Str`): the items are read in place,
//!   and the loop variable is only the item's index, which only a hash
//!   element's `++`/`--` consumes (its helper reads the item as the key).
//!
//! The kernel runs before the driver binds anything. On a bail it hands back
//! the index of the iteration it could not finish, and the driver starts
//! there, so the generic path reruns that iteration from its beginning with
//! the state the kernel left at the end of the one before it.

use super::vm_control_ops::ForLoopSpec;
use super::vm_loop_kernel::LoopOutcome;
use super::vm_loop_kernel_types::{ListKind, LoopShape, LoopVar};
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
        // The loop variable's own slot: the topic's (`topic_slot_only`) or a
        // single named parameter's (`param_slot_only`).
        let slot = if spec.param_idx.is_none() {
            spec.topic_local.filter(|_| spec.topic_slot_only)
        } else {
            spec.param_local.filter(|_| spec.param_slot_only)
        };
        let Some(slot) = slot else {
            return Ok(None);
        };
        if items.len() < MIN_LIST_ITEMS
            || !list_kernel_spec(spec)
            || self.async_state.gather_for_loop_resume.is_some()
            || self.async_state.gather_resume_body_ip.is_some()
        {
            return Ok(None);
        }
        // Every item a plain `Int` (copied out as machine words), or every
        // item a plain `Str` (read in place by the hash helpers).
        let mut words = Vec::new();
        let kind = match items.first().map(Value::view) {
            Some(ValueView::Int(_)) => {
                words.reserve_exact(items.len());
                for item in items {
                    match item.view() {
                        ValueView::Int(n) => words.push(n),
                        _ => return Ok(None),
                    }
                }
                ListKind::Int
            }
            Some(ValueView::Str(_)) => {
                if !items.iter().all(Value::is_str_value) {
                    return Ok(None);
                }
                ListKind::Str
            }
            _ => return Ok(None),
        };
        let Ok(len) = i64::try_from(items.len()) else {
            return Ok(None);
        };
        let shape = LoopShape::Range {
            body: (body_start, loop_end),
            var: LoopVar::Slot(slot),
            list: kind,
        };
        let (list, str_items) = match kind {
            ListKind::Int => (Some(words.as_slice()), None),
            _ => (None, Some(items)),
        };
        Ok(
            match self.try_loop_kernel(
                code,
                body_start - 1,
                shape,
                Some(super::vm_loop_kernel::KernelRange {
                    start: 0,
                    end: len,
                    inclusive: false,
                    list,
                    items: str_items,
                }),
                true,
                compiled_fns,
            )? {
                LoopOutcome::Declined => None,
                LoopOutcome::Done => Some(items.len()),
                LoopOutcome::Bailed(at) => Some(usize::try_from(at).unwrap_or(0)),
            },
        )
    }
}

/// A one-item-per-iteration loop over its implicit topic or one slot-only
/// named parameter, with nothing the driver must do per item beyond binding
/// it: no collection, threads, writeback,
/// rw or typed binding, chunking, block callable or routine declarations.
// Cost: O(1).
fn list_kernel_spec(spec: &ForLoopSpec) -> bool {
    // An implicit topic, or a named parameter bound in its slot only.
    (spec.param_idx.is_none() && spec.param_local.is_none() || spec.param_slot_only)
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
