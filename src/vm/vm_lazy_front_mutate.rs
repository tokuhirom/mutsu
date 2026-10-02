//! Front mutation (`shift` / `unshift` / `prepend` / `splice`) of a lazy
//! `@`-array backed by one of the infinite generator shapes: a
//! [`SequenceSpec`](crate::value::SequenceSpec) LazyList (`my @a = 1..*`,
//! `my @a = 1, 2, 4 ... *`, `.roll(*)`), an endpoint-less closure sequence
//! (`my @a = 1, 1, * + * ... *`) or a triangle reduce (`my @a = [\+] 1..*`).
//!
//! None of them can be strictly forced: the strict force throws
//! `X::Cannot::Lazy` (#10846, #10861). A front mutation only touches a bounded
//! prefix, though, and Rakudo keeps the array lazy across it (`my @a = 1..*;
//! @a.shift; @a[^3]` is `(2 3 4)`, and `@a.is-lazy` stays `True`). So the
//! mutation runs in three steps:
//!
//! 1. [`Interpreter::lazy_seq_front_mutation_prepare`] works out how many leading
//!    elements the call touches (`k`), reifies exactly those as a real Array and
//!    swaps it in for the receiver, both on the operand stack and in the env.
//! 2. The ordinary Array method runs on that finite prefix, unchanged.
//! 3. [`Interpreter::lazy_seq_front_mutation_finish`] stitches the mutated prefix back in
//!    front of the untouched tail, as a fresh LazyList that keeps the live
//!    generator, and installs it over the temporary Array.
//!
//! The cache always becomes `prefix ++ old[k..]`. What happens to the
//! generator's own state depends on the shape:
//!
//! - A sequence spec keeps `cache` and `generation_state` the same length, as
//!   `extend_sequence_cache` requires: both are rewritten. It computes its next
//!   element from the last generated one only, and step 1 generates at least
//!   one element past `k`, so the generator's last value is never part of the
//!   rewritten prefix.
//! - A closure sequence's generator may read any amount of its trailing
//!   history (a Fibonacci generator reads two elements, a slurpy one all of
//!   them), so its `generation_state` is left untouched: as in Rakudo, the
//!   sequence never sees the array it feeds. `extend_closure_sequence` only
//!   needs the cache and the history to end at the same generator frontier,
//!   which the stitch preserves.
//! - A triangle reduce keeps its accumulator and source position in its
//!   `ScanSpec`, which the stitch does not touch either; `force_scan_lazy_list`
//!   walks the source from that position, not from the cache length.

use super::*;

/// A front mutation in flight: the source list and the prefix length `k` the
/// mutation was handed as a real Array.
pub(super) struct LazySeqFrontMutation {
    source: crate::gc::Gc<LazyList>,
    prefix_len: usize,
    returns_self: bool,
}

impl Interpreter {
    /// Step 1 (see the module docs). `None` when the call is not a front
    /// mutation of an infinite-generator lazy `@`-array; `Some(Err)` when it is one
    /// that would have to reach the (non-existent) end of the list.
    ///
    /// Cost: O(k) generator steps, k = the number of leading elements the call
    /// touches.
    pub(super) fn lazy_seq_front_mutation_prepare(
        &mut self,
        code: &CompiledCode,
        method: &str,
        arity: u32,
        target_name: &str,
    ) -> Option<Result<LazySeqFrontMutation, RuntimeError>> {
        if !matches!(method, "shift" | "unshift" | "prepend" | "splice")
            || !target_name.starts_with('@')
        {
            return None;
        }
        let target_idx = self.stack.len().checked_sub(arity as usize + 1)?;
        let ValueView::LazyList(ll) = self.stack[target_idx].view() else {
            return None;
        };
        let ll = ll.clone();
        let unbounded_closure_seq = ll
            .closure_seq
            .as_ref()
            .is_some_and(|state| state.lock().unwrap().endpoint.is_none());
        if !ll.in_array_context()
            || !(ll.sequence_spec.is_some() || unbounded_closure_seq || ll.scan_spec.is_some())
        {
            return None;
        }
        let args = &self.stack[target_idx + 1..];
        let prefix_len: usize = match method {
            "shift" if args.is_empty() => 1,
            "unshift" | "prepend" => 0,
            "splice" => match Self::lazy_splice_extent(args) {
                Some(k) => k,
                None => {
                    return Some(Err(RuntimeError::cannot_lazy_with_action(
                        "splice", "Array",
                    )));
                }
            },
            // A wrong-arity call: leave the arity error to the ordinary path.
            _ => return None,
        };
        // One element past the prefix, so the generator's last value stays in
        // the untouched tail (see the module docs).
        let reified = match ll.sequence_spec.as_ref() {
            Some(spec) => Self::extend_sequence_cache(&ll, spec, prefix_len.saturating_add(1)),
            None if unbounded_closure_seq => {
                self.extend_closure_sequence(&ll, prefix_len.saturating_add(1))
            }
            None => self.force_scan_lazy_list(&ll, prefix_len.saturating_add(1)),
        };
        let mut items = match reified {
            Ok(items) => items,
            Err(e) => return Some(Err(e)),
        };
        items.truncate(prefix_len);
        let prefix = Value::real_array(items);
        self.stack[target_idx] = prefix.clone();
        self.env_mut()
            .insert(target_name.to_string(), prefix.clone());
        self.locals_set_by_name(code, target_name, prefix);
        Some(Ok(LazySeqFrontMutation {
            source: ll,
            prefix_len,
            returns_self: matches!(method, "unshift" | "prepend"),
        }))
    }

    /// The prefix length `splice(offset, length, ...)` touches, or `None` when
    /// the call reaches the end of the list (no length, or an offset/length
    /// counted from the end).
    ///
    /// Cost: O(1).
    fn lazy_splice_extent(args: &[Value]) -> Option<usize> {
        let non_negative = |v: &Value| match v.view() {
            ValueView::Int(i) if i >= 0 => Some(i as usize),
            _ => None,
        };
        let offset = non_negative(args.first()?)?;
        let length = non_negative(args.get(1)?)?;
        offset.checked_add(length)
    }

    /// Step 3 (see the module docs): rebuild the lazy array from the mutated
    /// prefix the Array method left in `target_name` and the source's tail.
    ///
    /// Cost: O(c), c = elements the source has cached so far.
    pub(super) fn lazy_seq_front_mutation_finish(
        &mut self,
        code: &CompiledCode,
        target_name: &str,
        pending: LazySeqFrontMutation,
    ) {
        let Some(current) = self.env().get(target_name).cloned() else {
            return;
        };
        let ValueView::Array(prefix, crate::value::ArrayKind::Array) = current.view() else {
            return;
        };
        let prefix = prefix.to_vec();
        let rebuilt = LazyList::clone(&pending.source);
        let k = pending.prefix_len;
        let restitch = |slot: &mut Option<Vec<Value>>| {
            let old = slot.take().unwrap_or_default();
            let mut next = prefix.clone();
            next.extend(old.into_iter().skip(k));
            *slot = Some(next);
        };
        restitch(&mut rebuilt.cache.lock().unwrap());
        // Only a sequence spec reads its history in lockstep with the cache
        // (see the module docs).
        if rebuilt.sequence_spec.is_some() {
            restitch(&mut rebuilt.generation_state.lock().unwrap());
        }
        let restored = Value::lazy_list(crate::gc::Gc::new(rebuilt));
        self.env_mut()
            .insert(target_name.to_string(), restored.clone());
        self.locals_set_by_name(code, target_name, restored.clone());
        // `unshift` / `prepend` answer the array itself, which is now the lazy
        // list rather than the temporary prefix.
        if pending.returns_self
            && let Some(top) = self.stack.last_mut()
        {
            *top = restored;
        }
    }
}
