//! The Iterator protocol lane in front of `CallMethodMut` (#17337).
//!
//! `$it.pull-one` on one of mutsu's built-in iterators is answered by a row
//! of the `Iterator` / `PredictiveIterator` roles found by its owner
//! (`try_native_iterator`, ADR-11276 §9.126). Those rows are `OWNER_ONLY`, so
//! the method-table lane (`vm_method_site_lane`) never reaches them and every
//! call ran the full `CallMethodMut` path first: ~14,000 instructions of
//! probes (accessor and constructor lanes, proto bodies, the user-shadow
//! check, sequence delegation, Failure explosion, ...) in front of a row that
//! costs ~4,500. A user iterator that delegates to a built-in one
//! (`$!source.pull-one`, ParaSeq's `BufferIterator`) pays it per element.
//!
//! # The guard
//!
//! This lane answers a call only when every one of these holds, and otherwise
//! leaves it to the full path untouched:
//!
//! - the site has no argument, no `.^`/`.!` modifier and is not quoted, and
//!   the receiver is not an `@!`/`%!` attribute;
//! - no accessor-ref marker or rw writeback is pending (the retain-on-miss
//!   caller-var writebacks are drained after the answer, as the full path
//!   drains them), no user `find_method`
//!   exists, no method is wrapped and no deferral bypass is active;
//! - the receiver is a built-in iterator instance (its `DispatchShape` is
//!   `Iterator` or `PredictiveIterator`), and one of that shape's owners has an
//!   `OWNER_ONLY` row for the method with no argument;
//! - the method is none the full path inspects by name before its native
//!   probe, and no user method, accessor, `augment` or `handles`/`FALLBACK`
//!   shadows it (`user_shadows_builtin`, `native_lever_a_user_override_sym`:
//!   the checks the full path itself makes in front of `try_native_iterator`).
//!
//! The last two checks depend only on the method, the receiver's shape and the
//! registry, so the site's memo (`CompiledCode::method_sites`) keeps the
//! verdict for one registry write generation, as the method-table lane does.
//!
//! # The maintenance net
//!
//! The row advances the iterator, so the answer cannot be computed twice and
//! compared. Debug builds instead run only the full path and assert that it was
//! answered by `try_native_iterator`: a probe added to the full path later that
//! claims one of these calls fails CI's `debug-tap` job.

use super::vm_method_site_lane::{SiteLaneAnswer, pack, payload_matches, payload_row};
use super::*;
use crate::builtins::method_table::{self, Receiver, RowFlags, RowId};

#[cfg(debug_assertions)]
thread_local! {
    /// Debug builds: how many calls `try_native_iterator` has answered on this
    /// thread (see the module docs).
    static NATIVE_ITERATOR_ANSWERS: std::cell::Cell<u64> = const { std::cell::Cell::new(0) };
}

/// Debug builds: count one call `try_native_iterator` answered.
// Cost: O(1).
#[cfg(debug_assertions)]
pub(super) fn note_native_iterator_answer() {
    NATIVE_ITERATOR_ANSWERS.with(|n| n.set(n.get() + 1));
}

impl Interpreter {
    /// The `OWNER_ONLY` Iterator-protocol row that answers the
    /// `CallMethodMut` at `code.ops[ip]`, or `None` to run the full path.
    /// Leaves the stack untouched.
    // Cost: O(1) on a memo hit (one lock and a few compares); a miss adds an
    // owner-row lookup and the user-shadow probes.
    pub(super) fn try_iterator_site_lane(
        &mut self,
        code: &CompiledCode,
        ip: usize,
    ) -> Option<RowId> {
        let OpCode::CallMethodMut {
            name_idx,
            arity: 0,
            target_name_idx,
            modifier_idx: None,
            quoted: false,
            ..
        } = code.ops[ip]
        else {
            return None;
        };
        if self.accessor_ref_pending
            || !self.pending_rw_writeback_sources.is_empty()
            || !self.pending_local_updates.is_empty()
            || self.dispatch.native_base_bypass.is_some()
            || crate::runtime::find_method_intercept::any_user_find_method()
            || self.has_any_wrap_chains()
        {
            return None;
        }
        let target = self.stack.last()?;
        let shape = target.receiver_shape()?;
        if !matches!(
            shape,
            crate::value::DispatchShape::Iterator | crate::value::DispatchShape::PredictiveIterator
        ) {
            return None;
        }
        let receiver = Receiver::instance(shape);
        let sites = code.constants.len();
        let idx = name_idx as usize;
        let generation = self.registry_write_generation();
        if let Some(payload) = code.method_sites.cached(sites, idx, generation)
            && payload_matches(payload, receiver, 0)
        {
            return payload_row(payload);
        }
        if Self::is_array_hash_attr_twigil(Self::const_str(code, target_name_idx)) {
            return None;
        }
        let row = self.resolve_iterator_site_lane(code, name_idx, shape);
        code.method_sites
            .remember(sites, idx, generation, pack(receiver, 0, row));
        row
    }

    /// The memo-miss half of [`Self::try_iterator_site_lane`].
    // Cost: O(o) owner-row lookups, o = the shape's owners, plus the
    // user-shadow probes (memoized per registry generation by the caller).
    fn resolve_iterator_site_lane(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        shape: crate::value::DispatchShape,
    ) -> Option<RowId> {
        let method = Self::const_str(code, name_idx);
        if Self::scalar_early_lane_skips(method)
            || Self::dispatch_branches_before_native_probe(method)
        {
            return None;
        }
        let method_sym = code.const_sym(name_idx);
        let row = shape.opaque_owners()?.iter().find_map(|owner| {
            method_table::owner_row(crate::symbol::Symbol::intern(owner), method_sym, 0)
        })?;
        if !method_table::row(row).flags.contains(RowFlags::OWNER_ONLY) {
            return None;
        }
        let target = self.stack.last()?.clone();
        if self.user_shadows_builtin(&target, method_sym, &[], false)
            || self.native_lever_a_user_override_sym(&target, method_sym)
        {
            return None;
        }
        Some(row)
    }

    /// Answer the `CallMethodMut` at `code.ops[ip]` with `row`, which
    /// [`Self::try_iterator_site_lane`] chose. `None` when the row's handler
    /// declines the receiver; the stack is then untouched.
    // Cost: O(1) plus the row's handler.
    // Debug builds run the full path instead (see the module docs).
    #[cfg_attr(debug_assertions, allow(dead_code))]
    pub(super) fn run_iterator_site_lane(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        row: RowId,
    ) -> Option<Result<(), RuntimeError>> {
        self.sync_source_line(code, ip);
        let target = self.stack.last()?.clone();
        let result = self.in_method_call(|vm| method_table::invoke_in(row, vm, &target, &[]))?;
        let answer = SiteLaneAnswer::interpreter_row(result, row, 0);
        Some(self.finish_method_site_lane(code, ip, answer))
    }

    /// Debug builds: run the full path for the `CallMethodMut` at
    /// `code.ops[ip]`, which the lane would have answered with `row`, and
    /// assert that `try_native_iterator` answered it (see the module docs).
    #[cfg(debug_assertions)]
    pub(super) fn check_iterator_site_lane(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        row: RowId,
    ) -> Result<(), RuntimeError> {
        let before = NATIVE_ITERATOR_ANSWERS.with(std::cell::Cell::get);
        let full = self.in_method_call(|vm| vm.exec_call_method_mut_site_inner(code, ip));
        let after = NATIVE_ITERATOR_ANSWERS.with(std::cell::Cell::get);
        debug_assert!(
            full.is_err() || after > before,
            "the Iterator lane would answer .{} with row {} at ip {ip}, but the full \
             CallMethodMut path did not reach try_native_iterator -- a probe the lane \
             skips now claims this call",
            Self::const_str(
                code,
                match code.ops[ip] {
                    OpCode::CallMethodMut { name_idx, .. } => name_idx,
                    _ => 0,
                }
            ),
            method_table::row(row).owner,
        );
        full
    }
}
