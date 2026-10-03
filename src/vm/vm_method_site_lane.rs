//! ADR-11276 §2.4/§2.5: the method-table lane in front of `CallMethodMut`.
//!
//! A built-in method call on a variable (`@a.elems`, `$r.numerator`) compiles
//! to `CallMethodMut`, whose full path runs a chain of probes before it ever
//! reaches the native dispatch: accessor and constructor lanes, user
//! `find_method`, Proxy and lazy-Seq handling, Failure explosion, the lever-A
//! augment gate, the receiver writeback around the call. For a plain receiver
//! of a [`DispatchShape`](crate::value::DispatchShape) whose method has a row
//! in the built-in method table, every one of those probes answers "no", and
//! the row answers the call. Measured before this lane (ADR-11276 §9), the
//! probes were ~4,700 of the ~6,300 instructions an `@a.elems` cost.
//!
//! # The guard
//!
//! The lane answers only when all of these hold, and otherwise leaves the call
//! to the full path untouched:
//!
//! - the site is static-eligible: no arguments (every row so far takes none),
//!   no `.^`/`.!` modifier, not a quoted name, no argument sources, and the
//!   receiver is not an `@!`/`%!` attribute (whose writeback runs around the
//!   call);
//! - the method name is none the full path inspects by name before its native
//!   probe (`scalar_early_lane_skips`, `dispatch_branches_before_native_probe`);
//! - no accessor-ref marker is pending, no user `find_method` exists anywhere,
//!   and no writeback is pending (the full path would drain it at this call);
//! - the receiver has a `DispatchShape` and the table has a row for
//!   `(shape, method)` at arity 0;
//! - user code has not `augment`ed the receiver's type with the method
//!   (`native_lever_a_user_override_sym`, the same gate the full path applies).
//!
//! # The inline cache
//!
//! The last two checks are what cost: a hash lookup and the registry probe.
//! Both depend only on the receiver's shape, the method and the registry, so
//! the site's memo (`CompiledCode::method_sites`, keyed by the method name's
//! constant) remembers `(shape, row)` for one registry write generation. A hit
//! is one lock, a generation compare and a shape compare, then the handler.
//! The memo is never filled while a `native_base_bypass` is active, because
//! that makes the augment gate answer "no" for the duration of one deferral.
//!
//! # The maintenance net
//!
//! In debug builds the lane's answer is not used: the full path runs as before
//! and its answer must agree with the lane's (`check_method_site_lane`), so
//! CI's `debug-tap` job checks every lane hit over the whole TAP suite. A probe
//! added to the full path later that claims one of these calls fails there.

use super::*;
use crate::builtins::method_table::{self, RowId};

/// A lane answer: the row's result, and the row it came from.
pub(super) struct SiteLaneAnswer {
    result: Result<Value, RuntimeError>,
    row: RowId,
}

impl Interpreter {
    /// The answer the method table gives the `CallMethodMut` at `code.ops[ip]`,
    /// or `None` to run the full path. Leaves the stack untouched.
    // Cost: O(1) on a memo hit (one lock and a few compares) plus the row's
    // handler; a miss adds one table lookup and the memoized augment probe.
    pub(super) fn try_method_site_lane(
        &mut self,
        code: &CompiledCode,
        ip: usize,
    ) -> Option<SiteLaneAnswer> {
        let OpCode::CallMethodMut {
            name_idx,
            arity: 0,
            target_name_idx,
            modifier_idx: None,
            quoted: false,
            arg_sources_idx: None,
        } = code.ops[ip]
        else {
            return None;
        };
        if self.accessor_ref_pending
            || !self.pending_rw_writeback_sources.is_empty()
            || !self.pending_caller_var_writeback.is_empty()
            || !self.pending_local_updates.is_empty()
            || crate::runtime::find_method_intercept::any_user_find_method()
        {
            return None;
        }
        // Most calls name a method no row has. A bit test answers those
        // without taking the memo's lock, which an `Int` receiver (it has a
        // shape) would otherwise pay on every such call.
        if !method_table::names_a_row(code.const_sym(name_idx)) {
            return None;
        }
        let shape = self.stack.last()?.dispatch_shape()?;
        let sites = code.constants.len();
        let idx = name_idx as usize;
        let generation = self.registry_write_generation();
        let row = match code.method_sites.cached(sites, idx, generation) {
            Some(payload) if payload_shape(payload) == shape as u8 => {
                if Self::is_array_hash_attr_twigil(Self::const_str(code, target_name_idx)) {
                    return None;
                }
                RowId::from_bits(payload as u16)
            }
            _ => {
                let row = self.resolve_method_site_lane(code, name_idx, target_name_idx, shape)?;
                if self.native_base_bypass.is_none() {
                    code.method_sites
                        .remember(sites, idx, generation, pack(shape, row));
                }
                row
            }
        };
        let target = self.stack.last()?;
        Some(SiteLaneAnswer {
            result: method_table::invoke(row, target, &[]),
            row,
        })
    }

    /// The memo-miss half of [`Self::try_method_site_lane`]: every check whose
    /// answer the memo remembers.
    // Cost: O(1) amortized: a name match, one table lookup and the memoized
    // augment probe.
    fn resolve_method_site_lane(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        target_name_idx: u32,
        shape: crate::value::DispatchShape,
    ) -> Option<RowId> {
        let method = Self::const_str(code, name_idx);
        if Self::scalar_early_lane_skips(method)
            || Self::dispatch_branches_before_native_probe(method)
            || Self::is_array_hash_attr_twigil(Self::const_str(code, target_name_idx))
        {
            return None;
        }
        let method_sym = code.const_sym(name_idx);
        let row = method_table::resolve(shape, method_sym, 0)?;
        let target = self.stack.last()?.clone();
        if self.native_lever_a_user_override_sym(&target, method_sym) {
            return None;
        }
        Some(row)
    }

    /// Complete the `CallMethodMut` at `code.ops[ip]` with the lane's answer:
    /// the bookkeeping the full path performs for a native answer to a call
    /// with no arguments, then the result in place of the receiver.
    // Cost: O(1).
    // Debug builds keep the full path's answer instead (see the module docs).
    #[cfg_attr(debug_assertions, allow(dead_code))]
    pub(super) fn finish_method_site_lane(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        answer: SiteLaneAnswer,
    ) -> Result<(), RuntimeError> {
        crate::vm::vm_stats::record_method_dispatch();
        self.set_pending_call_arg_sources(None);
        self.pending_call_arg_source_slots.clear();
        self.caches.ctor_lane_candidate = None;
        self.method_dispatch_pure = true;
        if crate::vm::vm_stats::enabled() {
            self.record_method_site_lane_stats(answer.row);
        }
        match self.settle_native_warning(answer.result) {
            Ok(value) => {
                self.stack.pop();
                self.stack.push(value);
                Ok(())
            }
            Err(e) => {
                self.sync_source_line(code, ip);
                self.record_call_resume_point(code, ip, &e);
                Err(e)
            }
        }
    }

    /// The `MUTSU_VM_STATS` counters the full path's native completion records.
    #[cold]
    #[cfg_attr(debug_assertions, allow(dead_code))]
    fn record_method_site_lane_stats(&mut self, row: RowId) {
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethodmut", "native");
        let Some(target) = self.stack.last().cloned() else {
            return;
        };
        let name = method_table::row(row).name;
        self.record_native_row_coverage("vm_method_site_lane", &target, name, 0);
    }

    /// Debug builds: run the full path for the `CallMethodMut` at
    /// `code.ops[ip]` and assert it reaches the lane's answer (see the module
    /// docs). The full path's outcome is the one kept.
    #[cfg(debug_assertions)]
    pub(super) fn check_method_site_lane(
        &mut self,
        code: &CompiledCode,
        ip: usize,
        answer: SiteLaneAnswer,
        full: Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let render = |r: Result<&Value, &RuntimeError>| match r {
            Ok(v) => format!("ok:{}", crate::runtime::gist_value(v)),
            Err(e) => format!("err:{}", e.message),
        };
        let lane = render(answer.result.as_ref());
        let full_rendered = match &full {
            Ok(()) => self
                .stack
                .last()
                .map_or_else(|| "<empty stack>".to_string(), |v| render(Ok(v))),
            Err(e) => render(Err(e)),
        };
        debug_assert_eq!(
            lane,
            full_rendered,
            "the method-table lane disagrees with the full CallMethodMut path for .{} \
             (row owner {}) at ip {ip} -- a probe the lane skips now claims this call",
            method_table::row(answer.row).name,
            method_table::row(answer.row).owner,
        );
        let _ = code;
        full
    }
}

/// The memo payload for `row` on a receiver of `shape`.
fn pack(shape: crate::value::DispatchShape, row: RowId) -> u32 {
    (u32::from(shape as u8) << 16) | u32::from(row.to_bits())
}

/// The receiver shape a memo payload was filled for.
fn payload_shape(payload: u32) -> u8 {
    (payload >> 16) as u8
}
