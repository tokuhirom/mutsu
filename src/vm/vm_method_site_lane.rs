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
//! - the site is static-eligible: at most two arguments, all positional (its
//!   argument-source descriptor names no named argument and no `|` spread),
//!   no `.^`/`.!` modifier, not a quoted name, and the receiver is not an
//!   `@!`/`%!` attribute (whose writeback runs around the call);
//! - every argument is a plain scalar (`method_table::plain_args`): a
//!   `Junction` must autothread, a `Failure` may have to explode under
//!   `use fatal`, a lazy `Seq` must be reified, and the full path does each
//!   of those before its native probe;
//! - the method name is none the full path inspects by name before its native
//!   probe (`scalar_early_lane_skips`, `dispatch_branches_before_native_probe`);
//! - no accessor-ref marker is pending, no user `find_method` exists anywhere,
//!   and no writeback is pending (the full path would drain it at this call);
//! - the receiver has a `DispatchShape` and the table has a row for
//!   `(shape, method, arity)`, whose handler binds these arguments (a
//!   `Handler::Narrow` row may decline them);
//! - user code has not `augment`ed the receiver's type with the method
//!   (`native_lever_a_user_override_sym`, the same gate the full path applies).
//!
//! # The inline cache
//!
//! The last two checks are what cost: a hash lookup and the registry probe.
//! Both depend only on the receiver's shape, the argument count, the method
//! and the registry, so the site's memo (`CompiledCode::method_sites`, keyed
//! by the method name's constant) remembers `(shape, arity, row)` for one
//! registry write generation. It remembers a miss the same way: a name with a
//! row for another shape or arity (`$i.chars` once `Str.chars` has a row,
//! `$s.index($n, $from)` beside the one-needle row) would otherwise repeat the
//! lookup on every call. A hit is one lock, a generation compare and a
//! shape-and-arity compare, then the handler.
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

/// A lane answer: the row's result, the row it came from, and the number of
/// arguments above the receiver on the stack.
pub(super) struct SiteLaneAnswer {
    result: Result<Value, RuntimeError>,
    row: RowId,
    arity: usize,
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
            arity,
            target_name_idx,
            modifier_idx: None,
            quoted: false,
            arg_sources_idx,
        } = code.ops[ip]
        else {
            return None;
        };
        let arity = arity as usize;
        if arity > MAX_LANE_ARITY {
            return None;
        }
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
        if !method_table::names_a_row(code.const_sym(name_idx), arity) {
            return None;
        }
        let base = self.stack.len().checked_sub(arity + 1)?;
        let shape = self.stack[base].dispatch_shape()?;
        if !method_table::shape_has_row(shape, code.const_sym(name_idx)) {
            return None;
        }
        let sites = code.constants.len();
        let idx = name_idx as usize;
        let generation = self.registry_write_generation();
        let row = match code.method_sites.cached(sites, idx, generation) {
            // The memo is keyed by the method name, which sites calling it
            // with another arity share, so the arity is part of the payload.
            Some(payload) if payload_matches(payload, shape, arity) => {
                let row = payload_row(payload)?;
                if Self::is_array_hash_attr_twigil(Self::const_str(code, target_name_idx)) {
                    return None;
                }
                row
            }
            _ => {
                let resolved =
                    self.resolve_method_site_lane(code, name_idx, target_name_idx, shape, arity);
                let row = match resolved {
                    Resolved::Row(row) => Some(row),
                    Resolved::Miss => None,
                    Resolved::Skip => return None,
                };
                if self.dispatch.native_base_bypass.is_none() {
                    code.method_sites
                        .remember(sites, idx, generation, pack(shape, arity, row));
                }
                row?
            }
        };
        // After the row is known: a call that misses (a remembered miss
        // above) never pays for the argument checks.
        if arity > 0
            && (!method_table::plain_args(&self.stack[base + 1..])
                || arg_sources_idx.is_some_and(|idx| !site_args_are_positional(code, idx)))
        {
            return None;
        }
        let (target, args) = self.stack[base..].split_first()?;
        Some(SiteLaneAnswer {
            result: method_table::invoke(row, target, args)?,
            row,
            arity,
        })
    }

    /// The memo-miss half of [`Self::try_method_site_lane`]: every check whose
    /// answer the memo remembers, and the one it cannot.
    // Cost: O(1) amortized: a name match, one table lookup and the memoized
    // augment probe.
    fn resolve_method_site_lane(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        target_name_idx: u32,
        shape: crate::value::DispatchShape,
        arity: usize,
    ) -> Resolved {
        // A property of this site's receiver, not of the method: the memo,
        // which every site naming the method shares, must not remember it.
        if Self::is_array_hash_attr_twigil(Self::const_str(code, target_name_idx)) {
            return Resolved::Skip;
        }
        let method = Self::const_str(code, name_idx);
        if Self::scalar_early_lane_skips(method)
            || Self::dispatch_branches_before_native_probe(method)
        {
            return Resolved::Miss;
        }
        let method_sym = code.const_sym(name_idx);
        let Some(row) = method_table::resolve(shape, method_sym, arity) else {
            return Resolved::Miss;
        };
        let Some(base) = self.stack.len().checked_sub(arity + 1) else {
            return Resolved::Skip;
        };
        let target = self.stack[base].clone();
        if self.native_lever_a_user_override_sym(&target, method_sym) {
            return Resolved::Miss;
        }
        Resolved::Row(row)
    }

    /// Complete the `CallMethodMut` at `code.ops[ip]` with the lane's answer:
    /// the bookkeeping the full path performs for a native answer to a call
    /// with no argument sources left pending, then the result in place of the
    /// receiver and its arguments.
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
        self.dispatch.method_dispatch_pure = true;
        if crate::vm::vm_stats::enabled() {
            self.record_method_site_lane_stats(answer.row);
        }
        // The one way the lane runs user code: settling a warning runs a
        // CONTROL handler inline.
        let warned = matches!(&answer.result, Err(e) if e.is_warn());
        match self.settle_native_warning(answer.result) {
            Ok(value) => {
                let base = self.stack.len() - answer.arity - 1;
                self.stack.truncate(base);
                self.stack.push(value);
                if warned {
                    // What the handler wrote to the caller's lexicals reaches
                    // their slots the way the full path's post-call drains put
                    // it there (`call_method_mut_site_around`); the entry guard
                    // (no pending writeback) says nothing about what the
                    // handler leaves behind. Without this, `$seen++` in
                    // `CONTROL { when CX::Warn { $seen++; .resume } }` was lost
                    // around a `@list.contains(...)` in release builds.
                    self.apply_pending_rw_writeback(code);
                    self.drain_pending_local_updates_after_call(code);
                }
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
        let arity = usize::from(method_table::row(row).arity);
        let Some(target) = self
            .stack
            .len()
            .checked_sub(arity + 1)
            .map(|base| self.stack[base].clone())
        else {
            return;
        };
        let name = method_table::row(row).name;
        self.record_native_row_coverage("vm_method_site_lane", &target, name, arity);
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
        // A row may answer with a resumable warning (`List.contains`, `.index`)
        // instead of a value: that is not an outcome yet, it settles at the
        // raise site (`settle_native_warning`, which `finish_method_site_lane`
        // and the full path both run). The full path has settled it by now, so
        // compare the value the warning resumes with. A CONTROL handler may
        // divert the settled call into an error; that is the handler's doing,
        // not a dispatch difference, and there is nothing to compare it with.
        let resumed = match &answer.result {
            Err(e) if e.is_warn() => e.return_value.as_ref(),
            _ => None,
        };
        let lane = resumed.map_or_else(|| render(answer.result.as_ref()), |v| render(Ok(v)));
        let full_rendered = match &full {
            Ok(()) => self
                .stack
                .last()
                .map_or_else(|| "<empty stack>".to_string(), |v| render(Ok(v))),
            Err(e) => render(Err(e)),
        };
        let diverted_by_handler = resumed.is_some() && full.is_err();
        debug_assert!(
            diverted_by_handler || lane == full_rendered,
            "the method-table lane disagrees with the full CallMethodMut path for .{} \
             (row owner {}) at ip {ip} -- a probe the lane skips now claims this call\n  \
             lane: {lane:?}\n  full: {full_rendered:?}",
            method_table::row(answer.row).name,
            method_table::row(answer.row).owner,
        );
        let _ = code;
        full
    }
}

/// The most arguments a row takes, and so a lane call carries.
const MAX_LANE_ARITY: usize = 2;

/// Whether a call site's argument-source descriptor (see
/// `Compiler::add_arg_sources_constant`) names only positional arguments: no
/// named argument (`FALSE`, or an array led by it) and no `|` spread
/// (`TRUE`). A named argument reaches the stack as a `Pair`, which
/// `plain_args` refuses anyway; the spread is refused because it changes the
/// argument count at run time.
// Cost: O(a), a = arguments of the site.
fn site_args_are_positional(code: &CompiledCode, idx: u32) -> bool {
    let ValueView::Array(items, ..) = code.constants[idx as usize].view() else {
        return false;
    };
    items.iter().all(|item| {
        matches!(
            item.view(),
            ValueView::Nil | ValueView::Str(_) | ValueView::Pair(..)
        )
    })
}

/// What the memo-miss half of the lane decided.
enum Resolved {
    /// The row that answers; remembered.
    Row(RowId),
    /// No row answers this method for this shape and arity in this registry
    /// generation; remembered, so later calls skip the lookup.
    Miss,
    /// Not answered at this site, for a reason of the site's own; not
    /// remembered.
    Skip,
}

/// The row bits a memo payload holds for a remembered miss.
const MISS_ROW_BITS: u16 = u16::MAX;

/// The memo payload: the row (or a miss) for a receiver of `shape` called
/// with `arity` arguments.
fn pack(shape: crate::value::DispatchShape, arity: usize, row: Option<RowId>) -> u32 {
    let row = row.map_or(MISS_ROW_BITS, RowId::to_bits);
    // `arity` is at most `MAX_LANE_ARITY`.
    (u32::from(arity as u8) << 24) | (u32::from(shape as u8) << 16) | u32::from(row)
}

/// Whether a memo payload was filled for `shape` and `arity`.
fn payload_matches(payload: u32, shape: crate::value::DispatchShape, arity: usize) -> bool {
    payload >> 16 == (u32::from(arity as u8) << 8) | u32::from(shape as u8)
}

/// The row a memo payload remembers, `None` for a remembered miss.
fn payload_row(payload: u32) -> Option<RowId> {
    let bits = payload as u16;
    (bits != MISS_ROW_BITS).then(|| RowId::from_bits(bits))
}
