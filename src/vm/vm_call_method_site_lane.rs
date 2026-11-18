//! ADR-11276 §2.4/§2.5: the method-table lane in front of the plain
//! `CallMethod`, whose receiver is an inline expression (`$<a>.ast`,
//! `$s.substr(0, 2).lc`, `.words.join(' ')`).
//!
//! `vm_method_site_lane` answers a `CallMethodMut` (a call on a variable) from
//! its method-table row before the opcode's probe chain runs. An inline
//! receiver took the whole chain on every call: the `.perl` rename, the
//! parametric-role pun, the raw-invocant arrival channel, then
//! `exec_call_method_op_impl`'s pseudo-method, attribute, `take` and
//! meta-method lanes, its Seq, Slip and gather handling, the `message`,
//! `raku`, `VAR`, `of` and stash arms, the Failure explosion of `use fatal`
//! and the native probe's own gauntlet. For a plain receiver of a
//! [`DispatchShape`](crate::value::DispatchShape) whose method has a row,
//! none of those claims the call.
//!
//! The lane shares the `CallMethodMut` lane's guard and memo
//! (`Interpreter::site_lane_answer`), so everything its module docs say
//! holds here too, and adds what the plain opcode's full path does by name
//! or by site before its native probe:
//!
//! - a modified (`.?m`, `.^m`) or quoted call never takes the lane, nor does
//!   `.perl` (renamed to `.raku`), `.throw` (a resumable throw site), or a
//!   method whose answer the full path retypes to a native array receiver's
//!   type (`retype_receiver`);
//! - the names the plain path inspects before its native probe that the
//!   `CallMethodMut` lists do not already cover (`of`, `package_at_key`,
//!   `ASSIGN-KEY`, which the shared resolve leaves to the full path anyway);
//! - `map`, `grep` and `first` are left to the full path for every inline
//!   receiver (the variable lane tells a named array apart; this one cannot);
//! - a receiver with no shape is never answered here (the shapeless lane keys
//!   on the receiver variable's name), and neither is a collection: only an
//!   immutable scalar or a `Match` is admitted (`inline_receiver_admitted`),
//!   because the plain path streams or reifies a deferred `Seq` a collection
//!   holds before its native probe.
//!
//! In debug builds the full path still runs for a pure row and must agree
//! (`check_method_site_lane`), so CI's `debug-tap` job checks every hit over
//! the whole TAP suite.

use super::vm_method_site_lane::SiteLaneAnswer;
use super::*;

impl Interpreter {
    /// The answer the method table gives the `CallMethod` at `code.ops[ip]`,
    /// or `None` to run the full path. Leaves the stack untouched.
    // Cost: O(1) on a memo hit plus the row's handler, as
    // `try_method_site_lane`.
    pub(super) fn try_call_method_site_lane(
        &mut self,
        code: &CompiledCode,
        ip: usize,
    ) -> Option<SiteLaneAnswer> {
        let OpCode::CallMethod {
            name_idx,
            arity,
            modifier_idx: None,
            quoted: false,
            arg_sources_idx,
        } = code.ops[ip]
        else {
            return None;
        };
        if matches!(
            Self::const_str(code, name_idx),
            "perl" | "throw" | "of" | "package_at_key" | "ASSIGN-KEY"
        ) || self.retype_receiver_needed(code, name_idx)
        {
            return None;
        }
        let base = self.stack.len().checked_sub(arity as usize + 1)?;
        if !inline_receiver_admitted(&self.stack[base]) {
            return None;
        }
        self.site_lane_answer(code, ip, name_idx, arity, None, arg_sources_idx)
    }

    /// Whether the full path would retype the call's answer to a native
    /// array receiver's type (`my int @a; (@a).sort`).
    // Cost: O(|name|).
    fn retype_receiver_needed(&self, code: &CompiledCode, name_idx: u32) -> bool {
        crate::builtins::method_table::native_array_result::retypes(Self::const_str(code, name_idx))
    }

    /// Complete the `CallMethod` at `code.ops[ip]` with the lane's answer:
    /// the bookkeeping the full path performs for a native answer, then the
    /// result in place of the receiver and its arguments.
    // Cost: O(1).
    pub(super) fn finish_call_method_site_lane(
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
        match self.settle_native_warning(answer.result) {
            Ok(value) => {
                let base = self.stack.len() - answer.arity - 1;
                self.stack.truncate(base);
                self.stack.push(value);
                // What a CONTROL handler settling a warning wrote to the
                // caller's lexicals reaches their slots as after the full
                // path (`exec_call_method_site_full`); both are no-ops when
                // nothing is pending, which the lane's guard required.
                self.apply_pending_rw_writeback(code);
                self.drain_pending_local_updates_after_call(code);
                Ok(())
            }
            Err(e) => {
                self.sync_source_line(code, ip);
                self.record_call_resume_point(code, ip, &e);
                Err(e)
            }
        }
    }
}

/// Whether the lane answers a call on this inline receiver: an immutable
/// scalar, or a `Match` (eager, lazy, or a grammar cursor the cursor-match
/// lane decides). A collection is left to the full path, which streams or
/// reifies a deferred `Seq` the receiver holds (a lazy `.keys`, a `Pair`
/// whose value is a `.map`) before the native probe, where the variable
/// lane's receiver never holds one unreified.
// Cost: O(1), a tag probe.
fn inline_receiver_admitted(receiver: &Value) -> bool {
    use crate::value::DispatchShape as S;
    receiver.is_lazy_match_value()
        || matches!(
            receiver.dispatch_shape(),
            Some(S::Str | S::Int | S::Num | S::Rat | S::FatRat | S::Complex | S::Bool | S::Match)
        )
}
