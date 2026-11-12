//! The reading metamethods of a plain `Attribute` metaobject (`.name`,
//! `.package`, `.type`, `.get_value($obj)`, ...), answered at the top of the
//! method-call opcodes.
//!
//! `try_attribute_meta_row` (`vm_attribute_meta_row.rs`) already answers these
//! from `Attribute`'s rows, but from inside the compiled dispatch, after the
//! opcode's receiver probes and argument handling: ~20k instructions of a
//! `$attr.package` call. EEL's state copier calls four of them per attribute
//! of every event it copies (#17530). For a receiver that is exactly an
//! `Attribute` instance (no subclass, no mixed-in role, no `augment` of
//! `Attribute` declaring the method), none of those probes claims the call:
//! they key on a container or `Proxy` receiver, a user class, a mixin, a
//! wrapped method, a `^find_method`, or a lazy or `|`-spread argument, which the
//! guard below refuses. The lane answers with the same rows, before them.
//!
//! Only the methods that read the metaobject are admitted; `set_value`,
//! `set_rw`, `set_build` and the rest keep the full path (their argument
//! sources and writes matter).

use super::*;

/// The `Attribute` methods the lane answers: pure reads of the metaobject
/// or, for `get_value`, of the object it is asked about.
// Cost: O(1).
fn lane_method(method: &str) -> bool {
    matches!(
        method,
        "name"
            | "package"
            | "type"
            | "has_accessor"
            | "rw"
            | "readonly"
            | "required"
            | "container"
            | "get_value"
    )
}

impl Interpreter {
    /// Answer a reading metamethod of a plain `Attribute`, replacing the
    /// receiver and arguments on the stack with the result. `None` leaves the
    /// stack untouched and the call to the generic path.
    // Cost: O(1) for the guard (a name compare, a tag test, a memoized
    // `has_user_method` probe, an argument scan); a hit costs the row.
    pub(super) fn try_attribute_meta_lane(
        &mut self,
        code: &CompiledCode,
        method: &str,
        method_sym: crate::symbol::Symbol,
        arity: u32,
        plain_call: bool,
        arg_sources_idx: Option<u32>,
    ) -> Option<Result<Value, RuntimeError>> {
        // `plain_call`: no `.^`/`.?`/`.!` modifier and no quoted name.
        if !lane_method(method)
            || !plain_call
            || self.accessor_ref_pending
            || self.has_any_wrap_chains()
            || crate::runtime::find_method_intercept::any_user_find_method()
        {
            return None;
        }
        let arity = arity as usize;
        let target_idx = self.stack.len().checked_sub(arity + 1)?;
        if !matches!(
            self.stack[target_idx].view(),
            ValueView::Instance { class_name, .. } if class_name == "Attribute"
        ) {
            return None;
        }
        // Arguments the generic path would rewrite before binding: a `|`
        // spread, a container, a `Proxy`, a lazy or deferred list.
        if Self::decode_arg_slip_positions(code, arg_sources_idx).is_some()
            || self.stack[target_idx + 1..].iter().any(|a| {
                a.is_proxy_value()
                    || a.is_container_ref()
                    || matches!(
                        a.view(),
                        ValueView::LazyList(_) | ValueView::Seq(_) | ValueView::Slip(_)
                    )
            })
        {
            return None;
        }
        if !crate::builtins::method_table::owner_declares("Attribute", method_sym)
            || self.has_user_method_sym("Attribute", method_sym)
        {
            return None;
        }
        let args = Self::hllize_call_args(self.stack[target_idx + 1..].to_vec());
        let target = self.stack[target_idx].clone();
        let answer = crate::builtins::method_table::invoke_owner(
            self,
            &["Attribute"],
            method,
            &args,
            || target,
        )?;
        self.stack.truncate(target_idx);
        self.pending_call_arg_source_slots.clear();
        self.set_pending_call_arg_sources(None);
        crate::vm::vm_stats::record_dispatch_entry_outcome("callmethod", "attribute-lane");
        Some(answer)
    }
}
