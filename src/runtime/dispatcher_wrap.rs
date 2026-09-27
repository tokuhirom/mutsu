//! `.wrap` on a multi method's DISPATCHER — the proto `.^method_table<m>` /
//! `.^lookup('m')` returns, as opposed to one of its `.candidates[N]`.
//!
//! A candidate wrap (ADR-0019 E10) runs after candidate selection, around the
//! one chosen candidate. A dispatcher wrap runs BEFORE selection, once per
//! call: its chain lives in `Registry::method_wrap_chains` under the sentinel
//! slot [`DISPATCHER_WRAP_IDX`] of the class that owns the multi family, and
//! the method-wrap entry sites (`class_dispatch.rs`,
//! `Interpreter::check_method_wrap_chain`) consult it first. The frame they
//! push (ADR-0019 E9b-2's single `MethodDispatchFrame`) ends in a
//! [`DeferralEntry::Redispatch`] instead of a resolved `Candidate`, so the
//! innermost `callsame`/`callwith` re-dispatches the multi on the frame's
//! current invocant and args — `callwith($instance, |c)` from a wrapper that
//! swaps a type-object invocant (Staticish) selects against the new invocant.
//! The re-dispatch is an ordinary fresh method call except that it skips the
//! dispatcher chain it came from (`Interpreter::dispatcher_wrap_bypass`);
//! nothing re-enters the wrapper chain by name.

use super::*;
use crate::runtime::registry::DISPATCHER_WRAP_IDX;
use crate::symbol::Symbol;
use crate::value::AttrMap;

/// The `method_wrap_chains` slot a `.wrap`/`.unwrap` on a `Method` object
/// addresses: its own candidate index, or [`DISPATCHER_WRAP_IDX`] when the
/// object is a multi dispatcher (which carries no candidate index).
pub(crate) fn method_object_wrap_slot(attrs: &AttrMap) -> Option<usize> {
    match attrs.get("__mutsu_lookup_candidate_idx").map(Value::view) {
        Some(ValueView::Int(idx)) => Some(idx as usize),
        _ if attrs.get("is_dispatcher").is_some_and(Value::truthy) => Some(DISPATCHER_WRAP_IDX),
        _ => None,
    }
}

impl Interpreter {
    /// The dispatcher wrap chain a call of `method` on `receiver_class`
    /// enters, if any: that of the first class in the MRO declaring
    /// `method` (the class whose dispatcher the call resolves through).
    /// `None` inside the chain's own terminal re-dispatch.
    // Cost: O(1) when no dispatcher of this name was ever wrapped; otherwise
    // O(m), m = length of the receiver's MRO.
    pub(crate) fn dispatcher_wrap_chain(
        &mut self,
        receiver_class: &str,
        method: &str,
    ) -> Option<Vec<(u64, Value)>> {
        if !self.registry().dispatcher_wrapped_methods.contains(method) {
            return None;
        }
        if let Some((name, frames, routines)) = &self.dispatcher_wrap_bypass
            && name == method
            && *frames == self.call_frames.len()
            && *routines == self.routine_stack_len()
        {
            return None;
        }
        let mro = self.class_mro(receiver_class);
        let registry = self.registry();
        let owner = mro
            .iter()
            .find(|cls| registry.method_overloads_present(**cls, method))?;
        registry
            .method_wrap_chain(owner.as_str(), method, DISPATCHER_WRAP_IDX)
            .cloned()
    }

    /// Enter the wrap chain of a method call about to run `method_def` (the
    /// winner resolved for `receiver_class`), if any: a wrapped multi
    /// dispatcher takes precedence over the winner's own candidate chain.
    /// Pushes the samewith context and the wrap-prefixed dispatch frame and
    /// returns the outermost wrapper, which the caller invokes with
    /// `[invocant, ...args]` and then pops both. Shared by the two method-wrap
    /// entry sites (`class_dispatch.rs`, `check_method_wrap_chain`).
    // Cost: O(m + c), m = receiver MRO length, c = candidates of `method` on
    // `owner_class` (the candidate-index scan); O(1) when nothing is wrapped.
    pub(crate) fn enter_method_wrap_chain(
        &mut self,
        receiver_class: &str,
        method: &str,
        owner_class: Symbol,
        method_def: &MethodDef,
        args: &[Value],
        invocant: Value,
    ) -> Option<Value> {
        if !self.has_any_wrap_chains() {
            return None;
        }
        if let Some(chain) = self.dispatcher_wrap_chain(receiver_class, method) {
            self.push_method_samewith_context(receiver_class, method, args, Some(invocant.clone()));
            self.push_dispatcher_wrap_frame(receiver_class, method, args, invocant, &chain);
            return chain.last().map(|(_, wrapper)| wrapper.clone());
        }
        let cand_idx =
            self.find_method_candidate_index(owner_class.as_str(), method, method_def)?;
        let chain = self.get_method_wrap_chain(owner_class.as_str(), method, cand_idx)?;
        self.push_method_samewith_context(receiver_class, method, args, Some(invocant.clone()));
        self.push_wrapped_method_dispatch_frame(
            receiver_class,
            method,
            args,
            invocant,
            owner_class,
            method_def,
            &chain,
        );
        chain.last().map(|(_, wrapper)| wrapper.clone())
    }

    /// Push the `MethodDispatchFrame` for a dispatcher wrap: the
    /// below-outermost wrappers in call order, then the terminal
    /// [`DeferralEntry::Redispatch`]. The caller invokes `chain`'s outermost
    /// wrapper directly with `[invocant, ...args]`, exactly as for
    /// [`Self::push_wrapped_method_dispatch_frame`].
    pub(crate) fn push_dispatcher_wrap_frame(
        &mut self,
        receiver_class: &str,
        method_name: &str,
        args: &[Value],
        invocant: Value,
        chain: &[(u64, Value)],
    ) {
        let arg_sources = self.pending_call_arg_sources().cloned();
        let mut remaining: Vec<DeferralEntry> = Vec::with_capacity(chain.len());
        for i in (0..chain.len() - 1).rev() {
            remaining.push(DeferralEntry::Wrapper(chain[i].1.clone()));
        }
        remaining.push(DeferralEntry::Redispatch {
            name: method_name.to_string(),
        });
        let dispatch_token = self.next_dispatch_token();
        self.method_dispatch_stack.push(MethodDispatchFrame {
            receiver_class: receiver_class.to_string(),
            invocant,
            args: args.to_vec(),
            remaining,
            rw_params: Vec::new(),
            dispatch_token,
            arg_sources,
            in_wrapper: true,
        });
    }

    /// The terminal leg of a dispatcher wrap chain: dispatch `method` afresh
    /// on `invocant` with `args`, bypassing only the dispatcher chain for
    /// this one call (see `dispatcher_wrap_bypass`).
    pub(crate) fn redispatch_after_dispatcher_wrap(
        &mut self,
        invocant: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let saved = self.dispatcher_wrap_bypass.replace((
            method.to_string(),
            self.call_frames.len(),
            self.routine_stack_len(),
        ));
        let result = self.call_method_with_values(invocant, method, args);
        self.dispatcher_wrap_bypass = saved;
        result
    }
}
