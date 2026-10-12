//! The MRO tail of a method deferral frame, built only when a deferral builtin
//! asks (#12875).
//!
//! The two sites that resolve their winner themselves -- the method-wrap entry
//! (`push_wrapped_method_dispatch_frame`) and a resolved instance-method call
//! (`run_resolved_instance_method`) -- used to compute the candidates after
//! the winner on every call: the deferral expansion (an MRO walk cloning every
//! candidate `MethodDef`), a signature match per `multi` candidate and a
//! fingerprint each. Almost no call ever reads them. As with the pending frames
//! of `method_dispatch_lazy`, the frame now records what the build needs and
//! [`Interpreter::materialize_pending_method_dispatch`] builds it the moment a
//! deferral builtin looks at the innermost frame.
//!
//! The build is deferred only when it reads nothing of the call site but the
//! argument values (`deferral_build_is_context_free`), so building it late
//! gives the answer an eager build would have given.
//!
//! A resolved instance-method call pushed its frame only when the tail was
//! non-empty, so a deferral in the candidate saw the caller's frame otherwise.
//! A lazy frame of that call is marked `drop_if_empty`: when it is built and
//! turns out empty it is removed, which leaves exactly the stack the eager
//! code would have left. Only the innermost frame is ever built (it is the only
//! one a deferral builtin reads), so the removal never shifts a frame index
//! someone else holds.

use super::*;
use crate::symbol::Symbol;

/// What [`Interpreter::materialize_lazy_deferral_tail`] needs to build a
/// frame's tail: the rest of the inputs (receiver class, arguments) are the
/// frame's own, which nothing changes before the build.
#[derive(Debug, Clone)]
pub(crate) struct LazyDeferralTail {
    method_name: Symbol,
    /// Fingerprint of the winner, which the tail skips.
    chosen_fp: u64,
    /// The invocant the signature matches see (a `where` clause's `self`).
    invocant: Value,
    /// Remove the frame when the built tail is empty (see the module docs).
    drop_if_empty: bool,
}

impl LazyDeferralTail {
    /// The value this entry keeps alive, for the GC root walk.
    pub(crate) fn root(&self) -> &Value {
        &self.invocant
    }
}

/// How a winner-resolving site gets its frame's MRO tail.
pub(crate) enum DeferralTail {
    /// Structurally empty: the winner is the method's only candidate.
    Empty,
    /// Built when a deferral builtin first asks.
    Lazy(Box<LazyDeferralTail>),
    /// Built now: matching a candidate reads the call site.
    Eager(Vec<DeferralEntry>),
}

impl Interpreter {
    /// The MRO tail of a frame whose winner `chosen_def` is already known.
    // Cost: O(1) amortized for `Empty`/`Lazy` (memoized structural probes);
    // `Eager` is `deferral_tail_entries`' O(c) over the deferral candidates.
    pub(crate) fn plan_deferral_tail(
        &mut self,
        receiver_class: &str,
        method_name: &str,
        args: &[Value],
        chosen_def: &MethodDef,
        invocant: &Value,
        drop_if_empty: bool,
    ) -> DeferralTail {
        // With at most one structural candidate the winner is it, and nothing
        // is left to defer to (the same fast path the pending frame build
        // takes).
        if !self.has_multiple_dispatch_candidates(receiver_class, method_name) {
            return DeferralTail::Empty;
        }
        if !self.deferral_build_is_context_free(receiver_class, method_name) {
            return DeferralTail::Eager(self.deferral_tail_entries(
                receiver_class,
                method_name,
                args,
                chosen_def,
                invocant,
            ));
        }
        DeferralTail::Lazy(Box::new(LazyDeferralTail {
            method_name: Symbol::intern(method_name),
            chosen_fp: self.method_def_fingerprint(chosen_def),
            invocant: invocant.clone(),
            drop_if_empty,
        }))
    }

    /// Build the innermost method frame's lazy tail, if it has one; a
    /// `drop_if_empty` frame whose tail is empty is removed and the next one
    /// is considered in its place.
    // Cost: O(1) when the innermost frame has no lazy tail; otherwise
    // `deferral_tail_entries`' O(c) per frame built, each built at most once.
    pub(super) fn materialize_lazy_deferral_tail(&mut self) {
        loop {
            let Some(idx) = self.dispatch.method_dispatch_stack.len().checked_sub(1) else {
                return;
            };
            let frame = &mut self.dispatch.method_dispatch_stack[idx];
            let Some(lazy) = frame.lazy_tail.take() else {
                return;
            };
            let token = frame.dispatch_token;
            let receiver_class = frame.receiver_class.clone();
            let args = frame.args.clone();
            let method_name = lazy.method_name.resolve();
            // A `where` clause run by the match may make method calls of its
            // own; they push and pop their frames, leaving this one at `idx`.
            let entries = self.deferral_tail_entries_after(
                &receiver_class,
                &method_name,
                &args,
                lazy.chosen_fp,
                &lazy.invocant,
            );
            let Some(frame) = self.dispatch.method_dispatch_stack.get_mut(idx) else {
                return;
            };
            debug_assert_eq!(frame.dispatch_token, token);
            if entries.is_empty() && lazy.drop_if_empty && frame.remaining.is_empty() {
                self.dispatch.method_dispatch_stack.remove(idx);
                continue;
            }
            frame.remaining.extend(entries);
            return;
        }
    }

    /// Pop the frame `run_resolved_instance_method` pushed under `token`,
    /// unless building its lazy tail already removed it.
    // Cost: O(1).
    pub(crate) fn pop_method_dispatch_frame_with_token(&mut self, token: u64) {
        if self
            .dispatch
            .method_dispatch_stack
            .last()
            .is_some_and(|frame| frame.dispatch_token == token)
        {
            self.dispatch.method_dispatch_stack.pop();
        }
    }
}
