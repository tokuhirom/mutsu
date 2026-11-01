//! Method deferral frames, built only when a deferral builtin asks (#10108).
//!
//! `callsame`/`callwith`/`nextsame`/`nextwith`/`nextcallee`/`lastcall` are
//! dynamically scoped: a helper `sub` called from a multi method's body defers
//! to that method's next candidate exactly as the body itself would. So
//! whether a call's frame will be read is not a property of the winning
//! candidate's body, and a program that mentions a deferral builtin anywhere
//! (the `dispatcher_possible` latch) must give every method call a frame.
//!
//! Building one is the expensive part: the deferral expansion, a signature
//! match per candidate, a second resolution of the winner and a fingerprint per
//! candidate. Almost every frame is popped unread, so a call records only what
//! the build needs ([`PendingMethodDispatch`]) and reserves its push-order
//! token. [`Interpreter::materialize_pending_method_dispatch`] builds every
//! still-pending frame the moment a deferral builtin needs the innermost one,
//! so the builtins see exactly the stack an eager push would have left.
use super::*;
use crate::symbol::Symbol;

/// A resolved winner's applicability and writeback metadata, without GC values.
#[derive(Debug)]
pub(super) struct MethodDispatchWinner {
    pub(super) owner: Symbol,
    pub(super) fingerprint: u64,
    pub(super) rw_params: Vec<(usize, String)>,
}

/// The arguments of [`Interpreter::build_method_dispatch_frame`], kept until
/// a deferral builtin needs the frame.
#[derive(Debug)]
pub(crate) struct PendingMethodCall {
    receiver_class: Symbol,
    method_name: Symbol,
    invocant: Value,
    args: Vec<Value>,
    winner: Option<MethodDispatchWinner>,
}

/// One method call's slot on the lazy deferral stack.
#[derive(Debug)]
pub(crate) struct PendingMethodDispatch {
    /// Reserved at call time, so a frame built later sorts into
    /// `method_dispatch_stack` exactly where an eager push would have put it.
    dispatch_token: u64,
    call: PendingMethodCall,
}

impl PendingMethodDispatch {
    /// The values this entry keeps alive, for the GC root walk.
    pub(crate) fn roots(&self) -> (&Value, &[Value]) {
        (&self.call.invocant, self.call.args.as_slice())
    }
}

impl Interpreter {
    /// Whether `invocant` is an instance of a class that inherits a builtin
    /// container, so its data lives in a backing attribute
    /// (`__mutsu_hash_storage` / `__mutsu_array_storage` / `__baggy_data__`) and
    /// the native behavior on that storage is a deferral base candidate
    /// (`native_storage_base_entry`). Decided by the receiver VALUE, not its
    /// class name: a role punned onto a container (`my %h is MK`) has a role as
    /// its receiver class and still carries the storage.
    // Cost: O(1), three attribute probes.
    pub(super) fn invocant_carries_native_storage(invocant: &Value) -> bool {
        // A role mixed into such an instance (`$dict but Info`) keeps the
        // storage on the wrapped instance.
        let invocant = match invocant.view() {
            ValueView::Mixin(inner, _) => inner.as_ref(),
            _ => invocant,
        };
        matches!(
            invocant.view(),
            ValueView::Instance { attributes, .. }
                if attributes.contains_key(crate::symbol::wk::array_storage())
                    || attributes.contains_key(crate::symbol::wk::hash_storage())
                    || attributes.contains_key(crate::symbol::wk::baggy_data())
        )
    }

    // Cost: O(1) in a program that names no deferral builtin; otherwise
    // O(a + p), a = the call's argument count, p = winner parameters (the args
    // and winner writeback metadata are copied for a later
    // build), when the name's frame can be built late — the candidate work
    // then happens only if a deferral builtin runs (see
    // `materialize_pending_method_dispatch`). A name that cannot is built now,
    // at `build_method_dispatch_frame`'s cost.
    pub(crate) fn push_method_dispatch_frame(
        &mut self,
        receiver_class: &str,
        method_name: &str,
        args: &[Value],
        invocant: Value,
        chosen: Option<(&str, &MethodDef)>,
    ) -> bool {
        self.push_method_dispatch_frame_named(
            receiver_class,
            method_name,
            std::borrow::Cow::Owned(method_name.to_string()),
            args,
            invocant,
            chosen,
        )
    }

    /// [`Self::push_method_dispatch_frame`] for a caller holding the method's
    /// interned [`Symbol`]: the samewith context borrows the symbol's string
    /// instead of copying it, which is one allocation per method call.
    // Cost: as `push_method_dispatch_frame`, minus the name's allocation.
    pub(crate) fn push_method_dispatch_frame_sym(
        &mut self,
        receiver_class: &str,
        method_sym: Symbol,
        args: &[Value],
        invocant: Value,
        chosen: Option<(&str, &MethodDef)>,
    ) -> bool {
        let method_name = method_sym.as_str();
        self.push_method_dispatch_frame_named(
            receiver_class,
            method_name,
            std::borrow::Cow::Borrowed(method_name),
            args,
            invocant,
            chosen,
        )
    }

    fn push_method_dispatch_frame_named(
        &mut self,
        receiver_class: &str,
        method_name: &str,
        name: std::borrow::Cow<'static, str>,
        args: &[Value],
        invocant: Value,
        chosen: Option<(&str, &MethodDef)>,
    ) -> bool {
        // Always push samewith context so samewith() can find the method name/invocant
        self.push_method_samewith_context_named(
            receiver_class,
            name,
            args,
            Some(invocant.clone()),
        );
        // Only the deferral builtins (`callsame` & co., `nextcallee`,
        // `lastcall`) read a dispatch frame, so a program that names none of
        // them anywhere never needs one. See `dispatcher_possible`.
        if !crate::opcode::dispatcher_possible() {
            return false;
        }
        let dispatch_token = self.next_dispatch_token();
        // The resolver already tested this exact winner. Keep its identity and
        // writeback metadata so building a frame cannot run its predicates again.
        let winner = chosen.map(|(owner, def)| MethodDispatchWinner {
            owner: Symbol::intern(owner),
            fingerprint: self.method_def_fingerprint(def),
            rw_params: super::builtins_dispatch_next::rw_scalar_positional_params(&def.param_defs),
        });
        if !self.deferral_build_is_context_free(receiver_class, method_name) {
            let frame = self.build_method_dispatch_frame(
                receiver_class,
                method_name,
                args.to_vec(),
                invocant,
                dispatch_token,
                winner,
            );
            self.dispatch.method_dispatch_stack.push(frame);
            return true;
        }
        self.dispatch
            .pending_method_dispatch
            .push(PendingMethodDispatch {
                dispatch_token,
                call: PendingMethodCall {
                    receiver_class: Symbol::intern(receiver_class),
                    method_name: Symbol::intern(method_name),
                    invocant,
                    args: args.to_vec(),
                    winner,
                },
            });
        true
    }

    /// Push the deferral frame of a call qualified by a ROLE (`self.R::m()`,
    /// `self.R::new(|%a)`): one with no next candidate. The role's method is
    /// not part of any class's dispatch chain, so a `nextsame` / `callsame`
    /// in it finds nothing to defer to and answers Nil, as in rakudo. Without
    /// a frame of its own the callee's deferral read the CALLER's frame and
    /// ran the receiver's next MRO candidate (#11592). Returns whether a frame
    /// was pushed (pop it with [`Self::pop_method_dispatch`]).
    // Cost: O(a), a = the call's argument count (copied into the frame); O(1)
    // in a program that names no deferral builtin.
    pub(crate) fn push_qualified_method_dispatch_frame(
        &mut self,
        receiver_class: &str,
        args: &[Value],
        invocant: Value,
    ) -> bool {
        if !crate::opcode::dispatcher_possible() {
            return false;
        }
        let dispatch_token = self.next_dispatch_token();
        self.dispatch
            .method_dispatch_stack
            .push(super::MethodDispatchFrame {
                receiver_class: receiver_class.to_string(),
                invocant,
                args: args.to_vec(),
                remaining: Vec::new(),
                rw_params: Vec::new(),
                dispatch_token,
                arg_sources: None,
                in_wrapper: false,
                lazy_tail: None,
            });
        true
    }

    /// Whether building `(receiver_class, method_name)`'s frame reads nothing
    /// of the call site but the argument values, so it gives the same answer
    /// after the callee has started. Matching a candidate's parameter reads
    /// the caller's context for an `is rw`/`is raw`/sigilless parameter (the
    /// call-site source names), a typed `@`/`%` parameter and a native-typed
    /// one (the source variable's declared type, looked up in the current
    /// env, and the call-site literal mask), and a `where` clause, sub-signature
    /// or shape runs user code against the current env. Any of those in any
    /// deferral candidate keeps the frame eager.
    // Cost: O(1) amortized (memoized per `(class, method)` for one registry
    // write generation); a miss is O(c * p) over the deferral expansion's
    // candidates and their parameters.
    pub(super) fn deferral_build_is_context_free(&mut self, receiver_class: &str, method_name: &str) -> bool {
        self.refresh_method_caches_for_generation();
        let key = (Symbol::intern(receiver_class), Symbol::intern(method_name));
        if let Some(&free) = self.caches.deferral_build_context_free.get(&key) {
            return free;
        }
        let free = self
            .resolve_deferral_expansion(receiver_class, method_name)
            .iter()
            .all(|(_, def)| def.param_defs.iter().all(param_match_is_context_free));
        self.caches.deferral_build_context_free.insert(key, free);
        free
    }

    /// Pop a method dispatch frame (must only be called if push returned true).
    ///
    /// The innermost live frame is the one being popped, and it is on top of
    /// whichever of the two stacks holds the higher token: a pending entry or
    /// a built frame.
    // Cost: O(1).
    pub(crate) fn pop_method_dispatch(&mut self) {
        let pending = self
            .dispatch
            .pending_method_dispatch
            .last()
            .map(|p| p.dispatch_token);
        let built = self
            .dispatch
            .method_dispatch_stack
            .last()
            .map(|f| f.dispatch_token);
        if pending.is_some_and(|p| built.is_none_or(|b| p > b)) {
            self.dispatch.pending_method_dispatch.pop();
        } else {
            self.dispatch.method_dispatch_stack.pop();
        }
    }

    /// Build every pending method frame into `method_dispatch_stack` (in token
    /// order). Called by each deferral builtin before it looks for the
    /// innermost dispatch frame.
    // Cost: O(1) when nothing is pending; otherwise the eager build's cost per
    // pending frame, each frame built at most once.
    pub(super) fn materialize_pending_method_dispatch(&mut self) {
        self.materialize_pending_method_frames();
        self.materialize_lazy_deferral_tail();
    }

    // Cost: O(1) when nothing is pending; otherwise the eager build's cost per
    // pending frame, each frame built at most once.
    fn materialize_pending_method_frames(&mut self) {
        if self.dispatch.pending_method_dispatch.is_empty() {
            return;
        }
        let unbuilt = std::mem::take(&mut self.dispatch.pending_method_dispatch);
        // Resolving the winner resets this flag; the caller's view of the
        // last dispatch must not change because a frame was built late.
        let saved_ambiguous = self.dispatch.dispatch_ambiguous;
        for entry in unbuilt {
            let call = entry.call;
            let frame = self.build_method_dispatch_frame(
                &call.receiver_class.resolve(),
                &call.method_name.resolve(),
                call.args,
                call.invocant,
                entry.dispatch_token,
                call.winner,
            );
            let at = self
                .dispatch
                .method_dispatch_stack
                .partition_point(|f| f.dispatch_token < frame.dispatch_token);
            self.dispatch.method_dispatch_stack.insert(at, frame);
        }
        self.dispatch.dispatch_ambiguous = saved_ambiguous;
        // A `where` clause run by the build may make method calls of its own;
        // they push and pop their own pending entries during the build.
        debug_assert!(self.dispatch.pending_method_dispatch.is_empty());
    }

}
/// See [`Interpreter::deferral_build_is_context_free`].
fn param_match_is_context_free(pd: &ParamDef) -> bool {
    let reads_call_site =
        pd.sigilless || pd.traits.iter().any(|t| matches!(t.as_str(), "rw" | "raw"));
    let runs_user_code = pd.where_constraint.is_some()
        || pd.sub_signature.is_some()
        || pd.outer_sub_signature.is_some()
        || pd.code_signature.is_some()
        || pd.shape_constraints.is_some();
    let reads_source_type = pd.type_constraint.as_deref().is_some_and(|tc| {
        pd.name.starts_with(['@', '%'])
            || crate::runtime::native_types::native_family(Interpreter::constraint_base_name(tc))
                .is_some()
    });
    !(reads_call_site || runs_user_code || reads_source_type)
}
