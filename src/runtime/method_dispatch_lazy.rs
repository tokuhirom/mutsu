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

/// The arguments of [`Interpreter::build_method_dispatch_frame`], kept until
/// a deferral builtin needs the frame.
#[derive(Debug)]
pub(crate) struct PendingMethodCall {
    receiver_class: Symbol,
    method_name: Symbol,
    invocant: Value,
    args: Vec<Value>,
}

/// One method call's slot on the lazy deferral stack.
#[derive(Debug)]
pub(crate) struct PendingMethodDispatch {
    /// Reserved at call time, so a frame built later sorts into
    /// `method_dispatch_stack` exactly where an eager push would have put it.
    dispatch_token: u64,
    /// `None` once built and found to need no frame (nothing to defer to): the
    /// entry stays only so the call's `pop_method_dispatch` pops it.
    call: Option<PendingMethodCall>,
}

impl PendingMethodDispatch {
    /// The values this entry keeps alive, for the GC root walk.
    pub(crate) fn roots(&self) -> Option<(&Value, &[Value])> {
        self.call.as_ref().map(|c| (&c.invocant, c.args.as_slice()))
    }
}

impl Interpreter {
    /// The subscript/`STORE` protocol whose native implementation on a
    /// container subclass's backing storage is a deferral base candidate. See
    /// `container_protocol_override` in [`Self::push_method_dispatch_frame`].
    fn is_container_protocol_method(method: &str) -> bool {
        matches!(
            method,
            "AT-KEY"
                | "ASSIGN-KEY"
                | "BIND-KEY"
                | "DELETE-KEY"
                | "EXISTS-KEY"
                | "AT-POS"
                | "ASSIGN-POS"
                | "BIND-POS"
                | "DELETE-POS"
                | "EXISTS-POS"
                | "STORE"
        )
    }

    /// True when `class_key` inherits a builtin container whose data lives in a
    /// backing attribute (`__mutsu_hash_storage` / `__mutsu_array_storage` /
    /// `__baggy_data__`), i.e. one of the `native_*_storage_next_candidate`
    /// fallbacks can serve as a deferral base.
    fn class_has_native_container_backing(&mut self, class_key: &str) -> bool {
        self.class_mro(class_key).iter().any(|n| {
            let name = n.resolve();
            Self::is_associative_base(&name)
                || Self::is_positional_base(&name)
                || matches!(
                    name.as_str(),
                    "Set" | "SetHash" | "Bag" | "BagHash" | "Mix" | "MixHash"
                )
        })
    }

    // Cost: O(1) in a program that names no deferral builtin; otherwise
    // O(a), a = the call's argument count (the args are copied for a later
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
    ) -> bool {
        // Always push samewith context so samewith() can find the method name/invocant
        self.push_method_samewith_context(
            receiver_class,
            method_name,
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
        if !self.deferral_build_is_context_free(receiver_class, method_name) {
            let frame = self.build_method_dispatch_frame(
                receiver_class,
                method_name,
                args.to_vec(),
                invocant,
                dispatch_token,
            );
            let Some(frame) = frame else {
                return false;
            };
            self.method_dispatch_stack.push(frame);
            return true;
        }
        self.pending_method_dispatch.push(PendingMethodDispatch {
            dispatch_token,
            call: Some(PendingMethodCall {
                receiver_class: Symbol::intern(receiver_class),
                method_name: Symbol::intern(method_name),
                invocant,
                args: args.to_vec(),
            }),
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
    fn deferral_build_is_context_free(&mut self, receiver_class: &str, method_name: &str) -> bool {
        self.refresh_method_caches_for_generation();
        let key = (Symbol::intern(receiver_class), Symbol::intern(method_name));
        if let Some(&free) = self.deferral_build_context_free.get(&key) {
            return free;
        }
        let free = self
            .resolve_deferral_expansion(receiver_class, method_name)
            .iter()
            .all(|(_, def)| def.param_defs.iter().all(param_match_is_context_free));
        self.deferral_build_context_free.insert(key, free);
        free
    }

    /// Pop a method dispatch frame (must only be called if push returned true).
    ///
    /// The innermost live frame is the one being popped, and it is on top of
    /// whichever of the two stacks holds the higher token: a pending (or
    /// settled) entry, or a built frame.
    // Cost: O(1).
    pub(crate) fn pop_method_dispatch(&mut self) {
        let pending = self
            .pending_method_dispatch
            .last()
            .map(|p| p.dispatch_token);
        let built = self.method_dispatch_stack.last().map(|f| f.dispatch_token);
        if pending.is_some_and(|p| built.is_none_or(|b| p > b)) {
            self.pending_method_dispatch.pop();
        } else {
            self.method_dispatch_stack.pop();
        }
    }

    /// Build every still-pending method frame into `method_dispatch_stack`
    /// (in token order), settling the ones that need no frame. Called by each
    /// deferral builtin before it looks for the innermost dispatch frame.
    ///
    /// Unbuilt entries always form the top of the pending stack: a build
    /// settles all of them at once, and later calls push above.
    // Cost: O(1) when nothing is pending; otherwise the eager build's cost per
    // pending frame, each frame built at most once.
    pub(super) fn materialize_pending_method_dispatch(&mut self) {
        let start = self
            .pending_method_dispatch
            .iter()
            .rposition(|p| p.call.is_none())
            .map_or(0, |i| i + 1);
        if start == self.pending_method_dispatch.len() {
            return;
        }
        let unbuilt = self.pending_method_dispatch.split_off(start);
        // Resolving the winner resets this flag; the caller's view of the
        // last dispatch must not change because a frame was built late.
        let saved_ambiguous = self.dispatch_ambiguous;
        let mut settled = Vec::new();
        for entry in unbuilt {
            let Some(call) = entry.call else {
                unreachable!("settled entries sit below every unbuilt one");
            };
            let frame = self.build_method_dispatch_frame(
                &call.receiver_class.resolve(),
                &call.method_name.resolve(),
                call.args,
                call.invocant,
                entry.dispatch_token,
            );
            match frame {
                Some(frame) => {
                    let at = self
                        .method_dispatch_stack
                        .partition_point(|f| f.dispatch_token < frame.dispatch_token);
                    self.method_dispatch_stack.insert(at, frame);
                }
                None => settled.push(PendingMethodDispatch {
                    dispatch_token: entry.dispatch_token,
                    call: None,
                }),
            }
        }
        self.dispatch_ambiguous = saved_ambiguous;
        // A `where` clause run by the build may make method calls of its own;
        // they push above `start` and have popped again by now.
        debug_assert_eq!(self.pending_method_dispatch.len(), start);
        self.pending_method_dispatch.extend(settled);
    }

    /// The deferral frame a method call establishes, or `None` when the call
    /// has nothing to defer to (a deferral builtin then falls through to an
    /// enclosing frame or a native fallback).
    // Cost: O(c) in the candidates the deferral expansion collects (each
    // matched against the args once); the single-candidate fast path is O(1)
    // amortized (the MRO probes are memoized per `(class, method)` for one
    // registry write generation).
    fn build_method_dispatch_frame(
        &mut self,
        receiver_class: &str,
        method_name: &str,
        args: Vec<Value>,
        invocant: Value,
        dispatch_token: u64,
    ) -> Option<super::MethodDispatchFrame> {
        // A user-overridden grammar `parse`/`subparse`/`parsefile` needs an MRO frame
        // even with a single user candidate, so a `nextsame`/`nextwith` inside it can
        // defer to the NATIVE grammar parse — the base candidate that is not a
        // `MethodDef` and so never appears in the dispatch candidates (YAMLish's
        // `method parse` does `nextwith($input, :actions(Actions))`).
        let grammar_parse_override = matches!(method_name, "parse" | "subparse" | "parsefile")
            && self.class_is_grammar(receiver_class)
            && self.has_user_method(receiver_class, method_name);
        // A user BUILDALL/POPULATE/clone (e.g. installed by a custom HOW via
        // `add_method` — OO::Monitors) likewise needs an MRO frame even as a
        // single candidate, so its `callsame` reaches the NATIVE base
        // implementation (`native_mu_base_next_candidate`: the built instance
        // for BUILDALL/POPULATE, the native attribute-copying clone for clone).
        // A user `method new` is the same situation, and the one that bites
        // hardest: Raku's `Mu.new(*%attrinit)` is ALWAYS the base candidate of
        // a `new` MRO, but mutsu models it natively (bless), so it is not a
        // `MethodDef` and never appears among the deferral candidates. Without
        // a frame, a `method new { ... callwith(|%args) ... }` had no dispatch
        // context of its own, and `callwith` resolved against whatever frame an
        // ENCLOSING routine happened to leave live — an outer `multi sub`'s,
        // typically — calling a completely unrelated candidate or dying with
        // "Cannot resolve caller". (Raku lets a plain SUB see an enclosing
        // dispatcher, but a method call always establishes its own, so the leak
        // is only ever wrong.)
        let new_base_override =
            method_name == "new" && self.has_user_method(receiver_class, method_name);
        let mu_base_override = matches!(method_name, "BUILDALL" | "POPULATE" | "clone")
            && self.has_user_method(receiver_class, method_name);
        // A user (or role-composed) override of a native container protocol
        // method on an `is Hash`/`is Array`/`is BagHash`-style subclass is the
        // same situation: the native behavior on the instance's backing storage
        // is a base candidate that is not a `MethodDef`, so without a frame the
        // override's `nextsame`/`callsame` answered Nil and the write was
        // dropped (`AccountableBagHash`'s `multi method ASSIGN-KEY`).
        let container_protocol_override = Self::is_container_protocol_method(method_name)
            && self.class_has_native_container_backing(receiver_class)
            && self.has_user_method_including_role(receiver_class, method_name);
        // A user method that shadows an ancestor's auto-generated public
        // attribute accessor of the same name (`has $.body = ""` in a parent,
        // `method body(...) { ...callwith()... }` in the child) needs an MRO
        // frame too: the accessor is registry metadata, not a `MethodDef`, so
        // it never appears among `matched_deferral_candidates` and a
        // single-user-candidate call would otherwise skip the frame entirely
        // — leaving `callwith`/`callsame`/`nextsame` with nothing to defer to
        // and silently answering Nil instead of reading the attribute
        // (Email::MIME's `Email::Simple` subclass overrides the auto `body`
        // reader this way).
        let accessor_owner = super::user_method_probe_memo::probe_key(method_name)
            .and_then(|name| self.first_public_accessor_owner(receiver_class, name));
        let accessor_base_override =
            accessor_owner.is_some() && self.has_user_method(receiver_class, method_name);
        // A user method on a subclass of a builtin metamodel HOW (OO::Monitors'
        // `MonitorHOW.new_type`) is the same situation once more: its base
        // candidate is the native metamethod (`native_metamodel_next_candidate`),
        // not a `MethodDef`. Without a frame of its own, a `callsame` in it
        // resolved against an enclosing routine's live frame whenever there was
        // one -- `use-ok 'Terminal::ANSI'` loads a `monitor` from inside the
        // `multi sub use-ok`, whose multi frame answered instead, handing back
        // the wrong value and breaking the class declaration.
        let metamodel_base_override = self.is_metamodel_how_class(receiver_class)
            && self.has_user_method(receiver_class, method_name);
        let native_base_override = grammar_parse_override
            || metamodel_base_override
            || mu_base_override
            || new_base_override
            || container_protocol_override
            || accessor_base_override;
        // Fast path: a name with at most one *structural* dispatch candidate across
        // the MRO can never produce a deferral frame (arg-matching only reduces the
        // candidate count), so skip the per-call `resolve_all_methods_with_owner`
        // MRO walk + MethodDef clones. The structural shape depends only on
        // (class, method), so it is memoized in `dispatch_multi_candidate` and
        // invalidated with the other method caches on any registry change.
        if !native_base_override
            && !self.has_multiple_dispatch_candidates(receiver_class, method_name)
        {
            return None;
        }
        // ADR-0019 E9a: the flat deferral expansion (`resolution_deferral.rs`) replaces
        // `resolve_all_methods_with_owner` as the ordering source — see its module doc for why
        // a raw MRO walk in declaration order does not reproduce raku's own deferral order once
        // a `multi method` spans MRO levels. The expansion is structural (unfiltered); apply the
        // same per-call argument match as `resolve_all_methods_with_owner` used to
        // apply internally, with the actual invocant available to `where` clauses.
        let all_candidates =
            self.matched_deferral_candidates(receiver_class, method_name, &args, &invocant);
        // Fast path: with zero or one candidate there is nothing to defer to, so no
        // dispatch frame is ever pushed (the single candidate is the chosen one and
        // gets skipped, leaving `remaining` empty). Returning early here avoids the
        // per-call `function_body_fingerprint` work below — which Debug-traverses the
        // whole method body AST to derive a candidate identity — for the overwhelmingly
        // common single-method case. Mirrors `push_multi_dispatch_frame`'s `<= 1` guard.
        // A grammar parse / Mu-base override still pushes a frame (empty
        // `remaining`) so its `nextsame`/`nextwith` reaches the native fallback.
        if !native_base_override && all_candidates.len() <= 1 {
            return None;
        }
        // Identify the chosen candidate and skip exactly that one
        let chosen =
            self.resolve_method_with_owner_invocant(receiver_class, method_name, &args, &invocant);
        let chosen_fp = chosen
            .as_ref()
            .map(|(_, def)| self.method_def_fingerprint(def));
        let mut remaining: Vec<(Symbol, super::MethodDef)> = Vec::new();
        let mut skipped_chosen = false;
        for (owner, def) in all_candidates {
            let fp = self.method_def_fingerprint(&def);
            if !skipped_chosen && Some(fp) == chosen_fp {
                skipped_chosen = true;
                continue;
            }
            if self.should_skip_defer_method_candidate(receiver_class, owner.as_str()) {
                continue;
            }
            remaining.push((owner, def));
        }
        // ADR-0019 E8a shadow probe (zero behavior change): see
        // `todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`.
        self.shadow_check_deferral_sequence(
            receiver_class,
            method_name,
            &args,
            &invocant,
            chosen_fp,
            &remaining,
        );
        if remaining.is_empty() && !native_base_override {
            return None;
        }
        {
            let rw_params = chosen
                .as_ref()
                .map(|(_, def)| {
                    super::builtins_dispatch_next::rw_scalar_positional_params(&def.param_defs)
                })
                .unwrap_or_default();
            // ADR-0019 E9b-1: every entry is a plain Candidate — this builder
            // never wraps a method's own chain into the frame (that is E9b-2).
            let mut remaining: Vec<super::DeferralEntry> = remaining
                .into_iter()
                .map(|(owner, def)| super::DeferralEntry::Candidate {
                    owner,
                    def: Box::new(def),
                    wraps_spliced: false,
                })
                .collect();
            // The shadowed ancestor accessor (if any) is the terminal
            // candidate, exactly like `push_wrapped_accessor_dispatch_frame`'s
            // trailing entry — read directly by `dispatch_next_candidate`'s
            // `DeferralEntry::Accessor` arm since it has no `MethodDef`.
            if let Some(owner) = accessor_owner {
                remaining.push(super::DeferralEntry::Accessor {
                    owner,
                    name: method_name.to_string(),
                    want_container: false,
                });
            }
            Some(super::MethodDispatchFrame {
                receiver_class: receiver_class.to_string(),
                invocant,
                args,
                remaining,
                rw_params,
                dispatch_token,
                arg_sources: None,
                in_wrapper: false,
            })
        }
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
