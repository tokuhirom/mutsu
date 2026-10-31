//! Materialize a method call's deferral frame from its resolved winner.
use super::*;

impl Interpreter {
    /// The deferral frame a method call establishes. With no next candidate,
    /// its empty `remaining` list still separates this call from its caller.
    // Cost: O(c) in the candidates the deferral expansion collects (each
    // matched against the args once); the single-candidate fast path is O(1)
    // amortized (the MRO probes are memoized per `(class, method)` for one
    // registry write generation).
    pub(super) fn build_method_dispatch_frame(
        &mut self,
        receiver_class: &str,
        method_name: &str,
        args: Vec<Value>,
        invocant: Value,
        dispatch_token: u64,
        winner: Option<super::method_dispatch_lazy::MethodDispatchWinner>,
    ) -> super::MethodDispatchFrame {
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
        // implementation (`native_mu_base`: the built instance
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
            method_name == "new" && self.has_user_method_including_role(receiver_class, method_name);
        // `Mu`'s own answers (`defined`, `Bool`, `so`, `not`, `WHICH`, `WHERE`, and `gist`
        // and `perl` everywhere, `Str`/`raku` of a type object) are the base candidate too.
        let mu_base_override = (matches!(method_name, "BUILDALL" | "POPULATE" | "clone")
            // A container subclass (`is Array`, `is Hash`) defers to its storage's
            // answer instead (`container_protocol_override`).
            || (!Self::invocant_carries_native_storage(&invocant)
                && (matches!(
                    method_name,
                    "defined" | "Bool" | "so" | "not" | "WHICH" | "WHERE" | "gist" | "perl" | "say"
                        | "print" | "put" | "note"
                ) || (matches!(method_name, "Str" | "raku")
                    && matches!(invocant.view(), ValueView::Package(_)))
                    // `Mu.Numeric` / `Mu.Int`: the last candidate of a user
                    // `Numeric`/`Int` on a class with no number behind it (a
                    // subclass of `Int` defers to `Int`'s own answer).
                    || (matches!(method_name, "Numeric" | "Int")
                        && matches!(
                            invocant.view(),
                            ValueView::Package(_) | ValueView::Instance { .. }
                        )
                        && crate::value::numeric_payload::numeric_subclass_payload(&invocant)
                            .is_none()))))
            && self.has_user_method_including_role(receiver_class, method_name);
        // A user (or role-composed) override of a native container protocol
        // method on an `is Hash`/`is Array`/`is BagHash`-style subclass is the
        // same situation: the native behavior on the instance's backing storage
        // is a base candidate that is not a `MethodDef`, so without a frame the
        // override's `nextsame`/`callsame` answered Nil and the write was
        // dropped (`AccountableBagHash`'s `multi method ASSIGN-KEY`).
        // `Mu`'s own names (`new`, `BUILDALL`, `POPULATE`, `clone`) are answered
        // by the `Mu` base candidate above the storage's: `Array.new` on the
        // backing array is not what `nextwith` from a user `new` reaches.
        let container_protocol_override = Self::invocant_carries_native_storage(&invocant)
            && !matches!(method_name, "new" | "BUILDALL" | "POPULATE" | "clone")
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
        // candidate is the native metamethod (`native_metamodel_base`),
        // not a `MethodDef`. Without a frame of its own, a `callsame` in it
        // resolved against an enclosing routine's live frame whenever there was
        // one -- `use-ok 'Terminal::ANSI'` loads a `monitor` from inside the
        // `multi sub use-ok`, whose multi frame answered instead, handing back
        // the wrong value and breaking the class declaration.
        let metamodel_base_override = self.is_metamodel_how_class(receiver_class)
            && self.has_user_method(receiver_class, method_name);
        // A method the user `augment`ed onto a core type: the builtin of the
        // receiver's type is the last candidate (`DeferralEntry::Native`).
        let core_type_override = self.core_type_receiver_has_user_override(&invocant, method_name);
        // A user `gist`/`Str`/`raku` on an instance: the default rendering is
        // the last candidate (`DeferralEntry::Native`, `any_base_native_entry`).
        let any_base_override = matches!(method_name, "gist" | "Str" | "raku")
            && matches!(invocant.view(), ValueView::Instance { .. })
            && self.has_user_method(receiver_class, method_name);
        // A grammar's own `method ws` / `alpha` / ...: the built-in rule on the
        // cursor is the last candidate (`DeferralEntry::Native`).
        // A user method on a class whose MRO reaches a built-in method of the
        // same name (`class S is Str { method uc { callsame } }`, `class I is
        // Int { method succ { nextsame } }`): that built-in is a real
        // candidate in Rakudo's MRO, so it is the last one here, run on the
        // receiver with the override hidden (ADR-11276 slice 4). Without it a
        // deferral out of the override answered Nil.
        // The receiver is an instance or a built-in value its class wraps (an
        // `is List` subclass's list); a type object and a role mixin have
        // their own bases.
        let builtin_ancestor_override = !matches!(
            invocant.view(),
            ValueView::Package(_) | ValueView::Mixin(..)
        ) && !Self::invocant_carries_native_storage(&invocant)
            && self.has_user_method(receiver_class, method_name)
            && {
                let mro = self.class_mro(receiver_class);
                Self::builtin_method_level(&mro, method_name).is_some()
            };
        let grammar_rule_override = super::regex::regex_builtin_rule::is_builtin_rule_name(method_name)
            && matches!(invocant.view(), ValueView::Instance { .. })
            && self.class_is_grammar(receiver_class)
            && self.has_user_method(receiver_class, method_name);
        let how_receiver = metamodel_base_override
            || (matches!(invocant.view(), ValueView::Mixin(..))
                && Self::how_target_from_value(&invocant).is_some()
                && self.has_user_method_including_role(receiver_class, method_name));
        let native_base_override = grammar_parse_override
            || how_receiver
            || grammar_rule_override
            || core_type_override
            || any_base_override
            || metamodel_base_override
            || mu_base_override
            || new_base_override
            || container_protocol_override
            || builtin_ancestor_override
            || accessor_base_override;
        // Fast path: a name with at most one *structural* dispatch candidate across
        // the MRO has no user candidate to defer to (arg-matching only reduces the
        // candidate count), so skip the per-call `resolve_all_methods_with_owner`
        // MRO walk + MethodDef clones. It still needs an empty boundary so a
        // deferral in this method cannot consume an enclosing call's frame.
        // The structural shape depends only on
        // (class, method), so it is memoized in `dispatch_multi_candidate` and
        // invalidated with the other method caches on any registry change.
        if !native_base_override
            && !self.has_multiple_dispatch_candidates(receiver_class, method_name)
        {
            return Self::empty_method_dispatch_frame(
                receiver_class,
                invocant,
                args,
                dispatch_token,
            );
        }
        // ADR-0019 E9a: the flat deferral expansion (`resolution_deferral.rs`) replaces
        // `resolve_all_methods_with_owner` as the ordering source — see its module doc for why
        // a raw MRO walk in declaration order does not reproduce raku's own deferral order once
        // a `multi method` spans MRO levels. The expansion is structural (unfiltered); apply the
        // same per-call argument match as `resolve_all_methods_with_owner` used to
        // apply internally, with the actual invocant available to `where` clauses.
        let winner = winner.or_else(|| {
            self.resolve_method_with_owner_invocant(receiver_class, method_name, &args, &invocant)
                .map(|(owner, def)| super::method_dispatch_lazy::MethodDispatchWinner {
                    owner,
                    fingerprint: self.method_def_fingerprint(&def),
                    rw_params: super::builtins_dispatch_next::rw_scalar_positional_params(&def.param_defs),
                })
        });
        let chosen_fp = winner.as_ref().map(|winner| winner.fingerprint);
        let all_candidates = self.matched_deferral_candidates(
            receiver_class, method_name, &args, &invocant,
            winner.as_ref().map(|winner| (winner.owner, winner.fingerprint)),
        );
        // Fast path: with zero or one candidate there is nothing to defer to (the
        // single candidate is the chosen one and gets skipped). Returning early
        // with an empty boundary avoids the
        // per-call `function_body_fingerprint` work below — which Debug-traverses the
        // whole method body AST to derive a candidate identity — for the overwhelmingly
        // common single-method case. Mirrors `push_multi_dispatch_frame`'s `<= 1` guard.
        // A grammar parse / Mu-base override still pushes a frame (empty
        // `remaining`) so its `nextsame`/`nextwith` reaches the native fallback.
        if !native_base_override && all_candidates.len() <= 1 {
            return Self::empty_method_dispatch_frame(
                receiver_class,
                invocant,
                args,
                dispatch_token,
            );
        }
        // Identify the chosen candidate and skip exactly that one
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
            chosen_fp,
            &remaining,
        );
        if remaining.is_empty() && !native_base_override {
            return Self::empty_method_dispatch_frame(
                receiver_class,
                invocant,
                args,
                dispatch_token,
            );
        }
        {
            let rw_params = winner
                .map(|winner| winner.rw_params)
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
            // The bridge, in the order the former advance-time probe tried them.
            let native_base = if grammar_parse_override {
                Some(super::NativeBase::GrammarParse)
            } else if mu_base_override || new_base_override {
                Some(super::NativeBase::MuBase)
            } else if how_receiver {
                Some(super::NativeBase::Metamodel)
            } else if grammar_rule_override {
                Some(super::NativeBase::GrammarRule)
            } else if container_protocol_override {
                Some(super::NativeBase::Storage)
            } else if core_type_override
                || any_base_override
                || (Self::plain_io_handle_view(&invocant).is_some()
                    && self.has_user_method(receiver_class, method_name))
            {
                Some(super::NativeBase::Value)
            } else if builtin_ancestor_override {
                Some(super::NativeBase::BuiltinAncestor)
            } else {
                None
            };
            if let Some(base) = native_base {
                remaining.push(super::DeferralEntry::Native {
                    name: method_name.to_string(),
                    base,
                });
            }
            super::MethodDispatchFrame {
                receiver_class: receiver_class.to_string(),
                invocant,
                args,
                remaining,
                rw_params,
                dispatch_token,
                arg_sources: None,
                in_wrapper: false,
                lazy_tail: None,
            }
        }
    }

    // Cost: O(1), independent of the enclosing dispatcher depth.
    pub(super) fn empty_method_dispatch_frame(
        receiver_class: &str,
        invocant: Value,
        args: Vec<Value>,
        dispatch_token: u64,
    ) -> super::MethodDispatchFrame {
        super::MethodDispatchFrame {
            receiver_class: receiver_class.to_string(),
            invocant,
            args,
            remaining: Vec::new(),
            rw_params: Vec::new(),
            dispatch_token,
            arg_sources: None,
            in_wrapper: false,
            lazy_tail: None,
        }
    }
}

