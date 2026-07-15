//! Instance-method dispatch: resolve a candidate across the MRO, set up the
//! `nextsame`/`samewith` dispatch frame and method-wrap chain, run the resolved
//! candidate as compiled bytecode, and forward `handles`-delegation methods.
//! Class lifecycle/MRO lives in `class`; introspection in `class_introspection`.

use super::*;
use crate::symbol::Symbol;
use crate::value::AttrMap;

impl Interpreter {
    /// Render a dispatch-failure call profile the way rakudo does: positionals
    /// as their type name, named arguments as `:name(Type)` — falling back to
    /// `:Type` when the value's `.raku` dies (rakudo stringifies the value via
    /// `.raku` for the profile and degrades to the bare type on failure).
    pub(crate) fn format_call_arg_profile(&mut self, args: &[Value]) -> String {
        let mut parts = Vec::new();
        for a in args {
            match a.view() {
                ValueView::Pair(k, v) => {
                    parts.push(self.format_named_arg_profile(k, v));
                }
                ValueView::ValuePair(k, v) => {
                    let key = k.to_string_value();
                    parts.push(self.format_named_arg_profile(&key, v));
                }
                _ => parts.push(crate::value::types::what_type_name(a)),
            }
        }
        parts.join(", ")
    }

    fn format_named_arg_profile(&mut self, key: &str, val: &Value) -> String {
        let type_name = crate::value::types::what_type_name(val);
        if matches!(
            val.view(),
            ValueView::Instance { .. } | ValueView::Package(_)
        ) && self
            .call_method_with_values(val.clone(), "raku", vec![])
            .is_err()
        {
            return format!(":{}", type_name);
        }
        format!(":{}({})", key, type_name)
    }

    /// Map-in/map-out wrapper over [`Self::run_instance_method_celled`]: the
    /// historical `(value, post-method attribute map)` contract. The map is
    /// materialized from the invocant's live cell (or the entry snapshot for a
    /// cell-less invocant) only here — the celled core returns `None` on the
    /// common path so cell-threading callers (the construction phases) never
    /// pay the whole-map `to_map()`.
    pub(crate) fn run_instance_method(
        &mut self,
        receiver_class_name: &str,
        attributes: AttrMap,
        method_name: &str,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        let cell = invocant.as_ref().and_then(Self::self_instance_attrs);
        let fallback = if cell.is_none() {
            Some(attributes.clone())
        } else {
            None
        };
        let (v, reconciled) = self.run_instance_method_celled(
            receiver_class_name,
            &attributes,
            method_name,
            args,
            invocant,
        )?;
        let final_attrs = match reconciled {
            Some(m) => m,
            None => cell.map(|c| c.to_map()).or(fallback).unwrap_or_default(),
        };
        Ok((v, final_attrs))
    }

    /// Cell-authoritative instance-method dispatch: resolve across the MRO, set
    /// up the dispatch frame / wrap chain, run the candidate compiled. Returns
    /// `(value, None)` on the common path — the invocant's shared attribute
    /// cell already holds every attribute mutation — and `(value, Some(map))`
    /// only when the exit reconcile recovered a `:=`-bound attribute beyond the
    /// cell contents (the caller must commit that map to its cell).
    pub(crate) fn run_instance_method_celled(
        &mut self,
        receiver_class_name: &str,
        attributes: &AttrMap,
        method_name: &str,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, Option<AttrMap>), RuntimeError> {
        crate::vm::vm_stats::record_resolver_method_dispatch(method_name);
        let inv_value = if let Some(inv) = &invocant {
            inv.clone()
        } else if attributes.is_empty() {
            Value::package(crate::symbol::Symbol::intern(receiver_class_name))
        } else {
            Value::make_instance(
                crate::symbol::Symbol::intern(receiver_class_name),
                attributes.clone(),
            )
        };
        let Some((owner_class, method_def)) = self.resolve_method_with_owner_invocant(
            receiver_class_name,
            method_name,
            &args,
            &inv_value,
        ) else {
            // Distinguish X::Multi::NoMatch (method exists but no candidate
            // matched) from X::Method::NotFound (method does not exist at all,
            // e.g. submethod on ancestor only).
            let has_visible_method = self.class_mro(receiver_class_name).iter().any(|cn| {
                self.registry()
                    .classes
                    .get(cn.as_str())
                    .and_then(|c| c.methods.get(method_name))
                    .is_some_and(|ovs| {
                        let is_ancestor = cn.as_str() != receiver_class_name;
                        ovs.iter()
                            .any(|d| !d.is_private && (!d.is_my || !is_ancestor))
                    })
            });
            if has_visible_method {
                let sigs =
                    self.format_method_candidate_signatures(receiver_class_name, method_name);
                let profile = self.format_call_arg_profile(&args);
                let concrete = !matches!(inv_value.view(), ValueView::Package(_));
                return Err(
                    super::methods_signature_errors::make_multi_no_match_error_detailed(
                        method_name,
                        receiver_class_name,
                        concrete,
                        &profile,
                        &sigs,
                    ),
                );
            }
            let type_name = receiver_class_name.to_string();
            return Err(
                super::methods_signature_errors::make_method_not_found_error(
                    method_name,
                    &type_name,
                    false,
                ),
            );
        };
        // Ambiguous multi dispatch: two or more candidates were equally
        // specific. Raise X::Multi::Ambiguous rather than silently choosing.
        if self.dispatch_ambiguous {
            self.dispatch_ambiguous = false;
            let sigs = self.format_method_candidate_signatures(receiver_class_name, method_name);
            return Err(super::methods_signature_errors::make_multi_ambiguous_error(
                method_name,
                receiver_class_name,
                &sigs,
            ));
        }
        // Helper to build remaining candidates, skipping the chosen one.
        // Submethod prefilter: a submethod is never inherited, so when the
        // visible-candidate table holds at most one entry the remaining list is
        // empty by construction — skip the full `resolve_all_methods_with_owner`
        // pass (per-MRO-class overload-Vec clones + signature matching +
        // fingerprints) that every construction-phase BUILD/TWEAK dispatch
        // otherwise pays just to learn "nothing to defer to".
        let skip_remaining = (method_def.is_my || method_def.is_submethod)
            && self.count_visible_method_candidates(receiver_class_name, method_name) <= 1;
        let build_remaining = |this: &mut Self,
                               method_def: &MethodDef|
         -> Vec<(String, MethodDef)> {
            if skip_remaining {
                return Vec::new();
            }
            let all = this.resolve_all_methods_with_owner(receiver_class_name, method_name, &args);
            let chosen_fp = this.method_def_fingerprint(method_def);
            let mut remaining = Vec::new();
            let mut skipped = false;
            for (owner, def) in all {
                let fp = this.method_def_fingerprint(&def);
                if !skipped && fp == chosen_fp {
                    skipped = true;
                    continue;
                }
                if this.should_skip_defer_method_candidate(receiver_class_name, owner.as_str()) {
                    continue;
                }
                remaining.push((owner.resolve(), def));
            }
            remaining
        };
        let make_invocant_for_dispatch =
            |invocant: &Option<Value>, attributes: &AttrMap| -> Value {
                if let Some(inv) = invocant {
                    inv.clone()
                } else if attributes.is_empty() {
                    Value::package(Symbol::intern(receiver_class_name))
                } else {
                    Value::make_instance(Symbol::intern(receiver_class_name), attributes.clone())
                }
            };
        // Check for method-level wrap chain on this candidate. The
        // `has_any_wrap_chains` prefilter matters: without it every slow-path
        // instance call (e.g. every bless/TWEAK) pays a candidate-index scan
        // even though no `.wrap` exists in the whole program (mirrors the
        // compiled path's guard in vm_call_method_compiled.rs).
        if self.has_any_wrap_chains()
            && !self.is_inside_wrap_dispatch()
            && let Some(cand_idx) =
                self.find_method_candidate_index(owner_class.as_str(), method_name, &method_def)
            && let Some(chain) = self
                .get_method_wrap_chain(owner_class.as_str(), method_name, cand_idx)
                .cloned()
        {
            let invocant_for_dispatch = make_invocant_for_dispatch(&invocant, attributes);
            let remaining = build_remaining(self, &method_def);
            let pushed_dispatch = !remaining.is_empty();
            self.push_method_samewith_context(
                receiver_class_name,
                method_name,
                &args,
                Some(invocant_for_dispatch.clone()),
            );
            if pushed_dispatch {
                let rw_params = super::builtins_dispatch_next::rw_scalar_positional_params(
                    &method_def.param_defs,
                );
                self.method_dispatch_stack.push(MethodDispatchFrame {
                    receiver_class: receiver_class_name.to_string(),
                    invocant: invocant_for_dispatch,
                    args: args.clone(),
                    remaining,
                    rw_params,
                });
            }
            let mut orig_env = crate::env::Env::new();
            orig_env.insert("__mutsu_method_wrap_original".to_string(), Value::TRUE);
            let original_sub = Value::make_sub(
                owner_class,
                Symbol::intern(method_name),
                method_def.params.clone(),
                method_def.param_defs.clone(),
                (*method_def.body).clone(),
                method_def.is_rw,
                orig_env,
            );
            let outermost = chain.last().unwrap().1.clone();
            let mut wrap_remaining: Vec<Value> = Vec::new();
            for i in (0..chain.len() - 1).rev() {
                wrap_remaining.push(chain[i].1.clone());
            }
            wrap_remaining.push(original_sub);
            let mut call_args = vec![inv_value.clone()];
            call_args.extend(args);
            let frame = WrapDispatchFrame {
                sub_id: 0,
                remaining: wrap_remaining,
                args: call_args.clone(),
            };
            let wrapper_id = if let ValueView::Sub(wd) = outermost.view() {
                Some(wd.id)
            } else {
                None
            };
            self.wrap_dispatch_stack.push(frame);
            let result = self.call_sub_value(outermost, call_args, false);
            self.wrap_dispatch_stack.pop();
            // Propagate closure variable mutations from the wrapper back to
            // the current env so captured variables are visible to the caller.
            if let Some(wid) = wrapper_id
                && let Some(persisted) = self.closure_env_overrides.get(&wid).cloned()
            {
                for (k, v) in persisted.iter() {
                    if self.env.contains_key_sym(*k) {
                        self.env.insert_sym(*k, v.clone());
                    }
                }
            }
            self.pop_method_samewith_context();
            if pushed_dispatch {
                self.method_dispatch_stack.pop();
            }
            // The wrapped call mutated attributes (if any) through the live
            // cell; there is no `:=` reconcile on this path.
            return result.map(|v| (v, None));
        }
        let invocant_for_dispatch = make_invocant_for_dispatch(&invocant, attributes);
        let remaining = build_remaining(self, &method_def);
        let pushed_dispatch = !remaining.is_empty();
        self.push_method_samewith_context(
            receiver_class_name,
            method_name,
            &args,
            Some(invocant_for_dispatch.clone()),
        );
        if pushed_dispatch {
            let rw_params =
                super::builtins_dispatch_next::rw_scalar_positional_params(&method_def.param_defs);
            self.method_dispatch_stack.push(MethodDispatchFrame {
                receiver_class: receiver_class_name.to_string(),
                invocant: invocant_for_dispatch,
                args: args.clone(),
                remaining,
                rw_params,
            });
        }
        // Check for `is DEPRECATED` trait on the method
        if let Some(ref msg) = method_def.deprecated_message {
            let cl = self.test_pending_callsite_line;
            self.check_deprecation_for_method_with_line(method_name, owner_class.as_str(), msg, cl);
        }
        // §B: run the resolved (non-wrapped) candidate as compiled bytecode via the
        // VM-native `call_compiled_method` instead of the tree-walk
        // `run_instance_method_resolved` recompile-each-call path. The MRO frame is
        // already pushed above, so the candidate's own `nextsame`/`callsame`
        // continues this chain. This is the hot path for multi-method dispatch and
        // `samewith` re-dispatch reached through `call_method_with_values`
        // (roast S12-methods/defer-next.t: method `m` x10000). Delegation forwarders
        // (synthesized, `compiled_code = None`) and any other uncompiled method keep
        // the interpreter path.
        let result = self.run_resolved_method_celled(
            receiver_class_name,
            owner_class.as_str(),
            method_name,
            method_def,
            attributes,
            args,
            invocant,
        );
        self.pop_method_samewith_context();
        if pushed_dispatch {
            self.method_dispatch_stack.pop();
        }
        result
    }

    /// §B: run an already-resolved method/submethod candidate as compiled bytecode
    /// (`call_compiled_method`). A candidate with no `compiled_code` is compiled
    /// on-demand in place first (`compile_method_def_in_place`), so the only thing that
    /// still reaches `run_instance_method_resolved` is a `handles`-delegation forwarder
    /// (#3658 — its former tree-walk method-execution arm has been deleted). Same
    /// `(result, attrs-to-commit)` contract at every resolved-candidate call site (the
    /// `run_instance_method` general dispatch and the construction/destruction
    /// BUILD/TWEAK/DESTROY runners).
    ///
    /// `pending_rw_writeback_sources` is merged (not restored) around the call so a
    /// sibling `submethod BUILD { $o++ }` write queued for an outer `.new` survives a
    /// nested `.new` (#3620) and the body's own captured-outer writes also propagate. A
    /// `submethod` that `fail`ed comes back as an unhandled Failure *value*; re-raise it
    /// as `Err(is_fail)` so the construction sites' fail-to-Failure handling is unchanged
    /// (a non-submethod keeps a Failure as a legitimate return). Attributes are committed
    /// from the live cell (unwrapping a `Mixin` self to its inner instance) unless
    /// `:=`-adjusted.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn run_resolved_method_compiled_or_treewalk(
        &mut self,
        receiver_class_name: &str,
        owner_class: &str,
        method_name: &str,
        method_def: MethodDef,
        attributes: AttrMap,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        let cell = invocant.as_ref().and_then(Self::self_instance_attrs);
        // Non-instance invocant (no live cell): keep the entry snapshot as the
        // unadjusted fallback map, mirroring the pre-Option return contract.
        let fallback = if cell.is_none() {
            Some(attributes.clone())
        } else {
            None
        };
        let (v, reconciled) = self.run_resolved_method_celled(
            receiver_class_name,
            owner_class,
            method_name,
            method_def,
            &attributes,
            args,
            invocant,
        )?;
        // Read the committed attribute map from the live cell of `self`,
        // unwrapping a `Mixin` invocant to its inner instance (so a
        // runtime-`does` mixin method's attribute mutations are captured,
        // not a stale entry snapshot). A `Some` reconcile keeps the
        // `:=`-recovered snapshot.
        let final_attrs = match reconciled {
            Some(m) => m,
            None => cell.map(|c| c.to_map()).or(fallback).unwrap_or_default(),
        };
        Ok((v, final_attrs))
    }

    /// Cell-authoritative core of [`Self::run_resolved_method_compiled_or_treewalk`]:
    /// `(value, None)` on the common path (the invocant's shared cell holds every
    /// attribute mutation); `(value, Some(map))` when the exit reconcile recovered a
    /// `:=`-bound attribute (or a delegation forwarder rebuilt the map) — the caller
    /// commits that map to its cell.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn run_resolved_method_celled(
        &mut self,
        receiver_class_name: &str,
        owner_class: &str,
        method_name: &str,
        method_def: MethodDef,
        attributes: &AttrMap,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, Option<AttrMap>), RuntimeError> {
        // Writeback-safety gate (§B, #3658 step 4 — free_var_writes filter REMOVED).
        // Any resolved candidate that has compiled bytecode and is not a delegation
        // forwarder now runs compiled, regardless of what free vars it writes:
        //   - captured-outer *lexical* writes (`method m { $outer++ }`) propagate via
        //     `call_compiled_method` queuing `pending_rw_writeback_sources` + the
        //     caller drain MERGING it (#3664);
        //   - dynamic (`$*x`) / captured-outer writes from a nested CLOSURE inside
        //     the body (the grammar reduce-time action `method delim { my $f = { $*L =
        //     '<' }; $f() }`) propagate now that the method fast-path `can_skip_merge`
        //     also gates on `has_calls` (#3670) — so the closure's env write is merged
        //     back instead of dropped;
        //   - attribute twigils (`.count`/`!x`/…) are not in `free_var_writes` at all
        //     (#3666).
        // A delegation forwarder is synthesized with `compiled_code = None`, so it
        // falls through to `forward_resolved_delegation` (the remaining reason it
        // still exists).
        // On-demand compile (§B, #3658): a resolved candidate reached before its
        // owner's registration compile pass — or one added at runtime (a role method
        // punned via `does`, a custom-HOW method) — can still have no `compiled_code`.
        // Compile this exact candidate's body IN PLACE (not via re-resolution, which
        // would mis-pick an override for a qualified `$obj.Class::meth` call) so it
        // runs compiled instead of tree-walked. Only a genuinely body-less method
        // (stub / delegation forwarder) then stays on the tree-walk path. The
        // compiled-execution Mixin/instance attribute writeback is handled by
        // `self_instance_attrs` (unwraps a `Mixin` self to the inner cell) in
        // the attr ops and the `final_attrs` commit below.
        let mut method_def = method_def;
        if method_def.compiled_code.is_none() && method_def.delegation.is_none() {
            let dist = self.resolve_package_distribution(owner_class);
            Self::compile_method_def_in_place_with_dist(&mut method_def, owner_class, dist);
        }
        let writeback_safe_compiled =
            method_def.compiled_code.is_some() && method_def.delegation.is_none();
        if !writeback_safe_compiled {
            // A delegation forwarder threads the map functionally (it may insert
            // an updated delegate); surface it as `Some` so the caller commits.
            return self
                .forward_resolved_delegation(
                    receiver_class_name,
                    owner_class,
                    method_def,
                    attributes.clone(),
                    args,
                    invocant,
                )
                .map(|(v, updated)| (v, Some(updated)));
        }
        let cc = method_def.compiled_code.clone().unwrap();
        let empty_fns = crate::opcode::CompiledFns::default();
        let saved_pending = std::mem::take(&mut self.pending_rw_writeback_sources);
        let call_result = self.call_compiled_method(
            receiver_class_name,
            owner_class,
            method_name,
            &method_def,
            &cc,
            attributes,
            args,
            invocant,
            &empty_fns,
        );
        // MERGE the saved sibling writes (e.g. a sibling BUILD's captured-outer
        // write queued for the outer `.new` caller to drain, #3620) with the
        // body's OWN captured-outer writes that `call_compiled_method` recorded
        // on exit — rather than RESTORING `saved_pending`, which would discard
        // the body's writes (so a method's `$outer++` would be lost). With the
        // `free_var_writes.is_empty()` writeback-safety gate above on, the body
        // records nothing, so this is inert; it becomes correct once the gate is
        // relaxed for captured-outer-writing bodies.
        let mut merged = saved_pending;
        for src in std::mem::take(&mut self.pending_rw_writeback_sources) {
            if !merged.contains(&src) {
                merged.push(src);
            }
        }
        self.pending_rw_writeback_sources = merged;
        match call_result {
            Ok((v, reconciled)) => {
                if method_def.is_submethod
                    && let Some(mut err) = self.failure_to_runtime_error_if_unhandled(&v)
                {
                    err.control = Some(crate::value::Control::Fail);
                    return Err(err);
                }
                Ok((v, reconciled))
            }
            Err(e) => Err(e),
        }
    }

    /// Forward a `handles`-delegation method to its delegate. (§B #3658: the former
    /// tree-walk method-execution arm — the `run_block` of the user method body —
    /// has been deleted. Every non-delegation candidate is now compiled on-demand by
    /// its caller (`compile_method_def_in_place`) before reaching here, so this only
    /// handles delegation forwarders.)
    pub(super) fn forward_resolved_delegation(
        &mut self,
        receiver_class_name: &str,
        owner_class: &str,
        method_def: MethodDef,
        attributes: AttrMap,
        args: Vec<Value>,
        invocant: Option<Value>,
    ) -> Result<(Value, AttrMap), RuntimeError> {
        if let Some((ref attr_var_name, ref target_method)) = method_def.delegation {
            // Clear skip_pseudo_method_native: the outer call set it for the
            // delegator's own method name, but we're about to forward to a
            // different name on a (possibly different) target, so the flag
            // must not leak into the delegate dispatch.
            let saved_skip_pseudo = self.skip_pseudo_method_native.take();
            // Method-based delegation: attr_var_name starts with `&`, meaning
            // the delegate is obtained by invoking the named method on self.
            let delegate = if let Some(source_method) = attr_var_name.strip_prefix('&') {
                let invocant_val = if let Some(ref inv) = invocant {
                    inv.clone()
                } else if attributes.is_empty() {
                    Value::package(Symbol::intern(receiver_class_name))
                } else {
                    Value::make_instance(Symbol::intern(receiver_class_name), attributes.clone())
                };
                self.call_method_with_values(invocant_val, source_method, Vec::new())?
            } else {
                let attr_key = attr_var_name
                    .trim_start_matches('.')
                    .trim_start_matches('!');
                attributes.get(attr_key).cloned().unwrap_or(Value::NIL)
            };
            let is_method_based = attr_var_name.starts_with('&');
            let attr_key = attr_var_name
                .trim_start_matches('&')
                .trim_start_matches('.')
                .trim_start_matches('!');
            if delegate == Value::NIL {
                return Err(RuntimeError::new(format!(
                    "No such method '{}' for invocant of type '{}'",
                    target_method, receiver_class_name
                )));
            }
            let delegate_id = match delegate.view() {
                ValueView::Instance { id, .. } => Some(id),
                _ => None,
            };
            let delegate_class = match delegate.view() {
                ValueView::Instance { class_name, .. } => Some(class_name),
                _ => None,
            };
            let result = self.call_method_with_values(delegate, target_method, args)?;
            // Restore the saved skip_pseudo flag so the outer caller is unaffected.
            self.skip_pseudo_method_native = saved_skip_pseudo;
            // For Instance delegates, check if the delegate was mutated and update
            // the frontend's attribute with the updated delegate.
            if !is_method_based && let (Some(did), Some(dcn)) = (delegate_id, delegate_class) {
                // Look for the updated delegate in env bindings
                let mut updated_delegate = None;
                for val in self.env.values() {
                    if let ValueView::Instance { class_name, id, .. } = val.view()
                        && class_name == dcn
                        && id == did
                    {
                        updated_delegate = Some(val.clone());
                        break;
                    }
                }
                if let Some(updated) = updated_delegate {
                    let mut attrs = attributes;
                    attrs.insert(attr_key.to_string(), updated);
                    return Ok((result, attrs));
                }
            }
            return Ok((result, attributes));
        }
        // §B (#3658): the non-delegation tree-walk method-execution arm (the
        // `run_block` of the user method body) has been DELETED. Every caller now
        // compiles a non-delegation candidate on-demand
        // (`compile_method_def_in_place`) before reaching here, so this function is
        // only entered for a delegation forwarder (handled above). A non-delegation
        // method reaching this point would be an internal invariant violation.
        let _ = (&owner_class, &attributes, &args, &invocant);
        Err(RuntimeError::new(format!(
            "internal error: forward_resolved_delegation reached for a non-delegation method on '{}' (should have been compiled on-demand)",
            receiver_class_name
        )))
    }
}
