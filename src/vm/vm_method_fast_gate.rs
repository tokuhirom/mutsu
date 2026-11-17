use super::vm_method_call_attrs::CallAttrs;
use super::*;

/// The verdict of [`Interpreter::method_fast_gate`].
#[derive(Debug, Clone, Copy)]
pub(crate) struct MethodFastGate {
    /// A parameter aliases the caller's container (`is rw`, `is raw`,
    /// sigilless, `does`-target): the caller's argument cells must be
    /// published before the binder runs.
    pub(crate) has_rw_params: bool,
    /// The call may drop the method's env overlay on exit instead of merging
    /// it into the caller's.
    pub(crate) can_skip_merge: bool,
    /// The receiver's attributes carry sigilless-alias metadata.
    pub(crate) has_attr_aliases: bool,
    /// The fast binder (`call_compiled_method_fast`) can bind this call.
    pub(crate) fast: bool,
}

impl Interpreter {
    /// Decide how a compiled method call binds: whether the fast binder can
    /// take it, and whether its exit may skip the env merge. The one
    /// implementation of that decision, shared by `call_compiled_method_in_unit`
    /// and the method-call plan's install (ADR-15737 D1,
    /// `vm_method_call_plan`).
    ///
    /// Reads only the method's signature and compiled code, the argument
    /// list, the receiver (`base`) and its attribute cell; writes nothing.
    // Cost: O(p * a), p = parameters, a = arguments.
    pub(crate) fn method_fast_gate(
        &mut self,
        method_def: &crate::runtime::MethodDef,
        cc: &CompiledCode,
        owner_class: &str,
        base: &Value,
        attributes: CallAttrs<'_>,
        args: &[Value],
    ) -> MethodFastGate {
        let mut fast = false;
        // Pre-compute whether we can skip the expensive env merge on exit.
        // `is raw` is included alongside `is rw`: an `is raw` param bound to an
        // lvalue argument also writes back to the caller (`bind_function_args_values`
        // adds it to `rw_bindings`), and that writeback lives only on the merge
        // path — so a raw-param method must not take the fast path / skip the merge
        // (mirrors how the sub dispatch treats rw and raw alike).
        // A sigilless `\p` is implicitly raw and aliases the caller's container
        // exactly like `is raw`, so it must keep the full binder path too — see
        // [`ParamDef::binds_caller_container`]. Skipping the binder for it is
        // what made `method set(\p) { $!s := p }` bind a value copy, so a later
        // `$!s = v` never reached the caller's variable.
        let has_rw_params = method_def
            .param_defs
            .iter()
            .any(|pd| pd.binds_caller_container() || pd.is_does_target())
            // A sigilless or `is raw` parameter whose argument is a value, a
            // shared cell, a Proxy or a promotable caller variable binds on
            // the fast path below (`vm_method_container_params.rs`).
            && !self.method_container_args_light(method_def, cc, args);
        // A plain (non-`is copy`) `@`/`%` positional param binds the caller's
        // container by alias (Raku readonly-container semantics): `.push`,
        // element assign, and whole-container `=` all propagate to the caller.
        // mutsu realizes this via the same exit-time writeback as `is rw`
        // (`bind_function_args_values` adds it to `rw_bindings`,
        // `apply_rw_bindings_to_env`/the merge writes it back). That writeback
        // lives only on the full path's `else` (merge) branch — so a method with
        // such a param must NOT take the fast path, and must NOT be allowed to
        // `can_skip_merge` (a `.push` mutation is not a static env write, so
        // `cc.has_env_writes` can be false while the container still changed).
        // Subs already route through their writeback; only the method fast path
        // dropped this. Excludes slurpy/named/`is copy`/attr-twigil params.
        //
        // A plain `@x`/`%x` (`FastParamCheck::light_aggregate_sigil`) handed an
        // untyped Array/Hash is the exception: assignment to a mutsu container
        // stores in place, so binding the caller's container itself already
        // propagates every one of those mutations, and the fast binder does
        // that (`method_aggregate_args_light`).
        let light_aggregates = Self::method_aggregate_args_light(method_def, args);
        let has_aliasable_container_params = !light_aggregates
            && method_def.param_defs.iter().any(|pd| {
                !pd.is_invocant
                    && !pd.slurpy
                    && !pd.double_slurpy
                    && !pd.named
                    && !pd.traits.iter().any(|t| t == "copy")
                    && pd.sub_signature.is_none()
                    && pd.name.len() > 1
                    && (pd.name.starts_with('@') || pd.name.starts_with('%'))
                    && !pd.name[1..].starts_with(['!', '.'])
            });
        // Slice 2d (method follow-up): an array/hash variable passed into a
        // plain readonly scalar `$` param shares the caller's container in Raku
        // (`method m($n) { $n.push }` mutates the caller's `@z`). The promotion
        // + rw-writeback lives only on the slow `bind_function_args_values`
        // path, so such a call must avoid the fast path and the merge skip —
        // mirroring how `has_aliasable_container_params` handles `@`/`%` params.
        let shares_scalar_container =
            self.method_shares_container_into_scalar_param(method_def, args);
        // `cc.has_calls` (not just `has_env_writes`): a body that calls a CLOSURE
        // (`my $f = { $*x = 1 }; $f()` — `CallOnValue`/`CallOnCodeVar`) or any other
        // call op NOT listed in `has_env_writes` (CallDefined) can
        // write a captured-outer lexical, dynamic var, or global into this frame's
        // env. Skipping the merge would silently drop that write (e.g. a grammar
        // action `method delim { my $f = { $*L = '<' }; $f() }` — the dynamic-var
        // change is lost). The closure dispatch already gates on
        // `has_calls || has_env_writes` (vm_closure_dispatch.rs); the method fast
        // path dropped `has_calls`. #3658.
        let can_skip_merge = !has_rw_params
            && !has_aliasable_container_params
            && !shares_scalar_container
            && !cc.has_env_writes
            && !cc.has_calls;

        // Whether any sigilless-attribute alias metadata is present. Computed
        // once: the fast-path gate below needs it, and the merge path's
        // `is_method_local` predicate uses it to skip the per-env-key
        // `attr_alias_local` scan over ALL attributes (O(env keys x attrs) on
        // every full-path method exit) in the overwhelmingly common no-alias
        // case.
        let has_attr_aliases = attributes.has_alias_meta();

        // Fast path: bypass env entirely and populate locals directly from
        // source data. Avoids the ~12μs Arc::make_mut deep clone that the
        // first env_mut() call triggers.
        if !has_rw_params && !has_aliasable_container_params && !shares_scalar_container {
            // A body that cannot see a role's parameters, its statics or the
            // class a `::?CLASS` invocant captured (`cc.ignores_role_context`),
            // over parameters typed with built-in types only, has nothing to
            // bind for them: the call keeps the fast path (#16215).
            let role_blind = cc.ignores_role_context()
                && method_def.param_defs.iter().all(|pd| {
                    pd.is_invocant
                        || (pd.type_capture.is_none()
                            && pd.type_constraint.as_deref().is_none_or(|tc| {
                                let base = tc
                                    .strip_suffix(":D")
                                    .or_else(|| tc.strip_suffix(":U"))
                                    .unwrap_or(tc);
                                crate::runtime::utils::is_known_type_constraint(base)
                            }))
                });
            let has_invocant_constraint = method_def.param_defs.iter().any(|pd| {
                pd.is_invocant && (pd.type_constraint.is_some() || pd.type_capture.is_some())
            }) && !Self::fast_invocant_constraints_trivial(
                method_def,
                base,
                owner_class,
                role_blind,
            );
            // A `::T` type capture has to be BOUND before the body runs, and the
            // fast path has no binder step that does it. It used to be kept out
            // by accident: the capture lived in `type_constraint`, so either
            // `has_invocant_constraint` or the fast path's own nominal type check
            // (which `::T` could never satisfy) bounced the call. Now that the
            // capture is a field of its own, the gate has to name it (#7984).
            let has_type_capture = method_def.param_defs.iter().any(|pd| {
                pd.captured_type_name().is_some()
                    && !(pd.captures_class_fixture() && pd.is_invocant && role_blind)
            });
            // Role composition bindings do not keep a call off the fast path:
            // the fast binder installs them too (`install_method_role_bindings`,
            // #17331).
            // Classify arguments the way the slow binder does
            // (`bind_function_args_values`): a string-keyed `Pair` (after VarRef
            // unwrap) is a named argument; everything else is positional. Two
            // shapes always keep the full path: a non-string-keyed `ValuePair`
            // (the main binder treats it as positional but the legacy
            // placeholder path as named — ambiguous), and an internal
            // `__mutsu_*` Pair (the slow path filters it before binding).
            let mut positional_arg_count = 0usize;
            let mut has_named_args = false;
            let mut has_unmodeled_pair_arg = false;
            for a in args {
                let u = a.unwrap_varref();
                if u.is_string_pair_value() {
                    has_named_args = true;
                    if let ValueView::Pair(key, _) = u.view()
                        && key.starts_with("__mutsu_")
                    {
                        has_unmodeled_pair_arg = true;
                    }
                } else if u.is_any_pair_value() || a.is_any_pair_value() {
                    has_unmodeled_pair_arg = true;
                } else {
                    positional_arg_count += 1;
                }
            }
            // S6 (bench-ctor): simple named/attributive params now bind on the
            // fast path (`named_param_fast_eligible` — plain readonly scalar
            // `:$x`, attributive `:$!x`/`:@!a`/`:%!h` via the live attribute
            // cell). Everything else named-shaped bails via this predicate.
            let base_is_instance = matches!(base.view(), ValueView::Instance { .. });
            let named_params_fast_ok = method_def.param_defs.iter().enumerate().all(|(idx, pd)| {
                if pd.is_invocant || !pd.named {
                    return true;
                }
                Self::named_param_fast_eligible(
                    pd,
                    args,
                    base_is_instance,
                    method_def.named_fast(idx),
                )
            });
            // A named `@`/`%`-container argument bound to a plain named scalar
            // param shares the caller's container (slow-path ContainerRef
            // promotion + exit writeback) — keep such calls on the full path
            // (mirrors the sub side's named-share gate).
            // A `Mu`-only named argument (`:x(Mu)`) fails an untyped `:$x`'s
            // implicit `Any` (#10878): the full binder raises that, so such a
            // call keeps the full path.
            let named_mu_only_arg = has_named_args
                && args.iter().any(|a| match a.unwrap_varref().view() {
                    ValueView::Pair(_, v) => !self.light_arg_is_any(v),
                    _ => false,
                });
            let named_container_share = has_named_args
                && self.method_shares_container_into_named_scalar_param(method_def, args);
            // Every named argument must have somewhere to land: a named slurpy
            // (`*%_` — implicit, or explicit) absorbing leftovers, or an
            // eligible named param consuming its key. An `is hidden` class's
            // methods get NO implicit `*%_` (`effective_method_param_defs`),
            // and the slow binder rejects an unexpected named argument for
            // them (roast S12-class/interface-consistency.t test 5:
            // `Bar.new.m1(1, :x)` must die) — the fast path must not silently
            // drop it, so such calls keep the full path.
            let named_args_all_land = !has_named_args
                || method_def
                    .param_defs
                    .iter()
                    .any(|pd| pd.slurpy && pd.name.starts_with('%'))
                || args.iter().all(|a| match a.unwrap_varref().view() {
                    ValueView::Pair(k, _) => method_def.param_defs.iter().any(|pd| {
                        pd.named && Self::named_param_fast_match_key(pd) == Some(k.as_str())
                    }),
                    _ => true,
                });
            let has_complex_params = method_def.param_defs.iter().any(|pd| {
                if pd.is_invocant {
                    return false;
                }
                // Allow the implicit %_ slurpy that every method gets
                if pd.slurpy && pd.name == "%_" {
                    return false;
                }
                // Named params are vetted by `named_params_fast_ok` above.
                if pd.named {
                    return false;
                }
                // A POSITIONAL attributive parameter (`$!x`/`@!a`) binds straight
                // to an attribute, i.e. it mutates `self` — so it is not
                // read-only and must take the full path (which mirrors it into
                // the shared cell and writes it back). The read-only fast path
                // drops the write. (NAMED attributive params write the live cell
                // on the fast path now — see `named_param_fast_eligible`.)
                if Self::attr_twigil_base(&pd.name).is_some() {
                    return true;
                }
                pd.slurpy
                    || pd.double_slurpy
                    || pd.where_constraint.is_some()
                    || pd.sub_signature.is_some()
                    || pd.outer_sub_signature.is_some()
                    || pd.code_signature.is_some()
                    || pd
                        .type_constraint
                        .as_ref()
                        .is_some_and(|tc| tc.contains('('))
            });
            // If a default expr needs evaluation and args are missing, fall back
            // (named-param defaults are handled by `named_param_fast_eligible`;
            // only positional params consume positional args here).
            let needs_default_eval = {
                let mut pos = 0;
                method_def.param_defs.iter().any(|pd| {
                    if pd.is_invocant || pd.named || pd.slurpy || pd.double_slurpy {
                        return false;
                    }
                    let result = pos >= positional_arg_count && pd.default.is_some();
                    pos += 1;
                    result
                })
            };
            // A required positional left unfilled (`method m($x){}; X.()`) must die
            // with too-few-positionals; the fast path would silently bind it to Nil.
            // Route such calls to the full path, which validates arity.
            let has_missing_required = {
                let mut pos = 0;
                method_def.param_defs.iter().any(|pd| {
                    if pd.is_invocant || pd.slurpy || pd.double_slurpy || pd.named {
                        return false;
                    }
                    let missing =
                        pos >= positional_arg_count && pd.default.is_none() && !pd.optional_marker;
                    pos += 1;
                    missing
                })
            };
            // Count positional params to check for arg count mismatch
            let positional_count = method_def
                .param_defs
                .iter()
                .filter(|pd| !pd.is_invocant && !pd.slurpy && !pd.double_slurpy && !pd.named)
                .count();
            let has_arg_mismatch = positional_arg_count > positional_count;

            if !has_unmodeled_pair_arg
                && named_params_fast_ok
                && named_args_all_land
                && !named_container_share
                && !named_mu_only_arg
                && !has_missing_required
                && !has_invocant_constraint
                && !has_type_capture
                && !has_attr_aliases
                && !has_complex_params
                && !needs_default_eval
                && !has_arg_mismatch
                && !Self::method_binds_seq_into_array_param(method_def, args)
                // A `Proxy` without a subclass cell has no attribute store to
                // read; a subclass instance's lives in the same kind of cell
                // an ordinary instance carries (`self_instance_attrs`).
                && !matches!(base.view(), ValueView::Proxy { subclass: None, .. })
            {
                fast = true;
            }
        }
        MethodFastGate {
            has_rw_params,
            can_skip_merge,
            has_attr_aliases,
            fast,
        }
    }
}
