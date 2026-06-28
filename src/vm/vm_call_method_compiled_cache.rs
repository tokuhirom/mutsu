use super::*;

impl Interpreter {
    /// Try compiled method fast path; fall back to interpreter.
    ///
    /// Wrapper that preserves the caller's env `self`: the inner dispatch can run
    /// an interpreted instance method, which binds `self` in env without the
    /// save/restore that `call_method_with_values` performs. Without this, a bare
    /// stringification (`"$obj"` → `StringConcat` → this) leaves the caller's
    /// `self` pointing at `$obj`, breaking a later `self` read in an enclosing
    /// nested sub (which resolves `self` from env via `GetSelfOrNoSelf`).
    /// Build a per-positional-arg type key for the sound multi-resolution cache.
    /// Returns `None` (do not cache) when any arg is value-/identity-dependent or
    /// autothreads (`Junction`), or is a container/named-pair arg, so dispatch can
    /// not be keyed on the positional types alone.
    pub(crate) fn multi_arg_type_keys(args: &[Value]) -> Option<Vec<crate::symbol::Symbol>> {
        let mut keys = Vec::with_capacity(args.len());
        for a in args {
            let key = match a {
                Value::Instance { class_name, .. } => *class_name,
                Value::Junction { .. }
                | Value::Mixin(..)
                | Value::Scalar(_)
                | Value::ContainerRef(_)
                | Value::Pair(..)
                | Value::ValuePair(..)
                | Value::Capture { .. } => return None,
                other => {
                    crate::symbol::Symbol::intern(crate::runtime::utils::value_type_name(other))
                }
            };
            keys.push(key);
        }
        Some(keys)
    }

    /// Whether a `(class, method)` is a MULTI whose dispatch is purely type+arity
    /// based — i.e. the resolved candidate is a function of the receiver class +
    /// method + positional arg types, so it is safe to cache in
    /// `multi_resolve_cache`. False for non-multi methods (the existing
    /// `method_resolve_cache` handles those) and for any multi with a value-/
    /// identity-dependent candidate (`where` / literal / subset / `:D`/`:U` smiley /
    /// coercion). Memoized per `(class, method)`.
    pub(crate) fn multi_dispatch_type_cacheable(
        &mut self,
        class_sym: crate::symbol::Symbol,
        method_sym: crate::symbol::Symbol,
        class_name: &str,
        method_name: &str,
    ) -> bool {
        if let Some(&c) = self.multi_type_cacheable.get(&(class_sym, method_sym)) {
            return c;
        }
        let mro = self.class_mro(class_name);
        let mut any_multi = false;
        let mut value_dependent = false;
        'outer: for cn in &mro {
            let Some(overloads) = self.registry().get_method_overloads(cn, method_name) else {
                continue;
            };
            for def in &overloads {
                if def.is_multi {
                    any_multi = true;
                }
                for pd in &def.param_defs {
                    if pd.where_constraint.is_some() || pd.literal_value.is_some() {
                        value_dependent = true;
                        break 'outer;
                    }
                    if let Some(tc) = &pd.type_constraint {
                        // `:D`/`:U`/`:_` smiley or `Int(Str)` coercion => value/identity
                        // dependent; a subset type carries an implicit `where`.
                        if tc.contains(':') || tc.contains('(') {
                            value_dependent = true;
                            break 'outer;
                        }
                        let base = tc.split(['[', ' ']).next().unwrap_or(tc.as_str());
                        if self.registry().subsets.contains_key(base) {
                            value_dependent = true;
                            break 'outer;
                        }
                    }
                }
            }
        }
        let cacheable = any_multi && !value_dependent;
        self.multi_type_cacheable
            .insert((class_sym, method_sym), cacheable);
        cacheable
    }

    /// Resolve a method, consulting the sound multi-resolution cache for a
    /// type+arity-deterministic multi (avoids the per-call MRO/specificity walk).
    /// Non-multi / uncacheable / un-keyable calls resolve fresh (the non-multi
    /// caches live at the call sites). An AMBIGUOUS multi resolution is never
    /// cached — it must re-raise `X::Multi::Ambiguous` on every call (a cache hit
    /// would not set `dispatch_ambiguous`).
    pub(crate) fn resolve_method_cached(
        &mut self,
        cn: &str,
        method: &str,
        class_sym: crate::symbol::Symbol,
        method_sym: crate::symbol::Symbol,
        args: &[Value],
        target: &Value,
    ) -> Option<(String, std::sync::Arc<crate::runtime::MethodDef>)> {
        // Non-multi resolution depends only on (class, method) — not on arg
        // types/values — so it can be memoized. This mirrors the cache hierarchy
        // already used by the interpret path (`vm_call_method_compiled_interpret`);
        // without it the compiled-mut hot path re-ran the full MRO/specificity
        // walk in `resolve_method_with_owner_invocant` on *every* call to a plain
        // (non-multi) method. Both caches are invalidated together at every
        // registry/type/module mutation site (see `method_resolve_cache.clear()`
        // / `last_method_resolve = None`).

        // 1. Monomorphic inline cache: single-entry check before any HashMap.
        if let Some((cc, cm, ref co, ref cd)) = self.last_method_resolve
            && cc == class_sym
            && cm == method_sym
            && !cd.is_multi
        {
            return Some((co.clone(), cd.clone()));
        }
        // 2. Non-multi HashMap cache.
        if let Some(hit) = self
            .method_resolve_cache
            .get(&(class_sym, method_sym))
            .cloned()
            && let Some((ref owner, ref def)) = hit
            && !def.is_multi
        {
            self.last_method_resolve = Some((class_sym, method_sym, owner.clone(), def.clone()));
            return hit;
        }
        // 3. Sound multi-method resolution cache (type+arity deterministic).
        if let Some(arg_keys) = Self::multi_arg_type_keys(args)
            && self.multi_dispatch_type_cacheable(class_sym, method_sym, cn, method)
        {
            let mkey = (class_sym, method_sym, arg_keys);
            if let Some(hit) = self.multi_resolve_cache.get(&mkey) {
                return hit.clone();
            }
            let resolved = loan_env!(
                self,
                resolve_method_with_owner_invocant(cn, method, args, target)
            );
            let resolved_arc = resolved.map(|(o, d)| (o, std::sync::Arc::new(d)));
            if !self.dispatch_ambiguous {
                self.multi_resolve_cache.insert(mkey, resolved_arc.clone());
            }
            return resolved_arc;
        }
        // 4. Resolve fresh; cache the result when it is non-multi.
        let resolved_arc = loan_env!(
            self,
            resolve_method_with_owner_invocant(cn, method, args, target)
        )
        .map(|(o, d)| (o, std::sync::Arc::new(d)));
        if resolved_arc.as_ref().is_none_or(|(_, def)| !def.is_multi) {
            self.method_resolve_cache
                .insert((class_sym, method_sym), resolved_arc.clone());
            if let Some((ref owner, ref def)) = resolved_arc {
                self.last_method_resolve =
                    Some((class_sym, method_sym, owner.clone(), def.clone()));
            }
        }
        resolved_arc
    }

    /// Compile a resolved user method's body on demand when it lacks bytecode,
    /// then dispatch as bytecode instead of through the interpreter bridge.
    /// Almost all user methods are already compiled at class registration
    /// (`compile_class_methods`); the gap this closes is methods inserted after
    /// that pass without their own bytecode — notably `.^add_multi_method`
    /// (which hardcodes `compiled_code = None`) and any future such site.
    /// (`.^add_method` already carries the method literal's compiled code.)
    /// Populates `compiled_code` in the canonical registry (idempotent) and
    /// re-resolves so the returned def carries the bytecode. Returns `None`
    /// when the owner is not a user class/role (native receiver) or the body
    /// stays uncompilable, preserving the interpreter fallback. Ledger §1.
    pub(crate) fn populate_uncompiled_method(
        &mut self,
        cn: &str,
        owner_class: &str,
        method: &str,
        args: &[Value],
        target: &Value,
    ) -> Option<(String, std::sync::Arc<crate::runtime::MethodDef>)> {
        // Both compile passes are no-ops if the name is absent from the
        // respective registry, so calling both safely covers class- and
        // role-owned methods. Neither re-enters user code (pure compilation),
        // so the registry re-entrancy discipline (②) is respected.
        self.compile_class_methods(owner_class);
        self.compile_role_methods(owner_class);
        let (owner, def) = loan_env!(
            self,
            resolve_method_with_owner_invocant(cn, method, args, target)
        )?;
        if def.compiled_code.is_some() {
            Some((owner, std::sync::Arc::new(def)))
        } else {
            None
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn dispatch_compiled_method(
        &mut self,
        cn: &str,
        owner_class: &str,
        method: &str,
        method_def: &std::sync::Arc<crate::runtime::MethodDef>,
        cc: &std::sync::Arc<CompiledCode>,
        target: Value,
        args: Vec<Value>,
        can_skip_merge: Option<bool>,
    ) -> Result<Value, RuntimeError> {
        let target_id = match &target {
            Value::Instance { id, .. } => Some(*id),
            _ => None,
        };
        let attrs_cell = match &target {
            Value::Instance { attributes, .. } => Some(attributes.clone()),
            _ => None,
        };
        let attributes = match &target {
            Value::Instance { attributes, .. } => attributes.to_map(),
            _ => std::collections::HashMap::new(),
        };
        let empty_fns = HashMap::new();
        let method_result = if let Some(csm) = can_skip_merge {
            // Fast path: move target directly as base (avoid extra clone).
            let invocant_for_dispatch = if attributes.is_empty() {
                Value::Package(crate::symbol::Symbol::intern(cn))
            } else {
                target.clone()
            };
            let pushed_dispatch = loan_env!(
                self,
                push_method_dispatch_frame(cn, method, &args, invocant_for_dispatch,)
            );
            let result = self.call_compiled_method_fast(
                cn,
                owner_class,
                method,
                method_def,
                cc,
                attributes,
                args,
                target,
                &empty_fns,
                csm,
            );
            if pushed_dispatch {
                self.pop_method_dispatch();
            }
            self.pop_method_samewith_context();
            result
        } else {
            let invocant_for_dispatch = if attributes.is_empty() {
                Value::Package(crate::symbol::Symbol::intern(cn))
            } else {
                target.clone()
            };
            let pushed_dispatch = loan_env!(
                self,
                push_method_dispatch_frame(cn, method, &args, invocant_for_dispatch,)
            );
            let invocant = Some(target);
            let result = self.call_compiled_method(
                cn,
                owner_class,
                method,
                method_def,
                cc,
                attributes,
                args,
                invocant,
                &empty_fns,
            );
            if pushed_dispatch {
                self.pop_method_dispatch();
            }
            self.pop_method_samewith_context();
            result
        };
        let (result, new_attrs, attrs_adjusted) = method_result?;
        if let Some(id) = target_id {
            // Commit only a `:=`-adjusted snapshot: an unadjusted one equals the
            // cell and the whole-map write would race with concurrent cell-CAS
            // from another thread (lost updates).
            if attrs_adjusted && let Some(cell) = &attrs_cell {
                cell.commit_attrs(new_attrs.clone());
            }
            if !self.in_lvalue_assignment
                && let Value::Proxy { ref fetcher, .. } = result
            {
                // Without a `:=` adjustment the triple's map is the stale entry
                // snapshot (the lazy reconcile skips the cell clone) —
                // re-snapshot the live cell for the proxy fetcher.
                let proxy_attrs = if !attrs_adjusted && let Some(cell) = &attrs_cell {
                    cell.to_map()
                } else {
                    new_attrs
                };
                return loan_env!(self, proxy_fetch(fetcher, None, cn, &proxy_attrs, id));
            }
        }
        Ok(result)
    }

    /// Pre-compute and cache fast dispatch eligibility for a method.
    pub(crate) fn try_populate_fast_cache(
        &mut self,
        cache_key: (crate::symbol::Symbol, crate::symbol::Symbol),
        receiver_class: &str,
        owner_class: &str,
        method_def: &std::sync::Arc<crate::runtime::MethodDef>,
        cc: &std::sync::Arc<CompiledCode>,
    ) {
        let has_rw_params = method_def
            .param_defs
            .iter()
            .any(|pd| pd.traits.iter().any(|t| t == "rw"));
        if has_rw_params {
            return;
        }
        let has_invocant_constraint = method_def.param_defs.iter().any(|pd| {
            (pd.is_invocant || pd.traits.iter().any(|t| t == "invocant"))
                && pd.type_constraint.is_some()
        });
        let has_complex_params = method_def.param_defs.iter().any(|pd| {
            if pd.is_invocant || pd.traits.iter().any(|t| t == "invocant") {
                return false;
            }
            if pd.slurpy && pd.name == "%_" {
                return false;
            }
            pd.slurpy
                || pd.double_slurpy
                || pd.named
                || pd.where_constraint.is_some()
                || pd.sub_signature.is_some()
                || pd.outer_sub_signature.is_some()
                || pd.code_signature.is_some()
                || pd
                    .type_constraint
                    .as_ref()
                    .is_some_and(|tc| tc.contains('('))
        });
        let has_role_bindings = self.class_role_param_bindings(owner_class).is_some()
            || self.class_role_param_bindings(receiver_class).is_some();
        if has_invocant_constraint || has_complex_params || has_role_bindings {
            return;
        }
        let positional_count = method_def
            .param_defs
            .iter()
            .filter(|pd| {
                !pd.is_invocant
                    && !pd.traits.iter().any(|t| t == "invocant")
                    && !pd.slurpy
                    && !pd.double_slurpy
                    && !pd.named
            })
            .count();
        // Also gate on `cc.has_calls`: a body that invokes a CLOSURE
        // (`$f()` — `CallOnValue`/`CallOnCodeVar`) or a CallDefined/ExecCallSlip can
        // write a dynamic var / captured-outer lexical / global into this frame's
        // env (those call ops are NOT in `has_env_writes`). Skipping the merge would
        // drop that write — e.g. `method go { my $f = { $*x = 1 }; $f() }`. #3658.
        let can_skip_merge = !cc.has_env_writes && !cc.has_calls;
        let has_defaults = method_def.param_defs.iter().any(|pd| {
            !pd.is_invocant && !pd.traits.iter().any(|t| t == "invocant") && pd.default.is_some()
        });
        self.fast_method_cache.insert(
            cache_key,
            super::FastMethodCacheEntry {
                owner_class: crate::symbol::Symbol::intern(owner_class),
                method_def: method_def.clone(),
                compiled_code: cc.clone(),
                can_skip_merge,
                positional_count,
                has_defaults,
            },
        );
    }
}
