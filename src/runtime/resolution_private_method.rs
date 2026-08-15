use super::*;
use crate::type_id::TypeId;

impl Interpreter {
    pub(crate) fn should_skip_defer_method_candidate(
        &self,
        receiver_class: &str,
        candidate_owner: &str,
    ) -> bool {
        if receiver_class != candidate_owner && self.registry().is_hidden_class(candidate_owner) {
            return true;
        }
        self.registry()
            .is_hidden_defer_parent(receiver_class, candidate_owner)
    }

    pub(super) fn resolve_private_method_with_owner(
        &mut self,
        class_name: &str,
        owner_class: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<(String, MethodDef)> {
        let role_bindings = self.registry().get_role_param_bindings(class_name);
        let mro = self.class_mro(class_name);
        for cn in mro.iter().map(|s| s.as_str()) {
            if cn != owner_class {
                continue;
            }
            // Hoist clone to a `let` so the guard drops before re-entry (&mut self).
            let overloads = self
                .registry()
                .get_method_overloads_with_role_fallback(cn, method_name);
            if let Some(overloads) = overloads {
                for def in overloads {
                    if !def.is_private {
                        continue;
                    }
                    if self.method_args_match_for_invocant(
                        class_name,
                        &def,
                        arg_values,
                        role_bindings.as_ref(),
                        None,
                    ) {
                        return Some((cn.to_string(), def));
                    }
                }
            }
        }
        None
    }

    pub(super) fn resolve_private_method_any_owner(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<(String, MethodDef)> {
        // ADR-0019 Phase F box F5: refresh here, at the cache's own read site,
        // rather than relying on every caller to have gone through
        // `resolve_private_method_for_vm` first -- `methods_call_dispatch.rs`,
        // `methods_signature_shaped.rs`, and `methods_instance_ops.rs` (two
        // sites) all call this function directly and previously depended
        // entirely on the eager `clear_private_zeroarg_method_cache()` calls at
        // class/role/augment registration sites, the same generation-blind gap
        // `func_multi_resolve_cache` had (#6425) before it gained its own
        // read-site refresh.
        self.refresh_method_caches_for_generation();
        let role_bindings = self.registry().get_role_param_bindings(class_name);
        if arg_values.is_empty()
            && let Some(cached) = self
                .private_zeroarg_method_cache
                .get(&(class_name.to_string(), method_name.to_string()))
        {
            return cached.clone();
        }
        let mro = self.class_mro(class_name);
        // Fast path: when there are no positional args, avoid cloning the
        // overloads vector by scanning with a shared borrow first. This covers
        // the common case of zero-argument private method calls in tight loops.
        if arg_values.is_empty() {
            // Scan with a shared registry borrow (avoids cloning the whole
            // overloads Vec — only the single matched def is cloned), find the
            // candidate, then drop the guard before mutating the cache.
            let mut resolved: Option<(String, MethodDef)> = None;
            'scan: for cn in mro.iter() {
                let registry = self.registry();
                if let Some(overloads) = registry
                    .classes
                    .get(cn.as_str())
                    .and_then(|c| c.methods.get(method_name))
                {
                    // First pass: skip stubs
                    for def in overloads {
                        if !def.is_private {
                            continue;
                        }
                        if Self::is_stub_method_body(&def.body) {
                            continue;
                        }
                        if def
                            .param_defs
                            .iter()
                            .all(|p| p.is_invocant || p.traits.iter().any(|t| t == "invocant"))
                        {
                            resolved = Some((cn.resolve(), def.clone()));
                            break 'scan;
                        }
                    }
                    // Second pass: include stubs
                    for def in overloads {
                        if !def.is_private {
                            continue;
                        }
                        if def
                            .param_defs
                            .iter()
                            .all(|p| p.is_invocant || p.traits.iter().any(|t| t == "invocant"))
                        {
                            resolved = Some((cn.resolve(), def.clone()));
                            break 'scan;
                        }
                    }
                }
            }
            if let Some(resolved) = resolved {
                self.private_zeroarg_method_cache.insert(
                    (class_name.to_string(), method_name.to_string()),
                    Some(resolved.clone()),
                );
                return Some(resolved);
            }
        }
        for cn in mro.iter().map(|s| s.as_str()) {
            // Hoist clone to a `let` so the guard drops before re-entry (&mut self).
            let overloads = self
                .registry()
                .get_method_overloads_with_role_fallback(cn, method_name);
            if let Some(overloads) = overloads {
                for def in &overloads {
                    if !def.is_private {
                        continue;
                    }
                    if Self::is_stub_method_body(&def.body) {
                        continue;
                    }
                    if self.method_args_match_for_invocant(
                        class_name,
                        def,
                        arg_values,
                        role_bindings.as_ref(),
                        None,
                    ) {
                        return Some((cn.to_string(), def.clone()));
                    }
                }
                for def in overloads {
                    if !def.is_private {
                        continue;
                    }
                    if self.method_args_match_for_invocant(
                        class_name,
                        &def,
                        arg_values,
                        role_bindings.as_ref(),
                        None,
                    ) {
                        return Some((cn.to_string(), def));
                    }
                }
            }
        }
        None
    }

    /// Private-method candidates for `class_name`, found by NAME only —
    /// `arg_values` are not consulted.
    ///
    /// The signature-matching resolvers above answer `None` both when the class
    /// has no such private method AND when it has exactly one whose parameters
    /// the arguments fail to bind, and the callers then report "No such private
    /// method". raku reports the binding failure instead
    /// (`Type check failed in binding to parameter '$n'`, `Too many positionals
    /// passed`), which is what the *public* dispatch path already does.
    ///
    /// So a caller that is about to give up asks this: nothing here means the
    /// method really is absent; exactly one means "run it and let the binding
    /// error speak"; more than one is a genuine `X::Multi::NoMatch`.
    ///
    /// `owner_class` restricts the walk to one MRO entry, mirroring
    /// [`Self::resolve_private_method_with_owner`] for the `$obj!Owner::m` form.
    pub(crate) fn private_method_candidates_by_name(
        &mut self,
        class_name: &str,
        owner_class: Option<&str>,
        method_name: &str,
    ) -> Vec<(String, MethodDef)> {
        let mro = self.class_mro(class_name);
        let mut out = Vec::new();
        for cn in mro.iter().map(|s| s.as_str()) {
            if let Some(owner) = owner_class
                && cn != owner
            {
                continue;
            }
            let overloads = self
                .registry()
                .get_method_overloads_with_role_fallback(cn, method_name);
            if let Some(overloads) = overloads {
                for def in overloads {
                    if def.is_private && !Self::is_stub_method_body(&def.body) {
                        out.push((cn.to_string(), def));
                    }
                }
            }
        }
        out
    }

    pub(crate) fn resolve_private_method_for_vm(
        &mut self,
        class_name: &str,
        method: &str,
        arg_values: &[Value],
    ) -> Option<(String, MethodDef)> {
        // ADR-0019 E3 (design decision 5, `todo/deep/adr0019-e2-e4-resolver-core.md`):
        // `resolve_private_method_any_owner`'s `private_zeroarg_method_cache`
        // read is generation-blind — it relied entirely on the manual clear
        // blocks (`clear_private_zeroarg_method_cache`'s call sites), not on
        // `method_generation`. This is the one entry point both private-method
        // resolvers share, so refreshing here covers the cache before it is
        // ever consulted.
        self.refresh_method_caches_for_generation();
        let private_rest = method.strip_prefix('!')?;
        let split = private_rest.split_once("::");
        let owner_class = split.map(|(o, _)| o);
        let pm_name = split.map(|(_, n)| n).unwrap_or(private_rest);
        let real = match owner_class {
            Some(owner) => {
                self.resolve_private_method_with_owner(class_name, owner, pm_name, arg_values)
            }
            None => self.resolve_private_method_any_owner(class_name, pm_name, arg_values),
        };
        // ADR-0019 Phase E box E7 (third consumer family, private-as-
        // sequence-query -- see `todo/deep/adr0019-e5-e7-entry-routing.md`
        // "E7 step 3"): shadow-check this ad-hoc private-method MRO walk
        // against the E4 resolver's `resolve_sequence`, now extended with a
        // `MethodVisibility::Private` tier (E7 steps 1/2 only ever built the
        // `Public` tier). The owner-qualified form (`$obj!Owner::m`)
        // restricts the shadow chain to exactly `[owner]` -- but ONLY when
        // `owner` actually appears in the RECEIVER's own MRO
        // (`self.class_mro(class_name)`), matching
        // `resolve_private_method_with_owner`'s own `for cn in
        // self.class_mro(class_name) { if cn != owner_class { continue } }`:
        // the real walk is rooted at the receiver's MRO and merely filters it
        // down to one level, it is not a direct lookup on `owner_class`'s own
        // methods regardless of relation to the receiver. An unrelated
        // `owner` (e.g. `$b!A::p()` where `$b`'s class `B` does not inherit
        // from `A`) must yield an EMPTY chain, not `[A]` -- an empty chain
        // was exactly the fix for the one mismatch the initial sweep found
        // (`t/private-owner-qualified-permission.t`, `class=B method=p
        // real=None shadow=Some("A")`): `class_mro("B")` is just `[B]`, so
        // `A` never appears and the real walk finds nothing, but a naive
        // `[TypeId::intern("A")]` chain found `A`'s own private `p` anyway.
        // The unqualified form (`$obj!m`) walks the receiver's own full MRO
        // via `self.class_mro(class_name)`, exactly what
        // `resolve_private_method_any_owner` itself walks (`class_name` here
        // is already the receiver's class, not an arbitrary qualifier, so no
        // `Value::package(...)` round-trip through `dispatch_mro` is needed
        // the way E7 step 2's qualifier-rooted chain required one).
        // `resolve_private_method_for_vm` has exactly two callers in the
        // whole codebase (both VM carrier sites,
        // `vm_call_method_compiled_interpret.rs` /
        // `vm_call_method_compiled_mut.rs`), so -- like E7 step 2 -- this is
        // gated inline rather than threaded through a `site` parameter. A
        // no-op unless `MUTSU_VM_STATS` is set: zero behavior change.
        if crate::vm::vm_stats::enabled() {
            let method_sym = Symbol::intern(pm_name);
            let receiver_mro = self.class_mro(class_name);
            let chain: Vec<TypeId> = match owner_class {
                Some(owner) => {
                    if receiver_mro.iter().any(|s| s.as_str() == owner) {
                        vec![TypeId::intern(owner)]
                    } else {
                        Vec::new()
                    }
                }
                None => receiver_mro
                    .iter()
                    .map(|s| TypeId::intern(s.as_str()))
                    .collect(),
            };
            let real_sym = real
                .as_ref()
                .map(|(owner, def)| (Symbol::intern(owner), def.clone()));
            self.shadow_check_resolver_chain(
                "privatedispatch",
                class_name,
                pm_name,
                method_sym,
                arg_values,
                None,
                &chain,
                super::resolution_sequence::MethodVisibility::Private,
                real_sym.as_ref(),
            );
            if real.is_some() {
                crate::vm::vm_stats::record_dispatch_entry_intercept("privatedispatch", pm_name);
            } else {
                crate::vm::vm_stats::record_dispatch_entry_outcome("privatedispatch", "notfound");
            }
        }
        real
    }

    pub(crate) fn can_fast_dispatch_private_method_vm(&self, owner_class: &str) -> bool {
        self.method_class_stack
            .last()
            .is_some_and(|caller| caller == owner_class)
    }

    /// Resolve the MRO for `class_name`. Tries the read-only resolution first
    /// ([`Registry::class_mro_readonly`]) so the hot dispatch path holds only a
    /// read guard — a write guard's first mutable deref after a spawn share
    /// deep-clones the whole registry (COW), which also resets every
    /// `CompiledFunction`'s JIT state (fresh `JitCodeState` per clone). Only a
    /// registered class whose MRO is not yet cached falls through to the
    /// compute-and-cache write side ([`Registry::class_mro`]).
    /// Returns the cached interned-symbol MRO — an `Arc` clone, no per-call
    /// `String` allocations (the old `Vec<String>` clone was a per-dispatch
    /// allocation hot spot).
    pub(crate) fn class_mro(&mut self, class_name: &str) -> std::sync::Arc<[Symbol]> {
        if let Some(mro) = self.registry().class_mro_readonly(class_name) {
            return mro;
        }
        self.registry_mut().class_mro(class_name)
    }
}
