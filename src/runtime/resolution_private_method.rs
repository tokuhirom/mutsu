use super::*;

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
            let overloads = self.registry().get_method_overloads(cn, method_name);
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
            let overloads = self.registry().get_method_overloads(cn, method_name);
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
            let overloads = self.registry().get_method_overloads(cn, method_name);
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
        let private_rest = method.strip_prefix('!')?;
        if let Some((owner_class, pm_name)) = private_rest.split_once("::") {
            self.resolve_private_method_with_owner(class_name, owner_class, pm_name, arg_values)
        } else {
            self.resolve_private_method_any_owner(class_name, private_rest, arg_values)
        }
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
