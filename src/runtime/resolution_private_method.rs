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
        for cn in mro {
            if cn != owner_class {
                continue;
            }
            // Hoist clone to a `let` so the guard drops before re-entry (&mut self).
            let overloads = self.registry().get_method_overloads(&cn, method_name);
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
                        return Some((cn.clone(), def));
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
            'scan: for cn in &mro {
                let registry = self.registry();
                if let Some(overloads) = registry
                    .classes
                    .get(cn)
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
                            resolved = Some((cn.clone(), def.clone()));
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
                            resolved = Some((cn.clone(), def.clone()));
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
        for cn in mro {
            // Hoist clone to a `let` so the guard drops before re-entry (&mut self).
            let overloads = self.registry().get_method_overloads(&cn, method_name);
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
                        return Some((cn.clone(), def.clone()));
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
                        return Some((cn.clone(), def));
                    }
                }
            }
        }
        None
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

    /// Resolve the MRO for `class_name`. Delegates to [`Registry::class_mro`],
    /// which holds a single write guard for the whole compute-and-cache op.
    pub(crate) fn class_mro(&mut self, class_name: &str) -> Vec<String> {
        self.registry_mut().class_mro(class_name)
    }
}
