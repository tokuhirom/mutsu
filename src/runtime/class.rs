//! Class lifecycle and structure: instance DESTROY running, role-method
//! composition-conflict detection, C3 MRO computation, and candidate-signature
//! formatting for dispatch error messages. Attribute/method introspection lives
//! in `class_introspection`; instance-method dispatch in `class_dispatch`.

use super::*;

impl Interpreter {
    pub(crate) fn run_pending_instance_destroys(&mut self) -> Result<(), RuntimeError> {
        let pending = take_pending_instance_destroys();
        if pending.is_empty() {
            return Ok(());
        }
        // Set reentrancy guard to prevent infinite DESTROY recursion:
        // instances created during DESTROY execution should not queue new DESTROYs.
        crate::value::set_in_destroy_handler(true);
        let result = self.run_pending_instance_destroys_inner(&pending);
        crate::value::set_in_destroy_handler(false);
        result
    }

    /// Whether a type was declared under Raku 6.e+ semantics, keyed on the
    /// type's *declaration* revision (captured as type metadata). DESTROY/BUILD
    /// run long after parsing, when the globally-current language version may
    /// have been reset to the default, so reading that global would be wrong.
    pub(crate) fn type_decl_is_6e(&self, name: &str) -> bool {
        match self
            .type_metadata
            .get(name)
            .and_then(|meta| meta.get("language-revision"))
        {
            Some(Value::Str(rev)) => rev.as_str() >= "e",
            _ => crate::parser::current_language_version().starts_with("6.e"),
        }
    }

    fn run_pending_instance_destroys_inner(
        &mut self,
        pending: &[crate::value::PendingInstanceDestroy],
    ) -> Result<(), RuntimeError> {
        for item in pending {
            let instance_class = item.class_name.resolve();
            // 6.e role-submethod DESTROY dispatch is keyed on the instance's
            // class declaration revision, not the globally-current version.
            let is_6e = self.type_decl_is_6e(&instance_class);
            // Collect the MRO so we call DESTROY on each class in order (child → parent).
            let mro: Vec<String> = self
                .registry()
                .classes
                .get(&instance_class)
                .map(|cd| cd.mro.clone())
                .unwrap_or_default();
            // Track attributes across DESTROY calls so mutations are visible
            let mut current_attrs = item.attributes.clone();
            // Walk the MRO; submethods are per-class, not inherited.
            for mro_class in &mro {
                // Skip role entries in MRO
                if self.registry().roles.contains_key(mro_class)
                    && !self.registry().classes.contains_key(mro_class)
                {
                    continue;
                }
                // Clone DESTROY overloads out and drop the guard before re-entering
                // user code (run_instance_method_resolved).
                let destroy_overloads = match self.registry().classes.get(mro_class) {
                    Some(class_def) => class_def.methods.get("DESTROY").cloned(),
                    None => continue,
                };
                // Call class's own DESTROY submethod
                if let Some(overloads) = destroy_overloads
                    && let Some(method_def) = overloads.into_iter().find(|def| {
                        def.is_my && !def.is_private && self.method_args_match(&[], &def.param_defs)
                    })
                {
                    let invocant = Value::make_instance_without_destroy(
                        item.class_name,
                        current_attrs.clone(),
                    );
                    if let Ok((_v, updated)) = self.run_resolved_method_compiled_or_treewalk(
                        &instance_class,
                        mro_class,
                        "DESTROY",
                        method_def,
                        current_attrs.clone(),
                        Vec::new(),
                        Some(invocant),
                    ) {
                        current_attrs = updated;
                    }
                }
                // Under v6.e+, call DESTROY submethods from composed roles
                // (in reverse order: role submethods after the class's own DESTROY)
                if is_6e {
                    let role_order = self.ordered_role_submethods_for_class(mro_class, "DESTROY");
                    // DESTROY order is reverse of BUILD: role submethods after class
                    for (role_name, method_def) in role_order.into_iter().rev() {
                        let invocant = Value::make_instance_without_destroy(
                            item.class_name,
                            current_attrs.clone(),
                        );
                        if let Ok((_v, updated)) = self.run_resolved_method_compiled_or_treewalk(
                            &instance_class,
                            &role_name,
                            "DESTROY",
                            method_def,
                            current_attrs.clone(),
                            Vec::new(),
                            Some(invocant),
                        ) {
                            current_attrs = updated;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub(super) fn detect_unresolved_role_method_conflicts(
        &self,
        class_name: &str,
        class_def: &ClassDef,
    ) -> Result<(), RuntimeError> {
        for (method_name, defs) in &class_def.methods {
            // Submethods (like BUILD, TWEAK) from multiple roles do not conflict —
            // they are accumulated and all called during construction. Skip them.
            if defs.iter().all(|d| d.is_submethod) {
                continue;
            }
            // Check non-multi methods
            let non_multi: Vec<&MethodDef> = defs
                .iter()
                .filter(|d| !d.is_multi && !Self::is_stub_routine_body(&d.body))
                .collect();
            let class_defined_non_multi = non_multi.iter().any(|d| d.role_origin.is_none());
            if !class_defined_non_multi {
                let mut conflicting_roles = Vec::new();
                let mut seen_origins = Vec::new();
                for def in &non_multi {
                    let Some(role_name) = &def.role_origin else {
                        continue;
                    };
                    // Use original_role for diamond detection: if two methods
                    // trace back to the same original role, they are not in conflict.
                    let origin = def.original_role.as_ref().unwrap_or(role_name);
                    if seen_origins.contains(origin) {
                        continue;
                    }
                    seen_origins.push(origin.clone());
                    if !conflicting_roles.contains(role_name) {
                        conflicting_roles.push(role_name.clone());
                    }
                }
                if conflicting_roles.len() > 1 {
                    conflicting_roles.reverse();
                    return Err(RuntimeError::new(format!(
                        "X::Role::Composition::Conflict: Method '{}' must be resolved by class {} because it exists in multiple roles ({})",
                        method_name,
                        class_name,
                        conflicting_roles.join(", "),
                    )));
                }
            }

            // Check multi methods: detect per-signature conflicts from different roles
            let multi_defs: Vec<&MethodDef> = defs
                .iter()
                .filter(|d| d.is_multi && !Self::is_stub_routine_body(&d.body))
                .collect();
            if multi_defs.len() > 1 {
                // Group by signature, check if any signature has methods from multiple roles
                // without a class-provided resolution
                let mut checked: Vec<Vec<String>> = Vec::new();
                for (i, def_a) in multi_defs.iter().enumerate() {
                    let sig = Self::method_positional_signature(def_a);
                    if checked.contains(&sig) {
                        continue;
                    }
                    checked.push(sig);
                    // Find all methods with matching signature. Identify each
                    // candidate by its ORIGINAL defining role (diamond detection,
                    // mirroring the non-multi path above): a multi method composed
                    // transitively via several roles (`role R { multi method m }`,
                    // `role S does R`, `class C does S`) traces back to the same
                    // original role through every path and so is NOT a conflict.
                    // Using `role_origin` (the immediate composition source) instead
                    // would wrongly see it as `(S, R)` and demand resolution.
                    let mut roles_for_sig: Vec<String> = Vec::new();
                    let mut class_resolves = def_a.role_origin.is_none();
                    if let Some(r) = def_a.original_role.as_ref().or(def_a.role_origin.as_ref())
                        && !roles_for_sig.contains(r)
                    {
                        roles_for_sig.push(r.clone());
                    }
                    for def_b in multi_defs.iter().skip(i + 1) {
                        if Self::method_signatures_match(def_a, def_b) {
                            if def_b.role_origin.is_none() {
                                class_resolves = true;
                            }
                            if let Some(r) =
                                def_b.original_role.as_ref().or(def_b.role_origin.as_ref())
                                && !roles_for_sig.contains(r)
                            {
                                roles_for_sig.push(r.clone());
                            }
                        }
                    }
                    if roles_for_sig.len() > 1 && !class_resolves {
                        return Err(RuntimeError::new(format!(
                            "X::Role::Composition::Conflict: Method '{}' must be resolved by class {} because it exists in multiple roles ({})",
                            method_name,
                            class_name,
                            roles_for_sig.join(", "),
                        )));
                    }
                }
            }
        }

        Ok(())
    }

    /// Compute the C3 MRO for `class_name`. Delegates to the pure-registry
    /// [`Registry::compute_class_mro`] under a single read guard.
    pub(super) fn compute_class_mro(
        &mut self,
        class_name: &str,
        stack: &mut Vec<String>,
    ) -> Result<Vec<String>, RuntimeError> {
        self.registry().compute_class_mro(class_name, stack)
    }

    /// Format the candidate signatures of a (multi) method across the
    /// receiver class's MRO, e.g. `(WorkingTie: Int $z, *%_)`. Used to build a
    /// Raku-style `X::Multi::NoMatch` message naming the invocant type and the
    /// available candidates.
    pub(crate) fn format_method_candidate_signatures(
        &self,
        receiver_class_name: &str,
        method_name: &str,
    ) -> Vec<String> {
        let mut sigs = Vec::new();
        for cn in self.mro_readonly(receiver_class_name) {
            let is_ancestor = cn.as_str() != receiver_class_name;
            // No user-code re-entry in this loop body (pure signature-string
            // building), so a let-bound guard is safe.
            let registry = self.registry();
            let Some(class_def) = registry.classes.get(cn.as_str()) else {
                continue;
            };
            let Some(overloads) = class_def.methods.get(method_name) else {
                continue;
            };
            for def in overloads {
                if def.is_private || (def.is_my && is_ancestor) {
                    continue;
                }
                let mut parts = Vec::new();
                for pd in &def.param_defs {
                    if pd.is_invocant {
                        continue;
                    }
                    // Skip the implicit `*%_` slurpy named param; we append it
                    // explicitly at the end of every candidate signature.
                    if pd.named
                        && (pd.slurpy || pd.double_slurpy)
                        && (pd.name == "%_" || pd.name == "_" || pd.name.is_empty())
                    {
                        continue;
                    }
                    let ty = pd.type_constraint.as_deref().unwrap_or("Any");
                    let sigil_prefix = if pd.slurpy || pd.double_slurpy {
                        "*"
                    } else {
                        ""
                    };
                    let var = if pd.name.is_empty() {
                        String::new()
                    } else if pd.name.starts_with('$')
                        || pd.name.starts_with('@')
                        || pd.name.starts_with('%')
                    {
                        format!(" {}{}", sigil_prefix, pd.name)
                    } else {
                        format!(" {}${}", sigil_prefix, pd.name)
                    };
                    parts.push(format!("{}{}", ty, var));
                }
                let mut sig = format!("({}: ", receiver_class_name);
                sig.push_str(&parts.join(", "));
                if !parts.is_empty() {
                    sig.push_str(", ");
                }
                sig.push_str("*%_)");
                sigs.push(sig);
            }
        }
        sigs
    }
}
