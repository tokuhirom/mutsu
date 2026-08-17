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

    /// Whether ANY registered user class or role defines a `DESTROY`
    /// submethod. This is exactly the predicate `run_pending_instance_destroys`
    /// dispatches on (`class_def.methods.get("DESTROY")` / role submethods) —
    /// builtin native-method DESTROYs (IO::Handle etc.) live in
    /// `native_methods` and are not queued through the GC finalize path, so
    /// they do not count. Used to skip the program-end cycle collect when no
    /// DESTROY could possibly fire (`gc::collect_at_program_end`). O(types),
    /// called once at exit.
    pub(crate) fn registry_has_destroy_methods(&self) -> bool {
        let reg = self.registry();
        reg.classes
            .keys()
            .any(|name| reg.user_method_overloads(name, "DESTROY").is_some())
            || reg
                .roles
                .values()
                .any(|rd| rd.methods.contains_key("DESTROY"))
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
            .map(Value::view)
        {
            Some(ValueView::Str(rev)) => rev.as_str() >= "e",
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
            let mro: std::sync::Arc<[crate::symbol::Symbol]> = self
                .registry()
                .classes
                .get(&instance_class)
                .map(|cd| cd.mro.clone())
                .unwrap_or_else(|| [].into());
            // Track attributes across DESTROY calls so mutations are visible
            let mut current_attrs = item.attributes.clone();
            // Walk the MRO; submethods are per-class, not inherited.
            for mro_class in mro.iter().map(|s| s.as_str()) {
                // Skip role entries in MRO
                if self.registry().roles.contains_key(mro_class)
                    && !self.registry().classes.contains_key(mro_class)
                {
                    continue;
                }
                // Clone DESTROY overloads out and drop the guard before re-entering
                // user code (run_resolved_method_compiled_or_treewalk).
                if !self.registry().classes.contains_key(mro_class) {
                    continue;
                }
                let destroy_overloads = self.registry().user_method_overloads(mro_class, "DESTROY");
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

    /// Whether two multi-method candidates whose positional type signatures
    /// match are nonetheless distinguished by value-level constraints: a
    /// differing literal parameter (`multi method f(1)` vs `f(3)`) or a
    /// `where` clause on either side. Raku composes such candidates from
    /// different roles into one candidate set instead of demanding the class
    /// resolve a conflict (`where` equality is undecidable, so any `where` is
    /// treated as distinguishing).
    fn multi_constraints_distinguish(a: &MethodDef, b: &MethodDef) -> bool {
        let positionals = |def: &MethodDef| -> Vec<(Option<Value>, bool)> {
            def.param_defs
                .iter()
                .filter(|pd| !(pd.named || (pd.slurpy && pd.name.starts_with('%'))))
                .map(|pd| (pd.literal_value.clone(), pd.where_constraint.is_some()))
                .collect()
        };
        let pa = positionals(a);
        let pb = positionals(b);
        pa.iter()
            .zip(pb.iter())
            .any(|((lit_a, wh_a), (lit_b, wh_b))| {
                *wh_a
                    || *wh_b
                    || match (lit_a, lit_b) {
                        (Some(va), Some(vb)) => va != vb,
                        (None, None) => false,
                        _ => true,
                    }
            })
    }

    pub(super) fn detect_unresolved_role_method_conflicts(
        &self,
        class_name: &str,
    ) -> Result<(), RuntimeError> {
        // ADR-0019 F4c-9a-1: cut over from `class_def.methods` to the
        // canonical table. `resolve_class_stub_requirements` (called just
        // before this by `finalize_class_registration`) dual-writes every
        // `class_def.methods` mutation to the registry via the mutator API
        // (F4c-3), so unlike the pre-dual-write-bridge era this function's
        // own F4c-1 shadow check used to worry about, the two are now kept
        // in lockstep even mid-`finalize_class_registration` -- confirmed
        // empirically via that same shadow check reporting zero mismatches
        // across the full local `t/` suite (3185 files) and the S12/S14
        // role-composition roast subset (122 files) before this cutover.
        let registry = self.registry();
        for method_name in registry.owner_method_names(class_name) {
            let method_name = method_name.resolve();
            let Some(defs) = registry.user_method_overloads(class_name, &method_name) else {
                continue;
            };
            // Submethods (like BUILD, TWEAK) from multiple roles do not conflict —
            // they are accumulated and all called during construction. Skip them.
            if defs.iter().all(|d| d.is_submethod) {
                continue;
            }
            // Check non-multi methods. Public and private methods that share a
            // base name live in separate namespaces (dispatch filters on privacy),
            // so a public `foo` from one role and a private `!foo` from another do
            // NOT conflict — check each privacy class independently.
            for is_private in [false, true] {
                let non_multi: Vec<&MethodDef> = defs
                    .iter()
                    .filter(|d| {
                        !d.is_multi
                            && d.is_private == is_private
                            && !Self::is_stub_routine_body(&d.body)
                    })
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
                        if Self::method_signatures_match(def_a, def_b)
                            && !Self::multi_constraints_distinguish(def_a, def_b)
                        {
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
    /// available candidates. `boundary_owner`, when `Some`, mirrors
    /// `resolve_method_with_owner_impl`'s boundary parameter (ADR-0019 E9c-2):
    /// a proto `{*}` redispatch's no-match diagnostic should list only the
    /// candidates that redispatch can actually reach (at or below the
    /// proto's owner), not an ancestor's candidates beyond it. Ordinary
    /// (non-proto) dispatch passes `None`.
    pub(crate) fn format_method_candidate_signatures(
        &self,
        receiver_class_name: &str,
        method_name: &str,
        boundary_owner: Option<Symbol>,
    ) -> Vec<String> {
        let mut sigs = Vec::new();
        let mro = self.mro_readonly(receiver_class_name);
        let truncate_at =
            boundary_owner.and_then(|owner| mro.iter().position(|cn| Symbol::intern(cn) == owner));
        let mro = match truncate_at {
            Some(pos) => &mro[..=pos],
            None => &mro[..],
        };
        for cn in mro {
            let is_ancestor = cn.as_str() != receiver_class_name;
            // No user-code re-entry in this loop body (pure signature-string
            // building), so a let-bound guard is safe.
            let registry = self.registry();
            let Some(overloads) = registry.user_method_overloads(cn.as_str(), method_name) else {
                continue;
            };
            for def in &overloads {
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
                let invocant_pd = def.param_defs.iter().find(|pd| pd.is_invocant);
                let inv_type = invocant_pd
                    .and_then(|pd| pd.type_constraint.as_deref())
                    .unwrap_or(receiver_class_name);
                let inv_name = invocant_pd.map(|pd| pd.name.as_str()).unwrap_or("");
                let mut sig = format!("({} ${}:: ", inv_type, inv_name);
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
