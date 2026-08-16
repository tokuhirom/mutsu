//! Named phases of `register_class_decl` (ADR-0019 D0): execution of a
//! parameterized role's deferred body during composition, propagation of the
//! composed role's declared parents, and role punning. Pure mechanical
//! extraction from `registration_class_decl.rs` — no behavior change.

use super::registration_class::{
    parse_role_type_args, substitute_type_params_in_method, type_value_name,
};
use super::registration_class_compose::RoleCompositionCx;
use super::*;
use crate::symbol::Symbol;

impl Interpreter {
    /// Rename a class declared inside a parametric role body to its
    /// per-composition parameterized name. `old_name` is the registry key it was
    /// registered under while running the role's deferred body (`G::A` for a
    /// class nested in `my package G`, `R::A` for a direct role-body class);
    /// `role_name` is the composing role (`R`); `suffix` is the bracketed concrete
    /// type args (`[Int]`). Returns the new registry key, or `None` when no rename
    /// is needed.
    fn rename_generic_composed_class(
        &mut self,
        old_name: &str,
        role_name: &str,
        suffix: &str,
    ) -> Option<String> {
        // Prefix the class with the role unless it is already role-qualified, so
        // a `my package G` nested class (`G::A`) becomes `R::G::A` while a direct
        // role-body class (`R::A`) keeps its single role prefix.
        let role_prefix = format!("{role_name}::");
        let base = if old_name.starts_with(&role_prefix) || old_name == role_name {
            old_name.to_string()
        } else {
            format!("{role_prefix}{old_name}")
        };
        let new_name = format!("{base}{suffix}");
        if new_name == old_name {
            return None;
        }
        // Move the class definition to the new key with a cleared MRO cache so it
        // recomputes with the new name as its head.
        let mut def = self.registry_mut().classes.remove(old_name)?;
        def.mro = std::sync::Arc::from(Vec::<Symbol>::new());
        self.registry_mut().classes.insert(new_name.clone(), def);
        // ADR-0019 F4c-5: `rename_method_owner` replaces the old "sync old
        // (clears via the pure-clear path), sync new (re-derives from
        // `def.methods`)" idiom for the user-method column; the accessor
        // column has no owner-rename mutator (design: it stays keyed off
        // `ClassDef::attributes`, which the `classes.insert` above already
        // moved to `new_name`), so it still goes through the same
        // `sync_accessor_entries` calls the old `sync_user_method_entries`
        // pair made internally.
        let old_owner = Symbol::intern(old_name);
        let new_owner = Symbol::intern(&new_name);
        self.registry_mut()
            .rename_method_owner(old_owner, new_owner);
        self.registry_mut().sync_accessor_entries(old_owner);
        self.registry_mut().sync_accessor_entries(new_owner);
        if self.user_declared_classes.remove(old_name) {
            self.user_declared_classes.insert(new_name.clone());
        }
        // Register the new type object so `R::G::A[Int]` resolves; the caller
        // aliases the bare `G::A` reference to the same value.
        self.env
            .insert(new_name.clone(), Value::package(Symbol::intern(&new_name)));
        // Prime the MRO for the new name.
        self.class_mro(&new_name);
        Some(new_name)
    }

    /// Execute deferred body statements from parameterized roles
    /// with concrete type parameter bindings. These statements
    /// (e.g., `my T $v .= new;`) may create closure variables that
    /// are referenced by composed methods, so we must keep their
    /// effects on the env (only clean up the type capture markers).
    pub(super) fn run_composed_role_deferred_body(
        &mut self,
        cx: &mut RoleCompositionCx<'_>,
        base_role_name: &str,
        role: &RoleDef,
        role_param_values: &HashMap<String, Value>,
        role_arg_values: &[Value],
    ) -> Result<(), RuntimeError> {
        if role.deferred_body.is_empty() {
            return Ok(());
        }
        // Bind type parameters as type captures
        for (param_name, param_value) in role_param_values {
            self.bind_type_capture(param_name, param_value);
        }
        // A class declared inside a *parametric* role body becomes
        // parametric over the role's type args: `class A is Array[T]`
        // in `role R[::T]` composed with `Int` is `R::A[Int]` (or
        // `R::G::A[Int]` when nested in `my package G`). Snapshot the
        // registry so the freshly-declared nested class(es) can be
        // renamed to their per-composition parameterized names below.
        let class_suffix: Option<String> = if role_arg_values.is_empty() {
            None
        } else {
            Some(format!(
                "[{}]",
                role_arg_values
                    .iter()
                    .map(type_value_name)
                    .collect::<Vec<_>>()
                    .join(",")
            ))
        };
        let classes_before: HashSet<String> = if class_suffix.is_some() {
            self.registry().classes.keys().cloned().collect()
        } else {
            HashSet::new()
        };
        // Run a nested TYPE declaration (`my class CR2`) in the role
        // body with the ROLE as the current package so it is named
        // `R2::CR2`, not the composing class. Only type declarations get
        // the role package — a lexical `sub`/`my $x` keeps the outer
        // package so a bare reference from a role method still resolves.
        let saved_body_pkg = self.current_package().to_string();
        // The body's lexical effects are kept on purpose (a composed
        // method may close over `my $sol = nativesizeof(T)`), but the
        // *topic* is not one of them: each statement publishes its
        // value through `$_`, and composition happens wherever the
        // role was first parameterised — inside a `with`/`given`
        // block, that would retopicalize the caller.
        let saved_topic = self.env.get("_").cloned();
        let body_env_before: HashSet<crate::symbol::Symbol> = self.env.keys().copied().collect();
        for op in &role.deferred_body {
            let is_type_decl = op.kind == crate::opcode::DeferredBodyOpKind::TypeDecl;
            // A `token`/`rule`/`regex` in a role body is composed into
            // the consuming grammar, exactly like a method: it must
            // register under the COMPOSING class's package, not the
            // role's and not the enclosing one. Registering it under
            // the outer package makes every grammar share one global
            // `<item>`, so two roles declaring the same token name
            // silently alias (`grammar GA does A` seeing B's `item`).
            let is_regex_decl = op.kind == crate::opcode::DeferredBodyOpKind::TokenRule;
            if is_type_decl {
                self.set_current_package(base_role_name.to_string());
            } else if is_regex_decl {
                self.set_current_package(cx.name.to_string());
            }
            let r = match &op.chunk {
                Some(chunk) => self.run_compiled_block_raw(&chunk.code, &chunk.fns),
                None => self.run_block_raw(std::slice::from_ref(&op.raw)),
            };
            if is_type_decl || is_regex_decl {
                self.set_current_package(saved_body_pkg.clone());
            }
            // A role body statement that dies rejects this
            // parameterisation (`role R[::T] { die unless T.REPR eq
            // 'CStruct' }`). Restore the topic the composition
            // borrowed before unwinding, and report it as Rakudo
            // does: X::Role::Instantiation wrapping the original.
            if let Err(err) = r {
                if err.control.is_none() {
                    match saved_topic.clone() {
                        Some(topic) => {
                            self.env.insert("_".to_string(), topic);
                        }
                        None => {
                            self.env.remove("_");
                        }
                    }
                    self.set_current_package(saved_body_pkg.clone());
                    return Err(RuntimeError::role_instantiation(base_role_name, err));
                }
                return Err(err);
            }
        }
        match saved_topic {
            Some(topic) => {
                self.env.insert("_".to_string(), topic);
            }
            None => {
                self.env.remove("_");
            }
        }
        // Persist the role body's lexicals as class-body statics of
        // the composing class. Leaving them only in the live env
        // works for the frame that ran the composition, but a later
        // method call from another frame (a require-in-method load
        // whose frame is gone) reads them as Nil — DBIish's
        // `LinearArray[MYSQL_BIND].new` computed its stride from a
        // Nil `$sol` on the second construction and calloc'd a
        // 0-byte bind array. Same recognition rules as the
        // class-body statics pass at the end of this function: a
        // name the body explicitly declared counts even when a
        // same-named lexical already leaked into the outer env.
        {
            let declared: HashSet<&str> = role
                .deferred_body
                .iter()
                .flat_map(|op| op.declared_vars.iter())
                .map(|s| s.as_str())
                .collect();
            let new_lexicals: Vec<(String, Value)> = self
                .env
                .iter()
                .filter_map(|(k, v)| {
                    let bare = k.resolve();
                    if body_env_before.contains(k) && !declared.contains(bare.as_str()) {
                        return None;
                    }
                    if bare.contains("::")
                        || bare.starts_with("__")
                        || bare.starts_with('?')
                        || bare.starts_with('!')
                        || bare == "self"
                        || bare == "_"
                    {
                        return None;
                    }
                    if !declared.contains(bare.as_str())
                        && matches!(v.view(), ValueView::Package(_))
                    {
                        return None;
                    }
                    Some((bare, v.clone()))
                })
                .collect();
            if !new_lexicals.is_empty() {
                let marks = self
                    .class_body_static_names
                    .entry(cx.name.to_string())
                    .or_default();
                for (bare, _) in &new_lexicals {
                    marks.insert(bare.clone());
                }
                let store = self
                    .package_lexicals
                    .entry(cx.name.to_string())
                    .or_default();
                for (bare, v) in new_lexicals {
                    store.insert(bare, v);
                }
            }
        }
        // Rename each newly-declared nested class to its
        // per-composition parameterized name and record an alias so a
        // bare reference (`G::A`) from a composed method still resolves
        // to this instantiation's class.
        if let Some(suffix) = &class_suffix {
            let new_classes: Vec<String> = self
                .registry()
                .classes
                .keys()
                .filter(|k| !classes_before.contains(*k))
                .cloned()
                .collect();
            for old_name in new_classes {
                if let Some(new_name) =
                    self.rename_generic_composed_class(&old_name, base_role_name, suffix)
                {
                    // Repoint any attribute typed `is G::A` at the
                    // parameterized class so its element type is
                    // enforced at construction (`is_type_array_subclass_element`
                    // resolves it via the registry, not a runtime env
                    // alias that `.new` would have already reset).
                    let attrs_to_fix: Vec<(String, String)> = self
                        .registry()
                        .class_attribute_is_types
                        .iter()
                        .filter(|((c, _), t)| c == cx.name && *t == &old_name)
                        .map(|((c, a), _)| (c.clone(), a.clone()))
                        .collect();
                    for key in attrs_to_fix {
                        self.registry_mut()
                            .class_attribute_is_types
                            .insert(key, new_name.clone());
                    }
                    cx.out
                        .class_role_param_bindings
                        .insert(old_name, Value::package(Symbol::intern(&new_name)));
                }
            }
        }
        // Remove type capture markers (but keep the variables
        // created by the deferred stmts for method closures)
        for param_name in role_param_values.keys() {
            self.env.remove(&format!("__type_capture__{}", param_name));
            // Don't remove the param name itself - methods may need it
        }
        Ok(())
    }

    /// Propagate the composed role's declared parents (`role R is C1 does R2`)
    /// into the class: parent roles' methods and attributes transit into the
    /// class, parent classes become inheritance parents.
    pub(super) fn propagate_composed_role_parent_specs(
        &mut self,
        cx: &mut RoleCompositionCx<'_>,
        base_role_name: &str,
        role: &RoleDef,
        role_param_values: &HashMap<String, Value>,
    ) {
        // ADR-0019 F4c-3: hoisted out of `if let Some(x) = self.registry()
        // ....cloned() { .. }` -- see the matching comment further down in
        // this function for why (temporary-lifetime-extension keeps the
        // `RegistryReadGuard` alive for the whole body, which now contains a
        // `self.registry_mut()` call).
        let maybe_parent_specs = self.registry().role_parents.get(base_role_name).cloned();
        if let Some(parent_specs) = maybe_parent_specs {
            for parent_spec in parent_specs {
                let resolved_parent = if let Some(v) = role_param_values.get(&parent_spec) {
                    type_value_name(v)
                } else if let Some((pbase, _)) = parent_spec.split_once('[') {
                    let p_args_str = &parent_spec[pbase.len() + 1..parent_spec.len() - 1];
                    let p_args = parse_role_type_args(p_args_str)
                        .into_iter()
                        .map(|arg| {
                            role_param_values
                                .get(&arg)
                                .map(|v| match v.view() {
                                    ValueView::Package(name) => name.resolve(),
                                    _ => v
                                        .to_string_value()
                                        .trim_start_matches('(')
                                        .trim_end_matches(')')
                                        .to_string(),
                                })
                                .unwrap_or(arg)
                        })
                        .collect::<Vec<_>>();
                    format!("{pbase}[{}]", p_args.join(","))
                } else {
                    parent_spec.clone()
                };
                let parent_base = resolved_parent
                    .split_once('[')
                    .map(|(b, _)| b)
                    .unwrap_or(resolved_parent.as_str());
                // ADR-0019 F4c-3: bind the clone in its own `let` rather
                // than `if let Some(x) = self.registry()....cloned() { ...
                // }` -- the latter's temporary-lifetime-extension rule keeps
                // the `RegistryReadGuard` alive for the WHOLE if-let body
                // (not just the condition), which panics the moment that
                // body calls `self.registry_mut()` (read -> write reentrant
                // lock upgrade; see `lock_reentry.rs`). This is the exact
                // hazard the F4c design note's R8 warns about.
                let maybe_parent_role = self.registry().roles.get(parent_base).cloned();
                if let Some(parent_role) = maybe_parent_role {
                    if !cx.out.composed_roles_list.contains(&resolved_parent) {
                        cx.out.composed_roles_list.push(resolved_parent.clone());
                    }
                    let parent_type_subs: Vec<(String, String)> = if let Some(parent_tps) =
                        self.registry().role_type_params.get(parent_base)
                    {
                        if let Some(bracket_start) = resolved_parent.find('[') {
                            let args_str =
                                &resolved_parent[bracket_start + 1..resolved_parent.len() - 1];
                            let args = parse_role_type_args(args_str);
                            parent_tps
                                .iter()
                                .zip(args.iter())
                                .map(|(p, a)| (p.clone(), a.clone()))
                                .collect()
                        } else {
                            Vec::new()
                        }
                    } else {
                        Vec::new()
                    };
                    for attr in &parent_role.attributes {
                        if !cx.class_def.attributes.iter().any(|a| a.name == attr.name) {
                            cx.class_def.attributes.push(attr.clone());
                        }
                    }
                    for (mname, overloads) in &parent_role.methods {
                        // Skip methods declared with `my` scope -- role-private
                        let non_my_overloads: Vec<&MethodDef> =
                            overloads.iter().filter(|md| !md.is_my).collect();
                        if non_my_overloads.is_empty() {
                            continue;
                        }
                        // If the composing role (base_role_name, e.g. R2) defines
                        // this method itself, it has already resolved the same-named
                        // method it inherits from its parent role (parent_base, e.g.
                        // R1). Do not re-propagate the parent's copy into the consumer
                        // as an independent candidate -- doing so would create a spurious
                        // X::Role::Composition::Conflict. The parent role's method is
                        // still reachable via a qualified call (self.R1::method).
                        let resolved_by_composing_role =
                            role.methods.get(mname).is_some_and(|defs| {
                                defs.iter().any(|d| d.role_origin.is_none() && !d.is_my)
                            });
                        if resolved_by_composing_role {
                            continue;
                        }
                        let composed: Vec<MethodDef> = if parent_type_subs.is_empty() {
                            non_my_overloads
                                .into_iter()
                                .map(|md| {
                                    let mut method = md.clone();
                                    if method.original_role.is_none() {
                                        method.original_role = method.role_origin.clone();
                                    }
                                    method.role_origin = Some(parent_base.to_string());
                                    method
                                })
                                .collect()
                        } else {
                            non_my_overloads
                                .into_iter()
                                .map(|md| {
                                    let mut method =
                                        substitute_type_params_in_method(md, &parent_type_subs);
                                    if method.original_role.is_none() {
                                        method.original_role = method.role_origin.clone();
                                    }
                                    method.role_origin = Some(parent_base.to_string());
                                    method
                                })
                                .collect()
                        };
                        let owner = Symbol::intern(cx.name);
                        let method_sym = Symbol::intern(mname);
                        let mut registry = self.registry_mut();
                        for def in composed {
                            registry.push_user_method(owner, method_sym, def);
                        }
                    }
                } else if self.registry().classes.contains_key(parent_base)
                    && !cx.class_def.parents.iter().any(|p| p == &resolved_parent)
                {
                    cx.class_def.parents.push(resolved_parent.clone());
                }
            }
        }
    }

    /// Handle role punning: `is Role` creates a punned class from the role.
    pub(super) fn install_role_puns(
        &mut self,
        punned_roles: &[String],
        hidden_punned_role_bases: &HashSet<String>,
    ) {
        for punned_role in punned_roles {
            let base_role = punned_role
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(punned_role.as_str());
            // Create a punned class entry if one doesn't already exist
            if !self.registry().classes.contains_key(punned_role.as_str())
                && !self.registry().classes.contains_key(base_role)
            {
                // Collect class parents and composed roles recursively from role hierarchy
                let mut punned_class_parents = Vec::new();
                let mut punned_composed_roles = Vec::new();
                let mut role_stack = vec![base_role.to_string()];
                let mut seen_roles = HashSet::new();
                while let Some(role_name) = role_stack.pop() {
                    if !seen_roles.insert(role_name.clone()) {
                        continue;
                    }
                    if let Some(rparents) = self.registry().role_parents.get(&role_name).cloned() {
                        for rp in rparents {
                            let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                            if self.registry().roles.contains_key(rp_base) {
                                // It's a role - add as composed role and recurse
                                if !punned_composed_roles.contains(&rp) {
                                    punned_composed_roles.push(rp.clone());
                                }
                                role_stack.push(rp_base.to_string());
                            } else if self.registry().classes.contains_key(rp_base)
                                && !punned_class_parents.contains(&rp)
                            {
                                // It's a class - add as parent
                                punned_class_parents.push(rp);
                            }
                        }
                    }
                }
                let punned_class = ClassDef {
                    parents: punned_class_parents,
                    attributes: Vec::new(),
                    attribute_types: HashMap::new(),
                    attribute_smileys: HashMap::new(),
                    attribute_built: HashMap::new(),
                    native_methods: HashSet::new(),
                    mro: [].into(),
                    wildcard_handles: Vec::new(),
                    alias_attributes: HashSet::new(),
                    class_level_attrs: HashMap::new(),
                };
                self.registry_mut()
                    .classes
                    .insert(base_role.to_string(), punned_class);
                if !punned_composed_roles.is_empty() {
                    self.registry_mut()
                        .class_composed_roles
                        .insert(base_role.to_string(), punned_composed_roles);
                }
                // Propagate hidden status from role to punned class
                // Read the flag under the guard, drop it, then write (read->write on
                // the same lock would deadlock).
                let base_role_is_hidden = self
                    .registry()
                    .roles
                    .get(base_role)
                    .is_some_and(|role_def| role_def.is_hidden);
                if base_role_is_hidden {
                    self.registry_mut()
                        .hidden_classes
                        .insert(base_role.to_string());
                }
                if hidden_punned_role_bases.contains(base_role) {
                    self.registry_mut()
                        .hidden_classes
                        .insert(base_role.to_string());
                }
                // Recompute MRO for the punned class
                let mro = self.class_mro(base_role);
                if let Some(cd) = self.registry_mut().classes.get_mut(base_role) {
                    cd.mro = mro;
                }
            }
            if hidden_punned_role_bases.contains(base_role) {
                self.registry_mut()
                    .hidden_classes
                    .insert(base_role.to_string());
            }
        }
    }
}
