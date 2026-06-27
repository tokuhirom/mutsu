use super::registration_class::{
    AttrValidationCtx, ResolvedRoleCandidate, builtin_role_def, looks_like_type_arg_expr,
    make_delegation_method, parse_role_type_args, should_treat_role_arg_as_type_expr,
    substitute_type_params_in_method, type_value_name,
};
use super::*;
use crate::ast::{HandleSpec, ParamDef};
use crate::symbol::Symbol;

impl Interpreter {
    fn eval_role_arg_values(&mut self, arg_exprs: &[String]) -> Result<Vec<Value>, RuntimeError> {
        let mut values = Vec::with_capacity(arg_exprs.len());
        for expr in arg_exprs {
            if expr.trim_start().starts_with("::") {
                return Err(RuntimeError::new(
                    "X::Syntax::Malformed: cannot use ::T in role application".to_string(),
                ));
            }
            if should_treat_role_arg_as_type_expr(expr) {
                values.push(Value::Package(Symbol::intern(expr.trim())));
                continue;
            }
            match crate::parse_dispatch::parse_source(expr)
                .and_then(|(stmts, _)| self.eval_block_value(&stmts))
            {
                Ok(value) => values.push(value),
                Err(_) if looks_like_type_arg_expr(expr) => {
                    values.push(Value::Package(Symbol::intern(expr.trim())));
                }
                Err(err) => return Err(err),
            }
        }
        Ok(values)
    }

    fn role_constraint_specificity(&self, constraint: Option<&str>) -> i32 {
        let Some(constraint) = constraint else {
            return 0;
        };
        if constraint.starts_with("::") {
            return 1;
        }
        if constraint == "Any" || constraint == "Mu" {
            return 2;
        }
        if let Some(def) = self.registry().classes.get(constraint) {
            return 10 + def.parents.len() as i32;
        }
        if self.registry().roles.contains_key(constraint) {
            return 9;
        }
        8
    }

    fn role_candidate_specificity_score(&self, param_defs: &[ParamDef]) -> i32 {
        let mut score = 0i32;
        for pd in param_defs.iter().filter(|pd| !pd.named) {
            score += self.role_constraint_specificity(pd.type_constraint.as_deref());
            if pd.where_constraint.is_some() {
                score += 20;
            }
            if pd.literal_value.is_some() {
                score += 30;
            }
        }
        score
    }

    fn role_candidate_arity_ok(&self, args: &[Value], param_defs: &[ParamDef]) -> bool {
        if param_defs.is_empty() {
            return args.is_empty();
        }
        let positional_arg_count = args
            .iter()
            .filter(|arg| !matches!(arg, Value::Pair(..)))
            .count();
        let positional_params: Vec<&ParamDef> = param_defs.iter().filter(|pd| !pd.named).collect();
        let has_positional_slurpy = positional_params
            .iter()
            .any(|pd| pd.slurpy && !pd.name.starts_with('%'));
        let required = positional_params
            .iter()
            .filter(|pd| !pd.slurpy && pd.default.is_none() && !pd.optional_marker)
            .count();
        if positional_arg_count < required {
            return false;
        }
        if !has_positional_slurpy && positional_arg_count > positional_params.len() {
            return false;
        }
        true
    }

    pub(crate) fn resolve_role_candidate(
        &mut self,
        parent: &str,
    ) -> Result<Option<ResolvedRoleCandidate>, RuntimeError> {
        let parent = self.resolve_declared_type_name(parent);
        if let Some(bracket_start) = parent.find('[') {
            let args_str = &parent[bracket_start + 1..parent.len() - 1];
            let arg_exprs = parse_role_type_args(args_str);
            if arg_exprs
                .iter()
                .any(|expr| expr.trim_start().starts_with("::"))
            {
                return Err(RuntimeError::new(
                    "X::Syntax::Malformed: cannot use ::T in role application".to_string(),
                ));
            }
        }

        let base_role_name = if let Some(bracket) = parent.find('[') {
            &parent[..bracket]
        } else {
            &parent
        };
        let Some(candidates) = self.registry().role_candidates.get(base_role_name).cloned() else {
            if let Some(role) = self.registry().roles.get(base_role_name).cloned() {
                return Ok(Some((role, Vec::new(), Vec::new())));
            }
            if matches!(base_role_name, "Positional" | "Associative" | "Callable") {
                return Ok(Some((builtin_role_def(), Vec::new(), Vec::new())));
            }
            return Ok(None);
        };

        let arg_exprs = if let Some(bracket_start) = parent.find('[') {
            let args_str = &parent[bracket_start + 1..parent.len() - 1];
            parse_role_type_args(args_str)
        } else {
            Vec::new()
        };
        let arg_values = self.eval_role_arg_values(&arg_exprs)?;

        let mut matches: Vec<(RoleCandidateDef, i32, usize)> = candidates
            .into_iter()
            .enumerate()
            .filter_map(|(idx, candidate)| {
                let candidate_param_names = candidate
                    .type_param_defs
                    .iter()
                    .map(|pd| pd.name.clone())
                    .collect::<Vec<_>>();
                let ok = if self.role_candidate_arity_ok(&arg_values, &candidate.type_param_defs) {
                    let saved_env = self.env.clone();
                    let ok = self
                        .bind_function_args_values(
                            &candidate.type_param_defs,
                            &candidate_param_names,
                            &arg_values,
                        )
                        .is_ok();
                    self.env = saved_env;
                    ok
                } else {
                    false
                };
                if ok {
                    Some((
                        candidate.clone(),
                        self.role_candidate_specificity_score(&candidate.type_param_defs),
                        idx,
                    ))
                } else {
                    None
                }
            })
            .collect();

        if matches.is_empty() {
            return Err(RuntimeError::typed_msg(
                "X::Role::Parametric::NoSuchCandidate",
                "No matching candidate found for the parametric role",
            ));
        }

        matches.sort_by(|a, b| b.1.cmp(&a.1).then(b.2.cmp(&a.2)));
        let selected = matches.remove(0).0;
        // Properly bind args (handling named params, defaults, etc.) and extract
        // resolved values per param name, instead of using raw positional zip.
        let resolved_values = if !selected.type_param_defs.is_empty() {
            let saved_env = self.env.clone();
            let candidate_param_names: Vec<String> = selected
                .type_param_defs
                .iter()
                .map(|pd| pd.name.clone())
                .collect();
            let _ = self.bind_function_args_values(
                &selected.type_param_defs,
                &candidate_param_names,
                &arg_values,
            );
            let mut resolved = Vec::with_capacity(selected.type_params.len());
            for param_name in &selected.type_params {
                let value = self.env.get(param_name).cloned().unwrap_or(Value::Nil);
                resolved.push(value);
            }
            self.env = saved_env;
            resolved
        } else {
            arg_values
        };
        Ok(Some((
            selected.role_def,
            selected.type_params,
            resolved_values,
        )))
    }

    pub(crate) fn register_role_decl(
        &mut self,
        name: &str,
        type_params: &[String],
        type_param_defs: &[ParamDef],
        body: &[Stmt],
        role_is_rw: bool,
    ) -> Result<(), RuntimeError> {
        self.clear_private_zeroarg_method_cache();

        // Check for our-scoped declarations inside the role body.
        // In Raku, class/subset/enum/constant/role are implicitly our-scoped,
        // and explicit `our sub/method/variable` are also forbidden inside roles.
        let check_body: Vec<&Stmt> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .collect();
        for stmt in &check_body {
            let declaration = match stmt {
                // A `my class` inside a role is a lexically-scoped class private to the
                // role and is allowed; only an implicitly our-scoped `class` is forbidden.
                Stmt::ClassDecl {
                    is_lexical: false, ..
                } => Some("class"),
                Stmt::ClassDecl { .. } => None,
                Stmt::SubsetDecl { .. } => Some("subset"),
                Stmt::EnumDecl { .. } => Some("enum"),
                // A `my role` is lexically scoped and private to the role body, which
                // is allowed (like `my class`); only an implicitly our-scoped `role` is
                // forbidden.
                Stmt::RoleDecl { custom_traits, .. }
                    if custom_traits.iter().any(|(t, _)| t == "__my_scoped") =>
                {
                    None
                }
                Stmt::RoleDecl { .. } => Some("role"),
                Stmt::VarDecl {
                    is_our: true,
                    custom_traits,
                    ..
                } => {
                    if custom_traits.iter().any(|(t, _)| t == "__constant") {
                        Some("constant")
                    } else {
                        Some("variable")
                    }
                }
                Stmt::SubDecl { custom_traits, .. }
                    if custom_traits.iter().any(|(t, _)| t == "__our_scoped") =>
                {
                    Some("sub")
                }
                Stmt::MethodDecl { is_our: true, .. } => Some("method"),
                _ => None,
            };
            if let Some(decl) = declaration {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("declaration".to_string(), Value::str(decl.to_string()));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Cannot declare our-scoped {} inside of a role",
                        decl
                    )),
                );
                return Err(RuntimeError::typed("X::Declaration::OurScopeInRole", attrs));
            }
        }

        for param_def in type_param_defs {
            if param_def.name == "__type_only__"
                && let Some(type_name) = param_def.type_constraint.as_deref()
                && !type_name.starts_with("::")
                && !self.is_resolvable_type(type_name)
            {
                let mut attrs = std::collections::HashMap::new();
                attrs.insert("type".to_string(), Value::str(type_name.to_string()));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!(
                        "Invalid type '{}' used in role parameter list",
                        type_name
                    )),
                );
                return Err(RuntimeError::typed("X::Parameter::InvalidType", attrs));
            }
        }

        // If this is a stub declaration (body is `...`, `!!!`, or `???`)
        // and a real (non-stub) role already exists under this name, treat
        // the stub as a forward declaration / no-op — don't register a new
        // stub candidate that would shadow the real one.
        let is_stub_body = body.iter().any(|s| {
            matches!(s, Stmt::Expr(Expr::Call { name, .. })
                if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn")
        });
        if is_stub_body
            && type_params.is_empty()
            && self
                .registry()
                .roles
                .get(name)
                .is_some_and(|existing| !existing.is_stub_role)
        {
            return Ok(());
        }
        // Clean up stale punned class entry for this role name.
        self.registry_mut().classes.remove(name);
        self.registry_mut().hidden_classes.remove(name);
        self.registry_mut().class_composed_roles.remove(name);
        // When registering a parametric variant of an existing non-parametric role
        // (forming a role group), save the non-parametric role's parents so we can
        // restore them after the parametric variant adds its own parents.
        let prev_parents = if !type_params.is_empty()
            && self
                .registry()
                .roles
                .get(name)
                .is_some_and(|existing| !existing.is_stub_role)
        {
            self.registry().role_parents.get(name).cloned()
        } else {
            None
        };
        self.registry_mut().role_parents.remove(name);
        self.registry_mut().role_hides.remove(name);
        let mut role_def = RoleDef {
            attributes: Vec::new(),
            methods: HashMap::new(),
            // A yada-body forward declaration (`role Foo { ... }`) is a stub role:
            // mark it so the real definition that follows replaces it instead of
            // being treated as a second, conflicting role of the same name (which
            // made transitively-composed methods like `add_route` look like a
            // cross-role X::Role::Composition::Conflict).
            is_stub_role: is_stub_body,
            is_hidden: false,
            is_rw: role_is_rw,
            captured_env: None,
            wildcard_handles: Vec::new(),
            role_id: super::next_role_id(),
            attribute_conflicts: Vec::new(),
            own_attribute_names: HashSet::new(),
            deferred_body_stmts: Vec::new(),
            deferred_custom_traits: Vec::new(),
        };
        let is_parametric = !type_params.is_empty();
        let flattened_body: Vec<&Stmt> = body
            .iter()
            .flat_map(|s| match s {
                Stmt::SyntheticBlock(inner) => inner.iter().collect::<Vec<_>>(),
                other => vec![other],
            })
            .collect();
        // Pre-scan: collect attribute names declared in this role body.
        let mut role_own_attrs: HashSet<String> = HashSet::new();
        for stmt in &flattened_body {
            if let Stmt::HasDecl {
                name: attr_name, ..
            } = stmt
            {
                role_own_attrs.insert(attr_name.resolve());
            }
        }
        let role_attr_ctx = AttrValidationCtx {
            attrs: &role_own_attrs,
            pkg_name: name,
            pkg_kind: "role",
        };
        for stmt in flattened_body {
            match stmt {
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                    handles,
                    is_rw,
                    is_readonly,
                    type_constraint: _,
                    type_smiley: _,
                    is_required,
                    sigil,
                    where_constraint,
                    is_alias: _,
                    is_our: _,
                    is_my: _,
                    is_default: _,
                    is_type: _,
                    deprecated_message: _,
                    is_built: _,
                    unknown_traits: _,
                } => {
                    let attr_name_str = attr_name.resolve();
                    role_def.own_attribute_names.insert(attr_name_str.clone());
                    // Check if this attribute already exists from a composed role
                    if let Some(existing) = role_def
                        .attributes
                        .iter()
                        .find(|(n, ..)| n == &attr_name_str)
                    {
                        // The attribute already exists from a parent role composition.
                        // Record the conflict; the existing one came from a composed role.
                        // We need to figure out which role contributed it.
                        let parent_role = self
                            .registry()
                            .role_parents
                            .get(name)
                            .and_then(|parents| {
                                parents.iter().find(|p| {
                                    let base =
                                        p.split_once('[').map(|(b, _)| b).unwrap_or(p.as_str());
                                    self.registry().roles.get(base).is_some_and(|r| {
                                        r.attributes.iter().any(|(n, ..)| n == &attr_name_str)
                                    })
                                })
                            })
                            .cloned()
                            .unwrap_or_else(|| "unknown".to_string());
                        let _ = existing;
                        role_def.attribute_conflicts.push((
                            attr_name_str.clone(),
                            name.to_string(),
                            parent_role,
                        ));
                    }
                    // Apply role-level `is rw`: same logic as class_is_rw
                    // `is readonly` on individual attributes overrides `is rw` on the role
                    let effective_is_rw = !*is_readonly && (*is_rw || (role_is_rw && *is_public));
                    role_def.attributes.push((
                        attr_name_str.clone(),
                        *is_public,
                        default.clone(),
                        effective_is_rw,
                        is_required.clone(),
                        *sigil,
                        where_constraint.as_ref().map(|wc| wc.as_ref().clone()),
                    ));
                    let attr_var_name = if *is_public {
                        format!(".{}", attr_name_str)
                    } else {
                        format!("!{}", attr_name_str)
                    };
                    self.apply_handle_specs_to_role(handles, &attr_var_name, &mut role_def);
                }
                Stmt::DoesDecl { name: role_name } => {
                    if *role_name == "__mutsu_role_hidden__" {
                        role_def.is_hidden = true;
                        continue;
                    }
                    let role_name_str = role_name.resolve();
                    if let Some(hidden_name) = role_name_str.strip_prefix("__mutsu_role_hides__") {
                        // Track hidden class relationship for this role
                        self.registry_mut()
                            .role_hides
                            .entry(name.to_string())
                            .or_default()
                            .push(hidden_name.to_string());
                        continue;
                    }
                    if self.registry().classes.contains_key(&role_name_str) {
                        self.registry_mut()
                            .role_parents
                            .entry(name.to_string())
                            .or_default()
                            .push(role_name_str);
                        continue;
                    }
                    let base_role_name = role_name_str
                        .split_once('[')
                        .map(|(b, _)| b)
                        .unwrap_or(role_name_str.as_str());
                    if type_params.iter().any(|tp| tp == base_role_name)
                        || (!self.registry().roles.contains_key(base_role_name)
                            && matches!(
                                base_role_name,
                                "Real"
                                    | "Numeric"
                                    | "Cool"
                                    | "Any"
                                    | "Mu"
                                    | "Positional"
                                    | "Associative"
                            ))
                    {
                        self.registry_mut()
                            .role_parents
                            .entry(name.to_string())
                            .or_default()
                            .push(role_name_str);
                        continue;
                    }
                    let role = match self.registry().roles.get(base_role_name).cloned() {
                        Some(r) => r,
                        None => {
                            // If trait_mod:<is> is defined and this is a lowercase name,
                            // defer to custom trait dispatch instead of erroring.
                            if (self.has_proto("trait_mod:<is>")
                                || self.has_multi_candidates("trait_mod:<is>"))
                                && role_name_str
                                    .chars()
                                    .next()
                                    .is_some_and(|c| c.is_ascii_lowercase())
                            {
                                role_def
                                    .deferred_custom_traits
                                    .push(role_name_str.to_string());
                                continue;
                            }
                            return Err(RuntimeError::new(format!(
                                "Unknown role: {}",
                                role_name_str
                            )));
                        }
                    };
                    if role.is_stub_role {
                        return Err(RuntimeError::typed_msg(
                            "X::Role::Parametric::NoSuchCandidate",
                            "No matching candidate found for the parametric role",
                        ));
                    }
                    self.registry_mut()
                        .role_parents
                        .entry(name.to_string())
                        .or_default()
                        .push(role_name_str.clone());
                    // Use resolve_role_candidate to properly handle named
                    // arguments and default values in parameterized role
                    // composition.
                    let type_subs: Vec<(String, String)> =
                        if let Some((_, resolved_param_names, resolved_values)) =
                            self.resolve_role_candidate(&role_name_str)?
                        {
                            // Store the resolved param bindings so they are
                            // available when the child role is punned to a class
                            // and methods referencing role params are dispatched.
                            {
                                let mut registry = self.registry_mut();
                                let bindings = registry
                                    .class_role_param_bindings
                                    .entry(name.to_string())
                                    .or_default();
                                for (p, v) in
                                    resolved_param_names.iter().zip(resolved_values.iter())
                                {
                                    bindings.insert(p.clone(), v.clone());
                                }
                            }
                            resolved_param_names
                                .iter()
                                .zip(resolved_values.iter())
                                .map(|(p, v)| (p.clone(), type_value_name(v)))
                                .collect()
                        } else if let Some(parent_type_params) =
                            self.registry().role_type_params.get(base_role_name)
                        {
                            if let Some(bracket_start) = role_name_str.find('[') {
                                let args_str =
                                    &role_name_str[bracket_start + 1..role_name_str.len() - 1];
                                let type_args = parse_role_type_args(args_str);
                                parent_type_params
                                    .iter()
                                    .zip(type_args.iter())
                                    .map(|(p, a)| (p.clone(), a.clone()))
                                    .collect()
                            } else {
                                Vec::new()
                            }
                        } else {
                            Vec::new()
                        };
                    for attr in &role.attributes {
                        if role_def.attributes.iter().any(|(n, ..)| n == &attr.0) {
                            // Already present. Only a real conflict if both
                            // sides declared it directly (vs. inherited from
                            // a shared ancestor in a diamond). Skip otherwise.
                            let parent_owns = role.own_attribute_names.contains(&attr.0);
                            let current_owns = role_def.own_attribute_names.contains(&attr.0);
                            if parent_owns && current_owns {
                                role_def.attribute_conflicts.push((
                                    attr.0.clone(),
                                    name.to_string(),
                                    base_role_name.to_string(),
                                ));
                            }
                        } else {
                            role_def.attributes.push(attr.clone());
                        }
                    }
                    for (mname, overloads) in role.methods {
                        // Skip methods declared with `my` scope -- role-private
                        // Submethods (is_submethod=true) ARE composed even though
                        // they have is_my=true.
                        let non_my_overloads: Vec<MethodDef> = overloads
                            .into_iter()
                            .filter(|md| !md.is_my || md.is_submethod)
                            .collect();
                        if non_my_overloads.is_empty() {
                            continue;
                        }
                        let composed: Vec<MethodDef> = if type_subs.is_empty() {
                            non_my_overloads
                                .into_iter()
                                .map(|mut md| {
                                    if md.original_role.is_none() {
                                        md.original_role = md.role_origin.clone();
                                    }
                                    md.role_origin = Some(base_role_name.to_string());
                                    md
                                })
                                .collect()
                        } else {
                            non_my_overloads
                                .iter()
                                .map(|md| {
                                    let mut method =
                                        substitute_type_params_in_method(md, &type_subs);
                                    if method.original_role.is_none() {
                                        method.original_role = method.role_origin.clone();
                                    }
                                    method.role_origin = Some(base_role_name.to_string());
                                    method
                                })
                                .collect()
                        };
                        role_def.methods.entry(mname).or_default().extend(composed);
                    }
                }
                Stmt::MethodDecl {
                    name: method_name,
                    name_expr,
                    params,
                    param_defs,
                    body: method_body,
                    multi,
                    is_rw,
                    is_private,
                    is_our: _,
                    is_my,
                    is_submethod,
                    our_variable_form: _,
                    return_type,
                    is_default_candidate,
                    deprecated_message: _,
                    handles: method_handles,
                    custom_traits: _,
                    is_export: _,
                    export_tags: _,
                } => {
                    // Validate that $!attr references in the method body are declared
                    // in this role (same check as for class methods).
                    Self::validate_attr_declared_in_class(&role_attr_ctx, method_body)?;
                    // Validate that type constraints in method parameters are resolvable.
                    // Undeclared types like A::C should throw X::Parameter::InvalidType.
                    for pd in param_defs {
                        if let Some(tc) = pd.type_constraint.as_deref() {
                            // Skip type captures (::T), invocant markers, and role type params
                            if tc.starts_with("::")
                                || tc == "__invocant__"
                                || type_params.iter().any(|tp| tp == tc)
                            {
                                continue;
                            }
                            if !self.is_resolvable_type(tc) {
                                let mut attrs = std::collections::HashMap::new();
                                attrs.insert("type".to_string(), Value::str(tc.to_string()));
                                attrs.insert(
                                    "message".to_string(),
                                    Value::str(format!(
                                        "Invalid typename '{}' in parameter declaration.",
                                        tc
                                    )),
                                );
                                return Err(RuntimeError::typed(
                                    "X::Parameter::InvalidType",
                                    attrs,
                                ));
                            }
                        }
                    }
                    // Stub multi methods (body is `{...}`) that use ::?CLASS
                    // must be implemented by the composing class.
                    // Non-stub multi methods with ::?CLASS are fine.
                    let body_is_stub = {
                        let filtered: Vec<_> = method_body
                            .iter()
                            .filter(|s| !matches!(s, Stmt::SetLine(_)))
                            .collect();
                        filtered.len() == 1
                            && matches!(
                                filtered[0],
                                Stmt::Expr(Expr::Call { name, .. })
                                    if name == "__mutsu_stub_die"
                                        || name == "__mutsu_stub_warn"
                            )
                    };
                    if *multi
                        && body_is_stub
                        && (param_defs.iter().any(|pd| {
                            pd.type_constraint
                                .as_deref()
                                .is_some_and(|tc| tc.contains("?CLASS"))
                        }) || return_type
                            .as_deref()
                            .is_some_and(|rt| rt.contains("?CLASS")))
                    {
                        return Err(RuntimeError::typed_msg(
                            "X::Role::Unimplemented::Multi",
                            "Unimplemented multi method from role",
                        ));
                    }
                    let resolved_method_name = if let Some(expr) = name_expr {
                        self.eval_block_value(&[Stmt::Expr(expr.clone())])?
                            .to_string_value()
                    } else {
                        method_name.resolve()
                    };
                    let def = MethodDef {
                        params: params.clone(),
                        param_defs: param_defs.clone(),
                        body: std::sync::Arc::new(method_body.clone()),
                        is_rw: *is_rw,
                        is_private: *is_private,
                        is_multi: *multi,
                        is_my: *is_submethod,
                        role_origin: None,
                        original_role: None,
                        return_type: return_type.clone(),
                        compiled_code: None,
                        delegation: None,
                        is_default: *is_default_candidate,
                        deprecated_message: None,
                        is_submethod: *is_submethod,
                    };
                    // `my method` in roles are role-private, skip method table.
                    // Submethods (is_submethod) DO get composed even though
                    // is_my is true.
                    let is_role_private = *is_my && !*is_submethod;
                    if !is_role_private {
                        if *multi {
                            role_def
                                .methods
                                .entry(resolved_method_name.clone())
                                .or_default()
                                .push(def);
                        } else {
                            role_def
                                .methods
                                .insert(resolved_method_name.clone(), vec![def]);
                        }
                    }
                    // `handles` on a role method: synthesize forwarder methods.
                    if !is_role_private && !method_handles.is_empty() {
                        let source_attr_marker = format!("&{}", resolved_method_name);
                        for spec in method_handles {
                            match spec {
                                HandleSpec::Name(target) => {
                                    role_def
                                        .methods
                                        .entry(target.clone())
                                        .or_default()
                                        .push(make_delegation_method(&source_attr_marker, target));
                                }
                                HandleSpec::Rename { exposed, target } => {
                                    role_def
                                        .methods
                                        .entry(exposed.clone())
                                        .or_default()
                                        .push(make_delegation_method(&source_attr_marker, target));
                                }
                                HandleSpec::Wildcard => {
                                    role_def.wildcard_handles.push(source_attr_marker.clone());
                                }
                                HandleSpec::Regex(pattern) => {
                                    role_def
                                        .wildcard_handles
                                        .push(format!("{}:regex:{}", source_attr_marker, pattern));
                                }
                                HandleSpec::Type(_) => {}
                            }
                        }
                    }
                }
                Stmt::Expr(Expr::Call { name, .. })
                    if name == "__mutsu_stub_die" || name == "__mutsu_stub_warn" =>
                {
                    role_def.is_stub_role = true;
                }
                Stmt::SetLine(_) => {
                    // Skip source line annotations
                }
                _ => {
                    if is_parametric {
                        // Defer non-method/non-attribute statements until composition
                        // time so they can be re-evaluated with concrete type bindings.
                        role_def.deferred_body_stmts.push(stmt.clone());
                    } else {
                        // Defer execution until after the role is registered so that
                        // role methods can be called from within the role block body
                        // (e.g. `role R { method foo {}; R.foo }`).
                        role_def.deferred_body_stmts.push(stmt.clone());
                    }
                }
            }
        }
        // Capture the current environment for anonymous roles so that attribute
        // defaults referencing closure variables can be evaluated later.
        let has_expr_default = role_def
            .attributes
            .iter()
            .any(|(_, _, default, ..)| default.is_some());
        if has_expr_default {
            role_def.captured_env = Some(self.env.flatten());
        }
        // Capture the parents that were added during this registration
        // (these are the parents specific to this candidate).
        let candidate_parents = self
            .registry()
            .role_parents
            .get(name)
            .cloned()
            .unwrap_or_default();
        self.registry_mut()
            .role_candidates
            .entry(name.to_string())
            .or_default()
            .push(RoleCandidateDef {
                type_params: type_params.to_vec(),
                type_param_defs: type_param_defs.to_vec(),
                role_def: role_def.clone(),
                parents: candidate_parents,
                language_version: crate::parser::current_language_version(),
            });
        if self
            .registry()
            .roles
            .get(name)
            .is_none_or(|existing| existing.is_stub_role || type_params.is_empty())
        {
            self.registry_mut().roles.insert(name.to_string(), role_def);
            self.registry_mut()
                .user_declared_roles
                .insert(name.to_string());
        }
        if !type_params.is_empty() && !self.registry().role_type_params.contains_key(name) {
            self.registry_mut()
                .role_type_params
                .insert(name.to_string(), type_params.to_vec());
        }
        // When a parametric variant was registered over an existing non-parametric
        // role (forming a role group), merge the previous parents back into
        // role_parents so that role_parent_args_for can find all candidates' parents.
        if let Some(prev) = prev_parents {
            let mut registry = self.registry_mut();
            let current = registry.role_parents.entry(name.to_string()).or_default();
            for p in prev {
                if !current.contains(&p) {
                    current.push(p);
                }
            }
        }
        Ok(())
    }
}
