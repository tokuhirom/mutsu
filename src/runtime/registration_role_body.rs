//! Named passes of `register_role_decl` (ADR-0019 D0): the `has`-attribute
//! and `does`-composition arms of the role-body walk. Pure mechanical
//! extraction from `registration_role.rs` — no behavior change.

use super::registration_class::{
    parse_role_type_args, substitute_type_params_in_method, type_value_name,
};
use super::registration_role_decl::RoleDeclCx;
use super::*;

impl Interpreter {
    /// The `has` arm of the role-body walk: record the attribute (or
    /// class-level attribute) and its per-(role, attr) trait metadata.
    pub(super) fn role_body_has_decl(
        &mut self,
        cx: &mut RoleDeclCx<'_>,
        stmt: &Stmt,
    ) -> Result<(), RuntimeError> {
        let decl = crate::opcode::CompiledAttrDecl::from_stmt(stmt);
        let attr_name_str = decl.name.clone();
        // A class-level (`my $.x` / `our $.x`) role attribute is NOT a
        // per-instance attribute: it becomes a class-level attribute on the
        // consuming class (accessor on the type object, `C.x`) — exactly
        // like `class C { my $.x }`. Record its default expr keyed by
        // (role, attr) and skip the per-instance attribute registration, so
        // no per-instance accessor shadows the class-level fallback.
        if decl.is_my || decl.is_our {
            self.registry_mut().role_class_level_attrs.insert(
                (cx.name.to_string(), attr_name_str.clone()),
                decl.default.clone(),
            );
            return Ok(());
        }
        cx.role_def
            .own_attribute_names
            .insert(attr_name_str.clone());
        // Carry an `is Type` container trait (`has @.a is G::A`) so it
        // can be transferred to the consuming class at composition and
        // its element type enforced.
        if let Some(it) = &decl.is_type {
            self.registry_mut()
                .role_attribute_is_types
                .insert((cx.name.to_string(), attr_name_str.clone()), it.clone());
        }
        // The declared type of a role attribute (`has Int $.x`,
        // `has Callable %!c{Mu:U}`) is recorded per (role, attr) and
        // copied onto every consuming/punned class, since a role has
        // no class of its own to hold `attribute_types`. `::?CLASS`
        // stays unresolved here — it names the *consuming* class, so
        // it is substituted at composition.
        if let Some(tc) = &decl.type_constraint {
            self.registry_mut()
                .role_attribute_types
                .insert((cx.name.to_string(), attr_name_str.clone()), tc.clone());
        }
        if let Some(ts) = &decl.type_smiley {
            self.registry_mut()
                .role_attribute_smileys
                .insert((cx.name.to_string(), attr_name_str.clone()), ts.clone());
        }
        // `is default(...)` on a role attribute can reference the role's
        // type parameters (`is default(T)`), so it cannot be evaluated
        // until composition. Stash the expression keyed by (role, attr);
        // it is copied to the consuming class and evaluated at instance
        // construction (with type params bound).
        if let Some(def_expr) = &decl.is_default {
            self.registry_mut().role_attribute_default_exprs.insert(
                (cx.name.to_string(), attr_name_str.clone()),
                def_expr.clone(),
            );
        }
        // Check if this attribute already exists from a composed role
        if let Some(existing) = cx
            .role_def
            .attributes
            .iter()
            .find(|a| a.name == attr_name_str)
        {
            // The attribute already exists from a parent role composition.
            // Record the conflict; the existing one came from a composed role.
            // We need to figure out which role contributed it.
            let parent_role =
                self.registry()
                    .role_parents
                    .get(cx.name)
                    .and_then(|parents| {
                        parents.iter().find(|p| {
                            let base = p.split_once('[').map(|(b, _)| b).unwrap_or(p.as_str());
                            self.registry().roles.get(base).is_some_and(|r| {
                                r.attributes.iter().any(|a| a.name == attr_name_str)
                            })
                        })
                    })
                    .cloned()
                    .unwrap_or_else(|| "unknown".to_string());
            let _ = existing;
            cx.role_def.attribute_conflicts.push((
                attr_name_str.clone(),
                cx.name.to_string(),
                parent_role,
            ));
        }
        // Apply role-level `is rw`: same logic as class_is_rw
        // `is readonly` on individual attributes overrides `is rw` on the role
        let effective_is_rw =
            !decl.is_readonly && (decl.is_rw || (cx.role_is_rw && decl.is_public));
        cx.role_def.attributes.push(ClassAttributeDef {
            name: attr_name_str.clone(),
            is_public: decl.is_public,
            default: decl.default.clone(),
            is_rw: effective_is_rw,
            is_required: decl.is_required.clone(),
            sigil: decl.sigil,
            where_constraint: decl.where_constraint.clone(),
        });
        let attr_var_name = if decl.is_public {
            format!(".{}", attr_name_str)
        } else {
            format!("!{}", attr_name_str)
        };
        self.apply_handle_specs_to_role(&decl.handles, &attr_var_name, &mut cx.role_def);
        Ok(())
    }

    /// The `does` arm of the role-body walk: record parent roles/classes and
    /// compose a parent role's attributes and methods into this role.
    pub(super) fn role_body_does_decl(
        &mut self,
        cx: &mut RoleDeclCx<'_>,
        stmt: &Stmt,
    ) -> Result<(), RuntimeError> {
        let Stmt::DoesDecl { name: role_name } = stmt else {
            unreachable!("role_body_does_decl called on a non-DoesDecl statement");
        };
        let name = cx.name;
        if *role_name == "__mutsu_role_hidden__" {
            cx.role_def.is_hidden = true;
            return Ok(());
        }
        let role_name_str = role_name.resolve();
        if let Some(hidden_name) = role_name_str.strip_prefix("__mutsu_role_hides__") {
            // Track hidden class relationship for this role
            self.registry_mut()
                .role_hides
                .entry(name.to_string())
                .or_default()
                .push(hidden_name.to_string());
            return Ok(());
        }
        // A sibling role referenced by its short name (`role Derived
        // does Base` inside `unit module M`, where Base is registered
        // as `M::Base`) must resolve to its qualified name — the same
        // resolution the class-body DoesDecl path already does.
        // JSON::Unmarshal composes all its CustomUnmarshaller roles
        // this way.
        let role_name_str = if !self.registry().roles.contains_key(&role_name_str)
            && !self.registry().classes.contains_key(&role_name_str)
            && !role_name_str.contains('[')
        {
            let resolved = self.resolve_declared_type_name(&role_name_str);
            if self.registry().roles.contains_key(&resolved) {
                resolved
            } else {
                role_name_str
            }
        } else {
            role_name_str
        };
        if self.registry().classes.contains_key(&role_name_str) {
            self.registry_mut()
                .role_parents
                .entry(name.to_string())
                .or_default()
                .push(role_name_str);
            return Ok(());
        }
        let base_role_name = role_name_str
            .split_once('[')
            .map(|(b, _)| b)
            .unwrap_or(role_name_str.as_str());
        if cx.type_params.iter().any(|tp| tp == base_role_name)
            || (!self.registry().roles.contains_key(base_role_name)
                && matches!(
                    base_role_name,
                    "Real" | "Numeric" | "Cool" | "Any" | "Mu" | "Positional" | "Associative"
                ))
        {
            self.registry_mut()
                .role_parents
                .entry(name.to_string())
                .or_default()
                .push(role_name_str);
            return Ok(());
        }
        // For a CONCRETE parametric parent (`does R1[Bool]`), resolve the
        // specific concretization's RoleDef by arity rather than the
        // by-name `roles` map — with a same-named role group (a
        // parameterized `R1[::T]` AND an unparameterized `R1`) the map
        // holds only the last-declared variant, so composing by bare name
        // would pull the wrong one and drop the parametric role's methods.
        // Skip when the application forwards a type param (`R1[::T]`),
        // which `resolve_role_candidate` rejects.
        let concretized_parent = if role_name_str.contains('[') && !role_name_str.contains("::") {
            self.resolve_role_candidate(&role_name_str)
                .ok()
                .flatten()
                .map(|(rd, _, _)| rd)
        } else {
            None
        };
        let role = match concretized_parent
            .or_else(|| self.registry().roles.get(base_role_name).cloned())
        {
            Some(r) => r,
            None => {
                // If trait_mod:<is> is defined and this is a lowercase name,
                // defer to custom trait dispatch instead of erroring.
                if (self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>"))
                    && role_name_str
                        .chars()
                        .next()
                        .is_some_and(|c| c.is_ascii_lowercase())
                {
                    cx.role_def
                        .deferred_custom_traits
                        .push(role_name_str.to_string());
                    return Ok(());
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
        // `does R1[::T]` inside a parameterized role forwards this
        // role's type parameter into the parent role. The concrete
        // value of `::T` is not known until THIS role is itself
        // concretized, so don't resolve the parent now (that errors
        // with "cannot use ::T in role application"); record the
        // forwarding binding (parent-param -> `::T`) via the
        // role_type_params branch below and defer real composition to
        // class-application time.
        let forwards_type_param = role_name_str.contains("::");
        let type_subs: Vec<(String, String)> = if !forwards_type_param
            && let Some((_, resolved_param_names, resolved_values)) =
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
                for (p, v) in resolved_param_names.iter().zip(resolved_values.iter()) {
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
                let args_str = &role_name_str[bracket_start + 1..role_name_str.len() - 1];
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
            if cx.role_def.attributes.iter().any(|a| a.name == attr.name) {
                // Already present. Only a real conflict if both
                // sides declared it directly (vs. inherited from
                // a shared ancestor in a diamond). Skip otherwise.
                let parent_owns = role.own_attribute_names.contains(&attr.name);
                let current_owns = cx.role_def.own_attribute_names.contains(&attr.name);
                if parent_owns && current_owns {
                    cx.role_def.attribute_conflicts.push((
                        attr.name.clone(),
                        name.to_string(),
                        base_role_name.to_string(),
                    ));
                }
            } else {
                cx.role_def.attributes.push(attr.clone());
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
                        let mut method = substitute_type_params_in_method(md, &type_subs);
                        if method.original_role.is_none() {
                            method.original_role = method.role_origin.clone();
                        }
                        method.role_origin = Some(base_role_name.to_string());
                        method
                    })
                    .collect()
            };
            cx.role_def
                .methods
                .entry(mname)
                .or_default()
                .extend(composed);
        }
        Ok(())
    }
}
