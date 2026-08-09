//! Named phases of `register_class_decl` (ADR-0019 D0): the body-level
//! `also does Role` arm of the class-body walk. Pure mechanical extraction
//! from `registration_class_decl.rs` — no behavior change.

use super::registration_class_body::{ClassBodyCx, ClassBodyFlow};
use super::*;

impl Interpreter {
    /// The `also does Role` arm of the class-body walk: compose the named
    /// role's attributes, methods, and parents into the class under
    /// construction and run its body for the composition.
    pub(super) fn class_body_does_decl(
        &mut self,
        cx: &mut ClassBodyCx<'_>,
        stmt: &Stmt,
    ) -> Result<ClassBodyFlow, RuntimeError> {
        let Stmt::DoesDecl {
            name: role_name, ..
        } = stmt
        else {
            unreachable!("class_body_does_decl called on a non-DoesDecl statement");
        };
        let raw_role_name = role_name.resolve();
        // An imported role referenced by its short alias (`does
        // PackageRepo` where the role is registered as
        // `MyMod::PackageRepo`) must be resolved to its qualified
        // name. `does_parents` on a named class already resolves via
        // `resolve_declared_type_name`; the body `DoesDecl` emitted
        // for an anonymous `class :: does R` did not, so it failed
        // with "Unknown role" for module-exported roles.
        let role_name_str = if self.registry().roles.contains_key(&raw_role_name) {
            raw_role_name.clone()
        } else {
            let resolved = self.resolve_declared_type_name(&raw_role_name);
            if self.registry().roles.contains_key(&resolved) {
                resolved
            } else {
                raw_role_name.clone()
            }
        };
        if !self.registry().roles.contains_key(&role_name_str)
            && matches!(
                role_name_str.as_str(),
                "Real" | "Numeric" | "Cool" | "Any" | "Mu" | "Positional" | "Associative"
            )
        {
            if !cx.class_def.parents.iter().any(|p| p == &role_name_str) {
                cx.class_def.parents.insert(0, role_name_str.clone());
                cx.class_def.mro = [].into();
            }
            return Ok(ClassBodyFlow::SkipTail);
        }
        let role = self
            .registry()
            .roles
            .get(&role_name_str)
            .cloned()
            .ok_or_else(|| RuntimeError::new(format!("Unknown role: {}", role_name_str)))?;
        if role.is_stub_role {
            return Err(RuntimeError::typed_msg(
                "X::Role::Parametric::NoSuchCandidate",
                "No matching candidate found for the parametric role",
            ));
        }
        // Look up the role's language revision for submethod composition rules.
        let role_lang_rev_does = self
            .type_metadata
            .get(&role_name_str)
            .and_then(|m| m.get("language-revision"))
            .map(|v| v.to_string_value())
            .unwrap_or_else(|| "c".to_string());
        let compose_submethods_does = cx.class_lang_rev == "c" && role_lang_rev_does == "c";
        for attr in &role.attributes {
            if !cx.class_def.attributes.iter().any(|a| a.name == attr.name) {
                cx.class_def.attributes.push(attr.clone());
            }
        }
        for (mname, overloads) in role.methods {
            let composed: Vec<MethodDef> = overloads
                .into_iter()
                .filter(|md| !md.is_my || (md.is_submethod && compose_submethods_does))
                .map(|mut md| {
                    if md.original_role.is_none() {
                        md.original_role = md.role_origin.clone();
                    }
                    md.role_origin = Some(role_name_str.clone());
                    md
                })
                .collect();
            if composed.is_empty() {
                continue;
            }
            cx.class_def
                .methods
                .entry(mname)
                .or_default()
                .extend(composed);
        }
        // Transfer wildcard handles from role to class
        for wh in &role.wildcard_handles {
            if !cx.class_def.wildcard_handles.contains(wh) {
                cx.class_def.wildcard_handles.push(wh.clone());
            }
        }
        if !cx.class_def.parents.iter().any(|p| p == &role_name_str) {
            // Keep role composition visible in MRO introspection.
            cx.class_def.parents.insert(0, role_name_str.clone());
            cx.class_def.mro = [].into();
        }
        // Transfer role's own parents (from `is` declarations) to the class
        if let Some(rparents) = self.registry().role_parents.get(&role_name_str).cloned() {
            for rp in rparents {
                let rp_base = rp.split_once('[').map(|(b, _)| b).unwrap_or(rp.as_str());
                if self.registry().classes.contains_key(rp_base)
                    && !cx.class_def.parents.iter().any(|p| p == &rp)
                {
                    cx.class_def.parents.push(rp.clone());
                    cx.class_def.mro = [].into();
                }
            }
        }
        // `also does R` is a composition like any other, so R's
        // body runs — and so do the bodies of the roles R composes.
        self.run_role_body_for_composition(&role_name_str, cx.name, &role.deferred_body)?;
        self.run_composed_role_ancestor_bodies(&role_name_str, cx.name)?;
        Ok(ClassBodyFlow::RunTail)
    }
}
