//! Named phases of `register_class_decl` (ADR-0019 D0): the body-level
//! `also does Role` arm of the class-body walk. Pure mechanical extraction
//! from `registration_class_decl.rs` — no behavior change.

use super::registration_class::{parse_role_type_args, should_treat_role_arg_as_type_expr};
use super::registration_class_body::{ClassBodyCx, ClassBodyFlow, PendingAttrCompose};
use super::registration_class_compose::{RoleCompositionCx, RoleCompositionOutcome};
use super::*;

impl Interpreter {
    /// The `also does Role` arm of the class-body walk: compose the named
    /// role's attributes, methods, and parents into the class under
    /// construction and run its body for the composition.
    pub(super) fn class_body_does_decl(
        &mut self,
        cx: &mut ClassBodyCx<'_>,
        role_name: Symbol,
        arg_chunks: Option<&[crate::opcode::DeclTraitArg]>,
    ) -> Result<ClassBodyFlow, RuntimeError> {
        let raw_role_name = role_name.resolve();
        // An imported role referenced by its short alias (`does
        // PackageRepo` where the role is registered as
        // `MyMod::PackageRepo`) must be resolved to its qualified
        // name. `does_parents` on a named class already resolves via
        // `resolve_declared_type_name`; the body `DoesDecl` emitted
        // for an anonymous `class :: does R` did not, so it failed
        // with "Unknown role" for module-exported roles.
        let role_name_str = self.resolve_declared_type_name(&raw_role_name);
        let base_role_name = role_name_str
            .split_once('[')
            .map(|(base, _)| base)
            .unwrap_or(role_name_str.as_str());
        if !self.registry().roles.contains_key(base_role_name)
            && crate::runtime::types::is_builtin_role_name(base_role_name)
        {
            if !cx.class_def.parents.iter().any(|p| p == &role_name_str) {
                cx.class_def.parents.insert(0, role_name_str.clone());
                cx.class_def.mro = [].into();
            }
            return Ok(ClassBodyFlow::SkipTail);
        }
        let has_type_expr_arg = role_name_str
            .find('[')
            .map(|start| {
                let args_str = &role_name_str[start + 1..role_name_str.len() - 1];
                parse_role_type_args(args_str)
                    .iter()
                    .any(|arg| should_treat_role_arg_as_type_expr(arg))
            })
            .unwrap_or(false);
        let pre_args = if has_type_expr_arg {
            None
        } else if let Some(chunks) = arg_chunks {
            let mut values = Vec::with_capacity(chunks.len());
            for chunk in chunks {
                values.push(self.eval_decl_trait_arg(chunk)?);
            }
            Some(values)
        } else {
            None
        };
        let resolved = self
            .resolve_role_candidate_with_args(&role_name_str, pre_args.as_deref())?
            .ok_or_else(|| RuntimeError::new(format!("Unknown role: {}", role_name_str)))?;

        let old_composed = self
            .registry()
            .class_composed_roles
            .get(cx.name)
            .cloned()
            .unwrap_or_default();
        let old_direct = self
            .registry()
            .class_direct_composed_roles
            .get(cx.name)
            .cloned()
            .unwrap_or_default();
        let mut composition = RoleCompositionCx {
            name: cx.name,
            class_lang_rev: cx.class_lang_rev,
            class_def: &mut cx.class_def,
            out: RoleCompositionOutcome::default(),
            is_hoisted_shell: cx.is_hoisted_shell,
        };
        self.compose_role_into_class(
            &mut composition,
            &role_name_str,
            base_role_name,
            false,
            resolved,
        )?;
        let mut outcome = composition.out;
        cx.class_own_attrs.extend(
            cx.class_def
                .attributes
                .iter()
                .map(|attribute| attribute.name.clone()),
        );
        outcome.composed_roles_list.splice(0..0, old_composed);
        outcome.direct_composed_roles.splice(0..0, old_direct);
        self.registry_mut()
            .class_role_param_bindings
            .entry(cx.name.to_string())
            .or_default()
            .extend(outcome.class_role_param_bindings);
        self.record_class_composed_roles(
            cx.name,
            &mut cx.class_def,
            &outcome.composed_roles_list,
            &outcome.direct_composed_roles,
        );
        Ok(ClassBodyFlow::RunTail)
    }

    /// Queue the `compose` hook of every role-composed attribute whose
    /// trait-mutated meta-object (copied onto this class by
    /// `compose_role_into_class`) overrides `Attribute.compose`, so it runs
    /// with this class as the package — before the stub-requirement check, as
    /// in Rakudo, where an accessor such a hook adds can satisfy a role's
    /// `method x {...}`. Attributes declared in the class body itself were
    /// already queued by `apply_attribute_traits`.
    ///
    /// Cost: O(a), a = attributes of the class.
    pub(super) fn queue_role_attribute_composes(
        &mut self,
        cx: &mut ClassBodyCx<'_>,
        own_attribute_names: &[Symbol],
    ) {
        for attr in &cx.class_def.attributes {
            if own_attribute_names
                .iter()
                .any(|own| own.resolve() == attr.name)
            {
                continue;
            }
            let Some(obj) = self
                .registry()
                .class_attribute_trait_objects
                .get(&(cx.name.to_string(), attr.name.clone()))
                .cloned()
            else {
                continue;
            };
            if self.mixin_has_compose_hook(&obj) {
                cx.pending_attr_composes.push(PendingAttrCompose::Attribute(
                    cx.name.to_string(),
                    attr.name.clone(),
                ));
            }
        }
    }
}
