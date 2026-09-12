//! Deferred `also is Parent` resolution (issue #8099).
//!
//! Rakudo executes `also is Parent` **at its position in the class body**, so
//! everything the body established before that line is visible to it. mutsu's
//! parser hoists the statement out of the body onto the declaration's parent
//! list, which would otherwise force the parent to resolve before the body has
//! run at all. Two shapes need the body to have run first:
//!
//! * the parent arrives from a `use` **inside** the body
//!   (`class Kid { use Parentish; also is Parentish }` -- every one of
//!   `Font::AFM`'s `Font::Metrics::*` compunits), and
//! * the parent is a type the body itself declares
//!   (`class UC2 { class Inner { }; also is Inner }` -- `Intl::CLDR`'s five
//!   `CLDR::*` format systems).
//!
//! `validate_class_parents` therefore collects such a parent instead of
//! erroring (`ParentValidation::deferred_body_parents`), the header phase runs
//! without it, and this module re-resolves it once `run_class_body` returns --
//! still before `finalize_class_registration` computes the C3 MRO, which is the
//! last point at which a parent can be added.

use super::registration_class_compose::{RoleCompositionCx, RoleCompositionOutcome};
use super::registration_class_decl::BUILTIN_PARENT_TYPES;
use super::*;

/// What the deferred pass resolved.
pub(super) struct DeferredParentOutcome {
    /// The resolved inheritance parents, already appended to
    /// `class_def.parents`. The caller still needs them for
    /// `finalize_class_registration`'s metamodel-parent probe, which reads the
    /// raw parent list rather than the computed MRO.
    pub(super) parents: Vec<String>,
    /// Names the body never introduced after all. `also is Foo` on a name that
    /// is not a type is the same thing as `is Foo` on one -- rakudo's spelling
    /// of the named trait argument `trait_mod:<is>($type, :Foo)` -- so these go
    /// on to the custom-trait dispatch rather than erroring here; that site
    /// raises X::Inheritance::UnknownParent when no candidate claims them.
    pub(super) unclaimed: Vec<String>,
}

/// Everything the deferred pass needs from the header phase, plus the header's
/// own role-composition outcome so a deferred parent that turns out to name a
/// role can be merged into it rather than replacing it.
pub(super) struct DeferredParentCx<'a> {
    pub(super) class_lang_rev: &'a str,
    pub(super) is_hoisted_shell: bool,
    pub(super) composed_roles_list: &'a mut Vec<String>,
    pub(super) direct_composed_roles: &'a mut Vec<String>,
}

impl Interpreter {
    /// Is `base` a name a class may legally inherit from or compose?
    ///
    /// The same five-way existence test `validate_class_parents` applies, kept
    /// in one place so the deferred pass cannot drift from the header pass.
    fn parent_name_exists(&self, base: &str) -> bool {
        self.registry().classes.contains_key(base)
            || BUILTIN_PARENT_TYPES.contains(&base)
            || crate::runtime::types::is_builtin_role_name(base)
            || self.registry().roles.contains_key(base)
            || self.registry().enum_types.contains_key(base)
    }

    /// Resolve one deferred `also is` name now that the body has run, or `None`
    /// if it still names nothing.
    ///
    /// The class's own package scope is tried FIRST: a type the body declares
    /// registers under the enclosing class's name (`class UC2 { class Inner }`
    /// is `UC2::Inner`), and inside the body that is what the bare name refers
    /// to -- exactly the scoping the header form already gets for a sibling
    /// (`class Outer { class Inner { }; class Sub is Inner { } }`).
    fn resolve_deferred_body_parent(&self, class_name: &str, parent: &str) -> Option<String> {
        let user_facing = crate::value::user_facing_type_name(class_name);
        let own_scope = if parent.contains("::") {
            None
        } else {
            Some(format!("{}::{}", user_facing, parent))
        };
        let candidates = own_scope
            .iter()
            .map(String::as_str)
            .chain(std::iter::once(parent));
        for candidate in candidates {
            let resolved = self.resolve_declared_type_name(candidate);
            let base = resolved
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(resolved.as_str());
            let base = base.strip_prefix("::").unwrap_or(base);
            if self.parent_name_exists(base) {
                return Some(resolved);
            }
        }
        None
    }

    /// Apply the parents `validate_class_parents` deferred past the body.
    ///
    /// See [`DeferredParentOutcome`] for what comes back.
    pub(super) fn apply_deferred_body_parents(
        &mut self,
        name: &str,
        class_def: &mut ClassDef,
        deferred: &[String],
        cx: DeferredParentCx<'_>,
    ) -> Result<DeferredParentOutcome, RuntimeError> {
        let user_facing = crate::value::user_facing_type_name(name).to_string();
        let mut out = DeferredParentOutcome {
            parents: Vec::new(),
            unclaimed: Vec::new(),
        };
        let mut late_roles: Vec<String> = Vec::new();
        for parent in deferred {
            let Some(resolved) = self.resolve_deferred_body_parent(name, parent) else {
                // The body never introduced it. Hand it to the same custom
                // `trait_mod:<is>` dispatch an unknown header parent takes;
                // when no `trait_mod:<is>` exists at all there is nothing left
                // to try, so raise the error the header phase would have.
                if self.has_proto("trait_mod:<is>") || self.has_multi_candidates("trait_mod:<is>") {
                    out.unclaimed.push(parent.clone());
                    continue;
                }
                return Err(self.unknown_parent_error(&user_facing, parent));
            };
            let base = resolved
                .split_once('[')
                .map(|(b, _)| b)
                .unwrap_or(resolved.as_str());
            if base == user_facing {
                let mut attrs = HashMap::new();
                attrs.insert("name".to_string(), Value::str(user_facing.clone()));
                attrs.insert(
                    "message".to_string(),
                    Value::str(format!("'{}' cannot inherit from itself.", user_facing)),
                );
                return Err(RuntimeError::typed("X::Inheritance::SelfInherit", attrs));
            }
            // A name that is a role and not also a class is composed, not
            // inherited -- `also is R` puns/composes R exactly like the header
            // `is R` form does.
            if !self.registry().classes.contains_key(base) && self.is_role_type_name(base) {
                late_roles.push(resolved);
                continue;
            }
            if !class_def.parents.contains(&resolved) {
                class_def.parents.push(resolved.clone());
            }
            out.parents.push(resolved);
        }
        if !late_roles.is_empty() {
            self.compose_deferred_body_roles(name, class_def, &late_roles, cx)?;
        }
        Ok(out)
    }

    /// Compose the deferred `also is` parents that turned out to name roles.
    ///
    /// The header pass already ran and recorded its own composition, so this
    /// runs the same helper over just the late names and then re-records the
    /// MERGED lists -- `record_class_composed_roles` rewrites the class's rows
    /// wholesale, so handing it only the late names would drop the header's.
    fn compose_deferred_body_roles(
        &mut self,
        name: &str,
        class_def: &mut ClassDef,
        late_roles: &[String],
        cx: DeferredParentCx<'_>,
    ) -> Result<(), RuntimeError> {
        let outcome = {
            let mut compose_cx = RoleCompositionCx {
                name,
                class_lang_rev: cx.class_lang_rev,
                class_def,
                out: RoleCompositionOutcome::default(),
                is_hoisted_shell: cx.is_hoisted_shell,
            };
            let no_pre_args: Vec<Option<&[crate::opcode::DeclTraitArg]>> =
                vec![None; late_roles.len()];
            self.compose_class_parent_roles(&mut compose_cx, late_roles, &[], &no_pre_args)?;
            compose_cx.out
        };
        if !outcome.class_role_param_bindings.is_empty() {
            self.registry_mut()
                .class_role_param_bindings
                .entry(name.to_string())
                .or_default()
                .extend(outcome.class_role_param_bindings);
        }
        self.install_role_puns(&outcome.punned_roles, &outcome.hidden_punned_role_bases);
        cx.composed_roles_list.extend(outcome.composed_roles_list);
        cx.direct_composed_roles
            .extend(outcome.direct_composed_roles);
        self.record_class_composed_roles(
            name,
            class_def,
            cx.composed_roles_list,
            cx.direct_composed_roles,
        );
        Ok(())
    }
}
