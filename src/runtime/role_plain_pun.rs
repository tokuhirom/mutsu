//! The class a plain (non-parametric) role puns to when a method is called on
//! its type object: `role Q { method m {...} }; Q.m` (#17408).
//!
//! Rakudo keeps one pun per role in the role's HOW and dispatches `Q.m` to it.
//! mutsu used to compose a class named after the role itself for each such
//! call and withdraw it again afterwards, because a class of the role's own
//! name left in the registry changes later role composition and construction
//! (see `withdraw_role_pun`). That rebuilt the pun -- cloning every method
//! definition -- and wrote the registry twice per call, which also moved the
//! write generation every generation-keyed cache is dropped by: ~80 us per
//! call against ~2 us for a method on a class.
//!
//! The pun now lives under a storage name of its own, `Q\0pun`. The `\0`
//! suffix is the storage-name mangling `.^name` already strips, the same way a
//! parametric role's default pun is `P\0default[Any]`. The class is composed
//! once through the ordinary class declaration path (`does Q`), so it is the
//! pun rakudo would build: `self.HOW` in its methods is a `ClassHOW`,
//! `self ~~ Q` holds, and the role body runs once.
use super::*;

impl Interpreter {
    /// The storage name of the pun class of the plain role `role_name`.
    // Cost: O(n), n = bytes of the role name.
    fn plain_role_pun_name(role_name: &str) -> String {
        // The inverse of `plain_role_pun_source`.
        format!("{role_name}\u{0}pun")
    }

    /// The role whose pun class is named `class_name` (`R` for `R\0pun`), or
    /// `None` for any other name.
    // Cost: O(n), n = bytes of the name.
    pub(crate) fn plain_role_pun_source(class_name: &str) -> Option<&str> {
        class_name.strip_suffix("\u{0}pun")
    }

    /// The pun class of the plain role `role_name`, composed on the first call
    /// and found by name afterwards. `None` when `role_name` is not a role, is
    /// a stub, or is a role group without a zero-parameter candidate (those
    /// pun through their parameterization instead). A group that has one puns
    /// that candidate, persistently, like a lone plain role (#18160).
    // Cost: O(n) while the pun exists, n = bytes of the role name (the name
    // and one registry probe); the first call composes the role into a class,
    // O(m), m = the role's and its parents' members.
    pub(crate) fn ensure_plain_role_pun_class(
        &mut self,
        role_name: &str,
    ) -> Result<Option<String>, RuntimeError> {
        let pun_name = Self::plain_role_pun_name(role_name);
        if self.registry().classes.contains_key(&pun_name) {
            return Ok(Some(pun_name));
        }
        let language_version = {
            let registry = self.registry();
            match registry.roles.get(role_name) {
                Some(role) if !role.is_stub_role => {}
                _ => return Ok(None),
            }
            // A role group puns its zero-parameter candidate, which is the one a
            // bare `R` selects (`role R[\a] {...}; role R {...}; R.m`, the
            // `CSS::Units` shape). A group with no such candidate puns through a
            // parameterization instead.
            match registry.role_candidates.get(role_name) {
                Some(cs) => match cs.iter().rev().find(|c| c.type_params.is_empty()) {
                    Some(plain) => plain.language_version.clone(),
                    None => return Ok(None),
                },
                None => crate::parser::current_language_version(),
            }
        };
        let parents = [role_name.to_string()];
        let modifiers = super::registration_class::ClassDeclModifiers {
            class_is_rw: false,
            is_hidden: false,
            is_lexical: false,
            hidden_parents: &[],
            body_parents: &[],
            does_parents: &parents,
            language_version: &language_version,
            is_stub: false,
            trusts: &[],
            own_attribute_names: &[],
            attr_decls: &[],
            attr_type_aliases: &[],
            method_name_chunks: &[],
            method_decls: &[],
            declared_static_names: &[],
            nested_decl_names: &[],
            parent_pre_args: &[None],
            compiled_fns: &crate::opcode::CompiledFns::default(),
            body_plan: &[],
            is_hoisted_shell: crate::runtime::HoistedShell::No,
            traits_ran_in_shell: false,
        };
        self.register_class_decl(&pun_name, &parents, modifiers)?;
        self.store_language_revision_from_version(&pun_name, &language_version);
        // Another thread can meet the pun by name (a method returned `self`);
        // let it compose the same class. A recipe without type arguments is a
        // plain role's pun (a parametric pun always has some).
        self.types.published_mixins.publish_pun(
            &pun_name,
            crate::runtime::published_mixins::PunRecipe {
                base_name: role_name.to_string(),
                type_args: Vec::new(),
                from_defaults: false,
            },
        );
        Ok(Some(pun_name))
    }
}
