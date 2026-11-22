//! Whose private attribute a `$!x` access reads (#17936).
//!
//! A class and its parent may each declare `$!x`; each keeps its own copy,
//! and `$!x` names the copy of the package the code is *written* in. A method
//! body learns that package from the dispatch stack (the running method's
//! owner). A closure the method made, or a block inside it, runs in a frame
//! of its own: once the method has returned, or when the closure is called
//! from another method, the dispatch stack names someone else (or nobody), so
//! the access fell back to the bare key -- the most-derived class's copy.
//! Such a frame carries the fixture record of the method it was created in
//! (`?CLASS` / `?ROLE`, ADR-14843), which names the lexical owner.

use super::*;

impl Interpreter {
    /// The owner of a `$!x` access made at `site`, and whether it is a role:
    /// the running method's owner for a method body, the creating method's
    /// `?ROLE` (else `?CLASS`) for any other frame that has a fixture record.
    // Cost: O(1).
    pub(crate) fn attr_access_owner(
        &self,
        site: Option<(&CompiledCode, usize)>,
    ) -> (Option<crate::symbol::Symbol>, bool) {
        let method_owner = || {
            (
                self.method_class_stack_top_sym(),
                self.method_class_top_is_role(),
            )
        };
        let Some((code, _)) = site else {
            return method_owner();
        };
        if Self::is_method_body(code) {
            return method_owner();
        }
        let Some(fixtures) = self.frame_fixtures() else {
            return method_owner();
        };
        if let Some(role) = fixtures.role {
            return (Some(role), true);
        }
        match fixtures.class.as_ref().map(Value::view) {
            Some(ValueView::Package(class)) => (Some(class), false),
            _ => method_owner(),
        }
    }
}

impl Interpreter {
    /// The class that stores role `role`'s own copy of the private attribute
    /// `bare` on `self_val`, when `self_val` is a plain instance whose class
    /// composed `role` at one of its MRO levels and that level keeps a
    /// per-class copy (`Level\0bare`).
    ///
    /// `$obj does N; $obj does P` reblesses into `C+{N}+{P}`; when N and P
    /// both declare `$!a`, each mixin level keeps its copy under its own
    /// class (`seed_mixin_role_attributes`). A role method runs with that
    /// level class as its owner, but a closure it made names the role
    /// (`?ROLE`), so the role is mapped back to the level composing it,
    /// most-derived first (#17890, #17936). `None` when no level does, and
    /// for a value that is no plain instance (a `Mixin` wrapper keeps its
    /// role cell).
    // Cost: O(l * r), l = MRO length of `self_val`'s class, r = roles a
    // level composes; taken only for a role-owned access outside a method
    // body, on an instance whose map holds owner-qualified keys.
    fn role_level_attr_owner(
        &self,
        self_val: &Value,
        role: crate::symbol::Symbol,
        bare: crate::symbol::Symbol,
        sigil: char,
        map: &crate::value::AttrMap,
    ) -> Option<crate::symbol::Symbol> {
        let class = match self_val.view() {
            ValueView::Instance { class_name, .. } => class_name,
            _ => return None,
        };
        let role_root = |name: &str| name.split(['[', '\0']).next().unwrap_or(name).to_string();
        let role = role_root(role.as_str());
        let registry = self.registry();
        let mro = registry.classes.get(class.as_str())?.mro.clone();
        mro.iter()
            .find(|level| {
                registry
                    .class_composed_roles
                    .get(level.as_str())
                    .is_some_and(|roles| roles.iter().any(|r| role_root(r) == role))
                    && {
                        let keys = Self::qualified_attr_keys(**level, bare, sigil);
                        map.contains_key(keys.with_sigil) || map.contains_key(keys.plain)
                    }
            })
            .copied()
    }

    /// The owner whose copy a private `$!bare` access with owner `owner`
    /// reads in `map`: for a role owner on an instance reblessed into a
    /// mixin type, the level class composing that role
    /// ([`Self::role_level_attr_owner`]); otherwise `owner` itself.
    // Cost: O(1) unless `owner` is a role; then role_level_attr_owner's.
    pub(super) fn private_attr_owner_in(
        &self,
        self_val: &Value,
        owner: Option<crate::symbol::Symbol>,
        owner_is_role: bool,
        bare: crate::symbol::Symbol,
        sigil: char,
        map: &crate::value::AttrMap,
    ) -> Option<crate::symbol::Symbol> {
        match owner {
            Some(role) if owner_is_role => self
                .role_level_attr_owner(self_val, role, bare, sigil, map)
                .or(owner),
            _ => owner,
        }
    }
}
