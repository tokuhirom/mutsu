//! The per-site memo for a typed declaration of a user class or role
//! (`my Tag $t = ...`), and the Nil seed both declaration ops derive from it.
//!
//! `SetVarTypeScoped`, the declaration's `TypeCheck` and a Nil store each
//! resolved the constraint spelling on every execution: the type-capture,
//! package-alias, constant-alias and lexical-type probes, the package-chain
//! qualification, and `nominal_type_object_name_for_constraint` for the type
//! object a Nil leaves in the variable. For a spelling that names a class or
//! role registered under exactly that name, every one of those answers is a
//! function of the registry, the running package and a few env bindings. The
//! registry and package parts are memoized per site
//! ([`crate::value::TypeDeclSiteCaches`]); the env parts are asked again on
//! every hit, so a binding that appears later still takes effect (#12917).
use super::*;

impl Interpreter {
    /// Whether the declaration constraint at constant `tc_idx` names a user
    /// class or role whose every resolution step returns the spelling
    /// unchanged — so the declaration may register it as written and seed a
    /// Nil with the type object of that very name.
    ///
    /// Asked on every call, because they read state the registry generation
    /// does not cover: a `::T` capture under the spelling (or under its head,
    /// which the capture resolver splits off at the first `:`), a module's
    /// package alias of it, a `CORE::` pin in progress, and the two env
    /// bindings [`Self::decl_type_name_rebound_in_env`] checks. Memoized per
    /// site for one (registry write generation, running package): the
    /// registry membership, the alias-chain and package-chain resolution, and
    /// the nominal type-object name.
    // Cost: O(|name|) plus one or two env probes on a memo hit; a miss adds
    // the full resolution, O(p), p = packages probed.
    pub(super) fn type_decl_constraint_is_plain_user_type(
        &self,
        code: &CompiledCode,
        tc_idx: u32,
        constraint: &str,
    ) -> bool {
        if self.types.core_pin_depth > 0 || !Self::plain_user_type_spelling(constraint) {
            return false;
        }
        let head = constraint.split(':').next().unwrap_or(constraint);
        if self.has_type_capture_binding(constraint)
            || (head.len() != constraint.len() && self.has_type_capture_binding(head))
            || self.types.package_type_aliases.contains_name(constraint)
        {
            return false;
        }
        let generation = self.registry_write_generation();
        let Some(key) = crate::value::TypeDeclSiteCaches::user_key(
            generation,
            self.current_package_sym().id(),
        ) else {
            return false;
        };
        let sites = code.constants.len();
        let idx = tc_idx as usize;
        if !code.type_decl_sites.user_cached(sites, idx, key) {
            if !self.user_type_constraint_resolves_to_itself(constraint) {
                return false;
            }
            if self.registry_write_generation() == generation {
                code.type_decl_sites.remember_user(sites, idx, key);
            }
        }
        !self.decl_type_name_rebound_in_env(constraint)
    }

    /// A type name and nothing else: optionally package-qualified (`A::B`),
    /// with no smiley, coercion, parameterization, hash-key part, capture
    /// sigil, `CORE::` pin or lexical storage mangling.
    // Cost: O(|name|).
    fn plain_user_type_spelling(constraint: &str) -> bool {
        let bytes = constraint.as_bytes();
        if bytes.is_empty() || constraint.starts_with("CORE::") {
            return false;
        }
        let mut i = 0;
        while i < bytes.len() {
            match bytes[i] {
                b':' => {
                    // Only a `::` separator with a name on both sides.
                    if i == 0 || bytes.get(i + 1) != Some(&b':') || i + 2 >= bytes.len() {
                        return false;
                    }
                    if bytes[i + 2] == b':' {
                        return false;
                    }
                    i += 2;
                }
                b'(' | b')' | b'[' | b']' | b'{' | b'}' | b' ' | b'<' | b'\0' => return false,
                _ => i += 1,
            }
        }
        true
    }

    /// The memoized half of [`Self::type_decl_constraint_is_plain_user_type`]:
    /// `constraint` is a class or role registered under that very name (not
    /// a builtin, subset or enum spelling), and the resolution
    /// `exec_set_var_type` and the Nil seed run return it unchanged.
    // Cost: O(p), p = packages probed by the qualification walk.
    fn user_type_constraint_resolves_to_itself(&self, constraint: &str) -> bool {
        if crate::runtime::utils::is_known_type_constraint(constraint)
            || Self::is_builtin_type(constraint)
        {
            return false;
        }
        {
            let reg = self.registry();
            if !(reg.classes.contains_key(constraint) || reg.roles.contains_key(constraint))
                || reg.subsets.contains_key(constraint)
                || reg.enum_types.contains_key(constraint)
            {
                return false;
            }
        }
        if self.try_resolved_type_capture_name(constraint).is_some() {
            return false;
        }
        if !crate::qualified::is_qualified_str(constraint)
            && self
                .resolve_type_in_current_package(constraint)
                .is_some_and(|resolved| resolved != constraint)
        {
            return false;
        }
        self.nominal_type_object_name_for_constraint(constraint) == constraint
    }

    /// Whether an env binding now makes `constraint` resolve to something
    /// other than itself: a lexical type in scope under the spelling (what
    /// `try_resolved_type_capture_name` substitutes), or a `Package` bound
    /// under it naming another type (what the Nil seed's
    /// `nominal_type_object_name_for_constraint` follows).
    // Cost: one env probe, two for an unqualified name.
    fn decl_type_name_rebound_in_env(&self, constraint: &str) -> bool {
        if !crate::qualified::is_qualified_str(constraint)
            && self.bound_lexical_type_storage(constraint).is_some()
        {
            return true;
        }
        let Some(sym) = Symbol::lookup(constraint) else {
            return false;
        };
        crate::symbol::maybe_env_key(sym)
            && matches!(
                self.env().get_sym(sym).map(Value::view),
                Some(ValueView::Package(bound)) if bound != sym
            )
    }
}
