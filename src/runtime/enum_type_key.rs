//! Resolving an enum type's source spelling to its registry key.
//!
//! A package-scoped enum is registered under its package-qualified identity
//! (`module A { enum E <x> }` is `A::E`, see
//! [`Interpreter::enum_registry_key`]), so a lookup by the spelling the source
//! wrote -- a type constraint `E`, a coercion call `E(1)` -- has to resolve
//! that spelling first, the way a bareword term does (#9654).

use super::*;

impl Interpreter {
    /// The `enum_types` key the enum spelled `name` resolves to here: the name
    /// itself when an enum is registered under it, else the type its lexical
    /// or imported binding names, else the one the current package chain
    /// holds. `None` when `name` names no enum.
    // Cost: O(d) hash probes, d = depth of the current package chain.
    pub(crate) fn resolve_enum_type_key(&self, name: &str) -> Option<String> {
        if self.registry().enum_types.contains_key(name) {
            return Some(name.to_string());
        }
        // `Symbol::lookup`, not `env.get`: this runs on every coercion to a
        // non-enum type (`Bool(Mu)` on each `Test` assertion), and a name that
        // was never interned cannot be an env key, so a miss must not intern.
        if let Some(ValueView::Package(sym)) = Symbol::lookup(name)
            .and_then(|key| self.env.get_sym(key))
            .map(Value::view)
        {
            let key = sym.resolve();
            if self.registry().enum_types.contains_key(key.as_str()) {
                return Some(key.to_string());
            }
        }
        // A module routine naming an enum by the short alias its module's own
        // `use` installed (`use A::Level; ... Level($n)`), after the load
        // restored the importer's env. `package_type_alias` answers `None` in
        // one probe for a name no package aliases.
        if let Some(target) = self.package_type_alias(name)
            && self.registry().enum_types.contains_key(target.as_str())
        {
            return Some(target);
        }
        // Only a package-scoped enum lives under a qualified key, so the
        // package-chain probe (which interns) runs only for a name one was
        // declared under -- this is reached on every unresolved call.
        if !crate::value::is_package_enum_declared_name(name) {
            return None;
        }
        self.resolve_type_in_current_package(name)
            .filter(|key| self.registry().enum_types.contains_key(key.as_str()))
    }

    /// The MRO of the base type the enum `name` inherits from, as Rakudo
    /// builds it (`enum E <a b>` is `(E Int Cool Any Mu)`): the catalog chain
    /// of its first value's type. `None` when `name` names no enum, or its base
    /// type has no catalog entry.
    // Cost: O(d) to resolve the key (see `resolve_enum_type_key`), plus one
    // catalog lookup.
    pub(crate) fn enum_base_type_mro(&self, name: &str) -> Option<&'static [&'static str]> {
        let key = self.resolve_enum_type_key(name)?;
        let base: String = match &self.registry().enum_types.get(key.as_str())?.first()?.1 {
            EnumValue::Int(_) => "Int".to_string(),
            EnumValue::Str(_) => "Str".to_string(),
            EnumValue::Generic(value) => crate::value::what_type_name(value),
        };
        Some(crate::builtin_types::catalog::builtin_type_info(&base)?.mro)
    }
}
