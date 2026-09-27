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
        if let Some(ValueView::Package(sym)) = self.env.get(name).map(Value::view) {
            let key = sym.resolve();
            if self.registry().enum_types.contains_key(key.as_str()) {
                return Some(key.to_string());
            }
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
}
