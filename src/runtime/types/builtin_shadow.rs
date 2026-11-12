//! A package-local type that shadows a built-in class of the same name.

use crate::runtime::Interpreter;
use crate::symbol::Symbol;

impl Interpreter {
    /// The package-local declaration a bare built-in type name denotes inside
    /// a package: `class Compiler` inside `unit module Red::Migration::Data`
    /// makes `Compiler` there `Red::Migration::Data::Compiler`, as in rakudo,
    /// where a package-local type shadows the core name. The runtime registers
    /// some core types (`Compiler`, `VM`, `Kernel`, `Lock`, ...) as classes,
    /// so the general `has_type` probe would answer the core class first.
    ///
    /// `None` at GLOBAL scope, for a name that is not a built-in type, and
    /// when no enclosing package declares it.
    // Cost: O(d), d = depth of the current package (one interned
    // `<pkg>::<name>` probe per enclosing package); O(1) at GLOBAL scope or
    // for a name that is not a built-in type.
    pub(crate) fn package_local_shadow_of_builtin(&self, name: &str) -> Option<Symbol> {
        let pkg = self.current_package_sym();
        if crate::qualified::is_global_package(pkg)
            || crate::qualified::is_qualified_str(name)
            || !(Self::is_builtin_type(name) || crate::known_types::is_known_type_constraint(name))
        {
            return None;
        }
        let name_sym = Symbol::intern(name);
        crate::qualified::package_ancestors(pkg)
            .filter(|p| !crate::qualified::is_global_package(*p))
            .map(|p| crate::qualified::qualified(p, name_sym))
            // Only a type declared by its short name inside the package is
            // its lexical: `class Foo::List` (or a loaded `unit class
            // Pkg::List`) leaves a bare `List` the core one (#12907).
            .find(|q| self.has_type_direct(q.as_str()) && !self.is_compound_declared_name(q.as_str()))
    }
}
