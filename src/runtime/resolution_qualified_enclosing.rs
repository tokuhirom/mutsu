//! Resolving a package-qualified routine name (`Q::k`) relative to the
//! packages that enclose the running code.

use super::*;
use crate::qualified::{package_ancestors, qualified};
use crate::symbol::Symbol;

impl Interpreter {
    /// The fully qualified name a qualified call `name` (`Q::k`) means when
    /// `Q` is a package declared inside a package enclosing the running code:
    /// inside a method of `class M::C`, `Q::k` is `M::C::Q::k` or else
    /// `M::Q::k`. The innermost match wins. A package is considered only when
    /// `Q` is declared under it, so an unrelated `Q` elsewhere never matches.
    ///
    /// Rakudo finds `Q` through the lexical scope of the code, which for a
    /// method body is the class block and then the module around it. mutsu
    /// tried only the name as written, so a method of a class or role
    /// declared in `unit module M` could not call `Q::k` from `M`'s own `our
    /// package Q` (Bitcoin's `P2PKH::address self` from `role
    /// Bitcoin::PrivateKey`). The enclosing packages are those of the
    /// current package and of the running method's class.
    // Cost: O(d), d = nesting depth of the current package and method class.
    pub(crate) fn enclosing_qualified_routine_name(&mut self, name: &str) -> Option<String> {
        let (pkg_prefix, _) = name.rsplit_once("::")?;
        let name_sym = Symbol::intern(name);
        let prefix_sym = Symbol::intern(pkg_prefix);
        let mut bases: Vec<Symbol> = package_ancestors(self.current_package_sym()).collect();
        if let Some(class) = self.method_class_stack_top_str() {
            bases.extend(package_ancestors(Symbol::intern(class)));
        }
        for anc in bases {
            if anc.as_str() == "GLOBAL" {
                continue;
            }
            if !self.is_known_package(qualified(anc, prefix_sym).as_str()) {
                continue;
            }
            let candidate = qualified(anc, name_sym);
            if self.fn_base_name_registered_sym(candidate.as_str(), candidate) {
                return Some(candidate.as_str().to_string());
            }
        }
        None
    }
}
