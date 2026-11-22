//! A loaded module's routine names its own package's type before the
//! importing program's type of the same short name (#18677).

use super::*;

impl Interpreter {
    /// The file-scope lexical type `name` (unqualified) of the `unit` module
    /// whose package is running (`unit module M; my class R`), recorded under
    /// its type-only key in the unit's bucket when the module loaded. Such a
    /// declaration is lexically visible to the module's routines, so it
    /// shadows a type of the same spelling the program that loaded the module
    /// binds in the env the routine runs against.
    // Cost: O(1), one bucket probe.
    pub(super) fn module_own_type_shadowing_global(&self, name: &str) -> Option<Symbol> {
        if crate::qualified::is_qualified_str(name)
            || crate::qualified::is_global_package(self.current_package_sym())
        {
            return None;
        }
        let key = crate::runtime::term_names::lexical_type_key(name);
        let lexicals = self.lexicals.unit_lexicals.read();
        let value = lexicals
            .get(self.current_package_sym().as_str())?
            .get(&key)?;
        match value.view() {
            ValueView::Package(storage) => Some(storage),
            _ => None,
        }
    }
}
