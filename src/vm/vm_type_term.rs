//! A type spelling resolved to the type object its bare term evaluates to.

use super::*;

impl Interpreter {
    /// The type object an imported term binds the short spelling `name` to
    /// in the current scope, for an answer that has to be *that* object
    /// rather than a `Package` built from the spelling: `&f.returns` for
    /// `sub f(--> size_t)` under `use NativeCall` is NativeCall's `size_t`
    /// (whose `.REPR` is `P6int`), not the same-spelled core alias. The bare
    /// `size_t` term resolves the same way (`push_bare_word_value`'s
    /// `term_value` branch outranks the type branch). `None` when no term of
    /// that spelling holds a type object of that name.
    // Cost: O(1) expected: one env probe.
    pub(crate) fn imported_type_term(&self, name: &str) -> Option<Value> {
        let value = self.term_value(name)?;
        let names_it = match value.view() {
            ValueView::Package(p) => {
                let storage = p.resolve();
                let facing = crate::value::user_facing_type_name(&storage);
                crate::qualified::unqualified_part(Symbol::intern(&facing)).as_str() == name
            }
            ValueView::CustomType(c) => crate::qualified::unqualified_part(c.name).as_str() == name,
            _ => false,
        };
        names_it.then(|| value.clone())
    }
}
