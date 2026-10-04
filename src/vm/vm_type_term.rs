//! A type spelling resolved to the type object its bare term evaluates to.

use super::*;

impl Interpreter {
    /// The type object a term binds the short spelling `name` to in the
    /// current scope, for an answer that has to be *that* object rather than
    /// a `Package` built from the spelling: `&f.returns` for
    /// `sub f(--> size_t)` under `use NativeCall` is NativeCall's `size_t`
    /// (whose `.REPR` is `P6int`), not the same-spelled core alias, and for
    /// `--> PwAlias` with `my constant PwAlias = PwLinux` it is `PwLinux`. The
    /// bare term resolves the same way (`push_bare_word_value`'s `term_value`
    /// branch outranks the type branch). `None` when no term of that spelling
    /// holds a type object.
    // Cost: O(1) expected: one env probe.
    pub(crate) fn imported_type_term(&self, name: &str) -> Option<Value> {
        let value = self.term_value(name)?;
        matches!(
            value.view(),
            ValueView::Package(_) | ValueView::CustomType(_)
        )
        .then(|| value.clone())
    }

    /// The type object `name` denotes in the current scope when that differs
    /// from the global spelling: a `my class` bound to its lexical storage
    /// name, or a type term ([`Self::imported_type_term`]). `None` for a
    /// spelling that means the same everywhere.
    // Cost: O(1) expected: two env probes.
    pub(crate) fn scoped_type_object(&self, name: &str) -> Option<Value> {
        if let Some(ValueView::Package(p)) = self.env().get(name).map(Value::view)
            && p.as_str().contains('\u{0}')
        {
            return Some(Value::package(p));
        }
        self.imported_type_term(name)
    }
}
