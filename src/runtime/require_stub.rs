//! The lexical placeholder a statically named `require` declares (see
//! `compiler/require_stubs.rs`).

use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// Bind `name`, the literal target of a `require` in the scope being
    /// entered, to a stub package unless it already resolves.
    ///
    /// The binding is lexical, exactly like a `my package`'s: it joins the
    /// scope's declared names, so the scope's exit takes it away again (see
    /// [`Self::register_lexical_class`]). It is not recorded as a package
    /// member or a registered type, so a later load of the real module
    /// registers over it without a clash, and a failed load leaves it as the
    /// only trace of the name.
    pub(crate) fn declare_require_stub(&mut self, name: &str) {
        if !Self::is_unresolved_symbol(&self.resolve_indirect_type_name(name)) {
            return;
        }
        self.env
            .insert(name.to_string(), Value::package(Symbol::intern(name)));
        self.register_lexical_class(name.to_string());
    }

    /// Whether `value` is the `Failure` `::('Name')` yields for a name that
    /// resolves to nothing.
    fn is_unresolved_symbol(value: &Value) -> bool {
        matches!(
            value.view(),
            ValueView::Instance { class_name, .. } if class_name == "Failure"
        )
    }
}
