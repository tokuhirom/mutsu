//! A metamethod of an `Attribute` metaobject (`$attr.get_value($obj)`,
//! `.set_value`, `.name`, ...) answered from `Attribute`'s rows directly.
//!
//! An `Attribute` instance has no dispatch shape in the method table, so the
//! guard step declines it, and a call fell into the interpreter's whole method
//! cascade (`call_method_with_values`: the by-name dispatch groups, the
//! metamodel probes, the carried-state owners, the MRO walk) before reaching
//! the same `invoke_owner(["Attribute"], ...)` answer, about 22k Ir of a
//! 42k Ir `get_value`. AttrX::Mooish calls `get_value` several times per
//! attribute access, on an attribute that `does` its role. When neither the
//! attribute's class nor a mixed-in role declares the method and no user
//! `^find_method` exists, the row is the answer and is reached directly.

use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

impl Interpreter {
    /// `method(args)` on an `Attribute` instance (or one mixed with roles)
    /// answered by its `Attribute` row; `None` when the receiver is anything
    /// else, a user method could claim the call, or no row answers it.
    // Cost: O(m + r), m = the receiver class's MRO (cached), r = mixed-in roles
    // (each a memoized method probe), plus the row's own cost.
    pub(crate) fn try_attribute_meta_row(
        &mut self,
        target: &Value,
        method_sym: Symbol,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let attribute = Symbol::intern("Attribute");
        let (instance, mixins) = match target.view() {
            // `Attribute` itself, a user subclass, or the anonymous class an
            // `$attr does R` composes (`Attribute+{R}`): a metaobject whose
            // facts the rows read. A method the class declares itself wins.
            ValueView::Instance { class_name, .. }
                if class_name == attribute
                    || (self.class_mro(class_name.as_str()).contains(&attribute)
                        && !self.has_user_method_sym(class_name.as_str(), method_sym)) =>
            {
                (target.clone(), None)
            }
            ValueView::Mixin(inner, mixins) => match inner.view() {
                ValueView::Instance { class_name, .. } if class_name == attribute => {
                    (Value::clone(inner), Some(mixins))
                }
                _ => return None,
            },
            _ => return None,
        };
        let method = method_sym.as_str();
        if !crate::builtins::method_table::owner_declares("Attribute", method_sym)
            || crate::runtime::find_method_intercept::any_user_find_method()
            || self.has_user_method("Attribute", method)
        {
            return None;
        }
        if let Some(mixins) = mixins
            && Self::mixin_role_applications(mixins)
                .iter()
                .any(|(role, _)| self.role_or_parent_has_method(role, method))
        {
            return None;
        }
        crate::builtins::method_table::invoke_owner(self, &["Attribute"], method, args, || {
            instance
        })
    }
}
