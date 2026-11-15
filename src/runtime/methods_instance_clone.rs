use super::*;
use crate::value::ValueView;

impl Interpreter {
    /// The native attribute-copying clone of an `Instance` value, with `:attr(v)`
    /// twiddle args applied. Shared between the direct `.clone` dispatch arm and
    /// the `callsame` base candidate of a user `clone` override
    /// (`native_mu_base`). `None` when the value is not an
    /// Instance.
    // Cost: O(a) in the instance's attribute count.
    pub(crate) fn native_instance_clone_value(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Instance {
            class_name,
            attributes,
            ..
        } = target.view()
        else {
            return None;
        };
        // Build sigil map from class attributes to coerce values properly
        let class_attrs_info = self.collect_class_attributes(&class_name.resolve());
        let initialized: std::collections::HashSet<&str> = class_attrs_info
            .iter()
            .filter(|attr| attr.default.is_some() && matches!(attr.sigil, '@' | '%'))
            .map(|attr| attr.name.as_str())
            .collect();
        let mut attrs = attributes.to_clone_map(&|name| initialized.contains(name));
        let sigil_map: HashMap<String, char> = class_attrs_info
            .iter()
            .map(|attr| (attr.name.clone(), attr.sigil))
            .collect();
        let public_attrs: std::collections::HashSet<&str> = class_attrs_info
            .iter()
            .filter(|attr| attr.is_public)
            .map(|attr| attr.name.as_str())
            .collect();
        let cn = class_name.resolve();
        for arg in args {
            if let ValueView::Pair(key, boxed) = arg.view()
                && (self.is_attribute_buildable(&cn, key) || public_attrs.contains(key.as_str()))
            {
                let sigil = sigil_map.get(key.as_str()).copied().unwrap_or('$');
                // A twiddle is an assignment to the attribute: a typed one
                // refuses a value of the wrong type (`X::TypeCheck::Assignment`).
                //
                // rakudo checks against the descriptor of the container already
                // in the slot. A slot holding a value that does NOT satisfy the
                // declared type got it through `Attribute.set_value`, which
                // binds without checking (a MOP-built attribute, RedFactory's
                // `Type.^add_attribute: Attribute.new(:type(Str), ...)` holding
                // a Block), so its container carries no such constraint and the
                // twiddle is not checked either.
                let incoming = boxed.deref_container();
                let slot_is_unchecked = sigil == '$'
                    && attrs.get(key.as_str()).is_some_and(|existing| {
                        !existing.is_nil()
                            && self
                                .check_attr_store_type(&cn, key, sigil, existing.deref_container())
                                .is_err()
                    });
                let checked = if slot_is_unchecked {
                    incoming
                } else {
                    match self.check_attr_store_type(&cn, key, sigil, incoming) {
                        Ok(value) => value,
                        Err(err) => return Some(Err(err)),
                    }
                };
                // `clone` binds an `@` attribute to an itemized Array as it is
                // (`.clone(tasks => $arr)`), where `new` would wrap the item
                // as one element.
                let checked = if sigil == '@' {
                    checked.deitemize_for_sigil_bind()
                } else {
                    checked
                };
                let coerced = Self::coerce_attr_value_by_sigil(checked, sigil);
                attrs.insert(key.clone(), coerced);
            }
        }
        Some(Ok(Value::make_instance(class_name, attrs)))
    }

}

