//! Named attribute initialization for the generic constructor.

use super::*;

impl Interpreter {
    // Cost: O(a * n), a = class attributes, n = provided arguments, plus coercions.
    pub(super) fn constructor_provided_attrs(
        &mut self,
        cn_resolved: &str,
        args: &[Value],
        plan: &NativeCtorPlan,
        arg_attr_idx: &[Option<u32>],
    ) -> Vec<Option<Value>> {
        plan
            .class_attrs
            .iter()
            .enumerate()
            .map(|(i, attr)| {
                args.iter()
                    .zip(arg_attr_idx.iter())
                    .filter_map(|(arg, attr_idx)| {
                        if *attr_idx != Some(i as u32) {
                            return None;
                        }
                        let ValueView::Pair(key, value) = arg.view() else {
                            return None;
                        };
                        if plan.attr_binds[i] {
                            return Some(value.clone());
                        }
                        let value = value.deref_container();
                        let coerced = if let Some(type_name) = plan.attr_is_types.get(key.as_str())
                        {
                            self.coerce_value_to_is_type(type_name, attr.sigil, value.clone())
                                .ok()?
                        } else {
                            Self::coerce_provided_attr_value_by_sigil(value.clone(), attr.sigil)
                        };
                        // A coercion-typed attribute (`has Str() $.x`) coerces a
                        // provided value through its target type, as `.new` does.
                        let coerced = match plan.attr_constraints[i].as_ref() {
                            Some(tc)
                                if attr.sigil == '$'
                                    && !coerced.is_nil()
                                    && crate::runtime::types::is_coercion_constraint(tc) =>
                            {
                                self.coerce_value_for_constraint(tc, coerced)
                            }
                            _ => coerced,
                        };
                        // Raku: assigning `Nil` to a scalar container resets it
                        // to the container's declared type default, exactly
                        // like `has $.x = Nil` and plain `$x = Nil` do — not
                        // the provided `Nil` itself.
                        let coerced = if attr.sigil == '$' && coerced.is_nil() {
                            self.attr_store_nil_default(cn_resolved, &attr.name, '$', coerced)
                        } else {
                            coerced
                        };
                        Some(coerced)
                    })
                    .last()
            })
            .collect()
    }
}

impl Interpreter {
    // Cost: O(a), a = class attributes, plus coercions and checks.
    pub(super) fn store_ctor_named_value(
        &mut self,
        class_key: &str,
        plan: &NativeCtorPlan,
        attrs: &mut AttrMap,
        attribute: (&str, char),
        value: Value,
    ) -> Result<(), RuntimeError> {
        let (key, sigil) = attribute;
        let class_attrs_info = &*plan.class_attrs;
        let attr_type_constraints = &*plan.type_constraints;
        if let Some(attr) = class_attrs_info.iter().find(|attr| attr.name == key && attr.sigil == sigil)
            && self.attribute_binds_in_mro(class_key, attr)
        {
            self.check_attr_store_type(class_key, key, sigil, value.deref_container())?;
            let storage = super::attribute_storage_key(class_attrs_info, key, sigil);
            Self::bind_constructor_attribute(attrs, storage, key, value);
            return Ok(());
        }
        let mut value = value.deref_container();
        // A `Proxy` is FETCHed as it lands in the attribute's
        // Scalar (ADR-0040 §9).
        if sigil == '$' && value.is_proxy_value() {
            value = self.auto_fetch_proxy(&value)?;
        }
        let attr_type_constraint = class_attrs_info
            .iter()
            .find(|attr| attr.name == *key && attr.sigil == sigil)
            .and_then(|attr| {
                super::attribute_type_constraint(
                    &class_attrs_info,
                    attr,
                    &attr_type_constraints,
                )
            });
        // A coercion-typed attribute (`has Int() $.x`)
        // coerces its provided value through the target
        // type (built-in coercion or a user COERCE method).
        if sigil == '$'
            && let Some(tc) = attr_type_constraint.as_ref()
            && crate::runtime::types::is_coercion_constraint(tc)
        {
            value = self.coerce_value_for_constraint(tc, value);
        }
        // An `is Type` container attribute (`has @.a is
        // Buf`) is owned by its DECLARED container type,
        // which coerces the provided value itself (Buf,
        // BagHash, Array[T], ...). It must see the value
        // as supplied: running the generic sigil
        // coercion first list-assigns an already-correct
        // `Buf` into a one-element array (`my @a = $buf`
        // is `[Buf]` in raku too), and the declared type
        // was then rebuilt from that wrapper.
        let is_type = matches!(sigil, '@' | '%')
            .then(|| self.attribute_is_type_in_mro(class_key, key))
            .flatten();
        let coerced = if let Some(type_name) = is_type {
            self.coerce_value_to_is_type(&type_name, sigil, value)?
        } else {
            let coerced =
                Self::coerce_provided_attr_value_by_sigil(value, sigil);
            // A `%`-sigil attribute bound to a non-Hash
            // object list-contextualizes it (Raku's
            // Hash.STORE): an object with a custom
            // `.iterator`/`.list` (e.g. `handles`)
            // contributes its pairs.
            if sigil == '%'
                && matches!(coerced.view(), ValueView::Instance { .. })
            {
                self.coerce_object_to_hash(coerced)
            } else {
                coerced
            }
        };
        let storage_key =
            super::attribute_storage_key(&class_attrs_info, key, sigil);
        attrs.insert(
            storage_key,
            Self::itemize_attr_store_value(sigil, coerced),
        );
        Ok(())
    }
}
