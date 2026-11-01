//! Container binding shared by native construction and BUILDPLAN execution.

use super::*;

impl Interpreter {
    /// A bound Proxy is the attribute's container; ordinary construction
    /// FETCHes it at the assignment boundary.
    // Cost: O(a), a = constructor arguments, plus any Proxy FETCH calls.
    pub(super) fn fetch_bound_ctor_proxy_args(
        &mut self,
        args: &[Value],
        plan: &NativeCtorPlan,
    ) -> Result<Vec<Value>, RuntimeError> {
        args.iter().map(|arg| {
            let binds = match arg.view() {
                ValueView::Pair(key, _) => plan.attr_index.get(key.as_str())
                    .is_some_and(|&index| plan.attr_binds[index as usize]),
                _ => false,
            };
            if binds {
                Ok(arg.clone())
            } else {
                self.fetch_proxy_in_store_arg(arg.clone())
            }
        }).collect()
    }

    // Cost: O(m * a), m = MRO classes, a = attributes per class; computed once in NativeCtorPlan.
    pub(crate) fn attribute_binds_in_mro(&self, class: &str, attr: &ClassAttributeDef) -> bool {
        if attr.sigil != '$'
            || attr.type_constraint.as_deref().is_some_and(|ty| Self::native_scalar_default(ty).is_some())
        {
            return false;
        }
        let registry = self.registry();
        std::iter::once(class)
            .chain(self.mro_readonly(class).iter().map(|owner| owner.as_str()))
            .find_map(|owner| {
                let definition = registry.classes.get(owner)?;
                definition.attributes.iter()
                    .any(|candidate| candidate.name == attr.name && candidate.sigil == attr.sigil)
                    .then(|| definition.attribute_built.get(&attr.name).copied()
                        == Some(crate::ast::BuiltTrait::Bind))
            })
            .unwrap_or(false)
    }

    /// Seat the same container, or mark a bare bound value immutable. This is
    /// the construction counterpart of an explicit `$!attribute := value`.
    // Cost: O(1).
    pub(crate) fn bind_constructor_attribute(
        attrs: &mut AttrMap,
        key: Symbol,
        bare: &str,
        value: Value,
    ) {
        let marker = crate::meta_ns::MetaNs::BoundAttr.key_for_str(bare);
        let value = if value.is_container_ref() || value.is_proxy_value() {
            attrs.remove(marker);
            value
        } else {
            attrs.insert(marker, Value::TRUE);
            let kind = Self::rebound_value_kind(&value).unwrap_or(crate::ast::ReadonlyKind::Immutable);
            Value::container_ref(crate::gc::Gc::new(
                crate::value::ContainerCell::new_readonly_binding(
                    value,
                    kind,
                ),
            ))
        };
        attrs.insert(key, value);
    }

    // Cost: the default's evaluation plus its attribute type check.
    pub(super) fn bound_constructor_default(
        &mut self,
        class: Symbol,
        attr: &ClassAttributeDef,
        instance: &Value,
        attrs: &AttrMap,
    ) -> Result<Value, RuntimeError> {
        let owner = attr.declaring_package.unwrap_or(class);
        let Some(default) = attr.default.as_ref() else {
            return Ok(Value::package(crate::symbol::wk::any()));
        };
        let value = self.eval_attr_default_expr(
            owner.as_str(), class, default, instance, attrs,
            super::attr_build_defaults::AttrDeclScope::of(attr),
        )?;
        self.check_attr_store_type(class.as_str(), &attr.name, '$', value.deref_container())?;
        Ok(value)
    }
}
