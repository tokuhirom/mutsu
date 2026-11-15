use super::*;
use crate::meta_ns::MetaNs;
use crate::symbol::Symbol;

impl Interpreter {
    // Cost: O(a + r + c), a = attributes, r = mixed-in roles, c = inner clone cost.
    pub(crate) fn try_clone_role_mixin(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let ValueView::Mixin(inner, mixins) = target.view() else {
            return None;
        };
        // `.clone` on a role mixin. A punned role's `__mutsu_attr__*` entries are
        // construction seeds for the Mixin-owned role cell, not the live state.
        // The generic instance clone unwraps to the inner value and drops the
        // role composition, so the clone lost all attributes (`Zef::Client.fetch`'s
        // `$candi.clone(:$dist)` then had no `.as`). Clone the mixin instead:
        // recursively clone the inner value, deep-copy the role cell, copy every
        // marker, and apply each `:attr(val)` override to the new role cell.
        // Restricted to role mixins (a `__mutsu_role__`/`__mutsu_attr__` marker is
        // present) so allomorph / non-role `but` mixins keep their existing path.
        if !Self::mixin_role_applications(mixins)
                .iter()
                .any(|(role, _)| self.role_or_parent_has_method(role, "clone"))
            && mixins
                .keys()
                .any(|k| k.starts_with("__mutsu_role__") || k.starts_with("__mutsu_attr__"))
        {
            let inner_owned = inner.as_ref().clone();
            let mut new_mixins = mixins.as_ref().clone();
            // An override names either a role attribute (a `__mutsu_attr__`
            // marker) or an attribute of the inner class. Apply the former to the
            // marker; forward the rest to the inner clone so `$w.clone(:id(99))`
            // on a `Widget but Named` still overrides Widget's own `$.id`.
            // An override that lands on a role marker must reach the deep-copied
            // role cell too — otherwise the accessor would keep serving the
            // pre-clone value from that cell.
            let inner_attrs = Self::self_instance_attrs(inner);
            let mut inner_args: Vec<Value> = Vec::new();
            for arg in args {
                if let ValueView::Pair(key, val) = arg.view()
                    && let std::collections::hash_map::Entry::Occupied(mut e) =
                        new_mixins.entry(MetaNs::Attr.owned_key_for_str(key))
                {
                    e.insert(val.clone());
                    let attr = Symbol::intern(key);
                    if new_mixins.set_role_attribute_by_name(key, val.clone()) {
                        continue;
                    }
                    let in_cell = inner_attrs
                        .as_ref()
                        .is_some_and(|c| c.as_map().contains_key(attr));
                    if !in_cell {
                        continue;
                    }
                }
                inner_args.push(arg.clone());
            }
            let inner_clone =
                match self.call_method_with_values_unviewed(inner_owned, "clone", inner_args) {
                    Ok(v) => v,
                    Err(e) => return Some(Err(e)),
                };
            let cloned = Value::mixin_with_state(inner_clone, new_mixins);
            // A cloned routine's own cell holds the copied composition
            // (ADR-11827 §2.3).
            return Some(Ok(Self::note_routine_composition(cloned, None)));
        }
        None
    }
}

