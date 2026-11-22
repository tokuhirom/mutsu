//! Buildable attributes (public, or private `is built`) in the default
//! instance `.raku` and `.gist` rendering.

use super::*;
use crate::meta_ns::MetaNs;
use crate::value::{AttrMap, MixinOverrides};

impl Interpreter {
    /// Render public class and role attributes from their respective live
    /// cells. A role mixed into an instance does not put its attributes in the
    /// wrapped class's attribute map.
    // Cost: O(a log a + r), a = public attributes, r = composed roles.
    pub(crate) fn collect_public_raku_attrs(
        &mut self,
        class_name: &str,
        attributes: &AttrMap,
        mixin: Option<&MixinOverrides>,
    ) -> Vec<String> {
        let class_attrs = self.collect_class_attributes_display_order(class_name);
        // A punned role's temporary class has been withdrawn by the time its
        // instance renders, but its attribute values remain on that instance.
        let class_attrs =
            if class_attrs.is_empty() && self.registry().roles.contains_key(class_name) {
                self.collect_role_attributes_for_class(class_name)
            } else {
                class_attrs
            };
        let mut entries: Vec<(ClassAttributeDef, Option<String>)> = Vec::new();
        if let Some(mixins) = mixin {
            let mut seen = std::collections::HashSet::new();
            let mut roles: Vec<(i64, String)> = mixins
                .keys()
                .filter_map(|key| key.strip_prefix("__mutsu_role__"))
                .map(|role| {
                    let seq = mixins
                        .get(MetaNs::RoleSeq.str_key_for_str(role))
                        .and_then(|value| match value.view() {
                            ValueView::Int(seq) => Some(seq),
                            _ => None,
                        })
                        .unwrap_or(i64::MIN);
                    (seq, role.to_string())
                })
                .collect();
            roles.sort_by(|a, b| b.0.cmp(&a.0).then_with(|| a.1.cmp(&b.1)));
            for (_, role) in roles {
                for attr in self.collect_role_attributes_for_class(&role) {
                    if seen.insert((attr.name.clone(), attr.sigil)) {
                        entries.push((attr, Some(role.clone())));
                    }
                }
            }
        }
        // Rakudo renders the most recently mixed role first, then earlier
        // roles, then the class; each owner keeps its declaration order.
        // A punned role also appears as the instance's class. Its attributes
        // share the same cells and must be rendered only once.
        let punned_role = mixin.is_some() && self.registry().roles.contains_key(class_name);
        if !punned_role {
            entries.extend(class_attrs.into_iter().map(|attr| (attr, None)));
        }
        let mut parts = Vec::new();
        for (attr, role) in entries {
            // Rakudo's `Mu.raku` lists the attributes `.new` builds: the public
            // ones and a private one declared `is built`.
            let owner = role.as_deref().unwrap_or(class_name);
            if !self.is_attribute_buildable(owner, &attr.name) {
                continue;
            }
            // Reading a class attribute vivifies it, matching
            // `Attribute.get_value` in Rakudo (#11003).
            let value = match (&role, mixin) {
                (Some(role), Some(mixins)) => mixins.role_attribute(role, &attr.name),
                _ => attributes.get_vivify(attr.name.as_str()).cloned(),
            };
            let Some(value) = value else { continue };
            let value = value.deref_container();
            let rendered = self
                .dispatch_raku_leaf(&value)
                .map(|v| v.to_string_value())
                .unwrap_or_else(|_| value.to_string_value());
            let rendered = if attr.sigil == '$' {
                crate::builtins::methods_0arg::raku_repr::itemize_scalar_repr(&value, rendered)
            } else {
                rendered
            };
            parts.push(format!("{} => {}", attr.name, rendered));
        }
        parts
    }
}
