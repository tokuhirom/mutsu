//! Attribute introspection for native RakuAST model classes and their parents.

use super::*;

impl Interpreter {
    // Cost: O(d + a), d = model MRO length, a = modeled attributes.
    pub(crate) fn collect_rakuast_attribute_objects(
        class_name: &str,
        local_only: bool,
    ) -> Option<Vec<Value>> {
        if local_only {
            return crate::rakuast::local_attribute_names(class_name).map(|names| {
                names
                    .iter()
                    .map(|name| Self::make_builtin_attribute_object(name, "Mu", class_name))
                    .collect()
            });
        }
        let mro = crate::rakuast::type_object_mro(class_name)?;
        let mut attributes = Vec::new();
        for owner in mro {
            if let Some(names) = crate::rakuast::local_attribute_names(&owner) {
                attributes.extend(
                    names
                        .iter()
                        .map(|name| Self::make_builtin_attribute_object(name, "Mu", &owner)),
                );
            }
        }
        Some(attributes)
    }
}
