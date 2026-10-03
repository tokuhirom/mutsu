//! `Metamodel::PrivateMethodContainer`: `.^private_methods` and
//! `.^private_method_table`, both built from one ordered walk of the class's
//! own private-method rows.

use super::*;
use crate::value::ValueMap;

impl Interpreter {
    /// The private methods (and private submethods) declared directly on
    /// `class_name`, in declaration order, as `(name, Method object)` pairs.
    /// A public attribute has no entry: only `!`-twigil declarations
    /// (`method !foo`) and their `submethod` equivalent are private methods
    /// (#8836). Inherited private methods are not included, matching Rakudo,
    /// where each class's private-method table is its own.
    // Cost: O(m), m = methods declared directly on the class.
    fn class_private_method_entries(&self, class_name: &str) -> Vec<(String, Value)> {
        let registry = self.registry();
        if !registry.classes.contains_key(class_name) {
            return Vec::new();
        }
        let mut entries = Vec::new();
        for method_name in registry.owner_method_names(class_name) {
            let method_name = method_name.resolve();
            let Some(overloads) = registry.user_method_overloads(class_name, &method_name) else {
                continue;
            };
            let Some(first) = overloads.first() else {
                continue;
            };
            if !first.is_private {
                continue;
            }
            let method = self.make_method_object_with_owner(
                &method_name,
                first,
                overloads.len() > 1,
                first.return_type.clone(),
                Some(&overloads),
                Some(class_name),
            );
            entries.push((method_name.to_string(), method));
        }
        entries
    }

    /// Build the class's own private method table (`.^private_method_table`),
    /// keyed by name — the counterpart `class_method_table` deliberately
    /// excludes (#8836).
    // Cost: O(m), m = methods declared directly on the class.
    pub(super) fn class_private_method_table(&self, class_name: &str) -> ValueMap {
        let mut table = ValueMap::default();
        for (name, method) in self.class_private_method_entries(class_name) {
            table.insert(name, self.mark_method_table_entry(method));
        }
        table
    }

    /// `.^private_methods`: the class's own private methods as a list of
    /// `Method` objects, in declaration order (Manifest::StopWar walks it and
    /// calls each one back with `self!"$name"()`).
    // Cost: O(m), m = methods declared directly on the class.
    pub(super) fn class_private_methods(&self, class_name: &str) -> Value {
        Value::array(
            self.class_private_method_entries(class_name)
                .into_iter()
                .map(|(_, method)| method)
                .collect(),
        )
    }
}
