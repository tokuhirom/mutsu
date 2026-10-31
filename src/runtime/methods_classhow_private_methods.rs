//! `Metamodel::PrivateMethodContainer`: `.^private_methods` and
//! `.^private_method_table`, both built from one ordered walk of the class's
//! own private-method rows.

use super::*;
use crate::symbol::Symbol;
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
            // A public method may share the name (`method !v`, `method v`).
            let overloads: Vec<_> = overloads.iter().filter(|def| def.is_private).cloned().collect();
            let Some(first) = overloads.first() else {
                continue;
            };
            let method = self.make_method_object_with_owner(
                &method_name,
                first,
                overloads.len() > 1,
                first.return_type.clone(),
                Some(&overloads),
                Some(class_name),
            );
            let method = stable_method_object(method, class_name, &method_name);
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

    /// The `Method` object of a private method a `role` declares, which the
    /// role's metaobject has no table for (`RakuAST::Method.compile-time-value`
    /// of one in a unit tree).
    // Cost: O(m), m = overloads of the method name in the role.
    pub(crate) fn role_private_method_object(&self, role_name: &str, name: &str) -> Option<Value> {
        let registry = self.registry();
        let overloads: Vec<_> = registry
            .roles
            .get(role_name)?
            .methods
            .get(name)?
            .iter()
            .filter(|def| def.is_private)
            .cloned()
            .collect();
        let first = overloads.first()?;
        let method = self.make_method_object_with_owner(
            name,
            first,
            overloads.len() > 1,
            first.return_type.clone(),
            Some(&overloads),
            Some(role_name),
        );
        Some(stable_method_object(method, role_name, name))
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

    /// `.^find_private_method($name)`: the class's own private `Method`, or
    /// `Mu` when it declares none by that name (inherited ones are not
    /// searched, as in Rakudo).
    // Cost: O(m), m = methods declared directly on the class.
    pub(super) fn class_find_private_method(&self, class_name: &str, name: &str) -> Value {
        self.class_private_method_entries(class_name)
            .into_iter()
            .find(|(n, _)| n == name)
            .map(|(_, method)| method)
            .unwrap_or_else(|| Value::package(Symbol::intern("Mu")))
    }

    /// `.^private_method_names`: the names of the class's own private methods,
    /// in declaration order.
    // Cost: O(m), m = methods declared directly on the class.
    pub(super) fn class_private_method_names(&self, class_name: &str) -> Value {
        Value::array(
            self.class_private_method_entries(class_name)
                .into_iter()
                .map(|(name, _)| Value::str(name))
                .collect(),
        )
    }
}

/// `method` under the object id its declaration keeps however often the table
/// is built (`.WHERE`, `nqp::objectid`), as Rakudo's one method object per
/// declaration.
// Cost: O(a), a = attributes of the method object.
pub(super) fn stable_method_object(method: Value, class_name: &str, method_name: &str) -> Value {
    use std::collections::HashMap;
    use std::sync::{Mutex, OnceLock};
    static IDS: OnceLock<Mutex<HashMap<(String, String), u64>>> = OnceLock::new();
    let ValueView::Instance {
        class_name: kind,
        attributes,
        ..
    } = method.view()
    else {
        return method;
    };
    let id = *IDS
        .get_or_init(Default::default)
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
        .entry((class_name.to_string(), method_name.to_string()))
        .or_insert_with(crate::value::next_instance_id);
    let attrs = attributes.as_map().clone();
    let own_candidate = attrs.get("candidates").is_some_and(|c| match c.view() {
        ValueView::Array(items, _) => {
            items.len() == 1 && crate::value::identity::values_identical(&items[0], &method)
        }
        _ => false,
    });
    let stable = Value::make_instance_with_id(kind, attrs, id);
    if own_candidate && let ValueView::Instance { attributes, .. } = stable.view() {
        attributes.insert("candidates", Value::array(vec![stable.clone()]));
    }
    stable
}
