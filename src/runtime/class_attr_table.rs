//! [`ClassAttrTable`]: a per-(class, attribute) table keyed class-first.
//!
//! The attribute-default tables used to be `HashMap<(String, String), V>`,
//! which made every lookup allocate two `String`s for the key and made "does
//! this class declare any entry at all?" a scan of the whole table. The method
//! fast path asks both on every call, once per attribute of the receiver
//! (#9494: a Text::CSV method call paid ~90 such lookups). Keyed class-first,
//! a lookup borrows its `&str`s and the per-class question is one probe.

use std::collections::HashMap;

/// `(class, attr) -> V`, stored as `class -> (attr -> V)`.
#[derive(Clone, Debug)]
pub(crate) struct ClassAttrTable<V> {
    by_class: HashMap<String, HashMap<String, V>>,
}

impl<V> Default for ClassAttrTable<V> {
    fn default() -> Self {
        Self {
            by_class: HashMap::new(),
        }
    }
}

impl<V> ClassAttrTable<V> {
    /// The entry for `attr` of `class`.
    // Cost: O(1) expected (two hash probes, no allocation).
    pub(crate) fn get(&self, class: &str, attr: &str) -> Option<&V> {
        self.by_class.get(class)?.get(attr)
    }

    /// Whether `class` has any entry.
    // Cost: O(1) expected.
    pub(crate) fn has_class(&self, class: &str) -> bool {
        self.by_class.contains_key(class)
    }

    /// Set the entry for `attr` of `class`, replacing any previous one.
    // Cost: O(1) expected.
    pub(crate) fn insert(&mut self, class: &str, attr: &str, value: V) {
        self.by_class
            .entry(class.to_string())
            .or_default()
            .insert(attr.to_string(), value);
    }

    /// Set the entry for `attr` of `class` unless it already has one.
    // Cost: O(1) expected.
    pub(crate) fn insert_if_absent(&mut self, class: &str, attr: &str, value: V) {
        self.by_class
            .entry(class.to_string())
            .or_default()
            .entry(attr.to_string())
            .or_insert(value);
    }
}

#[cfg(test)]
mod tests {
    use super::ClassAttrTable;

    #[test]
    fn lookups_are_per_class_and_per_attribute() {
        let mut t: ClassAttrTable<i32> = ClassAttrTable::default();
        assert!(!t.has_class("A"));
        t.insert("A", "x", 1);
        t.insert("B", "x", 2);
        assert!(t.has_class("A"));
        assert!(!t.has_class("C"));
        assert_eq!(t.get("A", "x"), Some(&1));
        assert_eq!(t.get("B", "x"), Some(&2));
        assert_eq!(t.get("A", "y"), None);
        assert_eq!(t.get("C", "x"), None);
    }

    #[test]
    fn insert_replaces_and_insert_if_absent_keeps() {
        let mut t: ClassAttrTable<i32> = ClassAttrTable::default();
        t.insert("A", "x", 1);
        t.insert("A", "x", 2);
        assert_eq!(t.get("A", "x"), Some(&2));
        t.insert_if_absent("A", "x", 3);
        assert_eq!(t.get("A", "x"), Some(&2));
        t.insert_if_absent("A", "y", 4);
        assert_eq!(t.get("A", "y"), Some(&4));
    }
}
