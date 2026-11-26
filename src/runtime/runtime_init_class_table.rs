//! The class table `build_builtin_registry` fills (#19197).
//!
//! The built-in classes used to be collected in an `FxHashMap<String,
//! ClassDef>` sized for 512 entries and then moved, one by one, into the
//! registry's [`ShardedMap`] of [`CowTable`]s. A `ClassDef` is ~530 bytes, so
//! that scratch table was ~570 KB of buckets: about 140 pages of fresh heap
//! that every process faulted in at start-up, only to free them again
//! (see the startup page-fault breakdown in `docs/rakupp-gap-survey.md`).
//!
//! [`BuiltinClassTable`] writes each class straight into the registry's own
//! representation instead, so the only per-class memory is the `CowTable`'s
//! `Arc<ClassDef>` that the registry keeps anyway.

use super::ClassDef;
use super::builtin_exception_attrs;
use super::registry_cow_table::CowTable;
use super::registry_sharded_map::ShardedMap;
use crate::symbol::Symbol;

/// The built-in class table while `build_builtin_registry` fills it.
pub(super) struct BuiltinClassTable(ShardedMap<String, CowTable<ClassDef>>);

impl BuiltinClassTable {
    // Cost: O(capacity), the shards' bucket allocation.
    pub(super) fn with_capacity(capacity: usize) -> Self {
        let mut map = ShardedMap::default();
        map.reserve(capacity);
        Self(map)
    }

    /// Insert (or replace) a built-in class.
    ///
    /// A built-in exception class has its rakudo attributes declared on first
    /// access, not here: declaring all of them doubled the startup of every
    /// program (#18956). Each such class is marked for that deferral as it is
    /// inserted (#18654), so the table never holds an unmarked one.
    // Cost: O(|name|) expected.
    pub(super) fn insert(&mut self, name: String, mut class_def: ClassDef) {
        if builtin_exception_attrs::may_declare(&name) {
            let class = class_def
                .mro
                .first()
                .copied()
                .filter(|own| own.as_str() == name)
                .unwrap_or_else(|| Symbol::intern(&name));
            class_def.attributes.defer_builtin_exception(class);
        }
        self.0.insert(name, class_def.into());
    }

    // Cost: O(|name|) expected.
    pub(super) fn get(&self, name: &str) -> Option<&ClassDef> {
        self.0.get(name).map(|class_def| &**class_def)
    }

    /// The table in the registry's representation.
    pub(super) fn into_inner(self) -> ShardedMap<String, CowTable<ClassDef>> {
        self.0
    }
}
