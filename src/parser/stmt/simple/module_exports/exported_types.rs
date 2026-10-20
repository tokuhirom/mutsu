//! The types a `use`d module exports, with the qualified name each one has,
//! for a block-scoped `use` (ADR-8751 stage 1).
//!
//! Rakudo installs a `use`'s symbols in the lexical pad of the block that
//! holds it, so an imported `Foo` names the module's type everywhere in the
//! block, a type position included, and hides an outer `Foo`. The parse-time
//! type qualification pass (`parser::type_qualify`) writes the qualified name
//! into the block's type positions from this record. A module whose export set
//! the scan cannot know (a `sub EXPORT` hook, an export stash under a computed
//! key) has no entry; its types keep the run-time import.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// One type a module exports.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub(crate) struct ExportedType {
    /// The name an importer writes: the declaration's last segment
    /// (`class A::B is export` imports `B`).
    pub(crate) name: String,
    /// The type's full name (`M::A::B`).
    pub(crate) qualified: String,
    /// The tags of its `is export` trait; `DEFAULT` for a bare one.
    pub(crate) tags: Vec<String>,
}

impl ExportedType {
    /// Whether a `use` naming `import_tags` imports this type, by the rule
    /// the run-time import applies (see `ExportedVar::imported_by`).
    // Cost: O(t * i), t = the type's tags, i = the import's tags.
    pub(crate) fn imported_by(&self, import_tags: &[String]) -> bool {
        if import_tags.iter().any(|t| t == "ALL") {
            return true;
        }
        self.tags.iter().any(|tag| {
            tag == "MANDATORY"
                || if import_tags.is_empty() {
                    tag == "DEFAULT"
                } else {
                    import_tags.contains(tag)
                }
        })
    }
}

thread_local! {
    /// The exported types of each module a `use` in the unit being parsed
    /// named; `None` for a module whose export set the scan cannot know.
    static SCANNED_EXPORTED_TYPES: RefCell<HashMap<String, Option<Rc<[ExportedType]>>>> =
        RefCell::new(HashMap::new());
}

/// Record what the scan of `module` learned about its exported types.
// Cost: O(n), n = length of `module`.
pub(super) fn record(module: &str, types: Option<Rc<[ExportedType]>>) {
    SCANNED_EXPORTED_TYPES.with(|m| {
        m.borrow_mut().insert(module.to_string(), types);
    });
}

/// The exported types of `module`, when a `use` of it was parsed and its
/// export set is known from its `is export` traits.
// Cost: O(n), n = length of `module`.
pub(crate) fn scanned_exported_types(module: &str) -> Option<Rc<[ExportedType]>> {
    SCANNED_EXPORTED_TYPES.with(|m| m.borrow().get(module).cloned().flatten())
}

thread_local! {
    /// The type names a module's `sub EXPORT` hook hands out under literal
    /// keys (`'MyAlias' => Int`), by module name: no `is export` trait names
    /// them, so the run time's pre-pass type checks learn them from here.
    static SCANNED_HOOK_TYPE_KEYS: RefCell<HashMap<String, Rc<[String]>>> =
        RefCell::new(HashMap::new());
}

/// Record the hook type keys the scan of `module` found.
// Cost: O(n), n = length of `module`.
pub(super) fn record_hook_type_keys(module: &str, keys: &[String]) {
    SCANNED_HOOK_TYPE_KEYS.with(|m| {
        let mut map = m.borrow_mut();
        if keys.is_empty() {
            map.remove(module);
        } else {
            map.insert(module.to_string(), keys.into());
        }
    });
}

/// The type names `module`'s `sub EXPORT` hook hands out under literal keys,
/// when a `use` of it was parsed.
// Cost: O(n), n = length of `module`.
pub(crate) fn scanned_hook_type_keys(module: &str) -> Option<Rc<[String]>> {
    SCANNED_HOOK_TYPE_KEYS.with(|m| m.borrow().get(module).cloned())
}
