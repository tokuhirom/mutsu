//! The `our` variables and constants a `use`d module exports, for a
//! block-scoped `use` (ADR-8751 stage 1).
//!
//! Rakudo installs a `use`'s symbols in the lexical pad of the block that
//! holds it, so an imported `$x` hides an outer `$x` there, and a routine
//! written outside the block never sees it. mutsu loads modules at run time,
//! so the compiler cannot ask the module what it exports. The parse-time scan
//! can, for a module whose exports are its `is export` traits: an exported
//! variable is always `our`-scoped, so it is the package variable
//! `$Pkg::name`, and an exported constant is a value; the compiler declares
//! the import as the block's own binding of it (`Compiler::bind_block_imported_vars`).
//!
//! The scan cannot tell for a module that computes its export set (a
//! `sub EXPORT` hook, an export stash under a computed key). A hook module's
//! entry lists only the names its hook spells as literal pair keys (`hook`
//! entries); everything else it exports keeps the run-time import. A module
//! with a computed export stash has no entry.

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// One `our` variable a module exports.
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
pub(crate) struct ExportedVar {
    /// The declaration's spelling: a scalar without its sigil (`ver`), an
    /// array or hash with it (`@list`), as `Stmt::VarDecl::name` holds it.
    pub(crate) name: String,
    /// The package the variable is declared in (`V`, `Outer::Inner`).
    pub(crate) package: String,
    /// The tags of its `is export` trait.
    pub(crate) tags: Vec<String>,
    /// A sigil-less constant (`constant K is export`): a term, imported
    /// under its term key (`term_names::term_key`).
    #[serde(default)]
    pub(crate) term: bool,
    /// A name a `sub EXPORT` hook returns under a literal key
    /// (`export_hook::collect_export_hook_value_keys`), not a package
    /// variable: the block's binding takes the hook's value from the import
    /// (`OpCode::GetHookExport`).
    #[serde(default)]
    pub(crate) hook: bool,
}

impl ExportedVar {
    /// The name the run-time import knows the export by: the term key of a
    /// sigil-less constant, the declaration spelling otherwise.
    // Cost: O(n), n = length of the name.
    pub(crate) fn export_key(&self) -> String {
        if self.term {
            crate::term_names::term_key(&self.name)
        } else {
            self.name.clone()
        }
    }

    /// Whether a `use` naming `import_tags` imports this variable, by the
    /// rule the run-time import applies (`import_module`): no tags requests
    /// `DEFAULT`, `ALL` requests everything, and `MANDATORY` is always taken.
    // Cost: O(t * i), t = the variable's tags, i = the import's tags.
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
    /// The exported variables of each module a `use` in the unit being
    /// parsed named, by module name; `None` for a module whose export set
    /// the scan cannot know. A module absent from the map was not scanned.
    static SCANNED_EXPORTED_VARS: RefCell<HashMap<String, Option<Rc<[ExportedVar]>>>> =
        RefCell::new(HashMap::new());
}

thread_local! {
    /// The modules whose `SCANNED_EXPORTED_VARS` entry lists only some of the
    /// variables they export (`ModuleDecls::unlisted_exported_vars`): enough
    /// for the block-scoped `use` to declare those, not for a check that
    /// needs every name a `use` brings in.
    static INCOMPLETE_EXPORTED_VARS: RefCell<std::collections::HashSet<String>> =
        RefCell::new(std::collections::HashSet::new());
}

/// Record that `module` exports variables its scan entry does not list.
// Cost: O(n), n = length of `module`.
pub(super) fn record_incomplete(module: &str) {
    INCOMPLETE_EXPORTED_VARS.with(|m| {
        m.borrow_mut().insert(module.to_string());
    });
}

/// Forget an earlier [`record_incomplete`] for a rescan of `module`.
// Cost: O(n), n = length of `module`.
pub(super) fn clear_incomplete(module: &str) {
    INCOMPLETE_EXPORTED_VARS.with(|m| {
        m.borrow_mut().remove(module);
    });
}

/// Record what the scan of `module` learned about its exported variables.
// Cost: O(n), n = length of `module`.
pub(super) fn record(module: &str, vars: Option<Rc<[ExportedVar]>>) {
    SCANNED_EXPORTED_VARS.with(|m| {
        m.borrow_mut().insert(module.to_string(), vars);
    });
}

/// The recorded scan entry of `module`: `None` when none was recorded.
// Cost: O(n), n = length of `module`.
pub(super) fn entry(module: &str) -> Option<Option<Rc<[ExportedVar]>>> {
    SCANNED_EXPORTED_VARS.with(|m| m.borrow().get(module).cloned())
}

/// Put back an [`entry`] of `module`.
// Cost: O(n), n = length of `module`.
pub(super) fn restore_entry(module: &str, entry: Option<Option<Rc<[ExportedVar]>>>) {
    SCANNED_EXPORTED_VARS.with(|m| {
        let mut m = m.borrow_mut();
        match entry {
            Some(vars) => m.insert(module.to_string(), vars),
            None => m.remove(module),
        };
    });
}

/// The entries of a `sub EXPORT` hook's literal keys `keys` (`'$x'`, `'@x'`,
/// `'vrai'`) for `module`, in the spelling a declaration of each holds.
// Cost: O(k), k = keys.
pub(super) fn hook_vars(module: &str, keys: &[String]) -> Rc<[ExportedVar]> {
    keys.iter()
        .map(|key| {
            let (name, term) = match key.chars().next() {
                Some('$') => (key[1..].to_string(), false),
                Some('@' | '%') => (key.clone(), false),
                _ => (key.clone(), true),
            };
            ExportedVar {
                name,
                package: module.to_string(),
                tags: vec!["MANDATORY".to_string()],
                term,
                hook: true,
            }
        })
        .collect()
}

/// The exported variables of `module`, when a `use` of it was parsed and its
/// export set is fully known from its `is export` traits.
// Cost: O(n), n = length of `module`.
pub(crate) fn scanned_exported_vars(module: &str) -> Option<Rc<[ExportedVar]>> {
    SCANNED_EXPORTED_VARS.with(|m| m.borrow().get(module).cloned().flatten())
}

/// [`scanned_exported_vars`] when the list is every variable `module`
/// exports, for a check that must know them all (#12800).
// Cost: O(n), n = length of `module`.
pub(crate) fn scanned_exported_vars_complete(module: &str) -> Option<Rc<[ExportedVar]>> {
    if INCOMPLETE_EXPORTED_VARS.with(|m| m.borrow().contains(module)) {
        return None;
    }
    scanned_exported_vars(module)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(tags: &[&str]) -> ExportedVar {
        ExportedVar {
            name: "x".to_string(),
            package: "M".to_string(),
            tags: tags.iter().map(|s| s.to_string()).collect(),
            term: false,
            hook: false,
        }
    }

    fn tags(t: &[&str]) -> Vec<String> {
        t.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn import_tags_select_exported_variables() {
        assert!(var(&["DEFAULT"]).imported_by(&tags(&[])));
        assert!(!var(&["t"]).imported_by(&tags(&[])));
        assert!(var(&["t"]).imported_by(&tags(&["t"])));
        assert!(!var(&["DEFAULT"]).imported_by(&tags(&["t"])));
        assert!(var(&["t"]).imported_by(&tags(&["ALL"])));
        assert!(var(&["MANDATORY"]).imported_by(&tags(&["t"])));
    }
}
