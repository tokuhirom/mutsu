//! The `our` variables a `use`d module exports, for a block-scoped `use`
//! (ADR-8751 stage 1).
//!
//! Rakudo installs a `use`'s symbols in the lexical pad of the block that
//! holds it, so an imported `$x` hides an outer `$x` there, and a routine
//! written outside the block never sees it. mutsu loads modules at run time,
//! so the compiler cannot ask the module what it exports. The parse-time scan
//! can, for a module whose exports are its `is export` traits: an exported
//! variable is always `our`-scoped, so it is the package variable
//! `$Pkg::name`, and the compiler declares the import as the block's own
//! binding of it (`Compiler::bind_block_imported_vars`).
//!
//! The scan cannot tell for a module that computes its export set (a
//! `sub EXPORT` hook, an export stash under a computed key); such a module
//! has no entry here, and its variables keep the run-time import.

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
}

impl ExportedVar {
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

/// Record what the scan of `module` learned about its exported variables.
// Cost: O(n), n = length of `module`.
pub(super) fn record(module: &str, vars: Option<Rc<[ExportedVar]>>) {
    SCANNED_EXPORTED_VARS.with(|m| {
        m.borrow_mut().insert(module.to_string(), vars);
    });
}

/// The exported variables of `module`, when a `use` of it was parsed and its
/// export set is fully known from its `is export` traits.
// Cost: O(n), n = length of `module`.
pub(crate) fn scanned_exported_vars(module: &str) -> Option<Rc<[ExportedVar]>> {
    SCANNED_EXPORTED_VARS.with(|m| m.borrow().get(module).cloned().flatten())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(tags: &[&str]) -> ExportedVar {
        ExportedVar {
            name: "x".to_string(),
            package: "M".to_string(),
            tags: tags.iter().map(|s| s.to_string()).collect(),
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
