//! Export stashes whose keys are computed at run time (#9500).
//!
//! `OUR::{'&postfix:<' ~ $code ~ '>'} := sub { ... }` inside a module's
//! `my package EXPORT::<tag> { ... }` exports routines whose names exist only
//! once the module body has run. The static scan cannot list them, so it only
//! records *that* the module does this; a `use` of such a module then runs it
//! at parse time (`runtime::parse_time_exports`) to learn the names, as Rakudo
//! does by compiling `use` at BEGIN time.

use super::{InlineModuleExport, is_export_stash_package};
use crate::ast::{Expr, Stmt};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

thread_local! {
    /// Probe results by module name and search path, so a module `use`d from
    /// several files of one process runs at parse time once.
    static PROBE_CACHE: RefCell<HashMap<(String, Vec<String>), Rc<Vec<InlineModuleExport>>>> =
        RefCell::new(HashMap::new());
}

/// Does `stmts` bind into an export stash under a key that is not a literal?
/// Walks the same package nesting the export collector does, and descends into
/// the control flow (`for`, `if`, `while`, `loop`, blocks) a generated export
/// list is built with.
pub(super) fn has_dynamic_export_stash_binding(stmts: &[Stmt]) -> bool {
    walk(stmts, "")
}

fn walk(stmts: &[Stmt], package: &str) -> bool {
    let in_export_stash = is_export_stash_package(package);
    stmts.iter().any(|stmt| match stmt {
        Stmt::Expr(Expr::IndexAssign { target, index, .. }) => {
            in_export_stash
                && matches!(target.as_ref(), Expr::PseudoStash(s) if s == "OUR::")
                && !matches!(index.as_ref(), Expr::Literal(_))
        }
        Stmt::Package { name, body, .. } => {
            let name = name.resolve();
            let nested = if package.is_empty() || name.contains("::") {
                name
            } else {
                format!("{package}::{name}")
            };
            walk(body, &nested)
        }
        // A class/role body is never an export stash (see the collector).
        Stmt::ClassDecl { body, .. } | Stmt::RoleDecl { body, .. } => walk(body, ""),
        Stmt::Block(body)
        | Stmt::SyntheticBlock(body)
        | Stmt::For { body, .. }
        | Stmt::While { body, .. }
        | Stmt::Loop { body, .. } => walk(body, package),
        Stmt::If {
            then_branch,
            else_branch,
            ..
        } => walk(then_branch, package) || walk(else_branch, package),
        _ => false,
    })
}

/// Run `module` at parse time and return the routines its load exported, as
/// parser export records. A probe that fails (the module dies while loading)
/// contributes nothing: the program's own run-time `use` loads the module
/// again and reports the failure there, at its real location.
pub(super) fn probe_dynamic_exports(module: &str) -> Rc<Vec<InlineModuleExport>> {
    let lib_paths = super::parser_lib_paths();
    let key = (module.to_string(), lib_paths.clone());
    if let Some(hit) = PROBE_CACHE.with(|c| c.borrow().get(&key).cloned()) {
        return hit;
    }
    let names =
        crate::runtime::parse_time_exports::probe_module_exports(module.to_string(), lib_paths)
            .unwrap_or_default();
    let exports: Rc<Vec<InlineModuleExport>> = Rc::new(
        names
            .into_iter()
            .map(|name| InlineModuleExport {
                name,
                precedence: None,
                associativity: None,
                is_test_assertion: false,
            })
            .collect(),
    );
    PROBE_CACHE.with(|c| c.borrow_mut().insert(key, exports.clone()));
    exports
}
