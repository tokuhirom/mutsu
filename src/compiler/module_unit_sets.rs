//! The lexical import sets of a module unit's top-level `use`s (ADR-8751).
//!
//! A module's top-level `use` binds the variables, constants and types it
//! imports in the module mainline's pad, as a block's `use` does
//! (`block_imported_vars`). The set id says which: it is the block set id,
//! marked `MODULE_UNIT_MARK`, whose variables section leaves out every name
//! the module declares at its top level itself. Such a declaration shadows
//! the import in rakudo (`my constant Atom is export` next to a `use` that
//! exports an `Atom`), so the name keeps the run-time import.

use super::*;
use crate::runtime::lexical_imports::{BLOCK_VARS_MARK, MODULE_UNIT_MARK};

impl Compiler {
    /// The id of a module unit's set for the block set id `id`: the same
    /// import list, its variables less the unit's own names
    /// (`module_unit_own_names`), marked `MODULE_UNIT_MARK`.
    // Cost: O(n), n = length of `id`.
    pub(super) fn module_unit_set_id(&self, id: Symbol) -> Symbol {
        let text = id.resolve();
        let (base, vars) = match text.split_once(BLOCK_VARS_MARK) {
            Some((base, vars)) => (base, vars),
            None => (text.as_str(), ""),
        };
        let own = self.module_unit_own_names.as_ref();
        let kept: Vec<&str> = vars
            .split(',')
            .filter(|name| !name.is_empty() && !own.is_some_and(|own| own.contains(*name)))
            .collect();
        let vars = if kept.is_empty() {
            String::new()
        } else {
            format!("{BLOCK_VARS_MARK}{}", kept.join(","))
        };
        Symbol::intern(&format!("{base}{vars}{MODULE_UNIT_MARK}"))
    }

    /// The binding keys of what the top level `stmts` declares itself: its
    /// variables and constants (under both a scalar's and a term's key, the
    /// spelling alone does not say which) and its types and enum values. The
    /// body of a `unit class` / `unit module` is the rest of the file, so it
    /// counts as the top level too.
    // Cost: O(n), n = top-level statements.
    pub(super) fn unit_declared_names(stmts: &[Stmt]) -> std::collections::HashSet<String> {
        let mut names = std::collections::HashSet::new();
        Self::collect_unit_declared_names(stmts, &mut names);
        names
    }

    // Cost: O(n), n = top-level statements.
    fn collect_unit_declared_names(stmts: &[Stmt], names: &mut std::collections::HashSet<String>) {
        let term = |names: &mut std::collections::HashSet<String>, name: &str| {
            names.insert(crate::term_names::term_key(name));
        };
        for stmt in crate::ast::scope_statements(stmts) {
            match stmt {
                Stmt::VarDecl { name, .. } => {
                    names.insert(name.clone());
                    term(names, name);
                }
                Stmt::ClassDecl {
                    name,
                    body,
                    is_unit,
                    ..
                } => {
                    term(names, &name.resolve());
                    if *is_unit {
                        Self::collect_unit_declared_names(body, names);
                    }
                }
                Stmt::Package { body, is_unit, .. } if *is_unit => {
                    Self::collect_unit_declared_names(body, names);
                }
                Stmt::RoleDecl { name, .. } | Stmt::SubsetDecl { name, .. } => {
                    term(names, &name.resolve())
                }
                Stmt::EnumDecl { name, variants, .. } => {
                    term(names, &name.resolve());
                    for (variant, _) in variants {
                        term(names, variant);
                    }
                }
                _ => {}
            }
        }
    }
}
