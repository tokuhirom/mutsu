//! The variables a package the `EVAL`'d unit itself declares exports, for the
//! undeclared-variable check: `module N { our $v is export(:t) = 3 }; import
//! N :t; $v` declares `$v` at the `import`, before anything runs.

use std::collections::{HashMap, HashSet};

use crate::ast::Stmt;
use crate::ast_visit::{Visit, walk_stmt, walk_stmts};

/// Each unit-declared package's exported variable names (in the form a
/// `VarDecl` carries them), keyed by the package's name. Tags are not
/// consulted: a name some `import` might bring in is declared, the safe
/// direction for an undeclared-variable check.
// Cost: O(n), n = size of the unit's AST.
pub(super) fn package_exported_vars(stmts: &[Stmt]) -> HashMap<String, Vec<String>> {
    let mut scan = PackageExports::default();
    walk_stmts(&mut scan, stmts);
    scan.exports
}

#[derive(Default)]
struct PackageExports {
    exports: HashMap<String, Vec<String>>,
}

impl<'ast> Visit<'ast> for PackageExports {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if let Stmt::Package { name, body, .. } = stmt {
            let mut seen = HashSet::new();
            let names: Vec<String> = body
                .iter()
                .filter_map(|s| match s {
                    Stmt::VarDecl {
                        name,
                        is_export: true,
                        ..
                    } if seen.insert(name.clone()) => Some(name.clone()),
                    _ => None,
                })
                .collect();
            if !names.is_empty() {
                self.exports
                    .entry(name.resolve())
                    .or_default()
                    .extend(names);
            }
        }
        walk_stmt(self, stmt);
    }
}
