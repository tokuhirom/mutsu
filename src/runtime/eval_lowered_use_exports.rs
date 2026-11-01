//! The parser-side import tables of a lowered RakuAST tree's `use` statements.
//!
//! The CHECK-time undeclared-routine scan asks the parser's import table
//! (`parser::is_imported_function`) which routines the unit's `use` statements
//! brought in; a parse fills it as it goes. A tree that was lowered from
//! `RakuAST` nodes (`EVAL($ast)`) was parsed elsewhere, earlier, so the table
//! holds none of it, and a call to a routine a module exports reads as
//! `Undeclared routine` (#15868). This registers the same exports again, in a
//! scope of their own, before the tree is validated.

use super::*;
use crate::ast_visit::{Visit, walk_stmt};

/// The `use` statements of a tree at any depth, in source order.
#[derive(Default)]
struct Uses {
    found: Vec<UseStmt>,
}

struct UseStmt {
    module: String,
    arg: Option<Expr>,
    tags: Vec<String>,
}

impl<'ast> Visit<'ast> for Uses {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if let Stmt::Use {
            module, arg, tags, ..
        } = stmt
        {
            self.found.push(UseStmt {
                module: module.clone(),
                arg: arg.clone(),
                tags: tags.clone(),
            });
        }
        walk_stmt(self, stmt);
    }
}

impl Interpreter {
    /// Register in the parser's import tables what the `use` statements of
    /// `stmts` import, as a parse of the same text would have: a `use lib`
    /// adds its (statically known) search path, every other `use` registers
    /// the module's exports. Call inside a parser scope
    /// ([`crate::parser::with_rakuast_lexical_scope`]) that ends with the check.
    // Cost: O(u * m), u = `use` statements of the tree, m = cost of scanning one
    // module's source for its exports.
    pub(super) fn register_lowered_unit_imports(stmts: &[Stmt]) {
        let mut uses = Uses::default();
        for stmt in stmts {
            uses.visit_stmt(stmt);
        }
        for used in &uses.found {
            if used.module == "lib" {
                if let Some(arg) = &used.arg {
                    for path in crate::parser::use_lib_args(arg) {
                        crate::parser::try_add_parse_time_lib_path(path);
                    }
                }
                continue;
            }
            let tags = (!used.tags.is_empty()).then_some(used.tags.as_slice());
            crate::parser::register_module_exports_with_tags(&used.module, tags);
        }
    }
}
