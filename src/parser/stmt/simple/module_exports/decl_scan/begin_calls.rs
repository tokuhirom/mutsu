//! Whether a module's BEGIN phaser calls a routine of a module it loads
//! (#17557).
//!
//! Such a call can change what the module provides to its importers at BEGIN
//! time -- CompUnit::Util's `re-exporthow` merges another unit's `EXPORTHOW`
//! into the module's own -- which no static scan can see. The importer's parse
//! then runs the module (`runtime::parse_time_exports`) to learn the result.
//!
//! The gate is narrow on purpose (every flagged module runs twice): a BEGIN
//! phaser on the package spine (the module's own statements and its package
//! bodies) must call a routine that is no core one, and the module must load
//! another module (`use` / `need` of a name that is no pragma), the only
//! place such a routine can come from.

use crate::ast::{Expr, PhaserKind, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt, walk_stmts};

/// Whether `stmts`, a module's statements, has a BEGIN phaser on its package
/// spine that calls a non-core routine, and loads another module.
// Cost: O(n), n = size of the module's AST.
pub(super) fn calls_loaded_routine(stmts: &[Stmt]) -> bool {
    let mut scan = SpineScan::default();
    walk_stmts(&mut scan, stmts);
    scan.loads_module && scan.begin_calls_routine
}

#[derive(Default)]
struct SpineScan {
    loads_module: bool,
    begin_calls_routine: bool,
}

impl<'ast> Visit<'ast> for SpineScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::Use { module, .. } | Stmt::Need { module } => {
                self.loads_module |= loads_a_module(module);
            }
            Stmt::Phaser {
                kind: PhaserKind::Begin,
                body,
                ..
            } => {
                let mut calls = Calls::default();
                walk_stmts(&mut calls, body);
                self.begin_calls_routine |= calls.found;
            }
            // The package spine: the module's own statements, its package-like
            // bodies, and the blocks the parser wraps a traited declarator in.
            Stmt::Package { .. }
            | Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::Block(_) => walk_stmt(self, stmt),
            _ => {}
        }
    }
}

/// Whether `module` names a module rather than a pragma (`lib`, `v6`,
/// `experimental`, `MONKEY-SEE-NO-EVAL`, ...).
// Cost: O(n), n = bytes of the name.
fn loads_a_module(module: &str) -> bool {
    module.starts_with(|c: char| c.is_ascii_uppercase()) && !module.starts_with("MONKEY")
}

/// Finds a call of a routine that is no core one.
#[derive(Default)]
struct Calls {
    found: bool,
}

impl<'ast> Visit<'ast> for Calls {
    // A call statement of a routine the parse already knows (`re-exporthow(...)`
    // once the module's own `use` imported it).
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if let Stmt::Call { name, .. } = stmt
            && !crate::builtin_functions::is_builtin_function(&name.resolve())
        {
            self.found = true;
            return;
        }
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if let Expr::Call { name, .. } = expr
            && !crate::builtin_functions::is_builtin_function(&name.resolve())
        {
            self.found = true;
            return;
        }
        walk_expr(self, expr);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn flag(src: &str) -> bool {
        let (stmts, _) = crate::parse_dispatch::parse_source(src).expect("parses");
        calls_loaded_routine(&stmts)
    }

    #[test]
    fn a_begin_call_of_a_loaded_routine_is_flagged() {
        assert!(flag("use Some::Util; BEGIN re-export('x');"));
        assert!(flag("need Some::Util; package P { BEGIN { helper(1) } }"));
        assert!(flag(
            "use CompUnit::Util :re-export;\nneed exports-stuff;\n\nmy package EXPORTHOW {\n    package DECLARE {\n        constant digimon = Metamodel::ClassHOW;\n    }\n}\n\nBEGIN re-exporthow('exports-stuff');\n"
        ));
    }

    #[test]
    fn other_shapes_are_not() {
        // No module loaded: the routine can only be the module's own.
        assert!(!flag("sub helper { }; BEGIN helper();"));
        // A core routine.
        assert!(!flag("use Some::Util; BEGIN say 1;"));
        // A pragma is no module.
        assert!(!flag("use lib 'x'; BEGIN helper();"));
        // Off the spine.
        assert!(!flag("use Some::Util; sub f { BEGIN helper() }"));
    }
}
