//! Whole-compunit scans a module load asks of the parsed module: which types
//! it exports, and whether any of its subs declares `state` (ADR-0137 typed
//! visitor).

use super::*;

impl Interpreter {
    /// The bare type names a compunit declared `is export`.
    ///
    /// `class C is export` / `grammar G is export` desugars to the declaration
    /// followed by a `__MUTSU_EXPORT_TYPE__("C", <tags>)` marker call (see
    /// `parser::stmt::class::class_decl::export_type_stmt`), so reading the
    /// markers back out of the parsed compunit is the same answer the runtime
    /// export table gets — without having to guess which package the runtime
    /// filed it under. Only unqualified names are returned: a `class A::B is
    /// export` publishes the compound name `A::B`, which the bare-short-name
    /// alias table this feeds cannot express.
    // Cost: O(n), n = size of the compunit's AST.
    pub(super) fn collect_exported_type_names(stmts: &[crate::ast::Stmt]) -> HashSet<String> {
        let mut scan = ExportedTypeNames::default();
        crate::ast_visit::walk_stmts(&mut scan, stmts);
        scan.0
    }

    /// True if `stmts` declares, anywhere, a `sub`/`proto`/`multi` whose body
    /// declares a `state` variable. Used to skip the shared-body capture
    /// compile for modules that cannot benefit.
    // Cost: O(n), n = size of the compunit's AST; stops at the first hit.
    pub(super) fn module_has_state_sub(stmts: &[crate::ast::Stmt]) -> bool {
        let mut scan = HasStateSub(false);
        crate::ast_visit::walk_stmts(&mut scan, stmts);
        scan.0
    }
}

/// The walk of `Interpreter::collect_exported_type_names` (ADR-0137 visitor).
/// It descends everywhere: rakudo exports a `my class C is export` declared
/// in a nested package body or inside a routine just the same.
#[derive(Default)]
struct ExportedTypeNames(HashSet<String>);

impl crate::ast_visit::Visit for ExportedTypeNames {
    fn visit_stmt(&mut self, stmt: &crate::ast::Stmt) {
        if let crate::ast::Stmt::ClassDecl {
            name,
            custom_traits,
            ..
        } = stmt
            && custom_traits
                .iter()
                .any(|(trait_name, _)| trait_name == "__mutsu_export_type")
        {
            self.0.insert(name.resolve());
        }
        crate::ast_visit::walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &crate::ast::Expr) {
        if let crate::ast::Expr::Call { name, args } = expr
            && name.resolve() == "__MUTSU_EXPORT_TYPE__"
            && let Some(crate::ast::Expr::Literal(value)) = args.first()
        {
            let name = value.to_string_value();
            if !name.is_empty() && !name.contains("::") {
                self.0.insert(name);
            }
        }
        crate::ast_visit::walk_expr(self, expr);
    }
}

/// The walk of `Interpreter::module_has_state_sub` (ADR-0137 visitor).
struct HasStateSub(bool);

impl crate::ast_visit::Visit for HasStateSub {
    fn visit_stmt(&mut self, stmt: &crate::ast::Stmt) {
        if self.0 {
            return;
        }
        if let crate::ast::Stmt::SubDecl { body, .. } | crate::ast::Stmt::ProtoDecl { body, .. } =
            stmt
            && Interpreter::function_body_declares_state(body)
        {
            self.0 = true;
            return;
        }
        crate::ast_visit::walk_stmt(self, stmt);
    }
}
