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

    /// The `(short name, registered name)` of every type a `my package
    /// EXPORT::<tag> { ... }` block declares directly (`class IP`, `role`,
    /// `subset`, `enum`): rakudo makes the stash's contents the tag's export
    /// list, so the importer sees them under their short names although the
    /// runtime filed them as `EXPORT::<tag>::Name` (#16332).
    // Cost: O(n), n = size of the compunit's AST.
    pub(super) fn collect_export_stash_types(stmts: &[crate::ast::Stmt]) -> Vec<(String, String)> {
        let mut scan = ExportStashTypes::default();
        crate::ast_visit::walk_stmts(&mut scan, stmts);
        scan.out
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

impl<'ast> crate::ast_visit::Visit<'ast> for ExportedTypeNames {
    fn visit_stmt(&mut self, stmt: &'ast crate::ast::Stmt) {
        if let crate::ast::Stmt::ClassDecl {
            name,
            export_tags: Some(_),
            ..
        } = stmt
        {
            let name = name.resolve();
            // A qualified declarator name publishes its last part, as rakudo does.
            let sym = crate::symbol::Symbol::intern(&name);
            if crate::qualified::is_qualified(sym) {
                self.0
                    .insert(crate::qualified::unqualified_part(sym).as_str().to_string());
            }
            self.0.insert(name);
        }
        // `unit role A::B::R is export;` publishes the short name `R` too.
        if let crate::ast::Stmt::RoleDecl {
            name,
            is_export: true,
            ..
        } = stmt
        {
            let name = name.resolve();
            let sym = crate::symbol::Symbol::intern(&name);
            if crate::qualified::is_qualified(sym) {
                self.0
                    .insert(crate::qualified::unqualified_part(sym).as_str().to_string());
            }
            self.0.insert(name);
        }
        crate::ast_visit::walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'ast crate::ast::Expr) {
        if let crate::ast::Expr::Call { name, args, .. } = expr
            && name.resolve() == "__MUTSU_EXPORT_TYPE__"
            && let Some(crate::ast::Expr::Literal(value)) = args.first()
        {
            let name = value.to_string_value();
            // `class A::B::C is export` publishes the short name `C`, as rakudo
            // does for a qualified declarator name.
            let sym = crate::symbol::Symbol::intern(&name);
            let short = crate::qualified::unqualified_part(sym).as_str().to_string();
            if !short.is_empty() {
                self.0.insert(short);
            }
            if crate::qualified::is_qualified(sym) {
                self.0.insert(name);
            }
        }
        crate::ast_visit::walk_expr(self, expr);
    }
}

/// The walk of `Interpreter::collect_export_stash_types` (ADR-0137 visitor).
/// `stash` is the `EXPORT::<tag>` package being walked; `nested` counts the
/// scopes (packages, classes, roles, routines) opened inside it, so only a type
/// declared directly in the stash counts.
#[derive(Default)]
struct ExportStashTypes {
    out: Vec<(String, String)>,
    stash: Option<String>,
    nested: usize,
}

impl<'ast> crate::ast_visit::Visit<'ast> for ExportStashTypes {
    fn visit_stmt(&mut self, stmt: &'ast crate::ast::Stmt) {
        use crate::ast::Stmt;
        if let Stmt::Package { name, .. } = stmt {
            let package = name.resolve();
            if Interpreter::export_stash_tag(&package).is_some() {
                let saved = (self.stash.replace(package), self.nested);
                self.nested = 0;
                crate::ast_visit::walk_stmt(self, stmt);
                (self.stash, self.nested) = saved;
                return;
            }
        }
        let declared = match stmt {
            Stmt::ClassDecl {
                name,
                is_lexical: false,
                ..
            }
            | Stmt::RoleDecl {
                name,
                is_lexical: false,
                ..
            }
            | Stmt::SubsetDecl {
                name, is_my: false, ..
            }
            | Stmt::EnumDecl {
                name, is_my: false, ..
            } => Some(name.resolve()),
            _ => None,
        };
        if let (Some(declared), Some(stash)) = (declared, self.stash.as_deref())
            && self.nested == 0
            && !declared.starts_with("__ANON_")
            && !crate::qualified::is_qualified_str(&declared)
        {
            let registered = crate::qualified::qualified_text(stash, &declared)
                .as_str()
                .to_string();
            self.out.push((declared, registered));
        }
        let opens_scope = matches!(
            stmt,
            Stmt::Package { .. }
                | Stmt::ClassDecl { .. }
                | Stmt::RoleDecl { .. }
                | Stmt::SubDecl { .. }
                | Stmt::ProtoDecl { .. }
        );
        if opens_scope {
            self.nested += 1;
        }
        crate::ast_visit::walk_stmt(self, stmt);
        if opens_scope {
            self.nested -= 1;
        }
    }
}

/// The walk of `Interpreter::module_has_state_sub` (ADR-0137 visitor).
struct HasStateSub(bool);

impl<'ast> crate::ast_visit::Visit<'ast> for HasStateSub {
    fn visit_stmt(&mut self, stmt: &'ast crate::ast::Stmt) {
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
