//! Compile-time detection of undeclared bareword terms in the mainline.
//!
//! Rakudo resolves a bareword term at compile time: `say FooBarBaz.^name`
//! dies with `===SORRY!=== ... Undeclared name:\n    FooBarBaz used at line 1`
//! before anything runs. Without this check the term reached `GetBareWord`
//! at run time, whose last resort is the name as a `Str`, so a typo'd type
//! name silently became a string (#9768).
//!
//! The judgement is the `EVAL` undeclared-name walk (`eval_name_scans`), with
//! the same scope-blind declaration set the undeclared-routine check uses, so
//! the two agree on what "declared" means. Like that check it is conservative
//! in the safe direction: a unit that imports names the walk cannot see
//! (any `use`/`need`/`import`/`require` other than a pragma or `Test`, a
//! dynamically named sub) is not judged at all.

use super::undeclared_routines::{module_imports_no_names, scope_blind_declared_names};
use super::*;
use crate::ast_visit::{NameKind, Visit, walk_stmt, walk_stmts};
use crate::value::RuntimeErrorCode;

/// Whether the unit pulls in names the walk cannot see.
#[derive(Default)]
struct UnseenImports {
    found: bool,
}

impl<'ast> Visit<'ast> for UnseenImports {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if self.found {
            return;
        }
        match stmt {
            // `Test` exports routines only, which are never barewords terms.
            Stmt::Use { module, .. } if module_imports_no_names(module) || module == "Test" => {}
            Stmt::Use { .. } | Stmt::Need { .. } | Stmt::Import { .. } => self.found = true,
            Stmt::SubDecl {
                name_expr: Some(_), ..
            } => self.found = true,
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if matches!(kind, NameKind::Call | NameKind::UserRoutineCall)
            && matches!(name, "require" | "import" | "need" | "use" | "EVALFILE")
        {
            self.found = true;
        }
    }
}

impl Interpreter {
    /// Reject a bareword term of the mainline that names nothing — no type,
    /// routine, constant or other declaration of the unit, and nothing the
    /// interpreter already has in scope (rakudo's compile-time "Undeclared
    /// name", X::Undeclared::Symbols).
    // Cost: O(n * l), n = size of the unit's AST, l = cost of one
    // registry/env lookup.
    pub(crate) fn check_undeclared_names_mainline(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut unseen = UnseenImports::default();
        walk_stmts(&mut unseen, stmts);
        if unseen.found {
            return Ok(());
        }
        let mut declared = scope_blind_declared_names(stmts);
        let types = Self::unit_declared_types(stmts);
        declared.extend(types.types);
        declared.extend(types.packages);
        let Some((name, line)) = self.first_undeclared_name(stmts, &declared) else {
            return Ok(());
        };
        let suggestions = self.suggest_type_names(&name);
        let mut err = RuntimeError::undeclared_type_symbols(
            &name,
            format!("Undeclared name:\n    {name} used at line {line}"),
            suggestions,
        );
        err.set_code(Some(RuntimeErrorCode::ParseGeneric));
        if line > 0 {
            err.set_line(Some(line as usize));
        }
        Err(err)
    }
}
