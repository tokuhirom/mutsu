//! A block-scoped `use` declares the variables it imports in the block
//! (ADR-8751 stage 1).
//!
//! Rakudo installs an imported `$x` in the lexical pad of the block that
//! holds the `use`: it hides an outer `$x` there, a closure taken in the block
//! keeps it, and a routine written outside the block never sees it. An
//! exported variable is always `our`-scoped, so the import is a binding of
//! the package variable: the compiler compiles `use M` as if the block went on
//! with `my $x := $M::x;`, for each variable the parse-time scan found the
//! `use` importing (`parser::scanned_exported_vars`). A module whose export
//! set the scan cannot know (a `sub EXPORT` hook) keeps the run-time import.

use super::*;

impl Compiler {
    /// Declare in the enclosing block each variable the `use` of `module`
    /// with `tags` imports, bound to the module's package variable. Called
    /// right after the `UseModule` of a `use` below the unit's top level, so
    /// the package variable exists when the binding reads it.
    // Cost: O(v), v = the module's exported variables.
    pub(super) fn bind_block_imported_vars(&mut self, module: &str, tags: &[String]) {
        let Some(vars) = crate::parser::scanned_exported_vars(module) else {
            return;
        };
        for var in vars.iter().filter(|var| var.imported_by(tags)) {
            let Some(decl) = Self::imported_var_binding(var) else {
                continue;
            };
            self.compile_stmt(&crate::ast::bind_decl::expand(decl));
        }
    }

    /// `my <name> := <sigil><package>::<name>` for the exported variable
    /// `var`, or `None` for a name with a twigil, which names no plain
    /// package variable.
    // Cost: O(n), n = length of the qualified name.
    fn imported_var_binding(var: &crate::parser::ExportedVar) -> Option<Stmt> {
        let (sigil, bare) = match var.name.chars().next()? {
            sigil @ ('@' | '%') => (Some(sigil), &var.name[1..]),
            _ => (None, var.name.as_str()),
        };
        if bare.is_empty() || !bare.starts_with(|c: char| c.is_alphabetic() || c == '_') {
            return None;
        }
        let qualified = format!("{}::{bare}", var.package);
        let (expr, custom_traits) = match sigil {
            Some('@') => (Expr::ArrayVar(qualified), Vec::new()),
            Some(_) => (Expr::HashVar(qualified), Vec::new()),
            None => (
                Expr::Var(qualified),
                vec![(crate::ast::bind_decl::SCALAR_BIND.to_string(), None)],
            ),
        };
        Some(Stmt::VarDecl {
            name: var.name.clone(),
            expr,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits,
            where_constraint: None,
        })
    }
}
