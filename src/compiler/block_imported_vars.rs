//! A block-scoped `use` declares the variables it imports in the block
//! (ADR-8751 stage 1).
//!
//! Rakudo installs an imported `$x` in the lexical pad of the block that
//! holds the `use`: it hides an outer `$x` there, a closure taken in the block
//! keeps it, and a routine written outside the block never sees it. An
//! exported variable is always `our`-scoped, so the import is a binding of
//! the module's package variable: the compiler compiles `use M` as if the
//! block went on with `my $x := $M::x;`, for each variable the parse-time
//! scan found the `use` importing (`parser::scanned_exported_vars`). The
//! binding's source is the module's own container (`OpCode::GetImportedVar`),
//! not a by-name read of `$M::x`. The run-time import writes no env alias for
//! such a variable (`runtime::lexical_imports::BLOCK_VARS_MARK`). A module
//! whose export set the scan cannot know (a `sub EXPORT` hook) keeps the
//! run-time import.

use super::*;

impl Compiler {
    /// Declare in the enclosing block each variable the `use` of `module`
    /// with `tags` imports, bound to the module's package variable. Called
    /// right after the `UseModule` of a `use` below the unit's top level, so
    /// the package variable exists when the binding reads it.
    // Cost: O(v), v = the module's exported variables.
    pub(super) fn bind_block_imported_vars(&mut self, module: &str, tags: &[String]) {
        let module_idx = self.code.add_constant(Value::str(module.to_string()));
        for (key, stmt) in Self::block_imported_var_bindings(module, tags) {
            let name_idx = self.code.add_constant(Value::str(key));
            self.imported_var_source = Some((module_idx, name_idx));
            self.compile_stmt(&stmt);
            self.imported_var_source = None;
        }
    }

    /// The suffix of a lexical import set id that names the variables the
    /// block declares for the `use` of `module` with `tags`
    /// (`runtime::lexical_imports::BLOCK_VARS_MARK`); empty when there are
    /// none.
    // Cost: O(v), v = the module's exported variables.
    pub(super) fn block_imported_vars_id_suffix(module: &str, tags: &[String]) -> String {
        let names: Vec<String> = Self::block_imported_var_bindings(module, tags)
            .into_iter()
            .map(|(key, _)| key)
            .collect();
        if names.is_empty() {
            return String::new();
        }
        format!(
            "{}{}",
            crate::runtime::lexical_imports::BLOCK_VARS_MARK,
            names.join(",")
        )
    }

    /// The export key and the binding declaration of each variable the `use`
    /// of `module` with `tags` imports, by the parse-time scan of the module.
    // Cost: O(v), v = the module's exported variables.
    fn block_imported_var_bindings(module: &str, tags: &[String]) -> Vec<(String, Stmt)> {
        let mut bindings: Vec<(String, Stmt)> = crate::parser::scanned_exported_vars(module)
            .iter()
            .flat_map(|vars| vars.iter())
            .filter(|var| var.imported_by(tags))
            .filter_map(|var| Some((var.export_key(), Self::imported_var_binding(var)?)))
            .collect();
        // An exported type is a term of the block too (`my \Foo := M::Foo`),
        // whose value is the module's type object; its type positions in the
        // block were qualified at parse time (`parser::type_qualify`).
        for ty in crate::parser::scanned_exported_types(module)
            .iter()
            .flat_map(|types| types.iter())
            .filter(|ty| ty.imported_by(tags))
        {
            let Some((package, _)) = ty.qualified.rsplit_once("::") else {
                continue;
            };
            let term = crate::parser::ExportedVar {
                name: ty.name.clone(),
                package: package.to_string(),
                tags: ty.tags.clone(),
                term: true,
            };
            if bindings.iter().any(|(key, _)| *key == ty.name) {
                continue;
            }
            if let Some(stmt) = Self::imported_var_binding(&term) {
                bindings.push((ty.name.clone(), stmt));
            }
        }
        bindings
    }

    /// `my <name> := <sigil><package>::<name>` for the exported variable
    /// `var` (`my \NAME := ...` for a sigil-less constant), expanded as the
    /// parser expands a binding declaration, or `None` for a name with a
    /// twigil, which names no plain package variable. The right-hand side
    /// only selects the bind shape: its value comes from `GetImportedVar`.
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
        if var.term {
            let decl = Stmt::VarDecl {
                anonymous: false,
                name: var.name.clone(),
                expr: Expr::BareWord(qualified),
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: vec![
                    ("__has_initializer".to_string(), None),
                    (
                        crate::ast::sigilless_decl::SIGILLESS_DECL.to_string(),
                        Some(Expr::Literal(Value::FALSE)),
                    ),
                ],
                where_constraint: None,
            };
            return Some(Stmt::SyntheticBlock(vec![
                Stmt::MarkBind,
                decl,
                Stmt::MarkSigilless(var.name.clone()),
            ]));
        }
        let (expr, custom_traits) = match sigil {
            Some('@') => (Expr::ArrayVar(qualified), Vec::new()),
            Some(_) => (Expr::HashVar(qualified), Vec::new()),
            None => (
                Expr::Var(qualified),
                vec![(crate::ast::bind_decl::SCALAR_BIND.to_string(), None)],
            ),
        };
        Some(crate::ast::bind_decl::expand(Stmt::VarDecl {
            anonymous: false,
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
        }))
    }
}
