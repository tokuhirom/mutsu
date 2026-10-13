//! The CHECK-time registration of the `my enum`s declared below a unit's top
//! level (#13010).
//!
//! An enum is a compile-time declaration in Raku, but mutsu registers one when
//! its `RegisterEnum` op runs, i.e. every time the enclosing routine or supply
//! body runs. The registration writes three tables shared copy-on-write with
//! every thread clone (`Registry::enum_types`, `my_scoped_package_items`, the
//! type metadata), and skips each write when the content is already there. A
//! body that first runs on a thread clone (`start { f() }`, a supply's
//! connection handler) found it new, so the clone copied the whole registry
//! and kept the copy.
//!
//! So the unit's load registers each such enum's shared part once, on the
//! interpreter that loads it, ahead of the mainline: [`RoutineEnumCollector`]
//! finds the declarations, [`Interpreter::preregister_routine_enums`] writes
//! the same entries the run-time registration would. The run-time op still
//! binds the lexical names and finds the shared tables already holding its
//! content. Only a declaration whose content is known without running code is
//! taken: auto-numbered variants, no base type, no `does`, not exported.

use super::*;
use crate::ast::EnumVariantForm;
use crate::ast_visit::{Visit, walk_stmt, walk_stmts};
use crate::value::EnumValue;

/// One `my enum` the unit declares below its top level, as the run-time
/// registration would record it.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct RoutineEnum {
    /// The package the declaration runs in, or `None` for the unit's own.
    package: Option<String>,
    /// That package is a class, whose body's types are reached by short name
    /// through `class_scoped_short_names`.
    in_class: bool,
    name: String,
    variants: Vec<String>,
    language_version: String,
}

/// Finds the declarations [`Interpreter::preregister_routine_enums`]
/// registers.
#[derive(Default)]
pub(crate) struct RoutineEnumCollector {
    /// The enclosing packages, innermost last, each fully qualified, and
    /// whether it is a class.
    packages: Vec<(String, bool)>,
    /// Below the unit's (or a package body's) own statement list.
    nested: bool,
    out: Vec<RoutineEnum>,
}

impl RoutineEnumCollector {
    /// The `my enum`s below the top level of `stmts`.
    // Cost: O(n), n = size of the unit's AST.
    pub(crate) fn collect(stmts: &[Stmt]) -> Vec<RoutineEnum> {
        let mut collector = RoutineEnumCollector::default();
        walk_stmts(&mut collector, stmts);
        collector.out
    }

    // Cost: O(n), n = size of `body`'s subtree.
    fn in_package(&mut self, name: Symbol, body: &[Stmt], is_class: bool) {
        let name = name.resolve();
        let package = if let Some(absolute) = name.strip_prefix("GLOBAL::") {
            absolute.to_string()
        } else {
            match self.packages.last() {
                Some((parent, _)) => {
                    crate::qualified::qualified(Symbol::intern(parent), Symbol::intern(&name))
                        .resolve()
                }
                None => name,
            }
        };
        self.packages.push((package, is_class));
        let saved = std::mem::replace(&mut self.nested, false);
        walk_stmts(self, body);
        self.nested = saved;
        self.packages.pop();
    }

    fn take(&mut self, stmt: &Stmt) {
        let Stmt::EnumDecl {
            name,
            variants,
            variant_form,
            is_export: false,
            is_my: true,
            base_type: None,
            roles,
            language_version,
            ..
        } = stmt
        else {
            return;
        };
        let name = name.resolve();
        if !self.nested
            || name.is_empty()
            || !roles.is_empty()
            || matches!(variant_form, EnumVariantForm::Computed)
            || variants
                .iter()
                .any(|(key, value)| value.is_some() || key == "__DYNAMIC__")
        {
            return;
        }
        let (package, in_class) = match self.packages.last() {
            Some((package, is_class)) => (Some(package.clone()), *is_class),
            None => (None, false),
        };
        self.out.push(RoutineEnum {
            package,
            in_class,
            name,
            variants: variants.iter().map(|(key, _)| key.clone()).collect(),
            language_version: language_version.clone(),
        });
    }
}

impl<'ast> Visit<'ast> for RoutineEnumCollector {
    // Cost: O(n), n = size of `stmt`'s subtree.
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::EnumDecl { .. } => self.take(stmt),
            Stmt::Package {
                name,
                body,
                is_my: false,
                ..
            }
            | Stmt::PackageRuntimeBody {
                name,
                body,
                decl: crate::ast::PackageRuntimeDecl::Package,
                ..
            } => self.in_package(*name, body, false),
            Stmt::PackageRuntimeBody {
                name,
                body,
                decl:
                    crate::ast::PackageRuntimeDecl::Class {
                        is_lexical: false, ..
                    },
                ..
            }
            | Stmt::ClassDecl {
                name,
                body,
                name_expr: None,
                is_lexical: false,
                ..
            } => self.in_package(*name, body, true),
            // A role's body runs in the package of the class that composes
            // it, and a lexical package is left alone too:
            // their declarations are registered when they run, as before.
            Stmt::Package { .. }
            | Stmt::PackageRuntimeBody { .. }
            | Stmt::ClassDecl { .. }
            | Stmt::RoleDecl { .. }
            | Stmt::AugmentClass { .. } => {}
            Stmt::SyntheticBlock(inner) => walk_stmts(self, inner),
            _ => {
                let saved = std::mem::replace(&mut self.nested, true);
                walk_stmt(self, stmt);
                self.nested = saved;
            }
        }
    }
}

impl Interpreter {
    /// Write the shared part of each enum's registration — its variants, its
    /// `my` scoping and its language revision — as its `RegisterEnum` op
    /// would, so a later run on a thread clone writes nothing shared.
    // Cost: O(e * v), e = enums, v = variants per enum.
    pub(crate) fn preregister_routine_enums(&mut self, enums: &[RoutineEnum]) {
        for decl in enums {
            let package = decl.package.as_deref().map(Symbol::intern);
            let key = match package {
                Some(package) if !crate::qualified::is_global_package(package) => {
                    crate::qualified::qualified(package, Symbol::intern(&decl.name)).resolve()
                }
                _ => decl.name.clone(),
            };
            let variants: Vec<(String, EnumValue)> = decl
                .variants
                .iter()
                .enumerate()
                .map(|(index, key)| (key.clone(), EnumValue::Int(index as i64)))
                .collect();
            if self.registry().enum_types.contains_key(&key) {
                // A same-named enum the unit registered already (a second
                // `my enum` of the name, or a package-scoped one): leave it.
                continue;
            }
            crate::value::note_enum_display_name(&key, &decl.name);
            self.registry_mut().enum_types.insert(key.clone(), variants);
            self.mark_my_scoped_package_item(decl.name.clone());
            self.store_language_revision_from_version(&key, &decl.language_version);
            if decl.in_class && !crate::qualified::is_qualified_str(&decl.name) {
                self.register_class_scoped_short_name(&decl.name);
            }
        }
    }
}
