//! What a `use`d module declares, as one walk over its parsed statements
//! (ADR-0137).
//!
//! The importer's parse needs to know which names the module makes routines,
//! types, enum values and value terms, whether it defines a slang, which
//! `EXPORTHOW::DECLARE` keywords it adds, and whether it binds into an export
//! stash under a computed key. All of that is read off one typed-visitor walk
//! of the module's AST, so every position is searched.
//!
//! Two notions of position matter:
//!
//! - The **package spine** is the module's own statement list and the bodies
//!   of the `package`/`module`/`class`/`role`/`grammar` declarations on it,
//!   through the bare and synthetic blocks the parser wraps a traited or
//!   adverbed declarator in. Everything a module declares there reaches the
//!   importer's parse, exported or not — the superset ADR-0087 describes.
//! - Anywhere else (a routine body, a control-flow block, an expression), a
//!   declaration is collected only when rakudo makes it reachable from the
//!   importer: an `is export` routine, operator alias, token, constant or enum
//!   is exported from any depth, and an `our`-scoped type is installed in its
//!   package from any depth (`sub f { class C { } }` declares `Pkg::C`). A
//!   lexical (`my`) type or a non-exported constant or enum value there stays
//!   private to its block, so it is not collected: as a bare-name superset it
//!   could shadow an importer's own routine or quote construct.

use super::{
    InlineModuleExport, compose_type_name, is_export_stash_package, is_our_scoped, sub_export_entry,
};
use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt, walk_stmts};
use std::collections::HashMap;

/// The declarations [`scan_module_decls`] found.
#[derive(Default)]
pub(super) struct ModuleDecls {
    /// Declared type names (class/role/enum/package/grammar), each both as
    /// written and composed with its enclosing packages.
    pub(super) type_names: Vec<String>,
    /// The subset of `type_names` that are enums.
    pub(super) enum_type_names: Vec<String>,
    /// Enum values every `use` of the module makes visible to the parse.
    pub(super) enum_values: Vec<String>,
    /// Enum values exported only under explicit non-default tags, with those
    /// tags (see `enum_values.rs`).
    pub(super) tagged_enum_values: Vec<(String, Vec<String>)>,
    /// Bare names of `constant` declarations.
    pub(super) constant_names: Vec<String>,
    /// Exported routines, by name.
    pub(super) exports: HashMap<String, InlineModuleExport>,
    /// `(keyword, HOW type name)` pairs from `EXPORTHOW::DECLARE` packages.
    pub(super) declare_keywords: Vec<(String, String)>,
    /// Whether the module calls `.define_slang` (an L10N distribution's
    /// generated EXPORT hook).
    pub(super) defines_slang: bool,
    /// Whether the module binds into an export stash under a key that is not
    /// a literal (`dynamic_stash.rs`, #9500).
    pub(super) dynamic_export_stash: bool,
}

/// Walk a module's parsed statements once and collect what the importer's
/// parse needs to know about it (see the module doc).
// Cost: O(n), n = size of the module's AST.
pub(super) fn scan_module_decls(stmts: &[Stmt]) -> ModuleDecls {
    let mut scan = DeclScan {
        out: ModuleDecls::default(),
        prefix: String::new(),
        package: String::new(),
        spine: true,
    };
    walk_stmts(&mut scan, stmts);
    scan.out
}

struct DeclScan {
    out: ModuleDecls,
    /// The `::`-joined path of the enclosing package-like declarators, which
    /// a nested type name is composed with. A `unit` declarator's path is
    /// carried across the rest of its statement list.
    prefix: String,
    /// The full name of the enclosing `package`/`module` — `""` inside a
    /// class or role body, which is never an export stash — so a nested
    /// `package EXPORT { package DEFAULT { } }` is recognised as the
    /// `EXPORT::DEFAULT` stash like the one-line spelling.
    package: String,
    /// Whether the walk is on the package spine (see the module doc).
    spine: bool,
}

impl DeclScan {
    /// Walk `stmt`, a package-like declaration whose body opens the package
    /// path `prefix` / `package`.
    fn enter_package(&mut self, stmt: &Stmt, prefix: String, is_unit: bool, package: String) {
        let saved_prefix = std::mem::replace(&mut self.prefix, prefix);
        let saved_package = std::mem::replace(&mut self.package, package);
        walk_stmt(self, stmt);
        self.package = saved_package;
        if !is_unit {
            self.prefix = saved_prefix;
        }
    }

    /// Run `f` off the package spine.
    fn off_spine(&mut self, f: impl FnOnce(&mut Self)) {
        let saved = std::mem::replace(&mut self.spine, false);
        f(self);
        self.spine = saved;
    }

    fn push_type(&mut self, composed: String, name: String, is_enum: bool) {
        if is_enum {
            self.out.enum_type_names.push(composed.clone());
            self.out.enum_type_names.push(name.clone());
        }
        self.out.type_names.push(composed);
        self.out.type_names.push(name);
    }

    fn export(&mut self, name: String) {
        self.out
            .exports
            .entry(name.clone())
            .or_insert(InlineModuleExport {
                name,
                precedence: None,
                associativity: None,
                is_test_assertion: false,
            });
    }

    /// `my package EXPORTHOW { package DECLARE { constant kw = SomeHOW } }`:
    /// a `constant` inside a package parses as an our-scoped `VarDecl`
    /// carrying the `__constant` marker trait, with the HOW type name as a
    /// bareword initializer.
    fn collect_declare_keywords(&mut self, exporthow_body: &[Stmt]) {
        for inner in exporthow_body {
            let Stmt::Package { name, body, .. } = inner else {
                continue;
            };
            if name.resolve() != "DECLARE" {
                continue;
            }
            for decl in body {
                if let Stmt::VarDecl {
                    name,
                    expr: Expr::BareWord(how_type),
                    custom_traits,
                    ..
                } = decl
                    && custom_traits.iter().any(|(t, _)| t == "__constant")
                {
                    self.out
                        .declare_keywords
                        .push((name.clone(), how_type.clone()));
                }
            }
        }
    }

    fn collect_enum_values(
        &mut self,
        variants: &[(String, Option<Expr>)],
        is_export: bool,
        export_tags: &[String],
    ) {
        let mut names: Vec<String> = variants
            .iter()
            .map(|(name, _)| name.clone())
            .filter(|name| name != "__DYNAMIC__" && !name.is_empty())
            .collect();
        if let [(name, Some(body))] = variants
            && name == "__DYNAMIC__"
        {
            crate::parser::stmt::decl::collect_dynamic_enum_value_names(body, &mut names);
        }
        if is_export && !super::enum_values::exported_by_default(export_tags) {
            self.out
                .tagged_enum_values
                .extend(names.into_iter().map(|n| (n, export_tags.to_vec())));
        } else {
            self.out.enum_values.extend(names);
        }
    }
}

impl Visit for DeclScan {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::ClassDecl {
                name,
                is_lexical,
                is_unit,
                ..
            } => {
                let name = name.resolve();
                // An anonymous class has no name to import, and opens no
                // package path of its own.
                if name.starts_with("__ANON_") {
                    return self.enter_package(stmt, self.prefix.clone(), false, String::new());
                }
                let composed = compose_type_name(&self.prefix, &name);
                if self.spine || !*is_lexical {
                    self.push_type(composed.clone(), name, false);
                }
                self.enter_package(stmt, composed, *is_unit, String::new());
            }
            Stmt::RoleDecl { name, .. } => {
                let name = name.resolve();
                if name.starts_with("__ANON_") {
                    return self.enter_package(stmt, self.prefix.clone(), false, String::new());
                }
                let composed = compose_type_name(&self.prefix, &name);
                self.push_type(composed.clone(), name, false);
                self.enter_package(stmt, composed, false, String::new());
            }
            Stmt::Package {
                name,
                body,
                is_unit,
                is_my,
                ..
            } => {
                let name = name.resolve();
                // `GLOBAL` is a pseudo-package: `package GLOBAL::X::Foo`
                // installs `X::Foo`, so it is not part of the composed name.
                let composed =
                    compose_type_name(&self.prefix, name.strip_prefix("GLOBAL::").unwrap_or(&name));
                let package = if self.package.is_empty() || name.contains("::") {
                    name.clone()
                } else {
                    format!("{}::{name}", self.package)
                };
                if self.spine && name == "EXPORTHOW" {
                    self.collect_declare_keywords(body);
                }
                // `grammar Foo { }` names a type; a `module`/`package` name is a
                // namespace, which is harmless to register.
                if self.spine || !*is_my {
                    self.push_type(composed.clone(), name, false);
                }
                self.enter_package(stmt, composed, *is_unit, package);
            }
            Stmt::EnumDecl {
                name,
                variants,
                is_export,
                export_tags,
                is_my,
                ..
            } => {
                if self.spine || !*is_my {
                    let name = name.resolve();
                    self.push_type(compose_type_name(&self.prefix, &name), name, true);
                }
                if self.spine || *is_export {
                    self.collect_enum_values(variants, *is_export, export_tags);
                }
                self.off_spine(|v| walk_stmt(v, stmt));
            }
            Stmt::VarDecl {
                name,
                is_export,
                custom_traits,
                ..
            } => {
                // Sigiled constants (`constant $x = 1`) are not barewords.
                if custom_traits.iter().any(|(t, _)| t == "__constant")
                    && (self.spine || *is_export)
                    && !name.is_empty()
                    && !name.starts_with(['$', '@', '%', '&'])
                {
                    self.out.constant_names.push(name.clone());
                }
                // `our &infix:<op> is export = &[other];` (PatternMatching's
                // `┇` alias) exports a routine under the same `&name` a
                // `sub name is export` would.
                if *is_export && name.len() > 1 && name.starts_with('&') {
                    self.export(name[1..].to_string());
                }
                self.off_spine(|v| walk_stmt(v, stmt));
            }
            Stmt::SubDecl {
                name,
                is_export,
                associativity,
                precedence_trait,
                is_test_assertion,
                custom_traits,
                ..
            } => {
                // Every `is export` sub is collected, whatever tag it carries:
                // the set answers only "is `name` a routine" for the
                // importer's parse (ADR-0087, #7939); run-time import honours
                // the tags. An `our` sub declared directly in a module's own
                // `EXPORT::<tag>` stash is exported by construction
                // (Net::IP::Parse's `our sub infix:<< ip== >>`).
                if *is_export
                    || (is_export_stash_package(&self.package) && is_our_scoped(custom_traits))
                {
                    let entry = sub_export_entry(
                        name.resolve(),
                        precedence_trait.as_ref(),
                        associativity.clone(),
                        *is_test_assertion,
                    );
                    self.out.exports.insert(entry.name.clone(), entry);
                }
                self.off_spine(|v| walk_stmt(v, stmt));
            }
            Stmt::ProtoDecl {
                name,
                is_export,
                is_our,
                ..
            } => {
                // Same superset rationale as a sub; an `our proto` in an export
                // stash is the only way to put a multi family there (raku
                // rejects `our multi sub`).
                if *is_export || (is_export_stash_package(&self.package) && *is_our) {
                    self.export(name.resolve());
                }
                self.off_spine(|v| walk_stmt(v, stmt));
            }
            // `my token foo is export { ... }` exports a Regex under `&foo`,
            // the namespace a `sub foo is export` uses.
            Stmt::TokenDecl {
                name,
                is_export,
                export_tags,
                ..
            }
            | Stmt::RuleDecl {
                name,
                is_export,
                export_tags,
                ..
            } => {
                if *is_export
                    && export_tags
                        .iter()
                        .any(|t| t == "DEFAULT" || t == "MANDATORY")
                {
                    self.export(name.resolve());
                }
                self.off_spine(|v| walk_stmt(v, stmt));
            }
            // A traited or adverbed declarator (`class Foo is export { }`,
            // `module Foo:auth<x> { }`) is wrapped in a bare or synthetic
            // block together with its metadata statements. The wrapper opens
            // no package, so it stays on the spine with the same paths.
            Stmt::Block(_) | Stmt::SyntheticBlock(_) => walk_stmt(self, stmt),
            _ => self.off_spine(|v| walk_stmt(v, stmt)),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            // An L10N distribution's generated EXPORT hook calls
            // `$*LANG.define_slang(...)` instead of `use Slangify`. Read off
            // the AST, not the source text: a comment mentioning
            // `define_slang` must not make a module run at parse time.
            Expr::MethodCall { name, .. } | Expr::HyperMethodCall { name, .. }
                if name.resolve() == "define_slang" =>
            {
                self.out.defines_slang = true;
            }
            // `OUR::{'&infix:<@~~>'} := ...` in an export stash binds that
            // routine into the tag's export list like an `our sub` declared
            // there (Data::Record). A literal key names it; a computed one
            // (`OUR::{'&postfix:<' ~ $c ~ '>'}`) flags the module for a
            // parse-time probe instead (#9500).
            Expr::IndexAssign { target, index, .. }
                if matches!(target.as_ref(), Expr::PseudoStash(s) if s == "OUR::")
                    && is_export_stash_package(&self.package) =>
            {
                if let Expr::Literal(key) = index.as_ref() {
                    if let crate::value::ValueView::Str(key) = key.view()
                        && let Some(routine) = key.strip_prefix('&')
                        && !routine.is_empty()
                    {
                        self.export(routine.to_string());
                    }
                } else {
                    self.out.dynamic_export_stash = true;
                }
            }
            _ => {}
        }
        self.off_spine(|v| walk_expr(v, expr));
    }
}
