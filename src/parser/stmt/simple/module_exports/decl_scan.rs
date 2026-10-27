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

use super::{InlineModuleExport, compose_type_name, is_export_stash_package, sub_export_entry};
use crate::ast::{Expr, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt, walk_stmts};
use std::collections::HashMap;

mod declare_keywords;
mod exported_type;
mod stash_expr;

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
    /// Built-in declarator keywords whose HOW `EXPORTHOW::SUPERSEDE` replaces.
    pub(super) supersede_keywords: Vec<String>,
    /// Whether the module calls `.define_slang` (an L10N distribution's
    /// generated EXPORT hook).
    pub(super) defines_slang: bool,
    /// Whether the module binds into an export stash under a key that is not
    /// a literal (`dynamic_stash.rs`, #9500).
    pub(super) dynamic_export_stash: bool,
    /// The `our` variables the module exports from its package spine
    /// (`exported_vars.rs`).
    pub(super) exported_vars: Vec<super::exported_vars::ExportedVar>,
    /// The types the module exports from its package spine
    /// (`exported_types.rs`).
    pub(super) exported_types: Vec<super::exported_types::ExportedType>,
    /// Whether the module exports a variable that is no `exported_vars` entry:
    /// an `our` declared directly in an `EXPORT::<tag>` stash (exported by
    /// construction), an `is export` one off the package spine, or an `our`
    /// variable or constant outside every package, which is GLOBAL's (a
    /// bare-file module's, one before `unit class`). Its exported variables
    /// are then not all known.
    pub(super) unlisted_exported_vars: bool,
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
    /// An exported qualified type is imported under its short name too
    /// (`class A::B::C is export { }` imports `C`).
    // Cost: O(t), t = export tags.
    fn header_exports(&mut self, decl: &Stmt, header: &crate::ast::package_header::Header) {
        let Some(tags) = &header.export_tags else {
            return;
        };
        let (Stmt::ClassDecl { name, .. }
        | Stmt::RoleDecl { name, .. }
        | Stmt::Package { name, .. }) = decl
        else {
            return;
        };
        let name = name.resolve();
        if let Some((_, short)) = name.rsplit_once("::") {
            self.out.type_names.push(short.to_string());
        }
        let lexical = matches!(
            decl,
            Stmt::ClassDecl {
                is_lexical: true,
                ..
            }
        ) || matches!(decl, Stmt::Package { is_my: true, .. });
        if !lexical {
            self.export_type(&name, tags);
        }
    }

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

    /// An `is export` routine installs it in its package's `EXPORT::DEFAULT`
    /// and `EXPORT::ALL` stashes, which a file that loaded the module names as
    /// packages (`need M; M::EXPORT::DEFAULT.WHO`).
    fn export_stash_types(&mut self) {
        if self.prefix.is_empty() {
            return;
        }
        let export = compose_type_name(&self.prefix, "EXPORT");
        for stash in ["DEFAULT", "ALL"] {
            let name = compose_type_name(&export, stash);
            self.push_type(name.clone(), name, false);
        }
        self.push_type(export.clone(), export, false);
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

    fn collect_enum_values(
        &mut self,
        variants: &[(String, Option<Expr>)],
        is_export: bool,
        export_tags: &[String],
    ) {
        let names: Vec<String> = crate::parser::enum_value_names(variants)
            .into_iter()
            .filter(|name| !name.is_empty())
            .collect();
        if is_export && !super::enum_values::exported_by_default(export_tags) {
            self.out
                .tagged_enum_values
                .extend(names.into_iter().map(|n| (n, export_tags.to_vec())));
        } else {
            self.out.enum_values.extend(names);
        }
    }
}

/// `name` as the importer can write it: a `GLOBAL::` prefix is the root
/// namespace every spelling already starts from (`class GLOBAL::X` is `X`).
fn absolute_name(name: &str) -> String {
    name.strip_prefix("GLOBAL::")
        .filter(|rest| !rest.is_empty())
        .unwrap_or(name)
        .to_string()
}

/// The full name of a type declared as `name` inside the package path
/// `prefix`: a `GLOBAL::`-qualified name is absolute, not nested.
fn package_type_name(prefix: &str, name: &str) -> String {
    match name.strip_prefix("GLOBAL::") {
        Some(rest) if !rest.is_empty() => rest.to_string(),
        _ => compose_type_name(prefix, name),
    }
}

impl<'ast> Visit<'ast> for DeclScan {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::ClassDecl {
                name,
                is_lexical,
                is_unit,
                export_tags,
                ..
            } => {
                let name = name.resolve();
                // An anonymous class has no name to import, and opens no
                // package path of its own.
                if name.starts_with("__ANON_") {
                    return self.enter_package(stmt, self.prefix.clone(), false, String::new());
                }
                let composed = package_type_name(&self.prefix, &name);
                // `is export` reaches the importer from any depth, a lexical
                // `my class` in a routine or block included, and under the
                // class's short name (`class A::B::C is export` imports `C`).
                let exported = export_tags.is_some();
                if exported && let Some((_, short)) = name.rsplit_once("::") {
                    self.out.type_names.push(short.to_string());
                }
                if self.spine || !*is_lexical || exported {
                    self.push_type(composed.clone(), absolute_name(&name), false);
                }
                self.enter_package(stmt, composed, *is_unit, String::new());
            }
            Stmt::RoleDecl {
                name,
                is_export,
                export_tags,
                is_lexical,
                ..
            } => {
                let name = name.resolve();
                if name.starts_with("__ANON_") {
                    return self.enter_package(stmt, self.prefix.clone(), false, String::new());
                }
                if *is_export && !*is_lexical {
                    self.export_type(&name, export_tags);
                }
                let composed = package_type_name(&self.prefix, &name);
                // `role A::B is export` imports `B`, as a class does.
                if *is_export && let Some((_, short)) = name.rsplit_once("::") {
                    self.out.type_names.push(short.to_string());
                }
                self.push_type(composed.clone(), absolute_name(&name), false);
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
            // A subset is a type name the importer can write as a bare `when`
            // matcher (`when Base64Binary { }`, from FHIR::Base), so it travels
            // with the module exactly like a class does.
            Stmt::SubsetDecl {
                name,
                is_my,
                is_export,
                export_tags,
                ..
            } => {
                if self.spine || !*is_my {
                    let name = name.resolve();
                    self.push_type(compose_type_name(&self.prefix, &name), name, false);
                }
                if *is_export && !*is_my {
                    self.export_type(&name.resolve(), export_tags);
                }
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
                if *is_export && !*is_my {
                    self.export_type(&name.resolve(), export_tags);
                }
                // `is export` imports a qualified enum under its short name, as
                // it does a class (`my enum M::Endian is export` makes
                // `Endian::Vax` the imported value).
                if *is_export && let Some((_, short)) = name.resolve().rsplit_once("::") {
                    self.out.enum_type_names.push(short.to_string());
                    self.out.type_names.push(short.to_string());
                }
                if self.spine || *is_export {
                    self.collect_enum_values(variants, *is_export, export_tags);
                }
                // An `our` enum's values are also installed in the enclosing
                // package, so `Pkg::Value` resolves globally, export or not
                // (`package WS::Msg { enum Opcode is export (:Ping(9)) }` makes
                // `WS::Msg::Ping` visible to any file that loaded the module).
                // An `our` enum's values are also reachable qualified by the
                // enum's own name (`enum DepsShort::LC <New>` makes
                // `DepsShort::LC::New` resolve in any file that loaded it).
                if !*is_my {
                    let enum_name = compose_type_name(&self.prefix, &name.resolve());
                    let mut packages = vec![absolute_name(&enum_name)];
                    if !self.prefix.is_empty() {
                        packages.push(self.prefix.clone());
                    }
                    for package in packages {
                        self.out.enum_values.extend(
                            crate::parser::enum_value_names(variants)
                                .iter()
                                .filter(|value| !value.is_empty())
                                .map(|value| compose_type_name(&package, value)),
                        );
                    }
                }
                self.off_spine(|v| walk_stmt(v, stmt));
            }
            Stmt::VarDecl {
                name,
                is_export,
                is_our,
                export_tags,
                constant,
                ..
            } => {
                // An exported variable is the package variable of the package
                // it is declared in; an exported constant may be `my`-scoped.
                // Only one on the package spine is known to be declared when
                // the module loads.
                let is_constant = constant.is_some();
                if *is_our && is_export_stash_package(&self.package) && !name.starts_with('&') {
                    self.out.unlisted_exported_vars = true;
                }
                // An `our` variable or constant outside every package (a
                // bare-file module's, one before `unit class`) is a GLOBAL
                // one, which the importer sees as well.
                if (*is_our || is_constant)
                    && !name.is_empty()
                    && !name.starts_with('&')
                    && (self.prefix.is_empty() || (*is_export && !self.spine))
                {
                    self.out.unlisted_exported_vars = true;
                }
                if *is_export
                    && (*is_our || is_constant)
                    && self.spine
                    && !name.is_empty()
                    && !name.starts_with('&')
                {
                    // Outside every package the variable is a `GLOBAL` one.
                    let package = if self.prefix.is_empty() {
                        "GLOBAL".to_string()
                    } else {
                        self.prefix.clone()
                    };
                    self.out
                        .exported_vars
                        .push(super::exported_vars::ExportedVar {
                            name: name.clone(),
                            package,
                            tags: export_tags.clone(),
                            term: crate::term_names::is_term_constant_decl(name, *constant),
                            hook: false,
                        });
                }
                // Sigiled constants (`constant $x = 1`) are not barewords.
                if constant.is_some() && !name.is_empty() && !name.starts_with(['$', '@', '%', '&'])
                {
                    if self.spine || *is_export {
                        self.out.constant_names.push(name.clone());
                    }
                    // An `our` constant is installed in its package, so
                    // `Pkg::NAME` resolves from any file that loaded the
                    // module, export or not.
                    if *is_our && !self.prefix.is_empty() {
                        self.out
                            .constant_names
                            .push(compose_type_name(&self.prefix, name));
                    }
                }
                // `our &infix:<op> is export = &[other];` (PatternMatching's
                // `┇` alias) exports a routine under the same `&name` a
                // `sub name is export` would.
                if *is_export && name.len() > 1 && name.starts_with('&') {
                    self.export(literal_marked_name(&name[1..]));
                }
                self.off_spine(|v| walk_stmt(v, stmt));
            }
            Stmt::SubDecl {
                name,
                is_export,
                associativity,
                precedence_trait,
                is_test_assertion,
                scope,
                ..
            } => {
                // Every `is export` sub is collected, whatever tag it carries:
                // the set answers only "is `name` a routine" for the
                // importer's parse (ADR-0087, #7939); run-time import honours
                // the tags. An `our` sub declared directly in a module's own
                // `EXPORT::<tag>` stash is exported by construction
                // (Net::IP::Parse's `our sub infix:<< ip== >>`).
                if *is_export
                    || (is_export_stash_package(&self.package)
                        && *scope == crate::ast::SubScope::Our)
                {
                    let entry = sub_export_entry(
                        name.resolve(),
                        precedence_trait.as_ref(),
                        associativity.clone(),
                        *is_test_assertion,
                    );
                    self.out.exports.insert(entry.name.clone(), entry);
                    if *is_export {
                        self.export_stash_types();
                    }
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
            Stmt::Block(_) | Stmt::SyntheticBlock(_) => {
                if let Some((decl, header)) = crate::ast::package_header::unwrap(stmt) {
                    self.header_exports(decl, &header);
                }
                walk_stmt(self, stmt)
            }
            // The same declaration a `.AST` parse keeps as written.
            Stmt::Ast(node)
                if matches!(
                    node.as_ref(),
                    crate::ast::parsed::ParsedStmt::PackageHeader { .. }
                ) =>
            {
                if let crate::ast::parsed::ParsedStmt::PackageHeader { decl, header, .. } =
                    node.as_ref()
                {
                    self.header_exports(decl, header);
                }
                walk_stmt(self, stmt)
            }
            _ => self.off_spine(|v| walk_stmt(v, stmt)),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        self.scan_expr(expr);
        self.off_spine(|v| walk_expr(v, expr));
    }
}

/// An operator name whose `(...)` adverb value is a single string literal
/// (`&infix:["\c[DOUBLE PLUS]"]`) spelled out, as the compiler resolves it at
/// BEGIN time. The scan has no constant environment, so any other marked value
/// keeps its sentinel spelling.
// Cost: O(n), n = length of the name.
fn literal_marked_name(name: &str) -> String {
    use crate::adverb_name::INTERP_MARK;
    if !crate::adverb_name::needs_interp(name) {
        return name.to_string();
    }
    let mut out = String::with_capacity(name.len());
    let mut rest = name;
    while let Some(open) = rest.find(INTERP_MARK) {
        out.push_str(&rest[..open]);
        let after = &rest[open + INTERP_MARK.len_utf8()..];
        let Some(close) = after.find(INTERP_MARK) else {
            return name.to_string();
        };
        let marked = &after[..close];
        let literal = marked
            .strip_prefix('(')
            .and_then(|m| m.strip_suffix(')'))
            .and_then(|content| crate::parser::parse_fragment_spelled(content).ok())
            .and_then(|(stmts, _)| match stmts.as_slice() {
                [Stmt::Expr(e)] | [Stmt::SetLine(..), Stmt::Expr(e)] => match e.peel_parens() {
                    Expr::Literal(v) => Some(v.to_string_value()),
                    _ => None,
                },
                _ => None,
            });
        let Some(text) = literal else {
            return name.to_string();
        };
        out.push('<');
        out.push_str(&text);
        out.push('>');
        rest = &after[close + INTERP_MARK.len_utf8()..];
    }
    out.push_str(rest);
    out
}
