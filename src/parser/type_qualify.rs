//! A bare type name in a type position resolves where it is written
//! (ADR-12529 §2.1: a type resolves through the symbol table, at compile time
//! when the name is known).
//!
//! `class C { role R {}; method m(R $x) {} }` names the role `C::R`: the
//! constraint is written inside package `C`, whose `R` hides any outer `R`.
//! The run-time type checks used to bridge the gap by treating a qualified
//! type as the same type as any bare name equal to its last segment, which
//! also equated unrelated types (`class A {}; module Q { class A {} }; A.new ~~
//! Q::A` was `True`). This pass writes the qualified name into the AST
//! instead, for the type positions whose spelling the run time resolves by
//! name: a parameter's type, a typed variable or attribute, and a routine's
//! return type.
//!
//! The pass knows the `our`-scoped types the unit declares inside packages
//! (classes, roles, grammars, enums, subsets and packages, under their
//! composed names). A bare name in a type position inside package `P` takes
//! the innermost `P'::name` among `P` and its enclosing packages that the
//! unit declares; any other name is left alone, for the run time to resolve
//! (a top-level or imported type, a `my`-scoped one, a type capture).

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, ParamDef, Stmt};
use crate::ast_visit::{Visit, VisitMut, walk_expr_mut, walk_param_mut, walk_stmt, walk_stmt_mut};

/// Qualifies the bare type names of the unit `stmts` (see the module comment).
// Cost: O(n * d), n = size of the AST, d = package nesting depth.
pub(crate) fn qualify_program(stmts: &mut [Stmt]) {
    let mut decls = PackageTypes::default();
    for stmt in stmts.iter() {
        decls.visit_stmt(stmt);
    }
    if !decls.opens_package && !decls.block_use && !decls.shadows_core {
        return;
    }
    let mut pass = Qualify {
        declared: decls.declared,
        local_names: decls.local_names,
        packages: Vec::new(),
        scopes: Vec::new(),
        pinned: Vec::new(),
    };
    let later = later_core_types(stmts);
    pass.in_scope(stmts, |pass, stmts| {
        for (stmt, later) in stmts.iter_mut().zip(later) {
            pass.pinned.push(later);
            pass.visit_stmt_mut(stmt);
            pass.pinned.pop();
        }
    });
}

/// The package a statement opens: its name, or `None` for a `my`-scoped
/// one, whose nested types the run time stores under lexical names.
/// `unit` tells whether it extends over the rest of the enclosing list.
fn opened_package(stmt: &Stmt) -> Option<(Option<&str>, bool)> {
    match stmt {
        Stmt::ClassDecl {
            name,
            is_lexical,
            is_unit,
            ..
        } => Some(((!is_lexical).then(|| name.as_str()), *is_unit)),
        Stmt::RoleDecl {
            name, is_lexical, ..
        } => Some(((!is_lexical).then(|| name.as_str()), false)),
        Stmt::Package {
            name,
            is_unit,
            is_my,
            ..
        } => Some(((!is_my).then(|| name.as_str()), *is_unit)),
        // A class or package body's run-time statements, which the BEGIN
        // prologue moved out of its declaration; they run in its package.
        Stmt::PackageRuntimeBody { name, decl, .. } => {
            let my = matches!(
                decl,
                crate::ast::PackageRuntimeDecl::Class {
                    is_lexical: true,
                    ..
                }
            );
            Some(((!my).then(|| name.as_str()), false))
        }
        _ => None,
    }
}

/// The name a type declaration installs in its package, when it is
/// `our`-scoped: the opened packages above, an enum and a subset.
fn declared_type(stmt: &Stmt) -> Option<&str> {
    match stmt {
        Stmt::EnumDecl { name, is_my, .. } | Stmt::SubsetDecl { name, is_my, .. } => {
            (!is_my).then(|| name.as_str())
        }
        _ => opened_package(stmt).and_then(|(name, _)| name),
    }
}

/// The name of a `my`-scoped type declaration.
fn lexical_type(stmt: &Stmt) -> Option<&str> {
    match stmt {
        Stmt::ClassDecl {
            name,
            is_lexical: true,
            ..
        }
        | Stmt::EnumDecl {
            name, is_my: true, ..
        }
        | Stmt::SubsetDecl {
            name, is_my: true, ..
        }
        | Stmt::Package {
            name, is_my: true, ..
        } => Some(name.as_str()),
        Stmt::RoleDecl {
            name,
            is_lexical: true,
            ..
        } => Some(name.as_str()),
        _ => None,
    }
}

/// For each statement of `body`, the names of core types that a `my` type
/// declared at a later statement of `body` takes over.
// Cost: O(n), n = statements of `body`.
fn later_core_types(body: &[Stmt]) -> Vec<HashSet<String>> {
    let mut later = vec![HashSet::new(); body.len()];
    let mut seen_after: HashSet<String> = HashSet::new();
    for (i, stmt) in body.iter().enumerate().rev() {
        later[i] = seen_after.clone();
        // A traited declaration is wrapped with its metadata.
        let mut pending = vec![stmt];
        while let Some(stmt) = pending.pop() {
            match stmt {
                Stmt::SyntheticBlock(inner) => pending.extend(inner.iter()),
                _ => {
                    if let Some(name) = lexical_type(stmt)
                        && crate::builtin_types::catalog::builtin_type_info(name).is_some()
                    {
                        seen_after.insert(name.to_string());
                    }
                }
            }
        }
    }
    later
}

/// The stack of open packages, `None` for a `my`-scoped one.
type Packages<'a> = Vec<Option<&'a str>>;

/// The composed prefix of the open packages, or `None` when one of them is
/// `my`-scoped (or none is open).
fn prefix(packages: &[Option<&str>]) -> Option<String> {
    if packages.is_empty() {
        return None;
    }
    let mut parts = Vec::with_capacity(packages.len());
    for p in packages {
        parts.push((*p)?);
    }
    Some(parts.join("::"))
}

/// Collects the composed names of the `our`-scoped types declared inside a
/// package.
#[derive(Default)]
struct PackageTypes<'a> {
    declared: HashSet<String>,
    /// The short names the unit declares as something other than an
    /// `our`-scoped type: a `my` type, a constant or another sigil-less term.
    local_names: HashSet<String>,
    packages: Packages<'a>,
    opens_package: bool,
    /// A `use` below the unit's top level was seen.
    block_use: bool,
    /// A `my` type takes the name of a core type (`my class DateTime is
    /// DateTime`), so type positions before it must keep the core meaning.
    shadows_core: bool,
    depth: usize,
}

impl<'a> Visit<'a> for PackageTypes<'a> {
    fn visit_stmt(&mut self, stmt: &'a Stmt) {
        if let Some(prefix) = prefix(&self.packages)
            && let Some(name) = declared_type(stmt)
        {
            self.declared.insert(format!("{prefix}::{name}"));
        }
        if let Some(name) = lexical_type(stmt) {
            self.local_names.insert(name.to_string());
            self.shadows_core |= crate::builtin_types::catalog::builtin_type_info(name).is_some();
        }
        if let Stmt::VarDecl { name, .. } = stmt
            && name.starts_with(|c: char| c.is_alphabetic() || c == '_')
        {
            self.local_names.insert(name.clone());
        }
        self.block_use |= self.depth > 0 && matches!(stmt, Stmt::Use { .. });
        let opened = opened_package(stmt);
        self.opens_package |= opened.is_some();
        self.depth += 1;
        match opened {
            // A `unit` package holds the rest of the unit: it stays open.
            Some((name, true)) => {
                self.packages.push(name);
                walk_stmt(self, stmt);
            }
            Some((name, false)) => {
                self.packages.push(name);
                walk_stmt(self, stmt);
                self.packages.pop();
            }
            None => walk_stmt(self, stmt),
        }
        self.depth -= 1;
    }
}

struct Qualify<'d> {
    declared: HashSet<String>,
    local_names: HashSet<String>,
    packages: Vec<Option<&'d str>>,
    /// The types each open statement list binds lexically, by source name:
    /// the types a block's `use` imports, under their qualified names, and
    /// the `my` classes it declares inside a package, under the storage name
    /// the run time registers them by (`Pkg::Name\u{0}<decl-id>`, ADR-0047).
    scopes: Vec<HashMap<String, String>>,
    /// Per open statement list, the core-named `my` types declared at a
    /// LATER statement than the one being visited. A type position names the
    /// scope as it stands where it is written, so it keeps the core type and
    /// is spelled `CORE::Name` (the `my class` is not yet in scope there).
    pinned: Vec<HashSet<String>>,
}

impl Qualify<'_> {
    /// Runs `f` over `body` with the `my` classes it declares in scope.
    // Cost: O(b), b = statements of `body`, plus `f`.
    fn in_scope<B: ?Sized + AsRef<[Stmt]>>(
        &mut self,
        body: &mut B,
        f: impl FnOnce(&mut Self, &mut B),
    ) {
        let mut scope = HashMap::new();
        if let Some(prefix) = prefix(&self.packages) {
            let mut pending: Vec<&Stmt> = body.as_ref().iter().collect();
            while let Some(stmt) = pending.pop() {
                match stmt {
                    Stmt::ClassDecl {
                        name,
                        is_lexical: true,
                        decl_id,
                        ..
                    } if *decl_id != 0 => {
                        // A stub and its definition are one class: the
                        // stub's site names it.
                        scope
                            .entry(name.as_str().to_string())
                            .and_modify(|full: &mut String| {
                                *full = format!("{prefix}::{name}\u{0}{decl_id}")
                            })
                            .or_insert_with(|| format!("{prefix}::{name}\u{0}{decl_id}"));
                    }
                    // A traited declaration is wrapped with its metadata.
                    Stmt::SyntheticBlock(inner) => pending.extend(inner.iter().rev()),
                    _ => {}
                }
            }
        }
        self.scopes.push(scope);
        f(self, body);
        self.scopes.pop();
    }

    /// A `use` in a block binds the types it imports in the block's pad, for
    /// the statements after it (ADR-8751): a type name resolves at compile
    /// time, in source order. The unit's own top level is no block, and its
    /// imports stay the run time's.
    // Cost: O(t), t = the module's exported types.
    fn note_block_use(&mut self, stmt: &Stmt) {
        let Stmt::Use {
            module,
            arg: None,
            tags,
            ..
        } = stmt
        else {
            return;
        };
        let Some(types) = super::stmt::simple::scanned_exported_types(module) else {
            return;
        };
        let scope = self.scopes.last_mut().expect("an open scope");
        for ty in types.iter().filter(|ty| ty.imported_by(tags)) {
            scope.insert(ty.name.clone(), ty.qualified.clone());
        }
    }

    /// The type a block's `use` imports under the bare `name`, unless a
    /// nearer `my` class of that name hides it (whose storage name carries
    /// its declaration id).
    // Cost: O(s), s = open statement lists.
    fn resolve_import(&self, name: &str) -> Option<String> {
        let full = self.scopes.iter().rev().find_map(|scope| scope.get(name))?;
        (!full.contains('\u{0}')).then(|| full.clone())
    }

    /// The innermost declared `P::name` for the bare `name`, from the open
    /// packages outwards.
    // Cost: O(d * |name|), d = package nesting depth.
    fn resolve(&self, name: &str) -> Option<String> {
        if self.pinned.iter().any(|later| later.contains(name)) {
            return Some(format!("CORE::{name}"));
        }
        if let Some(full) = self.scopes.iter().rev().find_map(|scope| scope.get(name)) {
            return Some(full.clone());
        }
        for depth in (1..=self.packages.len()).rev() {
            let Some(prefix) = prefix(&self.packages[..depth]) else {
                continue;
            };
            let candidate = format!("{prefix}::{name}");
            // A type the unit declares, or one a `use`d module declared under
            // the package (`use URI::Path` inside `unit class URI`, where
            // `Path` names `URI::Path`). The parser's type registry does not
            // tell a type from a `my` type or a type-valued constant (`my
            // constant CArray is export = NativeCall::Types::CArray`), so a
            // name the unit declares as one of those is left alone. A
            // package's stash is consulted after GLOBAL, as in rakudo: a
            // `DS` role in GLOBAL wins over a loaded `P::DS` inside `P`.
            if self.declared.contains(&candidate)
                || (!self.local_names.contains(name)
                    // A core type (`Attribute`) is found in GLOBAL before the
                    // package's stash, so `use AttrX::Mooish::Attribute` inside
                    // `unit module AttrX::Mooish` leaves `Attribute` the core one.
                    && crate::builtin_types::catalog::builtin_type_info(name).is_none()
                    && !super::stmt::simple::is_user_declared_type(name)
                    && super::stmt::simple::is_user_declared_type(&candidate))
            {
                return Some(candidate);
            }
        }
        None
    }

    /// `spelling` with each bare type name qualified: the base name and the
    /// names in a parameterization or coercion (`R:D`, `Box[R]`, `R(Str)`).
    // Cost: O(s * d), s = length of the spelling.
    fn qualify(&self, spelling: &mut String) {
        self.qualify_with(spelling, Self::resolve);
    }

    /// [`Self::qualify`] by `resolve`.
    fn qualify_with(&self, spelling: &mut String, resolve: fn(&Self, &str) -> Option<String>) {
        let bytes = spelling.as_bytes();
        let mut out = String::with_capacity(spelling.len());
        let mut changed = false;
        let mut i = 0;
        while i < bytes.len() {
            let c = spelling[i..].chars().next().expect("a char boundary");
            if !(c.is_alphabetic() || c == '_') {
                out.push(c);
                i += c.len_utf8();
                continue;
            }
            // An identifier, `::`-joined parts included.
            let start = i;
            let mut end = i;
            loop {
                while let Some(ch) = spelling[end..].chars().next()
                    && (ch.is_alphanumeric() || matches!(ch, '_' | '-' | '\''))
                {
                    end += ch.len_utf8();
                }
                if spelling[end..].starts_with("::")
                    && spelling[end + 2..]
                        .chars()
                        .next()
                        .is_some_and(|ch| ch.is_alphabetic() || ch == '_')
                {
                    end += 2;
                    continue;
                }
                break;
            }
            let word = &spelling[start..end];
            // A smiley (`:D`) or a type capture (`::T`) follows a colon.
            let after_colon = start > 0 && bytes[start - 1] == b':';
            match (!after_colon && !word.contains("::"))
                .then(|| resolve(self, word))
                .flatten()
            {
                Some(q) => {
                    out.push_str(&q);
                    changed = true;
                }
                None => out.push_str(word),
            }
            i = end;
        }
        if changed {
            *spelling = out;
        }
    }

    fn qualify_opt(&self, spelling: &mut Option<String>) {
        if let Some(s) = spelling {
            self.qualify(s);
        }
    }
}

impl VisitMut for Qualify<'_> {
    fn visit_stmts_mut(&mut self, body: &mut Vec<Stmt>) {
        let later = later_core_types(body);
        self.in_scope(body, |pass, body| {
            for (stmt, later) in body.iter_mut().zip(later) {
                pass.note_block_use(stmt);
                pass.pinned.push(later);
                pass.visit_stmt_mut(stmt);
                pass.pinned.pop();
            }
        });
    }

    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        match stmt {
            Stmt::VarDecl {
                type_constraint, ..
            }
            | Stmt::HasDecl {
                type_constraint, ..
            } => self.qualify_opt(type_constraint),
            // A class's parents and roles a block's `use` imports.
            Stmt::ClassDecl {
                parents,
                does_parents,
                ..
            } => {
                for parent in parents.iter_mut().chain(does_parents.iter_mut()) {
                    self.qualify_with(parent, Self::resolve_import);
                }
            }
            Stmt::DoesDecl { name, .. } => {
                let mut spelling = name.as_str().to_string();
                self.qualify_with(&mut spelling, Self::resolve_import);
                if spelling != name.as_str() {
                    *name = crate::symbol::Symbol::intern(&spelling);
                }
            }
            Stmt::SubDecl { return_type, .. }
            | Stmt::MethodDecl { return_type, .. }
            | Stmt::ProtoDecl { return_type, .. } => self.qualify_opt(return_type),
            _ => {}
        }
        // The open package's name outlives the borrow of `stmt`: the stack
        // holds names leaked into the symbol table's `'static` strings.
        match opened_package(stmt).map(|(name, unit)| {
            (
                name.map(|n| crate::symbol::Symbol::intern(n).as_str()),
                unit,
            )
        }) {
            Some((name, true)) => {
                self.packages.push(name);
                walk_stmt_mut(self, stmt);
            }
            Some((name, false)) => {
                self.packages.push(name);
                walk_stmt_mut(self, stmt);
                self.packages.pop();
            }
            None => walk_stmt_mut(self, stmt),
        }
    }

    fn visit_expr_mut(&mut self, expr: &mut Expr) {
        if let Expr::AnonSubParams { return_type, .. } = expr {
            self.qualify_opt(return_type);
        }
        walk_expr_mut(self, expr);
    }

    fn visit_param_mut(&mut self, param: &mut ParamDef) {
        self.qualify_opt(&mut param.type_constraint);
        walk_param_mut(self, param);
    }
}
