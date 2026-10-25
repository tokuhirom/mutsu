//! Types, packages and imports declared ahead of a lifted BEGIN (ADR-0134,
//! slice 2; #10394).
//!
//! A BEGIN is lifted to run in the unit prologue, before the scope it is
//! written in has been entered. A type, package or import that scope declares
//! ahead of the BEGIN does not exist there yet.
//!
//! - **An import** (`use Foo`, `need Foo`, `import Foo`) can bring in any name,
//!   operators included, so no scan of the body can say which ones it uses. The
//!   lifted body's block for that scope repeats the import. The module is loaded
//!   once either way, and the import binds the same exported objects, so this is
//!   what the body sees in place. Rakudo performs the import at BEGIN time too.
//! - **A type or package** is a single object, created once at BEGIN time, so
//!   it cannot simply be declared again. Most BEGINs do not name it at all, and
//!   whether one does is decided on the AST: the typed visitor
//!   ([`crate::ast_visit`]) reports every name the body and the routines it
//!   copies mention, in every position (a type constraint, a qualified name, a
//!   parameter's type, source text compiled later), and never a string
//!   literal's content. A BEGIN that names none of the scope's types is lifted
//!   without them.
//! - **A BEGIN that does name one** gets the declaration repeated in its block,
//!   when repeating it is unobservable: the body holds declarations only (no
//!   statement that runs, no trait that runs code, no BEGIN of its own), and it
//!   reads nothing of the inner scopes. A `my` type is stored under its
//!   declaration site (ADR-0047), so the repeat registers the same type the
//!   scope declares in place, as each call of the scope already does. Any other
//!   named type keeps the BEGIN on its pre-ADR path, and so does a BEGIN that
//!   may change the type it names through its metaobject (`.^add_method`):
//!   the scope's own declaration registers the type afresh, which would lose
//!   the change.
//!
//! Names reached dynamically (`EVAL`, `::($name)`, a pseudo-package) are not
//! visible to the scan, so a body that uses one names every type. An `EVAL` may
//! also change a type through its metaobject, so a body that uses one is not
//! lifted from a scope that declares a type. Nor is a body that calls a routine
//! it does not know (which may evaluate a string where it is called from), as
//! for a scope that declares a routine ([`super::routines`]).

use super::pragmas::{Guard, Repeat};
use super::routines::{Access, Dependencies, FrameBlock};
pub(super) use super::mentions::Mentions;
use super::{BindingKind, Walker};
use crate::ast::{Expr, Stmt};
use crate::ast_visit::{VisitMut, walk_stmt_mut};
use std::collections::{BTreeMap, BTreeSet};

/// A type or package declared in an inner scope.
pub(super) struct TypeDecl {
    /// The names it declares, as the code that refers to it spells them: the
    /// type's own name, and an enum's keys.
    names: Vec<String>,
    /// The declaration, repeated in a lifted body's block when the body names
    /// it. `None` when repeating it would be observable.
    copy: Option<Stmt>,
}

/// The names a type or package declaration installs, or `None` when they are
/// not known statically (`class ::($name)`, a `unit` declarator).
pub(super) fn declared_names(decl: &Stmt) -> Option<Vec<String>> {
    let name = match decl {
        Stmt::ClassDecl {
            name,
            name_expr: None,
            is_unit: false,
            ..
        }
        | Stmt::RoleDecl { name, .. }
        | Stmt::SubsetDecl { name, .. }
        | Stmt::Package {
            name,
            is_unit: false,
            ..
        } => name,
        Stmt::EnumDecl { name, variants, .. } => {
            let mut names = vec![name.resolve()];
            names.extend(variants.iter().map(|(key, _)| key.clone()));
            return Some(names);
        }
        _ => return None,
    };
    Some(vec![
        name.resolve().trim_start_matches("GLOBAL::").to_string(),
    ])
}

/// Whether running `decl` once more, in the prologue, is unobservable: it only
/// declares, and runs no code of its own while doing so.
fn is_pure_declaration(decl: &Stmt) -> bool {
    match decl {
        Stmt::ClassDecl {
            body,
            custom_traits,
            ..
        }
        | Stmt::RoleDecl {
            body,
            custom_traits,
            ..
        } => !has_user_trait(custom_traits) && body.iter().all(is_pure_member),
        Stmt::Package { body, .. } => body.iter().all(is_pure_member),
        // A `where` predicate is a closure, run only by a type check.
        Stmt::SubsetDecl { .. } => true,
        Stmt::EnumDecl { variants, .. } => variants
            .iter()
            .all(|(_, value)| value.as_ref().is_none_or(|v| matches!(v, Expr::Literal(_)))),
        _ => false,
    }
}

/// What the prologue repeats for `decl`: the declaration itself when running
/// it once more is unobservable (a role's body runs only when it is composed),
/// otherwise (a class whose body runs code) its
/// declaration-only shell. The shell registers the same lexical type as the
/// in-place declaration (they share a `decl_id`, hence a storage name), which
/// later runs the body for real (#10471). `None` when neither is possible.
// Cost: O(n), n = size of the declaration's body.
fn declaration_copy(decl: &Stmt) -> Option<Stmt> {
    if is_pure_declaration(decl) {
        return Some(decl.clone());
    }
    // A role's body runs when the role is composed, not when it is declared,
    // so declaring it once more runs none of it.
    if let Stmt::RoleDecl { custom_traits, .. } = decl {
        return (!has_user_trait(custom_traits)).then(|| decl.clone());
    }
    if !matches!(decl, Stmt::ClassDecl { .. }) {
        return None;
    }
    let mut shell = decl.clone();
    let mut shelling = Shelling { possible: true };
    shelling.visit_stmt_mut(&mut shell);
    shelling.possible.then_some(shell)
}

/// Reduces a class declaration to its declaration-only shell
/// ([`declaration_copy`]): the run-time statements of its body are dropped,
/// and a nested class is reduced the same way. A nested role is kept whole (its
/// body runs only when it is composed). The shell is not `possible` when a
/// class has a user trait or a nested type that can be neither.
struct Shelling {
    possible: bool,
}

impl VisitMut for Shelling {
    // Cost: O(n), n = size of the declaration's body.
    fn visit_stmt_mut(&mut self, stmt: &mut Stmt) {
        let Stmt::ClassDecl {
            body,
            custom_traits,
            ..
        } = stmt
        else {
            return;
        };
        if has_user_trait(custom_traits) {
            self.possible = false;
            return;
        }
        body.retain(|member| {
            if is_pure_member(member) {
                return true;
            }
            match member {
                Stmt::ClassDecl { .. } => true,
                Stmt::RoleDecl { custom_traits, .. } => {
                    self.possible &= !has_user_trait(custom_traits);
                    true
                }
                // Any other nested type that runs code is not shelled: the
                // scope is left to the in-place handling.
                _ if declared_names(member).is_some() => {
                    self.possible = false;
                    true
                }
                // A run-time statement of the body runs in place.
                _ => false,
            }
        });
        // Reduce the nested classes the same way.
        walk_stmt_mut(self, stmt);
    }

    /// An expression of a kept member (an attribute default, a trait
    /// argument) is kept as written.
    fn visit_expr_mut(&mut self, _expr: &mut Expr) {}
}

/// A trait a user declared (`is foo`), which runs a `trait_mod` when the
/// declaration is composed. The parser's own markers start with `__`.
fn has_user_trait(traits: &[(String, Option<Expr>)]) -> bool {
    traits.iter().any(|(t, _)| !t.starts_with("__"))
}

/// A member of a type or package body that only declares.
fn is_pure_member(stmt: &Stmt) -> bool {
    match stmt {
        // A `use trace` hook prints; it declares nothing and runs no user code.
        Stmt::SetLine(..)
        | Stmt::Trace { .. }
        | Stmt::DoesDecl { .. }
        | Stmt::TrustsDecl { .. }
        | Stmt::TokenDecl { .. }
        | Stmt::RuleDecl { .. }
        | Stmt::ProtoToken { .. } => true,
        // An attribute's default is a thunk, run by `.new`; a class-level
        // `has my $x = ...` / `has our $x = ...` runs its initializer at once.
        Stmt::HasDecl {
            default,
            is_my,
            is_our,
            ..
        } => default.is_none() || !(*is_my || *is_our),
        Stmt::MethodDecl { custom_traits, .. } | Stmt::SubDecl { custom_traits, .. } => {
            !has_user_trait(custom_traits)
        }
        Stmt::SyntheticBlock(inner) => inner.iter().all(is_pure_member),
        // The written-traits record (`ast::routine_trait`) is no code at all.
        stmt if stmt.is_routine_traits() => true,
        other => declared_names(other).is_some() && is_pure_declaration(other),
    }
}

/// An import an inner scope's lifted body repeats. A lowercase pragma is not
/// one: whether it can be repeated depends on the pragma
/// ([`super::pragmas`]).
fn is_copyable_import(stmt: &Stmt) -> bool {
    let module = match stmt {
        Stmt::Use {
            module,
            condition: None,
            ..
        }
        | Stmt::Need { module }
        | Stmt::Import { module, .. } => module,
        _ => return false,
    };
    !module.starts_with(|c: char| c.is_ascii_lowercase())
}

impl Walker<'_> {
    /// Note a type, package, import or pragma declared in the current scope.
    /// One the prologue cannot supply (most pragmas, `class ::($name)`) blocks
    /// the scope ([`super::pragmas`]).
    pub(super) fn declare_type_or_import(&mut self, decl: &Stmt) {
        let Some(frame) = self.frames.last_mut() else {
            return;
        };
        if is_copyable_import(decl) {
            frame.imports.push(decl.clone());
            return;
        }
        if super::pragmas::is_pragma(decl) {
            let guard = |variables: bool| Guard {
                bindings: variables.then_some(frame.bindings.len()),
                routines: frame.routines.len(),
                types: frame.types.len(),
            };
            match super::pragmas::repeat_of(decl) {
                Some(Repeat::Anywhere) => {}
                Some(Repeat::BeforeRoutines) => frame.pragma_guards.push(guard(false)),
                Some(Repeat::BeforeDeclarations) => frame.pragma_guards.push(guard(true)),
                None => {
                    frame.blocked = true;
                    return;
                }
            }
            frame.imports.push(decl.clone());
            return;
        }
        let Some(names) = declared_names(decl) else {
            frame.blocked = true;
            return;
        };
        let mentions = Mentions::of([decl]);
        // The repeat runs in the lifted body's block, which declares only what
        // the body itself reads. A type whose methods read an inner lexical or
        // call an inner routine would not find it there.
        let reads_inner_scope = self.frames.iter().any(|f| {
            f.bindings
                .iter()
                .any(|b| mentions.names(b.name.trim_start_matches(['$', '@', '%', '&'])))
                || f.routines.iter().any(|r| mentions.names(&r.name))
        });
        let copy = (!mentions.has_begin && !reads_inner_scope)
            .then(|| declaration_copy(decl))
            .flatten();
        let frame = self.frames.last_mut().expect("checked above");
        frame.types.push(TypeDecl { names, copy });
    }

    /// Note that the current scope declares a type a lifted body cannot be
    /// given: it has no copy ([`TypeDecl`]), so a body that names it is not
    /// lifted.
    pub(super) fn declare_unavailable_type(&mut self, names: Vec<String>) {
        if let Some(frame) = self.frames.last_mut() {
            frame.types.push(TypeDecl { names, copy: None });
        }
    }

    /// Whether a call of `name` reaches a type, enum key or package an inner
    /// scope declares: a coercion (`K(...)`) or a routine qualified by it
    /// (`P::x()`). The body then names it, and gets it.
    pub(super) fn calls_into_inner_type(&self, name: &str) -> bool {
        self.frames.iter().flat_map(|f| &f.types).any(|t| {
            t.names.iter().any(|n| {
                crate::qualified::package_ancestors(crate::symbol::Symbol::intern(name))
                    .any(|pkg| pkg.as_str() == n.as_str())
            })
        })
    }

    /// Whether the static declaration `static_decl` of an inner variable names
    /// a type or package an inner scope declared (`my K $v`), which the
    /// variable's static cell, declared at the unit's level, cannot.
    // Cost: O(n * t), n = the names `static_decl` mentions, t = the names the
    // inner scopes declare.
    pub(super) fn cell_names_inner_type(&self, static_decl: &Stmt) -> bool {
        self.names_inner_type(&Mentions::of([static_decl]))
    }

    /// Whether `mentions` names a type or package an inner scope declared.
    fn names_inner_type(&self, mentions: &Mentions) -> bool {
        self.frames
            .iter()
            .flat_map(|f| &f.types)
            .any(|t| t.names.iter().any(|n| mentions.names(n)))
    }

    /// Add the imports and types the lifted `body` needs to its frame blocks.
    /// `deps` are the routines and inner lexicals it was resolved to need.
    /// `None` when it names a type it cannot be given.
    pub(super) fn add_declarations(
        &self,
        body: &[Stmt],
        deps: &Dependencies,
        blocks: &mut BTreeMap<usize, FrameBlock>,
    ) -> Option<()> {
        for (index, frame) in self.frames.iter().enumerate() {
            if !frame.imports.is_empty() {
                blocks.entry(index).or_default().imports = frame.imports.clone();
            }
        }
        if self.frames.iter().all(|f| f.types.is_empty()) {
            return self.check_pragma_guards(deps, &BTreeSet::new());
        }
        let mut stmts: Vec<&Stmt> = body.iter().collect();
        stmts.extend(self.routine_decls(deps));
        for (&(frame, binding), access) in &deps.bindings {
            match (access, &self.frames[frame].bindings[binding].kind) {
                (Access::CopyIn(decl), _) => stmts.push(decl),
                // The copy-in declaration repeats the variable's type
                // constraint, so a type of the scope it names is repeated too.
                // (The cell itself is declared at the unit's level, where no
                // inner type exists, so it is left untyped:
                // [`Walker::cell_names_inner_type`].)
                (Access::Cell, BindingKind::Local { static_decl, .. }) => stmts.push(static_decl),
                (Access::Cell, _) => {}
            }
        }
        let types = self.add_types(Mentions::of(stmts), blocks)?;
        self.check_pragma_guards(deps, &types)
    }

    /// Whether the block of each scope can repeat its pragmas: none of them
    /// precedes a declaration the block copies (`deps` and the selected
    /// `types`) that it would affect ([`Guard`]).
    fn check_pragma_guards(
        &self,
        deps: &Dependencies,
        types: &BTreeSet<(usize, usize)>,
    ) -> Option<()> {
        let precedes = |frame: usize, guard: &Guard| {
            deps.bindings
                .keys()
                .any(|&(f, b)| f == frame && guard.bindings.is_some_and(|n| b < n))
                || deps
                    .routines
                    .iter()
                    .any(|&(f, r)| f == frame && r < guard.routines)
                || types.iter().any(|&(f, t)| f == frame && t < guard.types)
        };
        let violated = self
            .frames
            .iter()
            .enumerate()
            .any(|(frame, f)| f.pragma_guards.iter().any(|guard| precedes(frame, guard)));
        (!violated).then_some(())
    }

    /// Add the types `mentions` names to their frame blocks, with the ones
    /// those name in turn. Returns them, by frame and position.
    fn add_types(
        &self,
        mut mentions: Mentions,
        blocks: &mut BTreeMap<usize, FrameBlock>,
    ) -> Option<BTreeSet<(usize, usize)>> {
        // A repeated type may itself name an earlier one (`is Base`), so the
        // selection runs until nothing new is named.
        let mut selected = BTreeSet::new();
        loop {
            let mut added = Vec::new();
            for (f, frame) in self.frames.iter().enumerate() {
                for (t, ty) in frame.types.iter().enumerate() {
                    if selected.contains(&(f, t)) || !ty.names.iter().any(|n| mentions.names(n)) {
                        continue;
                    }
                    added.push((f, t, ty.copy.as_ref()?));
                }
            }
            if added.is_empty() {
                break;
            }
            for (f, t, decl) in added {
                selected.insert((f, t));
                let extra = Mentions::of([decl]);
                mentions.names.extend(extra.names);
                mentions.symbolic |= extra.symbolic;
            }
        }
        // The scope's own declaration registers the type afresh when the
        // scope runs, so a change the BEGIN makes to the repeat would be lost
        // (`BEGIN K.^add_method(...)`).
        if !selected.is_empty() && mentions.changes_type {
            return None;
        }
        for &(f, t) in &selected {
            let decl = self.frames[f].types[t]
                .copy
                .clone()
                .expect("selected above");
            blocks.entry(f).or_default().types.push(decl);
        }
        Some(selected)
    }
}
