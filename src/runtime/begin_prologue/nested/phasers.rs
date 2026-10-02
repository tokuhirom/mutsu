//! INIT and CHECK phasers that read a lexical of the routine they are written
//! in (ADR-0134 §7, #10562).
//!
//! Rakudo runs an `INIT` once, at the start of the program, and a `CHECK` once,
//! at the end of compilation, wherever they are written. Both see the lexicals
//! of the scopes around them in their static state, and what they store there
//! is what every frame of that scope starts from:
//!
//! ```raku
//! sub f { my $z; INIT $z = 5; $z }   # f() is 5
//! ```
//!
//! mutsu lifts such a phaser out of its routine, to the unit's own sequence of
//! `INIT` (source order) or `CHECK` (reverse order) phasers. The lifted body
//! then names `$z` where no `$z` is declared. This module gives the phaser the
//! static cell that a nested `BEGIN` gets (see [`super`]): the lifted body runs
//! in a block that declares the name from the cell and copies it back, and the
//! routine's own declaration starts from the cell on every frame entry. The
//! cell is declared at the unit's level, ahead of both.
//!
//! The only difference from a `BEGIN` is where the lifted body goes and when it
//! runs: into the unit's `INIT`/`CHECK` sequence rather than into the prologue.
//! A phaser that reads nothing of an inner scope is not lifted here: the
//! per-level reordering (`runtime::phasers`) or [`super::super::package_phasers`]
//! already runs it at the right time.
//!
//! A method's phaser is lifted the same way. The class it is written in has to
//! be composed before the phaser runs, and the phaser may read the class body's
//! `my` lexicals, so the lifted body runs inside a [`Stmt::PackageRuntimeBody`]
//! of each enclosing package, as in `package_phasers`. That keeps to a class or
//! package at the unit's top level (or nested in one). A phaser that reads
//! `self`, an attribute or a name it cannot see statically stays where it is.
//!
//! A type a phaser cannot re-enter is walked without a package: a role (it has
//! no store, and runs once per composition), and a class declared inside code,
//! which does not exist at the unit's level until that code runs (#10645). A
//! phaser lifted from one of them runs in no package.
//!
//! - A role's body declares nothing a lifted phaser can reach, so a phaser that
//!   reads one of its names, or takes a role parameter, stays where it is.
//! - A class declared inside code runs its body each time the code runs, so its
//!   body is a scope like a routine's (#10711): its lexicals get static cells and
//!   its routines are copied into the lifted body. Every phaser in it, in the
//!   body or in a method, is lifted, because the per-level handling runs it when
//!   the code runs the declaration, and never if that code never runs. A phaser
//!   that names the class itself (which does not exist at the unit's level) stays
//!   where it is.

use super::routines::{Dependencies, FrameBlock};
use super::{Binding, BindingKind, Frame, Walker};
use crate::ast::{Expr, PhaserKind, Stmt};
use crate::ast_visit::{Visit, walk_expr, walk_stmt};
use std::collections::{BTreeMap, HashSet};

/// The label that makes a CHECK's value-form block report its errors as
/// compile-time ones.
// Cost: O(1).
pub(super) fn check_label(kind: &PhaserKind) -> Option<String> {
    (*kind == PhaserKind::Check).then(|| "__mutsu_check_phaser__".to_string())
}

/// Whether a phaser body that resolved to `deps` and `blocks` reads anything
/// of the scopes it is written in. A phaser that does not is not lifted here.
// Cost: O(b), b = the frames the body reads from.
pub(super) fn needs_scope(deps: &Dependencies, blocks: &BTreeMap<usize, FrameBlock>) -> bool {
    !deps.bindings.is_empty()
        || !deps.routines.is_empty()
        || blocks.values().any(|block| !block.types.is_empty())
}

impl Walker<'_> {
    /// Whether some scope around the current statement is a package or role
    /// body.
    // Cost: O(d), d = the nesting depth of the scopes around the statement.
    pub(super) fn in_package(&self) -> bool {
        self.frames
            .iter()
            .any(|f| f.package.is_some() || f.role.is_some())
    }

    /// Whether the innermost scope is the body of a package a lifted phaser
    /// re-enters, which reaches the package's own routines by name.
    pub(super) fn directly_in_real_package(&self) -> bool {
        self.frames.last().is_some_and(|f| f.package.is_some())
    }

    /// Whether some scope around the current statement is the body of a type
    /// the phaser cannot re-enter ([`Walker::walk_detached`]). Every phaser
    /// lifted from one runs at the unit's level, where the type's own
    /// declaration has not run.
    pub(super) fn in_detached_type(&self) -> bool {
        self.frames.iter().any(|f| f.role.is_some())
    }

    /// The scope a `BEGIN` written here is a member of, when that is the body
    /// of a package a declaration runs: the innermost package, role or class
    /// declared in code around the statement, if it is a package.
    // Cost: O(d), d = the nesting depth of the scopes around the statement.
    pub(super) fn innermost_package_frame(&self) -> Option<usize> {
        let frame = self
            .frames
            .iter()
            .rposition(|f| f.package.is_some() || f.role.is_some())?;
        self.frames[frame].package.is_some().then_some(frame)
    }

    /// Whether the innermost scope is a package or role body, so a method
    /// declared now is a member of it.
    pub(super) fn directly_in_package(&self) -> bool {
        self.frames
            .last()
            .is_some_and(|f| f.package.is_some() || f.role.is_some())
    }

    /// Whether the `INIT`/`CHECK` body may be lifted at all. It must be
    /// written in a scope (a phaser at the unit's top level is the unit's own),
    /// hold no phaser of its own, and in a package or role, reach nothing the
    /// unit's level cannot give it.
    pub(super) fn may_lift_phaser(&self, body: &[Stmt]) -> bool {
        if self.frames.is_empty() || super::decls::Mentions::of(body).has_begin {
            return false;
        }
        self.body_reaches_unit_level(body)
    }

    /// Whether a phaser body written in a package, role or class declared in
    /// code reads nothing that only that scope can give it: not `self`, an
    /// attribute, a `$?` variable of a role, an `EVAL` or a symbolic name. A
    /// body that is not in one reads what any scope gives it.
    pub(super) fn body_reaches_unit_level(&self, body: &[Stmt]) -> bool {
        if !self.in_package() {
            return true;
        }
        let mut locals = HashSet::from(["self".to_string()]);
        let mut in_role = false;
        for params in self.frames.iter().filter_map(|f| f.role.as_ref()) {
            locals.extend(params.iter().cloned());
            in_role = true;
        }
        super::super::package_phasers::movable(body, &locals, in_role)
    }

    /// Run `body` inside each package the current statement is written in, so
    /// it reaches the package's lexicals and finds it composed.
    pub(super) fn run_in_packages(&mut self, mut body: Vec<Stmt>) -> Vec<Stmt> {
        for enclosing in self.frames.iter().rev().filter_map(|f| f.package.as_ref()) {
            self.lifted.needs_prologue = true;
            body = enclosing.run_in(body);
        }
        body
    }

    /// Walk the routines of a class or package body, so the `INIT`/`CHECK`
    /// phasers in them can be lifted. Nothing else in the body is walked: a
    /// `BEGIN` there is not lifted ([`super`]).
    pub(super) fn walk_package(&mut self, stmt: &mut Stmt) {
        // A phaser lifted from here runs in the unit's own sequence, so every
        // package around it has to be reachable by name from the unit's level.
        if !self.frames.iter().all(|f| f.package.is_some()) || !has_phaser(stmt) {
            return;
        }
        let Some((enclosing, _)) = super::super::package_phasers::Enclosing::of_decl(stmt) else {
            return;
        };
        let (Stmt::ClassDecl { body, .. } | Stmt::Package { body, .. }) = stmt else {
            return;
        };
        let bindings = declared_bindings(body, enclosing.lexicals());
        self.walk_members(
            body,
            Frame {
                bindings,
                package: Some(enclosing),
                ..Frame::default()
            },
        );
    }

    /// Walk a type a lifted phaser cannot re-enter: a role (its body runs once
    /// per composition, and has no store of its own), and a class declared
    /// inside code (it does not exist at the unit's level until the code runs,
    /// so there is no package to re-enter by name). A phaser lifted from one of
    /// them runs in no package. A role's routines are walked, and a phaser that
    /// reads what the role declares stays put. A class's body is walked as a
    /// scope of its own.
    pub(super) fn walk_detached(&mut self, stmt: &mut Stmt) {
        if !has_phaser(stmt) {
            return;
        }
        let names = super::decls::declared_names(stmt);
        match stmt {
            Stmt::RoleDecl {
                type_params, body, ..
            } => {
                let params = type_params
                    .iter()
                    .map(|p| p.trim_start_matches(['$', '@', '%', '&']).to_string())
                    .collect();
                let bindings = declared_bindings(body, &[]);
                self.walk_members(
                    body,
                    Frame {
                        bindings,
                        role: Some(params),
                        ..Frame::default()
                    },
                );
            }
            // A class declared in code runs its body each time the code runs,
            // so its body is a scope like a routine's: its lexicals get static
            // cells, and its routines are copied into a lifted phaser.
            Stmt::ClassDecl {
                name_expr: None,
                is_unit: false,
                body,
                ..
            } => {
                let frame = Frame {
                    role: Some(Vec::new()),
                    ..Frame::default()
                };
                // The class itself does not exist where the phaser runs, so a
                // phaser that names it is not lifted.
                self.walk_list(body, frame, |w| {
                    if let Some(names) = names {
                        w.declare_unavailable_type(names);
                    }
                });
            }
            _ => {}
        }
    }

    /// Walk the routines and imports among the members of a package or role
    /// body, in the scope `frame` stands for. A class nested in it is walked for
    /// its own phasers.
    fn walk_members(&mut self, body: &mut Vec<Stmt>, frame: Frame) {
        let in_package = frame.package.is_some();
        self.frames.push(frame);
        // A BEGIN written directly in a package body already runs while the
        // package is declared, so it stays; the package only has to be declared
        // in the prologue (#10328).
        let mut declares_begin = false;
        for (i, member) in body.iter_mut().enumerate() {
            self.current_frame().member = i;
            let walked = match member {
                Stmt::SubDecl { .. } | Stmt::MethodDecl { .. } => true,
                // A class is walked in any body; a package only in a package.
                Stmt::ClassDecl { .. } => true,
                Stmt::Package { .. } => in_package,
                // A role's BEGIN is lifted to the prologue.
                Stmt::Phaser {
                    kind: PhaserKind::Begin,
                    ..
                } => {
                    declares_begin |= in_package;
                    !in_package
                }
                // What the body imports, its routines see too.
                Stmt::Use { .. } | Stmt::No { .. } | Stmt::Need { .. } | Stmt::Import { .. } => {
                    true
                }
                _ => false,
            };
            if walked {
                self.walk_stmt(member, Some((i, false)));
            }
        }
        if in_package && (declares_begin || !self.current_frame().inserts.is_empty()) {
            self.lifted.needs_prologue = true;
        }
        self.finish_scope(body);
    }
}

/// The variables a package or role body declares at its top level. The
/// package's static store holds the ones in `shared`; any other (`our`,
/// `state`, dynamic, a constant, and every one of a role) it does not, so a
/// phaser reading one is not lifted.
fn declared_bindings(body: &[Stmt], shared: &[String]) -> Vec<Binding> {
    let mut bindings = Vec::new();
    let mut pending: Vec<&Stmt> = body.iter().collect();
    while let Some(stmt) = pending.pop() {
        match stmt {
            Stmt::VarDecl { name, .. } => bindings.push(Binding {
                name: name.clone(),
                kind: if shared.binary_search(name).is_ok() {
                    BindingKind::PackageLexical
                } else {
                    BindingKind::Opaque
                },
            }),
            Stmt::SyntheticBlock(inner) => pending.extend(inner),
            _ => {}
        }
    }
    bindings
}

/// Whether a declaration holds a `BEGIN`, `INIT` or `CHECK` phaser anywhere.
// Cost: O(n), n = size of the declaration.
fn has_phaser(stmt: &Stmt) -> bool {
    let mut found = HasPhaser(false);
    found.visit_stmt(stmt);
    found.0
}

struct HasPhaser(bool);

impl<'ast> Visit<'ast> for HasPhaser {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        match stmt {
            Stmt::Phaser {
                kind: PhaserKind::Begin | PhaserKind::Init | PhaserKind::Check,
                ..
            } => self.0 = true,
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        match expr {
            Expr::PhaserExpr {
                kind: PhaserKind::Begin | PhaserKind::Init | PhaserKind::Check,
                ..
            } => self.0 = true,
            _ => walk_expr(self, expr),
        }
    }
}
