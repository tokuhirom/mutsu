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
//! phaser lifted from a method of one runs in no package, so it reads nothing
//! the type declares or takes as a parameter, which is all it needs when it only
//! reads the method's own lexicals. Whatever else such a phaser does, in a class
//! declared inside code, still runs when that code runs (#10711).

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
        if !self.frames.iter().all(|f| f.package.is_some()) || !has_init_or_check(stmt) {
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

    /// Walk the routines of a type a lifted phaser cannot re-enter: a role
    /// (its body runs once per composition, and has no store of its own), and
    /// a class declared inside code (it does not exist at the unit's level
    /// until the code runs, so there is no package to re-enter by name). A
    /// phaser lifted from one of their routines runs in no package, so it
    /// reads nothing the type declares or takes as a parameter.
    pub(super) fn walk_detached(&mut self, stmt: &mut Stmt) {
        if !has_init_or_check(stmt) {
            return;
        }
        let (params, body) = match stmt {
            Stmt::RoleDecl {
                type_params, body, ..
            } => (
                type_params
                    .iter()
                    .map(|p| p.trim_start_matches(['$', '@', '%', '&']).to_string())
                    .collect(),
                body,
            ),
            Stmt::ClassDecl {
                name_expr: None,
                is_unit: false,
                body,
                ..
            } => (Vec::new(), body),
            _ => return,
        };
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

    /// Walk the routines and imports among the members of a package or role
    /// body, in the scope `frame` stands for. A class nested in it is walked for
    /// its own phasers.
    fn walk_members(&mut self, body: &mut [Stmt], frame: Frame) {
        let in_package = frame.package.is_some();
        self.frames.push(frame);
        for (i, member) in body.iter_mut().enumerate() {
            let walked = match member {
                Stmt::SubDecl { .. } | Stmt::MethodDecl { .. } => true,
                // A class is walked in any body; a package only in a package.
                Stmt::ClassDecl { .. } => true,
                Stmt::Package { .. } => in_package,
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
        self.frames.pop();
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

/// Whether a declaration holds an `INIT` or `CHECK` phaser anywhere.
// Cost: O(n), n = size of the declaration.
fn has_init_or_check(stmt: &Stmt) -> bool {
    let mut found = HasInitOrCheck(false);
    found.visit_stmt(stmt);
    found.0
}

struct HasInitOrCheck(bool);

impl Visit for HasInitOrCheck {
    fn visit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Phaser {
                kind: PhaserKind::Init | PhaserKind::Check,
                ..
            } => self.0 = true,
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::PhaserExpr {
                kind: PhaserKind::Init | PhaserKind::Check,
                ..
            } => self.0 = true,
            _ => walk_expr(self, expr),
        }
    }
}
