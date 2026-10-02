//! The INIT and CHECK phasers of a top-level type or package body (#10552).
//!
//! Rakudo runs every `INIT` once, after the whole unit is compiled and before
//! its mainline, in source order; every `CHECK` runs once, at the end of
//! compilation, in reverse source order. Where the phaser is written does not
//! matter: a class body, a role body and a method body are no different from
//! the unit's own top level.
//!
//! The per-level phaser reordering (`phasers::reorder_phasers`) stops at a
//! type or package body, so a phaser there ran when that body ran: after the
//! mainline statements that precede the declaration, and once per composition
//! for a role. This module moves each such phaser out of the declaration, to
//! the unit's top level just ahead of it, where the unit's reordering puts it
//! among the unit's own INIT and CHECK phasers.
//!
//! A phaser of a class or package body still has to see the body's lexicals.
//! The moved phaser body therefore runs inside a [`Stmt::PackageRuntimeBody`]
//! of its package, which re-enters the package and reaches the body's `my`
//! lexicals through the package's static store, as the body's methods do. The
//! declaration itself becomes a BEGIN-time effect of the unit, so the
//! prologue composes the package before any INIT runs (ADR-0134 §7). The
//! lexicals hold their static value then, as on Rakudo: `class C { my $x = 3;
//! INIT say $x }` says `(Any)`.
//!
//! A phaser of a method or `sub` declared in such a body moves the same way
//! when it reads nothing local to the routine (its parameters, `self`, its
//! attributes, its own lexicals and routines). A value-form one (`my $h =
//! INIT ...`) leaves a read of a unit-level slot in its place. A phaser of a
//! top-level `sub` moves the same way, so that it keeps its source position
//! among the unit's phasers even when the prologue takes the `sub`.
//!
//! A role body is not re-entered: a role's body runs per composition, so it
//! has no store of its own. Its phaser moves only when it reads nothing the
//! role body declares, nor a role parameter or a `$?` compile-time variable.
//!
//! Any phaser this module does not move keeps the per-level handling.

use super::nested::slot_read;
use crate::ast::{AssignOp, Expr, PackageRuntimeDecl, PhaserKind, Stmt};
use crate::ast_visit::{NameKind, Visit, walk_expr, walk_stmt};
use std::collections::HashSet;
use std::sync::atomic::{AtomicUsize, Ordering};

static SLOT_COUNTER: AtomicUsize = AtomicUsize::new(0);

/// The name of a fresh unit-level slot for a value-form phaser.
fn next_value_slot() -> String {
    format!(
        "__init_value_{}",
        SLOT_COUNTER.fetch_add(1, Ordering::Relaxed)
    )
}

/// What moving the phasers out of one top-level statement produced.
#[derive(Default)]
pub(super) struct Moved {
    /// The unit-level slots of the value-form phasers. They head the prologue.
    pub(super) slots: Vec<Stmt>,
    /// One top-level `INIT`/`CHECK` per moved phaser, in source order. They
    /// precede the statement.
    pub(super) phasers: Vec<Stmt>,
    /// A moved phaser re-enters its package, so the package must be composed
    /// in the prologue.
    pub(super) needs_prologue: bool,
}

/// One enclosing package a moved phaser re-enters, outermost first.
#[derive(Clone)]
pub(super) struct Enclosing {
    name: crate::symbol::Symbol,
    decl: PackageRuntimeDecl,
    lexicals: Vec<String>,
}

impl Enclosing {
    /// The package a class or brace-scoped package declaration declares, with
    /// the `my` lexicals its body shares through the package's static store.
    /// `None` for any other statement, a `unit` declarator, and a class whose
    /// name is computed.
    // Cost: O(n), n = size of the body's top-level statement list.
    pub(super) fn of_decl(stmt: &Stmt) -> Option<(Enclosing, &[Stmt])> {
        let (name, decl, body) = match stmt {
            Stmt::ClassDecl {
                name,
                name_expr: None,
                is_unit: false,
                is_lexical,
                decl_id,
                body,
                ..
            } => (
                *name,
                PackageRuntimeDecl::Class {
                    is_lexical: *is_lexical,
                    decl_id: *decl_id,
                },
                body,
            ),
            Stmt::Package {
                name,
                is_unit: false,
                body,
                ..
            } => (*name, PackageRuntimeDecl::Package, body),
            _ => return None,
        };
        let mut lexicals: Vec<String> = crate::compiler::Compiler::package_body_lexical_names(body)
            .into_iter()
            .collect();
        lexicals.sort();
        Some((
            Enclosing {
                name,
                decl,
                lexicals,
            },
            body,
        ))
    }

    /// The `my` lexicals the package's static store holds.
    pub(super) fn lexicals(&self) -> &[String] {
        &self.lexicals
    }

    /// `body`, run inside this package: [`Stmt::PackageRuntimeBody`] re-enters
    /// the package and binds its lexicals from the store.
    pub(super) fn run_in(&self, body: Vec<Stmt>) -> Vec<Stmt> {
        vec![Stmt::PackageRuntimeBody {
            name: self.name,
            body,
            lexicals: self.lexicals.clone(),
            decl: self.decl,
        }]
    }
}

/// Move the INIT and CHECK phasers out of `stmt`, a top-level statement of a
/// unit. Statements other than type, package and routine declarations are
/// left alone.
pub(super) fn move_package_phasers(stmt: &mut Stmt, moved: &mut Moved) {
    let mut mover = Mover {
        enclosing: Vec::new(),
        moved,
    };
    mover.take_from_decl(stmt);
}

struct Mover<'a> {
    enclosing: Vec<Enclosing>,
    moved: &'a mut Moved,
}

impl Mover<'_> {
    fn take_from_decl(&mut self, stmt: &mut Stmt) {
        match stmt {
            // An exported type is the declaration plus its export marker.
            Stmt::SyntheticBlock(inner) => {
                for member in inner.iter_mut() {
                    self.take_from_decl(member);
                }
            }
            Stmt::ClassDecl {
                name,
                name_expr: None,
                is_unit: false,
                is_lexical,
                decl_id,
                body,
                ..
            } => {
                let decl = PackageRuntimeDecl::Class {
                    is_lexical: *is_lexical,
                    decl_id: *decl_id,
                };
                self.package_body(*name, decl, body);
            }
            Stmt::Package {
                name,
                is_unit: false,
                body,
                ..
            } => self.package_body(*name, PackageRuntimeDecl::Package, body),
            // A top-level routine's phasers move the same way, so they keep
            // their source order among the unit's (one the prologue takes
            // would otherwise not be lifted at all).
            Stmt::SubDecl { params, body, .. } if self.enclosing.is_empty() => {
                let mut locals = locals_of(body);
                locals.extend(params.iter().map(|p| bare(p).to_string()));
                self.routine_body(body, &locals);
            }
            // A role nested in a package body would need that package for
            // what it reads; only a top-level one moves its phasers.
            Stmt::RoleDecl {
                type_params, body, ..
            } if self.enclosing.is_empty() => {
                let mut locals = locals_of(body);
                locals.extend(type_params.iter().map(|p| bare(p).to_string()));
                self.role_body(body, &locals);
            }
            _ => {}
        }
    }

    fn package_body(
        &mut self,
        name: crate::symbol::Symbol,
        decl: PackageRuntimeDecl,
        body: &mut Vec<Stmt>,
    ) {
        let mut lexicals: Vec<String> = crate::compiler::Compiler::package_body_lexical_names(body)
            .into_iter()
            .collect();
        lexicals.sort();
        // What the store does not hold: `our` variables, code variables,
        // `state` and dynamic ones. A phaser that reads one stays put.
        let unshared = unshared_names(body, &lexicals);
        self.enclosing.push(Enclosing {
            name,
            decl,
            lexicals,
        });
        let mut i = 0;
        while i < body.len() {
            match &mut body[i] {
                Stmt::Phaser {
                    kind: kind @ (PhaserKind::Init | PhaserKind::Check),
                    body: phaser_body,
                    ..
                } if movable(phaser_body, &unshared, false) => {
                    let kind = kind.clone();
                    let phaser_body = std::mem::take(phaser_body);
                    self.push_phaser(kind, phaser_body, None);
                    body.remove(i);
                    continue;
                }
                Stmt::MethodDecl { params, body, .. } | Stmt::SubDecl { params, body, .. } => {
                    let mut locals = locals_of(body);
                    locals.extend(params.iter().map(|p| bare(p).to_string()));
                    locals.insert("self".to_string());
                    locals.extend(unshared.iter().cloned());
                    self.routine_body(body, &locals);
                }
                other => self.take_from_decl(other),
            }
            i += 1;
        }
        self.enclosing.pop();
    }

    /// The phasers written directly in a routine's body, statement form or
    /// the whole initializer of a declaration.
    fn routine_body(&mut self, body: &mut Vec<Stmt>, locals: &HashSet<String>) {
        let mut i = 0;
        while i < body.len() {
            match &mut body[i] {
                Stmt::Phaser {
                    kind: kind @ (PhaserKind::Init | PhaserKind::Check),
                    body: phaser_body,
                    ..
                } if movable(phaser_body, locals, false) => {
                    let kind = kind.clone();
                    let phaser_body = std::mem::take(phaser_body);
                    if i + 1 == body.len() {
                        // A phaser that ends the routine is its value: leave
                        // a read of the slot it stores into.
                        let slot = next_value_slot();
                        self.push_phaser(kind, phaser_body, Some(&slot));
                        body[i] = Stmt::Expr(slot_read(slot));
                        i += 1;
                    } else {
                        self.push_phaser(kind, phaser_body, None);
                        body.remove(i);
                    }
                    continue;
                }
                Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                    if let Expr::PhaserExpr {
                        kind: kind @ (PhaserKind::Init | PhaserKind::Check),
                        body: phaser_body,
                    } = expr
                        && movable(phaser_body, locals, false)
                    {
                        let kind = kind.clone();
                        let phaser_body = std::mem::take(phaser_body);
                        let slot = next_value_slot();
                        self.push_phaser(kind, phaser_body, Some(&slot));
                        *expr = slot_read(slot);
                    }
                }
                _ => {}
            }
            i += 1;
        }
    }

    fn role_body(&mut self, body: &mut Vec<Stmt>, locals: &HashSet<String>) {
        let mut i = 0;
        while i < body.len() {
            match &mut body[i] {
                Stmt::Phaser {
                    kind: kind @ (PhaserKind::Init | PhaserKind::Check),
                    body: phaser_body,
                    ..
                } if movable(phaser_body, locals, true) => {
                    let kind = kind.clone();
                    let phaser_body = std::mem::take(phaser_body);
                    self.push_phaser(kind, phaser_body, None);
                    body.remove(i);
                    continue;
                }
                Stmt::MethodDecl { params, body, .. } | Stmt::SubDecl { params, body, .. } => {
                    let mut routine_locals = locals_of(body);
                    routine_locals.extend(params.iter().map(|p| bare(p).to_string()));
                    routine_locals.insert("self".to_string());
                    routine_locals.extend(locals.iter().cloned());
                    self.role_routine_body(body, &routine_locals);
                }
                _ => {}
            }
            i += 1;
        }
    }

    /// A role's routine: as [`Mover::routine_body`], with the role's own
    /// restrictions on what a moved body reads.
    fn role_routine_body(&mut self, body: &mut Vec<Stmt>, locals: &HashSet<String>) {
        let last = body.len().saturating_sub(1);
        let mut at = 0;
        body.retain_mut(|stmt| {
            at += 1;
            match stmt {
                Stmt::Phaser {
                    kind: kind @ (PhaserKind::Init | PhaserKind::Check),
                    body: phaser_body,
                    ..
                } if movable(phaser_body, locals, true) => {
                    let kind = kind.clone();
                    let phaser_body = std::mem::take(phaser_body);
                    if at - 1 == last {
                        let slot = next_value_slot();
                        self.push_phaser(kind, phaser_body, Some(&slot));
                        *stmt = Stmt::Expr(slot_read(slot));
                        true
                    } else {
                        self.push_phaser(kind, phaser_body, None);
                        false
                    }
                }
                _ => true,
            }
        });
    }

    /// Add the moved phaser: its body, re-entering each enclosing package, as
    /// a top-level phaser. With `slot`, the body's value is stored there.
    fn push_phaser(&mut self, kind: PhaserKind, body: Vec<Stmt>, slot: Option<&str>) {
        let mut inner = match slot {
            Some(slot) => {
                self.moved.slots.push(slot_decl(slot));
                // A CHECK's error is reported as a BEGIN-time one; the
                // compiler wraps a DoBlock carrying this label accordingly.
                let label = (kind == PhaserKind::Check).then(|| "__mutsu_check_phaser__".into());
                vec![Stmt::Assign {
                    name: slot.to_string(),
                    expr: Expr::DoBlock {
                        body,
                        label,
                        origin: crate::ast::DoBlockOrigin::Desugar,
                    },
                    op: AssignOp::Assign,
                    target_is_sigilless: false,
                }]
            }
            None => body,
        };
        for enclosing in self.enclosing.iter().rev() {
            self.moved.needs_prologue = true;
            inner = vec![Stmt::PackageRuntimeBody {
                name: enclosing.name,
                body: inner,
                lexicals: enclosing.lexicals.clone(),
                decl: enclosing.decl,
            }];
        }
        self.moved.phasers.push(Stmt::Phaser {
            kind,
            body: inner,
            condition: None,
            end_index: None,
        });
    }
}

fn slot_decl(slot: &str) -> Stmt {
    Stmt::VarDecl {
        name: slot.to_string(),
        expr: Expr::Literal(crate::value::Value::NIL),
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: vec![],
        custom_traits: vec![],
        where_constraint: None,
    }
}

/// A name without its sigil (`$x`, `@a`, `&f` all name `x`, `a`, `f`).
fn bare(name: &str) -> &str {
    name.trim_start_matches(['$', '@', '%', '&'])
}

/// Whether a phaser body may run away from its site: it reads none of
/// `locals`, no attribute, and nothing it cannot name statically. In a role
/// or a class declared in code (`in_role`) a `$?` compile-time variable is
/// local too, and so is `$*PACKAGE`, which is the package being declared.
pub(super) fn movable(body: &[Stmt], locals: &HashSet<String>, in_role: bool) -> bool {
    let mut names = Names::default();
    for stmt in body {
        names.visit_stmt(stmt);
    }
    if names.symbolic || names.names.iter().any(|n| n == "EVAL" || n == "EVALFILE") {
        return false;
    }
    !names.names.iter().any(|name| {
        let name = bare(name);
        locals.contains(name)
            || name.starts_with(['!', '.'])
            || (in_role && (name.contains('?') || name == "*PACKAGE"))
    })
}

/// Every name a piece of code mentions, without its sigil.
#[derive(Default)]
struct Names {
    names: Vec<String>,
    symbolic: bool,
}

impl<'ast> Visit<'ast> for Names {
    fn visit_name(&mut self, name: &str, kind: NameKind) {
        self.symbolic |= kind == NameKind::Symbolic;
        self.names.push(name.to_string());
    }
}

/// The names a body declares, other than inside the INIT and CHECK phasers
/// that may move out of it.
// Cost: O(n), n = size of the body.
fn locals_of(body: &[Stmt]) -> HashSet<String> {
    let mut locals = Locals::default();
    for stmt in body {
        locals.visit_stmt(stmt);
    }
    locals.0
}

#[derive(Default)]
struct Locals(HashSet<String>);

impl<'ast> Visit<'ast> for Locals {
    fn visit_stmt(&mut self, stmt: &'ast Stmt) {
        if !matches!(
            stmt,
            Stmt::Phaser {
                kind: PhaserKind::Init | PhaserKind::Check,
                ..
            }
        ) {
            walk_stmt(self, stmt);
        }
    }

    fn visit_expr(&mut self, expr: &'ast Expr) {
        if !matches!(
            expr,
            Expr::PhaserExpr {
                kind: PhaserKind::Init | PhaserKind::Check,
                ..
            }
        ) {
            walk_expr(self, expr);
        }
    }

    fn visit_name(&mut self, name: &str, kind: NameKind) {
        if matches!(
            kind,
            NameKind::VarDecl
                | NameKind::SubDecl
                | NameKind::Param
                | NameKind::BlockParam
                | NameKind::Sigilless
        ) {
            self.0.insert(bare(name).to_string());
        }
    }
}

/// The variables a package body declares that its static store does not
/// hold, so a moved phaser could not reach them: everything it declares but
/// `lexicals` (its `our`, `state`, dynamic and code variables). Its routines
/// are reached through the package, as from its methods.
fn unshared_names(body: &[Stmt], lexicals: &[String]) -> HashSet<String> {
    let shared: HashSet<&str> = lexicals.iter().map(|l| bare(l)).collect();
    let mut declared = HashSet::new();
    // The body's own statement list, and the group declarations
    // (`my ($a, $b)`) in it.
    let mut pending: Vec<&Stmt> = body.iter().collect();
    while let Some(stmt) = pending.pop() {
        match stmt {
            Stmt::VarDecl { name, .. } => {
                declared.insert(bare(name).to_string());
            }
            Stmt::SyntheticBlock(inner) => pending.extend(inner),
            _ => {}
        }
    }
    declared.retain(|name| !shared.contains(name.as_str()));
    declared
}
