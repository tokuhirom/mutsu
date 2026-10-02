//! Nested and value-form BEGIN in the unit prologue (ADR-0134, slice 2).
//!
//! A `BEGIN` written inside a routine, closure, loop or block runs once, while
//! the enclosing unit is being compiled. It does not wait for the enclosing
//! code to run, and it does not run again each time that code runs. A
//! value-form `BEGIN` (`my $v = BEGIN expr`) behaves the same way, and its
//! value is a constant of its site. This module moves each such effect into the
//! unit's prologue, at the position of the top-level statement that contains
//! it.
//!
//! The effect sees the lexicals of its enclosing scopes in their static state.
//! A unit-level lexical is shared directly, because the prologue runs in the
//! unit's frame. A lexical of an inner scope does not exist yet when the
//! prologue runs, so it gets a **static cell**, a unit-level slot that stands
//! for the lexical's static value:
//!
//! - The lifted body runs in a block that declares the inner name, initialized
//!   from the cell. After the body, the block copies the value back to the cell.
//! - The inner declaration moves to the head of its scope and starts from the
//!   cell on every entry. Its initializer, if any, stays at the declaration's
//!   position as an assignment.
//!
//! The resulting value per entry is what rakudo's pad clone gives. One case
//! differs: an `@`/`%` lexical is copied from the cell on each entry, where
//! rakudo shares the same object (ADR-0134 §7).
//!
//! A parameter of an enclosing routine or block is unbound at BEGIN time. The
//! body sees a fresh, empty declaration of that name, as it does on rakudo.
//! An `our` variable is re-declared in the body's block, which binds the same
//! package variable.
//!
//! A routine an enclosing inner scope declares ahead of the BEGIN does not
//! exist in the prologue either. The body gets a copy of each one it calls (see
//! [`routines`]). It also gets the scope's imports and pragmas, a copy of each
//! type or package of the scope it names (see [`decls`], [`pragmas`]), and
//! every operator code variable of the scope.
//!
//! A nested `use lib` is a BEGIN-time effect too, and is lifted the same way:
//! it moves from its position into the prologue, so it extends the repository
//! chain even when its scope never runs (#10481). One that cannot be lifted
//! stays in position and blocks its scope ([`pragmas`]).
//!
//! A BEGIN is not lifted in these cases, and keeps its pre-ADR handling:
//!
//! - an enclosing inner scope declares, ahead of it, a pragma it cannot repeat
//!   (see [`pragmas`]), or a routine that is not a plain `sub` (a `multi`, an
//!   `our sub`, an operator, an exported one), which the prologue cannot
//!   reproduce yet;
//! - it names a type or package of an inner scope that cannot be declared again
//!   unobservably (its body runs code), or reads an inner variable typed by
//!   one;
//! - it can reach a name dynamically (`EVAL`, `CALLER::`, symbolic lookup), or
//!   calls a routine that is neither one of the scope's nor a core one (which
//!   may evaluate a string where it was called from), in a scope that declares
//!   a routine or a type, since it cannot say which one it needs;
//! - it sits in a package body;
//! - it reads a name that resolves to nothing the unit declares (for example
//!   an EVAL's caller lexical);
//! - it reads a `state`, `constant` or group-declared inner lexical.

mod cell_ast;
pub(super) use cell_ast::slot_read;
mod decls;
mod phasers;
mod pragmas;
mod routines;
mod walk;

use super::package_phasers::Enclosing;
use crate::ast::{Expr, PhaserKind, Stmt};
use cell_ast::{decl_from_cell, read_var, renamed_static_decl, sigil_of, static_scalar};
use decls::TypeDecl;
use routines::{Access, Dependencies, FrameBlock, Routine, Scan};
use std::collections::{BTreeMap, HashSet};
use std::sync::atomic::{AtomicUsize, Ordering};

static SLOT_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn next_slot(prefix: &str) -> String {
    format!("{prefix}{}", SLOT_COUNTER.fetch_add(1, Ordering::Relaxed))
}

/// The prologue contributions of the BEGINs nested in one top-level statement.
#[derive(Default)]
pub(super) struct Lifted {
    /// The static declarations of the cells and value slots. They head the
    /// prologue, so every effect can reach them.
    pub(super) decls: Vec<Stmt>,
    /// One statement-form `BEGIN` per lifted effect, in source order.
    pub(super) effects: Vec<Stmt>,
    /// One top-level `INIT` or `CHECK` per phaser lifted out of the statement
    /// being walked ([`phasers`]). They precede the statement; the caller
    /// takes them after each statement.
    pub(super) phasers: Vec<Stmt>,
    /// A lifted phaser re-enters a package, so the statement that declares it
    /// must be composed in the prologue.
    pub(super) needs_prologue: bool,
    /// A BEGIN-time effect could not be lifted. It keeps its pre-ADR handling,
    /// which runs it later than the prologue, so no later effect is lifted
    /// either: that would run it ahead of an effect that precedes it in the
    /// source.
    halted: bool,
}

/// What a unit's top level tells the walk about one of its statements.
#[derive(Clone, Copy)]
pub(super) struct UnitContext<'a> {
    /// The unit is an EVAL's: a free name it does not declare may be one of its
    /// caller's lexicals.
    pub(super) is_eval: bool,
    /// `strict` is off where the statement sits, by a `no strict` among the
    /// unit's top-level statements ahead of it.
    pub(super) strict_off: bool,
    /// The variable names the unit mentions outside its BEGIN bodies.
    pub(super) outside_begin: &'a HashSet<String>,
}

/// Lift every liftable BEGIN nested in `stmt`, a top-level statement of a
/// unit whose top-level lexical names are `unit_names`.
pub(super) fn lift_in_stmt<'a>(
    stmt: &mut Stmt,
    unit_names: &'a HashSet<String>,
    unit: UnitContext<'a>,
    lifted: &'a mut Lifted,
) {
    let mut walker = Walker {
        unit_names,
        unit,
        frames: Vec::new(),
        lifted,
    };
    walker.walk_stmt(stmt, None);
}

struct Walker<'a> {
    unit_names: &'a HashSet<String>,
    unit: UnitContext<'a>,
    frames: Vec<Frame>,
    lifted: &'a mut Lifted,
}

#[derive(Default)]
struct Frame {
    bindings: Vec<Binding>,
    /// A type, package, pragma, operator or other routine the prologue cannot
    /// reproduce was declared in this scope ahead of the current statement.
    blocked: bool,
    /// The imports and pragmas in this scope ahead of the current statement.
    /// A lifted BEGIN repeats them ([`decls`], [`pragmas`]).
    imports: Vec<Stmt>,
    /// What the repeated pragmas among `imports` must not precede in the
    /// lifted BEGIN's block ([`pragmas::Guard`]).
    pragma_guards: Vec<pragmas::Guard>,
    /// The types and packages declared in this scope ahead of the current
    /// statement ([`TypeDecl`]).
    types: Vec<TypeDecl>,
    /// The plain routines declared in this scope ahead of the current
    /// statement. A lifted BEGIN that calls one gets its own copy
    /// ([`Routine`]).
    routines: Vec<Routine>,
    /// Edits to this scope's statement list, applied once it has been walked.
    edits: Vec<(usize, Edit)>,
    /// A package body only: the member being walked, and the `BEGIN`s lifted
    /// out of it, which are inserted ahead of that member ([`phasers`]).
    member: usize,
    inserts: Vec<(usize, Stmt)>,
    /// The package this scope is the body of, when a lifted `INIT` or `CHECK`
    /// has to re-enter it ([`phasers`]).
    package: Option<Enclosing>,
    /// The scope is the body of a role, with the names of its type parameters.
    role: Option<Vec<String>>,
}

struct Binding {
    name: String,
    kind: BindingKind,
}

enum BindingKind {
    Param,
    /// An `our` declaration. Re-declaring it binds the same package variable.
    Our(Box<Stmt>),
    Local {
        index: usize,
        static_decl: Box<Stmt>,
        assign: Option<Box<Stmt>>,
        cell: Option<String>,
    },
    Opaque,
    /// A `my` variable of a package body. The package's static store holds it,
    /// and a phaser run inside the package reaches it there ([`phasers`]).
    PackageLexical,
}

enum Edit {
    Remove,
    Replace(Box<Stmt>),
    /// Move the declaration to the head of its scope, starting from its cell.
    /// The initializer's assignment stays in place.
    Split {
        head: Box<Stmt>,
        assign: Option<Box<Stmt>>,
    },
}

impl Walker<'_> {
    fn current_frame(&mut self) -> &mut Frame {
        self.frames
            .last_mut()
            .expect("a statement list is being walked")
    }

    fn block_current_frame(&mut self) {
        if let Some(frame) = self.frames.last_mut() {
            frame.blocked = true;
        }
    }

    fn bind_opaque(&mut self, name: String) {
        if let Some(frame) = self.frames.last_mut() {
            frame.bindings.push(Binding {
                name,
                kind: BindingKind::Opaque,
            });
        }
    }

    fn bind_decl(&mut self, stmt: &Stmt, index: Option<usize>) {
        let Stmt::VarDecl {
            name,
            is_state,
            is_our,
            ..
        } = stmt
        else {
            return;
        };
        if self.frames.is_empty() {
            return;
        }
        let split = crate::runtime::phasers::split_var_decl(stmt);
        let kind = match (split, index) {
            (Some((static_decl, _)), _) if *is_our => BindingKind::Our(Box::new(static_decl)),
            (Some((static_decl, assign)), Some(index)) if !*is_state => BindingKind::Local {
                index,
                static_decl: Box::new(static_decl),
                assign: assign.map(Box::new),
                cell: None,
            },
            _ => BindingKind::Opaque,
        };
        let name = name.clone();
        self.current_frame().bindings.push(Binding { name, kind });
    }

    /// A `constant` initializer is a BEGIN-time effect. One nested in a scope
    /// is lifted when it reads an inner lexical that a lifted BEGIN gave a
    /// static cell, since only the cell holds the value it must see. Any other
    /// nested constant is left to the constant handling of slice 3.
    fn lift_constant_initializer(&mut self, expr: &mut Expr) {
        if self.frames.is_empty() {
            return;
        }
        let body = [Stmt::Expr(expr.clone())];
        let reads_cell = Scan::of(&body).free().iter().any(|sym| {
            self.find_binding(&sym.resolve(), None)
                .is_some_and(|(f, b)| {
                    matches!(
                        self.frames[f].bindings[b].kind,
                        BindingKind::Local { cell: Some(_), .. }
                    )
                })
        });
        if !reads_cell {
            return;
        }
        let slot = next_slot("__begin_value_");
        if self.lift(&body, Some(&slot), &PhaserKind::Begin) {
            *expr = slot_read(slot);
        }
    }

    /// Lift `body`, the body of a phaser of `kind`, out of the scopes around it
    /// when every name it reads can be supplied outside them. A `BEGIN` goes
    /// into the prologue. An `INIT` or `CHECK` goes into the unit's own
    /// sequence of those, and only when it reads something of an inner scope
    /// ([`phasers`]). With `slot`, the body's value is stored in that
    /// unit-level slot. Returns whether the body was lifted.
    fn lift(&mut self, body: &[Stmt], slot: Option<&str>, kind: &PhaserKind) -> bool {
        let begin = *kind == PhaserKind::Begin;
        if begin && self.lifted.halted {
            return false;
        }
        if !begin && !self.may_lift_phaser(body) {
            return false;
        }
        // A BEGIN written in a role or a class declared in code runs in the
        // prologue, where only what the unit's level can give it exists
        // (#10328). One that reads more keeps its old handling.
        if begin && self.innermost_package_frame().is_none() && !self.body_reaches_unit_level(body)
        {
            self.lifted.halted = true;
            return false;
        }
        // A blockless `BEGIN my %h = ...` declares into the enclosing scope,
        // which the lifted body's block would hide. Its body is that one
        // declaration; `BEGIN { my $x ... }` keeps its `my` to itself.
        let declares = matches!(body, [Stmt::VarDecl { .. } | Stmt::SyntheticBlock(_)]);
        // A placeholder makes the body an error (`X::Placeholder::Block`), which
        // the in-place path reports and the lifted body would not.
        let has_placeholder = !crate::ast::collect_unattached_placeholders(body).is_empty();
        let deps = if declares || has_placeholder || self.frames.iter().any(|f| f.blocked) {
            None
        } else {
            self.resolve_dependencies(body)
        };
        let mut blocks: BTreeMap<usize, FrameBlock> = BTreeMap::new();
        let deps = deps.filter(|deps| self.add_declarations(body, deps, &mut blocks).is_some());
        let Some(deps) = deps else {
            // A BEGIN that stays behind keeps every later one behind it. An
            // INIT or CHECK that does is independent of the rest.
            self.lifted.halted |= begin;
            return false;
        };
        // Outside a type declared in code, a phaser that reads nothing of an
        // inner scope already runs at the right time. Inside one it does not:
        // it runs when the code runs the declaration.
        if !begin && !self.in_detached_type() && !phasers::needs_scope(&deps, &blocks) {
            return false;
        }
        self.add_routines(&deps, &mut blocks);
        self.add_bindings(deps, &mut blocks);
        let mut inner = Vec::new();
        match slot {
            Some(slot) => {
                self.lifted.decls.push(static_scalar(slot));
                inner.push(Stmt::Assign {
                    name: slot.to_string(),
                    expr: Expr::DoBlock {
                        body: body.to_vec(),
                        label: phasers::check_label(kind),
                        origin: crate::ast::DoBlockOrigin::Desugar,
                    },
                    op: crate::ast::AssignOp::Assign,
                    target_is_sigilless: false,
                });
            }
            None => inner.extend_from_slice(body),
        }
        let inner = vec![Stmt::Block(FrameBlock::nest(blocks, inner))];
        if begin {
            let effect = Stmt::Phaser {
                kind: PhaserKind::Begin,
                body: inner,
                condition: None,
                end_index: None,
            };
            // A BEGIN written in a package runs while the package is declared,
            // in source order with the body's own BEGINs: it becomes a member
            // of the package body, ahead of the member it was written in
            // (#10328). Any other goes to the prologue.
            match self.innermost_package_frame() {
                Some(frame) => {
                    let member = self.frames[frame].member;
                    self.frames[frame].inserts.push((member, effect));
                }
                None => self.lifted.effects.push(effect),
            }
        } else {
            let body = self.run_in_packages(inner);
            self.lifted.phasers.push(Stmt::Phaser {
                kind: kind.clone(),
                body,
                condition: None,
                end_index: None,
            });
        }
        true
    }

    /// Give the lifted body each inner binding it reads: a copy of the
    /// parameter or `our` variable, or the lexical's static cell.
    fn add_bindings(&mut self, deps: Dependencies, blocks: &mut BTreeMap<usize, FrameBlock>) {
        for ((frame, binding), access) in deps.bindings {
            let block = blocks.entry(frame).or_default();
            match access {
                Access::CopyIn(decl) => block.copy_in.push(*decl),
                Access::Cell => {
                    let (decl, back) = self.cell_access(frame, binding);
                    block.copy_in.push(decl);
                    block.copy_out.push(back);
                }
            }
        }
    }

    /// The copy-in declaration and copy-out assignment for an inner lexical,
    /// allocating its static cell on first use.
    fn cell_access(&mut self, frame: usize, binding: usize) -> (Stmt, Stmt) {
        let name = self.frames[frame].bindings[binding].name.clone();
        let BindingKind::Local {
            index,
            static_decl,
            assign,
            cell,
        } = &mut self.frames[frame].bindings[binding].kind
        else {
            unreachable!("only a Local binding gets a cell")
        };
        let cell_name = match cell {
            Some(cell_name) => cell_name.clone(),
            None => {
                let cell_name = format!("{}{}", sigil_of(&name), next_slot("__begin_cell_"));
                self.lifted
                    .decls
                    .push(renamed_static_decl(static_decl, &cell_name));
                let head = decl_from_cell(static_decl, &cell_name);
                let edit = Edit::Split {
                    head: Box::new(head),
                    assign: assign.take(),
                };
                let index = *index;
                *cell = Some(cell_name.clone());
                self.frames[frame].edits.push((index, edit));
                cell_name
            }
        };
        let BindingKind::Local { static_decl, .. } = &self.frames[frame].bindings[binding].kind
        else {
            unreachable!()
        };
        let copy_in = decl_from_cell(static_decl, &cell_name);
        let copy_out = Stmt::Assign {
            name: cell_name,
            expr: read_var(&name),
            op: crate::ast::AssignOp::Assign,
            target_is_sigilless: false,
        };
        (copy_in, copy_out)
    }
}
